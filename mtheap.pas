{ Add-on over the standard memory manager, for quick freeing up memory

  Copyright (C) 2018-2020 Red_prig

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.
}

unit mtHeap;

{$mode objfpc}{$H+}

interface

Function  Malloc(Size:PtrUint):Pointer; cdecl;
Procedure Free(P:Pointer); cdecl;
function  ReAlloc(P:Pointer;Size:ptruint):Pointer; cdecl;
Function  CAlloc(unitSize,UnitCount:ptruint):Pointer; cdecl;

implementation

Type
 PmtFreelist=^TmtFreelist;
 PQNode=^TQNode;
 TQNode=record
  freelist:PmtFreelist;
  next_:PQNode;
  Size_:PtrUInt;
  //some data
 end;

 PQNode_m=^TQNode_m;
 TQNode_m=record
  freelist:PmtFreelist;
  next_:PQNode;
 end;

 TmtFreelist=object
  Var
   tail_:PQNode;
   stub:TQNode_m;
   FCountFree:PtrUInt;
   FCountAlloc:PtrUInt;
   head_:PQNode;
   Procedure Create; inline;
   Function  Push(Node:Pointer):Boolean;
   Function  Pop(Var Node:Pointer):Boolean;
   Procedure WaitFree;
   Function  flGetMem(Size:PtrUint):Pointer; inline;
   Function  flFreeMem(P:Pointer):PtrUint;   inline;

   Function  flMalloc(Size:PtrUint):Pointer; inline;
   Procedure flFree(P:Pointer);  inline;

 end;

 TmtGlobalList=object
 Var
  tail_:PmtFreelist;
  stub:PmtFreelist;
  FmtFreelist:PmtFreelist;
  head_:PmtFreelist;
  Procedure Create; inline;
  Function  Push(Node:PmtFreelist):Boolean;
  Function  Pop(Var Node:PmtFreelist):Boolean;
  Procedure LazyWaitFree;
  Procedure GlobalWaitFree;
  Function  flGetList:PmtFreelist; inline;
  Procedure flFreeList(P:PmtFreelist); inline;
 end;

Const
 node_size=SizeOf(TQNode);
 node_size_m=SizeOf(TQNode_m);

threadvar
 mtFreelist:PmtFreelist;

var
 mtGlobal:TmtGlobalList;
 _ThreadDone:TProcedure;

function load_consume(Var addr:Pointer):Pointer; inline;
begin
 ReadDependencyBarrier;
 Result:=addr;
end;

Procedure store_release(Var addr:Pointer;v:Pointer); inline;
begin
 WriteBarrier;
 addr:=v;
end;

Procedure TmtFreelist.Create;inline;
begin
 FillChar(Self,SizeOf(Self),0);
 head_:=@stub;
 tail_:=@stub;
 ReadWriteBarrier;
end;

Function TmtFreelist.Push(Node:Pointer):Boolean;
Var
 prev:PQNode;
begin
 if not Assigned(Node) then Exit(False);
 PQNode(Node)^.next_:=nil;
 prev:=System.InterLockedExchange(head_,Node);
 store_release(prev^.next_,Node);
 System.InterLockedIncrement(Pointer(FCountFree));
 Result:=True;
end;

Function TmtFreelist.Pop(Var Node:Pointer):Boolean;
Var
 tail,n,head:PQNode;
begin
 Node:=nil;
 Result:=False;

 tail:=tail_;
 n:=load_consume(tail^.next_);

 if tail=@stub then
 begin
  if n=nil then Exit;
  store_release(tail_,n);
  tail:=n;
  n:=load_consume(n^.next_);
 end;

 if n<>nil then
 begin
  store_release(tail_,n);
  Node:=tail;
  System.InterLockedDecrement(Pointer(FCountFree));
  Exit(True);
 end;

 head:=head_;
 if tail<>head then Exit;

 stub.next_:=nil;
 n:=System.InterLockedExchange(head_,@stub);
 store_release(n^.next_,@stub);

 n:=load_consume(tail^.next_);

 if n<>nil then
 begin
  store_release(tail_,n);
  Node:=tail;
  System.InterLockedDecrement(Pointer(FCountFree));
  Exit(True);
 end;

end;

Procedure TmtFreelist.WaitFree;
Var
 rc:PtrUInt;
 Node:Pointer;
begin
 Node:=nil;
 rc:=FCountFree;
 While Pop(Node) do
 begin
  SysFreeMem(Node);
  System.InterLockedDecrement(Pointer(FCountAlloc));
  if rc=0 then Break;
  Dec(rc);
  if rc=0 then Break;
 end;
end;

Function TmtFreelist.flGetMem(Size:PtrUint):Pointer; inline;
begin
 WaitFree;
 Result:=SysGetMem(Size+node_size);
 Size:=SysMemSize(Result)-node_size;
 With PQNode(Result)^ do
 begin
  freelist:=@self;
  Size_:=Size;
  store_release(next_,nil);
 end;
 PtrUint(Result):=PtrUint(Result)+node_size;
 System.InterLockedIncrement(Pointer(FCountAlloc));
end;

Function TmtFreelist.flFreeMem(P:Pointer):PtrUint; inline;
begin
 WaitFree;
 PtrUint(P):=PtrUint(P)-node_size;
 With PQNode(P)^ do
 begin
  Result:=Size_;
  if freelist=@self then
  begin
   SysFreeMem(P);
   System.InterLockedDecrement(Pointer(FCountAlloc));
  end else
  begin
   freelist^.Push(P);
  end;
 end;
end;

Function TmtFreelist.flMalloc(Size:PtrUint):Pointer; inline;
begin
 WaitFree;
 Result:=SysGetMem(Size+node_size_m);
 With PQNode_m(Result)^ do
 begin
  freelist:=@self;
  store_release(next_,nil);
 end;
 PtrUint(Result):=PtrUint(Result)+node_size_m;
 System.InterLockedIncrement(Pointer(FCountAlloc));
end;

Procedure TmtFreelist.flFree(P:Pointer); inline;
begin
 WaitFree;
 PtrUint(P):=PtrUint(P)-node_size_m;
 With PQNode_m(P)^ do
 begin
  if freelist=@self then
  begin
   SysFreeMem(P);
   System.InterLockedDecrement(Pointer(FCountAlloc));
  end else
  begin
   freelist^.Push(P);
  end;
 end;
end;

//------------------------

Procedure TmtGlobalList.Create; inline;
begin
 FillChar(Self,SizeOf(Self),0);
 head_:=@stub;
 tail_:=@stub;
 ReadWriteBarrier;
end;

Function TmtGlobalList.Push(Node:PmtFreelist):Boolean;
Var
 prev:PmtFreelist;
begin
 if not Assigned(Node) then Exit(False);
 Node^.stub.freelist:=nil;
 prev:=System.InterLockedExchange(head_,Node);
 store_release(prev^.stub.freelist,Node);
 Result:=True;
end;

Function TmtGlobalList.Pop(Var Node:PmtFreelist):Boolean;
Var
 tail,n,head:PmtFreelist;
begin
 Node:=nil;
 Result:=False;

 tail:=tail_;
 n:=load_consume(tail^.stub.freelist);

 if tail=@stub then
 begin
  if n=nil then Exit;
  store_release(tail_,n);
  tail:=n;
  n:=load_consume(n^.stub.freelist);
 end;

 if n<>nil then
 begin
  store_release(tail_,n);
  Node:=tail;
  Exit(True);
 end;

 head:=head_;
 if tail<>head then Exit;

 n:=System.InterLockedExchange(head_,@stub);
 store_release(n^.stub.freelist,@stub);

 n:=load_consume(tail^.stub.freelist);

 if n<>nil then
 begin
  store_release(tail_,n);
  Node:=tail;
  Exit(True);
 end;

end;

Procedure TmtGlobalList.LazyWaitFree;
Var
 Node:PmtFreelist;
begin
 Node:=nil;
 if FmtFreelist=mtFreelist then
 if Pop(Node) then
 begin
  Node^.WaitFree;
  if Node^.FCountAlloc=0 then
  begin
   SysFreeMem(Node);
  end else
  begin
   Push(Node);
  end;
 end;
end;

Procedure TmtGlobalList.GlobalWaitFree;
Var
 Node:PmtFreelist;
begin
 Node:=nil;
 While Pop(Node) do
 begin
  Node^.WaitFree;
  SysFreeMem(Node);
 end;
end;

Function TmtGlobalList.flGetList:PmtFreelist; inline;
begin
 Result:=SysGetMem(SizeOf(TmtFreelist));
end;

Procedure TmtGlobalList.flFreeList(P:PmtFreelist); inline;
begin
 Push(P);
end;

Procedure mtThreadInit; forward;

Function mtGetMem(Size:PtrUint):Pointer;
begin
 mtThreadInit;
 if Size=0 then
 begin
  Result:=nil;
 end else
 begin
  Result:=mtFreelist^.flGetMem(Size);
 end;
end;

Function mtFreeMem(P:Pointer):PtrUint;
begin
 mtThreadInit;
 if Assigned(P) then
 begin
  Result:=mtFreelist^.flFreeMem(P);
 end else
 begin
  Result:=0;
 end;
 mtGlobal.LazyWaitFree;
end;

Function mtFreememSize(p:pointer;Size:ptruint):ptruint;
begin
 if size=0 then exit(0);
 Result:=mtFreeMem(p);
end;

Function mtMemSize(P:pointer):ptruint;
begin
 Result:=0;
 if PtrUint(P)>node_size then
 begin
  PtrUint(P):=PtrUint(P)-node_size;
  Result:=PQNode(P)^.Size_;
 end;
end;

Function mtReAllocMem(var p:pointer;size:ptruint):Pointer;
Var
 S:PtrUint;
begin
 Result:=nil;
 if (P=nil) then
 begin
  Result:=mtGetMem(Size);
 end else
 if Size>0 then
 begin

  S:=mtMemSize(P);
  if (S-(S div 3)>Size) then
  begin
   Result:=mtGetMem(Size);
   Move(P^,Result^,Size);
   mtFreeMem(P);
  end else
  if (S<Size) then
  begin
   Result:=mtGetMem(Size);
   Move(P^,Result^,S);
   mtFreeMem(P);
  end else
  begin
   Result:=P;
  end;

 end else
 begin
  mtFreeMem(P);
 end;
 P:=Result;
end;

function mtAllocMem(size:ptruint):pointer;
begin
 Result:=mtGetMem(size);
 if (Result<>nil) then
  FillChar(Result^,mtMemSize(Result),0);
end;

Function Malloc(Size:PtrUint):Pointer; cdecl;
begin
 mtThreadInit;
 if Size=0 then
 begin
  Result:=nil;
 end else
 begin
  Result:=mtFreelist^.flMalloc(Size);
 end;
end;

Procedure Free(P:Pointer); cdecl;
begin
 mtThreadInit;
 if Assigned(P) then
 begin
  mtFreelist^.flFree(P);
 end;
 mtGlobal.LazyWaitFree;
end;

Function cMemSize(P:pointer):ptruint;
begin
 Result:=0;
 if PtrUint(P)>node_size_m then
 begin
  PtrUint(P):=PtrUint(P)-node_size_m;
  Result:=SysMemSize(P);
  if Result>=node_size_m then
  begin
   Result:=Result-node_size_m;
  end;
 end;
end;

function ReAlloc(P:Pointer;Size:ptruint):Pointer; cdecl;
Var
 S:PtrUint;
begin
 Result:=nil;
 if (P=nil) then
 begin
  Result:=Malloc(Size);
 end else
 if Size>0 then
 begin

  S:=cMemSize(P);
  if (S-(S div 3)>Size) then
  begin
   Result:=Malloc(Size);
   Move(P^,Result^,Size);
   Free(P);
  end else
  if (S<Size) then
  begin
   Result:=Malloc(Size);
   Move(P^,Result^,S);
   Free(P);
  end else
  begin
   Result:=P;
  end;

 end else
 begin
  Free(P);
 end;

end;

Function CAlloc(unitSize,UnitCount:ptruint):Pointer; cdecl;
begin
 unitSize:=unitSize*UnitCount;
 Result:=Malloc(unitSize);
 if (Result<>nil) then
  FillChar(Result^,unitSize,0);
end;

Procedure mtThreadInit;
begin
 if mtFreelist=nil then
 begin
  mtFreelist:=mtGlobal.flGetList;
  mtFreelist^.Create;
 end;
end;

Procedure mtThreadFree;
begin
 if mtFreelist<>nil then
 begin
  mtFreelist^.WaitFree;
  mtGlobal.flFreeList(mtFreelist);
 end;
 if Assigned(_ThreadDone) then _ThreadDone();
end;

Procedure Init;
Var
 MM:TMemoryManager;
begin
 _ThreadDone:=nil;
 mtGlobal.Create;
 mtThreadInit;
 mtGlobal.FmtFreelist:=mtFreelist;

 MM:=Default(TMemoryManager);
 GetMemoryManager(MM);

 MM.DoneThread:=@mtThreadFree;

 MM.Getmem     :=@mtGetmem;
 MM.Freemem    :=@mtFreemem;
 MM.FreememSize:=@mtFreememSize;
 MM.AllocMem   :=@mtAllocMem;
 MM.ReAllocMem :=@mtReAllocMem;
 MM.MemSize    :=@mtMemSize;

 SetMemoryManager(MM);
end;

initialization
 Init;

finalization
 mtGlobal.GlobalWaitFree;

end.

