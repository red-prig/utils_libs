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
function  _msize(P:Pointer):SizeUint; cdecl;

implementation

uses
 LFQueue;

Type
 PmtFreelist=^TmtFreelist;

 TQNode_=packed object
  next_:Pointer;
  //some data
 end;

 PQNode=^TQNode;
 TQNode=packed object(TQNode_)
  freelist:PmtFreelist;
  Size_:PtrUInt;
  //some data
 end;

 PQNode_m=^TQNode_m;
 TQNode_m=packed object(TQNode_)
  freelist:PmtFreelist;
 end;

 TmtFreelist=packed object(TQNode_)
  Var
   FQueue:TIntrusiveMPSCQueue;
   FCountFree:PtrUInt;
   FCountAlloc:PtrUInt;
   FCountCheck:PtrUInt;
   Procedure Create; inline;
   Function  Push(Node:Pointer):Boolean; inline;
   Function  Pop(Var Node:Pointer):Boolean; inline;
   Procedure WaitFree;
   Function  flGetMem(Size:PtrUint):Pointer; inline;
   Function  flFreeMem(P:Pointer):PtrUint;   inline;

   Function  flMalloc(Size:PtrUint):Pointer; inline;
   Procedure flFree(P:Pointer);  inline;
 end;

 TmtGlobalList=packed object
  Var
   FmtFreelist:PmtFreelist;
   FCount:PtrUInt;
   FMainQueue:TIntrusiveMPSCQueue;
   FRareQueue:TIntrusiveMPSCQueue;
  Procedure Create; inline;
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

 _Getmem :Function(Size:ptruint):Pointer;
 _Freemem:Function(p:pointer):ptruint;
 _MemSize:function(p:pointer):ptruint;

Function  SysGetmem(Size:ptruint):Pointer; inline;
begin
 Result:=_Getmem(Size);
end;

Function  SysFreemem(p:pointer):ptruint; inline;
begin
 Result:=_Freemem(p);
end;

Function  SysMemSize(p:pointer):ptruint; inline;
begin
 Result:=_MemSize(p);
end;

Procedure TmtFreelist.Create; inline;
begin
 FillChar(Self,SizeOf(Self),0);
 FQueue.Create;
 ReadWriteBarrier;
end;

Function TmtFreelist.Push(Node:Pointer):Boolean; inline;
begin
 Result:=FQueue.Push(Node);
 Assert(Result);
 System.InterLockedIncrement(Pointer(FCountFree));
end;

Function TmtFreelist.Pop(Var Node:Pointer):Boolean; inline;
begin
 Result:=FQueue.Pop(Node);
 if Result then
 begin
  System.InterLockedDecrement(Pointer(FCountFree));
 end;
end;

Procedure TmtFreelist.WaitFree;
Var
 rc:PtrUInt;
 Node:Pointer;
begin
 Node:=nil;
 rc:=load_consume(FCountFree);
 While Pop(Node) do
 begin
  SysFreeMem(Node);
  System.InterLockedDecrement(Pointer(FCountAlloc));
  Node:=nil;
  if rc=0 then Break;
  Dec(rc);
  if rc=0 then Break;
 end;
 Assert(Node=nil);
end;

Function TmtFreelist.flGetMem(Size:PtrUint):Pointer; inline;
begin
 WaitFree;
 Result:=SysGetMem(Size+node_size);
 Size:=SysMemSize(Result)-node_size;
 With PQNode(Result)^ do
 begin
  freelist:=@self;
  Assert(freelist<>nil);
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
 FmtFreelist:=nil;
 FCount:=0;
 FMainQueue.Create;
 FRareQueue.Create;
end;

Procedure TmtGlobalList.LazyWaitFree;
Var
 Node:PmtFreelist;
 rc:PtrUInt;
begin
 Node:=nil;
 if FmtFreelist=mtFreelist then
 begin

  if FRareQueue.Pop(Node) then
  begin
   Node^.WaitFree;
   if (load_consume(Node^.FCountAlloc)=0) and
      (load_consume(Node^.FCountFree )=0) then
   begin
    SysFreeMem(Node);
   end else
   begin
    FRareQueue.Push(Node);
   end;
  end;

  rc:=load_consume(FCount);
  while FMainQueue.Pop(Node) do
  begin
   Node^.WaitFree;
   if (load_consume(Node^.FCountAlloc)=0) and
      (load_consume(Node^.FCountFree )=0) then
   begin
    System.InterLockedDecrement(Pointer(FCount));
    SysFreeMem(Node);
   end else
   begin
    if PtrUint(System.InterLockedIncrement(Pointer(Node^.FCountCheck)))>2 then
    begin
     System.InterLockedDecrement(Pointer(FCount));
     FRareQueue.Push(Node);
    end else
    begin
     FMainQueue.Push(Node);
    end;
   end;
   if rc=0 then Break;
   Dec(rc);
   if rc=0 then Break;
  end;
 end;
end;

Procedure TmtGlobalList.GlobalWaitFree;
Var
 Node:PmtFreelist;
begin
 Node:=nil;
 While FMainQueue.Pop(Node) do
 begin
  Node^.WaitFree;
  System.InterLockedDecrement(Pointer(FCount));
  SysFreeMem(Node);
 end;
 While FRareQueue.Pop(Node) do
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
 FMainQueue.Push(P);
 System.InterLockedIncrement(Pointer(FCount));
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

function _msize(P:Pointer):SizeUint; cdecl;
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

  S:=_msize(P);
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
 MM:=Default(TMemoryManager);
 GetMemoryManager(MM);

 _ThreadDone:=MM.DoneThread;

 _Getmem    :=MM.Getmem;
 _Freemem   :=MM.Freemem;
 _MemSize   :=MM.MemSize;

 mtGlobal.Create;
 mtThreadInit;
 mtGlobal.FmtFreelist:=mtFreelist;

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

