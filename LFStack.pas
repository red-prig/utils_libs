{ Implimentation of fine-grained stack

  Copyright (C) 2019-2020 Red_prig

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

unit LFStack;

{$mode objfpc}{$H+}

interface

Type
 generic TIntrusiveStack<back_off>=object
  protected
   type
    PQNode=^TQNode;
    TQNode=record
     Next:PQNode;
     //some data
    end;
   Var
    m_Head:PQNode;
    m_lock:Pointer;
  public
   Procedure Create;
   Function  Push(Node:Pointer):Boolean;
   Function  Pop(Var Node:Pointer):Boolean;
   Function  IsEmpty:Boolean; inline;
 end;

 generic TStack<TItem,Allocator,back_off>=object
  type
   TIS=specialize TIntrusiveStack<back_off>;
   PNode=^TNode;
   TNode=record
    dummy:TIS.TQNode;
    Item:TItem;
   end;
  var
   Data:TIS;
  Procedure Create; inline;
  Function  push_front(Const val:TItem):Boolean;
  Function  pop_front(Var val:TItem):Boolean;
 end;

function  load_consume(Var addr:Pointer):Pointer; inline;
function  load_consume(Var addr:SizeUInt):SizeUInt; inline;
function  load_acquire(Var addr:Pointer):Pointer; inline;
Procedure store_release(Var addr:Pointer;v:Pointer); inline;
Procedure store_release(Var addr:SizeUInt;v:SizeUInt); inline;
function  CAS(Var addr:Pointer;Comp,New:Pointer):Boolean; inline;
function  CAS(Var addr:SizeUInt;Comp,New:SizeUInt):Boolean; inline;
function  XCHG(Var addr:Pointer;New:Pointer):Pointer; inline;

implementation

function load_consume(Var addr:Pointer):Pointer; inline;
begin
 ReadDependencyBarrier;
 Result:=addr;
end;

function load_consume(Var addr:SizeUInt):SizeUInt; inline;
begin
 ReadDependencyBarrier;
 Result:=addr;
end;

function load_acquire(Var addr:Pointer):Pointer; inline;
begin
 ReadBarrier;
 Result:=addr;
end;

Procedure store_release(Var addr:Pointer;v:Pointer); inline;
begin
 WriteBarrier;
 addr:=v;
end;

Procedure store_release(Var addr:SizeUInt;v:SizeUInt); inline;
begin
 WriteBarrier;
 addr:=v;
end;

function CAS(Var addr:Pointer;Comp,New:Pointer):Boolean; inline;
begin
 Result:=system.InterlockedCompareExchange(addr,New,Comp)=Comp;
end;

function CAS(Var addr:SizeUInt;Comp,New:SizeUInt):Boolean; inline;
begin
 Result:=system.InterlockedCompareExchange(Pointer(addr),Pointer(New),Pointer(Comp))=Pointer(Comp);
end;

function XCHG(Var addr:Pointer;New:Pointer):Pointer; inline;
begin
 Result:=System.InterLockedExchange(addr,New);
end;

Procedure TIntrusiveStack.Create;
begin
 FillChar(Self,SizeOf(Self),0);
 ReadWriteBarrier;
end;

Function TIntrusiveStack.Push(Node:Pointer):Boolean;
var
 orig:PQNode;
 bkoff:back_off;
begin
 Result:=False;

 if not Assigned(Node) then Exit(False);

 bkoff.Reset;
 repeat
  orig:=load_consume(m_Head);

  store_release(PQNode(Node)^.Next,orig);

  if CAS(m_Head,orig,node) then Break;

  bkoff.Wait;
 until false;

 Result:=True;

end;

Function TIntrusiveStack.Pop(Var Node:Pointer):Boolean;
var
 next,orig:PQNode;
 bkoff:back_off;
begin

 Node:=nil;
 Result:=False;

 bkoff.Reset;
 While (XCHG(m_lock,Pointer(1))<>nil) do bkoff.Wait;

 bkoff.Reset;
 repeat
  orig:=load_consume(m_Head);

  if orig=nil then
  begin
   store_release(m_lock,nil); //unlock
   Exit;
  end;

  next:=orig^.Next;

  if CAS(m_Head,orig,next) then Break;

  bkoff.Wait;
 until false;

 store_release(m_lock,nil); //unlock

 Node:=orig;
 Result:=True;

end;

Function TIntrusiveStack.IsEmpty:Boolean; inline;
begin
 Result:=load_consume(m_Head)=nil;
end;

Procedure TStack.Create; inline;
begin
 Data.Create;
end;

Function TStack.push_front(Const val:TItem):Boolean;
Var
 Node:PNode;
begin
 Node:=Allocator.AllocMem(SizeOf(TNode));
 Node^.Item:=val;
 ReadWriteBarrier;
 Result:=Data.Push(Node);
end;

Function TStack.pop_front(Var val:TItem):Boolean;
Var
 Node:PNode;
begin

 Node:=nil;
 Result:=Data.Pop(Node);
 if Result then
 begin
  val:=Node^.Item;
  Allocator.Freemem(Node);
 end;

end;

end.



