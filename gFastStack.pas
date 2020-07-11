{ Generic template of fast stack implementation

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

unit gFastStack;

{$mode objfpc}{$H+}

interface

type
 generic TFastStack<TKey>=object
  private
   type
    PDefNode=^TDefNode;
    TDefNode=object
     Next,Prev:PDefNode;
     A:TKey;
    end;
   var
    Root,Node:PDefNode;
  public
   type
    PKey=^TKey;
    Iterator=object
     private
      Node:PDefNode;
     public
      function Key:PKey; inline;
      function Prev:Boolean; inline;
    end;
   function   cend:Iterator; inline;
   function   Top:PKey; inline;
   Procedure  Push(const A:TKey);
   function   Pop:Boolean;
   Procedure  Reset; inline;
   Procedure  Free;
 end;

implementation

function TFastStack.Iterator.Key:PKey; inline;
begin
 if (Node<>nil) then
  Result:=@Node^.A
 else
  Result:=nil;
end;

function TFastStack.Iterator.Prev:Boolean; inline;
begin
 Result:=Assigned(Node) and Assigned(Node^.Prev);
 if Result then Node:=Node^.Prev;
end;

function TFastStack.cend:Iterator; inline;
begin
 Result.Node:=Node;
end;

function TFastStack.Top:PKey; inline;
begin
 if (Node<>nil) then
  Result:=@Node^.A
 else
  Result:=nil;
end;

Procedure TFastStack.Push(const A:TKey);
Var
 New:PDefNode;
begin
 if (Node=nil) then
 begin
  if (Root=nil) then
  begin
   Root:=AllocMem(SizeOf(TDefNode));
  end;
  Node:=Root;
 end else
 if (Node^.Next<>nil) then
 begin
  Node:=Node^.Next;
 end else
 begin
  New:=AllocMem(SizeOf(TDefNode));
  Node^.Next:=New;
  New^.Prev:=Node;
  Node:=New;
 end;
 Node^.A:=A;
end;

function TFastStack.Pop:Boolean;
begin
 Result:=Assigned(Node);
 if Result then
 begin
  Finalize(Node^.A);
  Node:=Node^.Prev;
 end;
end;

Procedure TFastStack.Reset; inline;
begin
 Node:=nil;
end;

Procedure TFastStack.Free;
Var
 Old:PDefNode;
begin
 Node:=nil;
 While (Root<>nil) do
 begin
  Old:=Root;
  Root:=Root^.Next;
  Finalize(Old^.A);
  FreeMem(Old);
 end;
end;

end.

