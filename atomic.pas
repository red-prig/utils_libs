{ atomic utils

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

unit atomic;

{$mode objfpc}{$H+}

interface

Const
 CacheLineSize=64;

function  load_consume(Var addr:Pointer):Pointer; inline;
function  load_consume(Var addr:SizeUInt):SizeUInt; inline;
function  load_acquire(Var addr:Pointer):Pointer; inline;
function  load_acquire(Var addr:SizeUInt):SizeUInt; inline;
function  load_acq_rel(Var addr:Pointer):Pointer; inline;
function  load_acq_rel(Var addr:SizeUInt):SizeUInt; inline;
Procedure store_release(Var addr:Pointer;v:Pointer); inline;
Procedure store_release(Var addr:SizeUInt;v:SizeUInt); inline;
Procedure store_seq_cst(Var addr:Pointer;v:Pointer); inline;
Procedure store_seq_cst(Var addr:SizeUInt;v:SizeUInt); inline;
function  CAS(Var addr:Pointer;Comp,New:Pointer):Boolean; inline;
function  CAS(Var addr:SizeUInt;Comp,New:SizeUInt):Boolean; inline;
function  XCHG(Var addr:Pointer;New:Pointer):Pointer; inline;
function  XCHG(Var addr:SizeUInt;New:SizeUInt):SizeUInt; inline;
function  fetch_add(Var addr:SizeUInt;i:SizeUInt):SizeUInt; inline;
function  fetch_sub(Var addr:SizeUInt;i:SizeUInt):SizeUInt; inline;
Procedure fetch_xor(var Target:SizeUInt;mask:SizeUInt);
Procedure fetch_or(var Target:SizeUInt;mask:SizeUInt);
Procedure fetch_and(var Target:SizeUInt;mask:SizeUInt);
function  test_and_set(var Target:SizeUInt;mask:SizeUInt):SizeUInt;
function  test_and_clear(var Target:SizeUInt;mask:SizeUInt):SizeUInt;
function  marked_ptr(P:Pointer;B:SizeUInt=0):Pointer; inline;
function  ptr1(P:Pointer):Pointer; inline;
function  bits1(P:Pointer):SizeUInt; inline;
function  bits1(P:SizeUInt):SizeUInt; inline;
function  spin_trylock(Var P:Pointer):Boolean; inline;
function  spin_tryunlock(Var P:Pointer):Boolean; inline;
procedure spin_lock(Var P:Pointer); inline;
procedure spin_unlock(Var P:Pointer); inline;

type
 TfuncFree=Function(p:pointer):SizeUInt;
 Tretired_ptr=object
  m_p:Pointer;
  m_funcFree:TfuncFree;
  Procedure Free; inline;
 end;

 backoff_exp=object
  private
   Const
    lower_bound = 16;        ///< Minimum spinning limit
    upper_bound = 16*1024;   ///< Maximum spinning limit
   Var
    m_nExpCur:SizeUInt;      //=lower_bound
  public
   Procedure Wait;
   Procedure Reset;
 end;

function  retired_ptr(p:Pointer;funcFree:TfuncFree):Tretired_ptr; inline;

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

function load_acquire(Var addr:SizeUInt):SizeUInt; inline;
begin
 ReadBarrier;
 Result:=addr;
end;

function load_acq_rel(Var addr:Pointer):Pointer; inline;
begin
 Result:=System.InterLockedExchangeAdd(Pointer(addr),nil);
end;

function load_acq_rel(Var addr:SizeUInt):SizeUInt; inline;
begin
 Result:=SizeUInt(load_acq_rel(Pointer(addr)));
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

Procedure store_seq_cst(Var addr:Pointer;v:Pointer); inline;
begin
 System.InterLockedExchange(addr,v);
end;

Procedure store_seq_cst(Var addr:SizeUInt;v:SizeUInt); inline;
begin
 store_seq_cst(Pointer(addr),Pointer(v));
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

function XCHG(Var addr:SizeUInt;New:SizeUInt):SizeUInt; inline;
begin
 Result:=SizeUInt(System.InterLockedExchange(Pointer(addr),Pointer(New)));
end;

function fetch_add(Var addr:SizeUInt;i:SizeUInt):SizeUInt; inline;
begin
 Result:=SizeUInt(System.InterLockedExchangeAdd(Pointer(addr),Pointer(i)));
end;

function fetch_sub(Var addr:SizeUInt;i:SizeUInt):SizeUInt; inline;
begin
 Result:=fetch_add(addr,SizeUInt(-SizeInt(i)));
end;

{$push}
{$ASMMODE Intel}

//xor

function _fetch_xor(var Target:SizeUInt;mask:SizeUInt):SizeUInt; inline;
Var
 P,N:SizeUInt;
begin
 repeat
  P:=Target;
  N:=P xor mask;
 until CAS(Target,P,N);
 Result:=P;
end;

{$IFDEF CPU386}
Procedure fetch_xor(var Target:SizeUInt;mask:SizeUInt); assembler;
asm
 lock xor [eax],edx
end;
{$ELSE}
{$IFDEF CPUX86_64}
Procedure fetch_xor(var Target:SizeUInt;mask:SizeUInt); assembler;
asm
 lock xor [rcx],rdx
end;
{$ELSE}
Procedure fetch_xor(var Target:SizeUInt;mask:SizeUInt); inline;
begin
 _fetch_xor(Target,mask);
end;
{$ENDIF}
{$ENDIF}

//or

function _fetch_or(var Target:SizeUInt;mask:SizeUInt):SizeUInt; inline;
Var
 P,N:SizeUInt;
begin
 repeat
  P:=Target;
  N:=P or mask;
 until CAS(Target,P,N);
 Result:=P;
end;

{$IFDEF CPU386}
Procedure fetch_or(var Target:SizeUInt;mask:SizeUInt); assembler;
asm
 lock xor [eax],edx
end;
{$ELSE}
{$IFDEF CPUX86_64}
Procedure fetch_or(var Target:SizeUInt;mask:SizeUInt); assembler;
asm
 lock xor [rcx],rdx
end;
{$ELSE}
Procedure fetch_or(var Target:SizeUInt;mask:SizeUInt); inline;
begin
 _fetch_or(Target,mask);
end;
{$ENDIF}
{$ENDIF}

//and

function _fetch_and(var Target:SizeUInt;mask:SizeUInt):SizeUInt; inline;
Var
 P,N:SizeUInt;
begin
 repeat
  P:=Target;
  N:=P and mask;
 until CAS(Target,P,N);
 Result:=P;
end;

{$IFDEF CPU386}
Procedure fetch_and(var Target:SizeUInt;mask:SizeUInt); assembler;
asm
 lock and [eax],edx
end;
{$ELSE}
{$IFDEF CPUX86_64}
Procedure fetch_and(var Target:SizeUInt;mask:SizeUInt); assembler;
asm
 lock and [rcx],rdx
end;
{$ELSE}
Procedure fetch_and(var Target:SizeUInt;mask:SizeUInt); inline;
begin
 _fetch_and(Target,mask);
end;
{$ENDIF}
{$ENDIF}

{$pop}

function test_and_set(var Target:SizeUInt;mask:SizeUInt):SizeUInt;
Var
 P,N:SizeUInt;
begin
 repeat
  P:=Target;
  Result:=(P and mask);
  //if (Result=mask) then Exit;
  N:=P or mask;
 until CAS(Target,P,N);
end;

function test_and_clear(var Target:SizeUInt;mask:SizeUInt):SizeUInt;
Var
 P,N:SizeUInt;
begin
 mask:=not mask;
 repeat
  P:=Target;
  Result:=(P and mask);
  //if (Result=0) then Exit;
  N:=P and mask;
 until CAS(Target,P,N);
end;

function marked_ptr(P:Pointer;B:SizeUInt=0):Pointer; inline;
begin
 Result:=Pointer(SizeUInt(P) or B);
end;

function ptr1(P:Pointer):Pointer; inline;
begin
 Result:=Pointer(SizeUInt(P) and (not SizeUInt(1)));
end;

function bits1(P:Pointer):SizeUInt; inline;
begin
 Result:=SizeUInt(P) and SizeUInt(1);
end;

function bits1(P:SizeUInt):SizeUInt; inline;
begin
 Result:=SizeUInt(P) and SizeUInt(1);
end;

{$push}
{$ASMMODE Intel}
{$IFDEF CPU386 or CPUX86_64}
procedure spin_pause; assembler;
asm
 pause
end;
{$ELSE}
procedure spin_pause; inline;
begin
end;
{$ENDIF}
{$pop}

function spin_trylock(Var P:Pointer):Boolean; inline;
begin
 Result:=XCHG(P,Pointer(1))=nil;
end;

function spin_tryunlock(Var P:Pointer):Boolean; inline;
begin
 Result:=XCHG(P,nil)=Pointer(1);
end;

procedure spin_lock(Var P:Pointer); inline;
Var
 bkoff:backoff_exp;
begin
 bkoff.Reset;
 //while not CAS(P,nil,Pointer(1)) do system.ThreadSwitch;
 While (XCHG(P,Pointer(1))<>nil) do bkoff.Wait;
end;

procedure spin_unlock(Var P:Pointer); inline;
begin
 store_release(P,nil);
end;

Procedure backoff_exp.Wait;
Var
 n:SizeUInt;
begin
 if (m_nExpCur<=upper_bound) then
 begin
  For n:=0 to m_nExpCur-1 do
  begin
   spin_pause;
  end;
  m_nExpCur:=m_nExpCur*2;
 end else
 begin
  System.ThreadSwitch;
 end;
end;

Procedure backoff_exp.Reset;
begin
 m_nExpCur:=lower_bound;
end;

Procedure Tretired_ptr.Free; inline;
begin
 if Assigned(m_funcFree) then
  m_funcFree(m_p);
end;

function retired_ptr(p:Pointer;funcFree:TfuncFree):Tretired_ptr; inline;
begin
 Result.m_p       :=p;
 Result.m_funcFree:=funcFree;
end;

end.

