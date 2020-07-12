{ File memory mapping interface
   based on https://github.com/spotify/annoy/blob/master/src/mman.h

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

unit fpmmap_win;

{$mode objfpc}{$H+}

interface

uses
 Windows;

Const
 PROT_NONE     = 0;
 PROT_READ     = 1;
 PROT_WRITE    = 2;
 PROT_EXEC     = 4;

 MAP_FILE      = 0;
 MAP_SHARED    = 1;
 MAP_PRIVATE   = 2;
 MAP_TYPE      = $f;
 MAP_FIXED     = $10;
 MAP_ANONYMOUS = $20;
 MAP_ANON      = MAP_ANONYMOUS;

 MAP_FAILED    =Pointer(-1);

 FILE_MAP_EXECUTE=$0020;

 Sys_EBADF   =ERROR_INVALID_HANDLE;
 Sys_EACCES  =ERROR_INVALID_ACCESS;
 Sys_EINVAL  =ERROR_INVALID_PARAMETER;
 Sys_ENOMEM  =ERROR_NOT_ENOUGH_MEMORY;

function fpgeterrno:LongInt; inline;
function Fpmmap(addr:Pointer;len:size_t;prot,flags:LongInt;fd:THandle;offst:size_t):Pointer;
function Fpmunmap(addr:Pointer;len:size_t):Longint; inline;
function fpfsync(fd:THandle):Longint; inline;

implementation

function __map_mmap_prot_page(prot:LongInt):DWORD;
begin
 Result:=0;
 if (prot=PROT_NONE) then Exit;

 if ((prot and PROT_EXEC)<>0) then
 begin
  if (prot and PROT_WRITE)<>0 then
  begin
   Result:=PAGE_EXECUTE_READWRITE;
  end else
  begin
   Result:=PAGE_EXECUTE_READ;
  end;
 end else
 begin
  if (prot and PROT_WRITE)<>0 then
  begin
   Result:=PAGE_READWRITE;
  end else
  begin
   Result:=PAGE_READONLY;
  end;
 end;
end;

function __map_mmap_prot_file(prot:LongInt):DWORD;
begin
 Result:= 0;
 if (prot=PROT_NONE) then Exit;
 if (prot and PROT_READ) <>0 then Result:=Result or FILE_MAP_READ;
 if (prot and PROT_WRITE)<>0 then Result:=Result or FILE_MAP_WRITE;
 if (prot and PROT_EXEC) <>0 then Result:=Result or FILE_MAP_EXECUTE;
end;

function fpgeterrno:LongInt; inline;
begin
 Result:=GetLastError;
end;

function MapViewOfFileEx(hFileMappingObject:HANDLE;
                         dwDesiredAccess:DWORD;
                         dwFileOffsetHigh:DWORD;
                         dwFileOffsetLow:DWORD;
                         dwNumberOfBytesToMap:SIZE_T;
                         lpBaseAddress:LPVOID):LPVOID; stdcall; external 'kernel32' name 'MapViewOfFileEx';

function Fpmmap(addr:Pointer;len:size_t;prot,flags:LongInt;fd:THandle;offst:size_t):Pointer;
Var
 fm,h:THandle;
 map:Pointer;
 dwFileOffsetLow,dwFileOffsetHigh,protect,desiredAccess,dwMaxSizeLow,dwMaxSizeHigh:DWORD;
 maxSize:size_t;
begin
 map:=MAP_FAILED;

 maxSize:=offst+len;

 {$IF (sizeof(size_t)<=sizeof(DWORD))}
 dwFileOffsetLow :=DWORD(offst);
 dwFileOffsetHigh:=0;
 dwMaxSizeLow    :=DWORD(maxSize);
 dwMaxSizeHigh   :=0;
 {$ELSE}
 dwFileOffsetLow :=DWORD(offst and $FFFFFFFF);
 dwFileOffsetHigh:=DWORD(offst shr 32);
 dwMaxSizeLow    :=DWORD(maxSize and $FFFFFFFF);
 dwMaxSizeHigh   :=DWORD(maxSize shr 32);
 {$ENDIF}

 protect      :=__map_mmap_prot_page(prot);
 desiredAccess:=__map_mmap_prot_file(prot);

 SetLastError(0);

 if (len=0) or
    // Unsupported flag combinations
    ((flags and MAP_FIXED)<>0) then
    // Usupported protection combinations
    //(prot=PROT_EXEC) then
 begin
  SetLastError(Sys_EINVAL);
  Result:=MAP_FAILED;
  Exit;
 end;

 if ((flags and MAP_ANONYMOUS)=0) then
 begin
  if (fd=INVALID_HANDLE_VALUE) then
  begin
   SetLastError(Sys_EBADF);
   Result:=MAP_FAILED;
   Exit;
  end;
  h:=fd;
 end else
 begin
  h:=INVALID_HANDLE_VALUE;
 end;

 fm:=CreateFileMapping(h,nil,protect,dwMaxSizeHigh,dwMaxSizeLow,nil);
 if (fm=0) then
 begin
  Result:=MAP_FAILED;
  Exit;
 end;

 map:=MapViewOfFileEx(fm,desiredAccess,dwFileOffsetHigh,dwFileOffsetLow,len,addr);

 CloseHandle(fm);

 if (map=nil) then
 begin
  Result:=MAP_FAILED;
  Exit;
 end;

 Result:=map;
end;


function Fpmunmap(addr:Pointer;len:size_t):Longint; inline;
begin
 if (UnmapViewOfFile(addr))then
 begin
  Result:=0;
 end else
 begin
  Result:=-1;
 end;
end;

function fpfsync(fd:THandle):Longint; inline;
begin
 if (FlushFileBuffers(fd))then
 begin
  Result:=0;
 end else
 begin
  Result:=-1;
 end;
end;

end.

