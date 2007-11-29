{ Summary only for Windows platform. }
unit GR_WinAPI;

interface

uses
  Windows, Messages,
  SysUtils, Classes;

function IsMsImg: boolean;

function IsWin2k: boolean;

function IsWinNT: boolean;

function IsWinNT4: boolean;

//type
//MMWIN:STARTINTERFACE

var
  TransparentBltFunc: function (hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest,
    nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc,
    nHeightSrc: Integer; Color: Longint): BOOL; stdcall;
  SetLayeredWindowAttributes: function (hwnd: HWND; crKey: COLORREF; bAlpha: BYTE;
    dwFlags: DWORD): BOOL; stdcall;
  UpdateLayeredWindow: function (hWnd: HWND; hdcDst: HDC; pptDst: PPOINT;
    psize: PSIZE; hdcSrc: HDC; pptSrc: PPOINT; crKey: COLORREF;
    pblend: PBlendFunction; dwFlags: DWORD): BOOL; stdcall;

implementation

const
  MsImg32 = 'msimg32.dll';

var
  MsImg32Lib: THandle = 0;
  MsImg32Available: boolean = false;

function IsMsImg: boolean;
begin
  Result := MsImg32Available;
end;

procedure SetupMsImg;
begin
  //if not IsWinNT4 and (IsWinNT or IsWin2k or IsWinXP) then
  begin
    MsImg32Lib := LoadLibrary(MsImg32);
    if MsImg32Lib <> 0 then
    begin
      TransparentBltFunc := GetProcAddress(MsImg32Lib, 'TransparentBlt');
      if Assigned(TransparentBltFunc) then
        MsImg32Available := true;
    end;
  end;
end;

const
  User32 = 'user32.dll';

var
  User32Lib: THandle = 0;
  RunOnWin2K: boolean = false;

function IsWin2k: boolean;
begin
  Result := RunOnWin2K;
end;

procedure SetupWin2k;
begin
  User32Lib := LoadLibrary(User32);
  if User32Lib <> 0 then
  begin
    @SetLayeredWindowAttributes := GetProcAddress(User32Lib, 'SetLayeredWindowAttributes');
    @UpdateLayeredWindow := GetProcAddress(User32Lib, 'UpdateLayeredWindow');

    if @SetLayeredWindowAttributes <> nil then
      RunOnWin2k := true;
  end;
end;



var
  RunOnWinNT: boolean = false;
  RunOnWinNT4: boolean = false;

function IsWinNT: boolean;
begin
  Result := RunOnWinNT;
end;

function IsWinNT4: boolean;
begin
  Result := RunOnWinNT4;
end;

procedure SetupWinNT;
var
  VI: TOSVersionInfo;
begin
  VI.dwOSVersionInfoSize := SizeOf(VI);
  GetVersionEx(VI);

  RunOnWinNT := VI.dwPlatformId = VER_PLATFORM_WIN32_NT;
  RunOnWinNT4 := VI.dwMajorVersion = 4; 
end;

//MMWIN:STARTIMPLEMENTATION

initialization
  SetupWinNT;
  SetupWin2k;
  SetupMsImg;
finalization
  if MsImg32Lib <> 0 then FreeLibrary(MsImg32Lib);
  if User32Lib <> 0 then FreeLibrary(User32Lib);
end.
