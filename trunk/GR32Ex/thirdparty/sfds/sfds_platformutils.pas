unit sfds_platformutils;
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
{$I sfds.inc}

{*****************************************************************************}
{                                                                             }
{  SFDS (Single File Data Storage) Unit                                       }
{                                                                             }
{  For conditions of distribution and use, see LICENSE.TXT                    }
{                                                                             }
{*****************************************************************************}

interface
uses SysUtils;

const
  PathDelimUNIX = '/';
  PathDelimWindows = '\';
  InvalidWindowsChars = #0+'/:*?"<>|';
  InvalidUNIXChars = #0;  

  {Converts FileName to a valid Windows FileName.<BR>
   <UL>
   <LI>Converts <b>/</b> to <b>\</b> and also removes <b>* ? : " &lt; &gt; |</b></LI>
   <LI>Also removes the NULL character <b>#0</b></LI>
   </UL>
   If it finds two or more \ one after another, it only keeps one.}
  procedure MakeValidWindowsName(var FileName : AnsiString); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  {Converts FileName to a valid UNIX FileName.<BR>
   Converts <b>\</b> to <b>/</b>.<BR>
   Also removes the NULL character <b>#0</b>.
   If it finds two or more / one after another, it only keeps one.}
  procedure MakeValidUNIXName(var FileName : AnsiString); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  {On Windows it uses MakeValidWindowsName.<BR>
   On UNIX like systems it uses MakeValidUNIXName.}
  procedure MakeValidPlatformName(var FileName : AnsiString); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  {Returns True if the FileName is a valid Windows FileName}
  function IsValidWindowsName(var FileName : AnsiString) : boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  {Returns True if the FileName is a valid UNIX FileName}
  function IsValidUNIXName(var FileName : AnsiString) : boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  {Returns IsValidWindowsName on Windows.<BR>
   Returns IsValidUNIXName on UNIX like systems.}
  function IsValidPlatformName(var FileName : AnsiString) : boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

implementation

procedure MakeValidPlatformName(var FileName : AnsiString); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  MakeValidWindowsName(FileName);
{$ELSE}
  MakeValidUNIXName(FileName);
{$ENDIF}
end;

procedure MakeValidWindowsName(var FileName : AnsiString); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var I : Integer;
begin
  for I := Length(FileName) downto 1 do
  begin
    if IsDelimiter(PathDelimUNIX, FileName, I) then FileName[I] := PathDelimWindows;
    if IsDelimiter(InvalidWindowsChars, FileName, I) then Delete(FileName, I, 1);
  end;
  for I := Length(FileName) downto 1 do
    begin
      if IsDelimiter(PathDelimWindows, FileName, I) and
         IsDelimiter(PathDelimWindows, FileName, I-1)
      then Delete(FileName, I, 1);
    end;
end;

procedure MakeValidUNIXName(var FileName : AnsiString); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var I : Integer;
begin
  for I := Length(FileName) downto 1 do
  begin
    if IsDelimiter(PathDelimWindows, FileName, I) then FileName[I] := PathDelimUNIX;
    if IsDelimiter(InvalidUNIXChars, FileName, I) then Delete(FileName, I, 1);
  end;
  for I := Length(FileName) downto 1 do
    begin
      if IsDelimiter(PathDelimUNIX, FileName, I) and
         IsDelimiter(PathDelimUNIX, FileName, I-1)
      then Delete(FileName, I, 1);
    end;
end;

function IsValidPlatformName(var FileName : AnsiString) : boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  result := IsValidWindowsName(FileName);
{$ELSE}
  result := IsValidUNIXName(FileName);
{$ENDIF}
end;

function IsValidWindowsName(var FileName : AnsiString) : boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var I : Integer;
begin
result := True;
  for I := 1 to Length(FileName) do
  begin
    if IsDelimiter(InvalidWindowsChars + PathDelimUNIX, FileName, I) then
        begin
          result := False;
          exit;
        end;
  end;
end;

function IsValidUNIXName(var FileName : AnsiString) : boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var I : Integer;
begin
result := True;
  for I := 1 to Length(FileName) do
  begin
    if IsDelimiter(InvalidUNIXChars + PathDelimWindows, FileName, I) then
        begin
          result := False;
          exit;
        end;
  end;
end;

end.
