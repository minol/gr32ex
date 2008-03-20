unit sfds_searchutils;
{$I sfds.inc}

{*****************************************************************************}
{                                                                             }
{  SFDS (Single File Data Storage) Unit                                       }
{                                                                             }
{  For conditions of distribution and use, see LICENSE.TXT                    }
{                                                                             }
{*****************************************************************************}

interface
uses {$IFNDEF FPC}Windows, {$ENDIF}SysUtils, Classes;

type
  TOnFileFoundProc = procedure (const F : TSearchRec; SourceDir : AnsiString) of object;

{Searches for files on disk.<BR>
case DirMaxDepth of (no effect if recursive is false)<BR>
-1 : unlimited recursive search.<BR>
0  : non recursive. (even if recursive is True)<BR>
X, X>=1 : search in only maximum X subdirectories.}
procedure ListFiles(const Directory, Mask : AnsiString; const Recursive : Boolean; const OnFileFoundProc : TOnFileFoundProc; DirMaxDepth : Smallint = -1);

{Returns True if the Text matches the WildCard.<BR>
 Strings with wildcards(* and ?) separated with commas are allowed.<BR>
 This function is made for SFDS file wilcard match testing (*.* is treated as * and if the filename includes a full path, it also tries to see if only the filename matches) }
function WildcardMatchFile(Text, WildCard : AnsiString) : boolean;
{Returns True if the Text matches the WildCard.<BR>
 Strings with wildcards(* and ?) separated with commas are allowed.}
function WildcardMatch(Text, WildCard : AnsiString) : boolean;
{Returns the FileName from FileAndPath. (platform independent evaluation)<BR>
Ex:<BR>
'c:\Dir\FileName' returns 'FileName'<BR>
'/home/alex/dir/filename' returns 'filename'}
function ExtractFileName(FileAndPath : AnsiString) : AnsiString; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

implementation

{Internal Function}
function ExtractFileName(FileAndPath : AnsiString) : AnsiString; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var I : Integer;
    Pos : Integer;
begin
  Pos := -1;
  for I := Length(FileAndPath) downto 1 do
    begin
      if IsDelimiter('/\', FileAndPath, I) then
        begin
        Pos := I;
        Break;
        end;
    end;
  if Pos = -1 then result := FileAndPath
              else result := Copy(FileAndPath, Pos + 1, Length(FileAndPath) - Pos);
end;

procedure ListFiles(const Directory, Mask : AnsiString; const Recursive : Boolean; const OnFileFoundProc : TOnFileFoundProc; DirMaxDepth : Smallint = -1);
var F : TSearchRec;
    NextDirMaxDepth : SmallInt;
begin
  if FindFirst(IncludeTrailingPathDelimiter(Directory) + '*.*', faAnyFile, F) <> 0 then exit;
  repeat
    if (F.Name = '.') or (F.Name = '..') then continue;
        if ((F.Attr and faDirectory) = faDirectory)
            then
              if Recursive and (DirMaxDepth <> 0)
                 then
                 begin
                 if DirMaxDepth > 0 then NextDirMaxDepth := DirMaxDepth - 1
                                    else NextDirMaxDepth := DirMaxDepth;
                 ListFiles(IncludeTrailingPathDelimiter(Directory) + F.Name, Mask, Recursive, OnFileFoundProc, NextDirMaxDepth)
                 end
                 else begin end
            else
            if WildcardMatch(F.Name, Mask) then
              if Assigned(OnFileFoundProc) then
                 OnFileFoundProc(F, IncludeTrailingPathDelimiter(Directory));
  until FindNext(F) <> 0;
  FindClose(F);
end;

function ExtWCDS(s : AnsiString) : TStrings; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  result := TStringList.Create;
  S := Trim(S); //remove extra spaces fix
  if AnsiPos(';', s) = 0 then begin Result.Add(s); exit; end;
    repeat
    result.Add(Copy(S, 1, AnsiPos(';', S) - 1));
    s := Copy(S, AnsiPos(';', S) + 1, Length(S) - 2);
    if AnsiPos(';', s) = 0 then Result.Add(s);
    until AnsiPos(';', s) = 0;
end;

{$IFDEF FPC}
function CharNext(lpsz: PChar): PChar; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  result := lpsz;
  if lpsz = #0 then exit;
  result := lpsz + 1;
end;
{$ENDIF}

function WildcardMatchP(const Text, Pattern: PChar): Boolean;

  function CharCompare(const S1, S2: PChar): Boolean;
  var
    N, I: Integer;
  begin
    N := CharNext(S1) - S1;
    if N = CharNext(S2) - S2 then begin
      for I := 0 to N-1 do begin
        if S1[I] <> S2[I] then begin
          Result := False;
          Exit;
        end;
      end;
      Result := True;
    end else
      Result := False;
  end;

  function InternalWildcardMatch(Txt, Pat: PChar): Integer;
  begin
    while Pat^ <> #0 do begin
      case Pat^ of
        '?': ;
        '*': begin
               Inc(Pat);
               while Pat^ = '*' do begin
                 Inc(Pat);
               end;
               if Pat^ = #0 then begin
                 Result := 1;
                 Exit;
               end;
               while Txt^ <> #0 do begin
                 Result := InternalWildcardMatch(Txt, Pat);
                 if Result <> 0 then
                   Exit;
                 Txt := CharNext(Txt);
               end;
               Result := 2;
               Exit;
             end;
      else
        if not CharCompare(Txt, Pat) then begin
          Result := 0;
          Exit;
        end;
      end;
      Txt := CharNext(Txt);
      Pat := CharNext(Pat);
    end;
    if Txt^ = #0 then
      Result := 1
    else
      Result := 0;
  end;

begin
  Result := (InternalWildcardMatch(Text, Pattern) = 1);
end;

function WildcardMatchFile(Text, WildCard : AnsiString) : boolean;
var S : TStrings;
    I : Integer;
begin
  result := False;
  S := ExtWCDS(WildCard);
  try
  for I := 0 to S.Count - 1 do
      begin
      if S.Strings[I] = '*.*' then begin result := True; exit; end;
      result := WildcardMatchP(PChar(AnsiLowerCase(Text)), PChar(AnsiLowerCase(S.Strings[I])));
      if not Result then
        result := WildcardMatchP(PChar(AnsiLowerCase(ExtractFileName(Text))), PChar(AnsiLowerCase(S.Strings[I])));
      if result then exit;
      end;
  finally
    S.Free;
  end;
end;

function WildcardMatch(Text, WildCard : AnsiString) : boolean;
var S : TStrings;
    I : Integer;
begin
  result := False;
  S := ExtWCDS(WildCard);
  try
  for I := 0 to S.Count - 1 do
      begin
      if S.Strings[I] = '*' then begin result := True; exit; end;
      result := WildcardMatchP(PChar(AnsiLowerCase(Text)), PChar(AnsiLowerCase(S.Strings[I])));
      if result then exit;
      end;
  finally
    S.Free;
  end;
end;

end.
