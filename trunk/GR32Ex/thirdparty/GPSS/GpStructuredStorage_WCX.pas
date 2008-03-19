unit GpStructuredStorage_WCX;

{.$DEFINE DebugLog}

interface

uses
  wcxhead;

function  OpenArchive(var ArchiveData: TOpenArchiveData): THandle; stdcall;
function  ReadHeader(hArcData: THandle; var HeaderData: THeaderData): integer; stdcall;
function  ProcessFile(hArcData: THandle; Operation: integer; DestPath, DestName: PChar):
  integer; stdcall;
function  CloseArchive(hArcData: THandle): integer; stdcall;
procedure SetChangeVolProc(hArcData: THandle; pChangeVolProc: TChangeVolProc); stdcall;
procedure SetProcessDataProc(hArcData: THandle; pProcessDataProc: TProcessDataProc);
  stdcall;
function  GetPackerCaps: integer; stdcall;
function  PackFiles(PackedFile, SubPath, SrcPath, AddList: PChar; Flags: integer):
  integer; stdcall;
function  DeleteFiles(PackedFile, DeleteList: PChar): integer; stdcall;
function  CanYouHandleThisFile(FileName: PChar): boolean; stdcall;

implementation

uses
  SysUtils,
  Classes,
  {$IFDEF DebugLog}
  GpLogger,
  {$ENDIF DebugLog}
  GpLists,
  GpStructuredStorage;

type
  TGpStructuredStorageManipulator = class
  private
    FArchiveName    : string;
    FCurrentFile    : string;
    FCurrentIsFolder: boolean;
    FFileIdx        : integer;
    FFiles          : TStringList;
    FFolderIdx      : integer;
    FFolders        : TStringList;
    FStorage        : IGpStructuredStorage;
  protected
    procedure LoadHeaders;
    procedure ResetEnumerator;
  public
    constructor Create(const archiveName: string);
    destructor  Destroy; override;
    procedure Close;
    function  ExtractCurrentTo(const outFile: TStream): integer;
    function  GetNextHeader(var headerData: THeaderData): boolean;
    function  Open: integer;
    property ArchiveName: string read FArchiveName;
  end; { TGpStructuredStorageManipulator }

  TGpStructuredStorageManipulatorList = class
  private
    FLastHandle     : integer;
    FManipulatorList: TGpIntegerObjectList;
  protected
    function  Count: integer;
    function  GenerateHandle: integer;
    function  GetManipulator(idxManipulator: integer): TGpStructuredStorageManipulator;
    property Manipulator[idxManipulator: integer]: TGpStructuredStorageManipulator read
      GetManipulator;
  public
    constructor Create;
    destructor  Destroy; override;
    function  CloseArchive(archive: THandle): integer;
    function  LocateHandle(arcHandle: THandle): TGpStructuredStorageManipulator;
    function  OpenArchive(const archiveName: string; var openResult: integer): THandle;
  end; { TGpStructuredStorageManipulatorList }

var
  GStructuredStorageList: TGpStructuredStorageManipulatorList;

{ globals }

{$IFDEF DebugLog}
function Logger: IGpLogger;
begin
  Result := CreateGpLogger('c:\0\GpStructuredStorageWCX.log');
end; { Logger }
{$ENDIF DebugLog}

{ exports }

function OpenArchive(var ArchiveData: TOpenArchiveData): THandle;
begin
  {$IFDEF DebugLog}
  Result := 0; // to keep Delphi happy
  Logger.Log('> OpenArchive(%s)', [ArchiveData.ArcName]);
  try try
  {$ENDIF DebugLog}

  Result := GStructuredStorageList.OpenArchive(ArchiveData.ArcName, ArchiveData.OpenResult);
  ArchiveData.CmtBuf := nil;
  ArchiveData.CmtBufSize := 0;
  ArchiveData.CmtSize := 0;
  ArchiveData.CmtState := 0;

  {$IFDEF DebugLog}
  except on E: Exception do Logger.Log('*** OpenArchive: %s', [E.Message]); end;
  finally Logger.Log('< OpenArchive: %d', [Result]); end;
  {$ENDIF DebugLog}
end; { OpenArchive }

function ReadHeader(hArcData: THandle; var HeaderData: THeaderData): integer; stdcall;
var
  manip: TGpStructuredStorageManipulator;
begin
  {$IFDEF DebugLog}
  Result := E_NOT_SUPPORTED; // to keep Delphi happy
  Logger.Log('> ReadHeader(%d)', [hArcData]);
  try try
  {$ENDIF DebugLog}

  manip := GStructuredStorageList.LocateHandle(hArcData);
  if not assigned(manip) then
    Result := E_NOT_SUPPORTED
  else if manip.GetNextHeader(HeaderData) then
    Result := 0
  else
    Result := E_END_ARCHIVE;

  {$IFDEF DebugLog}
  except on E: Exception do Logger.Log('*** ReadHeader: %s', [E.Message]); end;
  finally Logger.Log('< ReadHeader: %d', [Result]); end;
  {$ENDIF DebugLog}
end; { ReadHeader }

function ProcessFile(hArcData: THandle; Operation: integer; DestPath, DestName: PChar):
  integer; stdcall;
var
  manip  : TGpStructuredStorageManipulator;
  outFile: TFileStream;
begin
  {$IFDEF DebugLog}
  Result := E_NOT_SUPPORTED; // to keep Delphi happy
  Logger.Log('> ProcessFile(%d, %d, %s, %s)', [hArcData, Operation, string(DestPath),
    string(DestName)]);
  try try
  {$ENDIF DebugLog}

  if (Operation = PK_SKIP) or (Operation = PK_TEST) then
    Result := 0
  else begin
    manip := GStructuredStorageList.LocateHandle(hArcData);
    if not assigned(manip) then
      Result := E_NOT_SUPPORTED
    else begin
      try
        outFile := TFileStream.Create(string(DestPath) + string(DestName), fmCreate);
        try
          Result := manip.ExtractCurrentTo(outFile);
        finally FreeAndNil(outFile); end;
      except
        on E: EFCreateError do
          Result := E_ECREATE;
      end;
    end;
  end;

  {$IFDEF DebugLog}
  except on E: Exception do Logger.Log('*** ProcessFile: %s', [E.Message]); end;
  finally Logger.Log('< ProcessFile: %d', [Result]); end;
  {$ENDIF DebugLog}
end; { ProcessFile }

function CloseArchive(hArcData: THandle): integer; stdcall;
begin
  {$IFDEF DebugLog}
  Result := E_NOT_SUPPORTED; // to keep Delphi happy
  Logger.Log('> CloseArchive(%d)', [hArcData]);
  try try
  {$ENDIF DebugLog}

  Result := GStructuredStorageList.CloseArchive(hArcData);

  {$IFDEF DebugLog}
  except on E: Exception do Logger.Log('*** CloseArchive: %s', [E.Message]); end;
  finally Logger.Log('< CloseArchive: %d', [Result]); end;
  {$ENDIF DebugLog}
end; { CloseArchive }

procedure SetChangeVolProc(hArcData: THandle; pChangeVolProc: TChangeVolProc);
begin
  {$IFDEF DebugLog}
  Logger.Log('> SetChangeVolProc(%d)', [hArcData]);
  try try
  {$ENDIF DebugLog}
  // currently unused
  {$IFDEF DebugLog}
  except on E: Exception do Logger.Log('*** SetChangeVolProc: %s', [E.Message]); end;
  finally Logger.Log('< SetChangeVolProc'); end;
  {$ENDIF DebugLog}
end; { SetChangeVolProc }

procedure SetProcessDataProc(hArcData: THandle; pProcessDataProc: TProcessDataProc);
  stdcall;
begin
  {$IFDEF DebugLog}
  Logger.Log('> SetProcessDataProc(%d)', [hArcData]);
  try try
  {$ENDIF DebugLog}
  // currently unused
  {$IFDEF DebugLog}
  except on E: Exception do Logger.Log('*** SetProcessDataProc: %s', [E.Message]); end;
  finally Logger.Log('< SetProcessDataProc'); end;
  {$ENDIF DebugLog}
end; { SeTProcessDataProc }

function GetPackerCaps: integer;
begin
  {$IFDEF DebugLog}
  Result := E_NOT_SUPPORTED; // to keep Delphi happy
  Logger.Log('> GetPackerCaps');
  try try
  {$ENDIF DebugLog}

  // TODO 1 -oPrimoz Gabrijelcic : implement: GetPackerCaps
  Result := {PK_CAPS_NEW	+ PK_CAPS_MODIFY + PK_CAPS_MULTIPLE	+ PK_CAPS_DELETE +}
    PK_CAPS_BY_CONTENT + PK_CAPS_SEARCHTEXT;
    
  {$IFDEF DebugLog}
  except on E: Exception do Logger.Log('*** GetPackerCaps: %s', [E.Message]); end;
  finally Logger.Log('< GetPackerCaps: %d', [Result]); end;
  {$ENDIF DebugLog}
end; { GetPackerCaps }

function PackFiles(PackedFile, SubPath, SrcPath, AddList: PChar; Flags: integer):
  integer; stdcall;
begin
  {$IFDEF DebugLog}
  Result := E_NOT_SUPPORTED; // to keep Delphi happy
  Logger.Log('> PackFiles(%s, %s, %s, %s, %d)', [PackedFile, SubPath, SrcPath, AddList, Flags]);
  try try
  {$ENDIF DebugLog}
  // TODO -cMM: PackFiles default body inserted
  Result := 0;
  {$IFDEF DebugLog}
  except on E: Exception do Logger.Log('*** PackFiles: %s', [E.Message]); end;
  finally Logger.Log('< PackFiles'); end;
  {$ENDIF DebugLog}
end; { PackFiles }

function DeleteFiles(PackedFile, DeleteList: PChar): integer; stdcall;
begin
  {$IFDEF DebugLog}
  Result := E_NOT_SUPPORTED; // to keep Delphi happy
  Logger.Log('> DeleteFiles(%s, %s)', [PackedFile, DeleteList]);
  try try
  {$ENDIF DebugLog}
  // TODO -cMM: DeleteFiles default body inserted
  Result := 0;
  {$IFDEF DebugLog}
  except on E: Exception do Logger.Log('*** DeleteFiles: %s', [E.Message]); end;
  finally Logger.Log('< DeleteFiles'); end;
  {$ENDIF DebugLog}
end; { DeleteFiles }

function CanYouHandleThisFile(FileName: PChar): boolean; stdcall;
begin
  {$IFDEF DebugLog}
  Result := false; // to keep Delphi happy
  Logger.Log('> CanYouHandleThisFile(%s)', [FileName]);
  try try
  {$ENDIF DebugLog}

  Result := CreateStructuredStorage.IsStructuredStorage(FileName);
  
  {$IFDEF DebugLog}
  except on E: Exception do Logger.Log('*** CanYouHandleThisFile: %s', [E.Message]); end;
  finally Logger.Log('< CanYouHandleThisFile: %d', [Ord(Result)]); end;
  {$ENDIF DebugLog}
end; { CanYouHandleThisFile }

{ TGpStructuredStorageManipulator }

constructor TGpStructuredStorageManipulator.Create(const archiveName: string);
begin
  FArchiveName := archiveName;
  FFolders := TStringList.Create;
  FFiles := TStringList.Create;
end; { TGpStructuredStorageManipulator.Create }

destructor TGpStructuredStorageManipulator.Destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FFolders);
  Close;
end; { TGpStructuredStorageManipulator.Destroy }

procedure TGpStructuredStorageManipulator.Close;
begin
  FStorage := nil;
end; { TGpStructuredStorageManipulator.Close }

function TGpStructuredStorageManipulator.ExtractCurrentTo(const outFile: TStream):
  integer;
var
  stgFile: TStream;
begin
  if FCurrentFile = '' then
    Result := E_NO_FILES
  else if FCurrentIsFolder then
    Result := E_NOT_SUPPORTED // Total Commander 6.51 never calls Extract when current header is a folder
  else begin
    try
      stgFile := FStorage.OpenFile(PathDelim + FCurrentFile, fmOpenRead);
      try
        outFile.CopyFrom(stgFile, 0);
      finally FreeAndNil(stgFile); end;
      Result := 0;
    except
      on E: EGpStructuredStorage do
        Result := E_NO_FILES;
    end;
  end;
end; { TGpStructuredStorageManipulator.ExtractCurrentTo }

function TGpStructuredStorageManipulator.GetNextHeader(var headerData: THeaderData):
  boolean;
begin
  if FFolderIdx >= FFolders.Count then begin
    ResetEnumerator;
    Result := false;
  end
  else begin
    FillChar(headerData, SizeOf(headerData), 0);
    StrPLCopy(headerData.ArcName, FStorage.DataFile, SizeOf(headerData.ArcName)-1);
    if FFileIdx = -1 then begin
      FCurrentIsFolder := true;
      FCurrentFile := ExcludeTrailingPathDelimiter(StringReplace(FFolders[FFolderIdx],
        CFolderDelim, PathDelim, [rfReplaceAll]));
      Delete(FCurrentFile, 1, 1);
      StrPLCopy(headerData.FileName, FCurrentFile, SizeOf(headerData.FileName)-1);
      headerData.FileAttr := $10;
      FStorage.FileNames(FFolders[FFolderIdx], FFiles);
      FFileIdx := 0;
    end
    else if FFileIdx < FFiles.Count then begin
      FCurrentIsFolder := false;
      FCurrentFile := FFolders[FFolderIdx]+FFiles[FFileIdx];
      headerData.PackSize := FStorage.FileInfo[FCurrentFile].Size;
      headerData.UnpSize := headerData.PackSize;
      FCurrentFile := StringReplace(FCurrentFile, CFolderDelim, PathDelim, [rfReplaceAll]);    
      Delete(FCurrentFile, 1, 1);
      StrPLCopy(headerData.FileName, FCurrentFile, SizeOf(headerData.FileName)-1);
      Inc(FFileIdx);
    end;
    if FFileIdx >= FFiles.Count then begin
      FFileIdx := -1;
      Inc(FFolderIdx);
    end;
    Result := true;
  end;
end; { TGpStructuredStorageManipulator.GetNextHeader }

procedure TGpStructuredStorageManipulator.LoadHeaders;

  procedure RecurseInto(const folderName: string);
  var
    folders: TStringList;
    iFolder: integer;
  begin
    FFolders.Add(folderName);
    folders := TStringList.Create;
    try
      FStorage.FolderNames(folderName, folders);
      for iFolder := 0 to folders.Count - 1 do
        RecurseInto(folderName + folders[iFolder] + '/');
    finally FreeAndNil(folders); end;
  end; { RecurseInto }

begin { TGpStructuredStorageManipulator.LoadHeaders }
  FFolders.Clear;
  RecurseInto('/');
  ResetEnumerator;
end; { TGpStructuredStorageManipulator.LoadHeaders }

function TGpStructuredStorageManipulator.Open: integer;
begin
  FStorage := CreateStructuredStorage;
  try
    FStorage.Initialize(ArchiveName, fmOpenRead);
    LoadHeaders;
    Result := 0;
  except
    on E: EGpStructuredStorage do begin
      FStorage := nil;
      Result := E_UNKNOWN_FORMAT;
    end;
  end;
end; { TGpStructuredStorageManipulator.Open }

procedure TGpStructuredStorageManipulator.ResetEnumerator;
begin
  FCurrentFile := '';
  FFolderIdx := 0;
  FFileIdx := 0;
  FStorage.FileNames('/', FFiles);
  if FFiles.Count = 0 then begin // no files in the root folder; prepare first subfolder to be returned
    FFolderIdx := 1;
    FFileIdx := -1;
  end;
end; { TGpStructuredStorageManipulator.ResetEnumerator }

{ TGpStructuredStorageManipulatorList }

constructor TGpStructuredStorageManipulatorList.Create;
begin
  FManipulatorList := TGpIntegerObjectList.Create;
end; { TGpStructuredStorageManipulatorList.Create }

destructor TGpStructuredStorageManipulatorList.Destroy;
begin
  FreeAndNil(FManipulatorList);
end; { TGpStructuredStorageManipulatorList.Destroy }

function TGpStructuredStorageManipulatorList.CloseArchive(archive: THandle): integer;
var
  idxManip: integer;
begin
  idxManip := FManipulatorList.IndexOf(archive);
  if idxManip < 0 then
    Result := E_NOT_SUPPORTED
  else begin
    FManipulatorList.Delete(idxManip);
    Result := 0;
  end;
end; { TGpStructuredStorageManipulatorList.CloseArchive }

function TGpStructuredStorageManipulatorList.Count: integer;
begin
  Result := FManipulatorList.Count;
end; { TGpStructuredStorageManipulatorList.Count }

function TGpStructuredStorageManipulatorList.GenerateHandle: integer;
begin
  Inc(FLastHandle);
  Result := FLastHandle;
end; { TGpStructuredStorageManipulatorList.GenerateHandle }

function TGpStructuredStorageManipulatorList.GetManipulator(idxManipulator: integer):
  TGpStructuredStorageManipulator;
begin
  Result := TGpStructuredStorageManipulator(FManipulatorList.Objects[idxManipulator]);
end; { TGpStructuredStorageManipulatorList.GetManipulator }

function TGpStructuredStorageManipulatorList.LocateHandle(arcHandle: THandle):
  TGpStructuredStorageManipulator;
var
  iManipulator: integer;
begin
  for iManipulator := 0 to Count - 1 do
    if THandle(FManipulatorList[iManipulator]) = arcHandle then begin
      Result := Manipulator[iManipulator];
      Exit;
    end;
  Result := nil;
end; { TGpStructuredStorageManipulatorList.LocateHandle }

function TGpStructuredStorageManipulatorList.OpenArchive(const archiveName: string; var
  openResult: integer): THandle;
var
  manip: TGpStructuredStorageManipulator;
begin
  manip := TGpStructuredStorageManipulator.Create(archiveName);
  openResult := manip.Open;
  if openResult <> 0 then begin
    Result := 0;
    FreeAndNil(manip);
  end
  else begin
    Result := GenerateHandle;
    FManipulatorList.AddObject(Result, manip);
  end;
end; { TGpStructuredStorageManipulatorList.OpenArchive }

initialization
  {$IFDEF DebugLog}
  Logger.Log('> initialization');
  {$ENDIF DebugLog}

  GStructuredStorageList := TGpStructuredStorageManipulatorList.Create;

  {$IFDEF DebugLog}
  Logger.Log('< initialization');
  {$ENDIF DebugLog}
finalization
  {$IFDEF DebugLog}
  Logger.Log('> finalization');
  {$ENDIF DebugLog}

  FreeAndNil(GStructuredStorageList);

  {$IFDEF DebugLog}
  Logger.Log('< finalization');
  {$ENDIF DebugLog}
end.
