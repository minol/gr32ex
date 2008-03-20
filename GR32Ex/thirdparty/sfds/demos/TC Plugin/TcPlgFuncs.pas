unit TcPlgFuncs;

interface
  uses Windows, SysUtils, Classes, wcxHead, SFDS, SFDS_Consts, unit_ConfigDlg, DateUtils, SFDS_CompressorZLib, SFDS_CompressorBZip2;

  function OpenArchive(ArchiveData : POpenArchiveData) : THandle; stdcall;
  function ReadHeader(hArcData : THandle; HeaderData : PHeaderData) : Integer; stdcall;
  function ProcessFile(hArcData : THandle; Operation : Integer; DestPath, DestName : PChar) : Integer; stdcall;
  function CloseArchive(hArcData : THandle) : Integer; stdcall;

  function PackFiles(PackedFile, SubPath, SrcPath, AddList : PChar; Flags : Integer) : Integer; stdcall;
  function DeleteFiles(PackedFile, DeleteList : PChar) : Integer; stdcall;

  function GetPackerCaps() : Integer; stdcall;
  procedure ConfigurePacker(Parent, DllInstance : THandle); stdcall;

  procedure SetChangeVolProc(hArcData : THandle; pChangeVolProc1 : PChangeVolProc); stdcall;
  procedure SetProcessDataProc(hArcData : THandle; pProcessDataProc1 : pProcessDataProc); stdcall;

  function CanYouHandleThisFile(FileName : PChar) : BOOL; stdcall;
  procedure PackSetDefaultParams(dps : PPackDefaultParamStruct); stdcall;

  type
  TProgressObject = class;

  PSFDSINFO_STRUCT = ^SFDSINFO_STRUCT;
  SFDSINFO_STRUCT = record
     Reader : TSFDSFileReader;
     NextFileIndex : Integer;
     ProgressObject : TProgressObject;
     Time : Integer;
    end;

  TProgressObject = class(TObject)
  protected
    PDP : TProcessDataProc;
    CVP : TChangeVolProc;
  public
    procedure PEVENT(Sender : TSFDSCustomSource; Progress, MaxProgress, ThisTime : Int64; var Cancel : Boolean; ArcName, StreamName : string);
  end;

  var CompFormat : string = 'ZLib (Deflate)';
      CompLevel : TSFDSCompressionLevel = clFastest;
      InternalFileListCompressionFormat : string = 'ZLib (Deflate)';
      CompressionStrategy : TSFDSCompressionStrategy = csDefault;
      TCINI : string;
      FileOpenedForReading : Boolean = False;
      PDP2 : TProcessDataProc;
      Title_, Dir_ : string;
      AppendMode : TAppendSFXMode = aSfxNone;

implementation

uses StdCtrls;

function UnixTimeToDosTime(mtime : Longint) : Longint;
var
  ft     : TFileTime;
  st     : TSystemTime;
  hw, lw : Word;
begin
  ft.dwLowDateTime  := $D53E8000;
  ft.dwHighDateTime := $019DB1DE;
  comp(ft):=comp(ft) + 10000000.0 * (mtime);
  FileTimeToSystemTime(ft, st);
  FileTimeToDosDateTime(ft, hw, lw);
  Result := 65536 * hw + lw;
end;

function IniWriteString(const Section, Key, Value, FileName: string) : boolean;
begin
  if Filename <> '' then
  result := WritePrivateProfileString(PChar(Section), PChar(Key),
    PChar(Value), PChar(FileName))
  else
    Result := WriteProfileString(PChar(Section), PChar(Key),
      PChar(Value));
end;

function IniReadString(const Section, Key, Default, Filename: String) : string;
var
  Buffer: array[0..2047] of Char;
begin
if Filename <> '' then
  SetString(Result, Buffer, GetPrivateProfileString(PChar(Section),
    PChar(Key), PChar(Default), Buffer, SizeOf(Buffer), PChar(FileName)))
  else
  SetString(Result, Buffer, GetProfileString(PChar(Section),
    PChar(Key), PChar(Default), Buffer, SizeOf(Buffer)));
end;

  procedure SupportedAlways(Sender : TSFDSCustomReader; GUID : TGUID; var Supported : Boolean);
  begin
    Supported := True;
  end;

  function OpenArchive(ArchiveData : POpenArchiveData) : THandle;
  var SF : TSFDSFileReader;
      SFDSINFO : PSFDSINFO_STRUCT;
      POB : TProgressObject;
      Time : TDateTime;
      Pos : Int64;
  begin
    result := 0;
    ArchiveData.OpenResult := E_UNKNOWN_FORMAT;
    if not SFDS_IsStorageFile(ArchiveData^.ArcName, Pos) then exit;
    SF := nil; POB := nil;
    New(SFDSINFO);    
    try
      SF := TSFDSFileReader.Create(ArchiveData^.ArcName, POS, nil, SupportedAlways, nil, True, sarmWindows);
      SFDSINFO^.Reader := SF;
      POB := TProgressObject.Create;
      SFDSINFO^.ProgressObject := POB;
      SFDSINFO^.NextFileIndex := 0;
      result := Integer(SFDSINFO);
      ArchiveData^.OpenResult := 0;
      Time := SF.TimeCreate;
      SFDSINFO^.Time := UnixTimeToDosTime(DateTimeToUnix(Time));
      FileOpenedForReading := True;
    except
      result := 0;
      ArchiveData^.OpenResult := E_UNKNOWN_FORMAT;
      Dispose(SFDSINFO);
      if Assigned(POB) then POB.Free;
      if Assigned(SF) then SF.Free;
      FileOpenedForReading := False;
    end;
  end;

  function ReadHeader (hArcData : THandle; HeaderData : PHeaderData) : Integer;
  var SFDSINFO : PSFDSINFO_STRUCT;
      FILEINFO : PSFDS_FileEntryRecord;
  begin
    SFDSINFO := PSFDSINFO_STRUCT(hArcData);  
    try
    result := 0;
    if SFDSINFO^.NextFileIndex > SFDSINFO^.Reader.FileCount - 1 then
       begin
         result := E_END_ARCHIVE;
         SFDSINFO^.NextFileIndex := 0;
         exit;
       end;
    FILEINFO := SFDSINFO^.Reader.FileEntrys.Entrys[SFDSINFO^.NextFileIndex];
    Inc(SFDSINFO^.NextFileIndex);
    StrPCopy(HeaderData^.ArcName, SFDSINFO^.Reader.FSFileName);
    StrPCopy(HeaderData^.FileName, FILEINFO^.FileName);
    HeaderData^.PackSize := FILEINFO^.EndPosition - FILEINFO^.StartPosition;
    HeaderData^.UnpSize := FILEINFO^.FileSize;
    HeaderData^.FileAttr := FILEINFO^.FileAttributes;
    HeaderData^.FileCRC := 0;
    HeaderData.FileTime := SFDSINFO^.Time;
    //SFDSINFO^.Reader.FSFileName
    //HeaderData.ArcName := ;
    except
      result := E_END_ARCHIVE;
      SFDSINFO^.NextFileIndex := 0;
    end;
  end;

  function ProcessFile(hArcData : THandle; Operation : Integer; DestPath, DestName : PChar) : Integer;
  var SFDSINFO : PSFDSINFO_STRUCT;
      S, StreamName : String;
      FILEINFO : PSFDS_FileEntryRecord;
  begin
    try
    result := 0;
    SFDSINFO := PSFDSINFO_STRUCT(hArcData);
    FILEINFO := SFDSINFO^.Reader.FileEntrys.Entrys[SFDSINFO^.NextFileIndex - 1];
    if StrPas(DestPath) <> '' then S := IncludeTrailingPathDelimiter(StrPas(DestPath));
    S := S + StrPas(DestName);
    StreamName := FILEINFO^.FileName;
     case Operation of
     PK_SKIP  :  begin
                   // Do nothing...
                   // All files can be accessed at any time...
                 end;
     PK_TEST  :  begin
                   result := SFDSINFO^.Reader.ExtractFile(StreamName, '', True, SFDSINFO^.ProgressObject.PEVENT, False);
                 end;
     PK_EXTRACT: begin
                   result := SFDSINFO^.Reader.ExtractFile(StreamName, S, False, SFDSINFO^.ProgressObject.PEVENT, False);
                 end;
     end;
     except
       result := E_BAD_DATA;
     end;
  end;

  function CloseArchive(hArcData : THandle) : Integer;
  var SFDSINFO : PSFDSINFO_STRUCT;
  begin
    result := 0;
    try
    SFDSINFO := PSFDSINFO_STRUCT(hArcData);
    if SFDSINFO = nil then Exit;
    SFDSINFO^.Reader.Free;
    SFDSINFO^.ProgressObject.Free;
    Dispose(SFDSINFO);
    except
    end;
    FileOpenedForReading := False;
  end;

  function GetFileNames(Names : PChar) : TStrings;
  var N : Pchar;
  begin
    N := Names;
    result := TStringList.Create;
    while (n <> nil) and (N[0] <> #0) do
       begin
         result.Add(StrPas(N));
         N := N + Length(N) + 1;
       end;
  end;

  function PackFiles (PackedFile, SubPath, SrcPath, AddList : PChar; Flags : Integer) : Integer;
  var List : TStrings;
      Writer : TSFDSFileWriter;
      I : Integer;
      POB : TProgressObject;
      DestF : string;
      RST : TResourceStream;
      FLDS : TSFDSFileExtraInfoList;
      Fld : TSFDSFileExtraInfoField;
      PackedFileStr : string;
  begin
    List := nil;
    Writer := nil;
    POB := nil;
    FLDS := nil;
    result := E_OK;
    PackedFileStr := PackedFile;
    try
    List := GetFileNames(AddList);
    POB := TProgressObject.Create;
    POB.PDP := PDP2;

    try
    if AppendMode <> aSfxNone then
       begin
         PackedFileStr := ChangeFileExt(PackedFileStr, '.exe');
         RST := TResourceStream.Create(HInstance, 'DEFAULT_SFX', 'BIN');
         RST.SaveToFile(PackedFileStr);
       end;
    except
      result := E_EREAD;
      exit;
    end;

    FLDS := TSFDSFileExtraInfoList.Create;
    if AppendMode <> aSfxNone then
       begin
        FLD.Name := 'SFX Title'; Fld.FieldType := EIStr;
        String2Pointer(Title_, FLD.ValueString); FLD.ValueStringSize := Length(Title_);
        if Title_ <> '' then FLDS.AddOrUpdateField(FLD);
        FLD.Name := 'SFX Target Dir'; Fld.FieldType := EIStr;
        String2Pointer(Dir_, FLD.ValueString); FLD.ValueStringSize := Length(Dir_);        
        if Dir_ <> '' then FLDS.AddOrUpdateField(FLD);
       end;

    Writer := TSFDSFileWriter.Create(PackedFileStr, (AppendMode <> aSfxNone), AppendMode, nil, '', '', '', 0, 'SFDS TC Plugin', 0, '', '', FLDS);
    Writer.InternalCompressionFormat := SFDS_CompressionFormatIDByFormatName(InternalFileListCompressionFormat);
    for I := 0 to List.Count - 1 do
        begin
          if not FileExists(IncludeTrailingPathDelimiter(SrcPath) + List.Strings[I]) then continue;
          if (Flags = PK_PACK_SAVE_PATHS) or (Flags = PK_PACK_SAVE_PATHS + PK_PACK_MOVE_FILES) then
            begin
              if SubPath <> '' then DestF := IncludeTrailingPathDelimiter(SubPath) + List.Strings[I]
                               else DestF := List.Strings[I];
            end
            else
            begin
            DestF := ExtractFileName(List.Strings[I]);
            end;

          result := Writer.WriteFile(IncludeTrailingPathDelimiter(SrcPath) + List.Strings[I], DestF, POB.PEVENT, 0, SFDS_CompressionFormatIDByFormatName(CompFormat), CompLevel, CompressionStrategy, '', 0, False);
          if result <> E_OK then Break;
        end;

      if Result <> E_OK then
         begin
         Writer.Free; Writer := nil;
         DeleteFile(PackedFileStr);
         end
         else
         begin
          if (Flags and PK_PACK_MOVE_FILES) = PK_PACK_MOVE_FILES then
             begin  //Delete original after packing

             end;
         end;
    finally
      if Assigned(FLDS) then FLDS.Free;
      if Assigned(List) then List.Free;
      if Assigned(Pob) then Pob.Free;
      if Assigned(Writer) then Writer.Free;
    end;
    AppendMode := aSfxNone;
  end;

  function DeleteFiles(PackedFile, DeleteList : PChar) : Integer;
  begin
    result := E_NOT_SUPPORTED;
  end;

  function GetPackerCaps() : Integer;
  begin
    result := PK_CAPS_NEW or {PK_CAPS_MODIFY or }PK_CAPS_MULTIPLE{ or PK_CAPS_DELETE} or PK_CAPS_OPTIONS or PK_CAPS_BY_CONTENT or PK_CAPS_SEARCHTEXT;
  end;

  procedure ConfigurePacker(Parent, DllInstance : THandle);
  var DLG : TConfigDlg;
  begin
    DLG := nil;
    try
    DLG := TConfigDlg.Create(nil);
    DLG.Update__;
    if DLG.ShowModal = ID_OK then
       begin
         InternalFileListCompressionFormat := Dlg.ComboInt.Text;
         CompFormat := Dlg.ComboFile.Text;
         CompLevel := TSFDSCompressionLevel(Dlg.ComboLevel.ItemIndex);
         AppendMode := TAppendSFXMode(Dlg.SFX.ItemIndex);
         CompressionStrategy := TSFDSCompressionStrategy(Dlg.ComboStrategy.ItemIndex);
         Title_ := Dlg.Title.Text;
         Dir_ := Dlg.Dir.Text;
         IniWriteString('SFDS_TCPlugin', 'CompressionFormat', CompFormat, TCINI);
         IniWriteString('SFDS_TCPlugin', 'CompressionLevel', IntToStr(Integer(CompLevel)), TCINI);
         IniWriteString('SFDS_TCPlugin', 'InternalFileListCompressionFormat', InternalFileListCompressionFormat, TCINI);
         IniWriteString('SFDS_TCPlugin', 'SFXTitle', Dlg.Title.Text, TCINI);
         IniWriteString('SFDS_TCPlugin', 'SFXDir', Dlg.Dir.Text, TCINI);
         IniWriteString('SFDS_TCPlugin', 'CompressionStrategy', IntToStr(Integer(CompressionStrategy)), TCINI);
       end;
    finally
    DLG.Free;
    end;
  end;

  procedure SetChangeVolProc(hArcData : THandle; pChangeVolProc1 : PChangeVolProc);
  var SFDSINFO : PSFDSINFO_STRUCT;
  begin
    if not FileOpenedForReading then Exit;
    SFDSINFO := PSFDSINFO_STRUCT(hArcData);
    SFDSINFO^.ProgressObject.CVP := TChangeVolProc(pChangeVolProc1);
  end;

  procedure SetProcessDataProc(hArcData : THandle; pProcessDataProc1 : pProcessDataProc);
  var SFDSINFO : PSFDSINFO_STRUCT;
  begin
    if not FileOpenedForReading then
       begin
       PDP2 := TProcessDataProc(pProcessDataProc1);
       Exit;
       end;
    SFDSINFO := PSFDSINFO_STRUCT(hArcData);
    SFDSINFO^.ProgressObject.PDP := TProcessDataProc(pProcessDataProc1);
  end;

  function CanYouHandleThisFile(FileName : PChar) : BOOL;
  var POS : Int64;
  begin
    result := SFDS_IsStorageFile(FileName, POS);
  end;

  procedure PackSetDefaultParams(dps : PPackDefaultParamStruct);
  var I : Integer;
  begin
    TCINI := dps^.DefaultIniName;
//    IniWriteString('SFDS_TCPlugin', 'CompressionFormat', IntToStr(Integer(CompFormat)), TCINI);
//    IniWriteString('SFDS_TCPlugin', 'CompressionLevel', IntToStr(Integer(CompLevel)), TCINI);
//    IniWriteString('SFDS_TCPlugin', 'InternalFileListCompressionFormat', IntToStr(Integer(InternalFileListCompressionFormat)), TCINI);
    Title_ := IniReadString('SFDS_TCPlugin', 'SFXTitle', '', TCINI);
    Dir_ := IniReadString('SFDS_TCPlugin', 'SFXDir', '', TCINI);
    CompFormat := IniReadString('SFDS_TCPlugin', 'CompressionFormat', 'ZLib (Deflate)', TCINI);
    I := StrToIntDef(IniReadString('SFDS_TCPlugin', 'CompressionLevel', '0', TCINI), 0);
    if (I < 0) or (I > 2) then I := 1;
    CompLevel := TSFDSCompressionLevel(I);
    InternalFileListCompressionFormat := IniReadString('SFDS_TCPlugin', 'InternalFileListCompressionFormat', 'ZLib (Deflate)', TCINI);
    I := StrToIntDef(IniReadString('SFDS_TCPlugin', 'CompressionStrategy', '0', TCINI), 0);
    if (I < 0) or (I > 4) then I := 0;
    CompressionStrategy := TSFDSCompressionStrategy(I);
  end;

{ TProgressObject }

procedure TProgressObject.PEVENT(Sender: TSFDSCustomSource; Progress,
  MaxProgress, ThisTime: Int64; var Cancel: Boolean; ArcName,
  StreamName: string);
begin
  if Assigned(PDP) then Cancel := (PDP('', ThisTime) = 0);
end;

end.
