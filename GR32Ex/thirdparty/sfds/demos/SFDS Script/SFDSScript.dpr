program SFDSScript;

{$APPTYPE CONSOLE}

uses
  {$IFNDEF FPC}Windows,{$ENDIF}
  SysUtils,
  Classes,
  SFDS in '..\..\SFDS.pas',
  Md5 in '..\..\Md5.pas',
  sfds_ZLIBEx in '..\..\ZLIBEx\sfds_ZLIBEx.pas',
  sfds_bzip2Ex in '..\..\bzip2\sfds_bzip2Ex.pas',
  SFDS_StreamingUtils in '..\..\SFDS_StreamingUtils.pas',
  SFDS_SearchUtils in '..\..\SFDS_SearchUtils.pas',
  SFDS_PlatformUtils in '..\..\SFDS_PlatformUtils.pas',
  SFDS_Consts in '..\..\SFDS_Consts.pas',
  SFDS_CompressorZLib in '..\..\SFDS_CompressorZLib.pas',
  SFDS_CompressorBZip2 in '..\..\SFDS_CompressorBZip2.pas';

  type
  progressObject = class
    procedure Progress(Sender : TSFDSCustomSource; Progress, MaxProgress, ThisTime : Int64; var Cancel : Boolean; ArcName, StreamName : string);
    procedure ListProc(const F : TSearchRec; SourceDir : string);
  end;

  var
  OutputFile,
  SFXMode,
  SFX,
  GUID,
  Title,
  Subject,
  Author,
  Version,
  Maker,
  MakerVersion,
  KeyWords,
  Comments,
  InternalFileListCompressionFormat : string;
  CompressionStrategy : Integer;
  po : progressObject;

  DiskFileName, FileName, ExtraDataStr : string;
  FileAttributes, CompressionFormat, CompressionLevel, ExtraDataInt : Integer;
  FileMetadataSource : string;
  Writer : TSFDSFileWriter;

  ES : string;

  procedure DisplayUsage;
  begin
    writeln('SFDSScript');
    writeln('Builds a SFDS file from a text script');
    writeln('');
    writeln('Usage: SFDSScript "SFDSScriptFile.sfdsscript"');
    writeln('');
    writeln('See the "sample script.sfdsscript" for a sample script.');
    writeln('');
    writeln('');
    writeln('Press ENTER to exit.');
    readln;
  end;

  procedure SplitText(InputText : string; out Str1, Str2 : string);
  begin
    if AnsiPos('=', InputText) = 0 then
       begin
         Str1 := InputText;
         Str2 := '';
         Exit;
       end;
    Str1 := Copy(InputText, 1, AnsiPos('=', InputText) - 1);
    Str2 := Copy(InputText, AnsiPos('=', InputText) + 1, Length(InputText) - AnsiPos('=', InputText));
  end;

  procedure TryPackDir(DirAndMask : string);
  var Dir, Mask : string;
      Recursive : Boolean;
  begin
    Dir := ExtractFileDir(DirAndMask);
    Mask := ExtractFileName(DirAndMask);
    Recursive := SameText(Copy(Mask, Length(Mask) - 1, 2), '/s');
    if Recursive then Delete(Mask, Length(Mask) - 1, 2);
    ListFiles(Dir, Mask, Recursive, po.ListProc);
  end;

  procedure ParseTextScript;
  var F : TextFile;
      CurrentLine : string;
      Str1, Str2 : string;
      PG : PGUID;

      procedure CheckWriter;
      var CF : Integer;
          F1, F2 : TFileStream;
          META : TSFDSFileExtraInfoList;
          Outpos : Int64;
          TmpRead : TSFDSFileReader;
      begin
      try
        if Assigned(Writer) then Exit;
        New(PG);
        try
         PG^ := StringToGuid(GUID);
        except
         PG := nil;
        end;
        try
         if FileExists(SFX) then
            begin
             try
                F1 :=nil; F2 := nil;             
              try
                F1 := TFileStream.Create(SFX, fmOpenRead or fmShareDenyWrite);
                F2 := TFileStream.Create(OutputFile, fmCreate or fmShareDenyWrite);
                F2.CopyFrom(F1, F1.Size);
              finally
                F1.Free; F2.Free;
              end;
             except
             exit;
             end;
            end;
         META := nil;
         try
         if (FileMetadataSource <> '') and
            FileExists(FileMetadataSource) and
            SFDS_IsStorageFile(FileMetadataSource, OutPos)
            then
            begin
              TmpRead := TSFDSFileReader(SFDS_CreateReader(FileMetadataSource));
              try
              META := TSFDSFileExtraInfoList.Create;
              META.Assign(TmpRead.ExtraInfoFields);
              finally
                TmpRead.Free;
              end;
            end
            else
            begin
              Writeln('Warning: FileMetadataSource is not valid or doesn''t exist');
            end;
         except
         end;

         Writer := TSFDSFileWriter.Create(OutputFile, (StrToIntDef(SFXMode, 0) <> 0) and (FileExists(SFX)),
                   TAppendSFXMode(StrToIntDef(SFXMode, 0)), PG, Title, Subject, Author, StrToIntDef(Version, 0),
                   Maker, StrToIntDef(MakerVersion, 0), Keywords, Comments, META);
         META.Free;
         CF := StrToIntDef(InternalFileListCompressionFormat, 0);
         if (CF < 0) or (CF > 2) then CF := 0;
         Writer.InternalCompressionFormat := CF;
        finally
         Dispose(PG);
        end;
      except
        on e : exception do
           begin
           writeln(#13#10 + 'Error: ' + e.message);
           writeln('Keep partial SFDS file? Y/N');
           readln(ES);
           if not SameText('Y', ES) then DeleteFile(OutputFile);
           end;
      end;
      end;

  label 1;

  begin
  try
  Writer := nil;
  {$I-}
    AssignFile(F, ParamStr(1));
    FileMode := 0;
    Reset(F);
    if (IOResult <> 0) then
       begin
         writeln('Unable to open script file.');
         Halt;
       end;

    CurrentLine := '';
    try

    1:
       if CurrentLine <> '' then
       begin
         if Copy(CurrentLine, 1, 1) = ';' then
            begin
              Delete(CurrentLine, 1, 1);
              writeln(CurrentLine); //Comment
            end
            else
            begin
            if SameText(CurrentLine, 'Execute=SFDSFile::Close') then Exit;
            if SameText(CurrentLine, 'Execute=SFDSFile::AddFile') then
               begin
                 CheckWriter;
                 if not FileExists(DiskFileName) then
                    begin
                     TryPackDir(DiskFileName);
                    end
                    else
                    begin
                     Write('Adding file: ' + FileName);
                     Writer.WriteFile(DiskFileName, FileName, po.Progress, FileAttributes, CompressionFormat, TSFDSCompressionLevel(CompressionLevel), TSFDSCompressionStrategy(CompressionStrategy), ExtraDataStr, ExtraDataInt, True);
                     Writeln('');
                    end;
               end
               else
               begin
                SplitText(CurrentLine, Str1, Str2);
                if SameText(Str1, 'ChunkSize') then
                   begin
                   SFDS_Set_ChunkSize(StrToIntDef(Str2, DEFAULT_SFDS_ChunkSize));
                   writeln('ChunkSize set to ' + IntToStr(SFDS_ChunkSize) + ' bytes');
                   end;
                if SameText(Str1, 'OutputFile') then OutputFile := Str2;
                if SameText(Str1, 'SFXMode') then SFXMode := Str2;
                if SameText(Str1, 'SFX') then SFX := Str2;
                if SameText(Str1, 'GUID') then GUID := Str2;
                if SameText(Str1, 'Title') then Title := Str2;
                if SameText(Str1, 'Subject') then Subject := Str2;
                if SameText(Str1, 'Author') then Author := Str2;
                if SameText(Str1, 'Version') then Version := Str2;
                if SameText(Str1, 'Maker') then Maker := Str2;
                if SameText(Str1, 'MakerVersion') then MakerVersion := Str2;
                if SameText(Str1, 'KeyWords') then KeyWords := Str2;
                if SameText(Str1, 'Comments') then Comments := Str2;
                if SameText(Str1, 'FileMetadataSource') then FileMetadataSource := Str2;
                if SameText(Str1, 'InternalFileListCompressionFormat') then InternalFileListCompressionFormat := Str2;

                if SameText(Str1, 'DiskFileName') then DiskFileName := Str2;
                if SameText(Str1, 'FileName') then FileName := Str2;
                if SameText(Str1, 'ExtraDataStr') then ExtraDataStr := Str2;
                if SameText(Str1, 'FileAttributes') then FileAttributes := StrToIntDef(Str2, 0);
                if SameText(Str1, 'CompressionFormat') then
                   begin
                   CompressionFormat := StrToIntDef(Str2, 0);
                   if (CompressionFormat < 0) or (CompressionFormat > 2) then CompressionFormat := 0;
                   end;
                if SameText(Str1, 'CompressionLevel') then
                   begin
                   CompressionLevel := StrToIntDef(Str2, 0);
                   if (CompressionLevel < 0) or (CompressionLevel > 2) then CompressionLevel := 0;
                   end;
                if SameText(Str1, 'CompressionStrategy') then
                   begin
                   CompressionStrategy := StrToIntDef(Str2, 0);
                   if (CompressionStrategy < 0) or (CompressionStrategy > 4) then CompressionStrategy := 0;
                   end;
                if SameText(Str1, 'ExtraDataInt') then ExtraDataInt := StrToIntDef(Str2, 0);
               end;
            end;
       end;

    if EOF(F) then Exit;
    ReadLn(F, CurrentLine);
    CurrentLine := Trim(CurrentLine);
    if IOResult <> 0 then
       begin
         writeln('Unable to read text line.');
         Halt;
       end;
    goto 1;
    finally
    CloseFile(F);
    if Assigned(Writer) then Writer.Free;
    end;
  {$I+}
  except
   on e : exception do
           begin
           writeln(#13#10 + 'Error: ' + e.message);
           writeln('Keep partial SFDS file? Y/N');
           readln(ES);
           if not SameText('Y', ES) then DeleteFile(OutputFile);
           end;
  end;
  end;

  procedure progressObject.ListProc(const F : TSearchRec; SourceDir : string);
  var FEXT : string;
  begin
    Write('Adding file: ' + F.Name);
    FEXT := SourceDir + F.Name;
    Delete(FEXT, 1, Length(ExtractFileDir(DiskFileName)) + 1);
    Writer.WriteFile(SourceDir + F.Name, IncludeTrailingPathDelimiter(FileName) + FEXT, po.Progress, FileAttributes, CompressionFormat, TSFDSCompressionLevel(CompressionLevel), TSFDSCompressionStrategy(CompressionStrategy), ExtraDataStr, ExtraDataInt, True);
    Writeln('');
  end;

  procedure progressObject.Progress(Sender : TSFDSCustomSource; Progress, MaxProgress, ThisTime : Int64; var Cancel : Boolean; ArcName, StreamName : string);
  begin
    write('#');
  end;

begin
  try
  po := progressObject.Create;
  if not FileExists(ParamStr(1)) then DisplayUsage
                                 else ParseTextScript;
  po.Free;
  except
  end;
end.
