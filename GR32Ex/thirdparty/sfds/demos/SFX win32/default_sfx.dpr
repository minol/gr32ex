program default_sfx;

uses
  Windows,
  SysUtils,
  SFDS in '..\..\SFDS.pas',
  Md5 in '..\..\Md5.pas',
  sfds_ZLIBEx in '..\..\ZLIBEx\sfds_ZLIBEx.pas',
  sfds_bzip2Ex in '..\..\bzip2\sfds_bzip2Ex.pas',
  SFDS_StreamingUtils in '..\..\SFDS_StreamingUtils.pas',
  SFDS_SearchUtils in '..\..\SFDS_SearchUtils.pas',
  SFDS_PlatformUtils in '..\..\SFDS_PlatformUtils.pas',    
  SFDS_Consts in '..\..\SFDS_Consts.pas',
  SFDS_CompressorZLib in '..\..\SFDS_CompressorZLib.pas',
  SFDS_CompressorBZip2 in '..\..\SFDS_CompressorBZip2.pas',
  SFX_DLGS in 'SFX_DLGS.pas';

{$R *.res}

resourcestring
SSELFEXTRACT = 'Self extracting file';

var Indx : Integer;
    OFS : Int64;

begin
  try
  //R := TSFDSFileReader(SFDS_CreateReader(ParamStr(0), True));
  if SFDS_IsStorageFile(ParamStr(0), OFS) then
     R := TSFDSFileReader.Create(ParamStr(0), OFS, nil, nil, nil, True, sarmWindows);
  except
  end;
  if R = nil then
     begin
       MessageBox(0, PChar(SNotFound), PChar(SError), MB_ICONSTOP);
       Exit;
     end
     else
     begin
       SFX_DLGS.ExtractPath := ExtractFileDir(ParamStr(0));
       SFXTitle := SSELFEXTRACT;
       if R.ExtraInfoFields.Count > 0 then
          begin
            Indx := R.ExtraInfoFields.IndexOfField('SFX Title');
            if (Indx <> -1) and (R.ExtraInfoFields.ExtraInfoField[Indx].FieldType = EIStr) then
                 SFXTitle := Pointer2String(R.ExtraInfoFields.ExtraInfoField[Indx].ValueString, R.ExtraInfoFields.ExtraInfoField[Indx].ValueStringSize);
            Indx := R.ExtraInfoFields.IndexOfField('SFX Target Dir');
            if (Indx <> -1) and (R.ExtraInfoFields.ExtraInfoField[Indx].FieldType = EIStr) then
                 ExtractPath := Pointer2String(R.ExtraInfoFields.ExtraInfoField[Indx].ValueString, R.ExtraInfoFields.ExtraInfoField[Indx].ValueStringSize);
          end;
       SFX_DLGS.ArcFullSize := R.AllFilesSize;
     end;
  if R.Comments <> '' then
     begin
       CMT := R.Comments;
       SFX_DLGS.CreateDialog(0, 'SFXINFORMATIONDLG', True, @SFX_DLGS.ResDlgProcInfo);
     end;
  SFX_DLGS.CreateDialog(0, 'SFXMAINDLG', True, @SFX_DLGS.ResDlgProcMain);
  if SFX_DLGS.DlgResult <> ID_OK then Exit;
  SFX_DLGS.CreateDialog(0, 'SFXPROGRESSDLG', False, @SFX_DLGS.ResDlgProcProgress);
  DoExtract;  
end.
