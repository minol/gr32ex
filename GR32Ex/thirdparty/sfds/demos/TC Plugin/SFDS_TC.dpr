library SFDS_TC;

uses
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
  TcPlgFuncs in 'TcPlgFuncs.pas',
  Unit_ConfigDlg in 'Unit_ConfigDlg.pas' {ConfigDlg};

{$E wcx}

{$R *.res}
{$R SFX.res}

  exports
  OpenArchive,
  ReadHeader,
  ProcessFile,
  CloseArchive,
  PackFiles,
  //DeleteFiles,
  GetPackerCaps,
  ConfigurePacker,
  SetChangeVolProc,
  SetProcessDataProc,
  CanYouHandleThisFile,
  PackSetDefaultParams;

begin
end.
 