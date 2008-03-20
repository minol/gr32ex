program SearchTest;

uses
  Forms,
  SFDS in '..\..\SFDS.pas',
  Md5 in '..\..\Md5.pas',
  sfds_ZLIBEx in '..\..\ZLIBEx\sfds_ZLIBEx.pas',
  sfds_bzip2Ex in '..\..\bzip2\sfds_bzip2Ex.pas',
  SFDS_StreamingUtils in '..\..\SFDS_StreamingUtils.pas',
  SFDS_SearchUtils in '..\..\SFDS_SearchUtils.pas',
  SFDS_Consts in '..\..\SFDS_Consts.pas',
  SFDS_CompressorZLib in '..\..\SFDS_CompressorZLib.pas',
  SFDS_CompressorBZip2 in '..\..\SFDS_CompressorBZip2.pas',
  UnitSearchForm in 'UnitSearchForm.pas' {FormSearchTest},
  sfds_platformutils in '..\..\sfds_platformutils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSearchTest, FormSearchTest);
  Application.Run;
end.
