program test_ttf;

uses
  Forms,
  main_test_ttf in 'main_test_ttf.pas' {fmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
