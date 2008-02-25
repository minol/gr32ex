program ImgEdit;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  //Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
