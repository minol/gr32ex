program Viewer;

uses 
  //ExceptionLog,
  Forms,
  ViewMain in 'VIEWMAIN.PAS' {Form1},
  CompDoc in 'COMPDOC.PAS';

{$R *.RES}

begin
  {$IFDEF WIN32}Application.Initialize;{$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
