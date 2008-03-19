program testGpStructuredStorage;

uses
  Forms,
  testGpStructuredStorage1 in 'testGpStructuredStorage1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
