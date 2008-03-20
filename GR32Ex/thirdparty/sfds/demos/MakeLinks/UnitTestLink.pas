unit UnitTestLink;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormTestLnk = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTestLnk: TFormTestLnk;

implementation
uses SFDS, mmsystem, sfds_compressorzlib, sfds_compressorbzip2;

{$R *.dfm}

procedure TFormTestLnk.Button1Click(Sender: TObject);
var W : TSFDSFileWriter;
begin
  W := TSFDSFileWriter.Create('test.tmp.sfds');
  W.InternalCompressionFormat := 1;
  W.WriteFile(Application.exename, extractfilename(application.ExeName), nil, 0, 1);
  W.CreateLink(extractfilename(application.ExeName), 'Link 1.exe');
  W.CreateLink(extractfilename(application.ExeName), 'Link 2.exe');
  W.CreateLink(extractfilename(application.ExeName), 'Link 3.exe');
  W.CreateLink(extractfilename(application.ExeName), 'Link 4.exe');
  W.CreateLink(extractfilename(application.ExeName), 'Link 5.exe');
  W.OpenStream('Test Stream');
  W.Write(PChar('Text')^, 4);
  W.OpenStream('Test Stream 2');
  W.Free;
end;

end.
