unit LoadImgTxtTestForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Image: TImage;
    LabelTxt: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses SFDS;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SFDS_CreateReader('data.sfds');
end;

procedure TForm1.Button1Click(Sender: TObject);
var Str : TStream;
    StrSz : Int64;
    S : string;
begin
  if not SFDS_OpenStreamIndirect('SFDS.bmp', Str, StrSz) then Exit;
  Image.Picture.Bitmap.LoadFromStream(Str);
  Str.Free;
  if not SFDS_OpenStreamIndirect('text', Str, StrSz) then Exit;
  SetLength(S, StrSz);
  Str.Read(PChar(S)^, StrSz);
  LabelTxt.Caption := S;
  Str.Free;
end;

end.
