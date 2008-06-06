unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs
  , GR32
  , GR32_Image
  , GR_Effects
  ;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FImg: TImage32;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  vBmp, vDst: TBitmap32;
  vEff: TReflectionEffect;
begin
  FImg := TImage32.Create(Self);
  with FImg do
  begin
    Parent := Self;
    Align := alClient;
  end;
	vBmp := TBitmap32.Create;
	vDst := TBitmap32.Create;
  vEff := TReflectionEffect.Create(nil);
	try
	  vBmp.LoadFromFile(ExtractFilePath(ParamStr(0))+ 'vcl.png');
	  vEff.Generate(vBmp, vDst, vBmp.ClipRect);
    FImg.Bitmap.Assign(vDst);

	  //vDst.SaveToFile(ExtractFilePath(ParamStr(0))+'vcl1.png');
	finally
	  vBmp.Free;
	  vDst.Free;
	  vEff.Free;
	end;
end;

end.
