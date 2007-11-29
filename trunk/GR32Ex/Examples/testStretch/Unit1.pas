unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, GR32_Image, StdCtrls, Controls,
  Forms, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Dest: TImage32;
    Src: TBitmap32List;
    Button4: TButton;
    LabelWait: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses JPeg,GR32
  ,GR32_Resamplers
  ,pngimage
;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //Src.Bitmap[0].LoadFromFile(ExtractFilePath(Application.ExeName)+'image.jpg');
  Src.Bitmap[0].LoadFromFile(ExtractFilePath(Application.ExeName)+'picdone.png');
  //TLinearResampler.Create(src.Bitmap[0]);
      with TKernelResampler.Create(src.Bitmap[0]) as TKernelResampler do
        Kernel := TSplineKernel.Create;
  Memo1.Lines.Add('Image('+IntToStr(Src.Bitmap[0].Width)+'x'+IntToStr(Src.Bitmap[0].Height)+')');
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  i:integer;
  m,t:Cardinal;
begin
  m := 0;
  LabelWait.Visible := true;
  LabelWait.Refresh;
  Dest.Bitmap.SetSize(1024,768);
  for i := 1 to 100 do
  begin
    //Dest.Bitmap.Assign(Src.Bitmap[0]);
    t := GetTickCount;
    Dest.Bitmap.draw(Rect(0,0,640,480), Rect(0,0,src.Bitmap[0].Width,src.Bitmap[0].Height), src.Bitmap[0]);
    m := m + (GetTickCount-t);
  end;
  LabelWait.Visible := false;
  Memo1.Lines.Add('Blur MMX/PPtr='+IntToStr(m div 100)+'ms');
end;
end.
