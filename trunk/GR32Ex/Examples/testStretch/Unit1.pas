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
    { D�clarations priv�es }
  public
    { D�clarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses JPeg,GR32
  ,GR32_Resamplers
  ,pngimage
;

type
{
1. ����������飬���� TARGB32 pic[gwidth*gheight];
Ȼ���÷�������ͼƬ���ݵ�pic��
����һ��TPicRegionָ��pic�Ϳ����ˣ�
���磺 TPicRegion pr;
pr.width=gwidth;
pr.height=gheight;
pr.pdata=&pic[0];
pr.byte_width=gwidth*sizeof��TARGB32);

(windows�¿���ֱ����SetDIBitsToDevice������TPicRegion��ʾ��ĳHDC��)

2. ����ʹ��GDI��DIB����(��ɫ��ʽ32bit RGB)��Ȼ����TPicRegionָ�������Ӷ�ʹ�����windows GDI����(�������ظ�������)

3. ����ʹ��DDraw�ı���(��ɫ��ʽ32bit RGB)�ȣ�Ȼ����TPicRegionָ�������Ӷ�ʹ�����DirectX����

4.cbuilder��̫��Ϥ��������TBitmap����ɣ�
(�Ұ���Delphi�ķ�������,cbuilderӦ������) ��TBitmap��������������Ϊ32bit RGB��Ҳ���� Bmp.PixelFormat=pf32bit��
TPicRegion pr;
pr.pdata=(TARGB32*)Bmp.ScanLine[0];
//! if (Bmp.Height==1) then pr.byte_width=Bmp.Width*sizeof(TARGB32);
pr.byte_width=integer(Bmp.ScanLine[1])-integer(Bmp.ScanLine[0]);
pr.width=Bmp.Width;
pr.height=Bmp.Height;
}
  PPicRegion = ^ TPicRegion;
  TPicRegion = packed record
    pData: PColor32Entry; //��ɫ�����׵�ַ
    byte_width: LongInt;//һ�����ݵ�������(�ֽڿ��)��
                //abs(byte_width)�п��ܴ��ڵ���width*sizeof(TARGB32);
    width,      //���ؿ��
    height: LongInt;//���ظ߶�
  end;

//����һ����ĺ���
function Pixels(const pic: PPicRegion; const x,y: LongInt): PColor32Entry;
begin
    Result := @PColor32Array(Integer(pic.pdata)+ pic.byte_width*y)^[x];
end;

//����һ����ĺ�����(x,y)������ܳ���ͼƬ�߽磻 //�߽紦��ģʽ:�߽籥��
function Pixels_Bound(const pic: PPicRegion; x, y: LongInt): TColor32Entry;
var
  IsInPic: Boolean;
begin
    //assert((pic.width>0)&&(pic.height>0));
    IsInPic := true;
    if (x<0) then
    begin 
    	x:=0; IsInPic:=false; 
    end
    else if (x>=pic.width ) then begin
    	x:=pic.width -1; IsInPic:=false; 
    end;
    if (y<0) then begin
    	y:=0; IsInPic:=false; 
    end 
    else if (y>=pic.height) then begin
    	y:=pic.height-1; IsInPic:=false;
    end;
    result:=Pixels(pic,x,y)^;
    if (not IsInPic) then result.a :=0;
end;


function SinXDivX(x: double): double;
const 
  a = -1.0; //a������ȡ a=-2,-1,-0.75,-0.5�ȵȣ��𵽵����񻯻�ģ���̶ȵ�����
var
  x2,x3: Double;
begin
    //�ú��������ֵ����sin(x*PI)/(x*PI)��ֵ //PI=3.1415926535897932385; 
    //���������Ľ�����ϱ��ʽ
    if (x<0) then x:=-x; //x=abs(x);
    x2:=x*x;
    x3:=x2*x;
    if (x<=1) then
      Result := (a+2)*x3 - (a+3)*x2 + 1
    else if (x<=2) then
      Result := a*x3 - (5*a)*x2 + (8*a)*x - (4*a)
    else
      Result := 0;
end;

function border_color(Color: Longint): Byte;
begin
            if (Color<=0) then
                Result := 0
            else if (Color>=255) then
                Result := 255
            else
                Result := Color;
end;


var
  SinXDivX_Table_8: array [0..(2 shl 8)] of Longint;
 //��ɫ���
  _color_table: array [0..256*3] of byte;

procedure _Init_SinXDivX_Table();
var
  i: integer;
begin
  for i:=0 to High(SinXDivX_Table_8) do
    SinXDivX_Table_8[i] := Trunc((0.5+256*SinXDivX(i*(1.0/(256))))*1);
end;


procedure _Init_color_table();
var
  i: integer;
begin
  for i:=0 to 256*3 do
    _color_table[i]:=border_color(i-256);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //Src.Bitmap[0].LoadFromFile(ExtractFilePath(Application.ExeName)+'image.jpg');
  Src.Bitmap[0].LoadFromFile(ExtractFilePath(Application.ExeName)+'zaka0.JPG');
  TLinearResampler.Create(src.Bitmap[0]);
  //TDraftResampler.Create(src.Bitmap[0]);
  //TNearestResampler.Create(src.Bitmap[0]);
  {    with TKernelResampler.Create(src.Bitmap[0]) as TKernelResampler do
        //Kernel := TSplineKernel.Create;
        Kernel := TMitchellKernel.Create;
  //}
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
  Dest.Bitmap.SetSize(src.Bitmap[0].Width*6, src.Bitmap[0].Height*6);
  for i := 1 to 100 do
  begin
    //Dest.Bitmap.Assign(Src.Bitmap[0]);
    t := GetTickCount;
    Dest.Bitmap.draw(Rect(0,0,Dest.Bitmap.Width,Dest.Bitmap.Height), Rect(0,0,src.Bitmap[0].Width,src.Bitmap[0].Height), src.Bitmap[0]);
    m := m + (GetTickCount-t);
  end;
  LabelWait.Visible := false;
  m := m div 100; // ms/frame
  Memo1.Lines.Add('Image ToSize('+IntToStr(Dest.Bitmap.Width)+'x'+IntToStr(Dest.Bitmap.Height)+')');
  Memo1.Lines.Add('Blur MMX/PPtr='+IntToStr(m)+'ms' + '; ' +  IntToStr(Trunc(1 / m * 1000)) +  'fps' );
  Dest.Bitmap.SaveToFile('sl1.jpg');
end;
end.
