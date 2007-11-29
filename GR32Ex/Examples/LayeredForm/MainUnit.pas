unit MainUnit;

interface

{$Define Debug}

uses
  {$ifdef Debug}
  DbugIntf,
  {$endif} 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls,
  Dialogs, Buttons
  , Jpeg
  //, GraphicEx
  , pngimage
  , GR32, GR32_Image, GR32_Layers, GR32_Filters, GR32_Transforms, GR32_Blend
  , GR_ControlLayers, GR_StdCtrlLayers
  , GR_ImageEx
  , GR_Forms
  , GR_BitmapEx
  , GR_Graphics
  , GR_GraphUtils
  ;

type
  TMainForm = class(TGRImg32Form)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    //MainPanel: TGRPanel;
    //SubPanel1: TGRPanel;
    //SubPanel2: TGRPanel;
    //Label1: TGRLabel;
    //gbMain: TGRGroupBox;
    Label1: TLabelLayer;
    Label2: TLabelLayer;
    //Image32: TImage32;
    procedure DoClick(Sender: TObject);
    procedure DoMouseEnter(Sender: TObject);
    procedure DoMouseLeave(Sender: TObject);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X: Integer; Y: Integer);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  GR32_PNG;
  
procedure TMainForm.FormCreate(Sender: TObject);
var
  P: TPoint;
begin
  //BorderIcons := [];
  //BorderStyle := bsNone;

  {Image32 := TImage32Ex.Create(Self);
  Image32.Parent := Self;
  Image32.Align := alClient;//}
    ShowHint := True;
  with Image32 do
  try
    BeginUpdate;
    Layers.Clear;
    Scale := 1;
    Bitmap.SetSize(Width, Height);
    Bitmap.Clear(0);
      with TBitmapLayer.Create(Layers) do
      try
        LoadPNGintoBitmap32(Bitmap, 'png.png');
        //Bitmap.LoadFromFile('png.png');
        Bitmap.DrawMode := dmBlend;
        //OnMouseMove := DoMouseMove;

        with Image32.GetViewportRect do
          P := Image32.ControlToBitmap(Point(Left, Top));

        //with Image32.Bitmap do
          Location := FloatRect(0, 0, Image32.Width, Image32.Height);

        Scaled := True;
        //Scaled := False;
      except
        Free;
        raise;
      end;

      with TBitmapLayer.Create(Layers) do
      try
        LoadPNGintoBitmap32(Bitmap, 'valkyries.png');
        //Bitmap.LoadFromFile('png.png');
        Bitmap.DrawMode := dmBlend;
        OnMouseMove := DoMouseMove;

        with Image32.GetViewportRect do
          P := Image32.ControlToBitmap(Point(Left, Top));

        //with Image32.Bitmap do
          Location := FloatRect(0, 10, 150, 150);

        Scaled := True;
        //Scaled := False;
      except
        Free;
        raise;
      end;

      {Label2 := TLabelLayer.Create(Layers);
      with Label2 do
      try
        AlphaBlend := True;
        AlphaBlendValue := 180;
        OnMouseEnter := DoMouseEnter;
        OnMouseLeave :=DoMouseLeave;
        OnMouseMove := DoMouseMove;
        with Image32.GetViewportRect do
          P := Image32.ControlToBitmap(Point(Left, Top));
        Caption := 'SDDD X:' + IntToStr(P.X) + ' Y:'+IntToStr(P.Y);

        //Inc(P.X, 20);
        //Inc(P.Y, 160);
        //FloatRect: Left, Top, Right, Bottom
        Location := FloatRect(P.X, P.Y, P.X+200, P.Y+148);

  with Background do
  try
    BeginUpdate;
    Enabled := True;
    Gradient.Style := gsNone;
    //WallPaper.Style := wlpsTile;
    WallPaper.Style := wlpsStretch;
    WallPaper.FileName := ExtractFilePath(ParamStr(0))+ 'valkyries.png';
  finally
    EndUpdate;
  end; //

        CaptionFont.Size := 26;
        CaptionFont.Name := 'Á¥Êé';
        with CaptionFont.Background do
        try
          BeginUpdate;
          //Gradient.AlphaChannel := True;
          Gradient.ColorBegin := clRed;
          Gradient.ColorEnd := clYellow;
          Gradient.Style := gsLinearV;
        finally
          EndUpdate;
        end;

        Scaled := True;
        //Scaled := False;
      except
        Free;
        raise;
      end;//}
      Label1 := TLabelLayer.Create(Layers);
      with Label1 do
      try
        AlphaBlend := True;
        AlphaBlendValue := 100;
        OnMouseEnter := DoMouseEnter;
        OnMouseLeave :=DoMouseLeave;
        OnMouseMove := DoMouseMove;
        with Image32.GetViewportRect do
          P := Image32.ControlToBitmap(Point(Left, Top));
        Caption := 'Heeloo X:' + IntToStr(P.X) + ' Y:'+IntToStr(P.Y);
        HInt := 'teste Hint';

        Inc(P.X, 40);
        Inc(P.Y, 100);
        //FloatRect: Left, Top, Right, Bottom
        Location := FloatRect(P.X, P.Y, P.X+300, P.Y+148);

  with Background do
  try
    BeginUpdate;
    Enabled := True;
    Gradient.Style := gsNone;
    //WallPaper.Style := wlpsTile;
    WallPaper.Style := wlpsStretch;
    WallPaper.FileName := ExtractFilePath(ParamStr(0))+ 'Lotus_Midnight.jpg';
  finally
    EndUpdate;
  end; //}

        CaptionFont.Size := 15;
        with CaptionFont.Background do
        try
          BeginUpdate;
          //Gradient.AlphaChannel := True;
          Gradient.ColorBegin := clRed;
          Gradient.ColorEnd := clYellow;
          Gradient.Style := gsLinearV;
        finally
          EndUpdate;
        end; //}

        Scaled := True;
        //Scaled := False;
      except
        Free;
        raise;
      end;
    //Gradient.Style := gsNone;
    //WallPaper.Style := wlpsTile;
    //WallPaper.Style := wlpsStretch;
    //WallPaper.FileName := ExtractFilePath(ParamStr(0))+ 'png.png';
  finally
    EndUpdate;
  end;//}

      with TLabel.Create(Self) do
      Begin
      	Parent := Image32;
      	//Parent := Self;
      	autoSize := False;
      	Left := 0; Top := 330;
      	Width := 200; Height := 100;
      	Caption := 'Hello Caption';
      	Hint:='Test hint hello';
      	ShowHint := True;
        OnMouseMove := DoMouseMove;
      end;
      
end;

procedure TMainForm.DoClick(Sender: TObject);
var
  i: integer;
begin
  if Sender is TCustomControl then with Sender as TCustomControl do
  begin
   //SetBounds(Left + 5, Top, Width+1, Height);
   //Left := Left + 5;
   For i := 0 To 150 Do
   Begin
     Left := Left + 1;
     Application.ProcessMessages;
   End; // For
   For i := 0 To 150 Do
   Begin
     Left := Left - 1;
     Application.ProcessMessages;
   End; // For
  end;
end;

procedure TMainForm.DoMouseEnter(Sender: TObject);
var 
  i: integer;
begin
  //showmessage('MouseEnter');
  //Label1.Caption := 'Mouse Enter';
  {if Label1.Top > 200 then
  For i := 0 to 200 do
    Label1.Top := Label1.Top - 1
  else
  For i := 0 to 100 do
  begin
  	Label1.BeginUpdate;
    Label1.Top := Label1.Top + 1;
    Label1.Left := Label1.Left + 1;
    Label1.EndUpdate;
  end;
  //SubPanel2.Caption := 'Mouse Enter';//}
  //Label1.Update;
  //ShowMessage(Label1.Caption);
end;

procedure TMainForm.DoMouseLeave(Sender: TObject);
begin
  //SubPanel2.Caption := 'Leave Mouse Enter';
  //Label1.Caption := 'Mouse Leave';
  //ShowMessage(Label1.Caption);
end;

procedure TMainForm.DoMouseMove(Sender: TObject; Shift: TShiftState; X: Integer; Y: Integer);
begin
  //SubPanel2.Caption := 'Leave Mouse Enter';
  Label1.Caption := Sender.ClassName +#10+ ' X='+IntToStr(x)+';Y='+IntToStr(Y);
  //ShowMessage(Label1.Caption);
end;


end.
