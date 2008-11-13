unit MainUnit;

interface

{$Define Debug}
{.$Define GR_FORM}

uses
  {$ifdef Debug}
  DbugIntf,
  {$endif} 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs
  , Jpeg
  , pngimage
  //, GraphicEx
  , GR32, GR32_Image, GR32_Filters, GR32_Transforms, GR32_Blend
  , GR_BitmapEx
  , GR_Graphics, GR_Controls
  , GR_FilterEx
  , GR_GraphUtils
  , GR_StdCtrls
  , GR_Edits
  //, psvShine
{$IFDEF GR_FORM}
  , GR_Forms
{$ENDIF}
  ;

type
  TMainForm = class({$IFDEF GR_FORM}TGRCustomShapeForm{$ELSE}TForm{$ENDIF})
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    LMainPanel: TGRPanel;
    SubPanel1: TGRPanel;
    SubPanel2: TGRPanel;
    Label1: TGRLabel;
    gbMain: TGRGroupBox;
    Btn1: TGRButton;
    //Shine1: TpsvShine;
    StyleCtrl: TGRStyleController;
    Edit1: TGREdit;
    procedure DoClick(Sender: TObject);
    procedure DoMouseEnter(Sender: TObject);
    procedure DoMouseLeave(Sender: TObject);
    procedure DoBtnClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
	StyleCtrl := TGRStyleController.Create(Self);
	with StyleCtrl.NormalStyle do
	try
	  BeginUpdate;
	  Enabled := True;
	  Color := clBlue;

    with Background do
    try
      BeginUpdate;
	    Enabled := False;
      Gradient.Enabled := True;
      Gradient.Style := gsPattern;
      Gradient.PatternFile := 'Background\button_Mask_Down.png';
      Mask.LoadFromFile('Background\button_Btn_Down_Mask.bmp');
      //WallPaper.Style := wlpsTile;
      WallPaper.Style := wlpsStretch;
      WallPaper.FileName := ExtractFilePath(ParamStr(0))+ 'Background\button_Btn_UP.png';
      //WallPaper.Alpha := 200;
    finally
      EndUpdate;
    end; //}

    with Frame do
    begin
      Enabled := True;
      FrameStyle := fsImage;
      //FrameSides := [sdLeft, sdBottom];
      Width := 3;
      Alpha := 255;
      Color := clRed;
      with ImageFrame do
      begin
        Enabled := True;
        //or PictureLeft.FileNameLeft := 'Background\left.png';
        LoadPicture(PictureLeft,'Background\left.png');
        LoadPicture(PictureLeftTop,'Background\topleft.png');
        LoadPicture(PictureTop,'Background\top.png');
        LoadPicture(PictureRightTop,'Background\topright.png');
        LoadPicture(PictureRight, 'Background\right.png');
        LoadPicture(PictureRightBottom,'Background\bottomright.png');
        LoadPicture(PictureLeftBottom, 'Background\bottomleft.png');
        LoadPicture(PictureBottom, 'Background\bottom.png');
      end;

    end; //}
	finally
	  EndUpdate;
	end;
	with StyleCtrl.HotStyle do
	try
	  BeginUpdate;
	  Enabled := True;
	  Color := clWhite;
	  //Frame.Assign(StyleCtrl.NormalStyle.Frame);
    with Background do
    try
      BeginUpdate;
      Enabled := False;
      Gradient.Enabled := True;
      Gradient.Style := gsPattern;
      Gradient.PatternFile := 'Background\button_Mask_Down.png';
      Gradient.AlphaChannel := True;
      Mask.LoadFromFile('Background\button_Btn_Down_Mask.bmp');
      //WallPaper.Style := wlpsTile;
      WallPaper.Style := wlpsStretch;
      WallPaper.FileName := ExtractFilePath(ParamStr(0))+ 'Background\button_Btn_Hot.png';
      //WallPaper.Alpha := 120;
    finally
      EndUpdate;
    end; //}

	finally
	  EndUpdate;
	end;

	with StyleCtrl.DownStyle do
	try
	  BeginUpdate;
	  Enabled := True;
    with Background do
    try
      BeginUpdate;
      Gradient.Enabled := True;
      Gradient.Style := gsPattern;
      Gradient.PatternFile := 'Background\button_Mask_Down.png';
      Mask.LoadFromFile('Background\button_Btn_Down_Mask.bmp');
      //WallPaper.Style := wlpsTile;
      WallPaper.Style := wlpsStretch;
      WallPaper.FileName := ExtractFilePath(ParamStr(0))+ 'Background\button_Btn_Down.png';
      //WallPaper.Alpha := 120;
    finally
      EndUpdate;
    end; //}

	finally
	  EndUpdate;
	end;

  {with Image32 do
  try
    BeginUpdate;
    Gradient.Style := gsNone;
    //WallPaper.Style := wlpsTile;
    WallPaper.Style := wlpsStretch;
    WallPaper.FileName := ExtractFilePath(ParamStr(0))+ 'Background\png.png';
  finally
    EndUpdate;
  end;//}

  //LMainPanel
  LMainPanel := TGRPanel.Create(Self);
  LMainPanel.Parent := Self;
  LMainPanel.Name := 'LMainPanel';
  LMainPanel.Align := alClient;
  with LMainPanel do
  try
    BeginUpdate;
    Align := alClient;
    //FastDraw := True;
    DragAsTitle := True;
    //Bug: here
    //AlphaBlend := True;
    //AlphaBlendValue := 100;
    //Transparent := True;
  finally
    EndUpdate;
  end;//}

  {LMainPanel.Left := 0;
  LMainPanel.Top := 0;
  LMainPanel.Width := 400;
  LMainPanel.Height := 400; //}
  with LMainPanel.Background do
  try
    BeginUpdate;
    Gradient.Style := gsNone;
    //WallPaper.Style := wlpsTile;
    WallPaper.Style := wlpsStretch;
    WallPaper.FileName := ExtractFilePath(ParamStr(0))+ 'Background\png1.png';
  finally
    EndUpdate;
  end; //}
  //*)

{$IFDEF GR_FORM}
  //GRForm
  //BorderIcons := [];
  //BorderStyle := bsNone;
  MainPanel := LMainPanel;
  //*)
{$ENDIF}

  SubPanel1 := TGRPanel.Create(Self);
  SubPanel1.Parent := LMainPanel;

  with SubPanel1 do
  try
    BeginUpdate;
    Left := 0;
    Top := 0;
    Width := 300;
    Height := 300;

    AlphaBlend := True;
    AlphaBlendValue := 155;
    //Transparent := True;
    Name := 'SubPanel1';
    Caption := 'Test it heeeloo'#10'OK';
    //Caption := '猪头!!!!';
    //CaptionFont.Background.Enabled := False;
    //CaptionFont.Name := '宋体';
    CaptionFont.Size := 42;
    CaptionFont.Color := clBlack;
    CaptionFont.Style := [fsBold];
    CaptionFont.Quality := fqNormal;
    //CaptionFont.Outline := True;
    with CaptionFont.Shadow do
    try
      BeginUpdate;
      Enabled := True;
    finally
      EndUpdate;
    end; //}

    with CaptionFont.Background do
    try
      BeginUpdate;
      Enabled := True;
      //Gradient.AlphaChannel := True;
      Gradient.Enabled := True;
      Gradient.ColorBegin := clRed;
      Gradient.ColorEnd := clYellow;
      Gradient.Style := gsLinearV;
    finally
      EndUpdate;
    end; //}

    BevelOuter := bvLowered;
    BevelWidth := 3;
    //BevelInner := bvRaised;
    OnClick := DoClick;
    with Frame do
    begin
      Enabled := True;
      FrameStyle := fsImage;
      //FrameSides := [sdLeft, sdBottom];
      Width := 3;
      Alpha := 255;
      Color := clRed;
      with ImageFrame do
      begin
        Enabled := True;
        PictureTop.LoadFromFile('Background\2.png');
      end;

    end; //}
    with FrameHot do
    begin
      Enabled := True;
      FrameStyle := fsButtonUp;//fsRaised;
      //FrameSides := [sdLeft, sdBottom];
      Width := 2;
      Alpha := 255;
      Color := clRed;
    end;//}

    //Align := alClient;
    //Background.Enabled := false;
    with Background do
    try
      BeginUpdate;
      //Buffered := True;
      //Gradient.Enabled := True;
      Gradient.AlphaChannel := True;
      Gradient.Style := gsPattern;
      Gradient.patternFile := ExtractFilePath(ParamStr(0)) + 'Background\weightmap.jpg';
      Gradient.Tiled := True;
      WallPaper.Style := wlpsTile;
      //
      WallPaper.FileName := ExtractFilePath(ParamStr(0))+ 'Background\Blue Lace 16.bmp';
      Texture.Style := wlpsTile;
      //Texture.FileName := ExtractFilePath(ParamStr(0))+ 'Background\texture_b.jpg';
      Texture.Alpha := 200;
    finally
      EndUpdate;
    end; //}
  finally
    EndUpdate;
  end; //}

  SubPanel2 := TGRPanel.Create(Self);
  SubPanel2.Parent := LMainPanel;

  with SubPanel2 do
  try
    BeginUpdate;
    //StyleController := StyleCtrl;
    Name := 'SubPanel2';
    OnMouseEnter := DoMouseEnter;
    OnMouseLeave := DoMouseLeave;
    //Transparent := True;
    Caption := '你好测试';
    CaptionFont.Name := '隶书';
    CaptionFont.Size := 40;
    CaptionFont.Color := clBlack;
    CaptionFont.Style := [fsBold];
    CaptionFont.Quality := fqNormal;
    //CaptionFont.Outline := True;
    //BevelOuter := bvNone;
    BevelOuter := bvLowered;
    Color := clNone;
    with CaptionFont.Shadow do
    try
      BeginUpdate;
      Enabled := True;
      Opacity := 200;
      Blur := 15;
    finally
      EndUpdate;
    end; //}
    with CaptionFont.Background do
    try
      BeginUpdate;
      Enabled := True;
      Gradient.Enabled := True;
      Gradient.AlphaChannel := True;
      Gradient.Tiled := True;
      WallPaper.Style := wlpsTile;
      WallPaper.FileName := ExtractFilePath(ParamStr(0))+ 'Background\texture_a.jpg';
      Texture.Style := wlpsTile;
      Texture.FileName := ExtractFilePath(ParamStr(0))+ 'Background\texture_b.jpg';
      //Texture.Alpha := 200;
    finally
      EndUpdate;
    end; //}
    Left := 100;
    Top := 200;
    Width := 350;
    Height := 150;
    //AlphaBlend := True;
    //AlphaBlendValue := 220;
    //Transparent := True;
    OnClick := DoClick;
    {//Background.Enabled := False;
    with Background do
    try
      BeginUpdate;
      Gradient.Enabled := True;
      Gradient.Style := gsPattern;
      //Gradient.PatternFile := 'Panel_Mask_UP.png';
      //Mask.LoadFromFile('Panel_Btn_Down_Mask.bmp');
      //WallPaper.Style := wlpsTile;
      WallPaper.Style := wlpsStretch;
      WallPaper.FileName := ExtractFilePath(ParamStr(0))+ 'Background\valkyries.png';
    finally
      EndUpdate;
    end; //}
  finally
    EndUpdate;
  end; //}

  {Label1 := TGRLabel.Create(Self);
  Label1.Parent := SubPanel2;
  Label1.Caption := '&中国你好ddddd好好好!OK'#13#10'Next Line!'#13#10'4';
  Label1.Left := 0;
  Label1.Top := 0;
  //Label1.AutoSize := False;
  //Label1.Enabled := False;

  with Label1 do
  try
    BeginUpdate;
    AutoSize := False;
    Width := 100;
    Height := 300;
    //OnMouseEnter := DoMouseEnter;
    //OnMouseLeave := DoMouseLeave;
    //WordWrap := True;
    //ParentFont := False;
    AlphaBlend := True;
    AlphaBlendValue := 110;
    //Transparent := True;
    Alignment := taRightJustify;
    //Alignment := taCenter;
    CaptionFont.Name := 'Arial';
    CaptionFont.Size := 20;
    CaptionFont.Color := clBlack;
    CaptionFont.Style := [fsBold];
    CaptionFont.Quality := fqNormal;
    //CaptionFont.Outline := True;
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
  finally
    EndUpdate;
  end; //}
  
  Btn1 := TGRButton.Create(Self);
  with Btn1 do
  try
    BeginUpdate;
    Parent := SubPanel1;
    //StyleController := StyleCtrl;
    Transparent := True;
    Btn1.Color := clNone;
    //Flat := True;
    ActiveTextColor := clWhite;
    //Enabled := False;
    //Flat := True;
    //AlphaBlend := True;
    //AlphaBlendValue := 150;
    //Glyph.Transparent := True;
    //Glyph.TransparentMode := tmAuto;
    //LoadPicture(Glyph, 'Background\123.png');
    Glyph.LoadFromFile('Background\123.png');
    //Glyph.LoadFromFile('systray.bmp');
    //ApplyTransparentColor(Glyph, Glyph.Bits[0]);
    GroupIndex := 0;
    //AllowAllUp := True;    
    Caption := 'Text';
    ShowCaption := False;
    CaptionFont.Color := clSkyBlue;
    CaptionFont.Background.Enabled := False;
    Width := 69;//Round(Glyph.Width * AniMaxZoom);
    Height := 69;//Round(Glyph.Height * AniMaxZoom);
    Left := Parent.Width - Width - 20;
    Top := 20;
    OnClick := DoBtnClick;
  finally
    EndUpdate;
  end;

  {Shine1 := TpsvShine.Create(Self);
  with Shine1 do
  begin
  	//Control := Btn1;
  	Control := SubPanel2;
  	Position := spMousePos;
  	Color := clRed;
  end; //}

  with TGREdit.Create(Self) do
  try
    BeginUpdate;
    AutoSize := False;
    Left := 50;
    Top :=50;
    Width := 100;
    Height := 68;
    Parent := SubPanel2;
    //Transparent := True;
    Color := clBlack;
    //AlphaBlend := True;
    //AlphaBlendValue := 155;
    Text := '打响';
    CaptionFont.Color := clWhite;
    CaptionFont.Background.Enabled := False;
    Background.Enabled := False;
  finally
    EndUpdate;
  end;//}

  {gbMain := TGRGroupBox.Create(Self);
  with gbMain do
  try
    BeginUpdate;
    Left := 50;
    Parent := SubPanel2;
    //Transparent := True;
    //AlphaBlend := True;
    AlphaBlendValue := 150;
    Caption := 'Text';
    CaptionFont.Color := clBlue;
    CaptionFont.Background.Enabled := False;
    Width := 150;
  finally
    EndUpdate;
  end;

  with TGRCheckBox.Create(Self) do
  try
    BeginUpdate;
    Top := 20;
    Left := 50;
    Parent := gbMain;
    //Transparent := True;
    //AlphaBlend := True;
    //AlphaBlendValue := 150;
    Caption := 'Text';
    //CaptionFont.Color := clBlue;
    //CaptionFont.Background.Enabled := False;
    Width := 50;
  finally
    EndUpdate;
  end;//}
  //*)
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
     Sleep(1);
   End; // For
   For i := 0 To 150 Do
   Begin
     Left := Left - 1;
     Application.ProcessMessages;
     Sleep(1);
   End; // For
  end;
end;

procedure TMainForm.DoMouseEnter(Sender: TObject);
begin
  Label1.Caption := 'Mouse Enter';
  SubPanel2.Caption := 'Mouse Enter';
  //Label1.Update;
  //ShowMessage(Label1.Caption);
end;

procedure TMainForm.DoMouseLeave(Sender: TObject);
begin
  SubPanel2.Caption := 'Leave Mouse Enter';
  SubPanel1.Caption := 'Leave Mouse Enter';
  Label1.Caption := 'Mouse Leave';
  {//ShowMessage(Label1.Caption);
	if Btn1.Down then
    Btn1.Caption := 'Down'
  else
    Btn1.Caption := 'UP';
  //}
end;

procedure TMainForm.DoBtnClick(Sender: TObject);
begin
	{//ShowMessage('ddd');
	Shine1.Active := Btn1.Down;
	if Btn1.Down then
    Btn1.Caption := 'Down'
  else
    Btn1.Caption := 'UP';
  Label1.Caption := Btn1.Caption;
  //}
end;

end.
