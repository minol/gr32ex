unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls
  , Jpeg
  , GR32, GR32_Resamplers, GR32_Image, GR32_System
  //, GR32_ExtLayers
  , GR32_Png
  , GR_Layers
  , GR_AniEffects
  , GR_Sprites
  , GR_AniGEffetcts
  , GR_ParticleAniEffects
  , GR_AniEffectWater
  , GR_ParticleStar
  , GR_ParticleSnow
  ;

type
  TForm1 = class(TForm)
    Image: TImage32;
    OpenPlay: TButton;
    Snapshot: TButton;
    OpenDialog: TOpenDialog;
    CallBack: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Déclarations privées }
    //FBitmap: TBitmap;
    //FBitmap32: TBitmap32;
    //FLastTick: Integer;
    FTimer: TTimer;
    FEffEngine: TGRAnimationEffects;
    FStarSprites: TGRSprites;  
    FSnowSprites: TGRSprites;  
    vLayer, vLayer2: TGRBitmapLayer;
    vLayerContainer: TGRLayerContainer;
    FGB: TGroupBox;
  public
    { Déclarations publiques }
    constructor Create(aOwner: TComponent);override;
    destructor Destroy;override;
    procedure DoTimer(Sender: TObject);
    procedure DoBtnClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.DoTimer(Sender: TObject);
begin
  vLayerContainer.BeginUpdate;
  try
    with vLayer.Scaling do
    Begin
      if x >= 0.08 then 
        x := x - 0.0005;
      if y >= 0.08 then 
        y := y - 0.0005;
    end;
    vLayer.Angle:= (vLayer.Angle + 1.0);
    if vLayer.Angle > 360 then
      vLayer.Angle := 0;
    vLayerContainer.Angle:= (vLayerContainer.Angle + 1.0);
    if vLayerContainer.Angle > 360 then
      vLayerContainer.Angle := 0;
    {with vLayer.Scaling do
    begin
      if x >= 0.3 then 
        x := x - 0.01
      else
        x := x + 0.01;
      if y >= 0.3 then 
        y := y - 0.01
      else
        y := y + 0.01;
    end; //}
    vLayerContainer.Changed;
  finally
  vLayerContainer.EndUpdate;
  vLayerContainer.Changed;
  end;
end;

procedure TForm1.DoBtnClick(Sender: TObject);
begin
  {FGB.PaintTo(vLayer.Bitmap.Canvas, 0, 0);
  vLayer.Bitmap.ResetAlpha;
  vLayer.Changed;
  {
  OpenPlay.visible := True;
  OpenPlay.PaintTo(vLayer.Bitmap.Canvas, 0, 0);
  //vLayer.Bitmap.ResetAlpha;
  vLayer.Changed;
  OpenPlay.visible := False;
  }
end;

constructor TForm1.Create(aOwner: TComponent);
var
  LPic: TPicture;
  //vLayer, vLayer2: TGRBitmapLayer;
begin
  inherited;
  {
  FGB := TGroupBox.Create(nil);
  with FGB do
  begin
    Parent := Self;
    Visible := False;
    Caption := 'VBHello';
    Width := 200;
    Height := 200;
  end; //}
  OpenPlay.Visible := False;
  Snapshot.Visible := False;
  CallBack.Visible := False;
  Snapshot.OnClick := DoBtnClick;
  //Image.Color := clGray;
  Image.Bitmap.LoadFromFile('res\sky.jpg');
  vLayerContainer := TGRLayerContainer.Create(Image.Layers);
  vLayerContainer.DrawMode := dmBlend;
  vLayerContainer.Left := 0;
  vLayerContainer.Top := 0;
  vLayerContainer.Width := 100;
  vLayerContainer.Height := 100;
  with vLayerContainer.PivotPoint do
  begin
    X := vLayerContainer.Width div 2;
    Y := vLayerContainer.Height div 2 ;
  end;
  {with vLayerContainer.Scaling do
  begin
    x := 2;
    y := 2;
  end;}
  
  vLayer := TGRBitmapLayer.Create(vLayerContainer.Layers);
  vLayer.Bitmap.LoadFromFile('res\stars\sc-bluestars1.png');
  with vLayer.Scaling do //this means visit the FScaling field directly!!
  begin
    X := 0.3;
    Y := 0.3;
  end;
  with vLayer.Position do
  begin
    x := 0;
    y:= 0;
  end;
  
  vLayer.Changed;

  vLayer2 := TGRBitmapLayer.Create(Image.Layers);
  vLayer2.Bitmap.Assign(vLayer.Bitmap);
  with vLayer2.Position do
  begin
    x := 50;
  end;

  vLayer2.Changed;

  vLayer := TGRBitmapLayer.Create(vLayerContainer.Layers);
  vLayer.Bitmap.Assign(vLayer2.Bitmap);
  with vLayer.Position do
  begin
    x := 20;
    y:= 20;
  end;
  with vLayer.Scaling do
  begin
    X := 0.2;
    Y := 0.2;
  end;
  with vLayer.PivotPoint do
  begin
    X := vLayer.Width div 2;
    Y := vLayer.Height div 2;
  end;
  vLayer.Changed; 
  //}
  vLayerContainer.BringToFront;
  FTimer:= TTimer.Create(Self);
  FTimer.OnTimer:= DoTimer;
  FTimer.Interval := 30;
  FTimer.Enabled := True;

  
  //Image.Visible := False;
  //Image.DoubleBuffered :=True;
  //FBitmap := TBitmap.Create;
  //FBitmap32:= TBitmap32.Create;
  //TDraftResampler.Create(FBitmap32);
  //TLinearResampler.Create(FBitmap32);
  //with TMitchellKernel.Create(FBitmap32) do Kernel := TSplineKernel.Create;
  //with TKernelResampler.Create(FBitmap32) do Kernel := TSplineKernel.Create;
  FEffEngine := TGRAnimationEffects.Create();
  FStarSprites := TGRSprites.Create;  
  FSnowSprites := TGRSprites.Create;  
  with TGRWaterAnimationEffect.Create(FEffEngine) do
  begin
  	ClickBlob := 800;
  end;

  //the star particle animation effect
  with TGRParticlesEffect.Create(FEffEngine) do
  begin
    Sprites := FStarSprites;
    //UseCustomParticle(TGRSnowParticle);
    UseCustomParticle(TGRStarParticle);
    //Init the Particle property:
    with Particle do
    begin
    	LPic := TPicture.Create;
    	try
      //Looped := False;
      LPic.LoadFromFile('Res\pao.png');
      if LPic.Graphic is TBitmap then
      begin
        TBitmap(LPic.Graphic).Transparent := True;
        //TBitmap(LPic.Graphic).TransparentColor := clBlack;
      end;
      Picture.Assign(LPic);
      finally
        LPic.Free;
      end;
    end;

    Enabled := True;
    MaxParticles := 50;
    NumOfParticles := 2;
  end;

  //the snow particle animation effect
  with TGRParticlesEffect.Create(FEffEngine) do
  begin
    Sprites := FSnowSprites;
    UseCustomParticle(TGRSnowParticle);
    //Init the Particle property:
    with Particle do
    begin
    	LPic := TPicture.Create;
    	try
      //Looped := False;
      LPic.LoadFromFile('Res\snow.png');
      if LPic.Graphic is TBitmap then
      begin
        TBitmap(LPic.Graphic).Transparent := True;
        //TBitmap(LPic.Graphic).TransparentColor := clBlack;
      end;
      Picture.Assign(LPic);
      finally
        LPic.Free;
      end;
    end;
  
    Enabled := True;
    MaxParticles := 500;
    NumOfParticles := 200;
  end;

  FEffEngine.Control := Image;
  //FEffEngine.Control := Self;
  //FEffEngine.OnUpdating := DoTimer;

  //FEffEngine.Enabled := True;

end;

destructor TForm1.Destroy;
begin
	FEffEngine.Enabled := False;
	FreeAndNil(FStarSprites);
	FreeAndNil(FSnowSprites);
	FEffEngine.Free;
  //FBitmap.Free;
  //FBitmap32.Free;
  inherited;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CallBack.Checked := false;
end;

end.
