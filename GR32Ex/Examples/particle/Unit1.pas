unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls
  , Jpeg
  , GR32, GR32_Resamplers, GR32_Image, GR32_System
  , GR32_ExtLayers
  , GR32_Png
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
    FEffEngine: TGRAnimationEffects;
    FStarSprites: TGRSprites;  
    FSnowSprites: TGRSprites;  
    FTimer: TTimer;
    vLayer, vLayer2: TGRBitmapLayer;
  public
    { Déclarations publiques }
    constructor Create(aOwner: TComponent);override;
    destructor Destroy;override;
    procedure DoTimer(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.DoTimer(Sender: TObject);
begin
  vLayer.Angle:= (vLayer.Angle + 1.0);
  if vLayer.Angle > 360 then
    vLayer.Angle := 0;
end;

constructor TForm1.Create(aOwner: TComponent);
var
  LPic: TPicture;
  vLayerContainer: TGRLayerContainer;
  //vLayer, vLayer2: TGRBitmapLayer;
begin
  inherited;
  OpenPlay.Visible := False;
  Snapshot.Visible := False;
  CallBack.Visible := False;
  //Image.Color := clGray;
  Image.Bitmap.LoadFromFile('res\sky.jpg');
  vLayerContainer := TGRLayerContainer.Create(Image.Layers);
  vLayerContainer.DrawMode := dmBlend;
  vLayerContainer.Width := 100;
  vLayerContainer.Height := 100;
  with vLayerContainer.Scaling do
  begin
    x := 2;
    y := 2;
  end;
  
  vLayer := TGRBitmapLayer.Create(vLayerContainer.Layers);
  vLayer.Bitmap.LoadFromFile('res\stars\sc-bluestars1.png');
  with vLayer.Scaling do
  begin
    X := 0.3;
    Y := 0.3;
  end;
  with vLayer.Position do
  begin
    x := 0;
    y:= 0;
  end;

  vLayer2 := TGRBitmapLayer.Create(Image.Layers);
  vLayer2.Bitmap.Assign(vLayer.Bitmap);
  with vLayer2.Position do
  begin
    x := 50;
  end;

  vLayer := TGRBitmapLayer.Create(vLayerContainer.Layers);
  vLayer.Bitmap.Assign(vLayer2.Bitmap);
  with vLayer.Scaling do
  begin
    X := 0.2;
    Y := 0.2;
  end;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 30;
  FTimer.OnTimer:= DoTimer;
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
