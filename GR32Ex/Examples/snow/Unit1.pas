unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls
  , Jpeg
  , GR32, GR32_Resamplers, GR32_Image, GR32_System
  , GR32_Png
  , GR_AniEffects
  , GR_Sprites
  , GR_AniGEffetcts
  , GR_ParticleAniEffects
  //, GR_AniEffectWater
  , GR_ParticleStar
  , GR_ParticleSnow
  , GR_DesktopControl
  , SimpleTimer
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
    FDesktop: TGRDesktopControl;
    FEffEngine: TGRAnimationEffects;
    FStarSprites: TGRSprites;  
    FSnowSprites: TGRSprites;  

  public
    { Déclarations publiques }
    constructor Create(aOwner: TComponent);override;
    destructor Destroy;override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

constructor TForm1.Create(aOwner: TComponent);
var
  LPic: TPicture;
begin
  inherited;
  OpenPlay.Visible := False;
  Snapshot.Visible := False;
  CallBack.Visible := False;
  //Image.Color := clGray;
  //Image.Bitmap.LoadFromFile('res\sky.jpg');
  Caption := 'Desktop Snowflake : Alt+F4 to Close';
  FEffEngine := TGRAnimationEffects.Create();
  FSnowSprites := TGRSprites.Create;  
  FStarSprites := TGRSprites.Create;  

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

  FDesktop := TGRDesktopControl.Create(nil);
  FEffEngine.Control := FDesktop;
  FEffEngine.Enabled := True;

end;

destructor TForm1.Destroy;
begin
	FEffEngine.Enabled := False;
	FreeAndNil(FDesktop);
	FreeAndNil(FSnowSprites);
	FreeAndNil(FStarSprites);
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
