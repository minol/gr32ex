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
  //, GR_AniGEffetcts
  , GR_ParticleAniEffects
  //, GR_AniEffectWater
  //, GR_ParticleStar
  , GR_ParticleSnow
  , SimpleTimer
  ;

type
  TGRDesktopEffects = class(TGRCustomAnimationEffects)
  private
    FBuffer: TBitmap32;
    FTempBuffer: TBitmap32; //for TCustomControl, TGraphicControl and TCustomForm
    FDesktopDC: HDC;
  protected
    FTimer: TSimpleTimer;
    FWinStyle: LongInt;
    procedure DoControlResize(Sender: TObject); override;
    procedure DoPaint;
    procedure DoWMPaint(var Message: TWMPaint); override;
    procedure HookControl(Value: TControl; Hooked: Boolean); override;
    procedure InternalDoTimer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

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
    FEffEngine: TGRDesktopEffects;
    //FStarSprites: TGRSprites;  
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

constructor TGRDesktopEffects.Create;
begin
  inherited Create;
  FBuffer := TBitmap32.Create;
  FDesktopDC := 0;
end;

destructor TGRDesktopEffects.Destroy;
begin
  FreeAndNil(FBuffer);
  FreeAndNil(FTempBuffer);
  if FDesktopDC <> 0 then ReleaseDC(0, FDesktopDC);
  inherited Destroy;
end;

procedure TGRDesktopEffects.DoControlResize(Sender: TObject);
begin
	//if Sender is TCustomPaintBox32 then
  //with TCustomPaintBox32(Sender) do
  try
    FDrawing := True;
    //if not Assigned(FBuffer) then FBuffer := TBitmap32.Create;
    //Resize;
    //Repaint;
    //FBuffer.Assign(Buffer);
    FBuffer.Delete;
    //FControl.Repaint;
  finally
    FDrawing := False;
  end;
  //}
  inherited DoControlResize(Sender);
end;

procedure TGRDesktopEffects.DoPaint;
begin
  {$ifdef Debug}
  //SendDebug('Paint.DC=' + IntToStr(DC));
  {$endif}
  
  if FDesktopDC <> 0 then
  try
  //FControl.Invalidate;
  //FControl.Update;
  //FControl.Refresh;
   if FBuffer.Empty then
   begin
      FBuffer.SetSize(GetDeviceCaps(FDesktopDC,HORZRES), GetDeviceCaps(FDesktopDC,VERTRES));
      //FControl.Repaint;
      BitBlt(FBuffer.Handle, 0, 0, FBuffer.Width, FBuffer.Height, FDesktopDC, 0, 0, SRCCOPY);
      FBuffer.ResetAlpha;
    end;
  
    //FillRect(DC, FControl.ClientRect, $FFFFFFFF);
    //DoControlPaint(FControl, DC);
    FTempBuffer.Assign(FBuffer);
   	DoControlPaint(FTempBuffer);
    BitBlt(FDesktopDC, 0, 0, FBuffer.Width, FBuffer.Height, FTempBuffer.Handle, 0, 0, SRCCOPY);

    {$ifdef Debug}
    //TCustomControlAccess(FControl).Canvas.TextOut(0,0,'dddffdf');
    {$endif}
  finally
    //ReleaseDC(DC);
  end;
  
  
  ///InvalidateRect(DC, nil, true);
end;

procedure TGRDesktopEffects.DoWMPaint(var Message: TWMPaint);
begin
end;

procedure TGRDesktopEffects.HookControl(Value: TControl; Hooked: Boolean);
var
  LStyle: LongInt;
begin
  if Hooked then
  begin
    FTimer := TSimpleTimer.CreateEx(cMinIntervalValue, DoTimer);
    FTempBuffer := TBitmap32.Create;
    FDesktopDC := GetDC(0);
  end
  else begin
    FreeAndNil(FTimer);
    FreeAndNil(FBuffer);
    FreeAndNil(FTempBuffer);
    ReleaseDC(0, FDesktopDC);
    FDesktopDC := 0;
  end;
  //inherited HookControl(Value, Hooked);
  if Hooked then
  begin
    FTimer.Enabled := True;
  end;
end;

procedure TGRDesktopEffects.InternalDoTimer;
begin
  //InvalidateRect(TCustomControl(FControl).Handle, nil, false);
  //FControl.Update;
  
  //FControl.Invalidate;
  //FControl.Update;
  //FControl.Repaint;

  if not FDrawing then
  begin
    DoPaint;
  end;
  inherited InternalDoTimer;
end;

constructor TForm1.Create(aOwner: TComponent);
var
  LPic: TPicture;
begin
  inherited;
  OpenPlay.Visible := False;
  Snapshot.Visible := False;
  CallBack.Visible := False;
  //Image.Color := clGray;
  Image.Bitmap.LoadFromFile('res\sky.jpg');
  //Image.Visible := False;
  //Image.DoubleBuffered :=True;
  //FBitmap := TBitmap.Create;
  //FBitmap32:= TBitmap32.Create;
  //TDraftResampler.Create(FBitmap32);
  //TLinearResampler.Create(FBitmap32);
  //with TMitchellKernel.Create(FBitmap32) do Kernel := TSplineKernel.Create;
  //with TKernelResampler.Create(FBitmap32) do Kernel := TSplineKernel.Create;
  FEffEngine := TGRDesktopEffects.Create();
  //FStarSprites := TGRSprites.Create;  
  FSnowSprites := TGRSprites.Create;  

  {with TGRWaterAnimationEffect.Create(FEffEngine) do
  begin
  	ClickBlob := 800;
  end; //}

  {//the star particle animation effect
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
  end; //}

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
  FEffEngine.Enabled := True;

end;

destructor TForm1.Destroy;
begin
	FEffEngine.Enabled := False;
	//FreeAndNil(FStarSprites);
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
