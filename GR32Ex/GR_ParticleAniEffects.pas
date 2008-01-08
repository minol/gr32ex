{ Summary  (General Particle System Animation Effects)  }
{ Description
通用粒子系统动画特效

Usage:
You must set like this:
  Randomize;
with TGRParticlesEffect.Create(FEff) do
begin
  Sprites := aGRSprites;
  //Init the Particle property:
  with Particle do
  begin
    //Looped := False;
    Picture.LoadFromFile('Snow.bmp');
    if Picture.Graphic is TBitmap then
    begin
      TBitmap(Picture.Graphic).Transparent := True;
    end;
  end;

  UseCustomParticle(TGRSnowParticle);
  NumOfParticles :=100;
end;
}
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is GR_ParticleAniEffects
 *
 * The Initial Developer of the Original Code is Riceball LEE
 * Portions created by Riceball LEE are Copyright (C) 2004-2007
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_ParticleAniEffects;

interface

{.$Define DEBUG}

uses
  {$IFDEF DEBUG}
  //DbugIntf,
  CnDebug,
  {$ENDIF}
  Windows, Messages, Classes
  //, Dialogs //ShowMessage
  , SysUtils
  , Graphics
  , Controls
  , Math
  , GR32
  , GR32_Math
  , GR_AniEffects
  , GR_Sprites
  ;

const
  cMaxParticles = 255;
  cDefaultLifeTime = 100;
  cDefaultDecay = 1;
  cDefaultZOrder = 500;
  
type
  TGRParticleClass = class of TGRCustomParticle;
  TGRParticlesEffect = class;
  TParticleProperty = class(TObject)
  private
    FAccelX: TFloat;
    FAccelY: TFloat;
    FAlpha: Byte;
    FDecay: TFloat;
    FLifeTime: TFloat;
    FLooped: Boolean;
    FPicture: TPictureEx;
    FRandomLocation: Boolean;
    FRepeatCount: Integer;
    FRotation: TFloat;
    FScaleX: TFloat;
    FScaleY: TFloat;
    FVelocityX: TFloat;
    FVelocityY: TFloat;
    FX: TFloat;
    FY: TFloat;
    FZ: Integer;
    procedure SetPicture(const Value: TPictureEx);
  public
    constructor Create;
    destructor Destroy; override;
    { Summary 加速度 X分量 }
    property AccelX: TFloat read FAccelX write FAccelX;
    { Summary 加速度 Y分量 }
    property AccelY: TFloat read FAccelY write FAccelY;
    property Alpha: Byte read FAlpha write FAlpha default $FF;
    { Summary 衰减， See 存活期 }
    property Decay: TFloat read FDecay write FDecay;
    { Summary 存活期 }
    property LifeTime: TFloat read FLifeTime write FLifeTime;
    { Summary if true then the particle will never die. }
    property Looped: Boolean read FLooped write FLooped;
    property Picture: TPictureEx read FPicture write SetPicture;
    property RandomLocation: Boolean read FRandomLocation write FRandomLocation;
    property RepeatCount: Integer read FRepeatCount write FRepeatCount;
    property Rotation: TFloat read FRotation write FRotation;
    { Summary 缩放率 X分量 }
    property ScaleX: TFloat read FScaleX write FScaleX;
    { Summary 缩放率 X分量 }
    property ScaleY: TFloat read FScaleY write FScaleY;
    { Summary 自传率 X分量 }
    property VelocityX: TFloat read FVelocityX write FVelocityX;
    { Summary 自传率 X分量 }
    property VelocityY: TFloat read FVelocityY write FVelocityY;
    { Summary 加速度 X分量 }
    property X: TFloat read FX write FX;
    { Summary 加速度 X分量 }
    property Y: TFloat read FY write FY;
    property Z: Integer read FZ write FZ default cDefaultZOrder;
  end;
  
  TGRCustomParticle = class(TGRImageSprite)
  private
    FAccelX: TFloat;
    FAccelY: TFloat;
    FDecay: TFloat;
    FEngine: TGRParticlesEffect;
    FLifeTime: TFloat;
    FLooped: Boolean;
    FRepeatCount: Integer;
    FVelocityX: TFloat;
    FVelocityY: TFloat;
  protected
    FLifeUnderZero: Boolean;
    procedure Reset; virtual;
  public
    destructor Destroy; override;
    { Summary Init the consts of the particle. }
    class procedure InitParticleEngine(const aParticleEngine:
      TGRParticlesEffect); virtual;
    procedure Move(const MoveCount: TFloat); override;
    property Engine: TGRParticlesEffect read FEngine;
  published
    { Summary 加速度 X分量 }
    property AccelX: TFloat read FAccelX write FAccelX;
    { Summary 加速度 Y分量 }
    property AccelY: TFloat read FAccelY write FAccelY;
    { Summary 衰减， See 存活期 }
    property Decay: TFloat read FDecay write FDecay;
    { Summary 存活期 }
    property LifeTime: TFloat read FLifeTime write FLifeTime;
    { Summary if true then the particle will never die. }
    property Looped: Boolean read FLooped write FLooped;
    property RepeatCount: Integer read FRepeatCount write FRepeatCount;
    { Summary 自传率 X分量 }
    property VelocityX: TFloat read FVelocityX write FVelocityX;
    { Summary 自传率 X分量 }
    property VelocityY: TFloat read FVelocityY write FVelocityY;
  end;
  
  TGRParticlesEffect = class(TGRCustomAnimationEffect)
  private
    FMaxParticles: Integer;
    FNumOfParticles: Integer;
    FParticle: TParticleProperty;
    FSprites: TGRSprites;
    procedure SetNumOfParticles(Value: Integer);
  protected
    FBuffer: TBitmap32;
    FParticleClass: TGRParticleClass;
    { Summary not used. }
    FParticles: TList;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); overload;override;
    procedure DoPaint(Sender: TControl; DC: HDC); overload;override;
    procedure DoPaint(Sender: TBitmap32); override;
    procedure DoResize(Sender: TControl); override;
    procedure DoTimer(MoveCount: TFloat); override;
    procedure NewParticle(aParticle: TGRCustomParticle);
    procedure SizeChanged(Sender: TControl); override;
  public
    constructor Create(aOwner: TGRCustomAnimationEffects); override;
    destructor Destroy; override;
    procedure UseCustomParticle(aParticleClass: TGRParticleClass);
    property NumOfParticles: Integer read FNumOfParticles write
      SetNumOfParticles;
    property Particle: TParticleProperty read FParticle write FParticle;
    property Sprites: TGRSprites read FSprites write FSprites;
  published
    property MaxParticles: Integer read FMaxParticles write FMaxParticles
      default cMaxParticles;
  end;
  

implementation

type
  TGRSpritesAccess = class(TGRSprites);
  
constructor TParticleProperty.Create;
begin
  inherited Create;
  FPicture := TPictureEx.Create;
  
  FPicture.DrawMode := dmBlend;
  
  FLifeTime := cDefaultLifeTime;
  FDecay := cDefaultDecay;
  FAlpha := $FF;
  
  FZ := cDefaultZOrder;
  
  FScaleX := 1;
  FScaleY := 1;
end;

destructor TParticleProperty.Destroy;
begin
  FreeAndNil(FPicture);
  inherited Destroy;
end;

procedure TParticleProperty.SetPicture(const Value: TPictureEx);
begin
  if Value <> FPicture then
    FPicture.Assign(Value);
end;

destructor TGRCustomParticle.Destroy;
begin
  //if Assigned(FEngine) then
    //Engine.FParticles.Remove(Self);
  inherited Destroy;
end;

class procedure TGRCustomParticle.InitParticleEngine(const aParticleEngine:
  TGRParticlesEffect);
begin
end;

procedure TGRCustomParticle.Move(const MoveCount: TFloat);
var
  I: Integer;
  randTemp: Integer;
begin
  Left := Left + FVelocityX * MoveCount;
  Top := Top + FVelocityY * MoveCount;
  FVelocityX := FVelocityX + FAccelX * MoveCount;
  FVelocityY := FVelocityY + FAccelY * MoveCount;
  {$IFDEF DEBUG}
  //SendDebug('Move :'+FloatToStr(MoveCount));
  //SendDebug('Move Left:'+FloatToStr(Left));
  //SendDebug('Move VelocityX:'+FloatToStr(FVelocityX));
  {$ENDIF}
  
  {if FLifeunderZero then
  begin
    FLifeUnderZero := False;
    if (FEngine.FParticle.FRandomLocation) then
    begin
      randTemp := RoundNormal(FEngine.FParticle.FLifeTime);
      randTemp := Random(randTemp);
      for i := 0 to randTemp do
          Move(1);
    end;
  end; //}
  
  if FDecay <> 0 then
    FLifeTime := FLifeTime - FDecay * MoveCount;
  if FLifeTime <= 0 then
  begin
    if not FLooped then
    begin
      if FRepeatCount > 0 then
      begin
        Dec(FRepeatCount);
        Reset;
      end
      else
      begin
        Die;
      end;
    end
    else
    begin
      Reset;
    end;
  end;
end;

procedure TGRCustomParticle.Reset;
begin
  Left := FEngine.Particle.X;
  top := FEngine.Particle.Y;
  Z := FEngine.Particle.Z;
  
  AccelX := FEngine.Particle.AccelX;
  AccelY := FEngine.Particle.AccelY;
  VelocityX := FEngine.Particle.VelocityX;
  VelocityY := FEngine.Particle.VelocityY;
  
  if FLifeTime < -1 then
    FLifeUnderZero := True;
  
  
  FLifeTime := FEngine.Particle.FLifeTime;
  FDecay := FEngine.Particle.FDecay;
end;

constructor TGRParticlesEffect.Create(aOwner: TGRCustomAnimationEffects);
begin
  inherited Create(aOwner);
  FParticle := TParticleProperty.Create;
  FBuffer := TBitmap32.Create;
  //FParticles := TList.Create;
  FMaxParticles := cMaxParticles;
  
  FBuffer.DrawMode := dmBlend;
end;

destructor TGRParticlesEffect.Destroy;
begin
  {while FParticles.Count > 0 do
  begin
    TGRCustomParticle(FParticles.Count - 1).FEngine := nil;
  end;//}
  FreeAndNil(FParticle);
  FreeAndNil(FBuffer);
  //FreeAndNil(FParticles);
  inherited Destroy;
end;

procedure TGRParticlesEffect.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  {if PtInRect(Rect(Left, Top, Left+Width, Top+Height), Point(X, Y)) then
  begin
    if ssLeft in Shift then
      Blob(X-Left, Y-Top, 1, FClickBlob)
    else
      Blob(X-Left, Y-Top, 1, FTrackBlob);
  end;
  //}
end;

procedure TGRParticlesEffect.DoPaint(Sender: TBitmap32);
  //var
    //LBg: TBitmap32;
    //s: string
begin
  {$ifdef debug}
    CnDebugger.LogEnter('TGRParticlesEffect.DoPaint');
  {$endif}
  
  //LBg := TBitmap32.Create;
  try
  
  //LBg.SetSize(FWidth, FHeight);
  //BitBlt(LBg.Handle, 0, 0, LBg.Width, LBg.Height, DC, FLeft, FTop, SRCCOPY);
  //LBg.ResetAlpha;
  
  //BitBlt(FBuffer.Handle, 0, 0, FBuffer.Width, FBuffer.Height, DC, FLeft, FTop, SRCCOPY);
  //FBuffer.ResetAlpha;
  //Sender.DrawTo(FBuffer, 0,0, Rect(FLeft, FTop, FLeft+FBuffer.Width, FTop+FBuffer.Height));
  try
    Sprites.Draw(Sender);
  //FParticle.Picture.DrawTo(FBuffer, 0,0);
  except
    {$ifdef debug}
    On e: Exception do
    begin
      SendDebug(E.Message);
      raise;
    end;
    {$endif}
  end;
  //FSprites.Draw(DC, Rect(0,0,Width, Height));
  //FBuffer.DrawTo(LBG, 0, 0);
  
  
  //BitBlt(DC, FLeft, FTop, FBuffer.Width, FBuffer.Height, FBuffer.Handle, 0, 0, SRCCOPY);
  //*)
  
  finally
    //LBg.Free;
  {$ifdef debug}
    CnDebugger.LogLeave('TGRParticlesEffect.DoPaint');
  {$endif}
  end;
end;

procedure TGRParticlesEffect.DoPaint(Sender: TControl; DC: HDC);
  
  //var
    //LBg: TBitmap32;
    //s: string
  
begin
  {$ifdef debug}
    CnDebugger.LogEnter('TGRParticlesEffect.DoPaint');
  {$endif}
  
  //LBg := TBitmap32.Create;
  try
  
  //LBg.SetSize(FWidth, FHeight);
  //BitBlt(LBg.Handle, 0, 0, LBg.Width, LBg.Height, DC, FLeft, FTop, SRCCOPY);
  //LBg.ResetAlpha;
  
  BitBlt(FBuffer.Handle, 0, 0, FBuffer.Width, FBuffer.Height, DC, FLeft, FTop, SRCCOPY);
  FBuffer.ResetAlpha;
  try
    Sprites.Draw(FBuffer);
  //FParticle.Picture.DrawTo(FBuffer, 0,0);
  except
    {$ifdef debug}
    On e: Exception do
    begin
      SendDebug(E.Message);
      raise;
    end;
    {$endif}
  end;
  //FSprites.Draw(DC, Rect(0,0,Width, Height));
  //FBuffer.DrawTo(LBG, 0, 0);
  
  
  //LBg.DrawTo(DC, FLeft, FTop);
  //BitBlt(DC, FLeft, FTop, LBg.Width, LBg.Height, LBg.Handle, 0, 0, SRCCOPY);
  BitBlt(DC, FLeft, FTop, FBuffer.Width, FBuffer.Height, FBuffer.Handle, 0, 0, SRCCOPY);
  //*)
  
  finally
    //LBg.Free;
  {$ifdef debug}
    CnDebugger.LogLeave('TGRParticlesEffect.DoPaint');
  {$endif}
  end;
end;

procedure TGRParticlesEffect.DoResize(Sender: TControl);
begin
  //FWidth := Min(Sender.ClientWidth, 150);
  //FHeight := Min(Sender.ClientHeight, 150);
  //w := Sender.ClientWidth;
  //h := Sender.ClientHeight;
  
  FWidth := Sender.ClientWidth;
  FHeight := Sender.ClientHeight;
  
  SizeChanged(Sender);
end;

procedure TGRParticlesEffect.DoTimer(MoveCount: TFloat);
begin
  FSprites.Move(MoveCount);
  
  if FSprites.Count <> MaxParticles then
    NumOfParticles := MaxParticles;
  
  //FBuffer.Clear(0);
  //FSprites.Draw(FBuffer);
  
  if TGRSpritesAccess(FSprites).FDeadList.Count > 0 then
  begin
    FSprites.CleanDeadSprites;
  end;
  
  
  {$IFDEF DEBUG}
  //SendDebug('DoTimer Sprites:'+IntToStr(FSprites.Count));
  {$ENDIF}
end;

procedure TGRParticlesEffect.NewParticle(aParticle: TGRCustomParticle);
begin
  aParticle.BeginUpdate;
  try
  aParticle.FEngine := Self;
  aParticle.FLooped := FParticle.Looped;
  
  aParticle.Picture := FParticle.Picture;
  aParticle.FRotation  := FParticle.Rotation;
  aParticle.FScaleX    := FParticle.ScaleX;
  aParticle.FScaleY    := FParticle.ScaleY;
  
  aParticle.Reset;
  finally
    aParticle.EndUpdate;
  end;
end;

procedure TGRParticlesEffect.SetNumOfParticles(Value: Integer);
var
  p: TGRCustomParticle;
  I: Integer;
  randTemp: Integer;
  I2: Integer;
begin
  if Value < 0 then Value := 0;  // this way there is no error if the user accidently set then number of particles below zero
  
  if not Assigned(FSprites) then Exit;
  
  
  FNumOfParticles := FSprites.Count;
  {$IFDEF DEBUG}
  //SendDebug('SetNum Sprites:'+IntToStr(Value));
  SendDebug('SetNum CurrSprites:'+IntToStr(FNumOfParticles));
  {$ENDIF}
  
  if Value > FNumOfParticles then
  begin
    for i := FNumOfParticles to Value -1 do
    begin
      p := FParticleClass.Create(FSprites);
  {$IFDEF DEBUG}
  //SendDebug('SetNum Add Sprites:'+IntToStr(FSprites.Count));
  {$ENDIF}
  
      NewParticle(p);
  {$IFDEF DEBUG}
  //SendDebug('SetNum New Sprites:'+IntToStr(FSprites.Count));
  {$ENDIF}
  
      if FParticle.FRandomLocation then
      begin
        randTemp := RoundNormal(FParticle.FLifeTime);
        randTemp := Random(randTemp)+1;
        //for i2 := 0 to randTemp do
            p.Move(randTemp);
      end;
  
      //FParticles.Add(p);
    end;
    FNumOfParticles := Value;
  
  {$IFDEF DEBUG}
  SendDebug('SetNum Done Sprites:'+IntToStr(FSprites.Count));
  {$ENDIF}
  end
  else
  if Value < FNumOfParticles then
  begin
    if FNumOfParticles > FSprites.Count then
      FNumOfParticles := FSprites.Count;
    for i := FNumOfParticles - 1 downto Value do
    begin
      if TGRCustomParticle(FSprites.Items[i]) <> nil then
        TGRCustomParticle(FSprites.Items[i]).Die;
      //FParticles.Delete(i);
    end;
    FNumOfParticles := Value;
    //FSprites.CleanDeadSprites;
  end;
end;

procedure TGRParticlesEffect.SizeChanged(Sender: TControl);
begin
  FBuffer.SetSize(FWidth, FHeight);
end;

procedure TGRParticlesEffect.UseCustomParticle(aParticleClass:
  TGRParticleClass);
begin
  FParticleClass := aParticleClass;
  if Assigned(FParticleClass) then
    FParticleClass.InitParticleEngine(Self);
end;


end.
