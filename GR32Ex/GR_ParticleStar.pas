{ Description
FGRSprites := TGRSprites.Create;  

with TGRParticlesEffect.Create(FEff) do
begin
  Sprites := FGRSprites;
  //UseCustomParticle(TGRSnowParticle);
  UseCustomParticle(TGRStarParticle);
  //Init the Particle property:
  with Particle do
  begin
  	LPic := TPicture.Create;
  	try
    //Looped := False;
    LPic.LoadFromFile('Snow.png');
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

  MaxParticles := 12;
  NumOfParticles := 1;
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
 * The Original Code is GR_ParticleStar
 *
 * The Initial Developer of the Original Code is
 * Riceball LEE
 *
 * Portions created by Riceball LEE are Copyright (C) 2004-2007
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_ParticleStar;

interface

uses
  {$IFDEF DEBUG}
  //DbugIntf,
  CnDebug,
  {$ENDIF}
  Windows, Messages, Classes, Graphics, Controls
  , SysUtils
  , GR32
  , GR32_Math
  , GR_Sprites
  , GR_AniEffects
  , GR_ParticleAniEffects
  ;

type
  TGRStarParticle = class(TGRCustomParticle)
  private
    angle: TFloat;
    rot: Integer;
    sfacx: TFloat;
    sfacy: TFloat;
    sp: Integer;
    speed: TFloat;
    xpos: TFloat;
    ypos: TFloat;
    zpos: TFloat;
  protected
    procedure Reset; override;
  public
    constructor Create(aOwner: TGRSprites); override;
    class procedure InitParticleEngine(const aParticleEngine:
      TGRParticlesEffect); override;
    procedure Move(const MoveCount: TFloat); override;
  end;
  

implementation

constructor TGRStarParticle.Create(aOwner: TGRSprites);
begin
  inherited Create(aOwner);
  sfacx := 0.5;
  sfacy := 0.5;
  rot := 1;
  //rot := random(3);
  sp := 200 + random(100);
end;

class procedure TGRStarParticle.InitParticleEngine(const aParticleEngine:
  TGRParticlesEffect);
begin
  with aParticleEngine.Particle do
  begin
    //AccelX := 10 + random(100);
    //AccelY := 0;
    //X := -5;
    Decay := 0;
    LifeTime := 1;
  end;
end;

procedure TGRStarParticle.Move(const MoveCount: TFloat);
var
  tempx, tempy: Integer;
begin
  zpos := zpos - speed;
   if (zpos < 0) then
   begin
    //zpos := -10;
    reset;
   end;
   Rotation := Rotation + rot;
   tempx := round(xpos / zpos + Engine.Width / 2);
   tempy := round(ypos / zpos + Engine.Height / 2);
   if (tempx < -5) or (tempx > Engine.Width)
     or (tempy < -5) or (tempy > Engine.Height)
   then
     reset;
   Left := tempx;
   Top := tempy;
   Alpha := 256 - round(zpos);
   ScaleX := (alpha / 200) * sfacx;
   ScaleY := (alpha / 200) * sfacy;
end;

procedure TGRStarParticle.Reset;
begin
   zpos := 200;
   xpos := (-15 + random * 30 ) * zpos * 10;
   ypos := (-15 + random * 30) * zpos * 10;
  
   speed := 0.2 + random * 2;
   speed := speed *  (sp / 50);
   //Alpha := 0;
   //ScaleX := 40;
   //ScaleY := 40;
   Rotation := random(360);
  {
   if colored then
   begin
    red := random(256);
    green := random(256);
    blue := random(256);
   end else
   begin
    red := 255;
    green := 255;
    blue := 255;
   end;
  //}
end;


end.
