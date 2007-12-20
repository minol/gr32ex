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
 * The Original Code is GR_Effects
 *
 * The Initial Developer of the Original Code is Riceball LEE
 * Portions created by Riceball LEE are Copyright (C) 2004-2007
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_Effects;

interface

uses
  Windows, Messages, 
  SysUtils, Classes, Graphics
  , GR32
  , GR32_Transforms
  , GR32_Filters
  , GR_FilterEx
  , GR_GraphUtils
  ;

const
  DefaultShadowOffsetX = 8;
  DefaultShadowOffsetY = 8;
  DefaultShadowColor = clBlack;
  DefaultShadowBlur = 2;
  DefaultShadowOpacity = 256 div 2;

type
  TCustomEffectProperty = class(TCustomGraphicProperty)
  end;
  
  { Summary the drop shadow effect. }
  TShadowEffect = class(TCustomEffectProperty)
  private
    FBlur: Byte;
    FColor: TColor;
    FEnabled: Boolean;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FOpacity: Byte;
    procedure SetBlur(const Value: Byte);
    procedure SetColor(const Value: TColor);
    procedure SetEnabled(const Value: Boolean);
    procedure SetOffsetX(const Value: Integer);
    procedure SetOffsetY(const Value: Integer);
    procedure SetOpacity(const Value: Byte);
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    { Summary Generate drop Shadow from Src }
    procedure GenerateShadow(aSrc, aDst: TBitmap32; R:TRect);
    procedure PaintTo(aSrc, aDst: TBitmap32; aR: TRect; aDstX, aDstY: integer);
      overload; override;
    procedure SetOffset(const X, Y: Integer);
  published
    property Blur: Byte read FBlur write SetBlur default DefaultShadowBlur;
    { Summary The shadow color. }
    property Color: TColor read FColor write SetColor default
      DefaultShadowColor;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property OffsetX: Integer read FOffsetX write SetOffsetX default
      DefaultShadowOffsetX;
    property OffsetY: Integer read FOffsetY write SetOffsetY default
      DefaultShadowOffsetY;
    property Opacity: Byte read FOpacity write SetOpacity default
      DefaultShadowOpacity;
  end;
  

implementation

constructor TShadowEffect.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FOffsetX := DefaultShadowOffsetX;
  FOffsetY := DefaultShadowOffsetY;
  FBlur := DefaultShadowBlur;
  FColor := clBlack;
  FOpacity := DefaultShadowOpacity;
end;

procedure TShadowEffect.Assign(Source: TPersistent);
begin
  if Source is TShadowEffect then
    with Source as TShadowEffect do
    begin
      Self.BeginUpdate;
      try
        Self.FBlur := Blur;
        Self.FOffsetX := OffsetX;
        Self.FOffsetY := OffsetY;
        Self.FColor := Color;
        Self.FOpacity := Opacity;
        Self.FEnabled := Enabled;
      finally
        Self.EndUpdate;
      end;
    end
  else
    inherited Assign(Source);
end;

procedure TShadowEffect.GenerateShadow(aSrc, aDst: TBitmap32; R:TRect);
var
  I: Integer;
begin
  //CheckParams(Dst, Src);
  aDst.SetSize(R.Right - R.Left, R.Bottom - R.Top);
  aDst.Draw(0,0,R,aSrc);
  aDst.DrawMode := dmBlend;
  if Color <> clNone then
    ApplyBWImage(aDst, Color32(Color), 0);
  for i := 0 to FBlur - 1 do
    SplitBlur(aDst, 1);
  //ConvolveI5x5(SoftenFilter5x5, aDst);
  //with LowPassFilter3x3 do
    //ConvolveI(Ray, FBlur, aDst, Offset);
  aDst.MasterAlpha := Opacity;
end;

procedure TShadowEffect.PaintTo(aSrc, aDst: TBitmap32; aR: TRect; aDstX, aDstY:
  integer);
var
  aShadow: TBitmap32;
begin
  if not Enabled then Exit;
  aShadow := TBitmap32.Create;
  try
    GenerateShadow(aSrc, aShadow, aR);
    aShadow.DrawTo(aDst, aDstX+OffsetX, aDstY+OffsetY);
    //aSrc.DrawTo(aDst, aDstX, aDstY);
  finally
    aShadow.Free;
  end;
end;

procedure TShadowEffect.SetBlur(const Value: Byte);
begin
  if FBlur <> Value then
  begin
    FBlur := Value;
    Update;
  end;
end;

procedure TShadowEffect.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Update;
  end;
end;

procedure TShadowEffect.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Update;
  end;
end;

procedure TShadowEffect.SetOffset(const X, Y: Integer);
begin
  FOffsetX := X;
  FOffsetY := Y;
  Update;
end;

procedure TShadowEffect.SetOffsetX(const Value: Integer);
begin
  if FOffsetX <> Value then
    SetOffset(Value, FOffsetY);
end;

procedure TShadowEffect.SetOffsetY(const Value: Integer);
begin
  if FOffsetY <> Value then
    SetOffset(FOffsetX, Value);
end;

procedure TShadowEffect.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    Update;
  end;
end;


end.
