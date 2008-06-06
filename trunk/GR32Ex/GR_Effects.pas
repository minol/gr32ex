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
  DefaultShadowColor = clNone;
  DefaultShadowBlur = 2;
  DefaultShadowOpacity = 256 div 2;
  DefaultReflectionValue = 160;

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

  TReflectionEffect = class(TCustomEffectProperty)
  protected
    FReflectionImg: TBitmap32;
    FReflection: byte;
    FReflectionHeight: Integer;
    FRealHeight: Integer;

    function GetReflectionImg: TBitmap32;
    procedure SetReflection(const Value: byte);
    //procedure SetReflectionAxis(const Value: Integer);

    property ReflectionImg: TBitmap32 read GetReflectionImg;
  published
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    { Summary Generate Reflection image from Src }
    procedure Generate(aSrc, aDst: TBitmap32; R:TRect);
    procedure PaintTo(aSrc, aDst: TBitmap32; aR: TRect; aDstX, aDstY: integer);
      overload; override;

    //the source real height from bottom(no transparent line.).
    property RealHeight: Integer read FRealHeight;
    property ReflectionHeight: Integer read FReflectionHeight;
  published
    property Reflection: byte read FReflection write SetReflection default DefaultReflectionValue;
  end;

implementation

uses
  Math;

{ TShadowEffect }
constructor TShadowEffect.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FOffsetX := DefaultShadowOffsetX;
  FOffsetY := DefaultShadowOffsetY;
  FBlur := DefaultShadowBlur;
  FColor := DefaultShadowColor;
  FOpacity := DefaultShadowOpacity;
  FEnabled := True;
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

{ TReflectionEffect }
constructor TReflectionEffect.Create(AOwner: TPersistent);
begin
	inherited;
	FReflection := DefaultReflectionValue;
end;
	
destructor TReflectionEffect.Destroy;
begin
  FreeAndNil(FReflectionImg);
  inherited;
end;

procedure TReflectionEffect.Generate(aSrc, aDst: TBitmap32; R:TRect);
var
  x, y: Integer;
  w, h: Integer;
  vH: Integer;
  vAlpha: Integer;
  vFirstNoneEmptyLine: Integer;
  vDstX, vDstY: Integer;
  //vSrcBits, vDstBits: PColor32Array;
  vLine, vDstLine: PColor32Array;
begin
  w := R.Right - R.Left;
  h := R.Bottom - R.Top;
  aDst.SetSize(w, h);
  w := Min(aSrc.Width-1, w);
  vH := aSrc.Height - Round(FReflection / (255.0 / aSrc.Height));
  h := Min(vH, h);
  vH := aSrc.Height - h;

  //Check lasy empty line:
  vFirstNoneEmptyLine := aSrc.Height;
  for y := aSrc.Height - 1 downto vH do
  begin
    vFirstNoneEmptyLine := y;
    vLine := aSrc.ScanLine[y];
    x := 0;
    while x < aSrc.Width - 1 do
    begin
      if TColor32Entry(vLine[x]).A <> 0 then
      begin
        break;
      end;
      Inc(x);
    end;
    if x < aSrc.Width - 1 then
      break;
  end; //}

  vDstY := 0;
  FRealHeight := vFirstNoneEmptyLine;
  for y := vFirstNoneEmptyLine downto vH do
  begin
    vAlpha := ((255 * (aSrc.Height - vDstY) div aSrc.Height) ) - FReflection;
    if vAlpha <= 0 then
    begin
     break;
    end;
    if vAlpha > 255 then vAlpha := 255;

    vLine := aSrc.ScanLine[y];
    vDstLine := aDst.ScanLine[vDstY];
    vDstX := 0;
    for x := R.Left to w do
    begin
//      vC.ARGB := vLine[x];
    	vDstLine[vDstX] := vLine[x];
      with TColor32Entry(vDstLine[vDstX]) do
        if A <> 0 then
        begin
      	  A := (vAlpha * A div 255 );
        end;
      Inc(vDstX);
    end;
    Inc(vDstY);
  end;
  FReflectionHeight := vDstY;
  //aDst.Draw(0,0,R,aSrc);
  //aDst.DrawMode := dmBlend;
end;

function TReflectionEffect.GetReflectionImg: TBitmap32;
begin
  if not Assigned(FReflectionImg) then
    FReflectionImg := TBitmap32.Create;
  Result := FReflectionImg;
end;

procedure TReflectionEffect.PaintTo(aSrc, aDst: TBitmap32; aR: TRect; aDstX, aDstY: integer);
var
  vBmp: TBitmap32;
begin
  if not Enabled then Exit;
  vBmp := TBitmap32.Create;
  try
    Generate(aSrc, vBmp, aR);
    vBmp.DrawMode := dmBlend;
    vBmp.DrawTo(aDst, aDstX, aDstY);
    //aSrc.DrawTo(aDst, aDstX, aDstY);
  finally
    vBmp.Free;
  end;
end;

procedure TReflectionEffect.SetReflection(const Value: byte);
begin
  if FReflection <> Value then
  begin
    FReflection := Value;
    Update;
  end;
end;

end.
