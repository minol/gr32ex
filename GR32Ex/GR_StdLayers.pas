
{ Summary the standard GRLayers classes }

{
It contains a reimplementation of the layers used currently in Graphics32. The implementation starts from
TCustomLayer and defines the following new:

- TGRCustomTextLayer (derived from TGRCustomPropertyLayer)
- TGRCustomBitmapLayer (derived from TGRCustomPropertyLayer)

3D programmer here!

All the above may work, but they're too complicated and not clear . So, here is what i use when doing software 3d->2d projections (usually for software 3d rendering :

Code:

x_2d = x_3d * 256 / z_3d + screen_horizontal_center
y_2d = y_3d * 256 / z_3d + screen_vertical_center


(note: that 256 can be replaced with a shiftleft operation of 8 bits - but i think that the compiler does that automatically where possible .

In the above formulas, x_2d and y_2d are screen coordinates (that is, Canvas pixels) and x_3d, y_3d and z_3d are 3D world coordinates. screen_horizonal_center and screen_vertical_center are what the name specifices, but instead of screen think Canvas . Just use Canvas.Width div 2 and Canvas.Height div 2.

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
 * The Original Code is GR_Layers
 *
 * The Initial Developer of the Original Code is Riceball LEE
 * Portions created by Riceball LEE are Copyright (C) 2008
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit GR_StdLayers;

{$I Setting.inc}

interface

uses
  {$IFDEF Debug}
  CnDebug,
  {$ENDIF}
  Windows, Messages,
  SysUtils, Classes, Types, Graphics
  , GR32
  , GR32_Resamplers
  , GR32_Containers
  , GR32_Layers
  , GR32_RepaintOpt
  , GR32_Image
  , GR32_Types
  , GR32_Transforms
  , GR_Animation
  , GR_BitmapEx
  , GR_Layers
  , GR_Graphics
  ;


type
  TTextLayout = (tlTop, tlCenter, tlBottom);

  {
TGRCustomBitmapLayer
This is the last layer in the bundle and provides means to paint a
TBitmap32 with all the transformations applied. Additionally, it has a
PaintTo method, which allows to draw the content to other locations than the
ImgView32 container.
  }
  TGRCustomBitmapLayer = class(TGRLayer)
  protected
    FBitmap: TBitmap32Ex;
    FCropped: Boolean;
    procedure SetCropped(Value: Boolean);
    procedure SetBitmap(Value: TBitmap32Ex);
  protected
    function DoHitTest(X, Y: Integer): Boolean; override;
    function GetNativeSize: TSize; override;
    procedure Paint(Buffer: TBitmap32); override;
    procedure DoAlphaBlendChanged; override;

    procedure BitmapChanged(Sender: TObject);virtual;

    property Bitmap: TBitmap32Ex read FBitmap write SetBitmap;
    property Cropped: Boolean read FCropped write SetCropped;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;

    procedure PaintTo(Buffer: TBitmap32; const R: TRect);

  published
    property AlphaBlend default true;
    property AlphaBlendValue;
  end;

  TGRBitmapLayer = class(TGRCustomBitmapLayer)
  public
    procedure Assign(Source: TPersistent);override;

  published
    property Bitmap;
    property Cropped;
  end;

  { 
TGRCustomTextLayer
This layer is not really implemented, but used as a placeholder. Once
somebody decides to write a real text layer, this one can serve as the
starting point.
  }
  TGRCustomTextLayer = class(TGRCustomBitmapLayer)
  protected
    FText: string;
    FFont: TFont32;
    FAlignment: TAlignment;
    FLayout: TTextLayout;
    FWordWrap: Boolean;

    procedure SetAlignment(const Value: TAlignment);
    procedure SetLayout(const Value: TTextLayout);
    procedure SetText(const Value: string);
    procedure SetWordWrap(const Value: Boolean);
    procedure DoTextChanged(Sender: TObject);

    function GetNativeSize: TSize; override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;

    property Text: string read FText write SetText;
    property Font: TFont32 read FFont;
    property Alignment: TAlignment read FAlignment write SetAlignment default
      taCenter;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
  end;
  
  TGRTextLayer = class(TGRCustomTextLayer)
  published
    property Text;
    property Font;
    property Alignment;
    property Layout;
    property WordWrap;
  end;

implementation

uses
  Math, StrUtils, TypInfo, uMeTypInfo, GR32_Polygons, GR32_MicroTiles;

type
  TAffineTransformationAccess = class(TAffineTransformation);

{ TGRCustomTextLayer }
constructor TGRCustomTextLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  FAlignment := taCenter;
  FFont := TFont32.Create;
  FFont.OnChange := DoTextChanged;
end;

destructor TGRCustomTextLayer.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

procedure TGRCustomTextLayer.Assign(Source: TPersistent);
begin
  if Source is TGRCustomTextLayer then
    with Source as TGRCustomTextLayer do
    begin
      Changing;
      Self.FText := FText;
      Self.FFont.Assign(FFont);
      Self.FAlignment := FAlignment;
      Self.FLayout := FLayout;
      DoTextChanged(nil);
    end;
  inherited Assign(Source);
end;

procedure TGRCustomTextLayer.DoTextChanged(Sender: TObject);
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  vRect: TRect;
  vDrawStyle: Longint;
begin
	FBitmap.BeginUpdate;
	try
	  with FSize do FBitmap.SetSize(cx, cy);
	  FBitmap.Clear(0);
    vDrawStyle := DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];
    vRect := FBitmap.ClipRect;
    FFont.DrawText(FBitmap, FText, vRect, vDrawStyle);
	finally
	  FBitmap.EndUpdate;
	  FBitmap.Changed; //this will notify the bitmap onChange event.
	end;
  //Changed;
end;

function TGRCustomTextLayer.GetNativeSize: TSize;
begin
	Result := FSize;
	{
	if (Result.cx <> FBitmap.Width) or (Result.cy <> FBitmap.Height) then
	with Result do
	  FBitmap.SetSize(cx, cy);
	}
end;

procedure TGRCustomTextLayer.SetAlignment(const Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    DoTextChanged(nil);
  end;
end;

procedure TGRCustomTextLayer.SetLayout(const Value: TTextLayout);
begin
  if Value <> FLayout then
  begin
    FLayout := Value;
    DoTextChanged(nil);
  end;
end;

procedure TGRCustomTextLayer.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    DoTextChanged(nil);
  end;
end;

procedure TGRCustomTextLayer.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    DoTextChanged(nil);
  end;
end;

{ TGRCustomBitmapLayer }
constructor TGRCustomBitmapLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FAlphaBlend := True;
  FBitmap := TBitmap32Ex.Create;
  FBitmap.DrawMode := dmBlend;
  FBitmap.OnChange := BitmapChanged;
end;

destructor TGRCustomBitmapLayer.Destroy;
begin
  FBitmap.Free;

  inherited;
end;

procedure TGRCustomBitmapLayer.Assign(Source: TPersistent);
begin
  if Source is TGRCustomBitmapLayer then
    with Source as TGRCustomBitmapLayer do
    begin
      Changing;
      Self.FCropped := FCropped;
      Changed;
    end;
  inherited Assign(Source);
end;

procedure TGRCustomBitmapLayer.BitmapChanged(Sender: TObject);
begin
  Changing;
  with GetNativeSize do
    FTransformation.SrcRect := FloatRect(0, 0, cx - 1, cy - 1);
  Changed;
  //DoChange;
end;

procedure TGRCustomBitmapLayer.SetBitmap(Value: TBitmap32Ex);
begin
  Changing;
  FBitmap.Assign(Value);
  Changed;
end;

procedure TGRCustomBitmapLayer.SetCropped(Value: Boolean);
begin
  if Value <> FCropped then
  begin
    Changing;
    FCropped := Value;
    Changed;
  end;
end;

procedure TGRCustomBitmapLayer.DoAlphaBlendChanged;
begin
  Changing;
    if FAlphaBlend then 
      FBitmap.DrawMode := dmBlend
    else
      FBitmap.DrawMode := dmOpaque;
    FBitmap.MasterAlpha := FAlphaBlendValue;
  Changed;
end;

function TGRCustomBitmapLayer.DoHitTest(X, Y: Integer): Boolean;

var
  B: TPoint;
begin
  {$IFDEF Designtime_Supports}
  if FBitmap.Empty then
  begin
    Result := inherited DoHitTest(X, Y);
    exit;
  end;
  {$ENDIF}
  B := Point(X, Y);
  if TAffineTransformationAccess(FTransformation).TransformValid then
  begin
    B := FTransformation.ReverseTransform(B);
  end
  else begin
  	B := Point(GetLayerPosition(FloatPoint(X, Y)));
  end;
  with GetNativeSize do Result := PtInRect(Rect(0, 0, cx, cy), B);
  if Result and AlphaHit and (Bitmap.PixelS[B.X, B.Y] and $FF000000 = 0) then
    Result := False;
end;

function TGRCustomBitmapLayer.GetNativeSize: TSize;
begin
  {$IFDEF Designtime_Supports}
  if FBitmap.Empty then
  begin
    Result := inherited GetNativeSize;
    exit;
  end;
  {$ENDIF}
  Result.cx := FBitmap.Width;
  Result.cy := FBitmap.Height;
  //FSize := Result;
end;

procedure TGRCustomBitmapLayer.Paint(Buffer: TBitmap32);
var
  vRect: TRect;
begin 
  {$IFDEF Designtime_Supports}
  if FBitmap.Empty then
  begin
    inherited Paint(Buffer);
    exit;
  end;
  {$ENDIF}
  //UpdateTransformation;
  //if not TAffineTransformationAccess(FTransformation).TransformValid then
    //TAffineTransformationAccess(FTransformation).PrepareTransform;

  if TAffineTransformationAccess(FTransformation).TransformValid then
  begin
    if FCropped then
    begin
      //vRect := MakeRect(GetTransformedTargetRect);
      with GetAdjustedPosition(FPosition), GetNativeSize do
      begin
        vRect.Left := Round(x);
        vRect.Top  := Round(y);
        vRect.Right := Round(x + cx);
        vRect.Bottom := Round(y + cy);
      end;
      Transform(Buffer, FBitmap, FTransformation, vRect);
    end
    else
      Transform(Buffer, FBitmap, FTransformation);
  end
  else
  begin
    with GetAdjustedPosition(FPosition), GetNativeSize do
    begin
        vRect.Left := Round(x);
        vRect.Top  := Round(y);
        vRect.Right := Round(x + cx);
        vRect.Bottom := Round(y + cy);
    end;
    //with vRect do SendDebug(ClassName + ' vRect(%d,%d,%d,%d)', [Left, Top, Right, Bottom]);

    if FCropped then
    begin
      BlockTransfer(Buffer, vRect.Left, vRect.Top, Buffer.ClipRect, FBitmap, vRect, FBitmap.DrawMode, FBitmap.OnPixelCombine);
    end
    else
      BlockTransfer(Buffer, vRect.Left, vRect.Top, Buffer.ClipRect, FBitmap, FBitmap.ClipRect, FBitmap.DrawMode, FBitmap.OnPixelCombine);
  end;
  //OutputDebugString(PChar('paint bitmap:'+ IntToStr(Integer(FBitmap))));
end;

procedure TGRCustomBitmapLayer.PaintTo(Buffer: TBitmap32; const R: TRect);
// Paints the bitmap to the given buffer using the position and size/location given in R.
var
  Transformation: TAffineTransformation;
begin
  Transformation := nil;
  try
    GetLayerTransformation(Transformation);
    Transformation.SrcRect := FloatRect(0, 0, Bitmap.Width - 1, Bitmap.Height - 1);
    TAffineTransformationAccess(FTransformation).PrepareTransform;
    Transformation.Scale((R.Right - R.Left) / Bitmap.Width, (R.Bottom - R.Top) / Bitmap.Height);
    Transformation.Translate(R.Left, R.Top);
    Transform(Buffer, FBitmap, Transformation);
  finally
    Transformation.Free;
  end;
end;

{ TGRBitmapLayer }
procedure TGRBitmapLayer.Assign(Source: TPersistent);
begin
  if Source is TGRCustomBitmapLayer then
    with Source as TGRCustomBitmapLayer do
    begin
      Changing;
      Self.FBitmap.Assign(FBitmap);
      Changed;
    end;
  inherited Assign(Source);
end;

initialization
finalization
end.