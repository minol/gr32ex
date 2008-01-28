
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
unit GR_Layers;

{$I GR32.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes
  , GR32_Layers
  , GR32_ExtLayers
  ;


type
  TGRLayerCollection = class(TLayerCollection)
  end;

  TGRLayerContainer = class(TCustomLayerEx)
  protected
    FLayers: TGRLayerCollection;
    FBitmap : TBitmap32;
    CachedXForm: TCoordXForm;
    FOnResize: TNotifyEvent;

    procedure BitmapChanged(const Area: TRect); virtual;
    procedure BitmapResizeHandler(Sender: TObject);
    procedure BitmapChangeHandler(Sender: TObject);
    procedure BitmapAreaChangeHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);
    procedure BitmapDirectAreaChangeHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);
    procedure LayerCollectionChangeHandler(Sender: TObject);
    procedure LayerCollectionGDIUpdateHandler(Sender: TObject);
    procedure LayerCollectionGetViewportScaleHandler(Sender: TObject; var ScaleX, ScaleY: Single);
    procedure LayerCollectionGetViewportShiftHandler(Sender: TObject; var ShiftX, ShiftY: Single);
  public
    constructor Create(aLayerCollection: TLayerCollection);
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
  end;


implementation

{ TGRLayerContainer }

constructor TGRLayerContainer.Create(aLayerCollection: TLayerCollection);
begin
  FBitmap := TBitmap32.Create;
  FBitmap.OnResize := BitmapResizeHandler;
  
  FLayers := TLayerCollection.Create(Self);
  with TLayerCollectionAccess(FLayers) do
  begin
{$IFDEF DEPRECATEDMODE}
    CoordXForm := @CachedXForm;
{$ENDIF}
    OnChange := LayerCollectionChangeHandler;
    OnGDIUpdate := LayerCollectionGDIUpdateHandler;
    OnGetViewportScale := LayerCollectionGetViewportScaleHandler;
    OnGetViewportShift := LayerCollectionGetViewportShiftHandler;
  end;

  FRepaintOptimizer.RegisterLayerCollection(FLayers);
  RepaintMode := rmFull;
end;

procedure TGRLayerContainer.BitmapResizeHandler(Sender: TObject);
begin
  if Assigned(FOnResize) then FOnResize(Sender);;
end;

procedure TGRLayerContainer.BitmapChangeHandler(Sender: TObject);
begin
  FRepaintOptimizer.Reset;
  BitmapChanged(Bitmap.Boundsrect);
end;

procedure TGRLayerContainer.BitmapAreaChangeHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);
var
  T, R: TRect;
  Width, Tx, Ty, I, J: Integer;
begin
  if Sender = FBitmap then
  begin
    T := Area;
    Width := Trunc(FBitmap.Resampler.Width) + 1;
    InflateArea(T, Width, Width);
    T.TopLeft := BitmapToControl(T.TopLeft);
    T.BottomRight := BitmapToControl(T.BottomRight);

    if FBitmapAlign <> baTile then
      FRepaintOptimizer.AreaUpdateHandler(Self, T, AREAINFO_RECT)
    else
    begin
      with CachedBitmapRect do
      begin
        Tx := Buffer.Width div Right;
        Ty := Buffer.Height div Bottom;
        for J := 0 to Ty do
          for I := 0 to Tx do
          begin
            R := T;
            OffsetRect(R, Right * I, Bottom * J);
            FRepaintOptimizer.AreaUpdateHandler(Self, R, AREAINFO_RECT);
          end;
      end;
    end;
  end;

  BitmapChanged(Area);
end;

procedure TGRLayerContainer.BitmapDirectAreaChangeHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);
var
  T, R: TRect;
  Width, Tx, Ty, I, J: Integer;
begin
  if Sender = FBitmap then
  begin
    T := Area;
    Width := Trunc(FBitmap.Resampler.Width) + 1;
    InflateArea(T, Width, Width);
    T.TopLeft := BitmapToControl(T.TopLeft);
    T.BottomRight := BitmapToControl(T.BottomRight);

    if FBitmapAlign <> baTile then
      InvalidRects.Add(T)
    else
    begin
      with CachedBitmapRect do
      begin
        Tx := Buffer.Width div Right;
        Ty := Buffer.Height div Bottom;
        for J := 0 to Ty do
          for I := 0 to Tx do
          begin
            R := T;
            OffsetRect(R, Right * I, Bottom * J);
            InvalidRects.Add(R);
          end;
      end;
    end;
  end;

  if FUpdateCount = 0 then
  begin
    if not(csCustomPaint in ControlState) then Repaint;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TGRLayerContainer.LayerCollectionChangeHandler(Sender: TObject);
begin
  Changed;
end;

procedure TGRLayerContainer.LayerCollectionGDIUpdateHandler(Sender: TObject);
begin
  Paint;
end;

procedure TGRLayerContainer.LayerCollectionGetViewportScaleHandler(Sender: TObject; var ScaleX, ScaleY: Single);
begin
  UpdateCache;
  ScaleX := CachedXForm.ScaleX / FixedOne;
  ScaleY := CachedXForm.ScaleY / FixedOne;
end;

procedure TGRLayerContainer.LayerCollectionGetViewportShiftHandler(Sender: TObject; var ShiftX, ShiftY: Single);
begin
  UpdateCache;
  ShiftX := CachedXForm.ShiftX;
  ShiftY := CachedXForm.ShiftY;
end;

procedure TGRLayerContainer.BitmapChanged(const Area: TRect);
begin
  Changed;
end;

initialization
end.
