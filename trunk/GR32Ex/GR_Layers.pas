
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

{$I Setting.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes
  , GR32
  , GR32_Resamplers
  , GR32_Containers
  , GR32_Layers
  , GR32_RepaintOpt
  , GR32_Image
  , GR32_ExtLayers
  , GR_Animation
  ;

type
  TGRLayerClass = class of TGRLayer;
  TGRLayer = class(TExtBitmapLayer)
  protected
    FWidth: Integer;
    FHeight: Integer;

    function GetLeft: Integer;
    function GetTop: Integer;
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);

    function GetNativeSize: TSize; override;
    procedure Paint(Buffer: TBitmap32); override;
    procedure BitmapChanged(Sender: TObject); override;
    function DoHitTest(X, Y: Integer): Boolean;override;

  public
    constructor Create(ALayerCollection: TLayerCollection);override;
  published
    property Left:Integer read GetLeft write SetLeft;
    property Top:Integer read GetTop write SetTop;
    property Width:Integer read FWidth write SetWidth;
    property Height:Integer read FHeight write SetHeight;

    property Name;
    property Bitmap;
    property Cursor;
    property Cropped;
    property DrawMode;
    property Angle;
    property Skew;
    property PivotPoint;
    property ScaledViewport;
    property Scaling;
    property Visible;
    property MouseEvents;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TGRAnimationLayer = class(TGRLayer)
  protected
    FAnimation: TGRAnimation;
    procedure SetAnimation(const Value: TGRAnimation);
  public
    constructor Create(LayerCollection: TLayerCollection);override;
    destructor Destroy; override;
    property Animation: TGRAnimation read FAnimation write SetAnimation;
  end;

  TGRAniFrameEvent = procedure(const Sender: TGRLayer; const MoveCount: Longword; var Done: Boolean) of object;
  TGRLayerAnimator = class(TThread)
  protected
    FIsPaused: Boolean;
    FLastTick: Longword;
    FLayer: TGRLayer;
    FOnFrame: TGRAniFrameEvent;
    function DrawFrame(const MoveCount: Longword): Boolean; virtual;
    procedure Execute; override;
  public
    constructor Create(const aLayer: TGRLayer);
    procedure Start;
    property Paused: Boolean read FIsPaused write FIsPaused;
    property OnFrame: TGRAniFrameEvent read FOnFrame write FOnFrame;
  end;

  TGRLayerAnimator_Sample = class(TGRLayerAnimator)
  protected
    FCurStep: Integer;
    FOldTop: Integer;
    FMaxStep: Integer;
    function DrawFrame(const MoveCount: Longword): Boolean; override;
  public
    constructor Create(const aLayer: TGRLayer; const aMaxStep: Integer);
  end;

  TGRLayerCollection = class(TLayerCollection)
  end;

  TGRLayerContainer = class(TCustomLayerEx)
  protected
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FBuffer: TBitmap32;
    FBufferOversize: Integer;
    FBufferValid: Boolean;
    FForceFullRepaint: Boolean;
    FRepaintOptimizer: TCustomRepaintOptimizer;
    FRepaintMode: TRepaintMode;

    FLayers: TGRLayerCollection;


    FInvalidRects: TRectList;
    FScaleX: Single;
    FScaleY: Single;
    FScaleMode: TScaleMode;
    FUpdateCount: Integer;

    CachedBitmapRect: TRect;
    CachedXForm: TCoordXForm;
    CacheValid: Boolean;
    OldSzX, OldSzY: Integer;

    procedure SetRepaintMode(const Value: TRepaintMode); virtual;
    function GetBitmapRect: TRect;

    function  CustomRepaint: Boolean; virtual;
    procedure DoPaintBuffer; virtual;
    procedure UpdateCache; virtual;
    property  UpdateCount: Integer read FUpdateCount;
    procedure InvalidateCache;
    procedure Invalidate;
    function  InvalidRectsAvailable: Boolean; virtual;
    procedure DoPrepareInvalidRects; virtual;
    procedure ResetInvalidRects;
    procedure Paint(aBuffer: TBitmap32); override;

    procedure LayerCollectionChangeHandler(Sender: TObject);
    procedure LayerCollectionGDIUpdateHandler(Sender: TObject);
    procedure LayerCollectionGetViewportScaleHandler(Sender: TObject; var ScaleX, ScaleY: Single);
    procedure LayerCollectionGetViewportShiftHandler(Sender: TObject; var ShiftX, ShiftY: Single);

    property  BufferValid: Boolean read FBufferValid write FBufferValid;
    property  InvalidRects: TRectList read FInvalidRects;
  public
    constructor Create(aLayerCollection: TLayerCollection);override;
    destructor Destroy;override;
    function  GetViewportRect: TRect; virtual;

    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Buffer: TBitmap32 read FBuffer;
    property RepaintMode: TRepaintMode read FRepaintMode write SetRepaintMode default rmFull;
  end;


procedure RegisterLayer(const aLayerControlClass: TGRLayerClass);
function GetLayerClass(const aClassName: string): TGRLayerClass;
function GLayerClasses: TThreadList;

implementation

uses
  Math, TypInfo, GR32_MicroTiles, GR_ImageEx;

const
  cAniIntervalCount = 33; //ms
  DefaultRepaintOptimizerClass: TCustomRepaintOptimizerClass = TMicroTilesRepaintOptimizer;
  UnitXForm: TCoordXForm = (
    ScaleX: $10000;
    ScaleY: $10000;
    ShiftX: 0;
    ShiftY: 0;
    RevScaleX: 65536;
    RevScaleY: 65536);

type
  TBitmap32Access = class(TBitmap32);
  TLayerAccess = class(TCustomLayer);

var
  FLayerClasses: TThreadList;

function GetLayerClass(const aClassName: string): TGRLayerClass;
var
  I: integer;
begin
  with GLayerClasses.LockList do
  try
    for I := 0 to Count - 1 do
    begin
      Result := TGRLayerClass(Items[I]);
      if Result.ClassName = aClassName then exit;
    end;
    Result := nil;
  finally
    FLayerClasses.UnlockList;
  end;
end;

procedure RegisterLayer(const aLayerControlClass: TGRLayerClass);
begin
  with GLayerClasses.LockList do
  try
    if IndexOf(aLayerControlClass) < 0 then
      Add(aLayerControlClass);
  finally
    FLayerClasses.UnlockList;
  end;
end;

function GLayerClasses: TThreadList;
begin
  if not Assigned(FLayerClasses) then
  begin
    FLayerClasses := TThreadList.Create;
  end;
  Result := FLayerClasses;
end;

{ TGRLayerContainer }

constructor TGRLayerContainer.Create(aLayerCollection: TLayerCollection);
begin
  inherited;
  FBuffer := TBitmap32.Create;
  FBufferOversize := 40;
  FForceFullRepaint := True;
  FInvalidRects := TRectList.Create;
  FRepaintOptimizer := DefaultRepaintOptimizerClass.Create(Buffer, InvalidRects);
  Height := 192;
  Width := 192;


  FLayers := TGRLayerCollection.Create(Self);
  with FLayers do
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

destructor TGRLayerContainer.Destroy;
begin
  FRepaintOptimizer.Free;
  FInvalidRects.Free;
  FBuffer.Free;
  inherited;
end;

procedure TGRLayerContainer.LayerCollectionChangeHandler(Sender: TObject);
begin
  Changed;
end;

procedure TGRLayerContainer.LayerCollectionGDIUpdateHandler(Sender: TObject);
begin
  Changed;
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

procedure TGRLayerContainer.UpdateCache;
begin
  if CacheValid then Exit;
  CachedBitmapRect := GetBitmapRect;
  CachedXForm := UnitXForm;
  CacheValid := True;
end;

function TGRLayerContainer.InvalidRectsAvailable: Boolean;
begin
  // avoid calling inherited, we have a totally different behaviour here...
  DoPrepareInvalidRects;
  Result := FInvalidRects.Count > 0;
end;

procedure TGRLayerContainer.InvalidateCache;
begin
  if FRepaintOptimizer.Enabled then FRepaintOptimizer.Reset;
  CacheValid := False;
end;

procedure TGRLayerContainer.Invalidate;
begin
  BufferValid := False;
  CacheValid := False;
end;

procedure TGRLayerContainer.DoPrepareInvalidRects;
begin
  if FRepaintOptimizer.Enabled and not FForceFullRepaint then
    FRepaintOptimizer.PerformOptimization;
end;

function TGRLayerContainer.GetBitmapRect: TRect;
begin
    with Result do
    begin
      Left := 0;
      Right := 0;
      Top := 0;
      Bottom := 0;
    end
end;

procedure TGRLayerContainer.Paint(aBuffer: TBitmap32);
var
  I: Integer;
  vRect: TRect;
begin
  if FRepaintOptimizer.Enabled then
  begin
{$IFDEF CLX}
    if CustomRepaint then DoPrepareInvalidRects;
{$ENDIF}
    FRepaintOptimizer.BeginPaint;
  end;

  if not FBufferValid then
  begin
{$IFDEF CLX}
    TBitmap32Access(FBuffer).ImageNeeded;
{$ENDIF}
    DoPaintBuffer;
{$IFDEF CLX}
    TBitmap32Access(FBuffer).CheckPixmap;
{$ENDIF}
  end;

  FBuffer.Lock;
  try
    if FInvalidRects.Count > 0 then
      for i := 0 to FInvalidRects.Count - 1 do
      begin
        vRect := FInvalidRects[i]^;
        with vRect do
          BlockTransfer(aBuffer, Left, Top, aBuffer.ClipRect, FBuffer, vRect, FBuffer.DrawMode, FBuffer.OnPixelCombine);
      end
    else begin
      vRect := GetViewportRect;
      with vRect do
        BlockTransfer(aBuffer, Left, Top, aBuffer.ClipRect, FBuffer, vRect, FBuffer.DrawMode, FBuffer.OnPixelCombine);
    end;
  finally
    FBuffer.Unlock;
  end;

  
  if FRepaintOptimizer.Enabled then
    FRepaintOptimizer.EndPaint;
  ResetInvalidRects;
  FForceFullRepaint := False;
end;

function TGRLayerContainer.CustomRepaint: Boolean;
begin
  Result := FRepaintOptimizer.Enabled and not FForceFullRepaint and
    FRepaintOptimizer.UpdatesAvailable;
end;

procedure TGRLayerContainer.DoPaintBuffer;
var
  I, J: Integer;
begin
  if FRepaintOptimizer.Enabled then
    FRepaintOptimizer.BeginPaintBuffer;

  UpdateCache;


  Buffer.BeginUpdate;
  if FInvalidRects.Count = 0 then
  begin
    Buffer.ClipRect := GetViewportRect;

    for I := 0 to FLayers.Count - 1 do
      if (FLayers.Items[I].LayerOptions and LOB_VISIBLE) <> 0 then
        TLayerAccess(FLayers.Items[I]).DoPaint(Buffer);
  end
  else
  begin
    for J := 0 to FInvalidRects.Count - 1 do
    begin
      Buffer.ClipRect := FInvalidRects[J]^;
      for I := 0 to FLayers.Count - 1 do
        if (FLayers.Items[I].LayerOptions and LOB_VISIBLE) <> 0 then
          TLayerAccess(FLayers.Items[I]).DoPaint(Buffer);
    end;

    Buffer.ClipRect := GetViewportRect;
  end;
  Buffer.EndUpdate;

  if FRepaintOptimizer.Enabled then
    FRepaintOptimizer.EndPaintBuffer;

  // avoid calling inherited, we have a totally different behaviour here...
  FBufferValid := True;
end;

function TGRLayerContainer.GetViewportRect: TRect;
begin
  // returns position of the buffered area within the control bounds
  with Result do
  begin
    // by default, the whole control is buffered
    Left := 0;
    Top := 0;
    Right := Width;
    Bottom := Height;
  end;
end;

procedure TGRLayerContainer.SetRepaintMode(const Value: TRepaintMode);
begin
  if Assigned(FRepaintOptimizer) then
  begin
    FRepaintOptimizer.Enabled := Value = rmOptimizer;

    FRepaintMode := Value;
    Invalidate;
  end;
end;

procedure TGRLayerContainer.ResetInvalidRects;
begin
  FInvalidRects.Clear;
end;

{ TGRLayer }

constructor TGRLayer.Create(
  ALayerCollection: TLayerCollection);
begin
  inherited;
  LayerOptions := LOB_MOUSE_EVENTS or LOB_VISIBLE; 
end;

procedure TGRLayer.BitmapChanged(Sender: TObject);
begin
  inherited;
  if not Bitmap.Empty then
    Changed;
end;

function TGRLayer.DoHitTest(X, Y: Integer): Boolean;
var
  B: TPoint;
begin
  B := FTransformation.ReverseTransform(Point(X, Y));

  if Bitmap.Empty then
    Result := PtInRect(Rect(0, 0, FWidth, FHeight), B)
  else
  begin
    Result := PtInRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), B);
    if Result and AlphaHit and (Bitmap.PixelS[B.X, B.Y] and $FF000000 = 0) then
      Result := False;
  end;
end;

function TGRLayer.GetLeft: Integer;
begin
  Result := Trunc(FPosition.X);
end;

function TGRLayer.GetNativeSize: TSize;
begin
  if (FWidth <> 0) and (FHeight <> 0) then
  begin
    Result.cx := FWidth;
    Result.cy := FHeight;
  end
  else
    Result := Inherited GetNativeSize();
end;

function TGRLayer.GetTop: Integer;
begin
  Result := Trunc(FPosition.Y);
end;

procedure TGRLayer.Paint(Buffer: TBitmap32);
var
  vIsEmpty: Boolean;
begin
  vIsEmpty := Bitmap.Empty;
  if vIsEmpty then
  begin
    Bitmap.SetSize(Width, Height);
    if (LayerCollection.Owner is TImage32Editor) then
      Bitmap.Clear(SetAlpha(clWhite32, $3F))
    else
      Bitmap.Clear(0);
  end;
  inherited;
  if vIsEmpty and not Bitmap.Empty then Bitmap.Delete;
end;

procedure TGRLayer.SetHeight(const Value: Integer);
begin
  if FHeight <> value then
  begin
    Changing;
    FHeight := Value;
    Changed;

    DoChange;
  end;
end;

procedure TGRLayer.SetLeft(const Value: Integer);
begin
  if Trunc(FPosition.X) <> value then
  begin
    Changing;
    FPosition.X := Value;
    Changed;

    DoChange;
  end;
end;

procedure TGRLayer.SetTop(const Value: Integer);
begin
  if Trunc(FPosition.Y) <> value then
  begin
    Changing;
    FPosition.Y := Value;
    Changed;

    DoChange;
  end;
end;

procedure TGRLayer.SetWidth(const Value: Integer);
begin
  if FWidth <> value then
  begin
    Changing;
    FWidth := Value;
    Changed;

    DoChange;
  end;
end;

{ TGRAnimationLayer }
constructor TGRAnimationLayer.Create(LayerCollection: TLayerCollection);
begin
  inherited Create(LayerCollection);
  FAnimation := nil;//TGRAnimation.Create(Self);
end;

destructor TGRAnimationLayer.Destroy;
begin
  FreeAndNil(FAnimation);
  inherited;
end;

procedure TGRAnimationLayer.SetAnimation(const Value: TGRAnimation);
var
  vNewAni: TGRAnimation;
begin
  vNewAni := nil;
  if Value <> nil then
  begin
    vNewAni := TGRAnimation(Value.ClassType).Create(Self);
    vNewAni.Assign(Value);
  end;
  FreeAndNil(FAnimation);
  FAnimation := vNewAni;
  Changed;
end;

{ TGRLayerAnimator }
function TGRLayerAnimator.DrawFrame(const MoveCount: Longword): Boolean; 
begin
  Result := Assigned(FOnFrame);
  if Result then FOnFrame(FLayer, MoveCount, Result);
end;

procedure TGRLayerAnimator.Execute; 
var
  vDone: Boolean;
begin
  
  while not Terminated do
  begin
    vDone := DrawFrame(GetTickCount - FLastTick);
    FLastTick := GetTickCount;
    if vDone then break;
    Sleep(cAniIntervalCount);
    if FIsPaused then Suspend;
  end;
end;

constructor TGRLayerAnimator.Create(const aLayer: TGRLayer);
begin
  Assert(Assigned(aLayer), 'the layer must be Assigned');
  Inherited Create(True);
  FLayer := aLayer;  
end;

procedure TGRLayerAnimator.Start;
begin
  FLastTick := GetTickCount();
  Resume;
end;

{ TGRLayerAnimator_Sample }
constructor TGRLayerAnimator_Sample.Create(const aLayer: TGRLayer; const aMaxStep: Integer);
begin
  Inherited Create(aLayer);
  FMaxStep := aMaxStep;
end;

function TGRLayerAnimator_Sample.DrawFrame(const MoveCount: Longword): Boolean; 
begin
  Result := FCurStep < (FMaxStep * 2);
  if Result then
  begin 
    FLayer.BeginUpdate;
    try
      Inc(FCurStep);
      if FCurStep > FMaxStep then
        FLayer.Top := FLayer.Top - 1
      else
        FLayer.Top := FLayer.Top + 1;
    finally
      FLayer.EndUpdate;
    end;
  end;
end;

initialization
finalization
  FreeAndNil(FLayerClasses);
end.
