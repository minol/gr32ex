{
history by riceball
  + TGRCustomLayer
    + ChangeNotifiction
    + AddChangeNotifiction
    + RemoveChangeNotifiction
  * TGRRubberBandLayer
    + change the TGRRubberBandLayer when the childLayer changed
    * [bug] can not drap it(click then drag) directly before fixed
  + TGRLayerContainer

Based on the newsgroup post (March 18, 2002,  news://news.g32.org/g32org.public.graphics32)
 <public@lischke-online.de ; <news:a755io$6t1$1@webserver9.elitedev.com>...
----

Hi all,

recently I posted two images here to show a new layers concept I have worked
out. Now that I have finished my work on it and because I want to give back
something for what I got when using Graphics32 I decided to publish my
source code.

Note however: the code I'm presenting here is not fully bug free nor can it
be expected to work on all configurations, although I tried to make it as
good as possible for what my time allowed. Additionally, I cannot give
support for it nor will I be able to continue to work on it in the
foreseeable future.

What's now in there for you?

The package (which you can download from
www.lischke-online.de/download/GR32Ex.zip contains two units and an
additional folder with cursors, all very similar to those used in Photoshop
and most of them enhanced with a shadow for use under Windows XP and Win 2K.
These cursors are automatically bound to your application when you add the
GR32_Types.pas unit to your project. It checks for the system and loads
either plain cursors or those with the alpha channel. Since there are 125
cursors in the package the resulting res file is 470KB. Consider this when
adding the cursors. But since they are made for image editing applications
this doesn't matter probably.

The other unit (GR32_ExtLayers.pas) contains a reimplementation of the
layers used currently in Graphics32. The implementation starts from
TCustomLayer and defines the following new:

- TGRTransformationLayer (the base class from which all others are derived)
- TGRGridLayer (derived from TGRTransformationLayer)
- TGRRubberBandLayer (derived from TGRTransformationLayer)
- TGRPropertyLayer (derived from TGRTransformationLayer)
- TGRTextLayer (derived from TGRPropertyLayer)
- TGRBitmapLayer (derived from TGRPropertyLayer)

Additionally there is a slightly enhanced TAffineTransformation derivate
(TExtAffineTransformation).

Here a short description of all the layers:

TGRTransformationLayer
This layer is the fundament of the other enhanced layers and provides the
functionality to translate, rotate, sheer (skew) and scale any layer. It
supports a pivot point, which is the center for rotations and proportional
transformations. Because of the rotation and sheer feature this and derived
layers are slower in handling than the current layers, but my primary goal
was to provide as many of the features of Photoshop as I could implement.

TGRGridLayer
This layer provides you with a customizable grid and support for guides.
It allows to snap coordinates to either guides, grid lines or the image
borders (just like in Photoshop). All features are switchable. This grid
cannot be rotated however and is always axis aligned.

TGRRubberBandLayer
This layer is a much enhanced reimplementation of the current rubberband
and supports almost anything what Photoshop allows. This includes to
visually rotate and scale images (negative scale values will mirror them).
It supports grid snapping of the four corners and the center, regardless of
the transformation state and correctly handles all the difficult cases for
cursor and hit test managment. The shift and control keys are supported as
well, to limit rotation to 45? multiples, to allow skewing the image and to
make proportional scaling possible (width/height ration of the layer is
constant). To make the support complete also the Alt key will be considered,
with the same effect as in, you guess it, Photoshop (e.g. sizing is done to
all four directions, with the pivot as center etc.).
NOTE: the rubber band uses many of the cursors in the GR32_Types.pas unit!

TGRPropertyLayer
This layer is a generic ancestor for the following layers and only stores
some properties, which might be used, e.g. when loading PSD files.

TGRTextLayer
This layer is not really implemented, but used as a placeholder. Once
somebody decides to write a real text layer, this one can serve as the
starting point.

TGRBitmapLayer
This is the last layer in the bundle and provides means to paint a
TBitmap32 with all the transformations applied. Additionally, it has a
PaintTo method, which allows to draw the content to other locations than the
ImgView32 container.

I have also included a draw mode property in the property layer, which is
not directly implemented in the extended layer unit. Instead it can be used
by the application to determine the draw mode (e.g. blend, subtract, add,
multiply etc.) which should be applied to the layer's pixel.

OK, that's it so far. I hope you like what I have here and also hope it gets
much extended in the future (then perhaps also with the new G32 lib).

Ciao, Mike
--
www.delphi-gems.com 
www.lischke-online.de 
www.delphi-unicode.net 
}
unit GR32_ExtLayers;

{$I Setting.inc}
//----------------------------------------------------------------------------------------------------------------------

interface

uses
 {$IFDEF Debug}
 DbugIntf,
 {$ENDIF}
  Windows, SysUtils, Classes, Types, Controls, Forms, Graphics
  , GR32
  , GR32_Resamplers
  , GR32_Containers
  , GR32_Layers
  , GR32_RepaintOpt
  , GR32_Image
  , GR32_Types
  , GR32_Transforms
  ;

type
  TGRRubberBandOptions = set of (
    rboAllowPivotMove,
    rboAllowCornerResize,
    rboAllowEdgeResize,
    rboAllowMove,
    rboAllowRotation,
    rboShowFrame,
    rboShowHandles
  );

const
  DefaultRubberbandOptions = [rboAllowCornerResize, rboAllowEdgeResize, rboAllowMove,
    rboShowFrame, rboShowHandles];

type
  TGRGridLayer = class;

  TGRLayerClass = class of TGRPositionLayer;
  TGRCustomLayer = class(TCustomLayer)
  private
    function GetCaptured: Boolean;
    procedure SetCaptured(const Value: Boolean);
  protected
    FName: string;
    FOnChange: TNotifyEvent;                     // For individual change events.
    FChangeNotificationList: TList;

    procedure AddChangeNotification(ALayer: TGRCustomLayer);
    procedure RemoveChangeNotification(ALayer: TGRCustomLayer);
    procedure ChangeNotification(ALayer: TGRCustomLayer); virtual;
    procedure DoChange; virtual;

    procedure SetName(const Value: string);
  public
    constructor Create(aLayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    {$IFDEF Designtime_Supports}
    class function RubberbandOptions: TGRRubberBandOptions; virtual;
    {$ENDIF}

  public
    property Captured: Boolean read GetCaptured write SetCaptured;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    property Name: string read FName write SetName;
  end;

  //How to express the position layer at the designtime?
  TGRPositionLayer = class(TGRCustomLayer)
  protected
    FPosition: TFloatPoint;
    FScaled: Boolean;
    {$IFDEF Designtime_Supports}
    FGridLayer: TGRGridLayer;                      // Used to snap/align coordinates.
    {$ENDIF}

    procedure SetPosition(const Value: TFloatPoint);
    procedure SetScaled(const Value: Boolean);
    {$IFDEF Designtime_Supports}
    procedure SeTGRGridLayer(const Value: TGRGridLayer);
    class function RubberbandOptions: TGRRubberBandOptions; override;
    {$ENDIF}

  protected
    function GetNativeSize: TSize; virtual;
    procedure Notification(ALayer: TCustomLayer); override;
    function GetAdjustedPosition(const P: TFloatPoint): TFloatPoint;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;
    {$IFDEF Designtime_Supports}
    procedure Paint(Buffer: TBitmap32); override;
    function DoHitTest(aX, aY: Integer): Boolean; override;
    {$ENDIF}

    {$IFDEF Designtime_Supports}
    property GridLayer: TGRGridLayer read FGridLayer write SeTGRGridLayer;
    {$ENDIF}
    property Position: TFloatPoint read FPosition write SetPosition;
    property Scaled: Boolean read FScaled write SetScaled; //Scaled with the viewport of a possible owner ImgView32.
  end;

  TGRTransformationLayer = class(TGRPositionLayer)
  protected
    FAngle: Single;                              // Given in degrees.
    FAlphaHit: Boolean;
    FTransformation: TAffineTransformation;
    FSkew: TFloatPoint;
    FScaling: TFloatPoint;
    FPivotPoint: TFloatPoint;                    // Center of rotation and proportional scaling.
    FSize: TSize;

    procedure SetAngle(Value: Single);
    procedure SetPivot(const Value: TFloatPoint);
    procedure SetScaling(const Value: TFloatPoint);
    procedure SetSkew(const Value: TFloatPoint);
    procedure SetSize(const Value: TSize);
  protected
    {$IFDEF Designtime_Supports}
    class function RubberbandOptions: TGRRubberBandOptions; override;
    procedure Paint(Buffer: TBitmap32); override;
    function DoHitTest(aX, aY: Integer): Boolean; override;
    {$ENDIF}
    function GetNativeSize: TSize;override;

  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;

    procedure GetLayerTransformation(var Transformation: TAffineTransformation);
    function GetTransformedTargetRect: TFloatRect;
    procedure ResetTransformation; virtual;
    procedure UpdateTransformation; virtual;

    property AlphaHit: Boolean read FAlphaHit write FAlphaHit;
    property Angle: Single read FAngle write SetAngle;
    property PivotPoint: TFloatPoint read FPivotPoint write SetPivot;
    property Scaling: TFloatPoint read FScaling write SetScaling;
    property Skew: TFloatPoint read FSkew write SetSkew;
    property Size: TSize read FSize write SetSize;
  end;

  // The grid elements determine what will be painted of the grid layer.
  TGridElements = set of (
    geLines,
    geHalfTicks,
    geGuides,
    geQuarterTicks
  );

  TSnapOptions = set of (
    soSnapBorders,
    soSnapGuides,
    soSnapGrid
  );

  TGRGridLayer = class(TGRTransformationLayer)
  private
    FGridSize: Integer;
    FElements: TGridElements;
    FMainGridColor,
    FHalfTickColor,
    FQuaterTickColor,
    FGuidesColor: TColor32;
    FSnapOptions: TSnapOptions;
    FSnapThreshold: Integer;
    FHorizontalGuides,
    FVerticalGuides: TList;
    procedure SetColor(const Index: Integer; const Value: TColor32);
    procedure SetElements(const Value: TGridElements);
    procedure SetGridSize(const Value: Integer);
    procedure SetSnapThreshold(const Value: Integer);
  protected
    function DoHitTest(X, Y: Integer): Boolean; override;
    function GetNativeSize: TSize; override;
    procedure Paint(Buffer: TBitmap32); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    procedure AddHorizontalGuide(Y: Integer);
    procedure AddVerticalGuide(X: Integer);
    procedure ClearGuides;
    function Snap(var P: TFloatPoint): Boolean;
    procedure RemoveHorizontalGuide(Y: Integer);
    procedure RemoveVerticalGuide(X: Integer);

    property Elements: TGridElements read FElements write SetElements default [geLines..geQuarterTicks];
    property GridSize: Integer read FGridSize write SetGridSize default 15;
    property GuidesColor: TColor32 index 3 read FGuidesColor write SetColor default clBlue32;
    property HalfTickColor: TColor32 index 1 read FHalfTickColor write SetColor default clWhite32;
    property MainGridColor: TColor32 index 0 read FMainGridColor write SetColor default clWhite32;
    property QuaterTickColor: TColor32 index 2 read FQuaterTickColor write SetColor default clWhite32;
    property SnapOptions: TSnapOptions read FSnapOptions write FSnapOptions default [soSnapBorders..soSnapGrid];
    property SnapThreshold: Integer read FSnapThreshold write SetSnapThreshold default 8;
  end;

  TGRCursorDirection = (
    cdNotUsed,
    cdNorth,
    cdNorthEast,
    cdEast,
    cdSouthEast,
    cdSouth,
    cdSouthWest,
    cdWest,
    cdNorthWest
  );

  TGRContour = array[0..3] of TFixedPoint;

  TGRRubberBandLayer = class(TGRTransformationLayer)
  protected
    FChildLayer: TGRPositionLayer;
    //FSize: TSize;                      // Real (untransformed) size of the child layer (if there is one).
                                       // Otherwise the current (unscaled) size of the rubber band.
    FOptions: TGRRubberBandOptions;
    FHandleSize: Integer;
    FHandleFrame: TColor;
    FHandleFill: TColor;
    FThreshold: Integer;               // Distance from a point, which still considers this point as hit.
    FOuterColor: TColor32;             // If the alpha value of this color is > 0 then the color is used
                                       // to blend everything outside the rubberband rect by this color.

    // Drag/resize support
    FIsDragging: Boolean;
    FDragState: TRubberbandDragState;
    FOldPosition: TFloatPoint;         // Keep the old values to restore in case of a cancellation.
    FOldScaling: TFloatPoint;
    FOldPivot: TFloatPoint;
    FOldSkew: TFloatPoint;
    FOldAngle: Single;
    FDragPos: TPoint;

    procedure SetChildLayer(const Value: TGRPositionLayer);
    procedure SetHandleFill(const Value: TColor);
    procedure SetHandleFrame(const Value: TColor);
    procedure SetHandleSize(Value: Integer);
    procedure SetOptions(const Value: TGRRubberBandOptions);
    procedure SetOuterColor(const Value: TColor32);
    //procedure SetSize(const Value: TSize);
    function GetOptions: TGRRubberBandOptions;
  protected
    function DoHitTest(X, Y: Integer): Boolean; override;
    procedure FillOuter(Buffer: TBitmap32; OuterRect: TRect; Contour: TGRContour);
    function GetCursorDirection(X, Y: Integer; AxisTolerance: Integer; State: TRubberbandDragState): TGRCursorDirection;
    function GetHitCode(X, Y: Integer; Shift: TShiftState): TRubberbandDragState;
    //function GetNativeSize: TSize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(ALayer: TCustomLayer); override;
    procedure Paint(Buffer: TBitmap32); override;
    function SnapPosition: Boolean;
    procedure UpdateChildLayer;

    procedure ChangeNotification(ALayer: TGRCustomLayer); override;
  public
    constructor Create(LayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    procedure Cancel;

    property ChildLayer: TGRPositionLayer read FChildLayer write SetChildLayer;
    property DragState: TRubberbandDragState read FDragState;
    property HandleSize: Integer read FHandleSize write SetHandleSize default 3;
    property HandleFill: TColor read FHandleFill write SetHandleFill default clWhite;
    property HandleFrame: TColor read FHandleFrame write SetHandleFrame default clBlack;
    property IsDragging: Boolean read FIsDragging;
    property Options: TGRRubberBandOptions read GetOptions write SetOptions default DefaultRubberbandOptions;
    property OuterColor: TColor32 read FOuterColor write SetOuterColor;
    //property Size: TSize read FSize write SetSize;
    property Threshold: Integer read FThreshold write FThreshold default 8;
  end;

  // TGRBitmapLayer provides some special properties as used for the image editor, like the ability for affine
  // transformation, name, lock state and other things.
  TGRLayerDrawMode = (
    ldmBlend,           // Can also be opaque if opacity is 100%.
    ldmAdd,
    ldmSubtract,
    ldmModulate,
    ldmMax,
    ldmMin
  );

  TGRPropertyLayer = class(TGRTransformationLayer)
  protected
    FLocked: Boolean;
    FDrawMode: TGRLayerDrawMode;
    procedure SetDrawMode(const Value: TGRLayerDrawMode);
  public
    procedure Assign(Source: TPersistent);override;

    property DrawMode: TGRLayerDrawMode read FDrawMode write SetDrawMode;
    property Locked: Boolean read FLocked write FLocked;
  end;

  TGRTextLayer = class(TGRPropertyLayer)
  protected
    FText: WideString;
    FTextColor: TColor32;
    procedure SetText(const Value: WideString);
    procedure SetTextColor(const Value: TColor32);
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    procedure Assign(Source: TPersistent);override;

    property Text: WideString read FText write SetText;
    property TextColor: TColor32 read FTextColor write SetTextColor default clBlack32;
  end;

  TGRBitmapLayer = class(TGRPropertyLayer)
  protected
    FBitmap: TBitmap32;
    FCropped: Boolean;
    procedure SetCropped(Value: Boolean);
    procedure SetBitmap(Value: TBitmap32);
  protected
    function DoHitTest(X, Y: Integer): Boolean; override;
    function GetNativeSize: TSize; override;
    procedure Paint(Buffer: TBitmap32); override;

    procedure BitmapChanged(Sender: TObject);virtual;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;

    procedure PaintTo(Buffer: TBitmap32; const R: TRect);

    property Bitmap: TBitmap32 read FBitmap write SetBitmap;
    property Cropped: Boolean read FCropped write SetCropped;
  end;

  TGRLayerCollection = class(TLayerCollection)
  end;

  TGRLayerContainer = class(TGRCustomLayer)
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

//----------------------------------------------------------------------------------------------------------------------
function ComponentToStr(const Component: TComponent): string;
procedure SaveStrToFile(const aFileName, s: string);
procedure ComponentToTextFile(const Component: TComponent; const aFileName: string);

implementation

uses
  Math, GR32_Polygons, GR32_MicroTiles;

type
  TAffineTransformationAccess = class(TAffineTransformation);
  TLayerAccess = class(TCustomLayer);

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

procedure ComponentToTextFile(const Component: TComponent; const aFileName: string);
var
  vBinStream:TMemoryStream;
  vFileStream: TFileStream;
  s: string;
begin
  vBinStream := TMemoryStream.Create;
  try
    vFileStream := TFileStream.Create(aFileName, fmCreate or fmShareDenyWrite);
    try
      vBinStream.WriteComponent(Component);
      vBinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(vBinStream, vFileStream);
    finally
      vFileStream.Free;
    end;
  finally
    vBinStream.Free
  end;
end;

function ComponentToStr(const Component: TComponent): string;
var
  vBinStream:TMemoryStream;
  vStrStream: TStringStream;
  s: string;
begin
  vBinStream := TMemoryStream.Create;
  try
    vStrStream := TStringStream.Create(s);
    try
      vBinStream.WriteComponent(Component);
      vBinStream.Seek(0, soFromBeginning);
      //try
      ObjectBinaryToText(vBinStream, vStrStream);
      //except
      //  on E:Exception do
      //end;
      vStrStream.Seek(0, soFromBeginning);
      Result:= vStrStream.DataString;
    finally
      vStrStream.Free;
    end;
  finally
    vBinStream.Free
  end;
end;

procedure SaveStrToFile(const aFileName, s: string);
begin
  with TStringList.Create do
  try
    Text := s;
    SaveToFile(aFileName);
  finally
    Free;
  end;
end;

{ TGRCustomLayer }
constructor TGRCustomLayer.Create(aLayerCollection: TLayerCollection);
begin
  inherited;
  LayerOptions := LOB_MOUSE_EVENTS or LOB_VISIBLE; 
end;

destructor TGRCustomLayer.Destroy;
begin
  if Assigned(FChangeNotificationList) then 
  begin
    FChangeNotificationList.Free;
    FChangeNotificationList := nil;
  end;
  inherited;
end;

procedure TGRCustomLayer.AddChangeNotification(ALayer: TGRCustomLayer);
begin
  if not Assigned(FChangeNotificationList) then FChangeNotificationList := TList.Create;
  FChangeNotificationList.Add(ALayer);
end;

procedure TGRCustomLayer.ChangeNotification(ALayer: TGRCustomLayer); 
begin
end;

procedure TGRCustomLayer.DoChange;
var
  i: integer;
begin
  if Assigned(FChangeNotificationList) then
  begin
    for i := FChangeNotificationList.Count - 1 downto 0 do
    begin
      if Assigned(FChangeNotificationList[i]) then
      try
        TGRTransformationLayer(FChangeNotificationList[i]).ChangeNotification(Self);
      except
        FChangeNotificationList.Delete(i);
      end;
    end;
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TGRCustomLayer.GetCaptured: Boolean;
begin
  Result := FLayerOptions and LOB_NO_CAPTURE = 0;
end;

procedure TGRCustomLayer.RemoveChangeNotification(ALayer: TGRCustomLayer);
begin
  if Assigned(FChangeNotificationList) then
  begin
    FChangeNotificationList.Remove(ALayer);
    if FChangeNotificationList.Count = 0 then
    begin
      FChangeNotificationList.Free;
      FChangeNotificationList := nil;
    end;
  end;
end;

procedure TGRCustomLayer.SetCaptured(const Value: Boolean);
begin
  if Value then
    LayerOptions := LayerOptions or LOB_NO_CAPTURE
  else
  begin
    LayerOptions := LayerOptions and not LOB_NO_CAPTURE
  end;
end;

procedure TGRCustomLayer.SetName(const Value: string);
begin
  if Value <> FName then
  begin
    Changing;
    FName := Value;
    Changed;

    DoChange;
  end;
end;

{$IFDEF Designtime_Supports}
class function TGRCustomLayer.RubberbandOptions: TGRRubberBandOptions;
begin
  Result := [rboShowFrame];
end;
{$ENDIF}

{ TGRPositionLayer }
destructor TGRPositionLayer.Destroy;
begin
  {$IFDEF Designtime_Supports}
  if Assigned(FGridLayer) then
    FGridLayer.RemoveNotification(Self);
  {$ENDIF}
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TGRPositionLayer.Assign(Source: TPersistent);
begin
  if Source is TGRPositionLayer then
    with Source as TGRPositionLayer do
    begin
      Changing;
      Self.GridLayer := GridLayer;
      Self.FPosition := FPosition;
      Self.FScaled := FScaled;

      Changed; // Layer collection.
      DoChange; // Layer only.
    end;
  inherited Assign(Source);
end;

function TGRPositionLayer.GetAdjustedPosition(const P: TFloatPoint): TFloatPoint;
var
  ScaleX, ScaleY, ShiftX, ShiftY: Single;
begin
  if Scaled and Assigned(FLayerCollection) then
  begin
    FLayerCollection.GetViewportShift(ShiftX, ShiftY);
    FLayerCollection.GetViewportScale(ScaleX, ScaleY);

    with Result do
    begin
      X := P.X * ScaleX + ShiftX;
      Y := P.Y * ScaleY + ShiftY;
    end;
  end
  else
    Result := P;
end;

// Returns the untransformed size of the content. Must be overriden by descentants.
function TGRPositionLayer.GetNativeSize: TSize;
begin
  Result.cx := 1;
  Result.cy := 1;
end;

procedure TGRPositionLayer.Notification(ALayer: TCustomLayer);
begin
  inherited;
  
  {$IFDEF Designtime_Supports}
  if ALayer = FGridLayer then
    FGridLayer := nil;
  {$ENDIF}
end;

{$IFDEF Designtime_Supports}
const
  cPositionLayerWdith = 16;
  cPositionLayerHeight = 16;
  
function TGRPositionLayer.DoHitTest(aX, aY: Integer): Boolean;
begin
  with GetAdjustedPosition(FPosition) do
    Result := (aX >= X - cPositionLayerWdith) and (aX < X + cPositionLayerWdith) and (aY >= Y - cPositionLayerHeight) and (aY < Y + cPositionLayerHeight);
end;

procedure TGRPositionLayer.Paint(Buffer: TBitmap32);
var
  SrcRect, DstRect, ClipRect, TempRect: TRect;
  ImageRect: TRect;
begin
  with GetAdjustedPosition(FPosition) do
  begin
    DstRect.Left := Trunc(X - cPositionLayerWdith);
    DstRect.Top := Trunc(Y - cPositionLayerHeight);
    DstRect.Right := Trunc(X + cPositionLayerWdith);
    DstRect.Bottom := Trunc(Y + cPositionLayerHeight);
  end;
  ClipRect := Buffer.ClipRect;
  IntersectRect(TempRect, ClipRect, DstRect);
  if IsRectEmpty(TempRect) then Exit;
  Buffer.RaiseRectTS(DstRect, 80);
  with DstRect do
  begin
    Buffer.LineAS(Left, Top, Right, Bottom, clBlack32);
    Buffer.LineAS(Right, Top, Left, Bottom, clBlack32);
  end;
end;

procedure TGRPositionLayer.SeTGRGridLayer(const Value: TGRGridLayer);
begin
  if Value <> FGridLayer then
  begin
    if Assigned(FGridLayer) then
      FGridLayer.RemoveNotification(Self);
    FGridLayer := Value;
    if Assigned(FGridLayer) then
      FGridLayer.AddNotification(Self);
  end;
end;

class function TGRPositionLayer.RubberbandOptions: TGRRubberBandOptions;
begin
  Result := [rboAllowMove, rboShowFrame];
end;
{$ENDIF Designtime_Supports}

procedure TGRPositionLayer.SetPosition(const Value: TFloatPoint);

begin
  Changing;
  FPosition := Value;
  Changed;

  DoChange;
end;

procedure TGRPositionLayer.SetScaled(const Value: Boolean);
begin
  if Value <> FScaled then
  begin
    Changing;
    FScaled := Value;
    Changed;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TGRTransformationLayer }
class function TGRTransformationLayer.RubberbandOptions: TGRRubberBandOptions;
begin
  Result := [rboAllowCornerResize,
    rboAllowEdgeResize,
    rboAllowMove,
    rboAllowRotation,
    rboShowFrame,
    rboShowHandles
  ];
end;

constructor TGRTransformationLayer.Create(ALayerCollection: TLayerCollection);

begin
  inherited;

  FTransformation := TAffineTransformationAccess.Create;

  FSize.cx := 32;
  FSize.cy := 32;

  ResetTransformation;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TGRTransformationLayer.Destroy;
begin
  FTransformation.Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TGRTransformationLayer.Assign(Source: TPersistent);
begin
  if Source is TGRTransformationLayer then
    with Source as TGRTransformationLayer do
    begin
      Changing;
      Self.FAngle := FAngle;
      Self.FPivotPoint := FPivotPoint;
      Self.FScaling := FScaling;
      Self.FSkew := FSkew;
      Self.FSize := FSize;

      Changed; // Layer collection.
      DoChange; // Layer only.
    end;
  inherited Assign(Source);
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TGRTransformationLayer.SetAngle(Value: Single);

begin
  Changing;
  FAngle := Value;
  Changed; // Layer collection.

  DoChange; // Layer only.
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TGRTransformationLayer.SetPivot(const Value: TFloatPoint);

begin
  Changing;
  FPivotPoint := Value;
  Changed;

  DoChange;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TGRTransformationLayer.SetScaling(const Value: TFloatPoint);

begin
  Changing;
  FScaling := Value;
  Changed;

  DoChange;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRTransformationLayer.SetSize(const Value: TSize);
begin
  Changing;
  FSize := Value;
  Changed;

  DoChange;
end;

procedure TGRTransformationLayer.SetSkew(const Value: TFloatPoint);
begin
  Changing;
  FSkew := Value;
  Changed;

  DoChange;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TGRTransformationLayer.GetLayerTransformation(var Transformation: TAffineTransformation);

// Creates Transformation if it does not exist yet and applies the current layer transformations.
// This does not include viewport scaling.
// The caller is responsible for freeing Transformation!

begin
  if Transformation = nil then
    Transformation := TAffineTransformationAccess.Create
  else
    Transformation.Clear;

  Transformation.Translate(-FPivotPoint.X, -FPivotPoint.Y);
  Transformation.Scale(FScaling.X, FScaling.Y);
  Transformation.Skew(FSkew.X, FSkew.Y);
  TransFormation.Rotate(0, 0, FAngle);
  Transformation.Translate(FPosition.X + FPivotPoint.X, FPosition.Y + FPivotPoint.Y);
end;

{$IFDEF Designtime_Supports}
procedure TGRTransformationLayer.Paint(Buffer: TBitmap32); 
var
  SrcRect, DstRect, ClipRect, TempRect: TRect;
  ImageRect: TRect;
begin
  DstRect := MakeRect(GetTransformedTargetRect);
  ClipRect := Buffer.ClipRect;
  IntersectRect(TempRect, ClipRect, DstRect);
  if IsRectEmpty(TempRect) then Exit;
  Buffer.RaiseRectTS(DstRect, -80);
  {
  with DstRect do
  begin
    Buffer.LineAS(Left, Top, Right, Bottom, clBlack32);
    Buffer.LineAS(Right, Top, Left, Bottom, clBlack32);
  end; //}
end;

function TGRTransformationLayer.DoHitTest(aX, aY: Integer): Boolean;
begin
  with GetTransformedTargetRect do
    Result := (aX >= Left) and (aX < Right) and (aY >= Top) and (aY < Bottom);
end;
{$ENDIF Designtime_Supports}

//----------------------------------------------------------------------------------------------------------------------
function TGRTransformationLayer.GetNativeSize: TSize;
begin
  Result := FSize;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGRTransformationLayer.GetTransformedTargetRect: TFloatRect;
// Helper method to transform position, size and scale factor into a rectangle which
// determines the location in a container like TImgView32.
// The rotation is not taken into account here because it is meant for special handling.
var
  vSize: TSize;
begin
  UpdateTransformation;

  vSize := GetNativeSize;
  with FPosition do
    Result := FloatRect(X, Y, FScaling.X * (X + Size.cx), FScaling.Y * (Y + Size.cy));

  if Assigned(LayerCollection) then
    with Result, LayerCollection do
    begin
      LocalToViewport(TopLeft, FScaled);
      LocalToViewport(BottomRight, FScaled);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRTransformationLayer.ResetTransformation;

begin
  Changing;
  FTransformation.Clear;
  FSkew := FloatPoint(0, 0);
  FPosition := FloatPoint(0, 0);
  FScaling := FloatPoint(1, 1);
  FAngle := 0;
  Changed;

  DoChange;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRTransformationLayer.UpdateTransformation;
var
  ShiftX, ShiftY, ScaleX, ScaleY: Single;
begin
  FTransformation.Clear;

  FTransformation.Translate(-FPivotPoint.X, -FPivotPoint.Y);
  FTransformation.Scale(FScaling.X, FScaling.Y);
  FTransformation.Skew(FSkew.X, FSkew.Y);
  FTransFormation.Rotate(0, 0, FAngle);
  FTransformation.Translate(FPosition.X + FPivotPoint.X, FPosition.Y + FPivotPoint.Y);

  // Scale to viewport if activated.
  if FScaled and Assigned(LayerCollection) then
  begin
    LayerCollection.GetViewportScale(ScaleX, ScaleY);
    FTransformation.Scale(ScaleX, ScaleY);
    LayerCollection.GetViewportShift(ShiftX, ShiftY);
    FTransformation.Translate(ShiftX, ShiftY);
  end;

end;

//----------------- TGRGridLayer -----------------------------------------------------------------------------------------

constructor TGRGridLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FElements := [geLines..geQuarterTicks];
  FGridSize := 15;
  FMainGridColor := clWhite32;
  FHalfTickColor := clWhite32;
  FQuaterTickColor := clWhite32;
  FSnapOptions := [soSnapBorders..soSnapGrid];
  FSnapThreshold := 8;

  FHorizontalGuides := TList.Create;
  FVerticalGuides := TList.Create;
  FGuidesColor := clBlue32;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TGRGridLayer.Destroy;

begin
  FHorizontalGuides.Free;
  FVerticalGuides.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRGridLayer.SetColor(const Index: Integer; const Value: TColor32);

begin
  Changing;
  case Index of
    0:
      FMainGridColor := Value;
    1:
      FHalfTickColor := Value;
    2:
      FQuaterTickColor := Value;
    3:
      FGuidesColor := Value;
  end;
  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRGridLayer.SetElements(const Value: TGridElements);

begin
  if FElements <> Value then
  begin
    Changing;
    FElements := Value;
    Changed;

    DoChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRGridLayer.SetGridSize(const Value: Integer);

begin
  if FGridSize <> Value then
  begin
    Changing;
    FGridSize := Value;
    Changed;

    DoChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRGridLayer.SetSnapThreshold(const Value: Integer);

begin
  FSnapThreshold := Value;
  if FSnapThreshold < 1 then
    FSnapThreshold := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGRGridLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  // The grid layer is currently fully transparent for the mouse.
  // TODO: Ability to manipulate guides.
  Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  // To access protected properties and methods.
  TLayerCollectionCast = class(TLayerCollection);

function TGRGridLayer.GetNativeSize: TSize;
var
  Layers: TLayerCollectionCast;
begin
  Layers := TLayerCollectionCast(LayerCollection);
  if Layers.GetOwner is TCustomImage32 then
    with TCustomImage32(Layers.GetOwner) do
    begin
      Result.cx := Bitmap.Width;
      Result.cy := Bitmap.Height;
    end
  else
  begin
    Result.cx := 1;
    Result.cy := 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRGridLayer.Paint(Buffer: TBitmap32);
var
  R: TFloatRect;
  IntR: TRect;
  X, Y,
  LocalGridSize,
  HalfGridSize,
  QuaterGridSize: Single;
  Stipple: TArrayOfColor32;
  I: Integer;
  TickSize: Integer;
  Scale, Shift, ScaleX, ScaleY,
  ShiftX, ShiftY: Single;

begin
  R := GetTransformedTargetRect;
  IntR := MakeRect(R);
  LocalGridSize := FGridSize;
  if FScaled and Assigned(LayerCollection) then
  begin
    // Currently it is assumed the viewport of the container window is scaled proportionally
    // (X and Y scale factors are equal).
    LayerCollection.GetViewportScale(ScaleX, ScaleY);
    LocalGridSize := LocalGridSize * ScaleX;
  end;

  // Set a minimum size for the grid. No need to paint it finer.
  if LocalGridSize > 0 then
  begin
    while LocalGridSize < 8 do
      LocalGridSize := 2 * LocalGridSize;
    HalfGridSize := LocalGridSize / 2;
    QuaterGridSize := HalfGridSize / 2;

    Buffer.StippleStep := FGridSize / LocalGridSize;

    with Buffer do
    begin
      // Quater distance grid.
      if (geQuarterTicks in FElements) and (AlphaComponent(FQuaterTickColor) > 0) then
      begin
        // Create a stipple pattern which takes the first "grid size" / 8th and the last "grid size" / 8th pixels
        // and makes everything else fully transparent.
        SetLength(Stipple, FGridSize);
        TickSize := Round(FGridSize / 8);
        for I := 0 to High(Stipple) do
          if (I < TickSize) or (I > FGridSize - TickSize) then
            Stipple[I] := FQuaterTickColor
          else
            Stipple[I] := 0;
        Buffer.SetStipple(Stipple);

        Y := R.Top + QuaterGridSize;
        while Y < R.Bottom do
        begin
          Buffer.StippleCounter := 0;
          HorzLineTSP(IntR.Left, Round(Y), IntR.Right);
          Y := Y + HalfGridSize;
        end;

        X := R.Left + QuaterGridSize;
        while X <= R.Right do
        begin
          Buffer.StippleCounter := 0;
          Buffer.VertLineTSP(Round(X), IntR.Top, IntR.Bottom);
          X := X + HalfGridSize;
        end;
      end;

      // Half distance grid.
      if (geHalfTicks in FElements) and (AlphaComponent(FHalfTickColor) > 0) then
      begin
        // Create a stipple pattern which takes the first "grid size" / 4th and the last "grid size" / 4th pixels
        // and makes everything else fully transparent.
        SetLength(Stipple, FGridSize);
        TickSize := Round(FGridSize / 4);
        for I := 0 to High(Stipple) do
          if (I < TickSize) or (I > FGridSize - TickSize) then
            Stipple[I] := FHalfTickColor
          else
            Stipple[I] := 0;
        Buffer.SetStipple(Stipple);

        Y := R.Top + HalfGridSize;
        while Y <= R.Bottom do
        begin
          Buffer.StippleCounter := 0;
          Buffer.HorzLineTSP(IntR.Left, Round(Y), IntR.Right);
          Y := Y + LocalGridSize;
        end;

        X := R.Left + HalfGridSize;
        while X <= R.Right do
        begin
          Buffer.StippleCounter := 0;
          Buffer.VertLineTSP(Round(X), IntR.Top, IntR.Bottom);
          X := X + LocalGridSize;
        end;
      end;

      // Main grid.
      if (geLines in FElements) and (AlphaComponent(FMainGridColor) > 0) then
      begin
        Buffer.SetStipple([FMainGridColor, 0]);
        Buffer.StippleStep := 1;

        Y := R.Top;
        while Y <= R.Bottom do
        begin
          Buffer.StippleCounter := 0;
          Buffer.HorzLineTSP(IntR.Left, Round(Y), IntR.Right);
          Y := Y + LocalGridSize;
        end;
        // Draw a line as border too.
        if Y <> R.Bottom then
          Buffer.HorzLineTSP(IntR.Left, IntR.Bottom, IntR.Right);

        X := R.Left;
        while X <= R.Right do
        begin
          Buffer.StippleCounter := 0;
          Buffer.VertLineTSP(Round(X), IntR.Top, IntR.Bottom);
          X := X + LocalGridSize;
        end;
        // Draw a line as border too.
        if X <> R.Right then
          Buffer.VertLineTSP(IntR.Right, IntR.Top, IntR.Bottom);
      end;
    end;
  end;

  // Guides.
  if (geGuides in FElements) and ((FVerticalGuides.Count > 0) or (FHorizontalGuides.Count > 0)) then
  begin
    Buffer.SetStipple([FGuidesColor, 0]);
    Buffer.StippleStep := 1;

    Buffer.StippleCounter := 0;
    Scale := 1;
    Shift := 0;
    if FScaled and Assigned(LayerCollection) then
    begin
      LayerCollection.GetViewportShift(ShiftX, ShiftY);
      Scale := ScaleX;
      Shift := ShiftX;
    end;

    for I := 0 to FVerticalGuides.Count - 1 do
      Buffer.VertLineTSP(Round(Integer(FVerticalGuides[I]) * Scale + Shift), 0, Buffer.Height);

    Buffer.StippleCounter := 0;
    Scale := 1;
    Shift := 0;
    if FScaled then
    begin
      Scale := ScaleY;
      Shift := ShiftY;
    end;

    for I := 0 to FHorizontalGuides.Count - 1 do
      Buffer.HorzLineTSP(0, Round(Integer(FHorizontalGuides[I]) * Scale + Shift), Buffer.Width);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRGridLayer.AddHorizontalGuide(Y: Integer);

begin
  Changing;
  FHorizontalGuides.Add(Pointer(Y));
  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRGridLayer.AddVerticalGuide(X: Integer);

begin
  Changing;
  FVerticalGuides.Add(Pointer(X));
  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRGridLayer.ClearGuides;

begin
  Changing;
  FHorizontalGuides.Clear;
  FVerticalGuides.Clear;
  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGRGridLayer.Snap(var P: TFloatPoint): Boolean;

// This method takes the given coordinates and looks for a border, guide or grid line which is within
// snap threshold distance. It returnes True if something was found and modifies the coordinates which
// belong to the snap point.
// Coordinates must be in layer space.

var
  I: Integer;
  Size: TSize;
  XFound,
  YFound: Boolean;
  LocalX,
  LocalY: Integer;
  
begin
  XFound := False;
  YFound := False;

  // Check the image borders first.
  if soSnapBorders in FSnapOptions then
  begin
    Size := GetNativeSize;
    if Abs(P.X) <= FSnapThreshold then
    begin
      P.X := 0;
      XFound := True;
    end
    else
      if Abs(P.X - Size.cx) <= FSnapThreshold then
      begin
        P.X := Size.cx;
        XFound := True;
      end;

    if Abs(P.Y) <= FSnapThreshold then
    begin
      P.Y := 0;
      YFound := True;
    end
    else
      if Abs(P.Y - Size.cy) <= FSnapThreshold then
      begin
        P.Y := Size.cy;
        YFound := True;
      end;
  end;

  // Check guides for snap.
  if (soSnapGuides in FSnapOptions) and not (XFound and YFound) then
  begin
    for I := 0 to FHorizontalGuides.Count - 1 do
      if Abs(Integer(FHorizontalGuides[I]) - P.Y) <= SnapThreshold then
      begin
        P.Y := Integer(FHorizontalGuides[I]);
        YFound := True;
        Break;
      end;

    for I := 0 to FVerticalGuides.Count - 1 do
      if Abs(Integer(FVerticalGuides[I]) - P.X) <= SnapThreshold then
      begin
        P.X := Integer(FVerticalGuides[I]);
        XFound := True;
        Break;
      end;
  end;

  // If no snap was found yet then try the grid.
  if (soSnapGrid in FSnapOptions) and not (XFound and YFound) then
  begin
    if not XFound then
    begin
      LocalX := Round(P.X) mod FGridSize;
      if LocalX <= SnapThreshold then
      begin
        P.X := Round(P.X) div FGridSize * FGridSize;
        XFound := True;
      end;
      if (FGridSize - LocalX) <= SnapThreshold then
      begin
        P.X := ((Round(P.X) div FGridSize) + 1) * FGridSize;
        XFound := True;
      end;
    end;

    if not YFound then
    begin
      LocalY := Round(P.Y) mod FGridSize;
      if LocalY <= SnapThreshold then
      begin
        P.Y := Round(P.Y) div FGridSize * FGridSize;
        YFound := True;
      end;
      if (FGridSize - LocalY) <= SnapThreshold then
      begin
        P.Y := ((Round(P.Y) div FGridSize) + 1) * FGridSize;
        YFound := True;
      end;
    end;
  end;
  Result := XFound or YFound;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRGridLayer.RemoveHorizontalGuide(Y: Integer);

begin
  Changing;
  FHorizontalGuides.Remove(Pointer(Y));
  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRGridLayer.RemoveVerticalGuide(X: Integer);

begin
  Changing;
  FVerticalGuides.Remove(Pointer(X));
  Changed;
end;

//----------------- TGRRubberBandLayer --------------------------------------------------------------------------------

constructor TGRRubberBandLayer.Create(LayerCollection: TLayerCollection);

begin
  inherited;

  FHandleFrame := clBlack;
  FHandleFill := clWhite;
  FOptions := DefaultRubberbandOptions;
  FHandleSize := 3;
  LayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS;
  FThreshold := 8;
  FSize.cx := 1;
  FSize.cy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TGRRubberBandLayer.Destroy;

begin
  if Assigned(FChildLayer) then
  begin
    FChildLayer.RemoveNotification(Self);
    FChildLayer.RemoveChangeNotification(Self);
  end;
  inherited;
end;

function TGRRubberBandLayer.GetOptions: TGRRubberBandOptions;
begin
  Result := FOptions;
  if Assigned(FChildLayer) then
    Result := Result * FChildLayer.RubberbandOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRRubberBandLayer.SetChildLayer(const Value: TGRPositionLayer);

begin
  if Assigned(FChildLayer) then
  begin
    FChildLayer.RemoveNotification(Self);
    FChildLayer.RemoveChangeNotification(Self);
    //Self.RemoveNotification(FChildLayer);
  end;
  FChildLayer := Value;
  if Assigned(Value) then
  begin
    //Changing;
    FSize := Value.GetNativeSize;
    FScaled := Value.FScaled;
    FPosition := Value.Position;
    if Value is TGRTransformationLayer then
    with TGRTransformationLayer(Value) do
    begin
      Self.FAngle := Angle;
      Self.FPivotPoint := PivotPoint;
      Self.FScaling := Scaling;
      Self.FSkew := Skew;
    end;

    FChildLayer.AddNotification(Self);
    FChildLayer.AddChangeNotification(Self);
    //Changed;
    //Self.AddNotification(FChildLayer);
  end
  else
  begin
    FSize.cx := 1;
    FSize.cy := 1;
    FAngle := 0;
    FPosition := FloatPoint(0, 0);
    FPivotPoint := FloatPoint(0, 0);
    FScaled := False;
    FScaling := FloatPoint(1, 1);
    FSkew := FloatPoint(0, 0);
  end;
  //fixed bug: can not drag here (rb)
  UpdateTransformation;

  if FChildLayer <> nil then
    LayerOptions := LayerOptions or LOB_NO_UPDATE
  else
    LayerOptions := LayerOptions and not LOB_NO_UPDATE;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRRubberBandLayer.SetHandleFill(const Value: TColor);

begin
  if FHandleFill <> Value then
  begin
    FHandleFill := Value;
    TLayerCollectionCast(LayerCollection).GDIUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRRubberBandLayer.SetHandleFrame(const Value: TColor);

begin
  if FHandleFrame <> Value then
  begin
    FHandleFrame := Value;
    TLayerCollectionCast(LayerCollection).GDIUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRRubberBandLayer.SetHandleSize(Value: Integer);

begin
  if Value < 1 then
    Value := 1;
  if FHandleSize <> Value then
  begin
    FHandleSize := Value;
    TLayerCollectionCast(LayerCollection).GDIUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRRubberBandLayer.SetOptions(const Value: TGRRubberBandOptions);

begin
  if FOptions <> Value then
  begin
    Changing;
    FOptions := Value;
    Changed;
    
    DoChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRRubberBandLayer.SetOuterColor(const Value: TColor32);

begin
  if FOuterColor <> Value then
  begin
    Changing;
    FOuterColor := Value;
    Changed;

    DoChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGRRubberBandLayer.DoHitTest(X, Y: Integer): Boolean;

// Generally, a rubberband layer is always accepting a mouse event. However if rotation is not allowed
// then we reject the outside of the current bounds as hit.
 
var
  Local: TPoint;
begin
  Result := Visible and inherited DoHitTest(X, Y);
 {$IFDEF Debug}
  SendDebug('DoHitTest='+IntToStr(Integer(Result))+' X='+IntToStr(X)+' Y='+IntToStr(Y));
 {$ENDIF}

  if Result and not (rboAllowRotation in Options) then
  begin
    if not TAffineTransformationAccess(FTransformation).TransformValid then
      TAffineTransformationAccess(FTransformation).PrepareTransform;
    with FTransformation do
      Local := ReverseTransform(Point(X, Y));

 {$IFDEF Debug}
  SendDebug(' Local.X='+IntToStr(Local.X)+' Y='+IntToStr(Local.Y));
  SendDebug(' Sizel.left='+IntToStr(-FThreshold)+' right='+IntToStr(FSize.cx + FThreshold));
 {$ENDIF}

    Result := PtInRect(Rect(-FThreshold, -FThreshold, FSize.cx + FThreshold, FSize.cy + FThreshold), Local);

   {$IFDEF Debug}
    sendDebug('DoHitTest.PtInRect='+IntToStr(Integer(Result)));
   {$ENDIF}
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRRubberBandLayer.FillOuter(Buffer: TBitmap32; OuterRect: TRect; Contour: TGRContour);

var
  Polygon: TArrayOfArrayOfFixedPoint;

begin
  SetLength(Polygon, 2);
  SetLength(Polygon[0], 5);
  SetLength(Polygon[1], 5);

  // The outer rectangle. It must be given with counter clock winding.
  Polygon[0][0] := FixedPoint(OuterRect.TopLeft);
  Polygon[0][1] := FixedPoint(OuterRect.Right, OuterRect.Top);
  Polygon[0][2] := FixedPoint(OuterRect.BottomRight);
  Polygon[0][3] := FixedPoint(OuterRect.Left, OuterRect.Bottom);
  Polygon[0][4] := FixedPoint(OuterRect.TopLeft);

  // The inner rectangle is drawn with reverse winding.
  Polygon[1][0] := Contour[3];
  Polygon[1][1] := Contour[2];
  Polygon[1][2] := Contour[1];
  Polygon[1][3] := Contour[0];
  Polygon[1][4] := Contour[3];
  PolyPolygonTS(Buffer, Polygon, FOuterColor, pfWinding);
end;
           
//----------------------------------------------------------------------------------------------------------------------

function TGRRubberBandLayer.GetCursorDirection(X, Y: Integer; AxisTolerance: Integer;
  State: TRubberbandDragState): TGRCursorDirection;

// Returns, depending on X and Y as well as the current transformation, the direction either relative to
// the image bounds or the rotation pivot (if not within the bounds).
// State is used to determine the pivot point (virtual center), relative to which the orientation is
// to be calculated.
// AxisTolerance determines which angle difference from a coordinate axis is still to be considered as
// aligned.

const
  Directions: array[0..18] of TGRCursorDirection = (
    cdNorth, cdNorthEast, cdEast, cdSouthEast, cdSouth, cdSouthWest, cdWest, cdNorthWest, cdNorth, cdNorthEast, cdEast,
    cdSouthEast, cdSouth, cdSouthWest, cdWest, cdNorthWest, cdNorth, cdNorthEast, cdEast
  );

  SheerDirections: array[0..3] of TGRCursorDirection = (
    cdNorth, cdEast, cdSouth, cdWest
  );

var
  dX, dY: Integer;
  PivotX,
  PivotY: Integer;
  Angle: Integer;
  Index: Integer;

begin
  Result := cdNotUsed;

  case State of
    rdsSheerN..rdsSheerW:
      Result := SheerDirections[Ord(State) - 11];
    rdsResizeN..rdsResizeNW:
      begin
        Index := Ord(State) + 5;
        Angle := Round(FAngle) mod 180;
        if Angle > AxisTolerance then
        begin
          Dec(Index);
          if Angle > 90 - AxisTolerance then
          begin
            Dec(Index);
            if Angle > 90 + AxisTolerance then
            begin
              Dec(Index);
              if Angle > 180 - AxisTolerance then
                Index := Ord(State) + 5;
            end;
          end;
        end
        else
          if Angle < -AxisTolerance then
          begin
            Inc(Index);
            if Angle < -90 + AxisTolerance then
            begin
              Inc(Index);
              if Angle < -90 - AxisTolerance then
              begin
                Inc(Index);
                if Angle < -180 + AxisTolerance then
                  Index := Ord(State) + 5;
              end;
            end;
          end;
        Result := Directions[Index];
      end;
    rdsRotate:
      begin
        // Transform coordinates into local space.
        if not TAffineTransformationAccess(FTransformation).TransformValid then
          TAffineTransformationAccess(FTransformation).PrepareTransform;

        with FTransformation do
        begin
          PivotX := Round(Matrix[0, 0] * FPivotPoint.X + Matrix[1, 0] * FPivotPoint.Y + Matrix[2, 0]);
          PivotY := Round(Matrix[0, 1] * FPivotPoint.X + Matrix[1, 1] * FPivotPoint.Y + Matrix[2, 1]);
        end;

        dX := Round(X - PivotX);
        dY := Round(Y - PivotY);
        if dX = 0 then
        begin
          if dY < 0 then
            Result := cdNorth
          else
            Result := cdSouth;
        end
        else
          if dY = 0 then
          begin
            if dX > 0 then
              Result := cdEast
            else
              Result := cdWest;
          end
          else
          begin
            // Everything within AxisTolerance from an axis is considered as would the axis have been hit.
            // Check the axes (with tolerance) first before checking all other possible directions.
            Angle := Round(RadToDeg(ArcTan2(dY, dX)));
            if (-180 <= Angle) and (Angle < -180 + AxisTolerance) then
              Result := cdWest
            else
              if (-90 - AxisTolerance <= Angle) and (Angle < -90 + AxisTolerance) then
                Result := cdNorth
              else
                if (-AxisTolerance <= Angle) and (Angle < AxisTolerance) then
                  Result := cdEast
                else
                  if (90 - AxisTolerance <= Angle) and (Angle < 90 + AxisTolerance) then
                    Result := cdSouth
                  else
                    if (180 - AxisTolerance <= Angle) and (Angle < 180) then
                      Result := cdWest
                    else // No axis aligned direction, check the others.
                      if (-180 + AxisTolerance <= Angle) and (Angle < -90 - AxisTolerance) then
                        Result := cdNorthWest
                      else
                        if (-90 + AxisTolerance <= Angle) and (Angle < -AxisTolerance) then
                          Result := cdNorthEast
                        else
                          if (AxisTolerance <= Angle) and (Angle < 90 - AxisTolerance) then
                            Result := cdSouthEast
                          else
                            if (90 + AxisTolerance <= Angle) and (Angle < 180 - AxisTolerance) then
                              Result := cdSouthWest
                            else
                              Result := cdNotUsed;
          end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGRRubberBandLayer.GetHitCode(X, Y: Integer; Shift: TShiftState): TRubberbandDragState;

// Determines the possible drag state, which the layer could enter.

var
  dX, dY: Single;
  Local: TPoint;
  LocalThresholdX,
  LocalThresholdY: Integer;
  NearTop,
  NearRight,
  NearBottom,
  NearLeft: Boolean;
  ScaleX, ScaleY: Single;
begin
  Result := rdsNone;
  //if not Assigned(FChildLayer) then exit;

  // Transform coordinates into local space.
  Local := FTransformation.ReverseTransform(Point(X, Y));

  LocalThresholdX := Abs(Round(FThreshold / FScaling.X));
  LocalThresholdY := Abs(Round(FThreshold / FScaling.Y));
  if FScaled and Assigned(LayerCollection) then
  begin
    LayerCollection.GetViewportScale(ScaleX, ScaleY);
    LocalThresholdX := Round(LocalThresholdX / ScaleX);
    LocalThresholdY := Round(LocalThresholdY / ScaleY);
  end;

  // Check rotation Pivot first.
  dX := Round(Local.X - FPivotPoint.X);
  if Abs(dX) < LocalThresholdX then
    dX := 0;
  dY := Round(Local.Y - FPivotPoint.Y);
  if Abs(dY) < LocalThresholdY then
    dY := 0;

  // Special case: rotation Pivot is hit.
  if (dX = 0) and (dY = 0) and (rboAllowPivotMove in Options) then
    Result := rdsMovePivot
  else
  begin
    // Check if the mouse is within the bounds.
    if (Local.X >= -LocalThresholdX) and (Local.X <= FSize.cx + LocalThresholdX) and (Local.Y >= -LocalThresholdY) and
      (Local.Y <= FSize.cy + LocalThresholdY) then
    begin
      Result := rdsMoveLayer;

      NearLeft := Local.X <= LocalThresholdX;
      NearRight := FSize.cx - Local.X <= LocalThresholdX;
      NearTop := Abs(Local.Y) <= LocalThresholdY;
      NearBottom := Abs(FSize.cy - Local.Y) <= LocalThresholdY;

      if rboAllowCornerResize in Options then
      begin
        // Check borders.
        if NearTop then
        begin
          if NearRight then
            Result := rdsResizeNE
          else
            if NearLeft then
              Result := rdsResizeNW;
        end
        else
          if NearBottom then
          begin
            if NearRight then
              Result := rdsResizeSE
            else
              if NearLeft then
                Result := rdsResizeSW;
          end;
      end;
      if (Result = rdsMoveLayer) and (rboAllowEdgeResize in Options) then
      begin
        // Check for border if no corner hit.
        if NearTop then
          Result := rdsResizeN
        else
          if NearBottom then
            Result := rdsResizeS
          else
            if NearRight then
              Result := rdsResizeE
            else
              if NearLeft then
                Result := rdsResizeW;
      end;

      // If the user holds down the control key then sheering becomes active (only for edges).
      if ssCtrl in Shift then
      begin
        case Result of
          rdsResizeN:
            Result := rdsSheerN;
          rdsResizeE:
            Result := rdsSheerE;
          rdsResizeS:
            Result := rdsSheerS;
          rdsResizeW:
            Result := rdsSheerW;
        end;
      end;
    end
    else
    begin
      // Mouse is not within the bounds. So if rotating is allowed we can return the rotation state.
      if rboAllowRotation in Options then
        Result := rdsRotate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRRubberBandLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  //update the DragState first.
  FDragState := GetHitCode(X, Y, Shift);
  FIsDragging := FDragState <> rdsNone;
 {$IFDEF Debug}
  sendDebug('MouseDown: IsDrag='+IntTOStr(Integer(FIsDragging)));
  sendDebug('X='+IntToStr(X)+' Y='+IntToStr(Y));
 {$ENDIF}
  if FIsDragging then
  begin
 {$IFDEF Debug}
  sendDebug('...IsDrag');
 {$ENDIF}
    FOldPosition := FPosition;
    FOldScaling := FScaling;
    FOldPivot := FPivotPoint;
    FOldSkew := FSkew;
    FOldAngle := FAngle;
    FDragPos := Point(X, Y);
  end;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

const
  Epsilon = 1E-4; // Minimal changes are not taken into account. This is the threshold.

procedure TGRRubberBandLayer.MouseMove(Shift: TShiftState; X, Y: Integer);

const
  MoveCursor: array [TGRCursorDirection] of TCursor = (
    crDefault,
    crGrMovePointNS,    // cdNorth
    crGrMovePointNESW,  // cdNorthEast
    crGrMovePointWE,    // cdEast
    crGrMovePointNWSE,  // cdSouthEast
    crGrMovePointNS,    // cdSouth
    crGrMovePointNESW,  // cdSouthWest
    crGrMovePointWE,    // cdWest
    crGrMovePointNWSE   // cdNorthWest
  );

  RotateCursor: array [TGRCursorDirection] of TCursor = (
    crDefault,
    crGrRotateN,        // cdNorth
    crGrRotateNE,       // cdNorthEast
    crGrRotateE,        // cdEast
    crGrRotateSE,       // cdSouthEast
    crGrRotateS,        // cdSouth
    crGrRotateSW,       // cdSouthWest
    crGrRotateW,        // cdWest
    crGrRotateNW        // cdNorthWest
  );

  SheerCursor: array [TGRCursorDirection] of TCursor = (
    crDefault,
    crGrArrowMoveWE,    // cdNorth
    crDefault,          // cdNorthEast
    crGrArrowMoveNS,    // cdEast
    crDefault,          // cdSouthEast
    crGrArrowMoveWE,    // cdSouth
    crDefault,          // cdSouthWest
    crGrArrowMoveNS,    // cdWest
    crDefault           // cdNorthWest
  );

var
  ScaleRatioX, ScaleRatioY,
  dX, dY,
  PivotX, PivotY,
  TransX, TransY,
  ScaleX, ScaleY: Single;

  Angle,
  Sine,                      // Sine and cosine of the current rotation angle.
  Cosine,
  RevSine,                   // Sine and cosine of the negative current rotation angle.
  RevCosine,
  T: Extended;
  DirX,
  DirY: Integer;             // Used to calculate the correct direction of scale/translation.
  Snapped: Boolean;          

  LastPosition: TFloatPoint;
  LastRotation: Single;
  LastScaling: TFloatPoint;
  
begin
  if not TAffineTransformationAccess(FTransformation).TransformValid then
    TAffineTransformationAccess(FTransformation).PrepareTransform;
  if not FIsDragging then
  begin
    FDragState := GetHitCode(X, Y, Shift);
    case FDragState of
      rdsNone:
        Cursor := crDefault;
      rdsRotate:
        Cursor := RotateCursor[GetCursorDirection(X, Y, 15, FDragState)];
      rdsMoveLayer:
        Cursor := crGrArrow;
      rdsMovePivot:
        Cursor := crGrMoveCenter;
      rdsSheerN..rdsSheerW:
        Cursor := SheerCursor[GetCursorDirection(X, Y, 15, FDragState)];
    else
      Cursor := MoveCursor[GetCursorDirection(X, Y, 15, FDragState)];
    end;
  end
  else
  begin
    // Store the current values to learn at the end whether there really were changes.
    // Because of the coordinate snapping it could well be that nothing changes for several mouse moves.
    LastPosition := FPosition;
    LastRotation := FAngle;
    LastScaling := FScaling;

    Changing;

      dX := X - FDragPos.X;
      dY := Y - FDragPos.Y;
      // Account for the viewport scale.
      if FScaled and Assigned(LayerCollection) then
      begin
        LayerCollection.GetViewportScale(ScaleX, ScaleY);
        dX := dX / ScaleX;
        dY := dY / ScaleY;
      end;

      // Calculate sine and cosine values in advance that are used to convert mouse coordinates to image coordinates.
      // Keep in mind that under Windows -Y is up so the rotations have to be mirrored.
      SinCos(DegToRad(-FAngle), Sine, Cosine);
      RevSine := -Sine;
      RevCosine := Cosine;

      // Transform mouse coordinates into layer space.
      TransX := Cosine * dX + Sine * dY;
      TransY := -Sine * dX + Cosine * dY;

      // Scale values for local coordinates determined by the ratio between top/left border to pivot position
      // (which is the center when the image gets scaled). Note: the pivot point is local to the image, so the
      // image's position doesn't matter.
      ScaleRatioX := FOldPivot.X / FSize.cx;
      ScaleRatioY := FOldPivot.Y / FSize.cy;

      DirX := 1;
      DirY := 1;
      if ssAlt in Shift then
        FPosition := FOldPosition;
        
    case FDragState of
      rdsMoveLayer:
        begin
          FPosition.X := FOldPosition.X + dX;
          FPosition.Y := FOldPosition.Y + dY;
        end;
      rdsMovePivot: // Not fully implemented. Real implementation requires special rotation management.
        begin
          FPivotPoint.X := FOldPivot.X + dX;
          FPivotPoint.Y := FOldPivot.Y + dY;
        end;
      rdsResizeN:
        begin
          // Remove horizontal part.
          TransX := 0;
          DirY := -1;
          ScaleRatioY := 1 - ScaleRatioY;
        end;
      rdsResizeNE:
        begin
          DirY := -1;
          ScaleRatioY := 1 - ScaleRatioY;
        end;
      rdsResizeE:
        TransY := 0;
      rdsResizeSE: // Nothing special to do here.
        ;
      rdsResizeS:
        TransX := 0;
      rdsResizeSW:
        begin
          ScaleRatioX := 1 - ScaleRatioX;
          DirX := -1;
        end;
      rdsResizeW:
        begin
          // Remove vertical part.
          TransY := 0;
          ScaleRatioX := 1 - ScaleRatioX;
          DirX := -1;
        end;
      rdsResizeNW:
        begin
          DirX := -1;
          DirY := -1;
          ScaleRatioX := 1 - ScaleRatioX;
          ScaleRatioY := 1 - ScaleRatioY;
        end;
      rdsSheerS,
      rdsSheerN:
        begin
          if FDragState = rdsSheerN then
            DirX := -1;
          FSkew.X := FOldSkew.X + DirX * TransX / FSize.cx;
          if not (ssAlt in Shift) then
          begin
            dX := RevCosine * TransX * ScaleRatioX;
            dY := -RevSine * TransX * ScaleRatioX;

            FPosition.X := FOldPosition.X + dX;
            FPosition.Y := FOldPosition.Y + dY;
          end;
        end;
      rdsSheerW,
      rdsSheerE:
        begin
          if FDragState = rdsSheerW then
            DirY := -1;
          FSkew.Y := FOldSkew.Y + DirY * TransY / FSize.cy;
          if not (ssAlt in Shift) then                     
          begin
            dX := RevSine * TransY * ScaleRatioY;
            dY := RevCosine * TransY * ScaleRatioY;

            FPosition.X := FOldPosition.X + dX;
            FPosition.Y := FOldPosition.Y + dY;
          end;
        end;
      rdsRotate:
        begin
          // Update cursor properly.
          Cursor := RotateCursor[GetCursorDirection(X, Y, 15, FDragState)];

          // Calculate the angle opened by the old position, the new position and the pivot point.
          with FTransformation do
          begin
            PivotX := Matrix[0, 0] * FPivotPoint.X + Matrix[1, 0] * FPivotPoint.Y + Matrix[2, 0];
            PivotY := Matrix[0, 1] * FPivotPoint.X + Matrix[1, 1] * FPivotPoint.Y + Matrix[2, 1];
          end;

          Angle := RadToDeg(ArcTan2(FDragPos.Y - PivotY, FDragPos.X - PivotX) - ArcTan2(Y - PivotY, X - PivotX));
          FAngle := FOldAngle + Angle;
          // Limit rotations to multiple of the angle raster if the shift key is pressed.
          if ssShift in Shift then
            FAngle := Round(FAngle / 15) * 15;
          if FAngle <= -180 then
            FAngle := FAngle + 360;
          if FAngle >= 180 then
            FAngle := FAngle - 360;
        end;
    end;

    if FDragState in [rdsResizeN..rdsResizeNW] then
    begin
      // Recalculate transformed coordinates if the user requests a constant ratio.
      if (ssShift in Shift) and (FDragState in [rdsResizeNE, rdsResizeSE, rdsResizeSW, rdsResizeNW]) then
      begin
        // To achieve a constant ratio we calculate the projection of the actual mouse position onto the
        // diagonal vector depending on the four corners/directions.
        T := (DirX * TransX * FSize.cx + DirY * TransY * FSize.cy) / (FSize.cx * FSize.cx + FSize.cy * FSize.cy);
        TransX := DirX * T * FSize.cx;
        TransY := DirY * T * FSize.cy;
      end;

      if not (ssAlt in Shift) then
      begin
        // Transform local coordinates back to mouse space. ScaleRatioX/Y are used to weight the coordinates
        // in the same manner as the ratio between the pivot point and the layer bounds.
        // This is also necessary to let the bounds visually follow the mouse correctly.
        dX := RevCosine * TransX * ScaleRatioX + RevSine * TransY * ScaleRatioY;
        dY := -RevSine * TransX * ScaleRatioX + RevCosine * TransY * ScaleRatioY;

        FPosition.X := FOldPosition.X + dX;
        FPosition.Y := FOldPosition.Y + dY;
        FScaling.X := FOldScaling.X + DirX * TransX / FSize.cx;
        FScaling.Y := FOldScaling.Y + DirY * TransY / FSize.cy;
      end
      else
      begin
        if ScaleRatioX < 1 then
          FScaling.X := FOldScaling.X + DirX * TransX / FSize.cx / (1 - ScaleRatioX);
        if ScaleRatioY < 1 then
          FScaling.Y := FOldScaling.Y + DirY * TransY / FSize.cy / (1 - ScaleRatioY);
      end;
    end;

    if FDragState = rdsMoveLayer then
    begin
      // Snap movements to grid. Resizing is not yet covered here.
      if Assigned(FGridLayer) then
      begin
        Snapped := SnapPosition;
        if FDragState = rdsMoveLayer then
          if Snapped then
            Cursor := crGrArrowHollow
          else
            Cursor := crGrArrow;
      end;
    end;

    // Invalidate image data only for real changes.
    if (Abs(LastPosition.X - FPosition.X + LastPosition.Y - FPosition.Y) > Epsilon) or 
      (Abs(LastRotation - FAngle) > Epsilon) or
      (Abs(LastScaling.X - FScaling.X + LastScaling.Y - FScaling.Y) > Epsilon) then
    begin
      Changing;

      UpdateChildLayer;
      Changed;

      DoChange;
    end;
  end;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRRubberBandLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  FIsDragging := False;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRRubberBandLayer.ChangeNotification(ALayer: TGRCustomLayer);
var
  SomethingChanged: Boolean;

  function Different(const F1, F2: TFloatPoint): Boolean;
  begin
    Result := False;
    
    If (F1.X <> F2.X) or (F1.Y <> F2.Y) then
    begin
      Result := True;
      SomethingChanged := True;
    end;
  end;
  
begin
  if Assigned(FChildLayer) and not FIsDragging then
  begin
    BeginUpdate; // Do not trigger FChildLayer.OnChange 5 times in a row....
    Changing; // trigger for LayerCollection

    SomethingChanged := False;

    if Different(FChildLayer.Position, Position) then Position := FChildLayer.Position;

    if FChildLayer is TGRTransformationLayer then
    with TGRTransformationLayer(FChildLayer) do
    begin
      if Angle <> Self.Angle then
      begin
        Angle := Self.Angle;
        SomethingChanged := True;
      end;
  
      if Different(Skew, Self.Skew) then Self.Skew := Skew;
      if Different(Scaling, Self.Scaling) then Self.Scaling := Scaling;
      if Different(PivotPoint, Self.PivotPoint) then Self.PivotPoint := PivotPoint;
    end;

    EndUpdate;

    if SomethingChanged then
    begin
      Changed; // trigger for LayerCollection
      //DoChange; // trigger for Layer
    end;
  end;
end;

procedure TGRRubberBandLayer.Notification(ALayer: TCustomLayer);
begin
  inherited;

  if ALayer = FChildLayer then
  begin
    FChildLayer := nil;
    FSize.cx := 1;
    FSize.cy := 1;
    FAngle := 0;
    FPosition := FloatPoint(0, 0);
    FPivotPoint := FloatPoint(0, 0);
    FScaled := False;
    FScaling := FloatPoint(1, 1);
    FSkew := FloatPoint(0, 0);
    UpdateTransformation;

    LayerOptions := LayerOptions and not LOB_NO_UPDATE;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRRubberBandLayer.Paint(Buffer: TBitmap32);

var
  Contour: TGRContour;

  //--------------- local functions -------------------------------------------

  procedure CalculateContour(X, Y, W, H: Single);

  // Constructs four vertex points from the given coordinates and sizes and
  // transforms them into a contour structure, which corresponds to the
  // current transformations.

  var
    R: TFloatRect;

  begin
    R.TopLeft := FloatPoint(X, Y);
    R.BottomRight := FloatPoint(X + W, Y + H);

    with FTransformation do
    begin
      // Upper left
      Contour[0].X := Fixed(Matrix[0, 0] * R.Left + Matrix[1, 0] * R.Top + Matrix[2, 0]);
      Contour[0].Y := Fixed(Matrix[0, 1] * R.Left + Matrix[1, 1] * R.Top + Matrix[2, 1]);

      // Upper right
      Contour[1].X := Fixed(Matrix[0, 0] * R.Right + Matrix[1, 0] * R.Top + Matrix[2, 0]);
      Contour[1].Y := Fixed(Matrix[0, 1] * R.Right + Matrix[1, 1] * R.Top + Matrix[2, 1]);

      // Lower right
      Contour[2].X := Fixed(Matrix[0, 0] * R.Right + Matrix[1, 0] * R.Bottom + Matrix[2, 0]);
      Contour[2].Y := Fixed(Matrix[0, 1] * R.Right + Matrix[1, 1] * R.Bottom + Matrix[2, 1]);

      // Lower left
      Contour[3].X := Fixed(Matrix[0, 0] * R.Left + Matrix[1, 0] * R.Bottom + Matrix[2, 0]);
      Contour[3].Y := Fixed(Matrix[0, 1] * R.Left + Matrix[1, 1] * R.Bottom + Matrix[2, 1]);
    end;
  end;

  //---------------------------------------------------------------------------

  procedure DrawContour;

  begin
    with Buffer do
    begin
      MoveToX(Contour[0].X, Contour[0].Y);
      LineToXSP(Contour[1].X, Contour[1].Y);
      LineToXSP(Contour[2].X, Contour[2].Y);
      LineToXSP(Contour[3].X, Contour[3].Y);
      LineToXSP(Contour[0].X, Contour[0].Y);
    end;
  end;

  //---------------------------------------------------------------------------

  procedure DrawHandle(X, Y: Single);

  // Special version for handle vertex calculation. Handles are fixed sized and not rotated.

  var
    XNew, YNew: Single;

  begin
    with FTransformation do
    begin
      XNew := Matrix[0, 0] * X + Matrix[1, 0] * Y + Matrix[2, 0];
      YNew := Matrix[0, 1] * X + Matrix[1, 1] * Y + Matrix[2, 1];
    end;

    Buffer.FillRectS(Round(XNew - FHandleSize), Round(YNew - FHandleSize), Round(XNew + FHandleSize),
      Round(YNew + FHandleSize), FHandleFill);
    Buffer.FrameRectS(Round(XNew - FHandleSize), Round(YNew - FHandleSize), Round(XNew + FHandleSize),
      Round(YNew + FHandleSize), FHandleFrame);
  end;

  //---------------------------------------------------------------------------

  procedure DrawPivot(X, Y: Single);

  // Special version for the pivot image. Also this image is neither rotated nor scaled.

  var
    XNew, YNew, ShiftX, ShiftY: Single;

  begin
    if FScaled and Assigned(LayerCollection) then
    begin
      LayerCollection.GetViewportScale(XNew, YNew);
      LayerCollection.GetViewportShift(ShiftX, ShiftY);
      XNew := XNew * X + ShiftX;
      YNew := YNew * Y + ShiftY;
    end
    else
    begin
      XNew := X;
      YNew := Y;
    end;

    DrawIconEx(Buffer.Handle, Round(XNew - 8), Round(YNew - 8), Screen.Cursors[crGrCircleCross], 0, 0, 0, 0, DI_NORMAL);
  end;

  //--------------- end local functions ---------------------------------------

var
  Cx, Cy: Single;

begin
  UpdateTransformation;

  CalculateContour(0, 0, FSize.cx, FSize.cy);

  if AlphaComponent(FOuterColor) > 0 then
    FillOuter(Buffer, Rect(0, 0, Buffer.Width, Buffer.Height), Contour);

  if rboShowFrame in Options then
  begin
    Buffer.SetStipple([clWhite32, clWhite32, clBlack32, clBlack32]);
    Buffer.StippleCounter := 0;
    Buffer.StippleStep := 1;
    DrawContour;
  end;

  if rboShowHandles in Options then
  begin
    DrawHandle(0, 0);
    DrawHandle(FSize.cx, 0);
    DrawHandle(FSize.cx, FSize.cy);
    DrawHandle(0, FSize.cy);
  end;

  if rboShowHandles in Options then
  begin
    Cx := FSize.cx / 2;
    Cy := FSize.cy / 2;

    DrawHandle(Cx, 0);
    DrawHandle(FSize.cx, Cy);
    DrawHandle(Cx, FSize.cy);
    DrawHandle(0, Cy);
  end;
  if rboAllowPivotMove in Options then
    DrawPivot(FPivotPoint.X, FPivotPoint.Y);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGRRubberBandLayer.SnapPosition: Boolean;

// This method is called if there is a grid layer assigned to the rubber band layer and transformations took place
// which require alignment to the grid.
// Result is True if there was an alignment, otherwise it is False.
// The caller is responsible to invalidate the container and the child layer (if any) to cause correct display.

var
  Transformation: TAffineTransformation;

  //--------------- local function --------------------------------------------
  function TrySnap(const Point: TFloatPoint): Boolean;

  var
    POld,
    PNew: TFloatPoint;
    dX,
    dY: Single;
    
  begin
    POld := Transformation.ReverseTransform(Point);
    PNew := POld;
    Result := FGridLayer.Snap(PNew);
    dX := PNew.X - POld.X;
    dY := PNew.Y - POld.Y;
    if Result and ((Abs(dX) > Epsilon) or (Abs(dY) > Epsilon)) then
    begin
      // Apply difference of transformed point and the snapped equivalent to the main position.
      FPosition.X := FPosition.X + dX;
      FPosition.Y := FPosition.Y + dY;
      // Update transformation matrix, so following tests take the modification done here into account.
      Transformation.Translate(dX, dY);
    end;
  end;

  //--------------- end local function ----------------------------------------

var
  Size: TSize;

begin
  Transformation := nil;
  try
    // We use a local transformation to avoid frequent invalidation of the main transformation.
    GetLayerTransformation(Transformation);
    TAffineTransformationAccess(FTransformation).PrepareTransform;
    Size := GetNativeSize;

    // Indepedent of the current transformations we always have to check 5 points.
    // These are the four corners and the center of the layer. Photoshop however snaps the center only
    // if the pivot point is also there. We always snap the center too.
    // We need to make all tests because there could be more than one snap point. Tests are done in reversed order.
    // That means e.g. the left upper corner is tested later than the lower right corner having so the last word
    // about snapping (which is likely what the user wants).
    // 1.) Center.
    Result := TrySnap(FloatPoint(Size.cx / 2, Size.cy / 2));
    // 2.) Lower left corner.
    if TrySnap(FloatPoint(0, Size.cy)) then
      Result := True;
    // 3.) Lower right corner.
    if TrySnap(FloatPoint(Size.cx, Size.cy)) then
      Result := True;
    // 4.) Top right corner.
    if TrySnap(FloatPoint(Size.cx, 0)) then
      Result := True;
    // 5.) Top left corner.
    if TrySnap(FloatPoint(0, 0)) then
      Result := True;
  finally
    Transformation.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRRubberBandLayer.UpdateChildLayer;
var
  SomethingChanged: Boolean;

  function Different(const F1, F2: TFloatPoint): Boolean;
  begin
    Result := False;
    
    If (F1.X <> F2.X) or (F1.Y <> F2.Y) then
    begin
      Result := True;
      SomethingChanged := True;
    end;
  end;
  
begin
  if Assigned(FChildLayer) then
  begin
    FChildLayer.BeginUpdate; // Do not trigger FChildLayer.OnChange 5 times in a row....
    FChildLayer.Changing; // trigger for LayerCollection

    SomethingChanged := False;

    if Different(FChildLayer.Position, Position) then FChildLayer.Position := Position;

    if FChildLayer is TGRTransformationLayer then
    with TGRTransformationLayer(FChildLayer) do
    begin
      if Angle <> Self.Angle then
      begin
        Angle := Self.Angle;
        SomethingChanged := True;
      end;
  
      if Different(Skew, Self.Skew) then Skew := Self.Skew;
      if Different(Scaling, Self.Scaling) then Scaling := Self.Scaling;
      if Different(PivotPoint, Self.PivotPoint) then PivotPoint := Self.PivotPoint;
    end;

    FChildLayer.EndUpdate;

    if SomethingChanged then
    begin
      FChildLayer.Changed; // trigger for LayerCollection
      FChildLayer.DoChange; // trigger for Layer
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRRubberBandLayer.Cancel;

begin
  if FIsDragging then
  begin
    FIsDragging := False;

    FPosition := FOldPosition;
    FScaling := FOldScaling;
    FPivotPoint := FOldPivot;
    FSkew := FOldSkew;
    FAngle := FOldAngle;

    UpdateChildLayer;
    Changed;

    DoChange;
  end;
end;

//----------------- TGRPropertyLayer -------------------------------------------------------------------------------------
procedure TGRPropertyLayer.Assign(Source: TPersistent);
begin
  if Source is TGRPropertyLayer then
    with Source as TGRPropertyLayer do
    begin
      Changing;
      Self.FName := FName;
      Self.FDrawMode := FDrawMode;
      Self.FLocked := FLocked;
      Changed;
    end;
  inherited Assign(Source);
end;

procedure TGRPropertyLayer.SetDrawMode(const Value: TGRLayerDrawMode);

begin
  if FDrawMode <> Value then
  begin
    Changing;
    FDrawMode := Value;
    Changed;
  end;
end;

//----------------- TGRTextLayer -----------------------------------------------------------------------------------------

constructor TGRTextLayer.Create(ALayerCollection: TLayerCollection);

begin
  inherited;

  FTextColor := clBlack32;
end;

procedure TGRTextLayer.Assign(Source: TPersistent);
begin
  if Source is TGRTextLayer then
    with Source as TGRTextLayer do
    begin
      Changing;
      Self.FText := FText;
      Self.FTextColor := FTextColor;
      Changed;
    end;
  inherited Assign(Source);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRTextLayer.SetText(const Value: WideString);

begin
  if FText <> Value then
  begin
    Changing;
    FText := Value;
    Changed;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRTextLayer.SetTextColor(const Value: TColor32);

begin
  if FTextColor <> Value then
  begin
    Changing;
    FTextColor := Value;
    Changed;
  end;
end;

//----------------- TGRBitmapLayer ------------------------------------------------------------------------------------

constructor TGRBitmapLayer.Create(ALayerCollection: TLayerCollection);

begin
  inherited;

  FBitmap := TBitmap32.Create;
  FBitmap.DrawMode := dmBlend;
  FBitmap.OnChange := BitmapChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TGRBitmapLayer.Destroy;

begin
  FBitmap.Free;

  inherited;
end;

procedure TGRBitmapLayer.Assign(Source: TPersistent);
begin
  if Source is TGRBitmapLayer then
    with Source as TGRBitmapLayer do
    begin
      Changing;
      Self.FBitmap.Assign(FBitmap);
      Self.FCropped := FCropped;
      Changed;
    end;
  inherited Assign(Source);
end;
//----------------------------------------------------------------------------------------------------------------------

procedure TGRBitmapLayer.BitmapChanged(Sender: TObject);

begin
  Changing;
  FTransformation.SrcRect := FloatRect(0, 0, Bitmap.Width - 1, Bitmap.Height - 1);
  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRBitmapLayer.SetBitmap(Value: TBitmap32);

begin
  Changing;
  FBitmap.Assign(Value);
  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRBitmapLayer.SetCropped(Value: Boolean);

begin
  if Value <> FCropped then
  begin
    Changing;
    FCropped := Value;
    Changed;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGRBitmapLayer.DoHitTest(X, Y: Integer): Boolean;

var
  B: TPoint;
begin
  B := FTransformation.ReverseTransform(Point(X, Y));

  Result := PtInRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), B);
  if Result and AlphaHit and (Bitmap.PixelS[B.X, B.Y] and $FF000000 = 0) then
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGRBitmapLayer.GetNativeSize: TSize;
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
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRBitmapLayer.Paint(Buffer: TBitmap32);
begin 
  {$IFDEF Designtime_Supports}
  if FBitmap.Empty then
  begin
    inherited Paint(Buffer);
    exit;
  end;
  {$ENDIF}
  UpdateTransformation;
  // TODO: cropping
  if not TAffineTransformationAccess(FTransformation).TransformValid then
    TAffineTransformationAccess(FTransformation).PrepareTransform;
  Transform(Buffer, FBitmap, FTransformation);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGRBitmapLayer.PaintTo(Buffer: TBitmap32; const R: TRect);
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

//----------------------------------------------------------------------------------------------------------------------

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

initialization
finalization
  FreeAndNil(FLayerClasses);
end.
