
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
 *   Based on the newsgroup post GR32_ExtLayers (March 18, 2002,  news://news.g32.org/g32org.public.graphics32)
 *   <public@lischke-online.de ; <news:a755io$6t1$1@webserver9.elitedev.com>...
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_Layers;

{$I Setting.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes, Types
  , GR32
  , GR32_Resamplers
  , GR32_Containers
  , GR32_Layers
  , GR32_RepaintOpt
  , GR32_Image
  , GR32_Types
  , GR32_Transforms
  //, GR32_ExtLayers
  , GR_Animation
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
  cDefaultRubberbandOptions = [rboAllowCornerResize, rboAllowEdgeResize, rboAllowMove,
    rboShowFrame, rboShowHandles];

type
  TGRGridLayer = class;
  TGRLayerClass = class of TGRCustomLayer;

  TGRCustomLayer = class(TPositionedLayer)
  protected
    {$IFDEF Designtime_Supports}
    FGridLayer: TGRGridLayer;                      // Used to snap/align coordinates.
    {$ENDIF}
    FName: string;
    FOnChange: TNotifyEvent;                     // For individual change events.
    FChangeNotificationList: TList;

    procedure AddChangeNotification(ALayer: TGRCustomLayer);
    procedure RemoveChangeNotification(ALayer: TGRCustomLayer);
    function GetCaptured: Boolean;
    procedure SetCaptured(const Value: Boolean);
    procedure SetName(const Value: string);

    procedure ChangeNotification(ALayer: TGRCustomLayer); virtual;
    procedure DoChange; virtual;
    procedure Notification(ALayer: TCustomLayer); override;
    procedure Changed; overload; override;


    {$IFDEF Designtime_Supports}
    procedure SetGridLayer(const Value: TGRGridLayer);
    class function RubberbandOptions: TGRRubberBandOptions; virtual;
    procedure Paint(Buffer: TBitmap32); override;
    {$ENDIF}

  public
    //the RegisteredEvents are TGRNotifyEventStr only!
    class procedure GetRegisteredEvents(const aStrs: TStrings);
    class procedure GetRegisteredBehaviors(const aStrs: TStrings);

    constructor Create(aLayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;

  public
    {$IFDEF Designtime_Supports}
    property GridLayer: TGRGridLayer read FGridLayer write SetGridLayer;
    {$ENDIF}
    property Captured: Boolean read GetCaptured write SetCaptured;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    property Name: string read FName write SetName;
  end; //}

  TGRTransformationLayer = class(TGRCustomLayer)
  protected
    FAngle: Single;                              // Given in degrees.
    FTransformation: TAffineTransformation;
    FSkew: TFloatPoint;
    FScaling: TFloatPoint;
    FPivotPoint: TFloatPoint;                    // Center of rotation and proportional scaling.

    procedure SetAngle(Value: Single);
    procedure SetPivot(const Value: TFloatPoint);
    procedure SetScaling(const Value: TFloatPoint);
    procedure SetSkew(const Value: TFloatPoint);

    procedure ReadSkewData(aReader: TStream);
    procedure WriteSkewData(aWriter: TStream);
    procedure ReadPivotData(aReader: TStream);
    procedure WritePivotData(aWriter: TStream);
    procedure ReadScalingData(aReader: TStream);
    procedure WriteScalingData(aWriter: TStream);
  protected
    {$IFDEF Designtime_Supports}
    class function RubberbandOptions: TGRRubberBandOptions; override;
    procedure Paint(Buffer: TBitmap32); override;
    function DoHitTest(aX, aY: Integer): Boolean; override;
    {$ENDIF}
    function GetAdjustedRect(const R: TFloatRect): TFloatRect; override;


    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;

    // Creates Transformation if it does not exist yet and applies the current layer transformations.
    // This does not include viewport scaling.
    // The caller is responsible for freeing Transformation!
    procedure GetLayerTransformation(var aTransformation: TAffineTransformation);
    procedure ResetTransformation;
    procedure UpdateTransformation; virtual;

    property Angle: Single read FAngle write SetAngle;
    property PivotPoint: TFloatPoint read FPivotPoint write SetPivot;
    property Scaling: TFloatPoint read FScaling write SetScaling;
    property Skew: TFloatPoint read FSkew write SetSkew;

  end;

  TGRNotifyEvent = procedure(const Sender: TObject; const aParams: string = '') of object;
  TGRNotifyEventStr = type string;

  TGRLayer = class(TGRTransformationLayer)
    //the behaviors: the last string MUST be 'Behavior'
    procedure ScriptBehavior(const Sender: TObject; const aScript: string; const aEvent: string = ''; const aParams: string = '');
  protected
    FWidth: Integer;
    FHeight: Integer;

    //the script or method name.
    FOnClickStr: TGRNotifyEventStr;
    FOnRightClickStr: TGRNotifyEventStr;
    FOnDoubleClickStr: TGRNotifyEventStr;
    FOnRightDoubleClickStr: TGRNotifyEventStr;
    FOnMiddleClickStr: TGRNotifyEventStr;

    function GetLeft: Integer;
    function GetTop: Integer;
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure RunScriptBehavior(const aScript: TGRNotifyEventStr; const aEvent: string = ''; aParams: string = '');

  public
    constructor Create(ALayerCollection: TLayerCollection);override;
    procedure Assign(Source: TPersistent);override;
  published
    property Left:Integer read GetLeft write SetLeft;
    property Top:Integer read GetTop write SetTop;
    property Width:Integer read FWidth write SetWidth;
    property Height:Integer read FHeight write SetHeight;

    property OnClick: TGRNotifyEventStr read FOnClickStr write FOnClickStr;
    property OnRightClick: TGRNotifyEventStr read FOnRightClickStr write FOnRightClickStr;
    property OnDoubleClick: TGRNotifyEventStr read FOnDoubleClickStr write FOnDoubleClickStr;
    property OnRightDoubleClick: TGRNotifyEventStr read FOnRightDoubleClickStr write FOnRightDoubleClickStr;
    property OnMiddleClick: TGRNotifyEventStr  read FOnMiddleClickStr write FOnMiddleClickStr;

    property Name;
    property Cursor;
    //property Cropped;
    //property DrawMode;
    property Angle;
    //property Skew;
    //property PivotPoint;
    property Scaled;
    //property Scaling;
    property Visible;
    property MouseEvents;
  end;

  TGRBitmapLayer = class(TGRLayer)
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

  TGRAnimationLayer = class(TGRLayer)
  protected
    FAnimation: TGRAnimation;
    procedure SetAnimation(const Value: TGRAnimation);
  public
    constructor Create(LayerCollection: TLayerCollection);override;
    destructor Destroy; override;
    property Animation: TGRAnimation read FAnimation write SetAnimation;
  end;

  TGRAniFrameEvent = procedure(const Sender: TGRLayer; const aCurrDuration: Longword; var Done: Boolean) of object;
  TGRLayerAnimator = class(TThread)
  protected
    //the Duration is ms
    //means the play-time of the animation .
    //0 means for-ever, but some animator do not allow this!!
    FDuration: Longword;
    //the adjstedDuration for delay.
    FAdjustedDuration: Longword;
    FIsPaused: Boolean;
    FLastFrameTick: Longword;  //last Frame Tick
    FStartTick: Longword;
    FLayer: TGRLayer;
    FOnFrame: TGRAniFrameEvent;
    //return true means DrawFrame is done. the animation is over.
    function DrawFrame(const aCurrDuration: Longword): Boolean; virtual;
    procedure Execute; override;
  public
    constructor Create(const aLayer: TGRLayer; const aDuration: Longword);
    procedure Start;
    property Paused: Boolean read FIsPaused write FIsPaused;
    property OnFrame: TGRAniFrameEvent read FOnFrame write FOnFrame;
  end;

  TGRLayerAnimator_Line = class(TSDPlayingLayerAnimator)
  protected
    FBeginX, FBeginY: Integer;
    FEndX, FEndY: Integer;
    FMaxStep: Integer;
    function DrawFrame(const aCurrDuration: Longword): Boolean; override;
  public
    constructor Create(const aLayer: TGRLayer; const aDuration: Longword; const aEndX, aEndY: Integer);
  end;

  TGRLayerAnimator_Sample = class(TGRLayerAnimator)
  protected
    FCurStep: Integer;
    FOldTop: Integer;
    FMaxStep: Integer;
    function DrawFrame(const aCurrDuration: Longword): Boolean; override;
  public
    constructor Create(const aLayer: TGRLayer; const aMaxStep: Integer);
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

    property Layers: TGRLayerCollection read FLayers;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Buffer: TBitmap32 read FBuffer;
    property RepaintMode: TRepaintMode read FRepaintMode write SetRepaintMode default rmFull;
  end;

procedure RegisterLayer(const aLayerClass: TGRLayerClass);
function GetLayerClass(const aClassName: string): TGRLayerClass;
function GLayerClasses: TThreadList;
//----------------------------------------------------------------------------------------------------------------------
function ComponentToStr(const Component: TComponent): string;
procedure SaveStrToFile(const aFileName, s: string);
procedure ComponentToTextFile(const Component: TComponent; const aFileName: string);

implementation

uses
  Math, StrUtils, TypInfo, uMeTypInfo, GR32_MicroTiles, GR_ImageEx;

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
  TAffineTransformationAccess = class(TAffineTransformation);

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

procedure RegisterLayer(const aLayerClass: TGRLayerClass);
begin
  with GLayerClasses.LockList do
  try
    if IndexOf(aLayerClass) < 0 then
      Add(aLayerClass);
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
class procedure TGRCustomLayer.GetRegisteredEvents(const aStrs: TStrings);
var
  vPropCount: Integer;
  vPropList: PPropList;
  vPropInfo: PPropInfo;
  I: integer;
begin
  vPropCount := GetPropList(ClassInfo, tkProperties, nil);
  GetMem(vPropList, vPropCount * SizeOf(PPropInfo));
  with aStrs do
  try
    GetPropList(Self.ClassInfo, tkProperties, vPropList);
    Clear;
    for I := 0 to Pred(vPropCount) do
    begin
      vPropInfo := vPropList[I];
      if vPropInfo.PropType^ = TypeInfo(TGRNotifyEventStr) then
        Add(vPropInfo.Name);
    end;
  finally
    FreeMem(vPropList);
  end;
  //MessageBox(0, PChar(IntToStr(aStrs.Count)),'vPropCount', MB_OK);
end;

const
  cBehavior = 'Behavior';
class procedure TGRCustomLayer.GetRegisteredBehaviors(const aStrs: TStrings);
  procedure AddClassBehaviors(const aClass: TClass);
  var
    i: integer;
    vItem: PPublishedMethodEntry;
  begin
    vItem := GetFirstPublishedMethodEntry(aClass);
    for i := 0 to GetPublishedMethodCount(aClass)-1 do
    begin
      if Assigned(vItem) and (RightStr(vItem.Name, Length(cBehavior)) = cBehavior) then 
      begin
          aStrs.AddObject(vItem.Name, vItem.Address);
      end;
      vItem := GetNextPublishedMethodEntry(aClass, vItem);
    end;
  end;
var
  vParent: TClass;
begin
  with aStrs do
  begin
    Clear;
    vParent := Self;
    while vParent <> nil do
    begin
      AddClassBehaviors(vParent);
      vParent := vParent.ClassParent;
    end;
  end;
end;

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

  {$IFDEF Designtime_Supports}
  if Assigned(FGridLayer) then
    FGridLayer.RemoveNotification(Self);
  {$ENDIF}
  inherited;
end;

procedure TGRCustomLayer.Assign(Source: TPersistent);
begin
  if Source is TGRCustomLayer then
    with Source as TGRCustomLayer do
    begin
      Changing;
  {$IFDEF Designtime_Supports}
      Self.GridLayer := GridLayer;
  {$ENDIF}
      Self.FCursor := FCursor;
      Self.FLocation := FLocation;
      Self.FScaled := FScaled;

      Changed; // Layer collection.
      DoChange; // Layer only.
    end;
  inherited Assign(Source);
end;

procedure TGRCustomLayer.AddChangeNotification(ALayer: TGRCustomLayer);
begin
  if not Assigned(FChangeNotificationList) then FChangeNotificationList := TList.Create;
  FChangeNotificationList.Add(ALayer);
end;

procedure TGRCustomLayer.ChangeNotification(ALayer: TGRCustomLayer); 
begin
end;

procedure TGRCustomLayer.Changed;
begin
  inherited;
  DoChange;
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

procedure TGRCustomLayer.Notification(ALayer: TCustomLayer);
begin
  inherited;
  
  {$IFDEF Designtime_Supports}
  if ALayer = FGridLayer then
    FGridLayer := nil;
  {$ENDIF}
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
  end;
end;

{$IFDEF Designtime_Supports}
class function TGRCustomLayer.RubberbandOptions: TGRRubberBandOptions;
begin
  Result := [rboShowFrame];
  //Result := [rboAllowMove, rboShowFrame, rboAllowCornerResize, rboAllowEdgeResize];
end;

procedure TGRCustomLayer.Paint(Buffer: TBitmap32);
var
  SrcRect, DstRect, ClipRect, TempRect: TRect;
  ImageRect: TRect;
begin
  DstRect := MakeRect(GetAdjustedLocation);
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

procedure TGRCustomLayer.SetGridLayer(const Value: TGRGridLayer);
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
{$ENDIF Designtime_Supports}


{ TGRTransformationLayer }
{$IFDEF Designtime_Supports}
class function TGRTransformationLayer.RubberbandOptions: TGRRubberBandOptions;
begin
  Result := [rboAllowCornerResize,
    rboAllowEdgeResize,
    rboAllowMove,
    rboShowFrame,
    rboShowHandles
  ];
end;
{$ENDIF Designtime_Supports}

constructor TGRTransformationLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  FTransformation := TAffineTransformationAccess.Create;

  FLocation.Right  := 32;
  FLocation.Bottom := 32;

  ResetTransformation;
end;

destructor TGRTransformationLayer.Destroy;
begin
  FTransformation.Free;
  inherited;
end;

procedure TGRTransformationLayer.Assign(Source: TPersistent);
var
  vM: TMethod;
begin
  if Source is TGRTransformationLayer then
    with Source as TGRTransformationLayer do
    begin
      Changing;
      Self.FAngle := FAngle;
      Self.FPivotPoint := FPivotPoint;
      Self.FScaling := FScaling;
      Self.FSkew := FSkew;

      Changed; // Layer collection.
    end;
  inherited Assign(Source);
end;

procedure TGRTransformationLayer.DefineProperties(Filer: TFiler);
  function DoSkewWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TGRTransformationLayer)
    else
      Result := (Skew.X <> 0) or (Skew.Y <> 0);
  end;
  function DoPivotWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TGRTransformationLayer)
    else
      Result := (PivotPoint.X <> 0) or (PivotPoint.Y <> 0);
  end;
  function DoScalingWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TGRTransformationLayer)
    else
      Result := (Scaling.X <> 1) or (Scaling.Y <> 1);
  end;
begin
  inherited;
  Filer.DefineBinaryProperty('Skew', ReadSkewData, WriteSkewData, DoSkewWrite);
  Filer.DefineBinaryProperty('PivotPoint', ReadPivotData, WritePivotData, DoPivotWrite);
  Filer.DefineBinaryProperty('Scaling', ReadScalingData, WriteScalingData, DoScalingWrite);
end;

function TGRTransformationLayer.GetAdjustedRect(const R: TFloatRect): TFloatRect;
begin
  UpdateTransformation;
  Result := FloatRect(FTransformation.GetTransformedBounds);
end;

procedure TGRTransformationLayer.GetLayerTransformation(var aTransformation: TAffineTransformation);
begin
  if aTransformation = nil then
    aTransformation := TAffineTransformationAccess.Create
  else
    aTransformation.Clear;

  aTransformation.Translate(-FPivotPoint.X, -FPivotPoint.Y);
  aTransformation.Scale(FScaling.X, FScaling.Y);
  aTransformation.Skew(FSkew.X, FSkew.Y);
  aTransFormation.Rotate(0, 0, FAngle);
  aTransformation.Translate(FLocation.Left + FPivotPoint.X, FLocation.Top + FPivotPoint.Y);
end;

procedure TGRTransformationLayer.ResetTransformation;
begin
  Changing;
  FTransformation.Clear;
  //FSkew := FloatPoint(0, 0);
  //FLocation := FloatRect(0, 0, 0, 0);
  //FScaling := FloatPoint(1, 1);
  //FAngle := 0;
  Changed;
end;

procedure TGRTransformationLayer.UpdateTransformation;
var
  ShiftX, ShiftY, ScaleX, ScaleY: Single;
begin
  FTransformation.Clear;

  FTransformation.Translate(-FPivotPoint.X, -FPivotPoint.Y);
  FTransformation.Scale(FScaling.X, FScaling.Y);
  FTransformation.Skew(FSkew.X, FSkew.Y);
  FTransFormation.Rotate(0, 0, FAngle);
  FTransformation.Translate(FLocation.Left + FPivotPoint.X, FLocation.Top + FPivotPoint.Y);

  // Scale to viewport if activated.
  if FScaled and Assigned(LayerCollection) then
  begin
    LayerCollection.GetViewportScale(ScaleX, ScaleY);
    FTransformation.Scale(ScaleX, ScaleY);
    LayerCollection.GetViewportShift(ShiftX, ShiftY);
    FTransformation.Translate(ShiftX, ShiftY);
  end;
  //FTransformation.SrcRect := FLocation;
end;

procedure TGRTransformationLayer.ReadSkewData(aReader: TStream);
begin
  aReader.Read(FSkew, SizeOf(FSkew));
end;

procedure TGRTransformationLayer.ReadPivotData(aReader: TStream);
begin
  aReader.Read(FPivotPoint, SizeOf(FPivotPoint));
end;

procedure TGRTransformationLayer.ReadScalingData(aReader: TStream);
begin
  aReader.Read(FScaling, SizeOf(FScaling));
end;

procedure TGRTransformationLayer.WriteSkewData(aWriter: TStream);
begin
  aWriter.Write(FSkew, SizeOf(FSkew));
end;

procedure TGRTransformationLayer.WritePivotData(aWriter: TStream);
begin
  aWriter.Write(FPivotPoint, SizeOf(FPivotPoint));
end;

procedure TGRTransformationLayer.WriteScalingData(aWriter: TStream);
begin
  aWriter.Write(FScaling, SizeOf(FScaling));
end;

procedure TGRTransformationLayer.SetAngle(Value: Single);
begin
  Changing;
  FAngle := Value;
  Changed; // Layer collection.
end;

procedure TGRTransformationLayer.SetPivot(const Value: TFloatPoint);
begin
  Changing;
  FPivotPoint := Value;
  Changed;
end;

procedure TGRTransformationLayer.SetScaling(const Value: TFloatPoint);
begin
  Changing;
  FScaling := Value;
  Changed;
end;

procedure TGRTransformationLayer.SetSkew(const Value: TFloatPoint);
begin
  Changing;
  FSkew := Value;
  Changed;
end;

{ TGRLayer }
constructor TGRLayer.Create(aLayerCollection: TLayerCollection);
begin
  inherited;
  //LayerOptions := LOB_MOUSE_EVENTS or LOB_VISIBLE; 
end;

procedure TGRLayer.Assign(Source: TPersistent);
var
  vM: TMethod;
begin
  if Source is TGRLayer then
    with Source as TGRLayer do
    begin
      Self.FOnClickStr := FOnClickStr;
      Self.FOnRightClickStr := FOnRightClickStr;
      Self.FOnDoubleClickStr := FOnDoubleClickStr;
      Self.FOnRightDoubleClickStr := FOnRightDoubleClickStr;
      Self.FOnMiddleClickStr := FOnMiddleClickStr;
    end;
  inherited Assign(Source);
end;

function TGRLayer.GetLeft: Integer;
begin
  Result := Trunc(FPosition.X);
end;

function TGRLayer.GetTop: Integer;
begin
  Result := Trunc(FPosition.Y);
end;

procedure TGRLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF Designtime_Supports}
  if LayerCollection.Owner is TImage32Editor then
    exit;
  {$ENDIF}
  case Button of
    mbLeft: 
      begin
        if ssDouble in Shift then
        begin
          if FOnDoubleClickStr <> '' then RunScriptBehavior(FOnDoubleClickStr, 'OnDoubleClick', '');
        end
        else
          if FOnClickStr <> '' then RunScriptBehavior(FOnClickStr, 'OnClick', '');
      end;
    mbRight:
      begin
        if ssDouble in Shift then
        begin
          if FOnRightDoubleClickStr <> '' then RunScriptBehavior(FOnRightDoubleClickStr, 'OnRightDoubleClick', '');
        end
        else
          if FOnRightClickStr <> '' then RunScriptBehavior(FOnRightClickStr, 'OnRightClick', '');
      end;
    mbMiddle:
      begin
        if FOnMiddleClickStr <> '' then RunScriptBehavior(FOnMiddleClickStr, 'OnMiddleClick', '');
      end;
  end; //case
end;

procedure TGRLayer.ScriptBehavior(const Sender: TObject; const aScript: string; const aEvent: string; const aParams: string);
begin
  //TODO ScriptBehavior
  MessageBox(0, PChar(aScript+#13#10+'Event:'+aEvent),'Run Script', MB_OK);
end;

procedure TGRLayer.SetHeight(const Value: Integer);
begin
  if FSize.cy <> value then
  begin
    Changing;
    FSize.cy := Value;
    Changed;
;
  end;
end;

procedure TGRLayer.SetLeft(const Value: Integer);
begin
  if Trunc(FPosition.X) <> value then
  begin
    Changing;
    FPosition.X := Value;
    Changed;
  end;
end;

procedure TGRLayer.SetTop(const Value: Integer);
begin
  if Trunc(FPosition.Y) <> value then
  begin
    Changing;
    FPosition.Y := Value;
    Changed;
  end;
end;

procedure TGRLayer.SetWidth(const Value: Integer);
begin
  if FSize.cx <> value then
  begin
    Changing;
    FSize.cx := Value;
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

procedure TGRBitmapLayer.BitmapChanged(Sender: TObject);
begin
  Changing;
  with GetNativeSize do
    FTransformation.SrcRect := FloatRect(0, 0, cx - 1, cy - 1);
  Changed;
  //DoChange;
end;

procedure TGRBitmapLayer.SetBitmap(Value: TBitmap32);
begin
  Changing;
  FBitmap.Assign(Value);
  Changed;
end;

procedure TGRBitmapLayer.SetCropped(Value: Boolean);
begin
  if Value <> FCropped then
  begin
    Changing;
    FCropped := Value;
    Changed;
  end;
end;

function TGRBitmapLayer.DoHitTest(X, Y: Integer): Boolean;

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
  
  B := FTransformation.ReverseTransform(Point(X, Y));

  Result := PtInRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), B);
  if Result and AlphaHit and (Bitmap.PixelS[B.X, B.Y] and $FF000000 = 0) then
    Result := False;
end;

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
constructor TGRLayerAnimator.Create(const aLayer: TGRLayer; const aDuration: Longword);
begin
  Assert(Assigned(aLayer), 'the layer must be Assigned');
  Inherited Create(True);
  FLayer := aLayer;  
  FDuration := aDuration;
  FAdjustedDuration := aDuration;
end;

function TGRLayerAnimator.DrawFrame(const aCurrDuration: Longword): Boolean; 
begin
  Result := Assigned(FOnFrame) and ((FDuration=0) or (aCurrDuration > FDuration));
  if Result then FOnFrame(FLayer, aCurrDuration, Result);
end;

procedure TGRLayerAnimator.Execute; 
var
  vDone: Boolean;
begin
  
  while not Terminated do
  begin
    vDone := DrawFrame(GetTickCount - FStartTick);
    FLastFrameTick := GetTickCount;
    if vDone then break;
    Sleep(cAniIntervalCount);
    if FIsPaused then Suspend;
  end;
end;

procedure TGRLayerAnimator.Start;
begin
  FLastFrameTick := GetTickCount();
  FStartTick := FLastFrameTick;
  FIsPaused := False;
  Resume;
end;

{ TGRLayerAnimator_Line }
constructor TGRLayerAnimator_Line.Create(const aLayer: TGRLayer; const aDuration: Longword; const aEndX, aEndY: Integer);
begin
  Inherited Create(aLayer, aDuration);
  FEndX := aEndX;
  FEndY := aEndY;
  FBeginX := aLayer.Left;
  FBeginY := aLayer.Top;
  Assert(FDuration <> 0);
  FAdjustedDuration := FAdjustedDuration + 120;
end;

function TGRLayerAnimator_Line.DrawFrame(const aCurrDuration: Longword): Boolean;
var
  vP: TFloatPoint;
begin
  //Result := (FDuration=0) or (aCurrDuration > FAdjustedDuration);
  Result := ((FLayer.Left = FEndX) and (FLayer.Top = FEndY)) or (aCurrDuration > FAdjustedDuration);

  if not Result then
  begin
    FLayer.BeginUpdate;
    FLayer.Changing;
    try
      if FBeginX < FEndX then
      begin
        FLayer.Left := FBeginX + Trunc((aCurrDuration / FDuration) * (FEndX - FBeginX));
        if FLayer.Left > FEndX then FLayer.Left := FEndX;
      end
      else
      begin
        FLayer.Left := FBeginX - Trunc((aCurrDuration / FDuration) * (FBeginX - FEndX));
        if FLayer.Left < FEndX then FLayer.Left := FEndX;
      end;

      if FBeginY < FEndY then
      begin
        FLayer.Top := FBeginY + Trunc((aCurrDuration / FDuration) * (FEndY - FBeginY));
        if FLayer.Top > FEndY then FLayer.Top := FEndY;
      end
      else
      begin
        FLayer.Top := FBeginY - Trunc((aCurrDuration / FDuration) * (FBeginY - FEndY));
        if FLayer.Top < FEndY then FLayer.Top := FEndY;
      end;
    finally
      FLayer.EndUpdate;
    end;
    FLayer.Changed;
  end;
end;

{ TGRLayerAnimator_Sample }
constructor TGRLayerAnimator_Sample.Create(const aLayer: TGRLayer; const aMaxStep: Integer);
begin
  Inherited Create(aLayer);
  FMaxStep := aMaxStep;
end;

function TGRLayerAnimator_Sample.DrawFrame(const aCurrDuration: Longword): Boolean; 
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
  FLayers.Free;
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
end.
