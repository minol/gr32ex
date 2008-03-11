
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
 *   Based on the newsgroup post (March 18, 2002,  news://news.g32.org/g32org.public.graphics32)
 *   <public@lischke-online.de ; <news:a755io$6t1$1@webserver9.elitedev.com>...
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


    {$IFDEF Designtime_Supports}
    procedure SetGridLayer(const Value: TGRGridLayer);
    class function RubberbandOptions: TGRRubberBandOptions; override;
    procedure Paint(Buffer: TBitmap32); override;
    {$ENDIF}

  public
    class procedure GetRegisteredEvents(const aStrs: TStrings); virtual;
    class procedure GetRegisteredBehaviors(const aStrs: TStrings); virtual;

    {$IFDEF Designtime_Supports}
    class function RubberbandOptions: TGRRubberBandOptions; virtual;
    {$ENDIF}
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

    FOnClick: TNotifyEvent;
    FOnRightClick: TNotifyEvent;
    FOnDoubleClick: TNotifyEvent;
    FOnRightDoubleClick: TNotifyEvent;

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
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;


    procedure DefineProperties(Filer: TFiler); override;
  public
    class procedure GetRegisteredEvents(const aStrs: TStrings); override;
    class procedure GetRegisteredBehaviors(const aStrs: TStrings); override;
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

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnRightClick: TNotifyEvent read FOnRightClick write FOnRightClick;
    property OnDoubleClick: TNotifyEvent read FOnDoubleClick write FOnDoubleClick;
    property OnRightDoubleClick: TNotifyEvent read FOnRightDoubleClick write FOnRightDoubleClick;
  end;

  TGRLayer = class(TGRBitmapLayer)
  protected
    FWidth: Integer;
    FHeight: Integer;

    function GetLeft: Integer;
    function GetTop: Integer;
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);


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
    //property Cropped;
    //property DrawMode;
    property Angle;
    property Skew;
    property PivotPoint;
    property Scaled;
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


procedure RegisterLayer(const aLayerClass: TGRLayerClass);
function GetLayerClass(const aClassName: string): TGRLayerClass;
function GLayerClasses: TThreadList;
//----------------------------------------------------------------------------------------------------------------------
function ComponentToStr(const Component: TComponent): string;
procedure SaveStrToFile(const aFileName, s: string);
procedure ComponentToTextFile(const Component: TComponent; const aFileName: string);

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
begin
end;

class procedure TGRCustomLayer.GetRegisteredBehaviors(const aStrs: TStrings);
begin
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

    DoChange;
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
class procedure TGRTransformationLayer.GetRegisteredEvents(const aStrs: TStrings);
begin
  with aStrs do
  begin
    Clear;
    Add('OnClick');
    Add('OnDoubleClick');
    Add('OnRightClick');
    Add('OnRightDoubleClick');
  end;
end;

class procedure TGRTransformationLayer.GetRegisteredBehaviors(const aStrs: TStrings);
begin
  with aStrs do
  begin
    Clear;
    //AddObject('XxxBehavior', @TGRTransformationLayer.XxxBehavior);
  end;
end;

class function TGRTransformationLayer.RubberbandOptions: TGRRubberBandOptions;
begin
  Result := [rboAllowCornerResize,
    rboAllowEdgeResize,
    rboAllowMove,
    rboShowFrame,
    rboShowHandles
  ];
end;

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

      if Assigned(FOnClick) then
      begin
        vM := TMethod(FOnClick);
        if vM.Data = Source then
          vM.Data := Self;
        Self.FOnClick := TNotifyEvent(vM);
      end
      else
        Self.OnClick := OnClick;

      if Assigned(FOnDoubleClick) then
      begin
        vM := TMethod(FOnDoubleClick);
        if vM.Data = Source then
          vM.Data := Self;
        Self.FOnDoubleClick := TNotifyEvent(vM);
      end
      else
        Self.OnDoubleClick := OnDoubleClick;

      if Assigned(FOnRightClick) then
      begin
        vM := TMethod(FOnRightClick);
        if vM.Data = Source then
          vM.Data := Self;
        Self.FOnRightClick := TNotifyEvent(vM);
      end
      else
        Self.OnRightClick := OnRightClick;

      if Assigned(FOnRightClick) then
      begin
        vM := TMethod(FOnRightDoubleClick);
        if vM.Data = Source then
          vM.Data := Self;
        Self.FOnRightDoubleClick := TNotifyEvent(vM);
      end
      else
        Self.OnRightDoubleClick := OnRightDoubleClick;

      Changed; // Layer collection.
      DoChange; // Layer only.
    end;
  inherited Assign(Source);
end;

procedure TGRTransformationLayer.DefineProperties(Filer: TFiler);
  function DoSkewWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TSDPlayingLayer)
    else
      Result := (Skew.X <> 0) or (Skew.Y <> 0);
  end;
  function DoPivotWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TSDPlayingLayer)
    else
      Result := (PivotPoint.X <> 0) or (PivotPoint.Y <> 0);
  end;
  function DoScalingWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TSDPlayingLayer)
    else
      Result := (Scaling.X <> 0) or (Scaling.Y <> 0);
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

procedure TGRTransformationLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
          if Assigned(FOnDoubleClick) then FOnDoubleClick(Self);
        end
        else
          if Assigned(FOnClick) then FOnClick(Self);
      end;
    mbRight:
      begin
        if ssDouble in Shift then
        begin
          if Assigned(FOnRightDoubleClick) then FOnRightDoubleClick(Self);
        end
        else
          if Assigned(FOnRightClick) then FOnRightClick(Self);
      end;
    mbMiddle:
      begin
      end;
  end; //case
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

  DoChange;
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

  DoChange; // Layer only.
end;

procedure TGRTransformationLayer.SetPivot(const Value: TFloatPoint);
begin
  Changing;
  FPivotPoint := Value;
  Changed;

  DoChange;
end;

procedure TGRTransformationLayer.SetScaling(const Value: TFloatPoint);
begin
  Changing;
  FScaling := Value;
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

{ TGRLayer }
constructor TGRLayer.Create(aLayerCollection: TLayerCollection);
begin
  inherited;
  //LayerOptions := LOB_MOUSE_EVENTS or LOB_VISIBLE; 
end;

function TGRLayer.GetLeft: Integer;
begin
  Result := Trunc(FPosition.X);
end;

function TGRLayer.GetTop: Integer;
begin
  Result := Trunc(FPosition.Y);
end;

procedure TGRLayer.SetHeight(const Value: Integer);
begin
  if FSize.cy <> value then
  begin
    Changing;
    FSize.cy := Value;
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
  if FSize.cx <> value then
  begin
    Changing;
    FSize.cx := Value;
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
end.
