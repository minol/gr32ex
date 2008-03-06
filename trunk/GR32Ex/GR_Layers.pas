
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

{ TGRLayer }

constructor TGRLayer.Create(
  ALayerCollection: TLayerCollection);
begin
  inherited;
  LayerOptions := LOB_MOUSE_EVENTS or LOB_VISIBLE; 
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
