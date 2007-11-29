{ Summary The abstract Control Layer, like TControl }
{ Description
It work with TImage32
}
unit GR_ControlLayers;

interface

{$I Setting.inc}

uses
  {$ifdef Debug}
  DbugIntf,
  {$endif} 
  Windows, Messages, 
  Forms, //TCMHintShow
  SysUtils, Classes, 
  Graphics, Controls 
  , GR32
  , GR32_Image
  , GR32_Layers
  , GR32_LowLevel, GR32_Transforms
  , GR_BitmapEx
  , GR_Graphics
  , GR_GraphUtils
  //, GR_FilterEx
  ;

const
  AnchorAlign: array[TAlign] of TAnchors = (
    { alNone }
    [akLeft, akTop],
    { alTop }
    [akLeft, akTop, akRight],
    { alBottom }
    [akLeft, akRight, akBottom],
    { alLeft }
    [akLeft, akTop, akBottom],
    { alRight }
    [akRight, akTop, akBottom],
    { alClient }
    [akLeft, akTop, akRight, akBottom],
    { alCustom }
    [akLeft, akTop]
    );

type
  TMouseButtons = set of TMouseButton;
  TImage32Access = class(TCustomImage32);
  TLayerStyleController = class;
  TBGCustomControlLayer = class;
  TControlLayerContainer = class;
  { Summary The abstract control layer. }
  TControlLayer = class(TPositionedLayer)
  private
    FAlign: TAlign;
    FAlphaBlend: Boolean;
    FAlphaBlendValue: Byte;
    FAnchorMove: Boolean;
    FAnchorRules: TPoint;
    FColor: TColor;
    FControlState: TControlState;
    FCropped: Boolean;
    FFrame: TGRFrame;
    FFrameHot: TGRFrame;
    FHint: string;
    FMouseButtons: TMouseButtons;
    FMouseXPos: Integer;
    FMouseYPos: Integer;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOriginalParentSize: TPoint;
    FShiftState: TShiftState;
    FStyleController: TLayerStyleController;
    FTransparent: Boolean;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetMousePosition: TPoint;
    function GetParent: TLayerCollection;
    function GetTop: Integer;
    function GetWidth: Integer;
    procedure SetAlign(const Value: TAlign);
    procedure SetCaptionFont(const Value: TFont32);
    procedure SetColor(const Value: TColor);
    procedure SetCropped(const Value: Boolean);
    procedure SetFrame(const Value: TGRFrame);
    procedure SetFrameHot(const Value: TGRFrame);
    procedure SetHeight(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetParent(Value: TLayerCollection);
    procedure SetStyleController(const Value: TLayerStyleController);
    procedure SetTop(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    { Summary 保存的是背景和元件自身的界面！ }
    { Description
    不包括它的子元件的界面。
    
    See Also FBufferDirty, DoubleBuffered
    }
    FBuffer: TBitmap32;
    { Summary 表示需要重新绘制缓冲区 }
    { Description
    See Also FBuffer
    }
    FBufferDirty: Boolean;
    FCaptionFont: TFont32;
    FSelfBuffer: TBitmap32;
    { Summary 表示需要重新绘制缓冲区 }
    { Description
    See Also FBuffer
    }
    FSelfBufferDirty: Boolean;
    FUpdateCount: Integer;
    { Description
    See Also: BeforePaintBuffer, InternalPaintBuffer, PaintBuffer
    }
    procedure AfterPaintBuffer(aBitmap32: TBitmap32); virtual;
    { Summary Assign the CaptionFont to the Buffer. }
    { Description
    See Also InternalPaintBuffer, AfterPaintBuffer, PaintBuffer
    }
    procedure BeforePaintBuffer(aBitmap32: TBitmap32); virtual;
    function CheckNewSize(var NewWidth, NewHeight: Integer): Boolean;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure DoFrameChanged(Sender: TObject); dynamic;
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
    procedure DoSetLocation(const NewLocation: TFloatRect); override;
    procedure FontChanged(Sender: TObject); virtual;
    function GetClientRect: TRect; virtual;
    { Summary the derived class should override this to paint itself. }
    { Description
    the result specify the paint is ok or not. the default is ok(true).
    When u paint it failed, u should set result as false!
    
    See Also: BeforePaintBuffer, AfterPaintBuffer, PaintBuffer
    }
    function InternalPaintBuffer(aBitmap32: TBitmap32): Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure Notification(ALayer: TCustomLayer); override;
    procedure Paint(Buffer: TBitmap32); override;
    procedure RequestAlign; dynamic;
    procedure SetAlphaBlend(const Value: Boolean); virtual;
    procedure SetAlphaBlendValue(const Value: Byte); virtual;
    procedure SetTransparent(const Value: Boolean);
    procedure UpdateAnchorRules;
    property CaptionFont: TFont32 read FCaptionFont write SetCaptionFont;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetBorderHeight: Integer;
    function GetBorderWidth: Integer;
    function GetBottomBorderSize: Integer;
    function GetLeftBorderSize: Integer;
    { Summary return the current paint Frame. }
    function GetPaintFrame: TGRFrame;
    function GetRightBorderSize: Integer;
    function GetTopBorderSize: Integer;
    procedure Invalidate;
    procedure InvalidateBuffer;
    procedure PaintBuffer;
    { Summary Paint itself to DC }
    { Description
    srcRect: witch to paint. 表明需要绘制该元件的那一部分的区域。
    
    NOTE: this method do not paint its sub-controls to DC!
    }
    procedure PaintTo(DC: HDC); overload;
    { Summary Paint itself to DC }
    { Description
    srcRect: witch to paint. 表明需要绘制该元件的那一部分的区域。
    
    NOTE: this method do not paint its sub-controls to DC!
    }
    procedure PaintTo(DC: HDC; srcRect: TRect); overload;
    { Description
    srcRect: witch to paint. 表明需要绘制该元件的那一部分的区域。
    }
    procedure PaintTo(Dst: TBitmap32); overload;
    { Description
    srcRect: witch to paint. 表明需要绘制该元件的那一部分的区域。
    }
    procedure PaintTo(Dst: TBitmap32; srcRect: TRect); overload;
    { Summary Paint the Component Foreground itself(No parent background, or
      sons). }
    { Description
    只绘制前景（不包括它的父背景和它下属的子元件）
    }
    procedure PaintToBuffer(aBuffer: TBitmap32);
    { Summary //TODO }
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;
    property Align: TAlign read FAlign write SetAlign;
    property Color: TColor read FColor write SetColor;
    property ControlState: TControlState read FControlState write FControlState;
    property Cropped: Boolean read FCropped write SetCropped;
    property Frame: TGRFrame read FFrame write SetFrame;
    property FrameHot: TGRFrame read FFrameHot write SetFrameHot;
    property Height: Integer read GetHeight write SetHeight;
    property Hint: string read FHint write FHint;
    property IsBufferDirty: Boolean read FBufferDirty;
    property Left: Integer read GetLeft write SetLeft;
    { Summary the current mouse state }
    property MouseButtons: TMouseButtons read FMouseButtons;
    property MousePosition: TPoint read GetMousePosition;
    property MouseXPos: Integer read FMouseXPos;
    property MouseYPos: Integer read FMouseYPos;
    { Description
    The Owner is Parent, so do not allow to modifiy.
    }
    property Parent: TLayerCollection read GetParent write SetParent;
    { Summary the current shift state }
    property ShiftState: TShiftState read FShiftState;
    property StyleController: TLayerStyleController read FStyleController write
      SetStyleController;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
  published
    property AlphaBlend: Boolean read FAlphaBlend write SetAlphaBlend;
    { Summary Specifies the degree of translucency on a translucent control. }
    { Description
    Set AlphaBlendValue to a value between 0 and 255 to indicate the degree of 
    translucency when the AlphaBlend property is true. A value of 0 indicates 
    a completely transparent control. A value of 255 indicates complete opacity.
    
    Note:	AlphaBlendValue only has an effect when the AlphaBlend property is
    true.
    }
    property AlphaBlendValue: Byte read FAlphaBlendValue write
      SetAlphaBlendValue default 255;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property Transparent: Boolean read FTransparent write SetTransparent;
  end;
  
  TLayerStyleController = class(TCustomLayer)
  private
    FHotStyle: TGRStyle;
    { Summary Collect the ControlLayer }
    FList: TList;
    FNormalStyle: TGRStyle;
    procedure SetHotStyle(const Value: TGRStyle);
    procedure SetNormalStyle(const Value: TGRStyle);
  protected
    procedure DoNormalStyleChanged(Sender: TObject);
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    procedure FreeNotification(ALayer: TCustomLayer);
    procedure RemoveFreeNotification(ALayer: TCustomLayer);
    property HotStyle: TGRStyle read FHotStyle write SetHotStyle;
    property NormalStyle: TGRStyle read FNormalStyle write SetNormalStyle;
  end;
  
  TBGCustomControlLayer = class(TControlLayer)
  private
    FBackground: TGRBackground;
    procedure SetBackground(const Value: TGRBackground);
  protected
    procedure BeforePaintBuffer(aBitmap32: TBitmap32); override;
    procedure DoBackgroundChanged(Sender: TObject);
    { Summary The control's Background. }
    property Background: TGRBackground read FBackground write SetBackground;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
  end;
  
  { Description
  暂缓实现TLayerContainer
  }
  TControlLayerContainer = class(TBGCustomControlLayer)
  protected
    procedure AlignControl(aControl: TControlLayer);
    procedure AlignControls(AControl: TControlLayer; var Rect: TRect); virtual;
  public
    procedure InsertControl(AControl: TControlLayer);
    procedure RemoveControl(AControl: TControlLayer);
  end;
  

implementation

uses 
  Consts;
constructor TControlLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited Create(ALayerCollection);
  FBuffer := TBitmap32Ex.Create;
  //FSelfBuffer := TBitmap32Ex.Create;
  FBuffer.DrawMode := dmBlend;
  //FSelfBuffer.DrawMode := dmBlend;
  FBufferDirty := True;
  //FSelfBufferDirty := True;
  FAlphaBlendValue := 255;
  FCaptionFont := TFont32.Create;
  FCaptionFont.OnChange := FontChanged;
  
  FFrame := TGRFrame.Create(Self);
  FFrame.OnChanged := DoFrameChanged;
  
  FFrameHot := TGRFrame.Create(Self);
end;

destructor TControlLayer.Destroy;
begin
  //FreeAndNil(FSelfBuffer);
  FreeAndNil(FBuffer);
  FreeAndNil(FCaptionFont);
  FreeAndNil(FFrame);
  FreeAndNil(FFrameHot);
  inherited Destroy;
end;

procedure TControlLayer.AfterPaintBuffer(aBitmap32: TBitmap32);
begin
  aBitmap32.ResetClipRect;
  GetPaintFrame.PaintTo(aBitmap32, aBitmap32.ClipRect);
end;

procedure TControlLayer.BeforePaintBuffer(aBitmap32: TBitmap32);
begin
  with GetPaintFrame do
    if Enabled then
      aBitmap32.ClipRect := CalcClientRect(aBitmap32.BoundsRect);
end;

procedure TControlLayer.BeginUpdate;
begin
  if FUpdateCount = 0 then Changing;
  Inc(FUpdateCount);
end;

function TControlLayer.CheckNewSize(var NewWidth, NewHeight: Integer): Boolean;
begin
end;

procedure TControlLayer.CMHintShow(var Message: TCMHintShow);
begin
  //if Message.HintInfo^.CursorPos.X,Message.HintInfo^.CursorPos.Y
  Message.HintInfo^.HintStr := FHint;
  inherited;
end;

procedure TControlLayer.DoFrameChanged(Sender: TObject);
begin
  //if not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
  begin
    InvalidateBuffer;
  end;
end;

procedure TControlLayer.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TControlLayer.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TControlLayer.DoSetLocation(const NewLocation: TFloatRect);
begin
  inherited DoSetLocation(NewLocation);
  //
    UpdateAnchorRules;
    Invalidate;
    //Perform(WM_WINDOWPOSCHANGED, 0, 0);
    //RequestAlign;
    //if not (csLoading in ComponentState) then
    //Resize;
end;

procedure TControlLayer.EndUpdate;
begin
  Dec(FUpdateCount);
  InvalidateBuffer;
end;

procedure TControlLayer.FontChanged(Sender: TObject);
begin
  //if not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
  begin
    InvalidateBuffer;
    //InvalidateBuffer;
  end;
end;

function TControlLayer.GetBorderHeight: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Left + LFrame.ImageFrame.Width.Right
    else
      Result := LFrame.Width * 2;
  end
  else
    Result := 0;
end;

function TControlLayer.GetBorderWidth: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Bottom + LFrame.ImageFrame.Width.Top
    else
      Result := LFrame.Width * 2;
  end
  else
    Result := 0;
end;

function TControlLayer.GetBottomBorderSize: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Bottom
    else
      Result := LFrame.Width;
  end
  else
    Result := 0;
end;

function TControlLayer.GetClientRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

function TControlLayer.GetHeight: Integer;
begin
  Result := Round(Location.Bottom - Location.Top);
end;

function TControlLayer.GetLeft: Integer;
begin
  Result := Round(Location.Left);
end;

function TControlLayer.GetLeftBorderSize: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Left
    else
      Result := LFrame.Width;
  end
  else
    Result := 0;
end;

function TControlLayer.GetMousePosition: TPoint;
begin
  Result := Point(FMouseXPos, FMouseYPos);
end;

function TControlLayer.GetPaintFrame: TGRFrame;
begin
  if Assigned(StyleController) then
  begin
    if FMouseInControl and StyleController.HotStyle.Frame.Enabled then
      Result := StyleController.HotStyle.Frame
    else
      Result := StyleController.NormalStyle.Frame;
  end
  else begin
    if FMouseInControl then
      Result := FrameHot
    else
      Result := Frame;
  end;
end;

function TControlLayer.GetParent: TLayerCollection;
begin
  //Result := FLayerCollection;
end;

function TControlLayer.GetRightBorderSize: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Right
    else
      Result := LFrame.Width;
  end
  else
    Result := 0;
end;

function TControlLayer.GetTop: Integer;
begin
  Result := Round(Location.Top);
end;

function TControlLayer.GetTopBorderSize: Integer;
var
  LFrame: TGRFrame;
begin
  LFrame := GetPaintFrame;
  if Assigned(LFrame) and LFrame.Enabled then
  begin
    if LFrame.FrameStyle = fsImage then
      Result := LFrame.ImageFrame.Width.Top
    else
      Result := LFrame.Width;
  end
  else
    Result := 0;
end;

function TControlLayer.GetWidth: Integer;
begin
  Result := Round(Location.Right - Location.Left);
end;

function TControlLayer.InternalPaintBuffer(aBitmap32: TBitmap32): Boolean;
begin
  Result := True;
  //FBufferDirty := False;
end;

procedure TControlLayer.Invalidate;
begin
  if FUpdateCount <= 0 then
  begin
    Changed;
  end;
end;

procedure TControlLayer.InvalidateBuffer;
begin
  FBufferDirty := True;
  Invalidate;
end;

procedure TControlLayer.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  FShiftState := Shift;
end;

procedure TControlLayer.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
end;

procedure TControlLayer.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  FShiftState := Shift;
end;

procedure TControlLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FShiftState := Shift;
  Include(FMouseButtons, Button);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TControlLayer.MouseEnter;
begin
  inherited MouseEnter;
    if GetPaintFrame.Enabled then InvalidateBuffer;
    DoMouseEnter;
end;

procedure TControlLayer.MouseLeave;
begin
  inherited MouseLeave;
    if GetPaintFrame.Enabled then InvalidateBuffer;
    DoMouseLeave;
end;

procedure TControlLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  FMouseXPos := X;
  FMouseYPos := Y;
  
  //FShiftState := Shift;
  //if not FMouseInControl then
    //UpdateTracking;
end;

procedure TControlLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
begin
  FShiftState := Shift;
  Exclude(FMouseButtons, Button);
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TControlLayer.Notification(ALayer: TCustomLayer);
begin
  if (aLayer = FStyleController) then
  begin
    FStyleController := nil;
    DoFrameChanged(Self);
  end;
  inherited Notification(ALayer);
end;

procedure TControlLayer.Paint(Buffer: TBitmap32);
var
  SrcRect, DstRect, ClipRect: TRect;
  ImageRect: TRect;
  LayerWidth, LayerHeight: Single;
begin
  if FBufferDirty then
  begin
    PaintToBuffer(FBuffer);
  end;
  
  {if AlphaBlend then
  begin
    FBuffer.MasterAlpha := AlphaBlendValue;
  end
  else
    FBuffer.MasterAlpha := 255;
  //}
  
  DstRect := MakeRect(GetAdjustedLocation);
  SrcRect := MakeRect(0, 0, FBuffer.Width, FBuffer.Height);
  ClipRect := Buffer.ClipRect;
  
  if Cropped and (LayerCollection.Owner is TCustomImage32) and
    not (TImage32Access(LayerCollection.Owner).PaintToMode) then
  begin
    with DstRect do
    begin
      LayerWidth := Right - Left;
      LayerHeight := Bottom - Top;
    end;
    if (LayerWidth < 0.5) or (LayerHeight < 0.5) then Exit;
    ImageRect := TCustomImage32(LayerCollection.Owner).GetBitmapRect;
    IntersectRect(ClipRect, ClipRect, ImageRect);
  end;
  StretchTransfer(Buffer, DstRect, ClipRect, FBuffer, SrcRect,
    FBuffer.StretchFilter, FBuffer.DrawMode, FBuffer.OnPixelCombine);
  
  //FBuffer.DrawTo(Buffer, DstRect);
end;

procedure TControlLayer.PaintBuffer;
begin
  PaintToBuffer(FBuffer);
end;

procedure TControlLayer.PaintTo(DC: HDC);
var
  srcRect: TRect;
  I: Integer;
begin
  i := GetClipBox(DC, srcRect);
  if (i = RGN_ERROR) or (i = RGN_ERROR) then exit;
  PaintTo(DC, srcRect);
end;

procedure TControlLayer.PaintTo(DC: HDC; srcRect: TRect);
begin
  if IsBufferDirty then
    //PaintBuffer(not FTransparent or FAlphaBlend);
    PaintBuffer;
  
  {if Transparent and not FAlphaBlend then
    StretchToDCTransparentFunc(DC, SrcRect.Left, SrcRect.Top, FBuffer.Width, FBuffer.Height,
    FBuffer, 0,0, FBuffer.Width, FBuffer.Height)
  else //}
    FBuffer.DrawTo(DC, SrcRect.Left, SrcRect.Top);
end;

procedure TControlLayer.PaintTo(Dst: TBitmap32);
begin
  Dst.SetSize(Width, Height);
  PaintTo(Dst, Dst.BoundsRect);
end;

procedure TControlLayer.PaintTo(Dst: TBitmap32; srcRect: TRect);
var
  I: Integer;
  aControl: TControl;
  aBMP: TBitmap32;
  aRect: TRect;
begin
  if IsBufferDirty then PaintBuffer({True});
  FBuffer.DrawTo(Dst, 0,0, srcRect);
end;

procedure TControlLayer.PaintToBuffer(aBuffer: TBitmap32);
begin
  aBuffer.SetSize(Width, Height);
  if Transparent {or AlphaBlend} then
  begin
    aBuffer.Clear(0);
  end
  else begin
    aBuffer.Clear(Color32(Color));
  end;
  aBuffer.Font := FCaptionFont;
  BeforePaintBuffer(aBuffer);
  FBufferDirty := not InternalPaintBuffer(aBuffer);
  AfterPaintBuffer(aBuffer);
end;

procedure TControlLayer.RequestAlign;
begin
  if Parent <> nil then
  begin
    //Parent.AlignControl(Self);
  end;
end;

procedure TControlLayer.SetAlign(const Value: TAlign);
begin
  if FAlign <> Value then
  begin
    FAlign := Value;
  end;
end;

procedure TControlLayer.SetAlphaBlend(const Value: Boolean);
begin
  if FAlphaBlend <> Value then
  begin
    FAlphaBlend := Value;
    if Value {or Transparet} then
      FBuffer.DrawMode := dmBlend
    else
      FBuffer.DrawMode := dmOpaque;
    InvalidateBuffer;
  end;
end;

procedure TControlLayer.SetAlphaBlendValue(const Value: Byte);
begin
  if FAlphaBlendValue <> Value then
  begin
    FAlphaBlendValue := Value;
    FBuffer.MasterAlpha := Value;
    if AlphaBlend then
      InvalidateBuffer;
  end;
end;

procedure TControlLayer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  {if CheckNewSize(AWidth, AHeight) and
    ((ALeft <> FLeft) or (ATop <> FTop) or
    (AWidth <> FWidth) or (AHeight <> FHeight)) then
  begin
    FLocation.Left := aLeft;
    FLocation.Right :=  aLeft + aWidth;
    FLocation.Top := aTop;
    FLocation.Bottom :=  aTop + aHeight;
  
    UpdateAnchorRules;
    Invalidate;
    //Perform(WM_WINDOWPOSCHANGED, 0, 0);
    RequestAlign;
    //if not (csLoading in ComponentState) then
    Resize;
  end;
  //}
end;

procedure TControlLayer.SetCaptionFont(const Value: TFont32);
begin
  if FCaptionFont <> Value then
    FCaptionFont.Assign(Value);
end;

procedure TControlLayer.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
  //
  if not Transparent then
    InvalidateBuffer;
  end;
end;

procedure TControlLayer.SetCropped(const Value: Boolean);
begin
  if FCropped <> Value then
  begin
    FCropped := Value;
    Changed;
  end;
end;

procedure TControlLayer.SetFrame(const Value: TGRFrame);
begin
  if FFrame <> Value then
    FFrame.Assign(Value);
end;

procedure TControlLayer.SetFrameHot(const Value: TGRFrame);
begin
  if FFrameHot <> Value then
    FFrameHot.Assign(Value);
end;

procedure TControlLayer.SetHeight(const Value: Integer);
begin
  if Value >= 0 then
  begin
    BeginUpdate;
    try
      FLocation.Bottom := Location.Top + Value;
      InvalidateBuffer;
    finally
    EndUpdate;
    end;
  end;
end;

procedure TControlLayer.SetLeft(const Value: Integer);
var
  L: Double;
begin
  BeginUpdate;
  try
    L := FLocation.Right - FLocation.Left;
    FLocation.Left := Value;
    FLocation.Right := FLocation.Left + L;
  finally
    Changed;
  end;
end;

procedure TControlLayer.SetParent(Value: TLayerCollection);
begin
  //SetLayerCollection(Value);
end;

procedure TControlLayer.SetStyleController(const Value: TLayerStyleController);
begin
  if FStyleController <> Value then
  begin
    if Assigned(FStyleController) then
      FStyleController.RemoveFreeNotification(Self);
  FStyleController := Value;
    if Assigned(FStyleController) then
      FStyleController.FreeNotification(Self);
  end;
end;

procedure TControlLayer.SetTop(const Value: Integer);
var
  L: Double;
begin
  BeginUpdate;
  try
    L := FLocation.Bottom - FLocation.Top;
    FLocation.Top := Value;
    FLocation.Bottom := FLocation.Top + L;
  finally
    EndUpdate;
  end;
end;

procedure TControlLayer.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    InvalidateBuffer;
  end;
end;

procedure TControlLayer.SetWidth(const Value: Integer);
begin
  if Value >= 0 then
  begin
    BeginUpdate;
    try
      FLocation.Right :=  Location.Left + Value;
      InvalidateBuffer;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TControlLayer.UpdateAnchorRules;
var
  Anchors: TAnchors;
begin
  (*
  if not FAnchorMove {and not (csLoading in ComponentState)} then
  begin
    Anchors := FAnchors;
    if Anchors = [akLeft, akTop] then
    begin
      FOriginalParentSize.X := 0;
      FOriginalParentSize.Y := 0;
      Exit;
    end;
    if akRight in Anchors then
      if akLeft in Anchors then
        FAnchorRules.X := Width else
        FAnchorRules.X := Left
    else
      FAnchorRules.X := Left + Width div 2;
    if akBottom in Anchors then
      if akTop in Anchors then
        FAnchorRules.Y := Height else
        FAnchorRules.Y := Top
    else
      FAnchorRules.Y := Top + Height div 2;
    if Parent is TLayerCollection and (TLayerCollection(Parent).Owner is TCustomImage32) then
    with TCustomImage32(TLayerCollection(Parent).Owner) do
    begin
      if HandleAllocated then
        FOriginalParentSize := ClientRect.BottomRight
      else
      begin
        FOriginalParentSize.X := Width;
        FOriginalParentSize.Y := Height;
      end;
    end else if Parent is TControlLayerContainer then
    begin
      FOriginalParentSize.X := TControlLayerContainer(Parent).Width;
      FOriginalParentSize.Y := TControlLayerContainer(Parent).Height;
    end;
  end;
  *)
end;

constructor TLayerStyleController.Create(ALayerCollection: TLayerCollection);
begin
  inherited Create(ALayerCollection);
  FList := TList.Create;
  FNormalStyle := TGRStyle.Create(Self);
  FNormalStyle.OnChanged := DoNormalStyleChanged;
  
  FHotStyle := TGRStyle.Create(Self);
end;

destructor TLayerStyleController.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FNormalStyle);
  FreeAndNil(FHotStyle);
  inherited Destroy;
end;

procedure TLayerStyleController.DoNormalStyleChanged(Sender: TObject);
var
  I: Integer;
  LControl: TControlLayer;
begin
  //if not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
  begin
    //Notify all controls here
    if Assigned(FList) then
      for i := 0 to FList.Count - 1 do
      begin
        LControl := TControlLayer(FList[i]);
        //if csDestroying in LControl.ComponentState then continue;
        LControl.InvalidateBuffer
      end;
  end;
end;

procedure TLayerStyleController.FreeNotification(ALayer: TCustomLayer);
begin
  //if (Owner = nil) or (ALayer.Owner <> Owner) then
  begin
    // Never acquire a reference to a component that is being deleted.
    //assert(not (csDestroying in (ComponentState + AComponent.ComponentState)));
  
    if (ALayer is TControlLayer) then
    begin
      if FList.IndexOf(ALayer) < 0 then
        FList.Add(ALayer);
    end;
  end;
  inherited FreeNotification(ALayer);
end;

procedure TLayerStyleController.RemoveFreeNotification(ALayer: TCustomLayer);
begin
  if FList <> nil then
  begin
    FList.Remove(ALayer);
    {if FList.Count = 0 then
    begin
      FList.Free;
      FList := nil;
    end; //}
  end;
  inherited RemoveFreeNotification(ALayer);
end;

procedure TLayerStyleController.SetHotStyle(const Value: TGRStyle);
begin
  if FHotStyle <> Value then
    FHotStyle.Assign(Value);
end;

procedure TLayerStyleController.SetNormalStyle(const Value: TGRStyle);
begin
  if FNormalStyle <> Value then
    FNormalStyle.Assign(Value);
end;

constructor TBGCustomControlLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited Create(ALayerCollection);
  FBackground := TGRBackground.Create(Self);
  FBackground.OnChanged := DoBackgroundChanged;
end;

destructor TBGCustomControlLayer.Destroy;
begin
  FreeAndNil(FBackground);
  inherited Destroy;
end;

procedure TBGCustomControlLayer.BeforePaintBuffer(aBitmap32: TBitmap32);
begin
  //Paint the Background.
  aBitmap32.ResetClipRect;
  if FBackground.Enabled then
    FBackground.PaintTo(aBitmap32, aBitmap32.ClipRect)
    //FBackground.PaintTo(aBitmap32, aBitmap32.BoundsRect)
  else
    aBitmap32.Clear(Color32(Color));
  inherited BeforePaintBuffer(aBitmap32);
end;

procedure TBGCustomControlLayer.DoBackgroundChanged(Sender: TObject);
begin
  //if not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
  begin
    InvalidateBuffer;
  end;
end;

procedure TBGCustomControlLayer.SetBackground(const Value: TGRBackground);
begin
  if FBackground <> Value then
    FBackground.Assign(Value);
end;

procedure TControlLayerContainer.AlignControl(aControl: TControlLayer);
begin
end;

procedure TControlLayerContainer.AlignControls(AControl: TControlLayer; var
  Rect: TRect);
begin
end;

procedure TControlLayerContainer.InsertControl(AControl: TControlLayer);
begin
end;

procedure TControlLayerContainer.RemoveControl(AControl: TControlLayer);
begin
end;


end.
