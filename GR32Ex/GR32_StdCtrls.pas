unit GR32_StdCtrls;

{$I Setting.inc}
{.$undef XPTHEMES}
{.$define debug}
interface

uses
  {$ifdef Debug}
  DbugIntf,
  {$endif}
  Messages, {$IFDEF LINUX} WinUtils, {$ENDIF} Windows,
  SysUtils, Classes, Graphics, Controls, Forms, CommCtrl
  , GR32
  , GR32_Graphics
  , GR32_GraphUtils
  , GR32_Controls
  , GR32_FilterEx
  ;

type
  TTextLayout = (tlTop, tlCenter, tlBottom);
  TButtonLayout = (blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom);
  TButtonState = (bsUp, bsDisabled, bsDown, bsExclusive);
  TButtonStyle = (bsAutoDetect, bsWin31, bsNew);
  TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);
  TNumGlyphs = 1..4;

  TGRButtonActionLinkClass = class of TGRButtonActionLink;

  TGRButtonControl = class;
  TGRSpeedButton = class;
  TGRButtonActionLink = class(TWinControlActionLink)
  protected
    FClient: TGRButtonControl;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
  end;
  
  TGRButtonControl = class(TGRCustomControl)
  private
    FClicksDisabled: Boolean;
    FWordWrap: Boolean;
    procedure CNCtlColorStatic(var Message: TWMCtlColorStatic); message
      CN_CTLCOLORSTATIC;
    function IsCheckedStored: Boolean;
    procedure SetWordWrap(const Value: Boolean);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetChecked: Boolean; virtual;
    procedure SetChecked(Value: Boolean); virtual;
    procedure WndProc(var Message: TMessage); override;
    property Checked: Boolean read GetChecked write SetChecked stored
      IsCheckedStored default False;
    property ClicksDisabled: Boolean read FClicksDisabled write FClicksDisabled;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
  TGRCustomCheckBox = class(TGRButtonControl)
  private
    FAlignment: TLeftRight;
    FAllowGrayed: Boolean;
    FState: TCheckBoxState;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure SetAlignment(Value: TLeftRight);
    procedure SetState(Value: TCheckBoxState);
    procedure WMSize(var Message: TMessage); message WM_SIZE;
  protected
    procedure Click; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function GetChecked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure Toggle; virtual;
    property Alignment: TLeftRight read FAlignment write SetAlignment default
      taRightJustify;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default
      False;
    property State: TCheckBoxState read FState write SetState default
      cbUnchecked;
  public
    constructor Create(AOwner: TComponent); override;
    function GetControlsAlignment: TAlignment; override;
  published
    property TabStop default True;
  end;
  
  TGRCheckBox = class(TGRCustomCheckBox)
  published
    property Action;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Checked;
    property Color nodefault;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop;
    property Visible;
    property WordWrap;
  end;
  
  TGRRadioButton = class(TGRButtonControl)
  private
    FAlignment: TLeftRight;
    FChecked: Boolean;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure SetAlignment(Value: TLeftRight);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function GetChecked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetControlsAlignment: TAlignment; override;
  published
    property Action;
    property Alignment: TLeftRight read FAlignment write SetAlignment default
      taRightJustify;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Checked;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WordWrap;
  end;
  
  TGRCustomPanel = class(TGRBGCustomControl)
  private
    FAlignment: TAlignment;
    procedure SetAlignment(const Value: TAlignment);
  protected
    procedure BeforePaintBuffer(aBitmap32: TBitmap32); override;
    function InternalPaintBuffer(aBitmap32: TBitmap32): Boolean; override;
    property Alignment: TAlignment read FAlignment write SetAlignment default
      taCenter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
  TGRPanel = class(TGRCustomPanel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property Background;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property Caption;
    property CaptionFont;
    property Enabled;
    property MouseInControl;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property ParentFont;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;
  
  TGRCustomGroupBox = class(TGRBGCustomControl)
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CreateParams(var Params: TCreateParams); override;
    function InternalPaintBuffer(aBitmap32: TBitmap32): Boolean; overload;
      override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
  TGRGroupBox = class(TGRCustomGroupBox)
  published
    property Align;
    property Anchors;
    property Background;
    property BiDiMode;
    property Caption;
    property CaptionFont;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property ParentBackground default True;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;
  
  TGRCustomLabel = class(TGRGraphicControl)
  private
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FFocusControl: TWinControl;
    FLayout: TTextLayout;
    FShowAccelChar: Boolean;
    FWordWrap: Boolean;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetFocusControl(const Value: TWinControl);
    procedure SetLayout(const Value: TTextLayout);
    procedure SetShowAccelChar(const Value: Boolean);
    procedure SetWordWrap(const Value: Boolean);
  protected
    IsFontChanged: Boolean;
    procedure AdjustBounds; dynamic;
    { Summary only single line supports, to do for multi-line supports. }
    procedure DoDrawText(Dst: TBitmap32; var Rect: TRect; Flags: Longint);
      dynamic;
    procedure FontChanged(Sender: TObject); override;
    function GetLabelText: string; virtual;
    function InternalPaintBuffer(aBitmap32: TBitmap32): Boolean; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure PaintCaption(aBitmap32: TBitmap32);
    procedure SetAutoSize(Value: Boolean); override;
    property Alignment: TAlignment read FAlignment write SetAlignment default
      taCenter;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar
      default True;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
  end;
  
  TGRLabel = class(TGRCustomLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property CaptionFont;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Layout;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
  end;
  
  TGRSpeedButtonActionLink = class(TControlActionLink)
  protected
    FClient: TGRSpeedButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetGroupIndex(Value: Integer); override;
  end;
  
  TGRSpeedButton = class(TGRGraphicControl)
  private
    FAllowAllUp: Boolean;
    FDown: Boolean;
    FFlat: Boolean;
    FGroupIndex: Integer;
    FLayout: TButtonLayout;
    FMargin: Integer;
    FSpacing: Integer;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message: TMessage); message
      CM_ENABLEDCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message
      CM_SYSCOLORCHANGE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    function GetGlyph: TBitmap;
    function GetNumGlyphs: Integer;
    procedure SetAllowAllUp(const Value: Boolean);
    procedure SetDown(Value: Boolean);
    procedure SetFlat(const Value: Boolean);
    procedure SetGlyph(const Value: TBitmap);
    procedure SetGroupIndex(const Value: Integer);
    procedure SetLayout(const Value: TButtonLayout);
    procedure SetMargin(const Value: Integer);
    procedure SetNumGlyphs(Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message
      WM_LBUTTONDBLCLK;
  protected
    FDragging: Boolean;
    FGlyph: Pointer;
    FState: TButtonState;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure Click; override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetPalette: HPALETTE; override;
    procedure GlyphChanged(Sender: TObject);
    function InternalPaintBuffer(aBitmap32: TBitmap32): Boolean; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure UpdateExclusive;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Action;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp;
    property Anchors;
    property BiDiMode;
    property Caption;
    property CaptionFont;
    property Constraints;
    property Down: Boolean read FDown write SetDown;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
    property Layout: TButtonLayout read FLayout write SetLayout;
    property Margin: Integer read FMargin write SetMargin default -1;
    property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs default 1;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Visible;
  end;
  

implementation

uses
  Consts, RTLConsts, ActnList
  , ImgList
  {$ifdef XPTHEMES}
  , Themes
  {$endif}
  ;

  type
  TGlyphList = class(TImageList)
  private
    FCount: Integer;
    Used: TBits;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;
  
  TGlyphCache = class(TObject)
  private
    GlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function Empty: Boolean;
    function GetList(AWidth, AHeight: Integer): TGlyphList;
    procedure ReturnList(List: TGlyphList);
  end;
  
  TButtonGlyph = class(TObject)
  private
    FGlyphList: TGlyphList;
    FIndexs: array[TButtonState] of Integer;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    FOriginal: TBitmap;
    FTransparentColor: TColor;
    procedure CalcButtonLayout(bmp: TBitmap32; const Client: TRect; const
      Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
      Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect; BiDiFlags:
      Longint);
    function CreateButtonGlyph(State: TButtonState): Integer;
    procedure DrawButtonGlyph(bmp: TBitmap32; const GlyphPos: TPoint; State:
      TButtonState; Transparent: Boolean);
    procedure DrawButtonText(bmp: TBitmap32; const aFont: TFont32; const
      Caption: string; TextBounds: TRect; State: TButtonState; BiDiFlags:
      Longint);
    procedure GlyphChanged(Sender: TObject);
    procedure Invalidate;
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
  public
    constructor Create;
    destructor Destroy; override;
    function Draw(bmp: TBitmap32; const aFont: TFont32; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean;
      BiDiFlags: Longint): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  

constructor TGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  Used := TBits.Create;
end;

destructor TGlyphList.Destroy;
begin
  Used.Free;
  inherited Destroy;
end;

function TGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;

function TGlyphList.AllocateIndex: Integer;
begin
  Result := Used.OpenBit;
  if Result >= Used.Size then
  begin
    Result := inherited Add(nil, nil);
    Used.Size := Result + 1;
  end;
  Used[Result] := True;
end;

procedure TGlyphList.Delete(Index: Integer);
begin
  if Used[Index] then
  begin
    Dec(FCount);
    Used[Index] := False;
  end;
end;


constructor TGlyphCache.Create;
begin
  inherited Create;
  GlyphLists := TList.Create;
end;

destructor TGlyphCache.Destroy;
begin
  GlyphLists.Free;
  inherited Destroy;
end;

function TGlyphCache.Empty: Boolean;
begin
  Result := GlyphLists.Count = 0;
end;

function TGlyphCache.GetList(AWidth, AHeight: Integer): TGlyphList;
var
  I: Integer;
begin
  for I := GlyphLists.Count - 1 downto 0 do
  begin
    Result := GlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then Exit;
  end;
  Result := TGlyphList.CreateSize(AWidth, AHeight);
  GlyphLists.Add(Result);
end;

procedure TGlyphCache.ReturnList(List: TGlyphList);
begin
  if List = nil then Exit;
  if List.Count = 0 then
  begin
    GlyphLists.Remove(List);
    List.Free;
  end;
end;

var
  GlyphCache: TGlyphCache = nil;
  ButtonCount: Integer = 0;


constructor TButtonGlyph.Create;
var
  I: TButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clOlive;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
end;

destructor TButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;

procedure TButtonGlyph.CalcButtonLayout(bmp: TBitmap32; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect; BiDiFlags:
  Longint);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then Layout := blGlyphRight
    else
      if Layout = blGlyphRight then Layout := blGlyphLeft;
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);
  
  if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height) else
    GlyphSize := Point(0, 0);
  
  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(bmp.Handle, PChar(Caption), Length(Caption), TextBounds,
      DT_CALCRECT or BiDiFlags);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;
  
  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;
  
  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;
  
  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;
  
  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;
  
  { fixup the result variables }
  with GlyphPos do
  begin
    Inc(X, Client.Left + Offset.X);
    Inc(Y, Client.Top + Offset.Y);
  end;
  
  {$ifdef XPTHEMES}
  { Themed text is not shifted, but gets a different color. }
  if ThemeServices.ThemesEnabled then
    OffsetRect(TextBounds, TextPos.X + Client.Left, TextPos.Y + Client.Top)
  else
  {$endif}
    OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, TextPos.Y + Client.Top + Offset.Y);
end;

function TButtonGlyph.CreateButtonGlyph(State: TButtonState): Integer;
  
  const
    ROP_DSPDxax = $00E20746;
  var
    TmpImage, DDB, MonoBmp: TBitmap;
    IWidth, IHeight: Integer;
    IRect, ORect: TRect;
    I: TButtonState;
    DestDC: HDC;
  
begin
  if (State = bsDown) and (NumGlyphs < 3) then State := bsUp;
  Result := FIndexs[State];
  if Result <> -1 then Exit;
  if (FOriginal.Width or FOriginal.Height) = 0 then Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    TmpImage.Palette := CopyPalette(FOriginal.Palette);
    I := State;
    if Ord(I) >= NumGlyphs then I := bsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      bsUp, bsDown,
      bsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          if FOriginal.TransparentMode = tmFixed then
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, FTransparentColor)
          else
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
      bsDisabled:
        begin
          MonoBmp := nil;
          DDB := nil;
          try
            MonoBmp := TBitmap.Create;
            DDB := TBitmap.Create;
            DDB.Assign(FOriginal);
            DDB.HandleType := bmDDB;
            if NumGlyphs > 1 then
            with TmpImage.Canvas do
            begin    { Change white & gray to clBtnHighlight and clBtnShadow }
              CopyRect(IRect, DDB.Canvas, ORect);
              MonoBmp.Monochrome := True;
              MonoBmp.Width := IWidth;
              MonoBmp.Height := IHeight;
  
              { Convert white to clBtnHighlight }
              DDB.Canvas.Brush.Color := clWhite;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnHighlight;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
  
              { Convert gray to clBtnShadow }
              DDB.Canvas.Brush.Color := clGray;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnShadow;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
  
              { Convert transparent color to clBtnFace }
              DDB.Canvas.Brush.Color := ColorToRGB(FTransparentColor);
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnFace;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
            end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(FOriginal);
                HandleType := bmDDB;
                Canvas.Brush.Color := clBlack;
                Width := IWidth;
                if Monochrome then
                begin
                  Canvas.Font.Color := clWhite;
                  Monochrome := False;
                  Canvas.Brush.Color := clWhite;
                end;
                Monochrome := True;
              end;
              with TmpImage.Canvas do
              begin
                Brush.Color := clBtnFace;
                FillRect(IRect);
                Brush.Color := clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
                Brush.Color := clBtnShadow;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end;
            end;
          finally
            DDB.Free;
            MonoBmp.Free;
          end;
          FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;

function TButtonGlyph.Draw(bmp: TBitmap32; const aFont: TFont32; const Client:
  TRect; const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean;
  BiDiFlags: Longint): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(bmp, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphPos, Result, BiDiFlags);
  DrawButtonGlyph(bmp, GlyphPos, State, Transparent);
  DrawButtonText(bmp, aFont, Caption, Result, State, BiDiFlags);
end;

procedure TButtonGlyph.DrawButtonGlyph(bmp: TBitmap32; const GlyphPos: TPoint;
  State: TButtonState; Transparent: Boolean);
var
  Index: Integer;
  R: TRect;
  vBmp: TBitmap32;
  
  {$ifdef XPTHEMES}
  Details: TThemedElementDetails;
  Button: TThemedButton;
  {$endif}
  
begin
  if FOriginal = nil then Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
  Index := CreateButtonGlyph(State);
  vBmp := TBitmap32.Create;
  with GlyphPos do
  try
    vBmp.SetSize(FOriginal.Width, FOriginal.Height);
    {$ifdef XPTHEMES}
    if ThemeServices.ThemesEnabled then
    begin
      //R.TopLeft := GlyphPos;
      R.TopLeft := Point(0,0);
      R.Right := R.Left + FOriginal.Width div FNumGlyphs;
      R.Bottom := R.Top + FOriginal.Height;
      case State of
        bsDisabled:
          Button := tbPushButtonDisabled;
        bsDown,
        bsExclusive:
          Button := tbPushButtonPressed;
      else
        // bsUp
        Button := tbPushButtonNormal;
      end;
      Details := ThemeServices.GetElementDetails(Button);
      ThemeServices.DrawIcon(vBmp.Handle, Details, R, FGlyphList.Handle, Index);
    end
    else
    {$endif}
      if Transparent or (State = bsExclusive) then
      begin
        ImageList_DrawEx(FGlyphList.Handle, Index, vBmp.Handle, X, Y, 0, 0,
          clNone, clNone, ILD_Transparent)
      end
      else
        ImageList_DrawEx(FGlyphList.Handle, Index, vBmp.Handle, X, Y, 0, 0,
          ColorToRGB(clBtnFace), clNone, ILD_Normal);
      vBmp.ResetAlpha;
      if Transparent or (State = bsExclusive) then
      begin
        ApplyTransparentColor(vBmp, FTransparentColor);
        vBmp.DrawMode := dmBlend;
      end;
      vBmp.DrawTo(bmp, X, Y);
  finally
    vBmp.Free;
  end;
end;

procedure TButtonGlyph.DrawButtonText(bmp: TBitmap32; const aFont: TFont32;
  const Caption: string; TextBounds: TRect; State: TButtonState; BiDiFlags:
  Longint);
var
  bakEnabled: Boolean;
begin
  //with bmp.Canvas do
    //Brush.Style := bsClear;
  if State = bsDisabled then
  begin
    bakEnabled := aFont.Background.Enabled;
    aFont.Background.Enabled := False;
    try
    OffsetRect(TextBounds, 1, 1);
    aFont.Color := clBtnHighlight;
    aFont.DrawText(bmp, Caption, TextBounds, DT_CENTER or DT_VCENTER or BiDiFlags);
      //DrawText(bmp.Handle, PChar(Caption), Length(Caption), TextBounds,
        //DT_CENTER or DT_VCENTER or BiDiFlags);
    OffsetRect(TextBounds, -1, -1);
    aFont.Color := clBtnShadow;
    aFont.DrawText(bmp, Caption, TextBounds, DT_CENTER or DT_VCENTER or BiDiFlags);
      //DrawText(bmp.Handle, PChar(Caption), Length(Caption), TextBounds,
        //DT_CENTER or DT_VCENTER or BiDiFlags);
    finally
      aFont.Background.Enabled := bakEnabled;
    end;
  end else
  begin
      //bmp.Font.Color := clBlack;
      //bmp.UpdateFont;
      //bmp.UpdateFont;
    aFont.DrawText(bmp, Caption, TextBounds, DT_CENTER or DT_VCENTER or BiDiFlags);
      //DrawText(bmp.Handle, PChar(Caption), Length(Caption), TextBounds,
        //DT_CENTER or DT_VCENTER or BiDiFlags);
      //bmp.ResetAlpha;
  end;
end;

procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
  begin
    FTransparentColor := FOriginal.TransparentColor;
    Invalidate;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TButtonGlyph.Invalidate;
var
  I: TButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if FIndexs[I] <> -1 then FGlyphList.Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(FGlyphList);
  FGlyphList := nil;
end;

procedure TButtonGlyph.SetGlyph(Value: TBitmap);
var
  Glyphs: Integer;
begin
  Invalidate;
  FOriginal.Assign(Value);
  if (Value <> nil) and (Value.Height > 0) then
  begin
    FTransparentColor := Value.TransparentColor;
    if Value.Width mod Value.Height = 0 then
    begin
      Glyphs := Value.Width div Value.Height;
      if Glyphs > 4 then Glyphs := 1;
      SetNumGlyphs(Glyphs);
    end;
  end;
end;

procedure TButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
    GlyphChanged(Glyph);
  end;
end;


constructor TGRCustomPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FBackground := TBackground.Create(Self);
  //FBackground.OnChanged := DoBackgroundChanged;
  FAlignment := taCenter;
  BevelOuter := bvRaised;
  BevelInner := bvNone;
  BevelWidth := 1;
  //BorderStyle := bsNone;
end;

destructor TGRCustomPanel.Destroy;
begin
  //FreeAndNil(FBackground);
  inherited Destroy;
end;

procedure TGRCustomPanel.BeforePaintBuffer(aBitmap32: TBitmap32);
var
  TopColor, BottomColor: TColor;
  Rect: TRect;
  
  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    //Contrast := 100;
    //if Bevel = bvLowered then Contrast := -100;
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;
  
begin
  inherited BeforePaintBuffer(aBitmap32);
  {Rect := aBitmap32.BoundsRect;
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Rect:= DrawBevel(aBitmap32, Rect, TopColor, BottomColor, BevelWidth);
    //aBitmap32.RaiseRectTS(Rect, Contrast);
    //OffsetRect(Rect, -BevelWidth, -BevelWidth);
  end;
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Rect:= DrawBevel(aBitmap32, Rect, TopColor, BottomColor, BevelWidth);
  end;
  //if (BevelOuter <> bvNone) or (BevelInner <> bvNone) then
    //aBitmap32.ResetAlpha;
  
  //Paint the Background.
  aBitmap32.ClipRect := Rect;
  }
end;

function TGRCustomPanel.InternalPaintBuffer(aBitmap32: TBitmap32): Boolean;
var
  vTextSize: TSize;
  Rect: TRect;
begin
  if Trim(Caption) <> '' then
  begin
    Rect := aBitmap32.ClipRect;
  {$ifdef Unicode_Supports}
    vTextSize := FCaptionFont.TextExtentW(aBitmap32, Caption);
  {$else}
    vTextSize := FCaptionFont.TextExtent(aBitmap32, Caption);
  {$endif}
    case Alignment of
      taLeftJustify:
      begin
        vTextSize.cx := Rect.Left;
        vTextSize.cy := Rect.Top;
      end;
      taCenter:
      begin
        //align the text to the panel center.
        vTextSize.cx := Rect.Left + (Rect.Right-Rect.Left) div 2 - vTextSize.cx div 2;
        if vTextSize.cx < Rect.Left then vTextSize.cx := Rect.Left;
        vTextSize.cy := Rect.Top + (Rect.Bottom-Rect.Top) div 2 - vTextSize.cy div 2;
        if vTextSize.cy < Rect.Top then vTextSize.cy := Rect.Top;
      end;
      taRightJustify:
      begin
        vTextSize.cx := Rect.Right  - vTextSize.cx;
        if vTextSize.cx < Rect.Left then vTextSize.cx := Rect.Left;
        vTextSize.cy := Rect.Top - vTextSize.cy;
        if vTextSize.cy < Rect.Top then vTextSize.cy := Rect.Top;
      end;
    end; //case
  {$ifdef Unicode_Supports}
    FCaptionFont.RenderTextW(aBitmap32, vTextSize.cx, vTextSize.cy, Caption);
  {$else}
    FCaptionFont.RenderText(aBitmap32, vTextSize.cx, vTextSize.cy, Caption);
  {$endif}
  end;
  Result := inherited InternalPaintBuffer(aBitmap32);
end;

procedure TGRCustomPanel.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    if not (csLoading in ComponentState) then
    begin
      FSelfBuffer.Delete;
      InvalidateBuffer;
    end;
  end;
end;

constructor TGRCustomGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks, csReplicatable, csParentBackground];
  Width := 185;
  Height := 105;
end;

procedure TGRCustomGroupBox.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  //Inc(Rect.Top, CaptionFont.TextExtent(FBuffer, '0').cy);
    Canvas.Font := Font;
    Inc(Rect.Top, Canvas.TextHeight('0'));
  InflateRect(Rect, -1, -1);
  if Ctl3d then InflateRect(Rect, -1, -1);
end;

procedure TGRCustomGroupBox.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
  Realign;
end;

procedure TGRCustomGroupBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      SelectFirst;
      Result := 1;
    end else
      inherited;
end;

procedure TGRCustomGroupBox.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  Realign;
end;

procedure TGRCustomGroupBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

function TGRCustomGroupBox.InternalPaintBuffer(aBitmap32: TBitmap32): Boolean;
var
  H: Integer;
  R: TRect;
  Flags: LongInt;
  CaptionRect, OuterRect: TRect;
  Size: TSize;
  vColor: TColor32;
  
  {$ifdef XPTHEMES}
  Box: TThemedButton;
  Details: TThemedElementDetails;
  {$endif}
  
begin
  //with aBitmap32 do
  begin
    //Font := Self.CaptionFont;
  
    {$ifdef XPTHEMES}
    if ThemeServices.ThemesEnabled then
    begin
      if Text <> '' then
      begin
        GetTextExtentPoint32(aBitmap32.Handle, PChar(Text), Length(Text), Size);
        CaptionRect := Rect(0, 0, Size.cx, Size.cy);
        if not UseRightToLeftAlignment then
          OffsetRect(CaptionRect, 8, 0)
        else
          OffsetRect(CaptionRect, Width - 8 - CaptionRect.Right, 0);
      end
      else
        CaptionRect := Rect(0, 0, 0, 0);
  
      OuterRect := ClientRect;
      OuterRect.Top := (CaptionRect.Bottom - CaptionRect.Top) div 2;
      with CaptionRect do
        ExcludeClipRect(aBitmap32.Handle, Left, Top, Right, Bottom);
      if Enabled then
        Box := tbGroupBoxNormal
      else
        Box := tbGroupBoxDisabled;
      Details := ThemeServices.GetElementDetails(Box);
      ThemeServices.DrawElement(aBitmap32.Handle, Details, OuterRect);
  
      SelectClipRgn(aBitmap32.Handle, 0);
      if Text <> '' then
        ThemeServices.DrawText(aBitmap32.Handle, Details, Text, CaptionRect, DT_LEFT, 0);
      //aBitmap32.ResetAlpha;
      //if Transparent or AlphaBlend then
        //ApplyTransparentColor(aBitmap32, 0);
    end
    else
    {$endif}
    begin
      H := aBitmap32.TextHeight('0');
      R := Rect(0, H div 2 - 1, Width, Height);
      if Ctl3D then
      begin
        Inc(R.Left);
        Inc(R.Top);
        aBitmap32.FrameRectS(R, Color32(clBtnHighlight));
        OffsetRect(R, -1, -1);
        vColor := Color32(clBtnShadow);
      end else
        vColor := Color32(clWindowFrame);
      aBitmap32.FrameRectS(R, vColor);
      if Text <> '' then
      begin
        if not UseRightToLeftAlignment then
          R := Rect(8, 0, 0, H)
        else
          R := Rect(R.Right - Canvas.TextWidth(Text) - 8, 0, 0, H);
        Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
        CaptionFont.DrawText(aBitmap32, Text, R, Flags or DT_CALCRECT);
        CaptionFont.DrawText(aBitmap32, Text, R, Flags);
      end;
    end;
  end;
  Result := inherited InternalPaintBuffer(aBitmap32);
end;

constructor TGRCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width := 65;
  Height := 17;
  FAutoSize := True;
  FShowAccelChar := True;
  {$ifdef XPTHEMES}
  { The "default" value for the Transparent property depends on
    if you have Themes available and enabled or not. If you have
    ever explicitly set it, that will override the default value. }
  if ThemeServices.ThemesEnabled then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  {$endif}
  
  //没有明显的性能提升!
  //FCaptionBuffer := TBitmap32.Create;
  //FCaptionBuffer.DrawMode := dmBlend;
end;

destructor TGRCustomLabel.Destroy;
begin
  //FCaptionBuffer.Free;
  inherited Destroy;
end;

procedure TGRCustomLabel.AdjustBounds;
  
  const
    WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
  var
    DC: HDC;
    X: Integer;
    Rect: TRect;
    AAlignment: TAlignment;
  
begin
  if IsFontChanged and not (csReading in ComponentState) and FAutoSize then
  begin
    IsFontChanged := False;
    Rect := ClientRect;
    DoDrawText(FBuffer, Rect, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[FWordWrap]);
    X := Left;
    AAlignment := FAlignment;
    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
    if AAlignment = taRightJustify then Inc(X, Width - Rect.Right);
  {$ifdef debug}
  //SendDebug(Format('W=%x', [Rect.Right]));
  {$endif}
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

procedure TGRCustomLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and
    IsAccel(Message.CharCode, Caption) then
    with FFocusControl do
      if CanFocus then
      begin
        SetFocus;
        Message.Result := 1;
      end;
end;

procedure TGRCustomLabel.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  AdjustBounds;
end;

procedure TGRCustomLabel.DoDrawText(Dst: TBitmap32; var Rect: TRect; Flags:
  Longint);
var
  Text: string;
  bakColor: TColor;
  bakEnabled: Boolean;
begin
  Text := GetLabelText;
  Dst.Font := CaptionFont;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or FShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  if not FShowAccelChar then Flags := Flags or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);
  if not Enabled then
  begin
    bakEnabled := CaptionFont.Background.Enabled;
    CaptionFont.Background.Enabled := False;
    OffsetRect(Rect, 1, 1);
    //bakColor := CaptionFont.Color;
    try
      Dst.Font.Color := clBtnHighlight;
      CaptionFont.DrawText(Dst, Text, Rect, Flags);
      OffsetRect(Rect, -1, -1);
      Dst.Font.Color := clBtnShadow;
      CaptionFont.DrawText(Dst, Text, Rect, Flags);
    finally
      //CaptionFont.Color := bakColor;
      CaptionFont.Background.Enabled := bakEnabled;
    end;
  end
  else
    CaptionFont.DrawText(Dst, Text, Rect, Flags);
end;

procedure TGRCustomLabel.FontChanged(Sender: TObject);
begin
  //AdjustBounds;
  IsFontChanged := True;
  //FCaptionBuffer.SetSize(0,0);
  inherited FontChanged(Sender);
end;

function TGRCustomLabel.GetLabelText: string;
begin
  Result := Caption;
end;

function TGRCustomLabel.InternalPaintBuffer(aBitmap32: TBitmap32): Boolean;
begin
  {Rect := aBitmap32.ClipRect;
  if FCaptionBuffer.Empty
    or (OldCaption <> Caption)
    or (FCaptionBuffer.width <> Rect.Right-Rect.Left)
    or (FCaptionBuffer.Height <> Rect.Bottom-Rect.Top) then
  begin
    FCaptionBuffer.SetSize(Rect.Right-Rect.Left, Rect.Bottom-Rect.Top);
    //FCaptionBuffer.ClipRect := aBitmap32.ClipRect;
    PaintCaption(FCaptionBuffer);
  end;
  FCaptionBuffer.DrawTo(aBitmap32, Rect.Left, Rect.Top);
  //}
  PaintCaption(aBitmap32);
  Result := inherited InternalPaintBuffer(aBitmap32);
end;

procedure TGRCustomLabel.Invalidate;
begin
  if (FUpdateCount <= 0) then
  begin
    AdjustBounds;
    inherited Invalidate;
  end;
end;

procedure TGRCustomLabel.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TGRCustomLabel.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TGRCustomLabel.PaintCaption(aBitmap32: TBitmap32);
  
  const
    Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
    WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
  var
    Rect, CalcRect: TRect;
    DrawStyle: Longint;
  
begin
  Rect := aBitmap32.ClipRect;
  { DoDrawText takes care of BiDi alignments }
  DrawStyle := DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];
  { Calculate vertical layout }
  if FLayout <> tlTop then
  begin
    CalcRect := Rect;
    DoDrawText(aBitmap32, CalcRect, DrawStyle or DT_CALCRECT);
    if FLayout = tlBottom then OffsetRect(Rect, 0, Height - CalcRect.Bottom)
    else OffsetRect(Rect, 0, (Height - CalcRect.Bottom) div 2);
  end;
  {$ifdef debug}
  //SendDebug(Format('Font=%x', [Integer(CaptionFont.Outline)]));
  {$endif}
  DoDrawText(aBitmap32, Rect, DrawStyle);
end;

procedure TGRCustomLabel.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    if not (csLoading in ComponentState) then
    begin
      IsFontChanged := True;
      InvalidateSelfBuffer;
    end;
  end;
end;

procedure TGRCustomLabel.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if not (csLoading in ComponentState) then
    begin
      IsFontChanged := True;
      InvalidateSelfBuffer;
    end;
  end;
end;

procedure TGRCustomLabel.SetFocusControl(const Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TGRCustomLabel.SetLayout(const Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    if not (csLoading in ComponentState) then
    begin
      IsFontChanged := True;
      InvalidateSelfBuffer;
    end;
  end;
end;

procedure TGRCustomLabel.SetShowAccelChar(const Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    if not (csLoading in ComponentState) then
    begin
      IsFontChanged := True;
      InvalidateSelfBuffer;
    end;
  end;
end;

procedure TGRCustomLabel.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    if not (csLoading in ComponentState) then
    begin
      IsFontChanged := True;
      InvalidateSelfBuffer;
    end;
  end;
end;

procedure TGRSpeedButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TGRSpeedButton;
end;

function TGRSpeedButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and (FClient.GroupIndex <> 0) and
    FClient.AllowAllUp and (FClient.Down = (Action as TCustomAction).Checked);
end;

function TGRSpeedButtonActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := (FClient is TGRSpeedButton) and
    (FClient.GroupIndex = (Action as TCustomAction).GroupIndex);
end;

procedure TGRSpeedButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then FClient.Down := Value;
end;

procedure TGRSpeedButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then FClient.GroupIndex := Value;
end;

constructor TGRSpeedButton.Create(AOwner: TComponent);
begin
  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;
  inherited Create(AOwner);
  SetBounds(0, 0, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;
  Color := clBtnFace;
  FSpacing := 4;
  FMargin := -1;
  FLayout := blGlyphLeft;
  //FTransparent := True;
  FState := bsUp;
  Inc(ButtonCount);
end;

destructor TGRSpeedButton.Destroy;
begin
  Dec(ButtonCount);
  inherited Destroy;
  TButtonGlyph(FGlyph).Free;
end;

procedure TGRSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
end;

procedure TGRSpeedButton.Click;
begin
  inherited Click;
end;

procedure TGRSpeedButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TGRSpeedButton;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TGRSpeedButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        if (Action is TCustomAction) then
          TCustomAction(Action).Checked := False;
        InvalidateBuffer;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

procedure TGRSpeedButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled and Visible and
      (Parent <> nil) and Parent.Showing then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TGRSpeedButton.CMEnabledChanged(var Message: TMessage);
  
  const
    NewState: array[Boolean] of TButtonState = (bsDisabled, bsUp);
  
begin
  TButtonGlyph(FGlyph).CreateButtonGlyph(NewState[Enabled]);
  UpdateTracking;
  FBufferDirty := True;
  Repaint;
end;

procedure TGRSpeedButton.CMSysColorChange(var Message: TMessage);
begin
  with TButtonGlyph(FGlyph) do
  begin
    InvalidateBuffer;
    CreateButtonGlyph(FState);
  end;
end;

procedure TGRSpeedButton.CMTextChanged(var Message: TMessage);
begin
  InvalidateBuffer;
end;

function TGRSpeedButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TGRSpeedButtonActionLink;
end;

function TGRSpeedButton.GetGlyph: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;

function TGRSpeedButton.GetNumGlyphs: Integer;
begin
  Result := TButtonGlyph(FGlyph).NumGlyphs;
end;

function TGRSpeedButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

procedure TGRSpeedButton.GlyphChanged(Sender: TObject);
begin
  InvalidateBuffer;
end;

function TGRSpeedButton.InternalPaintBuffer(aBitmap32: TBitmap32): Boolean;
  
  const
    DownStyles: array[Boolean] of TFrameStyle = (fsButtonUp, fsButtonDown);
    FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);
  var
    FrameStyle: TFrameStyle;
    PaintRect: TRect;
    DrawFlags: Integer;
    Offset: TPoint;
    {$ifdef XPTHEMES}
    Button: TThemedButton;
    ToolButton: TThemedToolBar;
    Details: TThemedElementDetails;
    {$endif}
  
begin
  //if not Transparent then aBitmap32.FillRectS(PaintRect, Color32(Color));
  if not Enabled then
  begin
    FState := bsDisabled;
    FDragging := False;
  end
  else if FState = bsDisabled then
    if FDown and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;
    {$ifdef Debug}
    sendInteger('BtnState', Integer(FState));
    sendInteger('MouseInCtrl', Integer(FMouseInControl));
    {$endif}
  {$ifdef XPTHEMES}
  if ThemeServices.ThemesEnabled then
  begin
    PerformEraseBackground(Self, aBitmap32.Handle);
  
    if not Enabled then
      Button := tbPushButtonDisabled
    else
      if FState in [bsDown, bsExclusive] then
        Button := tbPushButtonPressed
      else
        if MouseInControl then
          Button := tbPushButtonHot
        else
          Button := tbPushButtonNormal;
  
    ToolButton := ttbToolbarDontCare;
    if FFlat then
    begin
      case Button of
        tbPushButtonDisabled:
          Toolbutton := ttbButtonDisabled;
        tbPushButtonPressed:
          Toolbutton := ttbButtonPressed;
        tbPushButtonHot:
          Toolbutton := ttbButtonHot;
        tbPushButtonNormal:
          Toolbutton := ttbButtonNormal;
      end;
    end;
  
    PaintRect := ClientRect;
    if ToolButton = ttbToolbarDontCare then
    begin
      Details := ThemeServices.GetElementDetails(Button);
      ThemeServices.DrawElement(aBitmap32.Handle, Details, PaintRect);
      PaintRect := ThemeServices.ContentRect(aBitmap32.Handle, Details, PaintRect);
    end
    else
    begin
      Details := ThemeServices.GetElementDetails(ToolButton);
      ThemeServices.DrawElement(aBitmap32.Handle, Details, PaintRect);
      PaintRect := ThemeServices.ContentRect(aBitmap32.Handle, Details, PaintRect);
    end;
  
    if Button = tbPushButtonPressed then
    begin
      // A pressed speed button has a white text. This applies however only to flat buttons.
      if ToolButton <> ttbToolbarDontCare then
        aBitmap32.Font.Color := clHighlightText;
      Offset := Point(1, 0);
    end
    else
      Offset := Point(0, 0);
    TButtonGlyph(FGlyph).Draw(aBitmap32, CaptionFont, PaintRect, Offset, Caption, FLayout, FMargin,
      FSpacing, FState, Transparent, DrawTextBiDiModeFlags(0));
  end
  else
  {$endif}
  begin
    PaintRect := Rect(0, 0, Width, Height);
    if not FFlat then
    begin
      //DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
      if FState in [bsDown, bsExclusive] then
      begin
        FrameStyle := fsLowered;
      end
      else begin
        FrameStyle := fsButtonUp;
      end;
      DrawBorder(aBitmap32, PaintRect, FrameStyle);
    end
    else  //flat
    begin
      if (FState in [bsDown, bsExclusive]) or
        (FMouseInControl and (FState <> bsDisabled)) or
        (csDesigning in ComponentState)
      then
      begin
        DrawBorder(aBitmap32, PaintRect, DownStyles[FState in [bsDown, bsExclusive]]);
      end;
      //else
        //DrawBorder(aBitmap32, PaintRect, fsFlat);
      InflateRect(PaintRect, -1, -1);
    end;
    if FState in [bsDown, bsExclusive] then
    begin
      if (FState = bsExclusive) and (not FFlat or not FMouseInControl) then
      begin
        //Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
        //Canvas.FillRect(PaintRect);
        //aBitmap32.FillRectS(PaintRect, clTrWhite32);
      end;
      Offset.X := 1;
      Offset.Y := 1;
    end
    else
    begin
      Offset.X := 0;
      Offset.Y := 0;
    end;
    TButtonGlyph(FGlyph).Draw(aBitmap32, CaptionFont, PaintRect, Offset, Caption, FLayout, FMargin,
      FSpacing, FState, Transparent, DrawTextBiDiModeFlags(0));
  end;
  //if Transparent then ApplyTransparentColor(aBitmap32, aBitmap32.Pixel[0,0]);
  Result := inherited InternalPaintBuffer(aBitmap32);
end;

procedure TGRSpeedButton.Loaded;
var
  State: TButtonState;
begin
  inherited Loaded;
  if Enabled then
    State := bsUp
  else
    State := bsDisabled;
  TButtonGlyph(FGlyph).CreateButtonGlyph(State);
end;

procedure TGRSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    if not FDown then
    begin
      FState := bsDown;
      InvalidateSelfBuffer;
    end;
    FDragging := True;
  end;
end;

procedure TGRSpeedButton.MouseEnter;
var
  NeedRepaint: Boolean;
begin
  inherited MouseEnter;
  { Don't draw a border if DragMode <> dmAutomatic since this button is meant to
    be used as a dock client. }
  NeedRepaint := FFlat and FMouseInControl and Enabled and (DragMode <> dmAutomatic) and (GetCapture = 0);
  
  { Windows XP introduced hot states also for non-flat buttons. }
  if (NeedRepaint
    {$ifdef XPTHEMES}or ThemeServices.ThemesEnabled{$endif}
    ) and not (csDesigning in ComponentState) then
  begin
    if Enabled then
    begin
      InvalidateSelfBuffer;
      Repaint;
    end;
  end;
end;

procedure TGRSpeedButton.MouseLeave;
var
  NeedRepaint: Boolean;
begin
  inherited MouseLeave;
  NeedRepaint := FFlat and not FMouseInControl and Enabled and not FDragging;
  { Windows XP introduced hot states also for non-flat buttons. }
  if NeedRepaint
    {$ifdef XPTHEMES}or ThemeServices.ThemesEnabled{$endif}
  then
  begin
    if Enabled then
    begin
      InvalidateSelfBuffer;
      Repaint;
    end;
  end;
end;

procedure TGRSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if not FDown then NewState := bsUp
    else NewState := bsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then NewState := bsExclusive else NewState := bsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      InvalidateSelfBuffer;
    end;
  end
end;

procedure TGRSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then
    begin
      { Redraw face in-case mouse is captured }
      FState := bsUp;
      //FMouseInControl := False;
      if DoClick and not (FState in [bsExclusive, bsDown]) then
        InvalidateSelfBuffer;
    end
    else
      if DoClick then
      begin
        SetDown(not FDown);
        if FDown then Repaint;
      end
      else
      begin
        if FDown then FState := bsExclusive;
        FBufferDirty := True;
        FSelfBufferDirty := True;
        Repaint;
      end;
    if DoClick then Click;
  end;
end;

procedure TGRSpeedButton.SetAllowAllUp(const Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TGRSpeedButton.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then
    begin
      if FState = bsUp then Invalidate;
      FState := bsExclusive
    end
    else
    begin
      FState := bsUp;
      FBufferDirty := True;
      FSelfBufferDirty := True;
      Repaint;
    end;
    if Value then UpdateExclusive;
  end;
end;

procedure TGRSpeedButton.SetFlat(const Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    InvalidateSelfBuffer;
  end;
end;

procedure TGRSpeedButton.SetGlyph(const Value: TBitmap);
begin
  TButtonGlyph(FGlyph).Glyph := Value;
  InvalidateSelfBuffer;
end;

procedure TGRSpeedButton.SetGroupIndex(const Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TGRSpeedButton.SetLayout(const Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    InvalidateSelfBuffer;
  end;
end;

procedure TGRSpeedButton.SetMargin(const Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    InvalidateSelfBuffer;
  end;
end;

procedure TGRSpeedButton.SetNumGlyphs(Value: Integer);
begin
  if Value < 0 then Value := 1
  else if Value > 4 then Value := 4;
  if Value <> TButtonGlyph(FGlyph).NumGlyphs then
  begin
    TButtonGlyph(FGlyph).NumGlyphs := Value;
    InvalidateSelfBuffer;
  end;
end;

procedure TGRSpeedButton.SetSpacing(const Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    InvalidateBuffer;
  end;
end;

procedure TGRSpeedButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TGRSpeedButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
end;

procedure TGRButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TGRButtonControl;
end;

function TGRButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    (FClient.Checked = (Action as TCustomAction).Checked);
end;

procedure TGRButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
  begin
    FClient.ClicksDisabled := True;
    try
      FClient.Checked := Value;
    finally
      FClient.ClicksDisabled := False;
    end;
  end;
end;

constructor TGRButtonControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if SysLocale.FarEast and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    ImeMode := imDisable;
end;

procedure TGRButtonControl.ActionChange(Sender: TObject; CheckDefaults:
  Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Checked = False) then
        Self.Checked := Checked;
    end;
end;

procedure TGRButtonControl.CNCtlColorStatic(var Message: TWMCtlColorStatic);
begin
  {$ifdef XPTHEMES}
  with ThemeServices do
    if ThemesEnabled then
    begin
      if Parent.DoubleBuffered then
        PerformEraseBackground(Self, Message.ChildDC)
      else
        DrawParentBackground(Handle, Message.ChildDC, nil, False);
      { Return an empty brush to prevent Windows from overpainting we just have created. }
      Message.Result := GetStockObject(NULL_BRUSH);
    end
    else
  {$endif}
      inherited;
end;

procedure TGRButtonControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if FWordWrap then
    Params.Style := Params.Style or BS_MULTILINE;
end;

function TGRButtonControl.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TGRButtonActionLink;
end;

function TGRButtonControl.GetChecked: Boolean;
begin
  Result := False;
end;

function TGRButtonControl.IsCheckedStored: Boolean;
begin
  Result := (ActionLink = nil) or not TGRButtonActionLink(ActionLink).IsCheckedLinked;
end;

procedure TGRButtonControl.SetChecked(Value: Boolean);
begin
end;

procedure TGRButtonControl.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    RecreateWnd;
  end;
end;

procedure TGRButtonControl.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      if not (csDesigning in ComponentState) and not Focused then
      begin
        FClicksDisabled := True;
        Windows.SetFocus(Handle);
        FClicksDisabled := False;
        if not Focused then Exit;
      end;
    CN_COMMAND:
      if FClicksDisabled then Exit;
  end;
  inherited WndProc(Message);
end;

constructor TGRCustomCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 97;
  Height := 17;
  TabStop := True;
  ControlStyle := [csSetCaption, csDoubleClicks];
  FAlignment := taRightJustify;
end;

procedure TGRCustomCheckBox.Click;
begin
  inherited Changed;
  inherited Click;
end;

procedure TGRCustomCheckBox.CMCtl3DChanged(var Message: TMessage);
begin
  RecreateWnd;
end;

procedure TGRCustomCheckBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      SetFocus;
      if Focused then Toggle;
      Result := 1;
    end else
      inherited;
end;

procedure TGRCustomCheckBox.CNCommand(var Message: TWMCommand);
begin
  if Message.NotifyCode = BN_CLICKED then Toggle;
end;

procedure TGRCustomCheckBox.CreateParams(var Params: TCreateParams);
  
  const
    Alignments: array[Boolean, TLeftRight] of DWORD =
      ((BS_LEFTTEXT, 0), (0, BS_LEFTTEXT));
  
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'BUTTON');
  with Params do
  begin
    Style := Style or BS_3STATE or
      Alignments[UseRightToLeftAlignment, FAlignment];
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TGRCustomCheckBox.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, BM_SETCHECK, Integer(FState), 0);
end;

function TGRCustomCheckBox.GetChecked: Boolean;
begin
  Result := State = cbChecked;
end;

function TGRCustomCheckBox.GetControlsAlignment: TAlignment;
begin
  if not UseRightToLeftAlignment then
    Result := taRightJustify
  else
    if FAlignment = taRightJustify then
      Result := taLeftJustify
    else
      Result := taRightJustify;
end;

procedure TGRCustomCheckBox.SetAlignment(Value: TLeftRight);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TGRCustomCheckBox.SetChecked(Value: Boolean);
begin
  if Value then State := cbChecked else State := cbUnchecked;
end;

procedure TGRCustomCheckBox.SetState(Value: TCheckBoxState);
begin
  if FState <> Value then
  begin
    FState := Value;
    if HandleAllocated then
      SendMessage(Handle, BM_SETCHECK, Integer(FState), 0);
    if not ClicksDisabled then Click;
  end;
end;

procedure TGRCustomCheckBox.Toggle;
begin
  case State of
    cbUnchecked:
      if AllowGrayed then State := cbGrayed else State := cbChecked;
    cbChecked: State := cbUnchecked;
    cbGrayed: State := cbChecked;
  end;
end;

procedure TGRCustomCheckBox.WMSize(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

{ TGRRadioButton }

constructor TGRRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 113;
  Height := 17;
  ControlStyle := [csSetCaption, csDoubleClicks];
  FAlignment := taRightJustify;
end;

procedure TGRRadioButton.CMCtl3DChanged(var Message: TMessage);
begin
  RecreateWnd;
end;

procedure TGRRadioButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(Message.CharCode, Caption) and CanFocus then
    begin
      SetFocus;
      Result := 1;
    end else
      inherited;
end;

procedure TGRRadioButton.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    BN_CLICKED: SetChecked(True);
    BN_DOUBLECLICKED: DblClick;
  end;
end;

procedure TGRRadioButton.CreateParams(var Params: TCreateParams);
  
  const
    Alignments: array[Boolean, TLeftRight] of DWORD =
      ((BS_LEFTTEXT, 0), (0, BS_LEFTTEXT));
  
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'BUTTON');
  with Params do
    Style := Style or BS_RADIOBUTTON or
      Alignments[UseRightToLeftAlignment, FAlignment];
end;

procedure TGRRadioButton.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, BM_SETCHECK, Integer(FChecked), 0);
end;

function TGRRadioButton.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TGRRadioButton.GetControlsAlignment: TAlignment;
begin
  if not UseRightToLeftAlignment then
    Result := taRightJustify
  else
    if FAlignment = taRightJustify then
      Result := taLeftJustify
    else
      Result := taRightJustify;
end;

procedure TGRRadioButton.SetAlignment(Value: TLeftRight);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TGRRadioButton.SetChecked(Value: Boolean);
  
  procedure TurnSiblingsOff;
  var
    I: Integer;
    Sibling: TControl;
  begin
    if Parent <> nil then
      with Parent do
        for I := 0 to ControlCount - 1 do
        begin
          Sibling := Controls[I];
          if (Sibling <> Self) and (Sibling is TGRRadioButton) then
            with TGRRadioButton(Sibling) do
            begin
              if Assigned(Action) and
                 (Action is TCustomAction) and
                 TCustomAction(Action).AutoCheck then
                TCustomAction(Action).Checked := False;
              SetChecked(False);
            end;
        end;
  end;
  
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    TabStop := Value;
    if HandleAllocated then
      SendMessage(Handle, BM_SETCHECK, Integer(Checked), 0);
    if Value then
    begin
      TurnSiblingsOff;
      inherited Changed;
      if not ClicksDisabled then Click;
    end;
  end;
end;

end.
