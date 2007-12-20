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
 * The Original Code is GR_Forms
 *
 * The Initial Developer of the Original Code is Riceball LEE
 * Portions created by Riceball LEE are Copyright (C) 2004-2007
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_Forms;

interface

{$I Setting.inc}

{$Define debug}
uses
  {$ifdef Debug}
  DbugIntf,
  {$endif} 
  Windows, Messages, 
  SysUtils, Classes, 
  Graphics, Controls, Forms
  //, uDeskUtils
  , GR32
  , GR32_Blend
  , GR32_Image
  //,GR_ImageEx
  {$IFDEF MSWINDOWS}
  , GR_WinAPI
  {$ENDIF}
  , GR_Graphics
  , GR_GraphUtils
  , GR_Controls
  , GR_StdCtrls
  ;

resourcestring
  rsCreateLayeredWinError = 'Can''t set Layered Window';

type
  TGRFormDragMode = (fdmBorder, fdmFullDrag, fdmSystemSettings);

  TGRWindowHitTest = (
    kwhtClient,
    kwhtCaption,
    kwhtLeft,
    kwhtTop,
    kwhtRight,
    kwhtBottom,
    kwhtBorder,
    kwhtTopLeft,
    kwhtTopRight,
    kwhtBottomLeft,
    kwhtBottomRight,
    kwhtCloseButton,
    kwhtHelpButton,
    kwhtMinButton,
    kwhtMaxButton,
    kwhtRollButton,
    kwhtTrayButton,
    kwhtSysMenu,
    kwhtNonClient,
    kwhtMenu
  );

type
  TGRDragging = class(TPersistent)
  private
    FDragMode: TGRFormDragMode;
    FMoveable: Boolean;
    FSizeable: Boolean;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property DragMode: TGRFormDragMode read FDragMode write FDragMode default
      fdmFullDrag;
    property Moveable: Boolean read FMoveable write FMoveable default true;
    property Sizeable: Boolean read FSizeable write FSizeable default true;
  end;
  
  { Summary the abstract GRForm }
  { Description
  Shaped Form Supports build-in.
  鼠标穿透
  嵌入桌面
  }
  TGRCustomForm = class(TForm)
  private
    FGRDragging: TGRDragging;
    FIgnoreMouse: Boolean;
    FInDesktop: Boolean;
    procedure SetGRDragging(const Value: TGRDragging);
    procedure SetIgnoreMouse(const Value: Boolean);
    procedure SetInDesktop(const Value: Boolean);
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    { Summary the desktop window handle }
    FDesktopHandle: HWND;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Edit: Boolean;
    function GetClientBounds: TRect;
    function GetHitTest(X, Y: integer): TGRWindowHitTest; virtual;
    function NormalizePoint(P: TPoint): TPoint;
    property GRDragging: TGRDragging read FGRDragging write SetGRDragging;
    property IgnoreMouse: Boolean read FIgnoreMouse write SetIgnoreMouse;
    property InDesktop: Boolean read FInDesktop write SetInDesktop;
  end;
  
  TGRCustomShapeForm = class(TGRCustomForm)
  private
    FMainPanel: TGRCustomControl;
    procedure SetMainPanel(Value: TGRCustomControl);
  protected
    FInitializedRegion: Boolean;
    FRealHeight: Integer;
    FRealWidth: Integer;
    FRegion: HRgn;
    procedure AssignMainPanel(const Value: TGRCustomControl); virtual;
    procedure DoImage32Changed(Sender: TObject);
    procedure DoShow; override;
    function GetClientRect: TRect; override;
    function GetRegionFromBitmap(const B: TBitmap32): HRGN; virtual;
    procedure InitRegion;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure UnAssignMainPanel(const Value: TGRCustomControl); virtual;
    procedure WMNCCalcSize(var Msg: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMWindowPosChanging(var Msg: TWMWindowPosChanging); message
      WM_WINDOWPOSCHANGING;
  public
    destructor Destroy; override;
    function GetHitTest(X, Y: integer): TGRWindowHitTest; override;
    procedure RecreateRegion;
    { Summary the Form for GRCustomControl derived. }
    { Description
    you need assign the MainPanel to use.
    }
    property MainPanel: TGRCustomControl read FMainPanel write SetMainPanel;
  end;
  
  { Description
  Layered Form(Win2k above)
  
  Paint The GRCustomControl(MainPanel) as the Layered Form.
  
    Edit 编辑 show the editor dialog to modify this Form properties.
  
  分析了VC的一个例子，发现似乎只要设置LayeredWindow，在wm_paint中就可以直接使用透明色
  }
  TGRCustomLayeredForm = class(TGRCustomForm)
  private
    { Summary Left Mouse down }
    FIsLMouseDown: Boolean;
  protected
    procedure Activate; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoImage32Changed(Sender: TObject);
    procedure iDraw;
    procedure InternalPaintBuffer(aBuffer: TBitmap32); virtual;
    procedure Paint; override;
    procedure PaintBuffer(aBuffer: TBitmap32);
    procedure Resize; override;
    { Summary when background changed }
    procedure UpdateLayeredWindowForDC(aDC: HDC; aBuffer: TBitmap32);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WndProc(var Message: TMessage); override;
  end;
  
  TGRPanelForm = class(TGRCustomLayeredForm)
  private
    FMainPanel: TGRPanel;
  protected
    procedure InternalPaintBuffer(aBuffer: TBitmap32); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MainPanel: TGRPanel read FMainPanel;
  end;
  
  TGRControlForm = class(TGRCustomLayeredForm)
  private
    FMainPanel: TGRCustomControl;
    procedure SetMainPanel(Value: TGRCustomControl);
  protected
    procedure AssignMainPanel(const Value: TGRCustomControl); virtual;
    procedure InternalPaintBuffer(aBuffer: TBitmap32); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure UnAssignMainPanel(const Value: TGRCustomControl); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Summary the Form for GRCustomControl derived. }
    { Description
    you need assign the MainPanel to use.
    }
    property MainPanel: TGRCustomControl read FMainPanel write SetMainPanel;
  end;
  
  TGRImg32Form = class(TGRCustomLayeredForm)
  private
    FImage32: TImage32;
  protected
    procedure InternalPaintBuffer(aBuffer: TBitmap32); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Image32: TImage32 read FImage32;
  end;
  
  TGRForm = class(TGRCustomLayeredForm)
  published
    property Caption;
    property Color;
    property Font;
    property OldCreateOrder;
    property OnActivate;
    property OnCanResize;
    property OnClick;
    property OnClose;
    property OnCloseQuery;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnCreate;
    property OnDblClick;
    property OnDeactivate;
    property OnDestroy;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnHelp;
    property OnHide;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnShortCut;
    property OnShow;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property PixelsPerInch;
  end;
  

implementation

//var
 //AppPath: string;

type
  TGRControlAccess = class(TGRCustomControl);
  
{$IFDEF MSWINDOWS}
const 
  FBlendOption: TBlendFunction = (
    BlendOp: AC_SRC_OVER;
    BlendFlags: 0;
    SourceConstantAlpha: 255;
    AlphaFormat: AC_SRC_ALPHA;
  );

{
http://msdn.microsoft.com/library/default.asp?url=/library/en-us/gdi/bitmaps_3b3m.asp

*snip*

AC_SRC_ALPHA
This flag is set when the bitmap has an Alpha channel (that is, per-pixel alpha). 
Note that the APIs use premultiplied alpha, which means that the red, green and 
blue channel values in the bitmap must be premultiplied with the alpha channel 
value. For example, if the alpha channel value is x, the red, green and blue 
channels must be multiplied by x and divided by 0xff prior to the call.

}
procedure PremultiplyBitmap(Bitmap: TBitmap32);
const w = Round($10000 * (1/255));
var
  p: PColor32;
  c: TColor32;
  i,a,r,g,b: Cardinal;
begin
  p:= Bitmap.PixelPtr[0,0];
  for i:= 0 to Bitmap.Width * Bitmap.Height - 1 do begin
    c:= p^;
    a:= (c shr 24) * w ;
    r:= c shr 16 and $FF; r:= r * a shr 16;
    g:= c shr 8 and $FF; g:= g * a shr 16;
    b:= c and $FF; b:= b * a shr 16;
    p^:= (c and $FF000000) or r shl 16 or g shl 8 or b;
    inc(p);
  end;
end;
{$ENDIF}

constructor TGRDragging.Create;
begin
  inherited Create;
  FMoveable := true;
  FSizeable := true;
  FDragMode := fdmFullDrag;
end;

procedure TGRDragging.Assign(Source: TPersistent);
begin
  if Source is TGRDragging then
  begin
    FMoveable := TGRDragging(Source).Moveable;
    FSizeable := TGRDragging(Source).Sizeable;
    FDragMode := TGRDragging(Source).DragMode;
  end
  else
    inherited ;
end;

constructor TGRCustomForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {FImage32 := TImage32Ex.Create(Self);
  FImage32.Parent := Self;
  FImage32.Align := alClient;
  FImage32.BitmapAlign := baTopLeft;
  FImage32.OnChange := DoImage32Changed;
  FImage32.ScaleMode := smStretch;
  //FImage32.Visible := False;
  //}
  //BorderStyle := bsNone;
  
  FGRDragging := TGRDragging.Create;
  
  {$IFDEF MSWINDOWS}
  FDesktopHandle := FindWindow('Progman','Program Manager');
  {$ENDIF}
end;

destructor TGRCustomForm.Destroy;
begin
  //FreeAndNil(FImage32);
  FreeAndNil(FGRDragging);
  inherited Destroy;
end;

function TGRCustomForm.Edit: Boolean;
begin
  Result := False;
  //Result := EditObject(MainPanel);
  //if Result then DoImage32Changed(nil);
end;

function TGRCustomForm.GetClientBounds: TRect;
begin
    Result := Rect(0, 0, Width, Height);
  (*
    { Default }
    case FBorderStyle of
      kbsStandard, kbsDialog:
        begin
          { Frame }
          Inc(Result.Left, DefaultBorderWidth);
          Dec(Result.Right, DefaultBorderWidth);
          Inc(Result.Top, DefaultBorderWidth);
          Dec(Result.Bottom, DefaultBorderWidth);
          { Caption }
          Inc(Result.Top, DefaultCaptionHeight);
        end;
      kbsToolWindow:
        begin
          { Frame }
          Inc(Result.Left, DefaultBorderWidth);
          Dec(Result.Right, DefaultBorderWidth);
          Inc(Result.Top, DefaultBorderWidth);
          Dec(Result.Bottom, DefaultBorderWidth);
          { Caption }
          Inc(Result.Top, DefaultToolCaptionHeight);
        end;
    end;
  //*)
end;

function TGRCustomForm.GetHitTest(X, Y: integer): TGRWindowHitTest;
  
  const
    SizeWidth = 20;
  var
    R: TRect;
    CaptionHeight: integer;
  
begin
    R := GetClientBounds;
    if (Y < R.Top) and (X < Width) or (FGRDragging.DragMode = fdmFullDrag) then
      Result := kwhtCaption
    else
      Result := kwhtClient;
  
  {  if (FWindowState = kwsMaximized) or
       (FWindowState = kwsMinimized) or
       (FWindowState = kwsRollup) then Exit;
   }
  
    { Top }
    if Y < R.Top - CaptionHeight then
      if X < SizeWidth then Result := kwhtTopLeft
      else
        if X > Width - SizeWidth then Result := kwhtTopRight
        else
          Result := kwhtTop;
  
    { Bottom }
    if (Y > R.Bottom) and (Y < Height) then
      if X < SizeWidth then Result := kwhtBottomLeft
      else
        if X > Width - SizeWidth then Result := kwhtBottomRight
        else
          Result := kwhtBottom;
  
    { Left }
    if X < R.Left then
      if Y < SizeWidth then Result := kwhtTopLeft
      else
        if Y > Height - SizeWidth then Result := kwhtBottomLeft
        else
          Result := kwhtLeft;
  
    { Right }
    if (X > R.Right) and (X < Width) then
      if Y < SizeWidth then Result := kwhtTopRight
      else
        if Y > Height - SizeWidth then Result := kwhtBottomRight
        else
          Result := kwhtRight;
end;

function TGRCustomForm.NormalizePoint(P: TPoint): TPoint;
var
  WindowPos, ClientPos: TPoint;
  HandleParent: HWND;
begin
  HandleParent := GetParent(Handle);
  { Calc windowpos - screen }
  WindowPos := Point(Left, Top);
  if HandleParent <> 0 then
    Windows.ClientToScreen(HandleParent, WindowPos);
  { Calc clientpos - screen }
  ClientPos := Point(0, 0);
  Windows.ClientToScreen(Handle, ClientPos);
  { Calc local pos }
  Result := P;
  Windows.ScreenToClient(Handle, Result);
  { Offset to client offset }
  Inc(Result.X, ClientPos.X-WindowPos.X);
  Inc(Result.Y, ClientPos.Y-WindowPos.Y);
end;

procedure TGRCustomForm.SetGRDragging(const Value: TGRDragging);
begin
  FGRDragging.Assign(Value);
end;

procedure TGRCustomForm.SetIgnoreMouse(const Value: Boolean);
var
  LWinExStyle: LongWord;
begin
  if FIgnoreMouse <> Value then
  begin
    FIgnoreMouse := Value;
    LWinExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
    if FIgnoreMouse then
    begin
      LWinExStyle := LWinExStyle or WS_EX_TRANSPARENT;
    end
    else
      LWinExStyle := LWinExStyle xor WS_EX_TRANSPARENT;
    SetWindowLong(Handle, GWL_EXSTYLE, LWinExStyle);
  end;
end;

procedure TGRCustomForm.SetInDesktop(const Value: Boolean);
begin
  if FInDesktop <> Value then
  begin
    FInDesktop := Value;
    if FInDesktop then
    begin
      //this will can not get the focus of the window if use the ParentWindow directly!!
      //ParentWindow := FDesktopHandle;
      Windows.SetParent(Handle, FDesktopHandle);
    end
    else
      Windows.SetParent(Handle, ParentWindow);
      //ParentWindow := 0;
  end;
end;

procedure TGRCustomForm.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
begin
  inherited;
  P := NormalizePoint(Point(Message.XPos, Message.YPos));
  
  Message.Result := HTCLIENT;
    case GetHitTest(P.X, P.Y) of
      kwhtCaption: if FGRDragging.Moveable then Message.Result := HTCAPTION;
      kwhtLeft: if FGRDragging.Sizeable and (WindowState <> wsMaximized) then Message.Result := HTLEFT;
      kwhtTop: if FGRDragging.Sizeable and (WindowState <> wsMaximized) then Message.Result := HTTOP;
      kwhtRight: if FGRDragging.Sizeable and (WindowState <> wsMaximized) then Message.Result := HTRIGHT;
      kwhtBottom: if FGRDragging.Sizeable and (WindowState <> wsMaximized) then Message.Result := HTBOTTOM;
      kwhtBorder: Message.Result := HTBORDER;
      kwhtTopLeft: if FGRDragging.Sizeable and (WindowState <> wsMaximized) then Message.Result := HTTOPLEFT;
      kwhtTopRight: if FGRDragging.Sizeable and (WindowState <> wsMaximized) then Message.Result := HTTOPRIGHT;
      kwhtBottomLeft: if FGRDragging.Sizeable and (WindowState <> wsMaximized) then Message.Result := HTBOTTOMLEFT;
      kwhtBottomRight: if FGRDragging.Sizeable and (WindowState <> wsMaximized) then Message.Result := HTBOTTOMRIGHT;
      kwhtCloseButton:Message.Result := HTCLOSE;
      kwhtHelpButton: Message.Result := HTHELP;
      kwhtMinButton: Message.Result := HTREDUCE;
      kwhtMaxButton: Message.Result := HTZOOM;
      kwhtRollButton: Message.Result := HTBORDER;
      kwhtTrayButton: Message.Result := HTBORDER;
      kwhtSysMenu: Message.Result := HTSYSMENU;
      kwhtNonClient: Message.Result := HTBORDER;
    end;
  
    {
    if FDisabledNCArea and (Message.Result = HTCLIENT) then
    begin
      C := Form.ControlAtPos(P, false);
      if C is TGraphicControl then
        Message.Result := HTCLIENT
      else
        Message.Result := HTCAPTION
    end;
  }
  (*
  with Message do
  begin
      If (Result=HTCLIENT) {and (FMoveable=fmsAlways)} then
      Begin
        Result:=HTCAPTION;
        //ShowMessage('ddd');
      End;
      (*Else If (Result=HTCAPTION) {and (FMoveable=fmsNever)} then
      Begin
        Result:=HTCLIENT;
      End; // * )
  end;
  //*)
end;

destructor TGRCustomShapeForm.Destroy;
begin
  if FRegion <> 0 then
    DeleteObject(FRegion);
  inherited Destroy;
end;

procedure TGRCustomShapeForm.AssignMainPanel(const Value: TGRCustomControl);
begin
  Value.Parent := Self;
  Value.Align := alClient;
  Value.OnChange := DoImage32Changed;
  Value.DragAsTitle := True;
  FreeNotification(Value);
end;

procedure TGRCustomShapeForm.DoImage32Changed(Sender: TObject);
begin
  if Assigned(FMainPanel) and (handle<> 0) and not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
  begin
    RecreateRegion;
  end;
end;

procedure TGRCustomShapeForm.DoShow;
begin
  if Assigned(FMainPanel) then
    InitRegion;
  inherited DoShow;
end;

function TGRCustomShapeForm.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  //InflateRect(Result, -13, -13);
end;

function TGRCustomShapeForm.GetHitTest(X, Y: integer): TGRWindowHitTest;
var
  BW: Integer;
begin
  //if BorderStyle = bsNone then
  begin
    Result := kwhtCaption;
  
    //if FGRDragging.Sizeable then
    begin
      //BW := FBorder.GetWidth;
      //if BW < 3 then
      BW := 3;
  
      if FRegion <> 0 then
      begin
        { Custom region }
        if (X > Width div 2) and (Y > Height div 2) then
          if not PtInRegion(FRegion, X+BW, Y+BW) then
            Result := kwhtBottomRight;
        if (X < Width div 2) and (Y < Height div 2) then
           if not PtInRegion(FRegion, X-BW, Y-BW) then
            Result := kwhtTopLeft;
        if (X > Width div 2) and (Y < Height div 2) then
          if not PtInRegion(FRegion, X+BW, Y-BW) then
            Result := kwhtTopRight;
        if (X < Width div 2) and (Y > Height div 2) then
          if not PtInRegion(FRegion, X-BW, Y+BW) then
            Result := kwhtBottomLeft;
      end
      else
      begin
        { Rect region }
        if (X < BW) then Result := kwhtLeft;
        if (X > Width-BW) then Result := kwhtRight;
        if (Y < BW) then Result := kwhtTop;
        if (Y > Height-BW) then Result := kwhtBottom;
  
        if (X < BW) and (Y < BW) then Result := kwhtTopLeft;
        if (X > Width-BW) and (Y < BW) then Result := kwhtTopRight;
        if (X < BW) and (Y > Height-BW) then Result := kwhtBottomLeft;
        if (X > Width-BW) and (Y > Height-BW) then Result := kwhtBottomRight;
      end;
    end;
    {$ifdef Debug}
    SendDebug('HitTest:'+ IntToStr(Integer(Result)));
    SendDebug('HitTest:'+ IntToStr(Integer(FRegion)));
    {$endif}
  end
  //else
end;

function TGRCustomShapeForm.GetRegionFromBitmap(const B: TBitmap32): HRGN;
  
  var
    Col, Row, MarkCol: Integer;
    RowRgn: HRgn;
    InShape, OnShapePixel: Boolean;
    //TransColor: TColor;
  
begin
  Result := 0;
  
  //TransColor := B.TransparentColor and $FFFFFF;          // Ignore Alpha Channel
  MarkCol := 0;
  
  for Row := 0 to B.Height - 1 do
  begin
    InShape := False;
    for Col := 0 to B.Width - 1 do
    begin
      OnShapePixel := TColor32Rec(B[Col, Row]).rgbAlpha <> 0;
  
      if OnShapePixel <> InShape then
      begin
        // If OnShapePixel <> InShape, then we have a state change...
        if InShape then
        begin
          // If currently InShape, then we are at the end of the current block of pixels.
          // Therefore, add the block to the region...
          RowRgn := CreateRectRgn( MarkCol, Row, Col, Row + 1 );
  
          if Result <> 0 then
          begin
            // First region has already been assigned, therefore just add this one to existing region
            CombineRgn( Result, Result, RowRgn, RGN_OR );
            DeleteObject( RowRgn );
          end
          else
            Result := RowRgn;
        end
        else
        begin
          // No currently in the shape, so mark this column
          MarkCol := Col;
        end;
  
        // change mode, looking for the first or last real pixel?
        InShape := not InShape;
      end;
    end; { for Col }
  
    // was the last pixel in this row a real pixel?
    if InShape then
    begin
      // If still InShape then last pixel is not transparent--add block to the region
      RowRgn := CreateRectRgn( MarkCol, Row, B.Width - 1, Row + 1 );
  
      if Result <> 0 then
      begin
        // First region has already been assigned, therefore just add this one to existing region
        CombineRgn( Result, Result, RowRgn, RGN_OR );
        DeleteObject( RowRgn );
      end
      else
        Result := RowRgn;
    end;
  end; { for Row }
end;

procedure TGRCustomShapeForm.InitRegion;
var
  aBuffer: TBItmap32;
begin
  if not FInitializedRegion then
  begin
    FInitializedRegion := True;
  
    aBuffer := TBitmap32.Create;
    try
      aBuffer.SetSize(Width, Height);
      //aBuffer.SetSize(FMainPanel.Width, FMainPanel.Height);
      //aBuffer.Clear(0);
  
      FMainPanel.PaintSelfToBuffer(aBuffer, True);
  
      if FRegion <> 0 then
        DeleteObject(FRegion);
      FRegion := GetRegionFromBitmap(aBuffer);
      // This should only be called if the Owner is indeed a TForm
      // i.e. constructor takes care of making sure this is true
      SetWindowRgn(Handle, FRegion, True);
    finally
      aBuffer.Free;
    end;
  end;
  //}
end;

procedure TGRCustomShapeForm.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (Operation = opRemove) and (aComponent = FMainPanel) then
    FMainPanel := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TGRCustomShapeForm.RecreateRegion;
begin
  FInitializedRegion := False;
  InitRegion;
end;

procedure TGRCustomShapeForm.SetMainPanel(Value: TGRCustomControl);
begin
  if Value <> FMainPanel then
  begin
    if Assigned(FMainPanel) then
    begin
      UnAssignMainPanel(FMainPanel);
    end;
    FMainPanel := Value;
    if Assigned(FMainPanel) then
    begin
      AssignMainPanel(FMainPanel);
    end;
  end;
end;

procedure TGRCustomShapeForm.UnAssignMainPanel(const Value: TGRCustomControl);
begin
  Value.Parent := nil;
  Value.OnChange := nil;
  Value.DragAsTitle := False;
  RemoveFreeNotification(Value);
end;

procedure TGRCustomShapeForm.WMNCCalcSize(var Msg: TWMNCCalcSize);
  
  var
    rgrc: PNCCalcSizeParams;
    WP: PWindowPos;
    m_lLeft, m_lRight, m_lTop, m_lBottom: integer;
  
begin
  (*
  if (Msg.CalcValidRects) then
  begin
    rgrc := Msg.CalcSize_Params;
  
    with GetClientRect do
    begin
      m_lLeft := left;
      m_lRight := Width-right;
      m_lTop := top;
      m_lBottom := Height-bottom;
    end;
  
    WP := rgrc.lppos;
    with rgrc^.rgrc[0] do
    begin
      left := WP^.x;
      top := WP^.y;
      right := WP^.x + WP^.cx;
      bottom := WP^.y + WP^.cy;
      left := left + m_lLeft;
      top := top + m_lTop;
      right := right - m_lRight;
      bottom := bottom - m_lBottom;
    end;
    rgrc^.rgrc[1] := rgrc^.rgrc[0];
    Msg.CalcSize_Params := rgrc;
    Msg.Result := WVR_VALIDRECTS;
  end;
  //*)
end;

procedure TGRCustomShapeForm.WMWindowPosChanging(var Msg: TWMWindowPosChanging);
begin
  (*
  { Cahnge Size }
  if (Msg.WindowPos^.cx <> FRealWidth) or (Msg.WindowPos^.cy <> FRealHeight) then
  begin
    FRealWidth := Msg.WindowPos^.cx;
    FRealHeight := Msg.WindowPos^.cy;
  end;
  *)
end;

procedure TGRCustomLayeredForm.Activate;
begin
  inherited Activate;
  iDraw;
end;

procedure TGRCustomLayeredForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  //if IsWin2k then
    //Shit! this would raise exception, it does not work!
    //with Params do
      //ExStyle := ExStyle or WS_EX_LAYERED;
end;

procedure TGRCustomLayeredForm.DoImage32Changed(Sender: TObject);
begin
  if (handle<> 0) and not ((csLoading in ComponentState) and (csDestroying in ComponentState)) then
  begin
    iDraw;
  end;
end;

procedure TGRCustomLayeredForm.iDraw;
var
  DC: HDC;
  LWinExStyle: LongWord;
  LBuffer: TBitmap32;
begin
  //if FImage32.Bitmap.Empty then exit;
  LWinExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
  if (LWinExStyle and WS_EX_LAYERED) <> WS_EX_LAYERED then
    if SetWindowLong(Handle, GWL_EXSTYLE, LWinExStyle or WS_EX_LAYERED) = 0 then
      Raise Exception.Create(rsCreateLayeredWinError);
  
  LBuffer := TBitmap32.Create;
  try
    PaintBuffer(LBuffer);
  
    DC := GetDC(Handle);
    try
      UpdateLayeredWindowForDC(DC, LBuffer);
    finally
      ReleaseDC(Handle, DC);
    end;
  
  finally
    LBuffer.Free;
  end;
  //}
end;

procedure TGRCustomLayeredForm.InternalPaintBuffer(aBuffer: TBitmap32);
begin
end;

procedure TGRCustomLayeredForm.Paint;
begin
  //FImage32.Bitmap.DrawTo(Canvas.Handle, 0, 0);
  //inherited;
end;

procedure TGRCustomLayeredForm.PaintBuffer(aBuffer: TBitmap32);
begin
  aBuffer.SetSize(Width, Height);
  aBuffer.Clear(0);
  
  InternalPaintBuffer(aBuffer);
  //FImage32.PaintTo(aBuffer, aBuffer.ClipRect);
  
  {$IFDEF MSWINDOWS}
  PremultiplyBitmap(aBuffer);
  {$endif}
end;

procedure TGRCustomLayeredForm.Resize;
begin
  inherited Resize;
  //
  DoImage32Changed(nil);
end;

procedure TGRCustomLayeredForm.UpdateLayeredWindowForDC(aDC: HDC; aBuffer:
  TBitmap32);
var
  TopLeft, BmpTopLeft: TPoint;
  vFormSize: TSize;
begin
  vFormSize.cx := Width;
  vFormSize.cy := Height;
  BmpTopLeft := Point(0, 0);
  TopLeft := BoundsRect.TopLeft;
  
  if not Win32Check(LongBool(aDC)) then
    RaiseLastWin32Error;
  
  if not Win32Check(UpdateLayeredWindow(Handle, aDC, @TopLeft, @vFormSize,
    aBuffer.Handle, @BmpTopLeft, clNone, @FBlendOption, ULW_ALPHA))
  then
    RaiseLastWin32Error;
end;

procedure TGRCustomLayeredForm.WMPaint(var Message: TWMPaint);
begin
  inherited;
  Application.ProcessMessages;
end;

procedure TGRCustomLayeredForm.WndProc(var Message: TMessage);
begin
  {$ifdef debug}
  //SendInteger('Message', Message.Msg);
  //SendInteger('WM_MOUSEMOVE', WM_MOUSEMOVE);
  {$endif}
  (*
  if (Message.Msg = WM_MOUSEMOVE) {and
     //判断光标是否在客户区内
     (DefWindowProc(Handle,WM_NCHitTest,
     0,GetMessagePos)=HTClient)} then
  begin
    if (TWMMouse(Message).Keys and MK_LBUTTON) = MK_LBUTTON then
    begin
      Message.Msg := WM_NCLButtonDown;
    end;
  end;
  *)
  inherited WndProc(Message);
end;

constructor TGRPanelForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMainPanel := TGRPanel.Create(Self);
  FMainPanel.Parent := Self;
  FMainPanel.Align := alClient;
  FMainPanel.OnChange := DoImage32Changed;
  FMainPanel.DragAsTitle := True;
end;

destructor TGRPanelForm.Destroy;
begin
  FreeAndNil(FMainPanel);
  inherited Destroy;
end;

procedure TGRPanelForm.InternalPaintBuffer(aBuffer: TBitmap32);
begin
  FMainPanel.PaintSelfToBuffer(aBuffer, True);
end;

constructor TGRControlForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {FMainPanel := TGRPanel.Create(Self);
  FMainPanel.Parent := Self;
  FMainPanel.Align := alClient;
  FMainPanel.OnChange := DoImage32Changed;
  FMainPanel.DragAsTitle := True;
  //}
end;

destructor TGRControlForm.Destroy;
begin
  //FreeAndNil(FMainPanel);
  inherited Destroy;
end;

procedure TGRControlForm.AssignMainPanel(const Value: TGRCustomControl);
begin
  Value.Parent := Self;
  Value.Align := alClient;
  Value.OnChange := DoImage32Changed;
  Value.DragAsTitle := True;
  FreeNotification(Value);
end;

procedure TGRControlForm.InternalPaintBuffer(aBuffer: TBitmap32);
begin
  if Assigned(FMainPanel) then
    FMainPanel.PaintSelfToBuffer(aBuffer, True);
end;

procedure TGRControlForm.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (Operation = opRemove) and (aComponent = FMainPanel) then
    FMainPanel := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TGRControlForm.SetMainPanel(Value: TGRCustomControl);
begin
  if Value <> FMainPanel then
  begin
    if Assigned(FMainPanel) then
    begin
      UnAssignMainPanel(FMainPanel);
    end;
    FMainPanel := Value;
    if Assigned(FMainPanel) then
    begin
      AssignMainPanel(FMainPanel);
    end;
  end;
end;

procedure TGRControlForm.UnAssignMainPanel(const Value: TGRCustomControl);
begin
  Value.Parent := nil;
  Value.OnChange := nil;
  Value.DragAsTitle := False;
  RemoveFreeNotification(Value);
end;

constructor TGRImg32Form.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImage32 := TImage32.Create(Self);
  FImage32.Parent := Self;
  FImage32.Align := alClient;
  FImage32.BitmapAlign := baTopLeft;
  FImage32.OnChange := DoImage32Changed;
  FImage32.ScaleMode := smStretch;
  //FImage32.Visible := False;
  //BorderStyle := bsNone;
end;

destructor TGRImg32Form.Destroy;
begin
  FreeAndNil(FImage32);
  inherited Destroy;
end;

procedure TGRImg32Form.InternalPaintBuffer(aBuffer: TBitmap32);
begin
  FImage32.PaintTo(aBuffer, aBuffer.ClipRect);
end;


initialization
  //AppPath := ExtractFilePath(ParamStr(0));
end.
