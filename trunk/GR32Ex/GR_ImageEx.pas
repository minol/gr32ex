unit GR_ImageEx;

// Transparent TImage32 by Michael Haralabos
// The layer MouseEnter MouseLeave by riceball

interface

{$I GR32.inc}

uses
{$IFDEF CLX}
  Qt, Types, QControls, QGraphics, QForms, QConsts,
  {$IFDEF LINUX}Libc,{$ENDIF}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
{$ELSE}
  Windows, Messages, Controls, Graphics, Forms, 
  //Dialogs,
{$ENDIF}
  Classes, SysUtils,
  GR32_Image, GR32_Layers, GR32;

type
  TImage32Ex = class(TImage32)
  private
    FTransparent: Boolean;

    procedure SetTransparent(const Value: Boolean);
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
  protected
    FIsDragging: Boolean;
    FLastShift: TShiftState;
    FLastMouseDownPos: TPoint;
{$IFDEF CLX}
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
{$ELSE}
    procedure MouseEnter; override;
    procedure MouseLeave; override;
{$ENDIF}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); overload; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); overload; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); overload; override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure WndProc(var Message: TMessage);override;
  public
    procedure ExecClearBackgnd(Dest: TBitmap32; StageNum: Integer); override;
  published
    property Enabled;
    property Transparent: Boolean read FTransparent write SetTransparent;
  end;

procedure Register;

implementation

Type
  TLayerHack = class(TCustomLayer);
  TMyShiftState  = (ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble);
  TMyShiftStates = set of TMyShiftState;

//Not double click in shift state
function IsMouseButtonDown(Shift: TMyShiftStates; Button: TMyShiftState): Boolean;
begin
  Result := (Button in Shift) and not (ssDouble in Shift);
end;

procedure TImage32Ex.ExecClearBackgnd(Dest: TBitmap32; StageNum: Integer);
var
  P: TPoint;
  SaveIndex: Integer;
begin
  if FTransparent and Assigned(Parent) and
     not (Assigned(Bitmap) and (BitmapAlign = baTile)) then
  begin
    SaveIndex := SaveDC(Dest.Handle);
    GetViewportOrgEx(Dest.Handle, P);
    SetViewportOrgEx(Dest.Handle, P.X - Left, P.Y - Top, nil);
    IntersectClipRect(Dest.Handle, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
    Parent.Perform(WM_ERASEBKGND, Dest.Handle, 0);
    Parent.Perform(WM_PAINT, Dest.Handle, 0);
    RestoreDC(Dest.Handle, SaveIndex);
  end
  else
    inherited;
end;

procedure TImage32Ex.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

{$IFDEF CLX}
procedure TImage32Ex.MouseEnter(AControl: TControl);
{$ELSE}
procedure TImage32Ex.MouseEnter;
{$ENDIF}
begin
  if (Layers.MouseEvents) and (Layers.MouseListener <> nil) and not Layers.MouseListener.MouseInControl then
    TLayerHack(Layers.MouseListener).MouseEnter;
  inherited;
end;

{$IFDEF CLX}
procedure TImage32Ex.MouseLeave(AControl: TControl);
{$ELSE}
procedure TImage32Ex.MouseLeave;
{$ENDIF}
begin
  if (Layers.MouseEvents) and (Layers.MouseListener <> nil) and Layers.MouseListener.MouseInControl then
    TLayerHack(Layers.MouseListener).MouseLeave;
  inherited;
end;

procedure TImage32Ex.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FLastShift := Shift;
  FLastMouseDownPos.X := X;
  FLastMouseDownPos.Y := Y;
  if not FIsDragging then
    inherited;
end;

const
  sc_DragMove=$F012; //61458

procedure TImage32Ex.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not FIsDragging then
  begin
    FIsDragging := IsMouseButtonDown(TMyShiftStates(Shift), ssLeft) and IsMouseButtonDown(TMyShiftStates(FLastShift), ssLeft) 
    and ((Abs(X - FLastMouseDownPos.X) >=2) or (Abs(Y - FLastMouseDownPos.Y) >=2));
  end;
  if FIsDragging and (Parent is TCustomForm) then
  begin
  	//no the mouseUp will be disabled!!
    ReleaseCapture;
    SendMessage(Parent.Handle, WM_SYSCOMMAND, sc_DragMove, 0);
  end;
  inherited;
end;

procedure TImage32Ex.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not FIsDragging then
    inherited;
  //when ReleaseCapture this would be never execute.
  FIsDragging := False;
end;

procedure TImage32Ex.WMKillFocus(var Message: TMessage);
begin
  DoExit;
  inherited;
end;

procedure TImage32Ex.KeyDown(var Key: Word; Shift: TShiftState); 
begin
  If (Layers.MouseListener <> nil) and Layers.KeyEvents Then
  Begin
    TLayerHack(Layers.MouseListener).KeyDown(Key, Shift);
  End; // If
  inherited;
end;

procedure TImage32Ex.KeyPress(var Key: Char); 
begin
  If (Layers.MouseListener <> nil) and Layers.KeyEvents Then
  Begin
    TLayerHack(Layers.MouseListener).KeyPress(Key);
  End; // If
  inherited;
end;

procedure TImage32Ex.KeyUp(var Key: Word; Shift: TShiftState); 
begin
  If (Layers.MouseListener <> nil) and Layers.KeyEvents Then
  Begin
    TLayerHack(Layers).KeyUp(Key, Shift);
  End; // If
  inherited;
end;

procedure TImage32Ex.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_MOUSEFIRST..WM_MOUSELAST: if IsControlMouseMsg(TWMMouse(Message)) then
    begin
      //first pass it to self to prevent from be processed by children  .
      Dispatch(Message);
      //exit;
    end
    //else ShowMessage(IntToStr(TWMMouse(Message).XPos))
    ;
  end;

  inherited;
  If (Layers.MouseListener <> nil) and Layers.MessageEvents Then
  Begin
    TLayerHack(Layers.MouseListener).WndProc(Message);
  End; // If
end;

procedure Register;
begin
  RegisterComponents('Graphics32', [TImage32Ex]);
end;

end.

