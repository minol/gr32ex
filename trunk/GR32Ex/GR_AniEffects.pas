{*******************************************************}
{                                                       }
{       Animation effect class  by riceball             }
{                                                       }
{       Copyright (c) Riceball LEE                      }
{       riceballl@hotmail.com                           }
{                                                       }
{*******************************************************}

{$I Setting.inc}
{$define debug} //Show FPS

{ Summary General Animation Graphic Effect Engine }
{ Description
通用动画特效引擎抽象层, 用以实现各种动画特效，如：雪花，星空。

Usage:
  FEff:= TRVBackEffects.Create;
  FEff.RichView := RichView2;
  TGRWaterAnimationEffect.Create(FEff);

  FEff.Enabled := True;
}

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
 * The Original Code is GR_AniEffects
 *
 * The Initial Developer of the Original Code is Riceball LEE
 * Portions created by Riceball LEE are Copyright (C) 2004-2007
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_AniEffects;

interface

uses
  {$ifdef Debug}
  //DbugIntf,
  {$endif} 
     Windows, Messages, Classes, Graphics
     //, Forms // Application
     , SysUtils
     , Controls
     , GR32
     ;

type
  TGRCustomAnimationEffect = class;
  { Summary the abstract Effefts egnine }
  { Description
  you should override this class to use.
  
  the DoWMPaint is no use!!
  so you have to do the paint by youself.
  
  and the Timer is your work by yourself.
  }
  TGRCustomAnimationEffects = class(TList)
  private
    procedure SetControl(const Value: TControl);
    procedure SetEnabled(Value: Boolean);
  protected
    FControl: TControl;
    FDrawing: Boolean;
    FEnabled: Boolean;
    FFPS: LongWord;
    FFPSCurrent: LongWord;
    FFPSOldTime: LongWord;
    FFPSTime: LongWord;
    FLastTickCount: LongWord;
    FOldResizeProc: TNotifyEvent;
    FOldWndProc: TWndMethod;
    FTimerId: Integer;
    FUpdating: Boolean;
    FWinStyle: LongInt;
    FOnUpdate: TNotifyEvent;
    FOnUpdating: TNotifyEvent;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure DoControlPaint(Sender: TControl; DC: HDC); overload; virtual;
    procedure DoControlPaint(Sender: TBitmap32); overload;virtual;
    procedure DoControlResize(Sender: TObject); virtual;
    procedure DoControlWndProc(var Message: TMessage); virtual;
    procedure DoMouseDown(var Message: TWMMouse; Button: TMouseButton);
    procedure DoMouseMove(var Message: TWMMouseMove);
    procedure DoMouseUp(var Message: TWMMouse; Button: TMouseButton);
    procedure DoTimer(Sender: TObject);
    { Summary not used }
    procedure DoWMPaint(var Message: TWMPaint); virtual;
    { Summary true means begin animation }
    procedure HookControl(Value: TControl; Hooked: Boolean); virtual;
    procedure InternalDoTimer; virtual;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    { Summary Adds a new item. }
    { Description
    If the number of items exceeds the maximum, frees the Item instead.
    }
    procedure Add(var aItem: TGRCustomAnimationEffect);
    property Control: TControl read FControl write SetControl;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnUpdating: TNotifyEvent read FOnUpdating write FOnUpdating;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;
  
  { Summary the Custom Background Effect }
  { Description
  you must override DoPaint method.
  the DoResize is optional.
  }
  TGRCustomAnimationEffect = class(TObject)
  private
    procedure SetHeight(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetTop(Value: Integer);
    procedure SetWidth(Value: Integer);
  protected
    FEnabled: Boolean;
    FHeight: Integer;
    FLeft: Integer;
    FOwner: TGRCustomAnimationEffects;
    FTop: Integer;
    FWidth: Integer;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); virtual;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); virtual;
    procedure DoPaint(Sender: TControl; DC: HDC); overload;virtual; abstract;
    procedure DoPaint(Sender: TBitmap32); overload;virtual; abstract;
    { Summary the Owner.FControl size changed. }
    procedure DoResize(Sender: TControl); virtual;
    procedure DoTimer(MoveCount: TFloat); virtual;
    { Summary the width or height changed. }
    procedure SizeChanged(Sender: TControl); virtual;
  public
    constructor Create(aOwner: TGRCustomAnimationEffects); virtual;
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Height: Integer read FHeight write SetHeight;
    property Left: Integer read FLeft write SetLeft;
    property Owner: TGRCustomAnimationEffects read FOwner;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
  end;
  

const
  cMaxWaterSize = 256;
  cMinIntervalValue= 30;
  cMaxAnimationEffects = 500;

implementation

uses
  Forms,  //KeysToShiftState
  Math;

type
  TControlAccess = Class(TControl);
  TCustomControlAccess = Class(TCustomControl);
  TGraphicControlAccess = Class(TGraphicControl);

//Extract from Forms
{function KeysToShiftState(Keys: Word): TShiftState;
begin
  Result := [];
  if Keys and MK_SHIFT <> 0 then Include(Result, ssShift);
  if Keys and MK_CONTROL <> 0 then Include(Result, ssCtrl);
  if Keys and MK_LBUTTON <> 0 then Include(Result, ssLeft);
  if Keys and MK_RBUTTON <> 0 then Include(Result, ssRight);
  if Keys and MK_MBUTTON <> 0 then Include(Result, ssMiddle);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
end;
//}

constructor TGRCustomAnimationEffects.Create;
begin
  inherited;
  
  //Application.OnIdle := AppIdle;
end;

destructor TGRCustomAnimationEffects.Destroy;
begin
  SetEnabled(False);
  inherited Destroy;
end;

procedure TGRCustomAnimationEffects.Add(var aItem: TGRCustomAnimationEffect);
begin
  if Count = cMaxAnimationEffects then begin
    aItem.Free;
    aItem := nil;
    exit;
  end;
  inherited Add(aItem);
  if aItem.Owner <> Self then aItem.FOwner := Self;
end;

procedure TGRCustomAnimationEffects.AppIdle(Sender: TObject; var Done: Boolean);
begin
  if FEnabled then
    DoTimer(Sender);
end;

procedure TGRCustomAnimationEffects.DoControlPaint(Sender: TBitmap32);
var
  LItem: TGRCustomAnimationEffect;
  i: Integer;
  s: string;
begin
  {$ifdef Debug}
  //SendDebug('Drawing:'+IntToStr(Integer(FDrawing)));
  //SendDebug('Updating:'+IntToStr(Integer(FUpdating)));
  {$endif}
  
  if not FDrawing then
  try
    FDrawing := True;
  
  
    For i := 0 to Count -1 Do
    begin
      LItem := TGRCustomAnimationEffect(Items[i]);
      if LItem.Enabled then
        LItem.DoPaint(Sender);
    end;
  
    {$ifdef Debug}
    //Count The FPS
    FFPSTime := GetTickCount;
    if FFPSTime - FFPSOldTime >= 1000 then
    begin
      FFPS := FFPSCurrent;
      FFPSCurrent := 0;
      FFPSOldTime := FFPSTime;
    end
    else
      Inc(FFPSCurrent);
  
    //FBuffer.PenColor := clBlack32;
    //FBuffer.TextOut(0,0, 'FPS:'+ IntToStr(Trunc(MoveCount / 100)));
      //FBuffer.RenderText(0,0, 'FPS:'+ IntToStr(FFPSCurrent), 0, clBlack32);
      s := 'FPS:'+ IntToStr(FFPS);
      Sender.FillRect(0,0,50,18, clWhite32);
      Sender.Textout(0,0,s);
    //-----}
    {$endif}
  
  finally
    //aCanvas.UnLock;
    FDrawing := False;
  end;
end;
//*)

procedure TGRCustomAnimationEffects.DoControlPaint(Sender: TControl; DC: HDC);
var
  LItem: TGRCustomAnimationEffect;
  i: Integer;
  s: string;
begin
  {$ifdef Debug}
  //SendDebug('Drawing:'+IntToStr(Integer(FDrawing)));
  //SendDebug('Updating:'+IntToStr(Integer(FUpdating)));
  {$endif}
  
  if not FDrawing then
  try
    FDrawing := True;
  
  
    For i := 0 to Count -1 Do
    begin
      LItem := TGRCustomAnimationEffect(Items[i]);
      if LItem.Enabled then
        LItem.DoPaint(Sender, DC);
    end;
  
    {$ifdef Debug}
    //Count The FPS
    FFPSTime := GetTickCount;
    if FFPSTime - FFPSOldTime >= 1000 then
    begin
      FFPS := FFPSCurrent;
      FFPSCurrent := 0;
      FFPSOldTime := FFPSTime;
    end
    else
      Inc(FFPSCurrent);
  
    //FBuffer.PenColor := clBlack32;
    //FBuffer.TextOut(0,0, 'FPS:'+ IntToStr(Trunc(MoveCount / 100)));
      //FBuffer.RenderText(0,0, 'FPS:'+ IntToStr(FFPSCurrent), 0, clBlack32);
      SetBkMode(dc, TRANSPARENT);
      s := 'FPS:'+ IntToStr(FFPS);
      textout(dc,0,0,PChar(s),Length(s));
    //-----}
    {$endif}
  
  finally
    //aCanvas.UnLock;
    FDrawing := False;
  end;
end;

procedure TGRCustomAnimationEffects.DoControlResize(Sender: TObject);
var
  LItem: TGRCustomAnimationEffect;
  i: Integer;
begin
  if Assigned(FOldResizeProc) then FOldResizeProc(Sender);
  For i := 0 to Count -1 Do
  begin
    LItem := TGRCustomAnimationEffect(Items[i]);
    if LItem.Enabled then
      LItem.DoResize(TControl(Sender));
  end;
end;

procedure TGRCustomAnimationEffects.DoControlWndProc(var Message: TMessage);
begin
  if Assigned(FOldWndProc) then FOldWndProc(Message);
  case Message.Msg of
    WM_Paint:
      DoWMPaint(TWMPaint(Message));
    {WM_Timer:
      if (FTimerId<>0) and (TWMTimer(Message).TimerId = FTimerId) then
      begin
        DoTimer;
      end;//}
    WM_MouseMove: DoMouseMove(TWMMouseMove(Message));
    WM_LButtonDown: DoMouseDown(TWMMouse(Message), mbLeft);
    WM_LBUTTONUP: DoMouseUp(TWMMouse(Message), mbLeft);
    WM_RButtonDown: DoMouseDown(TWMMouse(Message), mbRight);
    WM_RBUTTONUP: DoMouseUp(TWMMouse(Message), mbRight);
    WM_MBUTTONUP: DoMouseUp(TWMMouse(Message), mbMiddle);
    WM_MButtonDown: DoMouseDown(TWMMouse(Message), mbMiddle);
  end;
end;

procedure TGRCustomAnimationEffects.DoMouseDown(var Message: TWMMouse; Button:
  TMouseButton);
var
  I: Integer;
  LItem: TGRCustomAnimationEffect;
  Shift: TShiftState;
  LPos: TPoint;
begin
  if Assigned(FControl) then
  with Message do
  begin
    Shift := KeysToShiftState(Keys);
  
    with TControlAccess(FControl) do
    begin
      if (Width > 32768) or (Height > 32768) then
        LPos := CalcCursorPos
      else begin
        LPos.X := XPos;
        LPOs.Y := YPos;
      end;
    end;
  
    For i := 0 to Count -1 Do
    begin
      LItem := TGRCustomAnimationEffect(Items[i]);
      if LItem.Enabled then
        LItem.DoMouseDown(Button, Shift, LPos.X, LPos.Y);
    end;
  end;
end;

procedure TGRCustomAnimationEffects.DoMouseMove(var Message: TWMMouseMove);
var
  I: Integer;
  LItem: TGRCustomAnimationEffect;
  Shift: TShiftState;
  LPos: TPoint;
begin
  if Assigned(FControl) then
  with Message do
  begin
    Shift := KeysToShiftState(Keys);
    with TControlAccess(FControl) do
    begin
      if (Width > 32768) or (Height > 32768) then
        LPos := CalcCursorPos
      else begin
        LPos.X := XPos;
        LPOs.Y := YPos;
      end;
    end;
  
    For i := 0 to Count -1 Do
    begin
      LItem := TGRCustomAnimationEffect(Items[i]);
      if LItem.Enabled then
        LItem.DoMouseMove(Shift, LPos.X, LPos.Y);
    end;
  end;
end;

procedure TGRCustomAnimationEffects.DoMouseUp(var Message: TWMMouse; Button:
  TMouseButton);
var
  I: Integer;
  LItem: TGRCustomAnimationEffect;
  Shift: TShiftState;
begin
  //if Assigned(FOldMouseUpProc) then FOldMouseUpProc(Sender, Button, Shift, X, Y);
  with Message do
  begin
    Shift := KeysToShiftState(Keys);
  
    For i := 0 to Count -1 Do
    begin
      LItem := TGRCustomAnimationEffect(Items[i]);
      if LItem.Enabled then
        LItem.DoMouseUp(Button, Shift, XPos, YPos);
    end;
  end;
end;

procedure TGRCustomAnimationEffects.DoTimer(Sender: TObject);
begin
  if not FUpdating then
  try
    FUpdating := True;
    if Assigned(FOnUpdating) then FOnUpdating(Self);
    InternalDoTimer;
    if Assigned(FOnUpdate) then FOnUpdate(Self);
  finally
    FUpdating := False;
  end;
end;

procedure TGRCustomAnimationEffects.DoWMPaint(var Message: TWMPaint);
var
  DC: HDC;
  PS: TPaintStruct;
begin
  (*
  if Assigned(FControl)  then
  begin
    DC := Message.DC;
    {$ifdef Debug}
    SendDebug('WMPaint:'+IntToStr(DC));
    {$endif}
    if (DC <> 0) then
    begin
      //DoControlPaint(FControl, DC);
  
    end
    else if Control is TWinControl then
    begin
  
      {DC := BeginPaint(TWinControl(FControl).Handle, PS);
      try
        DoControlPaint(FControl, DC);
      finally
        EndPaint(TWinControl(FControl).Handle, PS);
      end;''}
    end;
  end;
  
  //*)
end;

procedure TGRCustomAnimationEffects.HookControl(Value: TControl; Hooked:
  Boolean);
begin
  if Hooked then
  begin
    FControl := Value;
    FOldWndProc := Value.WindowProc;
    Value.WindowProc := DoControlWndProc;
    FOldResizeProc := TControlAccess(Value).OnResize;
    TControlAccess(Value).OnResize := DoControlResize;
    DoControlResize(Value);
    FLastTickCount := GetTickCount;
  
  end
  else begin //Unhook
    Value.WindowProc := FOldWndProc;
    FOldWndProc := nil;
    TControlAccess(Value).OnResize := FOldResizeProc;
  
  
    FOldResizeProc := nil;
    FControl := nil;
  end;
end;

procedure TGRCustomAnimationEffects.InternalDoTimer;
var
  Tick: TFloat;
  I: Integer;
  LItem: TGRCustomAnimationEffect;
begin
    //FControl.Repaint;
  
    Tick := (GetTickCount - FLastTickCount) * 0.1;
    FLastTickCount := GetTickCount;
  
    For i := 0 to Count -1 Do
    begin
      LItem := TGRCustomAnimationEffect(Items[i]);
      if LItem.Enabled then
        LItem.DoTimer(Tick);
    end;
  
    //DoPaint;
end;

procedure TGRCustomAnimationEffects.Notify(Ptr: Pointer; Action:
  TListNotification);
begin
  if (Action = lnDeleted) and (TObject(Ptr) is TGRCustomAnimationEffect) then
  begin
    TGRCustomAnimationEffect(Ptr).FOwner := nil;
    TGRCustomAnimationEffect(Ptr).Free;
    //Ptr := nil;
  end;
end;

procedure TGRCustomAnimationEffects.SetControl(const Value: TControl);
begin
  if FControl <> Value then
  begin
    Enabled := False;
    FControl := Value;
  end;
end;

procedure TGRCustomAnimationEffects.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if Assigned(FControl) then
      HookControl(FControl, FEnabled);
  end;
end;

constructor TGRCustomAnimationEffect.Create(aOwner: TGRCustomAnimationEffects);
begin
  inherited Create;
  FEnabled := True;
  FOwner := aOwner;
  if Assigned(FOwner) then
  begin
    FOwner.Add(Self);
  end;
end;

destructor TGRCustomAnimationEffect.Destroy;
begin
  if Assigned(FOwner) then FOwner.Remove(Self);
  inherited;
end;

procedure TGRCustomAnimationEffect.DoMouseDown(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
end;

procedure TGRCustomAnimationEffect.DoMouseMove(Shift: TShiftState; X, Y:
  Integer);
begin
end;

procedure TGRCustomAnimationEffect.DoMouseUp(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
end;

procedure TGRCustomAnimationEffect.DoResize(Sender: TControl);
begin
  if (FHeight <= 0) or (FHeight > Sender.ClientHeight - FTop) then
    FHeight := Sender.ClientHeight - FTop;
  
  if (FWidth <= 0) or (FWidth > Sender.ClientWidth - FLeft) then
    FWidth := Sender.ClientWidth - FLeft;
  SizeChanged(Sender);
end;

procedure TGRCustomAnimationEffect.DoTimer(MoveCount: TFloat);
begin
end;

procedure TGRCustomAnimationEffect.SetHeight(Value: Integer);
begin
  if FHeight <> Value then
  begin
  if Value > Owner.FControl.ClientHeight - FTop then
    Value := Owner.FControl.ClientHeight - FTop;
  FHeight := Value;
  SizeChanged(Owner.FControl);
  end;
end;

procedure TGRCustomAnimationEffect.SetLeft(Value: Integer);
begin
  if FLeft <> Value then
  begin
  FLeft := Value;
  end;
end;

procedure TGRCustomAnimationEffect.SetTop(Value: Integer);
begin
  if FTop <> Value then
  begin
  FTop := Value;
  end;
end;

procedure TGRCustomAnimationEffect.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
  if Value > Owner.FControl.ClientWidth - FLeft then
    Value := Owner.FControl.ClientWidth - FLeft;
  FWidth := Value;
  SizeChanged(Owner.FControl);
  end;
end;

procedure TGRCustomAnimationEffect.SizeChanged(Sender: TControl);
begin
end;


initialization


end.
