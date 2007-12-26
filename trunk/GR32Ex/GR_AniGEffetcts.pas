{ Description
the Genernal Animation Effects Engine.
Supports the TCustomPaintBox32, TCustomControl, TGraphicControl and TCustomForm
  Note: it will flick on the TCustomControl, TGraphicControl and TCustomForm.
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
 * The Original Code is GR_AniGEffetcts
 *
 * The Initial Developer of the Original Code is Riceball LEE
 * Portions created by Riceball LEE are Copyright (C) 2004-2007
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_AniGEffetcts;

interface

{$define Debug}

uses
  {$ifdef Debug}
  DbugIntf,
  {$endif} 
  Windows, Messages
  , SyncObjs
  , Classes, SysUtils
  , Graphics, Controls
  , Forms
  //, Dialogs
  , GR32
  , GR32_Image
  , GR_DesktopControl
  , GR_AniEffects
  , SimpleTimer
  ;  

type
  TGRAnimationEffects = class(TGRCustomAnimationEffects)
  private
    FReqPaintTime: Longint;
    FInterlock: TCriticalSection;
    FBuffer: TBitmap32;
    FTempBuffer: TBitmap32; //for TCustomControl, TGraphicControl and TCustomForm
  protected
    FTimer: TSimpleTimer;
    FWinStyle: LongInt;
    procedure DoControlResize(Sender: TObject); override;
    procedure DoPaint;
    procedure DoWMPaint(var Message: TWMPaint); override;
    function GetControlDC(aControl: TControl): HDC; virtual;
    procedure HookControl(Value: TControl; Hooked: Boolean); override;
    procedure InternalDoTimer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure RequirePaint;
    //procedure BGRePaint;
  end;
  
implementation

type
  TControlAccess = Class(TControl);
  TCustomControlAccess = Class(TCustomControl);
  TGraphicControlAccess = Class(TGraphicControl);

constructor TGRAnimationEffects.Create;
begin
  inherited Create;
  FBuffer := TBitmap32.Create;
  FInterlock := TCriticalSection.Create;
end;

destructor TGRAnimationEffects.Destroy;
begin
  FreeAndNil(FInterlock);
  FreeAndNil(FBuffer);
  FreeAndNil(FTempBuffer);
  inherited Destroy;
end;

procedure TGRAnimationEffects.DoControlResize(Sender: TObject);
begin
	//if Sender is TCustomPaintBox32 then
  //with TCustomPaintBox32(Sender) do
  try
    FDrawing := True;
    //if not Assigned(FBuffer) then FBuffer := TBitmap32.Create;
    //Resize;
    //Repaint;
    //FBuffer.Assign(Buffer);
    FBuffer.Delete;
    FControl.Repaint;
  finally
    FDrawing := False;
  end;
  //}
  inherited DoControlResize(Sender);
end;

procedure TGRAnimationEffects.DoPaint;
var
  DC: HDC;
begin
  FInterlock.Enter;
 try
  DC := GetControlDC(FControl);
  {$ifdef Debug}
  //SendDebug('Paint.DC=' + IntToStr(DC));
  {$endif}
  
  if DC <> 0 then
  try
  //FControl.Invalidate;
  //FControl.Update;
  //FControl.Refresh;
   if FBuffer.Empty or (FReqPaintTime >= GetTickCount()) then
   begin
      FReqPaintTime := 0;
      FBuffer.SetSize(FControl.Width, FControl.Height);
      //FControl.Repaint;
      if FControl is TGRDesktopControl then
      begin
        //RedrawWindow(GetDesktopWindow(), nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or  RDW_ALLCHILDREN);
        //InvalidateRect(GetDesktopWindow(), nil, true);
        InvalidateRect(0, nil, true);
        //UpdateWindow(0);
        Sleep(1000);
        //UpdateWindow(GetDesktopWindow());
      end;
      BitBlt(FBuffer.Handle, 0, 0, FBuffer.Width, FBuffer.Height, DC, 0, 0, SRCCOPY);
      FBuffer.ResetAlpha;
      //FBuffer.SaveToFile(ExtractFilePath(ParamStr(0))+'bg.jpg');
    end;
  
    //FillRect(DC, FControl.ClientRect, $FFFFFFFF);
    //DoControlPaint(FControl, DC);
    FTempBuffer.Assign(FBuffer);
   	DoControlPaint(FTempBuffer);
    BitBlt(DC, 0, 0, FControl.Width, FControl.Height, FTempBuffer.Handle, 0, 0, SRCCOPY);

    {$ifdef Debug}
    //TCustomControlAccess(FControl).Canvas.TextOut(0,0,'dddffdf');
    {$endif}
  finally
    //ReleaseDC(DC);
  end;
  
  ///InvalidateRect(DC, nil, true);
 finally  
   FInterlock.Leave;
 end;
end;
{
procedure TGRAnimationEffects.BGRePaint;
var
  DC: HDC;
begin
  FInterlock.Enter;
 try
  DC := GetControlDC(FControl);
  if (DC <> 0) then
  try
    FDrawing := True;
    FBuffer.SetSize(FControl.Width, FControl.Height);
    if FControl is TGRDesktopControl then
    begin
      //RedrawWindow(GetDesktopWindow(), nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or  RDW_ALLCHILDREN);
      //InvalidateRect(GetDesktopWindow(), nil, true);
      InvalidateRect(0, nil, true);
      UpdateWindow(0);
      //UpdateWindow(GetDesktopWindow());
    end;
    BitBlt(FBuffer.Handle, 0, 0, FBuffer.Width, FBuffer.Height, DC, 0, 0, SRCCOPY);
    FBuffer.ResetAlpha;
    //FBuffer.SaveToFile(ExtractFilePath(ParamStr(0))+'bg.jpg');
  finally
    FDrawing := False;
  end;
 finally  
   FInterlock.Leave;
 end;
end;
}
procedure TGRAnimationEffects.DoWMPaint(var Message: TWMPaint);
begin
  //BGRePaint;


  //}
  //DoPaint;
end;

function TGRAnimationEffects.GetControlDC(aControl: TControl): HDC;
begin
  if aControl is TCustomControl then
  begin
    Result := TCustomControlAccess(aControl).Canvas.Handle;
  end
  else if aControl is TGraphicControl then
    Result := TGraphicControlAccess(aControl).Canvas.Handle
  else if aControl is TCustomForm then
    Result := TCustomForm(aControl).Canvas.Handle
  else if aControl is TGRDesktopControl then 
    Result := TGRDesktopControl(aControl).Canvas.Handle
  else Result := 0;
end;

procedure TGRAnimationEffects.HookControl(Value: TControl; Hooked: Boolean);
var
  LStyle: LongInt;
begin
	if not Assigned(Value) then exit;
  if Hooked then
  begin
    FTimer := TSimpleTimer.CreateEx(cMinIntervalValue, DoTimer);
    if not (Value is TCustomPaintBox32) then FTempBuffer := TBitmap32.Create;

    if (Value is TWinControl) then
    begin
      FWinStyle := GetWindowLong(TWinControl(Value).Handle, GWl_Style);
      LStyle := FWinStyle and (not WS_ClipChildren);
      SetWindowLong(TWinControl(Value).Handle, GWl_Style, LStyle);
    end;
  end
  else begin
    FreeAndNil(FTimer);
    FreeAndNil(FBuffer);
    if not (Value is TCustomPaintBox32) then FreeAndNil(FTempBuffer);
    if (Value is TWinControl) then
    begin
      SetWindowLong(TWinControl(Value).Handle, GWl_Style, FWinStyle);
      TWinControl(Value).Repaint;
    end;
  end;
  inherited HookControl(Value, Hooked);
  if Hooked then
  begin
    FTimer.Enabled := True;
  end;
end;

procedure TGRAnimationEffects.InternalDoTimer;
begin
  //InvalidateRect(TCustomControl(FControl).Handle, nil, false);
  //FControl.Update;
  
  //FControl.Invalidate;
  //FControl.Update;
  //FControl.Repaint;

  if not FDrawing then
  begin
    if FControl is TCustomPaintBox32 then with FControl as TCustomPaintBox32 do
    begin
    	//if FControl is TCustomImage32 then TCustomImage32(FControl).BeginUpdate;
    	//try
    	if not Assigned(FBuffer) or FBuffer.Empty then 
    	begin
    	  if not Assigned(FBuffer) then FBuffer := TBitmap32.Create;
    	  FBuffer.Assign(Buffer);
    	end
    	else
    	  Buffer.Assign(FBuffer);
    	DoControlPaint(Buffer);
    	Flush;
    	//finally
    	  //if FControl is TCustomImage32 then TCustomImage32(FControl).EndUpdate;
    	//end;
   	end
   	else
   	begin
      //FControl.Repaint;
      DoPaint;
    end;
  end;
  inherited InternalDoTimer;
end;

procedure TGRAnimationEffects.RequirePaint;
begin
  FInterlock.Enter;
  try
    FReqPaintTime := GetTickCount() + 1000;
  finally  
    FInterlock.Leave;
  end;
end;


Initialization
end.
