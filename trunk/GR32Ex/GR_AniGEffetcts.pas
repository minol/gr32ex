{ Description
the Genernal Animation Effects Engine.
Supports the TCustomPaintBox32, TCustomControl, TGraphicControl and TCustomForm
  Note: it will flick on the TCustomControl, TGraphicControl and TCustomForm.
}
unit GR_AniGEffetcts;

interface

{$define Debug}

uses
  {$ifdef Debug}
  DbugIntf,
  {$endif} 
  Windows, Messages
  , Classes, SysUtils
  , Graphics, Controls
  , Forms
  //, Dialogs
  , GR32
  , GR32_Image
  , GR_AniEffects
  , SimpleTimer
  ;  

type
  TGRAnimationEffects = class(TGRCustomAnimationEffects)
  private
    FBuffer: TBitmap32; //for the Bitmap32 only
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
  end;
  
implementation

type
  TControlAccess = Class(TControl);
  TCustomControlAccess = Class(TCustomControl);
  TGraphicControlAccess = Class(TGraphicControl);

constructor TGRAnimationEffects.Create;
begin
  inherited Create;
  //FBuffer := TBitmap32.Create;
end;

destructor TGRAnimationEffects.Destroy;
begin
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

procedure TGRAnimationEffects.DoControlResize(Sender: TObject);
begin
	if Sender is TCustomPaintBox32 then
  with TCustomPaintBox32(Sender) do
  try
    FDrawing := True;
    if not Assigned(FBuffer) then FBuffer := TBitmap32.Create;
    //Resize;
    //Repaint;
    //FBuffer.Assign(Buffer);
    FBuffer.Delete;
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
  DC := GetControlDC(FControl);
  {$ifdef Debug}
  //SendDebug('Paint.DC=' + IntToStr(DC));
  {$endif}
  
  if DC <> 0 then
  try
  //FControl.Invalidate;
  //FControl.Update;
  //FControl.Refresh;
   //if not FBuffer.Empty then
      //BitBlt(DC, 0, 0, FBuffer.Width, FBuffer.Height, FBuffer.Handle, 0, 0, SRCCOPY);
  
    //FillRect(DC, FControl.ClientRect, $FFFFFFFF);
    DoControlPaint(FControl, DC);
    {$ifdef Debug}
    //TCustomControlAccess(FControl).Canvas.TextOut(0,0,'dddffdf');
    {$endif}
  finally
    //ReleaseDC(DC);
  end;
  
  
  ///InvalidateRect(DC, nil, true);
end;

procedure TGRAnimationEffects.DoWMPaint(var Message: TWMPaint);
//var
  //DC: HDC;
begin
  {
  DC := GetControlDC(FControl);
  if (DC <> 0) then
  begin
    FBuffer.SetSize(FControl.Width, FControl.Height);
    BitBlt(FBuffer.Handle, 0, 0, FBuffer.Width, FBuffer.Height, DC, 0, 0, SRCCOPY);
  end;
  
  
  
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
  else if aCOntrol is TCustomForm then
    Result := TCustomForm(aControl).Canvas.Handle
  else Result := 0;
end;

procedure TGRAnimationEffects.HookControl(Value: TControl; Hooked: Boolean);
var
  LStyle: LongInt;
begin
  if Hooked then
  begin
    FTimer := TSimpleTimer.CreateEx(cMinIntervalValue, DoTimer);
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
  inherited InternalDoTimer;
  if FDrawing then exit;
  if FControl is TCustomPaintBox32 then with FControl as TCustomPaintBox32 do
  begin
  	if not Assigned(FBuffer) or FBuffer.Empty then 
  	begin
  	  if not Assigned(FBuffer) then FBuffer := TBitmap32.Create;
  	  FBuffer.Assign(Buffer);
  	end
  	else
  	  Buffer.Assign(FBuffer);
  	DoControlPaint(Buffer);
  	Flush;
 	end
 	else
 	begin
    FControl.Repaint;
    DoPaint;
  end;
end;


end.
