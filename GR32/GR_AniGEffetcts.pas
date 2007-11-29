{ Description
the Genernal Animation Effects Engine.
Supports the TCustomControl, TGraphicControl and TCustomForm
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
  , GR_AniEffects
  , SimpleTimer
  ;  

type
  TGRAnimationEffects = class(TGRCustomAnimationEffects)
  private
    FBuffer: TBitmap32;
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
  //FreeAndNil(FBuffer);
  inherited Destroy;
end;

procedure TGRAnimationEffects.DoControlResize(Sender: TObject);
var
  DC: HDC;
begin
  {FDrawing := True;
  with TControl(Sender) do
  try
    DC := GetControlDC(FControl);
    if DC <> 0 then
    begin
      FBuffer.SetSize(Width, Height);
      Repaint;
      BitBlt(FBuffer.Handle, 0, 0, FBuffer.Width, FBuffer.Height, DC, 0, 0, SRCCOPY);
    end;
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
  begin
  //FControl.Invalidate;
  //FControl.Update;
  //FControl.Refresh;
    //if not FBuffer.Empty then
      //BitBlt(DC, 0, 0, FBuffer.Width, FBuffer.Height, FBuffer.Handle, 0, 0, SRCCOPY);
  
    DoControlPaint(FControl, DC);
    {$ifdef Debug}
    //TCustomControlAccess(FControl).Canvas.TextOut(0,0,'dddffdf');
    {$endif}
  end;
  
  
  ///InvalidateRect(DC, nil, true);
end;

procedure TGRAnimationEffects.DoWMPaint(var Message: TWMPaint);
var
  DC: HDC;
begin
  {
  DC := GetControlDC(FControl);
  if (DC <> 0) then
  begin
    FBuffer.SetSize(FControl.Width, FControl.Height);
    BitBlt(FBuffer.Handle, 0, 0, FBuffer.Width, FBuffer.Height, DC, 0, 0, SRCCOPY);
  end;
  
  
  
  //}
  DoPaint;
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
    if (Value is TWinControl) then
    begin
      SetWindowLong(TWinControl(Value).Handle, GWl_Style, FWinStyle);
      TWinControl(Value).Repaint;
    end;
  end;
  inherited HookControl(Value, Hooked);
  if Hooked then
    FTimer.Enabled := True;
end;

procedure TGRAnimationEffects.InternalDoTimer;
begin
  //InvalidateRect(TCustomControl(FControl).Handle, nil, false);
  //FControl.Update;
  
  FControl.Invalidate;
  FControl.Update;
  inherited InternalDoTimer;
  DoPaint;
end;


end.
