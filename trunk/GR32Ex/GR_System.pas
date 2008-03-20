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
 * The Original Code is GR_System
 *
 * The Initial Developer of the Original Code is Riceball LEE
 * Portions created by Riceball LEE are Copyright (C) 2004-2007
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_System;

interface

{$I Setting.inc}

uses
  Windows,
  SysUtils, Classes
  {$IFDEF CLX}
  Qt, Types {$IFDEF LINUX}, Libc {$ENDIF}
  {$ELSE}
  {$ENDIF};

type

  {: the abstract buffer collection item }
  TGRBufferItem = class(TCollectionItem)
  protected
    {: keep it in the memory if > 0 even through the Cached property is not be set }
    FInUse: Integer;
    FCached: Boolean;
    FName: string;
    FURL: string;

    function GetDisplayName: string; override;
    procedure LoadBuffer; virtual;abstract;
    procedure ReleaseBuffer; virtual;abstract;
    function DoURLChanged(const aURL: string): Boolean; virtual;
    procedure SetURL(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    procedure Use;
    procedure UnUse;

    property Name: string read FName write FName;
    { Summary the URL address of the Item. }
    { http://
      file://
      res://
      pack://
    }
    property URL: string read FURL write SetURL;
    property InUse: Integer read FInUse;
    { Summary: whether cache this picture in the memory }
    property Cached: Boolean read FCached write FCached;
  end;

  //like the record do not call constructor to create a instance.
  //faster than class.
  TGRCounter = object
  private
    FStartValue, FStopValue: Int64;
    function GetElapsed: Extended;
    function GetCalibrate: Int64;
    function GetFrequency: Int64;
  protected
  public
    procedure Start;
    procedure Stop;
    function ReadMilliseconds: String;
    //the Elapsed TickCount value
    function ReadValue: Int64;

    //return the ns .
    property Elapsed: Extended read GetElapsed;
    property Calibrate: Int64 read GetCalibrate;
    property Frequency: Int64 read GetFrequency;
  end;


function StartCounter: TGRCounter;
function StopCounter(const aCounter: TGRCounter): Single;

implementation


var
  FCalibrate: Int64;
  FFrequency: Int64;
  {$IFDEF LINUX}
  FCalibrateUSleep: Int64;
  {$ENDIF}

function RDTSC: Int64;
asm
  dw 310Fh
end;

function GetFrequency: Int64;
begin
{$IFDEF LINUX}
  MeasureFrequency;
{$ENDIF}
{$IFDEF MSWINDOWS}
  QueryPerformanceFrequency(FFrequency);
{$ENDIF}
  Result:= FFrequency;
  {on LINUX this is the real Frequency, but not on Windows}
end;

{$IFDEF LINUX}
procedure CalibrateLinux;
var
  StartValue, StopValue: Int64;
  Val1, Val2, Val3, Val4, Val5: Int64;
begin
  USleep(1);

  StartValue:= RDTSC;
  StopValue:=  RDTSC;
  Val1:= (StopValue - StartValue);

  StartValue:= RDTSC;
  StopValue:=  RDTSC;
  Val2:= (StopValue - StartValue);

  StartValue:= RDTSC;
  StopValue:=  RDTSC;
  Val3:= (StopValue - StartValue);

  StartValue:= RDTSC;
  StopValue:=  RDTSC;
  Val4:= (StopValue - StartValue);

  StartValue:= RDTSC;
  StopValue:=  RDTSC;
  Val5:= (StopValue - StartValue);

  FCalibrate:=  (Val1 + Val2 + Val3 + Val4 + Val5) div 5;
end;

procedure CalibrateUSleep;
var
  StartValue, StopValue: Int64;
  Val1, Val2, Val3, Val4, Val5: Int64;
begin
  USleep(1);

  StartValue:= RDTSC;
  USleep(0);
  StopValue:=  RDTSC;
  Val1:= (StopValue - StartValue);

  StartValue:= RDTSC;
  USleep(0);
  StopValue:=  RDTSC;
  Val2:= (StopValue - StartValue);

  StartValue:= RDTSC;
  USleep(0);
  StopValue:=  RDTSC;
  Val3:= (StopValue - StartValue);

  StartValue:= RDTSC;
  USleep(0);
  StopValue:=  RDTSC;
  Val4:= (StopValue - StartValue);

  StartValue:= RDTSC;
  USleep(0);
  StopValue:=  RDTSC;
  Val5:= (StopValue - StartValue);

  FCalibrateUSleep := (Val1 + Val2 + Val3 + Val4 + Val5)div 5;
end;

procedure  MeasureFrequency;
var
  StartValue, StopValue: Int64;
  Val1, Val2, Val3, Val4, Val5: Int64;
begin
  CalibrateUSleep;

  StartValue:= RDTSC;
  USleep(1);
  StopValue:=  RDTSC;
  Val1:= (StopValue - StartValue -FCalibrateUSleep);

  StartValue:= RDTSC;
  USleep(1);
  StopValue:=  RDTSC;
  Val2:= (StopValue - StartValue -FCalibrateUSleep);

  StartValue:= RDTSC;
  USleep(1);
  StopValue:=  RDTSC;
  Val3:= (StopValue - StartValue -FCalibrateUSleep);

  StartValue:= RDTSC;
  USleep(1);
  StopValue:=  RDTSC;
  Val4:= (StopValue - StartValue -FCalibrateUSleep);

  StartValue:= RDTSC;
  USleep(1);
  StopValue:=  RDTSC;
  Val5:= (StopValue - StartValue -FCalibrateUSleep);

  FFrequency := (Val1 + Val2 + Val3 + Val4 + Val5) div 5 * 100;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure CalibrateWindows;
var
  StartValue, StopValue: Int64;
begin
  QueryPerformanceCounter(StartValue);
  QueryPerformanceCounter(StopValue);
  FCalibrate:= StopValue - StartValue;
end;
{$ENDIF}

{ TGRBufferItem }
procedure TGRBufferItem.Assign(Source: TPersistent);
begin
  if Source is TGRBufferItem then
  begin
    Name := TGRBufferItem(Source).Name;
    Cached := TGRBufferItem(Source).Cached;
  end
  else
    inherited;
end;

function TGRBufferItem.DoURLChanged(const aURL: string): Boolean;
begin
  Result := True;
end;

function TGRBufferItem.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TGRBufferItem.SetURL(const Value: string);
begin
  if FURL <> Value then
  begin
    if DoURLChanged(Value) then
      FURL := Value;
  end;
end;

procedure TGRBufferItem.UnUse;
begin
  if not FCached then
    if (InterlockedDecrement(FInUse) <= 0) then
    begin
      ReleaseBuffer;
    end;
end;

procedure TGRBufferItem.Use;
begin
  if not FCached then
  begin
    if (FInUse <= 0) then
    begin
      if (InterlockedIncrement(FInUse) <= 0) then
        InterlockedExchange(FInUse, 1);
      LoadBuffer;
    end;
  end;
end;

{ TGRCounter }
function TGRCounter.GetElapsed: Extended;
begin
  Result := (FStopValue - FStartValue - FCalibrate) / FFrequency;
end;

function TGRCounter.GetFrequency: Int64;
begin
  Result := FFrequency;
end;

function TGRCounter.GetCalibrate: Int64;
begin
  Result := FCalibrate;
end;

function TGRCounter.ReadMilliseconds: String;
begin
  Result := FloatToStr(Round(1000000 * Elapsed) / 1000); 
end;

function TGRCounter.ReadValue: Int64;
begin
  Result := FStopValue - FStartValue - FCalibrate; 
end;

procedure TGRCounter.Start;
begin
{$IFDEF LINUX}
  FStartValue:= RDTSC;
{$ENDIF}
{$IFDEF MSWINDOWS}
  QueryPerformanceCounter(FStartValue);
{$ENDIF}
end;

procedure TGRCounter.Stop;
begin
{$IFDEF LINUX}
  FStopValue:= RDTSC;
{$ENDIF}
{$IFDEF MSWINDOWS}
  QueryPerformanceCounter(FStopValue);
{$ENDIF}
end;

{ Timer Routines ==============================================================}

function StartCounter: TGRCounter;
begin
  Result.Start;
end;

function StopCounter(const aCounter: TGRCounter): Single;
begin
  //Result := 0.0;
  aCounter.Stop;
  Result := aCounter.Elapsed;
end;

initialization
  {$IFDEF LINUX}
  CalibrateLinux;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  CalibrateWindows;
  {$ENDIF}
  GetFrequency;
end.
