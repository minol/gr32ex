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
 * The Original Code is GR_DesktopControl
 *
 * The Initial Developer of the Original Code is
 * Riceball LEE
 *
 * Portions created by Riceball LEE are Copyright (C) 2007
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_DesktopControl;

interface

{.$Define DEBUG}

uses
  {$IFDEF DEBUG}
  //DbugIntf,
  CnDebug,
  {$ENDIF}
  Windows, Messages, Classes, Graphics, Controls
  , SysUtils
  //, GR32
  //, GR32_Math
  ;

type
   TGRDesktopCanvas = class(TCanvas)
   private
     DC : HDC;
     function GetWidth:Integer;
     function GetHeight:Integer;
   public
     constructor Create;
     destructor Destroy; override;
   published
     property Width: Integer read GetWidth;
     property Height: Integer read GetHeight;
   end;

  TGRDesktopControl = class(TControl)
  private
    FCanvas: TGRDesktopCanvas;
  protected
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Canvas: TGRDesktopCanvas read FCanvas;
  end;
  

implementation

{ TGRDesktopCanvas }
function TGRDesktopCanvas.GetWidth:Integer;
begin
   Result:=GetDeviceCaps(Handle,HORZRES) ;
end;

function TGRDesktopCanvas.GetHeight:Integer;
begin
   Result:=GetDeviceCaps(Handle,VERTRES) ;
end;

constructor TGRDesktopCanvas.Create;
begin
   inherited Create;
   DC := GetDC(0) ;
   Handle := DC;
end;

destructor TGRDesktopCanvas.Destroy;
begin
   Handle := 0;
   ReleaseDC(0, DC) ;
   inherited Destroy;
end;

{ TGRDesktopControl }
constructor TGRDesktopControl.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TGRDesktopCanvas.Create;
  SetBounds(0,0,Canvas.Width, Canvas.Height);
end;

destructor TGRDesktopControl.Destroy; 
begin
	FreeAndNil(FCanvas);
  inherited;
end;

function TGRDesktopControl.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := False;
end;

function TGRDesktopControl.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
	Result := (NewWidth = Canvas.Width) and (NewHeight = Canvas.Height);
end;

procedure TGRDesktopControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
	inherited SetBounds(0,0,Canvas.Width, Canvas.Height);
end;

end.
