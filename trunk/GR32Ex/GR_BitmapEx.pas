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
 * The Original Code is GR_BitmapEx
 *
 * The Initial Developer of the Original Code is Riceball LEE
 * Portions created by Riceball LEE are Copyright (C) 2004-2007
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_BitmapEx;

{$I Setting.inc}

interface

uses
  {$ifdef Debug}
  DbugIntf,
  {$endif} 
  Windows,
  SysUtils, Classes
  , Graphics
  , GR32
  , GR32_Transforms
  , GR32_Filters
  , GR_Graphics
  , GR_FilterEx
  , GR_GraphUtils
  //, PNGImage
  //, GR32_PNG
  ;

type
  TBitmap32Ex = class(TBitmap32)
  private
    function GetFont: TFont32;
    procedure SetFont(Value: TFont32);
  public
    class function CreateFont: TFont; override;
    procedure DrawGraphic(Graphic: TGraphic; DstRect: TRect);
    procedure DrawText(Text: string; var aRect: TRect; aFormat: LongWord);
    procedure DrawTextW(Text: widestring; var aRect: TRect; aFormat: LongWord);
    procedure RenderText(X, Y: Integer; const Text: String); overload;
    procedure RenderTextW(X, Y: Integer; const Text: WideString); overload;
    property Font: TFont32 read GetFont write SetFont;
  end;
  

implementation

class function TBitmap32Ex.CreateFont: TFont;
begin
  Result := TFont32.Create;
end;

procedure TBitmap32Ex.DrawGraphic(Graphic: TGraphic; DstRect: TRect);
var
  LBitmap32: TBItmap32;
begin
  LBitmap32 := TBitmap32.Create;
  try
    LBitmap32.Assign(Graphic);
    LBitmap32.DrawTo(Self, DstRect);
  finally
    LBitmap32.Free;
  end;
end;

procedure TBitmap32Ex.DrawText(Text: string; var aRect: TRect; aFormat:
  LongWord);
begin
  {$ifdef CLX}
  DrawTextW(Text, aRect, aFormat); // QT does Unicode
  {$else}
    Font.DrawText(Self, Text, aRect, aFormat);
  {$endif}
end;

procedure TBitmap32Ex.DrawTextW(Text: widestring; var aRect: TRect; aFormat:
  LongWord);
begin
  Font.DrawText(Self, Text, aRect, aFormat);
end;

function TBitmap32Ex.GetFont: TFont32;
begin
  Result := TFont32(inherited Font);
end;

procedure TBitmap32Ex.RenderText(X, Y: Integer; const Text: String);
begin
  Font.RenderText(Self, X, Y, Text);
  {with Font do
    if Background.Enabled and not Background.Empty then
    begin
      vTexture := nil;
      vTextBMP := nil;
      try
        vTexture := TBitmap32.Create;
        vTextBMP := TBitmap32Ex.Create;
        PaddedText := Text + ' ';
        //if Assigned(TBitmap32Ex(vTextBMP).Font.Background) then
          vTextBMP.Font := Font;
        Sz := vTextBMP.TextExtent(PaddedText);
        vTextBMP.SetSize(Sz.cx, Sz.cy);
        vTexture.SetSize(Sz.cx, Sz.cy);
        vTexture.DrawMode := dmBlend;
        vTexture.CombineMode := cmMerge;
        Background.PaintTo(vTexture, vTexture.BoundsRect);
        vTextBMP.Clear(clBlack32);
        vColor.Color := clWhite32;
        //vColor.rgbAlpha := $FF;
        vTextBMP.RenderText(0, 0, Text, Integer(Quality), vColor.Color);
        if Outline then
        begin
          ConvolveI(LaplaceFilter3x3, vTextBMP);
        end;
        BlueChannelToAlpha(vTexture, vTextBMP);
        vTexture.DrawTo(Self, X, Y);
      finally
        FreeAndNil(vTexture);
        FreeAndNil(vTextBMP);
      end;
    end
    else begin
      vColor.Color := Color32(Color);
      vColor.rgbAlpha := Alpha;
      Self.RenderText(X, Y, Text, Integer(Quality), vColor.Color);
    end;
  //}
end;

procedure TBitmap32Ex.RenderTextW(X, Y: Integer; const Text: WideString);
begin
  Font.RenderTextW(Self, X, Y, Text);
  
  {with Font do
    if Background.Enabled and not Background.Empty then
    begin
      vTexture := nil;
      vTextBMP := nil;
      try
        vTexture := TBitmap32.Create;
        vTextBMP := TBitmap32Ex.Create;
        PaddedText := Text + ' ';
        //if Assigned(TBitmap32Ex(vTextBMP).Font.Background) then
          vTextBMP.Font := Font;
        Sz := vTextBMP.TextExtentW(PaddedText);
        vTextBMP.SetSize(Sz.cx, Sz.cy);
        vTexture.SetSize(Sz.cx, Sz.cy);
        vTexture.DrawMode := dmBlend;
        vTexture.CombineMode := cmMerge;
        Background.PaintTo(vTexture, vTexture.BoundsRect);
        vTextBMP.Clear(clBlack32);
        vColor.Color := clWhite32;
        //vColor.rgbAlpha := $FF;
        vTextBMP.RenderTextW(0, 0, Text, Integer(Quality), vColor.Color);
        if Outline then
        begin
          ConvolveI(LaplaceFilter3x3, vTextBMP);
        end;
        BlueChannelToAlpha(vTexture, vTextBMP);
        vTexture.DrawTo(Self, X, Y);
      finally
        FreeAndNil(vTexture);
        FreeAndNil(vTextBMP);
      end;
    end
    else begin
      vColor.Color := Color32(Color);
      vColor.rgbAlpha := Alpha;
      Self.RenderTextW(X, Y, Text, Integer(Quality), vColor.Color);
    end;
  //}
end;

procedure TBitmap32Ex.SetFont(Value: TFont32);
begin
  inherited Font.Assign(Value);
end;


end.
