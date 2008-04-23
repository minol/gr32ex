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
 * The Original Code is GR_AnimationGif
 *
 * The Initial Developer of the Original Code is Michael Faust
 * Portions created by Michael Faust - http://www.alpha-interactive.de/ are Copyright (C) 2000-2005
 * Portions created by Riceball LEE are Copyright (C) 2008
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_AnimationGif;

interface

uses
     Windows, Messages, Classes, Graphics, Controls
     , GR32
     , GR_Animation
     , GifImage
     ;


type
  TGRAnimationGif = class(TGRAnimation)
  private
  protected
    function GetFrameDelay(const FrameIndex: Integer; const SafeMode: Boolean=True): Integer;override;
  public
    procedure LoadFromStream(const S: TStream); override;
    procedure SaveToStream(const aStream: TStream);override;
  published
  end;

implementation

{ TGRAnimationGif }
function TGRAnimationGif.GetFrameDelay(const FrameIndex: Integer; const SafeMode: Boolean=True): Integer;
begin
  Result := 0;
  if IndexIsValid(FrameIndex) then
    Result := FFrames.Items[FrameIndex].DelayTime;
  if SafeMode then
  begin
    if (Result > GIFMaximumDelay) then
      Result := GIFMaximumDelay
    else if (Result < GIFMinimumDelay) then
    begin
     if (Result = 0) then
       Result := GIFDefaultDelay
     else
       Result := GIFMinimumDelay;
    end;
  end;
  // this is called "cheating" ... hehehe.
  Result := Result * 10 + 4; 
end;

procedure TGRAnimationGif.LoadFromStream(const S: TStream); 
var
  I, TotalCount: Integer;
  AGif: TGifImage;
  SubGif: TGifSubImage;
  FrameItem: TGRAnimationFrame;
  ACanvas: TCanvas;
  bgClr: TColor;
  doClearBackupAfter,
  doClearBackupFirst,
  doCopyLastFrame,
  doGetBackup: Boolean;
  newDisposal,
  oldDisposal : TDisposalMethod;
  Bmp32Backup,
  Bmp32Frame: TBitmap32;
begin
  FFrames.Clear;
  AGif := TGifImage.Create;
  Bmp32Backup := TBitmap32.Create;
  Bmp32Frame := TBitmap32.Create;
  try
    OldDisposal := dmNoDisposal;
    
    AGif.LoadFromStream(S);
    Bmp32Frame.SetSize(AGif.Width, AGif.Height);
    //Bmp32Frame.ResamplerClassName := Self.OriginalFilter;
    Bmp32Backup.SetSize(AGif.Width, AGif.Height);
    //Bmp32Backup.ResamplerClassName := Self.OriginalFilter;

    TotalCount := AGif.Images.Count;

    bgClr := BackgroundColor;

    for I := 0 to TotalCount -1 do
    begin
      bgClr := BackgroundColor;      
      //PerformFrameLoad(Self, I, TotalCount, bgClr);

      SubGif := AGif.Images.SubImages[I];
      if (not Assigned(SubGif)) or (SubGif.Empty) then
        Continue; // ignore empty frames  

 //     if (I = 0) then
       begin
        Bmp32Frame.Clear(Color32(bgClr));
        if (I = 0) then
         Bmp32Backup.Clear(Color32(bgClr));
       end;

      if Assigned(SubGif.GraphicControlExtension) then
       newDisposal := SubGif.GraphicControlExtension.Disposal
      else
       newDisposal := dmNoDisposal;

      ACanvas := TCanvas.Create;
      try
       doGetBackup        := false;      
       doClearBackupAfter := false;
       doClearBackupFirst := false;
       doCopyLastFrame    := false;
       
       case newDisposal of
        dmNone,
        dmNoDisposal:
         begin
          doGetBackup := true;
          ACanvas.Handle := Bmp32Backup.Handle;
          doCopyLastFrame  := true;
         end;
        dmBackground:
         begin
          ACanvas.Handle := Bmp32Frame.Handle;
          doClearBackupAfter := true;
          doCopyLastFrame  := true;
         end;
        dmPrevious:
         begin
          doGetBackup := true;
          ACanvas.Handle := Bmp32Backup.Handle;
          case oldDisposal of
           dmNone, dmNoDisposal:
            begin
             ACanvas.Handle := Bmp32Backup.Handle;
            end;
           dmBackground, dmPrevious:
            begin
             ACanvas.Handle := Bmp32Backup.Handle;
             doClearBackupFirst := True;
             doCopyLastFrame := True;
            end;
          end;
         end;
       end;

       OldDisposal := newDisposal;

       // Already a frame loaded.
        if (I > 0) then
         begin
          if doClearBackupFirst then
           Bmp32Backup.Clear(Color32(bgClr));
          if doCopyLastFrame then
           Bmp32Frame.Assign(Bmp32Backup);

          SubGif.Draw(ACanvas, Rect(0, 0, AGif.Width, AGif.Height), true, false);
          
          if doClearBackupAfter then
           Bmp32Backup.Clear(Color32(bgClr))
         end else begin
          SubGif.Draw(ACanvas, Rect(0, 0, AGif.Width, AGif.Height), true, false);
         end;
      finally
       ACanvas.Free;
      end;  

      FrameItem := FFrames.Add;
      if Assigned(SubGif.GraphicControlExtension) then
       FrameItem.DelayTime := SubGif.GraphicControlExtension.Delay
      else
       FrameItem.DelayTime := GIFDefaultDelay;
     // Backgroundcolor for transperancy
      FrameItem.BackgroundColor := bgClr;
     // the stretch filter has to be assigned to each single bitmap32
     // FrameItem.Bitmap.StretchFilter := Self.OriginalFilter;
      if (doGetBackup) then
       begin
        FrameItem.Bitmap.Assign(Bmp32Backup);
       // Bmp32Frame.Assign(Bmp32Backup);
       end else begin
        FrameItem.Bitmap.Assign(Bmp32Frame);
        if not (doClearBackupAfter or doClearBackupFirst) then
         Bmp32Backup.Assign(Bmp32Frame);
       end;       
    end;
  finally
    AGif.Free;
    Bmp32Backup.Free;
    Bmp32Frame.Free;
  end;
                         
 RequestFlipAlphaChannel;

end;

procedure TGRAnimationGif.SaveToStream(const aStream: TStream);
begin
end;

Initialization
  RegisterAnimation('gif', '', TGRAnimationGif);
end.
