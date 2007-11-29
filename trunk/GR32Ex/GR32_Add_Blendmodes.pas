unit GR32_Add_BlendModes;
{

 This unit is distributed under same conditions as GR32.
 Version 0.2

 -------------

 Last changes:

  Added more blendmodes.
  Corrected and optimized a few.
  Added some utilily functions.
  Added the $Q- $R- directives.
  Added version number ;)

 To do:
  OPTIMIZE Optimize optimize ...
  Get rid of HSL callbacks ...
  Correct HSL modes more in direction of the Photoshop Blendmodes.
  Add some plugin feats??

 -------------
 This unit adds most popular blendmodes + some new ones to GR32.
 Some are already available in other GR32 units, but are for the
 completeness listed here.

 Some blendmodes have been created from the descriptions Adobe Photoshop
 gives in the help file.
 Others come from Jens Gruschel & Francesco Savastano(and others) from
 various newsgroup discussions...

 I provide the Reflection, Custom & Bright Light modes.
 The custom mode is a 0..255,0..255 LUT where you can load your
 own blendmaps to - The bad thing in this implementation is that
 if several layers uses custom mode with different LUTs, you
 have to take care of the temporary loading yourself.

 For descriptions and other stuff see Jens Gruschels page at:

 http://www.pegtop.net/delphi/blendmodes/

 If you have coded some interesting modes & want them added to this unit,
 pls send me the code along with a description of purpose and use
 (i.e. "Good for adding bright objects" or so).

 If you find any lines or structures that may be optimized or if
 you´re an shark with asm and want to rewrite the procs - please
 contact me - lots of the rewritting is just copy/paste stuff so ... :)

 Michael Hansen.


}
interface
{$Q-}{$R-}

uses
 Sysutils,Classes, GR32;

type
 {Blendmap - lookup table for Custom mode}
 TBlendmap = array [0..65535]of TColor32;

 {Just a wrapper to make procedures compatible - creation of object
  not neccessary, access via blendmode variable}
 TBlendmode = class
     procedure Normal      (F: TColor32; var B: TColor32; M: TColor32);
     procedure Screen      (F: TColor32; var B: TColor32; M: TColor32);
     procedure Lighten     (F: TColor32; var B: TColor32; M: TColor32);
     procedure Darken      (F: TColor32; var B: TColor32; M: TColor32);
     procedure Additive    (F: TColor32; var B: TColor32; M: TColor32);
     procedure Subtractive (F: TColor32; var B: TColor32; M: TColor32);
     procedure Multiply    (F: TColor32; var B: TColor32; M: TColor32);
     procedure Burn        (F: TColor32; var B: TColor32; M: TColor32);
     procedure Dodge       (F: TColor32; var B: TColor32; M: TColor32);
     procedure Reflect     (F: TColor32; var B: TColor32; M: TColor32);
     procedure Glow        (F: TColor32; var B: TColor32; M: TColor32);
     procedure Heat        (F: TColor32; var B: TColor32; M: TColor32);
     procedure Freeze      (F: TColor32; var B: TColor32; M: TColor32);
     procedure Overlay     (F: TColor32; var B: TColor32; M: TColor32);
     procedure Average     (F: TColor32; var B: TColor32; M: TColor32);
     procedure Soft_Light  (F: TColor32; var B: TColor32; M: TColor32);
     procedure Bright_Light(F: TColor32; var B: TColor32; M: TColor32);
     procedure Hard_Light  (F: TColor32; var B: TColor32; M: TColor32);
     procedure Difference  (F: TColor32; var B: TColor32; M: TColor32);
     procedure Exclusion   (F: TColor32; var B: TColor32; M: TColor32);
     procedure Negation    (F: TColor32; var B: TColor32; M: TColor32);
     procedure Red         (F: TColor32; var B: TColor32; M: TColor32);
     procedure Green       (F: TColor32; var B: TColor32; M: TColor32);
     procedure Blue        (F: TColor32; var B: TColor32; M: TColor32);
     procedure Hue         (F: TColor32; var B: TColor32; M: TColor32);
     procedure Saturation  (F: TColor32; var B: TColor32; M: TColor32);
     procedure Lightness   (F: TColor32; var B: TColor32; M: TColor32);
     procedure Custom      (F: TColor32; var B: TColor32; M: TColor32);
    end;

function BlendModeList: TStringList;
function GetBlendMode(index: integer): TPixelCombineEvent;
function GetIndex(Mode: TPixelCombineEvent):integer;
function RenderBlendmap(Blend: TPixelCombineEvent): TBlendmap;
procedure BitmapToBlendmap(Src: TBitmap32; out Dst:TBlendmap);
procedure BlendmapToBitmap(Src: TBlendmap; Dst: TBitmap32);

var
 Blendmode: TBlendMode;
 BlendMap : TBlendMap;

implementation

var
  SqrtTable : array [0..65535]of byte;
  x,y       : integer;
const
  SEmptySource      = 'The source is nil';

procedure FitBitmap(var B: TBitmap32);
var Tmp: TBitmap32;
begin
 Tmp:= TBitmap32.Create;
 Tmp.SetSize(256,256);
 Tmp.Clear;
 Tmp.Draw(Rect(0,0,256,256),Rect(0,0,B.Width,B.Height),B);
 B.Assign(Tmp);
 Tmp.Free;
end;

//---Utils-----------------------------

{Note: this only works for RGB channel processing events - not HSL and such modes}
function RenderBlendmap(Blend: TPixelCombineEvent): TBlendmap;
var x,y,m,B,F: cardinal;
begin
 m:= $FF;
 for x:=0 to 255 do for y:=0 to 255 do
  begin
   F:= Color32(x,x,x);
   B:= Color32(y,y,y);
   Blend(TColor32(F),TColor32(B),TColor32(m));
   result[y + x shl 8]:= B;
  end;
end;

procedure BlendmapToBitmap(Src: TBlendmap; Dst: TBitmap32);
var x,y: cardinal;
begin
 for x:=0 to 255 do for y:=0 to 255 do Dst.Pixel[x,y]:= Src[y + x shl 8];
end;

procedure BitmapToBlendmap(Src: TBitmap32; out Dst:TBlendmap);
var x,y: cardinal;
begin
 if  Src.PixelPtr[0,0]=nil then raise Exception.Create(SEmptySource);
 if (Src.Width<>256)or(Src.Height<>256)then FitBitmap(Src);
 for x:=0 to 255 do for y:=0 to 255 do Dst[y + x shl 8]:= Src.Pixel[x,y];
end;

function BlendModeList: TStringList;
begin
 Result:= TStringList.Create;
 with Result do
 begin
  Add('Normal');
  Add('Multiply');
  Add('Screen');
  Add('Overlay');
  Add('Reflect');
  Add('Soft Light');
  Add('Bright Light');
  Add('Hard Light');
  Add('Color Dodge');
  Add('Color Burn');
  Add('Lighten');
  Add('Darken');
  Add('Additive');
  Add('Subtractive');
  Add('Average');
  Add('Difference');
  Add('Exclusion');
  Add('Negation');
  Add('Glow');
  Add('Heat');
  Add('Freeze');
  Add('Red');
  Add('Green');
  Add('Blue');
  Add('Hue');
  Add('Saturation');
  Add('Lightness');
  Add('Custom BlendMap');
 end;
end;

function GetIndex(Mode: TPixelCombineEvent):integer;
var
 PM,PB: Pointer;
 Tmp  : TPixelCombineEvent;
begin
  {Workaround here.. couldn´t find any other way to do this...}
  PM:= @Mode;
  with BlendMode do
  begin
   Tmp:= Normal;      PB:= @Tmp;
   if PM = PB then begin result:= 0; exit end;
   Tmp:= Multiply;    PB:= @Tmp;
   if PM = PB then begin result:= 1; exit end;
   Tmp:= Screen;      PB:= @Tmp;
   if PM = PB then begin result:= 2; exit end;
   Tmp:= Overlay;     PB:= @Tmp;
   if PM = PB then begin result:= 3; exit end;
   Tmp:= Reflect;     PB:= @Tmp;
   if PM = PB then begin result:= 4; exit end;
   Tmp:= Soft_Light;  PB:= @Tmp;
   if PM = PB then begin result:= 5; exit end;
   Tmp:= Bright_Light;  PB:= @Tmp;
   if PM = PB then begin result:= 6; exit end;
   Tmp:= Hard_Light;  PB:= @Tmp;
   if PM = PB then begin result:= 7; exit end;
   Tmp:= Dodge;       PB:= @Tmp;
   if PM = PB then begin result:= 8; exit end;
   Tmp:= Burn;        PB:= @Tmp;
   if PM = PB then begin result:= 9; exit end;
   Tmp:= Lighten;     PB:= @Tmp;
   if PM = PB then begin result:=10; exit end;
   Tmp:= Darken;      PB:= @Tmp;
   if PM = PB then begin result:=11; exit end;
   Tmp:= Additive;    PB:= @Tmp;
   if PM = PB then begin result:=12; exit end;
   Tmp:= Subtractive; PB:= @Tmp;
   if PM = PB then begin result:=13; exit end;
   Tmp:= Average;     PB:= @Tmp;
   if PM = PB then begin result:=14; exit end;
   Tmp:= Difference;  PB:= @Tmp;
   if PM = PB then begin result:=15; exit end;
   Tmp:= Exclusion;   PB:= @Tmp;
   if PM = PB then begin result:=16; exit end;
   Tmp:= Negation;    PB:= @Tmp;
   if PM = PB then begin result:=17; exit end;
   Tmp:= Glow;        PB:= @Tmp;
   if PM = PB then begin result:=18; exit end;
   Tmp:= Heat;        PB:= @Tmp;
   if PM = PB then begin result:=19; exit end;
   Tmp:= Freeze;      PB:= @Tmp;
   if PM = PB then begin result:=20; exit end;
   Tmp:= Red;         PB:= @Tmp;
   if PM = PB then begin result:=21; exit end;
   Tmp:= Green;       PB:= @Tmp;
   if PM = PB then begin result:=22; exit end;
   Tmp:= Blue;       PB:= @Tmp;
   if PM = PB then begin result:=23; exit end;
   Tmp:= Hue;       PB:= @Tmp;
   if PM = PB then begin result:=24; exit end;
   Tmp:= Saturation;PB:= @Tmp;
   if PM = PB then begin result:=25; exit end;
   Tmp:= Lightness; PB:= @Tmp;
   if PM = PB then begin result:=26; exit end;
   Tmp:= Custom;      PB:= @Tmp;
   if PM = PB then begin result:=27; exit end;
   result:= -1; {Unknown adress}
  end;
end;

function GetBlendMode(index: integer): TPixelCombineEvent;
begin
 with Blendmode do
 case index of
   -1 : result:= nil;
    0 : result:= Normal;
    1 : result:= Multiply;
    2 : result:= Screen;
    3 : result:= Overlay;
    4 : result:= Reflect;
    5 : result:= Soft_Light;
    6 : result:= Bright_Light;
    7 : result:= Hard_Light;
    8 : result:= Dodge;
    9 : result:= Burn;
   10 : result:= Lighten;
   11 : result:= Darken;
   12 : result:= Additive;
   13 : result:= Subtractive;
   14 : result:= Average;
   15 : result:= Difference;
   16 : result:= Exclusion;
   17 : result:= Negation;
   18 : result:= Glow;
   19 : result:= Heat;
   20 : result:= Freeze;
   21 : result:= Red;
   22 : result:= Green;
   23 : result:= Blue;
   24 : result:= Hue;
   25 : result:= Saturation;
   26 : result:= Lightness;
   27 : result:= Custom;
  end;
end;

//---Blendmodes-------------------------

{Normal - if you´re only using this, use dmBlend instead of dmCustom - its faster}
procedure TBlendmode.Normal(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB,aM: cardinal;
begin
  if F = B then exit;
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit;

  fB := F        and $FF * aM;
  fG := F shr 8  and $FF * aM;
  fR := F shr 16 and $FF * aM;
  aM:= 256 - aM;

  fR := fR + B shr 16 and $FF * aM;
  fR := fR shr 8;

  fG := fG + B shr 8 and $FF * aM;
  fG := fG shr 8;

  fB := fB + B and $FF * aM;
  fB := fB shr 8;

  B:= B shl 24 or fR shl 16 or fG shl 8 or fB;
end;

{Average - useful in some cases - but the same as Normal with MasterAlpha = 128}
procedure TBlendmode.Average(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fR:= fR + bR;
  fR:= fR shr 1;
  fG:= fg + bG;
  fG:= fG shr 1;
  fB:= fB + bB;
  fB:= fB shr 1;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Screen}
procedure TBlendmode.Screen (F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  bR:= 255 - bR;
  fR:= 255 - fR;
  fR:= bR * fR shr 8;
  fR:= 255 - fR;
  bR:= 255 - bR;

  bG:= 255 - bG;
  fG:= 255 - fG;
  fG:= bG  * fG shr 8;
  fG:= 255 - fG;
  bG:= 255 - bG;

  bB:= 255 - bB;
  fB:= 255 - fB;
  fB:= bB  * fB shr 8;
  fB:= 255 - fB;
  bB:= 255 - bB;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;


{Lighten}
procedure TBlendmode.Lighten (F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  if fR < bR then fR:= bR;
  if fG < bG then fG:= bG;
  if fB < bB then fB:= bB;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Darken}
procedure TBlendmode.Darken (F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  if fR > bR then fR:= bR;
  if fG > bG then fG:= bG;
  if fB > bB then fB:= bB;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Add}
procedure TBlendmode.Additive(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fR := fR + bR;
  fG := fG + bG;
  fB := fB + bB;
  if fR > 255 then fR:= 255;
  if fG > 255 then fG:= 255;
  if fB > 255 then fB:= 255;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{subtract}
procedure TBlendmode.Subtractive(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fR := bR + fR - 256;
  fG := bG + fG - 256;
  fB := bB + fB - 256;
  if fR > 255 then fR:= 0; // > 255 only possible if <0 
  if fG > 255 then fG:= 0;
  if fB > 255 then fB:= 0;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Multiply}
procedure TBlendmode.Multiply(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fR := bR * fR shr 8;
  fG := bG * fG shr 8;
  fB := bB * fB shr 8;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Color Burn}
procedure TBlendmode.Burn(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  if fR > 0 then
   begin
    fR := (255-bR) shl 8 div fR;
    fR := 255 - fR;
    if fR > 255 then fR:= 0;
   end;
  if fG > 0 then
   begin
    fG := (255-bG) shl 8 div fG;
    fG := 255 - fG;
    if fG > 255 then fG:= 0;
   end;
  if fB > 0 then
   begin
    fB := (255-bB) shl 8 div fB;
    fB := 255 - fB;
    if fB > 255 then fB:= 0;
   end;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

procedure TBlendmode.Dodge  (F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  if fR < 255 then
   begin
    fR := 255 - fR;
    fR := bR shl 8 div fR;
    if fR > 255 then fR := 255;
   end;

  if fG < 255 then
   begin
    fG := 255 - fG;
    fG := bG shl 8 div fG;
    if fG > 255 then fG := 255;
   end;

  if fB < 255 then
   begin
    fB := 255 - fB;
    fB := bB shl 8 div fB;
    if fB > 255 then fB := 255;
   end;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Reflect - introduced by Michael Hansen}
procedure TBlendmode.Reflect(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  if fR < 255 then
   begin
    fR:= 255 - fR;
    fR:= sqr(bR) div fR;
    if fR > 255 then fR:=255;
   end;

  if fG < 255 then
   begin
    fG:= 255 -  fG;
    fG:= sqr(bG) div fG;
    if fG > 255 then fG:=255;
   end;

  if fB < 255 then
   begin
    fB:= 255 -  fB;
    fB:= sqr(bB) div fB;
    if fB > 255 then fB:=255;
   end;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Reflect variation by Jens Gruschel}
procedure TBlendmode.Freeze(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}

  if fR > 0 then
   begin
    fR:= Sqr(255-bR) div fR;
    if fR > 255 then fR := 255;
    fR:= 255 - fR;
   end else fR:= 0;

  if fG > 0 then
   begin
    fG:= Sqr(255-bG) div fG;
    if fG > 255 then fG := 255;
    fG:= 255 - fG;
   end else fG:= 0;

  if fB > 0 then
   begin
    fB:= Sqr(255-bB) div fB;
    if fB > 255 then fB := 255;
    fB:= 255 - fB;
   end else fB:= 0;

  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Reflect variation by Jens Gruschel}
procedure TBlendmode.Glow(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}

  if bR < 255 then
   begin
    fR:= Sqr(fR) div (255 - bR);
    if fR > 255 then fR := 255;
   end else fR:= 255;

  if bG < 255 then
   begin
    fG:= Sqr(fG) div (255 - bG);
    if fG > 255 then fG := 255;
   end else fG:= 255;

  if bB < 255 then
   begin
    fB:= Sqr(fB) div (255 - bB);
    if fB > 255 then fB := 255;
   end else fB:= 255;

  {Blend}
  bR := ( fR*aM + bR*(255-aM) )shr 8;
  bG := ( fG*aM + bG*(255-aM) )shr 8;
  bB := ( fB*aM + bB*(255-aM) )shr 8;

  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Reflect variation by Jens Gruschel}
procedure TBlendmode.Heat(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}

 if bR > 0 then
   begin
    fR:= Sqr(255-fR) div bR;
    if fR > 255 then fR := 255;
    fR:= 255 - fR;
   end else fR:= 0;

  if bG > 0 then
   begin
    fG:= Sqr(255-fG) div bG;
    if fG > 255 then fG := 255;
    fG:= 255 - fG;
   end else fG:= 0;

  if bB > 0 then
   begin
    fB:= Sqr(255-fB) div bB;
    if fB > 255 then fB := 255;
    fB:= 255 - fB;
   end else fB:= 0;

  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Overlay}
procedure TBlendmode.Overlay(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
 if bR < 128 then fR := bR * fR shr 7 else
  begin
   fR := 255 - fR;
   bR := 255 - bR;
   fR := bR  * fR shr 7;
   fR := 255 - fR;
   bR := 255 - bR;
  end;
 if bG < 128 then fG := bG * fG shr 7 else
  begin
   fG := 255 - fG;
   bG := 255 - bG;
   fG := bG  * fG shr 7;
   fG := 255 - fG;
   bG := 255 - bG;
  end;
 if bB < 128 then fB := bB * fB shr 7 else
  begin
   fB := 255 - fB;
   bB := 255 - bB;
   fB := bB  * fB shr 7;
   fB := 255 - fB;
   bB := 255 - bB;
  end;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B  := B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Soft Light - formula by Jens Gruschel}
procedure TBlendmode.Soft_Light(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM, c: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  c := bR * fR shr 8;
  fR := c + bR * (255 - ((255-bR)*(255-fR) shr 8)-c) shr 8;

  c := bG * fG shr 8;
  fG := c + bG * (255 - ((255-bG)*(255-fG) shr 8)-c) shr 8;

  c := bB * fB shr 8;
  fB := c + bB * (255 - ((255-bB)*(255-fB) shr 8)-c) shr 8;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Bright Light - Introduced by Michael Hansen -  much like average}
procedure TBlendmode.Bright_Light(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fR := SqrtTable[fR*bR];
  fG := SqrtTable[fG*bG];
  fB := SqrtTable[fB*bB];
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Hard Light}
procedure TBlendmode.Hard_Light(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  if fR < 128 then fR := bR*fR shr 7 else
   begin
    fR := 255 - fR;
    bR := 255 - bR;
    fR := 255 - fR * bR shr 7;
    bR := 255 - bR;
   end;
  if fG < 128 then fG := bG*fG shr 7 else
   begin
    fG := 255 - fG;
    bG := 255 - bG;
    fG := 255 - fG * bG shr 7;
    bG := 255 - bG;
   end;
  if fB < 128 then fB := bB*fB shr 7 else
   begin
    fB := 255 - fB;
    bB := 255 - bB;
    fB := 255 - fB * bB shr 7;
    bB := 255 - bB;
   end;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

procedure TBlendmode.Difference(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fR := abs(bR - fR);
  fG := abs(bG - fG);
  fB := abs(bB - fB);
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

procedure TBlendmode.Exclusion(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fR := bR + fR - (bR*fR shr 7);
  fG := bG + fG - (bG*fG shr 7);
  fB := bB + fB - (bB*fB shr 7);
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Negation - introduced by Jens Gruschel}
procedure TBlendmode.Negation(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fR := 255 - abs(255-bR-fR);
  fG := 255 - abs(255-bG-fG);
  fB := 255 - abs(255-bB-fB);
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Red - Background B,G with Foreground R}
procedure TBlendmode.Red(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fG := bG;
  fB := bB;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Green - Background R,B with Foreground G}
procedure TBlendmode.Green(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fG := F shr 8  and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fR := bR;
  fB := bB;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Blue - Background R,G with Foreground B}
procedure TBlendmode.Blue(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fR := bR;
  fG := bG;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Hue - Background S,L with Foreground H}
procedure TBlendmode.Hue(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal; fH,bH,S,L: single;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Combine}
  RGBtoHSL( F ,fH,S,L);
  RGBtoHSL( B ,bH,S,L);
  F:= HSLtoRGB(fH,S,L);
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Saturation - Background H,L with Foreground S}
procedure TBlendmode.Saturation(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal; H,fS,bS,L: single;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Combine}
  RGBtoHSL( F ,H,fS,L);
  RGBtoHSL( B ,H,bS,L);
  F:= HSLtoRGB(H,fS,L);
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

{Lightness - Background H,S with Foreground L}
procedure TBlendmode.Lightness(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal; H,S,fL,bL: single;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Combine}
  RGBtoHSL( F ,H,S,fL);
  RGBtoHSL( B ,H,S,bL);
  F:= HSLtoRGB(H,S,fL);
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

procedure TBlendmode.Custom(F: TColor32; var B: TColor32; M: TColor32);
var fR,fG,fB, bR,bG,bB, aM: cardinal;
begin
  {Foreground Alpha and Master Alpha combined}
  aM  := M and $FF;
  aM  := F shr 24 * aM;
  aM  := aM shr 8;
  if aM = 0 then exit; //exit if nothing changes ...
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fR := BlendMap[bR + fR shl 8]and $00FF0000 shr 16;
  fG := BlendMap[bG + fG shl 8]and $0000FF00 shr  8;
  fB := BlendMap[bB + fB shl 8]and $000000FF;
  {Blend}
  bR := bR + (fR-bR)*aM shr 8;// bR := ( fR*aM + bR*(255-aM) )shr 8
  bG := bG + (fG-bG)*aM shr 8;
  bB := bB + (fB-bB)*aM shr 8;
  {Reset - keeping B alpha}
  B:= B shl 24 or bR shl 16 or bG shl 8 or bB;
end;

initialization
 {Init SqrtTable }
 {This cannot be "realtime" due to the disabling of floating point operations}
 for x:=0 to 65535 do SqrtTable[x]:= Round( Sqrt(x) );
 {Init Custom Blendmap - like normal blend}
 for x:=0 to 255 do for y:=0 to 255 do FillChar(BlendMap[y + x shl 8],3,x);
end.

