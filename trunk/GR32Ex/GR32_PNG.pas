unit GR32_PNG;

interface

uses
  SysUtils, Windows, Classes, GR32, Graphics;

procedure LoadBitmap32FromPNG(DestBitmap: TBitmap32; const Filename: String; out AlphaChannelAvailable: Boolean); overload;
procedure LoadBitmap32FromPNG(DestBitmap: TBitmap32; SrcStream: TStream; out AlphaChannelAvailable: Boolean); overload;
procedure SaveBitmap32ToPNG(SrcBitmap: TBitmap32; Filename: String; Paletted: Boolean; Transparent: Boolean = True; BackgroundColor: TColor = clWhite); overload;
procedure SaveBitmap32ToPNG(SrcBitmap: TBitmap32; DestStream: TStream; Paletted: Boolean; Transparent: Boolean = True; BackgroundColor: TColor = clNone); overload;

implementation

uses PNGImage;

type
  TARGB = record
    b: Byte;
    g: Byte;
    r: Byte;
    a: Byte;
  end;
  PARGB = ^TARGB;

procedure LoadBitmap32FromPNG(DestBitmap: TBitmap32; SrcStream: TStream; out AlphaChannelAvailable: Boolean);
var
  png: TPngObject;
  tc: TColor32;
  x, y: Integer;
  bm: TBitmap;
  p: PByteArray;
begin
  bm := nil;
  png := nil;
  try
    png := TPngObject.Create;
    png.LoadFromStream(SrcStream);
    bm := TBitmap.Create;
    bm.Assign(png);
    DestBitmap.Assign(bm);
    DestBitmap.ResetAlpha;

    case png.TransparencyMode of
      ptmBit:
        begin
          tc := Color32(png.TransparentColor);
          for y := 0 to DestBitmap.Height - 1 do
            for x := 0 to DestBitmap.Width - 1 do
              if DestBitmap.Pixel[x, y] = tc then
                PARGB(DestBitmap.PixelPtr[x, y])^.a := 0;

          AlphaChannelAvailable := True;
        end;
      ptmPartial:
        begin
          if (png.Header.ColorType = COLOR_GRAYSCALEALPHA) or
            (png.Header.ColorType = COLOR_RGBALPHA) then
          begin
            for y := 0 to DestBitmap.Height - 1 do
            begin
              p := png.AlphaScanline[y];
              for x := 0 to DestBitmap.Width - 1 do
                PARGB(DestBitmap.PixelPtr[x, y])^.a := p[x];
            end;

            AlphaChannelAvailable := True;
          end;
        end;
    end;
  finally
    if Assigned(bm) then
      FreeAndNil(bm);
    if Assigned(png) then
      FreeAndNil(png);
  end
end;

procedure LoadBitmap32FromPNG(DestBitmap: TBitmap32; const Filename: String; out AlphaChannelAvailable: Boolean);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadBitmap32FromPNG(DestBitmap, FileStream, AlphaChannelAvailable);
  finally
    FileStream.Free;
  end;
end;

procedure SaveBitmap32ToPNG(SrcBitmap: TBitmap32; DestStream: TStream; Paletted: Boolean; Transparent: Boolean; BackgroundColor: TColor);
var
  bm: TBitmap;
  png: TPngObject;
  TRNS: TCHUNKtRNS;
  p: PByteArray;
  x, y: Integer;
begin
  bm := nil;
  png := nil;
  try
    bm := TBitmap.Create;
    png := TPngObject.Create;
    bm.Assign(SrcBitmap);
    
    if Paletted then
      bm.PixelFormat := pf8bit; // force paletted on TBitmap, transparent for the web must be 8bit
      
    png.InterlaceMethod := imNone;
    png.CompressionLevel := 9;
    png.Assign(bm); //Convert data into png

    if Transparent then
    begin
      if png.Header.ColorType in [COLOR_PALETTE] then
      begin
        if (png.Chunks.ItemFromClass(TChunktRNS) = nil) then
          png.CreateAlpha();
        TRNS := png.Chunks.ItemFromClass(TChunktRNS) as TChunktRNS;
        if Assigned(TRNS) then
          TRNS.TransparentColor := BackgroundColor;
      end;

      if png.Header.ColorType in [COLOR_RGB, COLOR_GRAYSCALE] then
        png.CreateAlpha();

      if png.Header.ColorType in [COLOR_RGBALPHA, COLOR_GRAYSCALEALPHA] then
      begin
        for y := 0 to png.Header.Height - 1 do
        begin
          p := png.AlphaScanline[y];
          for x := 0 to png.Header.Width - 1 do
            p[x] := TARGB(SrcBitmap.Pixel[x, y]).a;
        end;
      end;
    end;
    
    png.SaveToStream(DestStream);
  finally
    if Assigned(bm) then
      bm.Free;
    if Assigned(png) then
      png.Free;
  end
end;

procedure SaveBitmap32ToPNG(SrcBitmap: TBitmap32; Filename: String; Paletted: Boolean; Transparent: Boolean; BackgroundColor: TColor);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(Filename, fmCreate);
  try
    SaveBitmap32ToPNG(SrcBitmap, FileStream, Paletted, Transparent, BackgroundColor);
  finally
    FileStream.Free;
  end;
end;

end.
