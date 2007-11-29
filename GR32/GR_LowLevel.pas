unit GR_LowLevel;

interface

{$I GR32.inc}

uses
  SysUtils, Windows
  , Graphics
  , Classes, Controls
  , GR32
  , GR32_System
  ;

type
  TGRClearAlphaProc = procedure (Src: Pointer; Count: integer; Value: longword);

  TkfxMaskMode = (mmExclude, mmInclude);

const
  GRTransparent32       = $007F007F;
  AlphaMask             = $FF000000;

var
  //设置颜色值=Value的Alpha为0。
  ClearAlphaFunc: TGRClearAlphaProc;

procedure StretchAlphaRect(var X; Width, Height, XDst, YDst, WDst, HDst,
  XSrc, YSrc, WSrc, HSrc: integer; var Alpha);

function RectWidth(const aRect: TRect): integer;
function RectHeight(const aRect: TRect): integer;
function RectHCenter(var R: TRect; Bounds: TRect): TRect;
function RectVCenter(var R: TRect; Bounds: TRect): TRect;
function RectCenter(var R: TRect; Bounds: TRect): TRect;
function RectOffset(ARect: TRect; Offset: integer): TRect;
function IsRectEmpty(Rect: TRect): boolean;
function CompareRect(Rect1, Rect2: TRect): boolean;
function MarginRect(ARect, AMargin: TRect): TRect;
function RectInRect(ARect, ABounds: TRect): boolean;

function GetClientOrigin(AControl: TControl): TPoint;
function GetRealClientRect(AControl: TControl): TRect;
function GetRealBoundsRect(AControl: TControl): TRect;

{ Region }

function CreateRegionFromBitmap(Bitmap: TBitmap32; Left, Top: integer): HRgn;
{$IFNDEF CLX}
function CreateRegionFromPicture(APicture: TPicture; Width, Height: integer; MaskColor: TColor; MaskMode: TkfxMaskMode): HRgn;
{$ELSE}
function CreateRegionFromPicture(APicture: TPicture; Width, Height: integer; MaskColor: TColor; MaskMode: TkfxMaskMode): QRegionH;
{$ENDIF}

{ Drawing routines }

{$IFNDEF CLX}
function CreateHalftoneBrush(Color: TColor): HBRUSH;
{$ENDIF}

implementation

type

  PLongword = ^longword;

  PLongArray = ^TLongArray;
  TLongArray = array [0..0] of longword;

  PByteArray = ^TByteArray;
  TByteArray = array [0..0] of byte;

{ Window Routines =============================================================}

function GetClientOrigin(AControl: TControl): TPoint;
var
  ClientOrigin: TPoint;
  ControlOrigin: TPoint;
  R: TRect;
begin
  Result := Point(0, 0);

  {$IFDEF CLX}
  if AControl is TWidgetControl then
  begin
    ClientOrigin := TWidgetControl(AControl).ClientToScreen(Point(0, 0));

    QWidget_frameGeometry(TWidgetControl(AControl).Handle, @R);
    ControlOrigin := R.TopLeft;
    if AControl.Parent <> nil then
      ControlOrigin := AControl.Parent.ClientToScreen(Point(AControl.Left, AControl.Top));

    Result.X := ClientOrigin.X - ControlOrigin.X;
    Result.Y := ClientOrigin.Y - ControlOrigin.Y;
  end;
  {$ELSE}
  if AControl is TWinControl then
  begin
    ClientOrigin := TWinControl(AControl).ClientToScreen(Point(0, 0));
    ControlOrigin := Point(AControl.Left, AControl.Top);
    if AControl.Parent <> nil then
      ControlOrigin := AControl.Parent.ClientToScreen(Point(AControl.Left, AControl.Top));

    Result.X := ClientOrigin.X - ControlOrigin.X;
    Result.Y := ClientOrigin.Y - ControlOrigin.Y;
  end;
  {$ENDIF}
end;

function GetRealClientRect(AControl: TControl): TRect;
begin
  Result := AControl.ClientRect;
  with GetClientOrigin(AControl) do
    OffsetRect(Result, X, Y);
end;

function GetRealBoundsRect(AControl: TControl): TRect;
var
  P: TPoint;
  {$IFDEF MSWINDOWS}
  Wnd: Cardinal;
  {$ELSE}
  R: TRect;
  {$ENDIF}
begin
  Result := AControl.BoundsRect;
  {$IFDEF CLX}
  if AControl is TWidgetControl then
  begin
    {$IFDEF MSWINDOWS}
    if AControl is TCustomForm then
    begin
      Wnd := QWidget_winId(TWidgetControl(AControl).Handle);
      GetWindowRect(Wnd, Result);
    end
    else
    begin
      QWidget_frameSize(TWidgetControl(AControl).Handle, @P);
      Result.Right := Result.Left + P.X;
      Result.Bottom := Result.Top + P.Y;
    end;
    {$ELSE}
    QWidget_frameSize(TWidgetControl(AControl).Handle, @P);
    QWidget_frameGeometry(TWidgetControl(AControl).Handle, @Result);
    {$ENDIF}
  end;
  {$ENDIF}
end;

function RectHeight(const aRect: TRect): integer;
begin
  Result := aRect.Bottom - aRect.Top;
end;

function RectWidth(const aRect: TRect): integer;
begin
  Result := aRect.Right - aRect.Left;
end;

function RectVCenter(var R: TRect; Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, 0, (RectHeight(Bounds) - RectHeight(R)) div 2);
  OffsetRect(R, Bounds.Left, Bounds.Top);

  Result := R;
end;

function RectHCenter(var R: TRect; Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, (RectWidth(Bounds) - RectWidth(R)) div 2, 0);
  OffsetRect(R, Bounds.Left, Bounds.Top);

  Result := R;
end;

function RectCenter(var R: TRect; Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, (RectWidth(Bounds) - RectWidth(R)) div 2, (RectHeight(Bounds) - RectHeight(R)) div 2);
  OffsetRect(R, Bounds.Left, Bounds.Top);

  Result := R;
end;

function RectOffset(ARect: TRect; Offset: integer): TRect;
begin
  Result.Top := ARect.Top - Offset;
  Result.Left := ARect.Left - Offset;
  Result.Bottom := ARect.Bottom + Offset;
  Result.Right := ARect.Right + Offset;
end;

function IsRectEmpty(Rect: TRect): boolean;
begin
  Result := (RectWidth(Rect) <= 0) or (RectHeight(Rect) <= 0);
end;

function MarginRect(ARect, AMargin: TRect): TRect;
var
  Dx: single;
begin
  Result := ARect;

  if (AMargin.Left > RectWidth(ARect)) or (AMargin.Right > RectWidth(ARect)) or (AMargin.Left + AMargin.Right > RectWidth(ARect)) then
  begin
    if (AMargin.Left + AMargin.Right) <> 0 then
      Dx := RectWidth(ARect) / (AMargin.Left + AMargin.Right)
    else
      Dx := 1;
    Result.Left := Result.Left + Round(AMargin.Left * Dx);
    Result.Top := Result.Top + AMargin.Top;
    Result.Right := Result.Right - Round(AMargin.Right * Dx);
    Result.Bottom := Result.Bottom - AMargin.Bottom;
  end
  else
  if (AMargin.Top > RectHeight(ARect)) or (AMargin.Bottom > RectHeight(ARect)) or (AMargin.Top + AMargin.Bottom > RectHeight(ARect)) then
  begin
    if (AMargin.Top + AMargin.Bottom) <> 0 then
      Dx := RectHeight(ARect) / (AMargin.Top + AMargin.Bottom)
    else
      Dx := 1;
    Result.Left := Result.Left + AMargin.Left;
    Result.Top := Result.Top + Round(AMargin.Top * Dx);
    Result.Right := Result.Right - AMargin.Right;
    Result.Bottom := Result.Bottom - Round(AMargin.Bottom * Dx);
  end
  else
  begin
    Result.Left := Result.Left + AMargin.Left;
    Result.Top := Result.Top + AMargin.Top;
    Result.Right := Result.Right - AMargin.Right;
    Result.Bottom := Result.Bottom - AMargin.Bottom;
  end;
end;

function RectInRect(ARect, ABounds: TRect): boolean;
begin
  Result := (ARect.Left >= ABounds.Left) and (ARect.Top >= ABounds.Top) and
    (ARect.Right <= ABounds.Right) and (ARect.Bottom <= ABounds.Bottom);
end;

function CompareRect(Rect1, Rect2: TRect): boolean;
begin
  Result := (Rect1.Left = Rect2.Left) and (Rect1.Top = Rect2.Top) and
    (Rect1.Right = Rect2.Right) and (Rect1.Bottom = Rect2.Bottom);
end;

function PointInPolygon(const P: TPoint; const Points: array of TPoint): boolean;
type
  PPoints = ^TPoints;
  TPoints = array[0..0] of TPoint;
var
  Rgn: HRgn;
begin
  Rgn := CreatePolygonRgn(PPoints(@Points)^, High(Points) + 1, WINDING);
  try
    Result := PtInRegion(Rgn, P.X, P.Y);
  finally
    DeleteObject(Rgn);
  end;
end;

{ Region routines =============================================================}

var
  Rts: array [0..5000] of TRect;

function CreateRegionDataFromBitmap(Bitmap: TBitmap32; var RgnData: PRgnData;
  Left, Top: integer): HRgn;
var
  j, i, i1: integer;
  TrColor: TColor32;
  C: PColor32;
  Count: integer;
begin
  Result := 0;

  TrColor := GRTransparent32;

  if Bitmap.Empty then Exit;
  if Bitmap.Width * Bitmap.Height = 0 then Exit;

  Count := 0;
  for j := 0 to Bitmap.Height-1 do
  begin
    i := -1;
    while i < Bitmap.Width do
    begin
      repeat
        Inc(i);
        C := Bitmap.PixelPtr[i, j];
        if i >= Bitmap.Width then Break;
      until not ((C^ and not AlphaMask) = TrColor);

      if i >= Bitmap.Width then Break;

      i1 := i;
      repeat
        Inc(i1);
        If (i1 >= Bitmap.Width) Then Break;
        C := Bitmap.PixelPtr[i1, j];
      until ((C^ and not AlphaMask) = TrColor);

      if i <> i1 then
      begin
        Rts[Count] := Rect(Left + i, Top + j, Left + i1, Top + j + 1);
        Inc(Count);
      end;
      i := i1;
    end;
  end;
  { Make Region data }
  Result := Count * SizeOf(TRect);
  GetMem(Rgndata, SizeOf(TRgnDataHeader) + Result);
  RgnData^.rdh.dwSize := SizeOf(TRgnDataHeader);
  RgnData^.rdh.iType := RDH_RECTANGLES;
  RgnData^.rdh.nCount := Count;
  RgnData^.rdh.nRgnSize := 0;
  RgnData^.rdh.rcBound := Rect(0, 0, Bitmap.Width, Bitmap.Height);
  { Update New Region }
  Move(Rts, RgnData^.Buffer, Result);
  Result := SizeOf(TRgnDataHeader) + Count * SizeOf(TRect);
end;

function CreateRegionFromBitmap(Bitmap: TBitmap32; Left, Top: integer): HRgn;
var
  RgnData: PRgnData;
  Size: integer;
begin
  RgnData := nil;
  Size := CreateRegionDataFromBitmap(Bitmap, RgnData, Left, Top);
  Result := ExtCreateRegion(nil, Size, RgnData^);
  if RgnData <> nil then FreeMem(RgnData, Size);
end;

{$IFNDEF CLX}
function CreateRegionFromPicture(APicture: TPicture; Width, Height: integer; MaskColor: TColor; MaskMode: TkfxMaskMode): HRgn;
{$ELSE}
function CreateRegionFromPicture(APicture: TPicture; Width, Height: integer; MaskColor: TColor; MaskMode: TkfxMaskMode): QRegionH;
{$ENDIF}
var
  FBitmap: TBitmap;
  X, Y: Integer;
  StartX: Integer;
  Exclude: boolean;
  Line: PColor32Array;
  Color: TColor32;
  {$IFDEF CLX}
  Region: QRegionH;
  {$ELSE}
  Region: HRgn;
  {$ENDIF}
begin
  {$IFDEF CLX}
  Result := nil;
  {$ELSE}
  Result := 0;
  {$ENDIF}

  { Check Picture }
  if APicture.Graphic = nil then Exit;
  if (APicture.Width * APicture.Height) = 0 then Exit;

  { Create region }
  Color := Color32(MaskColor);
  Exclude := MaskMode = mmExclude;

  FBitmap := TBitmap.Create;
  try
    FBitmap.PixelFormat := pf32Bit;
    FBitmap.Width := Width;
    FBitmap.Height := Height;

    FBitmap.Canvas.Draw(0, 0, APicture.Graphic);

    for Y := 0 to FBitmap.Height - 1 do
    begin
      Line := FBitmap.Scanline[Y];
      X := 0;
      while X < FBitmap.Width do
      begin

        if Exclude then
        begin
          while (TColor32Entry(Line[X]).R = TColor32Entry(Color).R) and (TColor32Entry(Line[X]).G = TColor32Entry(Color).G) and (TColor32Entry(Line[X]).B = TColor32Entry(Color).B) do
          begin
            Inc(X);
            if X = FBitmap.Width then
              break;
          end;
        end
        else
        begin
          while (TColor32Entry(Line[X]).R <> TColor32Entry(Color).R) or (TColor32Entry(Line[X]).G <> TColor32Entry(Color).G) or (TColor32Entry(Line[X]).B <> TColor32Entry(Color).B) do
          begin
            Inc(X);
            if X = FBitmap.Width then
              break;
          end;
        end;

        if X = FBitmap.Width then
          break;

        StartX := X;
        if Exclude then
        begin
          while (TColor32Entry(Line[X]).R <> TColor32Entry(Color).R) or (TColor32Entry(Line[X]).G <> TColor32Entry(Color).G) or (TColor32Entry(Line[X]).B <> TColor32Entry(Color).B) do
          begin
            if X = FBitmap.Width then
              break;
            Inc(X);
          end;
        end
        else
        begin
          while (TColor32Entry(Line[X]).R = TColor32Entry(Color).R) and (TColor32Entry(Line[X]).G = TColor32Entry(Color).G) and (TColor32Entry(Line[X]).B = TColor32Entry(Color).B) do
          begin
            if X = FBitmap.Width then
              break;
            Inc(X);
          end;
        end;

        {$IFDEF CLX}
        if Result = nil then
          Result := QRegion_create(StartX, Y, X - StartX, 1, QRegionRegionType_Rectangle)
        else
        begin
          Region := QRegion_create(StartX, Y, X - StartX, 1, QRegionRegionType_Rectangle);
          if Region <> nil then
          begin
            QRegion_unite(Result, Result, Region);
            QRegion_destroy(Region);
          end;
        end;
        {$ELSE}
        if Result = 0 then
          Result := CreateRectRgn(StartX, Y, X, Y + 1)
        else
        begin
          Region := CreateRectRgn(StartX, Y, X, Y + 1);
          if Region <> 0 then
          begin
            CombineRgn(Result, Result, Region, RGN_OR);
            DeleteObject(Region);
          end;
        end;
        {$ENDIF}
      end;
    end;
  finally
    FBitmap.Free;
  end;
end;

{ Drawing routines ============================================================}

{$IFNDEF CLX}

function CreateHalftoneBrush(Color: TColor): HBRUSH;
const
  HalfColor = clWhite;
var
  i, j: Integer;
  GrayBitmap: TBitmap;
begin
  GrayBitmap := TBitmap.Create;
  GrayBitmap.Width := 8;
  GrayBitmap.Height := 8;

  GrayBitmap.Canvas.Brush.Color := clBtnFace;
  GrayBitmap.Canvas.FillRect(Rect(0, 0, 8, 8));

  for i := 0 to 7 do
    for j := 0 to 7 do
    begin
      if Odd(i) and Odd(j) then
        GrayBitmap.Canvas.Pixels[i, j] := Color;

      if not Odd(i) and not Odd(j) then
        GrayBitmap.Canvas.Pixels[i, j] := Color;
    end;

  Result := CreatePatternBrush(GrayBitmap.Handle);

  GrayBitmap.Free;
end;

{$ENDIF}

procedure StretchAlphaRect(var X; Width, Height, XDst, YDst, WDst, HDst,
  XSrc, YSrc, WSrc, HSrc: integer; var Alpha);
var
  C: PLongword;
  A, AP: PByte;
  CArray: PLongArray;
  AArray: PByteArray;

  R: TRect;
  SFX, SFY: integer;
  DstY, DstX, SrcX: integer;
  SX, SY: integer;
  DX, DY: integer;
  SrcRect, DstRect: TRect;
begin
  SrcRect := MakeRect(XSrc, YSrc, XSrc + WSrc, YSrc + HSrc);
  DstRect := MakeRect(XDst, YDst, XDst + WDst, YDst + HDst);

  IntersectRect(R, SrcRect, MakeRect(0, 0, Width, Height));
  if (RectWidth(R) <= 0) or (RectHeight(R) <= 0) then Exit;
  if (RectWidth(DstRect) <= 0) or (RectHeight(DstRect) <= 0) then Exit;
  IntersectRect(R, DstRect, MakeRect(0, 0, Width, Height));
  if (RectWidth(R) <= 0) or (RectHeight(R) <= 0) then Exit;

  SFX := MulDiv((R.Left - DstRect.Left) * WSrc, 65535, WDst);
  SFY := MulDiv((R.Top - DstRect.Top) * HSrc, 65535, HDst);

  DX := (WSrc shl 16) div WDst;
  DY := (HSrc shl 16) div HDst;
  SY := SFY;

  CArray := PLongArray(@X);
  AArray := PByteArray(@Alpha);

  for DstY := R.Top to R.Bottom - 1 do
  begin
    A := @AArray[SrcRect.Left + (SY shr 16 + SrcRect.Top) * RectWidth(SrcRect)];
    C := @CArray[R.Left + (DstY * Width)];
    SX := SFX;
    for DstX := R.Left to R.Right - 1 do
    begin
      SrcX := (SX shr 14 and $FFFFFFFC) shr 2;
      Inc(SX, DX);
      { Get alpha }
      AP := PByte(Integer(A) + SrcX);
      { Set Alpha }
      C^ := C^ and not AlphaMask;
      C^ := C^ or (AP^ shl 24);

      Inc(C);
    end;
    Inc(SY, DY);
  end;
end;



procedure _ClearAlpha(Src: Pointer; Count: integer; Value: longword);
asm
  { Clear alpha }
  PUSH   EDI

  MOV     EDI, EAX {X}
  MOV     EAX, ECX {Value}
  MOV     ECX, EDX {Count}
  TEST    ECX,ECX
  JS      @exit
  AND     EAX, $00FFFFFF
@1:
  MOV     EDX, [EDI]
  AND     EDX, $00FFFFFF
  CMP     EDX, EAX
  JNE     @2
  MOV     [EDI], EDX
@2:
  ADD     EDI, 4

  LOOP    @1

@exit:
  POP     EDI
end;



procedure SetupFunctions;
begin
  if HasMMX then
  begin
    { MMX }
    ClearAlphaFunc := _ClearAlpha;
  end
  else
  begin
    { Non-MMX }
    ClearAlphaFunc := _ClearAlpha;
  end;
end;

initialization
  SetupFunctions;
end.
