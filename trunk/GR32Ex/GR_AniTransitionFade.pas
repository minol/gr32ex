//TODO: the Bitmap transition is not work.
unit GR_AniTransitionFade;

{$I GR32.inc}
{$T-,W-,X+,P+}


interface

uses
  Graphics
  //, dialogs
  , GR32, GR32_Math
  , GR_LowLevel
  , GR_AniTransition
  ;

type
{ Fade Matrix }

  PGRMatrixFade = ^TGRMatrixFade;
  TGRMatrixFade = array[0..0] of Byte;

  TGRTransitionProcFade = procedure (var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);

  TGRFadeTransitionParam = class(TGRTransitionParam)
  private
  protected
    MatrixWidth, MatrixHeight: integer;
    MatrixLen: integer;
    MatrixFade: PGRMatrixFade;
    CopyMatrixFade: PGRMatrixFade;
    procedure Init; override;
  public
    destructor Destroy; override;
  end;

  TGRBitmapTransitionParamClass = class of TGRBitmapTransitionParam;
  TGRBitmapTransitionParam = class(TGRFadeTransitionParam)
  protected
    //FBitmap: TBitmap;
    //procedure SetBitmap(const Value: TBitmap);

    procedure Init; override;
  public
    constructor Create(aOwner: PGRTransitionRec = nil); override;
    destructor Destroy; override;
  published
    //property Bitmap: TBitmap read FBitmap write SetBitmap;
  end;

procedure RegisterFadeTransition(
  const aName: string; 
  const aProc: TGRTransitionProcFade;
  const aTitle: string = ''; 
  aParamClass: TGRTransitionParamClass = nil
);

implementation {===============================================================}

{ FadeMatrix routines }

procedure FadeMatrixLine(var Matrix: TGRMatrixFade; Width, Height: integer; x1, y1, x2, y2: integer; Alpha: byte);
var
  i, dx, dy, Plotx, Ploty, ix, iy, max, x, y: integer;
  Plot: boolean;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  ix := abs (dx);
  iy := abs (dy);
  if ix > iy then
    max := ix
  else
    max := iy;
  Plotx := x1;
  Ploty := y1;
  x := 0;
  y := 0;
  if (Plotx >= 0) and (Ploty >= 0) and (Plotx < Width) and (Ploty < Height) then
    Matrix [Plotx + Width*Ploty] := Alpha;
  for i := 0 to max do
  begin
    x := x + ix;
    y := y + iy;
    Plot := false;
    if x > max then
    begin
       Plot := true;
       x := x - max;
       if dx > 0 then
         inc (Plotx)
       else
         dec (Plotx);
    end;
    if y > max then
    begin
       Plot := true;
       y := y - max;
       if dy > 0 then
         inc (Ploty)
       else
         dec (Ploty);
     end;
     if (Plot) and (Plotx >= 0) and (Ploty >= 0) and (Plotx < Width) and (Ploty < Height) then
       Matrix[Plotx + Width*Ploty] := Alpha;
   end;
end;

procedure FadeMatrixCircle(var Matrix: TGRMatrixFade; Width, Height: integer; xc, yc, radius: integer; Alpha: byte);
var
  x, y, d: integer;
  Ratio: real;
 procedure Symmetry(x, y, xc, yc : integer);
 procedure SetMatrixpixel(xp, yp : integer);
 begin
   if (xp >= 0) and (xp < width) and (yp>=0) and (yp < Height) then
     Matrix [xp + Width*yp] := Alpha;
 end;
var
  x_start, x_end, x_out : integer;
  y_start, y_end, y_out : integer;
begin
  x_start := round (x * Ratio);
  x_end := round ((x+1) * Ratio);
  y_start := round (y * Ratio);
  y_end := round ((y+1) * Ratio);
  for x_out := x_start to x_end do
  begin
    setmatrixpixel(x_out + xc, y + yc);
    setmatrixpixel(x_out + xc, -y + yc);
    setmatrixpixel(-x_out + xc,-y + yc);
    setmatrixpixel(-x_out + xc,y + yc);
  end;
  for y_out := y_start to y_end do
  begin
    setmatrixpixel(y_out + xc, x + yc);
    setmatrixpixel(y_out + xc, -x + yc);
    setmatrixpixel(-y_out + xc, -x + yc);
    setmatrixpixel(-y_out + xc, x + yc);
  end;
end;
begin
  ratio := 1;
  y := Radius;
  d := 3 - 2 * Radius;
  x := 0;
  while x < y do
  begin
    symmetry (x, y, xc, yc);
    if d < 0 then
      d := d + 4*x + 6
    else
    begin
      d := d + 4*(x-y) + 10;
      dec (y);
    end;
    inc (x);
  end;
  if x = y then
    symmetry (x, y, xc, yc);
end;

{ FadeMatrix's Effects ========================================================}

procedure FadeMatrixFade(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  Value: Byte;
begin
  if Percent = 0 then Exit;

  Value := Round($FF * (Percent / 100));

  FillChar(Matrix, Width * Height, Value);
end;

procedure FadeMatrixDiagonal(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  DeltaX, DeltaY: integer;
  x, y: integer;
begin

  if percent = 0 then
    exit;

  DeltaX := MulDiv((Width-1), Percent, 100) * 2;
  DeltaY := MulDiv((Height-1), Percent, 100) * 2;

  if DeltaX > DeltaY then
  begin
    y := 0;
    for x := 0 to DeltaX do
    begin
      FadeMatrixLine(Matrix, Width, Height, 0, y, x, 0, MulDiv(Percent,$FF,100));
      if y < DeltaY then
        Inc (y);
    end;
  end
  else
  begin
    x := 0;
    for y := 0 to DeltaY do
    begin
      FadeMatrixLine(Matrix, Width, Height, 0, y, x, 0, MulDiv(Percent,$FF,100));
      if x < DeltaX then
        Inc(x);
    end;
  end;
end;

procedure FadeMatrixDiagonalIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  DeltaX, DeltaY: integer;
  x, y: integer;
begin

  if percent = 0 then
    exit;

  DeltaX := Round ((Percent/100)*(Width-1));
  DeltaY := Round ((Percent/100)*(Height-1));

  if DeltaX > DeltaY then
  begin
    y := 0;
    for x := 0 to DeltaX do
    begin
      FadeMatrixLine(Matrix, Width, Height, 0, Height-y-1, x, Height-1, MulDiv(Percent,$FF,100));
      if y < DeltaY then
        Inc (y);
    end;
    y := 0;
    for x := Width - 1 downto Width - DeltaX - 1 do
    begin
      FadeMatrixLine(Matrix, Width, Height, x, 0, width-1, y, MulDiv(Percent,$FF,100));
      if y < DeltaY then
        Inc (y);
    end;
  end
  else
  begin
    x := 0;
    for y := 0 to DeltaY do
    begin
      FadeMatrixLine(Matrix, Width, Height, 0, Height-y-1, x, Height-1, MulDiv(Percent,$FF,100));
      if x < DeltaX then
        Inc (x);
    end;
    x := Width - 1;
    for y := 0 to DeltaY do
    begin
      FadeMatrixLine(Matrix, Width, Height, x, 0, Width-1, y, MulDiv(Percent,$FF,100));
      if x > Width - 1 - DeltaX then
        Dec (x);
    end;
  end;
end;

procedure FadeMatrixDiagonalOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  DeltaX, DeltaY: integer;
  x, y: integer;
begin
  if percent = 0 then
    exit;
  DeltaX := Round ((Percent/100)*(Width-1));
  DeltaY := Round ((Percent/100)*(Height-1));
  if DeltaX > DeltaY then
  begin
    y := 0;
    for x := 0 to DeltaX do
    begin
      FadeMatrixLine (Matrix, Width, Height, 0, Y, Width - x -1, Height - 1, MulDiv(Percent,$FF,100));
      if y < DeltaY then
        Inc (y);
    end;
    y := 0;
    for x := 0 to DeltaX do
    begin
      FadeMatrixLine (Matrix, Width, Height, x, 0, Width, Height - y, MulDiv(Percent,$FF,100));
      if y < DeltaY then
        Inc (y);
    end;
  end
  else
  begin
    x := 0;
    for y := 0 to DeltaY do
    begin
      FadeMatrixLine (Matrix, Width, Height, x, 0, Width - 1, Height - y -1, MulDiv(Percent,$FF,100));
      if x < DeltaX then
        Inc (x);
    end;
    x := 0;
    for y := 0 to DeltaY do
    begin
      FadeMatrixLine (Matrix, Width, Height, 0, y, Width-x-1, height-1, MulDiv(Percent,$FF,100));
      if x < DeltaX then
        Inc (x);
    end;
  end;
end;

procedure FadeMatrixDown(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  j: integer;
begin
  if percent = 0 then
    exit;
  for j := 0 to MulDiv(Percent,Height-1,100) do
    FadeMatrixLine (Matrix, Width, Height, 0, j, Width-1, j, MulDiv(Percent,$FF,100));
end;

procedure FadeMatrixIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  j: integer;
begin
  if percent = 0 then
    exit;
  for j := Height-1 downto Height - 1 - MulDiv(Percent,Height div 2,100) do
    FadeMatrixLine (Matrix, Width, Height, 0, j, Width-1, j, MulDiv(Percent,$FF,100));
  for j := 0 to MulDiv(Percent,Height div 2,100) do
    FadeMatrixLine (Matrix, Width, Height, 0, j, Width-1, j, MulDiv(Percent,$FF,100));
end;

procedure FadeMatrixOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  j: integer;
begin
  if percent = 0 then
    exit;
  for j := 0 to MulDiv(Percent,Height div 2,100) do
  begin
    FadeMatrixLine (Matrix, Width, Height, 0, Height div 2 - j, Width-1, Height div 2 - j, MulDiv(Percent,$FF,100));
    FadeMatrixLine (Matrix, Width, Height, 0, Height div 2 + j, Width-1, Height div 2 + j, MulDiv(Percent,$FF,100));
  end;
end;

procedure FadeMatrixCrossOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  DeltaX, DeltaY: integer;
  Center: TPoint;
  i: integer;
begin
  if Percent = 0 then
    exit;
  DeltaX := MulDiv (Percent, Width*2, 100);
  DeltaY := MulDiv (Percent, Height*2, 100);
  Center.X := Width div 2;
  Center.Y := Height div 2;
  for i := -DeltaX div 4 to DeltaX div 4 do
    FadeMatrixLine (Matrix, Width, Height, Center.X+i, Center.Y-DeltaY,Center.X+i, Center.Y+DeltaY, MulDiv (Percent, $FF,100));
  for i := -DeltaY div 4 to DeltaY div 4 do
    FadeMatrixLine (Matrix, Width, Height, Center.X-DeltaX, Center.Y+i,Center.X+DeltaX, Center.Y+i, MulDiv (Percent, $FF,100));
end;

procedure FadeMatrixCrossIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i,j: integer;
begin
  if Percent = 0 then
    exit;
  FadeMatrixCrossOut (Matrix, Width, Height, 100-Percent);
  for i := 0 to Width-1 do
    for j := 0 to Height-1 do
      if Matrix[i+j*Width] = 0
      then
        Matrix[i+j*Width] := MulDiv (Percent, $FF,100)
      else
        Matrix[i+j*Width] := 0;
end;

procedure FadeMatrixRectOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  Delta: integer;
  Center: TPoint;
  i: integer;
  Alpha: integer;
  RealX,RealY: Real;
  x,y: integer;
begin
  if Percent = 0 then
    exit;
  if Width > Height then
  begin
    RealX := 1;
    Delta := MulDiv (Percent, Round (Width*1), 100);
    RealY := Height/Width;
  end
  else
  begin
    RealY := 1;
    Delta := MulDiv (Percent, Round (Height*1), 100);
    RealX:= Width/Height;
  end;
  Center.X := Width div 2;
  Center.Y := Height div 2;
  for i := 0 to Delta div 2 do
  begin
    Alpha := MulDiv ($FF, Percent, 100);
    X := Round (RealX*i);
    Y := Round (RealY * i);
    FadeMatrixLine (Matrix, Width, Height, Center.X-x, Center.Y+y,Center.X-x, Center.Y-y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X-x, Center.Y-y,Center.X+x, Center.Y-y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X+x, Center.Y-y,Center.X+x, Center.Y+y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X+x, Center.Y+y,Center.X-x, Center.Y+y, Alpha);
  end;
end;

procedure FadeMatrixRectIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i,j: integer;
begin
  if Percent = 0 then
    exit;
  FadeMatrixRectOut (Matrix, Width, Height, 100-Percent);
  for i := 0 to Width-1 do
    for j := 0 to Height-1 do
      if Matrix[i+j*Width] = 0
      then
        Matrix[i+j*Width] := MulDiv (Percent, $FF,100)
      else
        Matrix[i+j*Width] := 0;
end;

{ Pixel Effects ==============================================================}

procedure FadeMatrixPixel(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  MatrixSize: Cardinal;
  i, j: cardinal;
  PixelisZero: boolean;
  RandomPosition: Cardinal;
begin
  if percent = 0 then Exit;

  MatrixSize := Width * Height;
  RandSeed := MatrixSize;
  j := Round(MatrixSize * (Percent / 100));
  for i := 1 to j do
  begin
    PixelisZero := false;
    while not PixelisZero do
    begin
      RandomPosition := Random(MatrixSize);
      if Matrix [RandomPosition] = 0 then
      begin
        PixelisZero := true;
        Matrix[RandomPosition] := $FF;
      end;
    end;
  end;
end;

procedure FadeMatrixPixelFade(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  MatrixSize: Cardinal;
  i, j: cardinal;
  PixelisZero: boolean;
  RandomPosition: Cardinal;
begin
  if percent = 0 then
    exit;
  MatrixSize := Width*Height;
  RandSeed := (MatrixSize);
  j := Round (MatrixSize * Percent/100);
  for i := 1 to j do
  begin
    PixelisZero := false;
    while not PixelisZero do
    begin
      RandomPosition := random (MatrixSize);
      if matrix [RandomPosition] = 0 then
      begin
        PixelisZero := true;
        matrix [RandomPosition] := Round((Percent/100)*$FF);
      end;
    end;
  end;
end;

procedure FadeMatrixPixelLine(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  D: PGRMatrixFade;
  MatrixSize: Cardinal;
  i, j: cardinal;
  PixelisZero: boolean;
  RandomPosition: Cardinal;
  DeltaX, Delta: Integer;
  DeltaP: Real;
begin
  if percent = 0 then
    exit;

  GetMem(D, Width * Height);
  FillChar(D^, Width*Height, 0);

  MatrixSize := Width*Height;
  RandSeed := (MatrixSize);
  j := Round (MatrixSize * Percent/100);

  for i := 1 to j do
  begin
    PixelisZero := false;
    while not PixelisZero do
    begin
      RandomPosition := random (MatrixSize);
      if D^ [RandomPosition] = 0 then
      begin
        PixelisZero := true;
        D^ [RandomPosition] := $FF;
      end;
    end;
  end;

  DeltaX := Round ((Percent/100)*(Width*2-1));
  DeltaP := (0.1*Width) / Percent;
  for i := 0 to DeltaX do
    for j := 0 to Height - 1 do
      begin
        if i < Width then
          begin
            Delta := Round (Percent*2-DeltaP*i);
            if Delta < 0 then
              Delta := 0;
            Delta := Round((Delta/100)*$FF);
            if Delta > $FF then
              Delta := $FF;
            if D^ [i + j*Width] <> $0 then
              Matrix [i + j*Width] :=  Delta;
          end;
    end;

  FreeMem(D, MatrixSize);
end;

{ Smooth Effects ==============================================================}

procedure FadeMatrixSmoothRectOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  Delta: integer;
  Center: TPoint;
  i: integer;
  DeltaP: real;
  Alpha: integer;
  RealX,RealY: Real;
  x,y: integer;
begin
  if Percent = 0 then
    exit;
  if Width > Height then
  begin
    RealX := 1;
    Delta := MulDiv(Percent, Round(Width*1.6), 100);
    RealY := Height / Width;
  end
  else
  begin
    RealY := 1;
    Delta := MulDiv(Percent, Round(Height*1.6), 100);
    RealX:= Width/Height;
  end;
  Center.X := Width div 2;
  Center.Y := Height div 2;
  if (Delta div 2) = 0 then
    DeltaP := (Percent*2)
  else
    DeltaP := (Percent*2)  / (Delta div 2);

  for i := 0 to Delta div 2 do
  begin
    Alpha := MulDiv (Round(Percent*2-i*DeltaP), $FF, 100);
    if Alpha > $FF then
      Alpha := $FF;
    if Percent = 100 then
      Alpha := $FF;
    X := Round (RealX*i);
    Y := Round (RealY * i);
    FadeMatrixLine(Matrix, Width, Height, Center.X-x, Center.Y+y,Center.X-x, Center.Y-y, Alpha);
    FadeMatrixLine(Matrix, Width, Height, Center.X-x, Center.Y-y,Center.X+x, Center.Y-y, Alpha);
    FadeMatrixLine(Matrix, Width, Height, Center.X+x, Center.Y-y,Center.X+x, Center.Y+y, Alpha);
    FadeMatrixLine(Matrix, Width, Height, Center.X+x, Center.Y+y,Center.X-x, Center.Y+y, Alpha);
  end;
end;

{ Circle animations ===========================================================}

procedure FadeCircleSlideOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i: integer;
  delta: integer;
begin
  delta := muldiv (round(sqrt(sqr(height)+sqr(width))), percent, 100) div 2;
  for i := 0 to delta do
    FadeMatrixCircle(matrix, width, height, width div 2, height div 2, i, $FF);
end;

procedure FadeCircleFadeOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i: integer;
  delta: integer;
begin
  delta := muldiv (round(sqrt(sqr(height)+sqr(width))), percent, 100) div 2;
  for i := 0 to delta do
    FadeMatrixCircle (matrix, width, height, width div 2, height div 2, i, muldiv (percent, $FF, 100));
end;

procedure FadeCircleSmoothOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i: integer;
  Delta: integer;
  Deltap: real;
  Alpha: integer;
begin
  if percent = 0 then
    exit;
  Delta := MulDiv (Round(Sqrt(Sqr(Height)+Sqr(Width))*0.9), Percent, 100);
  if Delta = 0 then
    DeltaP := (Percent*2)
  else
    DeltaP := (Percent*2) / Delta;
  for i := 0 to Delta do
  begin
    Alpha := MulDiv (Round(Percent*2-i*DeltaP), $FF, 100);
    if Alpha > $FF then
      Alpha := $FF;
    if Percent = 100 then
      Alpha := $FF;
    FadeMatrixCircle(Matrix, Width, Height, Width div 2, Height div 2, i, Alpha);
  end;
end;

procedure FadeCircleSlideIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i: integer;
  delta,size: integer;
begin
  delta := muldiv (round(sqrt(sqr(height)+sqr(width))), percent, 100) div 2;
  size := round(sqrt(sqr(height)+sqr(width))) div 2;
  for i := delta downto 0 do
    FadeMatrixCircle (matrix, width, height, width div 2, height div 2, size-i, $FF);
end;

procedure FadeCircleFadeIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i: integer;
  delta,size: integer;
begin
  delta := muldiv (round(sqrt(sqr(height)+sqr(width))), percent, 100) div 2;
  size := round(sqrt(sqr(height)+sqr(width))) div 2;
  for i := delta downto 0 do
    FadeMatrixCircle (matrix, width, height, width div 2, height div 2, size-i, muldiv (percent, $FF, 100));
end;

procedure FadeCircleSmoothIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i,j: integer;
begin
  if Percent = 0 then
    exit;
  FadeCircleSmoothOut (Matrix, Width, Height, 100-Percent);
  for i := 0 to Width-1 do
    for j := 0 to Height-1 do
      Matrix[i+j*Width] := $FF - Matrix[i+j*Width];
end;

procedure FadeCircleSlide(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i: integer;
  Delta: integer;
begin
  Delta := muldiv (round(sqrt(sqr(Height)+sqr(Width))), Percent, 100);
  for i := Delta downto 0 do
    FadeMatrixCircle (Matrix, Width, Height, 0, Height - 1, Delta - i, $FF);
end;

procedure FadeCircleFade(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i: integer;
  Delta: integer;
begin
  Delta := muldiv (round(sqrt(sqr(Height)+sqr(Width))), Percent, 100);
  for i := Delta downto 0 do
    FadeMatrixCircle (Matrix, Width, Height, 0, Height - 1, Delta - i, muldiv (Percent, $FF, 100));
end;

procedure FadeCircleSmooth(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i: integer;
  delta: integer;
  deltap: real;
  Alpha: integer;
begin
  if Percent = 0 then
    exit;
  Delta := MulDiv (round(sqrt(sqr(Height)+sqr(Width))*1.5), Percent, 100);
  DeltaP := (Percent*2)  / Delta;
  for i := 0 to delta do
  begin
    Alpha := MulDiv (Round(Percent*2-i*DeltaP), $FF, 100);
    if Alpha > $FF then
      Alpha := $FF;
    if Percent = 100 then
      Alpha := $FF;
    FadeMatrixCircle (Matrix, Width, Height, 0, Height - 1, i, Alpha);
  end;
end;

{ Diamond Transition ===========================================================}

procedure FadeDiamondSlideOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  Delta: integer;
  Center: TPoint;
  i: integer;
  Alpha: integer;
  RealX,RealY: Real;
  x,y: integer;
begin
  if Percent = 0 then
    exit;
  if Width > Height then
  begin
    RealX := 1;
    Delta := MulDiv (Percent, Width, 100);
    RealY := Height/Width;
  end
  else
  begin
    RealY := 1;
    Delta := MulDiv (Percent, Height, 100);
    RealX:= Width/Height;
  end;
  Center.X := Width div 2;
  Center.Y := Height div 2;
  for i := 0 to Delta do
  begin
    Alpha := $FF;
    X := Round (RealX*i);
    Y := Round (RealY * i);
    FadeMatrixLine(Matrix, Width, Height, Center.X, Center.Y+y, Center.X-x, Center.Y, Alpha);
    FadeMatrixLine(Matrix, Width, Height, Center.X-x, Center.Y, Center.X, Center.Y-y, Alpha);
    FadeMatrixLine(Matrix, Width, Height, Center.X, Center.Y-y, Center.X+x, Center.Y, Alpha);
    FadeMatrixLine(Matrix, Width, Height, Center.X+x, Center.Y, Center.X, Center.Y+y, Alpha);
  end;
end;

procedure FadeDiamondFadeOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  Delta: integer;
  Center: TPoint;
  i: integer;
  Alpha: integer;
  RealX,RealY: Real;
  x,y: integer;
begin
  if Percent = 0 then
    exit;
  if Width > Height then
  begin
    RealX := 1;
    Delta := MulDiv (Percent, Width, 100);
    RealY := Height/Width;
  end
  else
  begin
    RealY := 1;
    Delta := MulDiv (Percent, Height, 100);
    RealX:= Width/Height;
  end;
  Center.X := Width div 2;
  Center.Y := Height div 2;
  for i := 0 to Delta do
  begin
    Alpha := MulDiv ($FF, Percent, 100);
    X := Round (RealX*i);
    Y := Round (RealY * i);
    FadeMatrixLine (Matrix, Width, Height, Center.X, Center.Y+y, Center.X-x, Center.Y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X-x, Center.Y, Center.X, Center.Y-y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X, Center.Y-y, Center.X+x, Center.Y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X+x, Center.Y, Center.X, Center.Y+y, Alpha);
  end;
end;

procedure FadeDiamondSmoothOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  Delta: integer;
  Center: TPoint;
  i: integer;
  DeltaP: real;
  Alpha: integer;
  RealX,RealY: Real;
  x,y: integer;
begin
  if Percent = 0 then
    exit;
  if Width > Height then
  begin
    RealX := 1;
    Delta := MulDiv (Percent, Round (Width*2.5), 100);
    RealY := Height/Width;
  end
  else
  begin
    RealY := 1;
    Delta := MulDiv (Percent, Round (Height*2.5), 100);
    RealX:= Width/Height;
  end;
  Center.X := Width div 2;
  Center.Y := Height div 2;
  if (Delta div 2) = 0 then
    DeltaP := (Percent*2)
  else
    DeltaP := (Percent*2)  / (Delta div 2);
  for i := 0 to Delta div 2 do
  begin
    Alpha := MulDiv (Round(Percent*2-i*DeltaP), $FF, 100);
    if Alpha > $FF then
      Alpha := $FF;
    if Percent = 100 then
      Alpha := $FF;
    X := Round (RealX*i);
    Y := Round (RealY * i);
    FadeMatrixLine (Matrix, Width, Height, Center.X, Center.Y+y, Center.X-x, Center.Y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X-x, Center.Y, Center.X, Center.Y-y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X, Center.Y-y, Center.X+x, Center.Y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X+x, Center.Y, Center.X, Center.Y+y, Alpha);
  end;
end;

procedure FadeDiamondSlideIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i,j: integer;
begin
  if Percent = 0 then
    exit;
  FadeDiamondSlideOut (Matrix, Width, Height, 100-Percent);
  for i := 0 to Width-1 do
    for j := 0 to Height-1 do
      if Matrix[i+j*Width] = 0
      then
        Matrix[i+j*Width] := $FF
      else
        Matrix[i+j*Width] := 0;
end;

procedure FadeDiamondFadeIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i,j : integer;
begin
  if Percent = 0 then
    exit;
  FadeDiamondFadeOut (Matrix, Width, Height, 100-Percent);
  for i := 0 to Width-1 do
    for j := 0 to Height-1 do
      if Matrix[i+j*Width] = 0
      then
        Matrix[i+j*Width] := MulDiv (Percent, $FF,100)
      else
        Matrix[i+j*Width] := 0;
end;


procedure FadeDiamondSmoothIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i,j: integer;
begin
  if Percent = 0 then
    exit;
  FadeDiamondSmoothOut (Matrix, Width, Height, 100-Percent);
  for i := 0 to Width-1 do
    for j := 0 to Height-1 do
      Matrix[i+j*Width] := $FF - Matrix[i+j*Width];
end;

{ Other Transition =============================================================}

procedure FadePlasma(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  SinLut,CosLut: array[0..2047]of Integer;
  Colors: array[0..2047]of byte;
  i: integer;
  y,x: integer;
  a: integer;
  xx,yy: integer;
  Keys: array [0..12] of byte;
  PlasmaWidth: integer;
procedure FillColors(i1,i2,nKeys:Integer);
var
  c1,c2: byte;
  i,n,cs,w1,w2,x,ii: Integer;
begin
  i:=0;
  n:=i2-i1;
  Dec(nKeys);
  ii:=(nKeys shl 16)div n;
  for x:=0 to n-1 do
  begin
    cs:=i shr 16;
    c1:=Keys[cs];
    if cs<nKeys then Inc(cs);
    c2:=Keys[cs];
    w1:=((not i)and $FFFF)+1;
    w2:=i and $FFFF;
    if(w1<(ii-w1))
    then
       Colors[x] := c2
    else
      if (w2<(ii-w2))
      then
         Colors[x]:=c1
      else
        Colors[x]:=((c1*w1)+(c2*w2))shr 16;
    Inc(i,ii);
  end;
  Colors[x] := c2;
end;
begin
  PlasmaWidth := 512;
  for i:=0 to PlasmaWidth - 1 do
  begin
    SinLut[i]:=(Trunc(Sin(2*Pi*i/PlasmaWidth)*(PlasmaWidth div 2))+PlasmaWidth div 2)and (PlasmaWidth-1);
    CosLut[i]:=(Trunc(Cos(2*Pi*i/PlasmaWidth)*(PlasmaWidth div 2))+PlasmaWidth div 2)and (PlasmaWidth-1);
  end;
  Keys[0]:=255;
  Keys[1]:=255;
  Keys[2]:=0;
  Keys[3]:=0;
  Keys[4]:=255;
  Keys[5]:=255;
  Keys[6]:=0;
  Keys[7]:=0;
  Keys[8]:=255;
  Keys[9]:=255;
  Keys[10]:=0;
  Keys[11]:=0;
  Keys[12]:=255;
  FillColors(0,PlasmaWidth-1,13);
  for y:=0 to Height-1 do
    begin
      xx:=SinLut[(y)and (PlasmaWidth-1)];
      yy:=CosLut[(y)and (PlasmaWidth-1)];
      for x:=0 to Width-1 do
      begin
        a:=MulDiv (Percent*2,255,100)-Colors[(SinLut[(x+xx)and (PlasmaWidth-1)]+yy)and (PlasmaWidth-1)];
        if a < 0 then
          a := 0;
        if a > 255 then
          a := 255;
        Matrix [x+y*Width] := a;
      end;
    end;
end;

procedure FadeStream(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
type
  PVector = ^TVector;
  TVector = array[0..0] of Integer;
var
  i,j: Word;
  YMax: PVector;
  Yy: PVector;
begin
  GetMem (YMax, Width*Sizeof (integer));
  GetMem (Yy, Width*Sizeof (integer));
  RandSeed := Width*Height;
  for i:=0 to Width - 1 do
  begin
    yy^ [i]:=-Random(80);
    YMax^ [i]:=Height - 1;
  end;
  for j:=1 to 8 do
    for I:=j to Width-1-j do
      yy^ [i]:=(yy^[i-1] + yy^[i]*2 + yy^[i+1]) div 4;

  for i:=0 to MulDiv (Height+79, Percent, 100) do
  begin
    for j:=0 to Width-1 do
    begin
      if yy^ [j] <= YMax^ [j] then
      begin
        if yy^ [j] >= 0 then
          Matrix [j + Width*yy^[j]] := $FF;
        Inc (yy^ [j]);
      end
      else
      begin
        yy^ [j] := 0;
        Dec (YMax^ [j]);
      end;
    end;
  end;
  FreeMem (YMax);
  FreeMem (Yy);
end;

{ Rotate Effects ==============================================================}

procedure FadeRotateSlide(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i: integer;
  size: integer;
begin
  if percent = 0 then
    exit;

  size := MulDiv (height + width, percent, 100);
  for i := 0 to size do
  begin
    if i < Height then
      FadeMatrixLine(Matrix, Width, Height, 0, height-1, Width-1, height-i, $FF)
    else
      FadeMatrixLine(Matrix, Width, Height, 0, height-1, width-(size-i), 0, $FF);
  end;
end;

procedure FadeRotateFade(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i: integer;
  size: integer;
begin
  if percent = 0 then
    exit;

  size := MulDiv (height + width, percent, 100);
  for i := 0 to size do
  begin
    if i < Height then
      FadeMatrixLine (Matrix, Width, Height, 0, height-1, Width-1, height-i, MulDiv (Percent, $FF, 100))
    else
      FadeMatrixLine (Matrix, Width, Height, 0, height-1, width-(size-i), 0, MulDiv (Percent, $FF, 100));
  end;
end;

procedure FadeRotateSmooth(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i: integer;
  Delta: integer;
  Deltap: real;
  Alpha: Integer;
begin
  if percent = 0 then
    exit;

  Delta := MulDiv ((Height + Width)*2, percent, 100);
  DeltaP := (Percent*2)  / Delta;
  for i := 0 to Delta do
  begin
    Alpha := MulDiv (Round(Percent*2-i*DeltaP), $FF, 100);
    if Alpha > $FF then
      Alpha := $FF;
    if Percent = 100 then
      Alpha := $FF;
    if i < Height then
      FadeMatrixLine (Matrix, Width, Height, 0, height-1, Width-1, height-i, Alpha)
    else
      FadeMatrixLine (Matrix, Width, Height, 0, height-1, width-(i-height), 0, Alpha);
  end;
end;

{ Slide Effects ===============================================================}

procedure FadeSlideDiagonal(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  DeltaX, DeltaY: integer;
  x, y: integer;
begin

  if percent = 0 then
    exit;

  DeltaX := MulDiv ((Width-1), Percent, 100) * 2;
  DeltaY := MulDiv ((Height-1), Percent, 100) * 2;

  if DeltaX > DeltaY then
  begin
    y := 0;
    for x := 0 to DeltaX do
    begin
      FadeMatrixLine(Matrix, Width, Height, 0, y, x, 0, $FF);
      if y < DeltaY then
        Inc (y);
    end;
  end
  else
  begin
    x := 0;
    for y := 0 to DeltaY do
    begin
      FadeMatrixLine(Matrix, Width, Height, 0, y, x, 0, $FF);
      if x < DeltaX then
        Inc(x);
    end;
  end;
end;

procedure FadeSlideDiagonalIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  DeltaX, DeltaY: integer;
  x, y: integer;
begin

  if percent = 0 then
    exit;

  DeltaX := Round ((Percent/100)*(Width-1));
  DeltaY := Round ((Percent/100)*(Height-1));

  if DeltaX > DeltaY then
  begin
    y := 0;
    for x := 0 to DeltaX do
    begin
      FadeMatrixLine(Matrix, Width, Height, 0, Height-y-1, x, Height-1, $FF);
      if y < DeltaY then
        Inc (y);
    end;
    y := 0;
    for x := Width - 1 downto Width - DeltaX - 1 do
    begin
      FadeMatrixLine(Matrix, Width, Height, x, 0, width-1, y, $FF);
      if y < DeltaY then
        Inc (y);
    end;
  end
  else
  begin
    x := 0;
    for y := 0 to DeltaY do
    begin
      FadeMatrixLine(Matrix, Width, Height, 0, Height-y-1, x, Height-1, $FF);
      if x < DeltaX then
        Inc (x);
    end;
    x := Width - 1;
    for y := 0 to DeltaY do
    begin
      FadeMatrixLine(Matrix, Width, Height, x, 0, Width-1, y, $FF);
      if x > Width - 1 - DeltaX then
        Dec (x);
    end;
  end;
end;

procedure FadeSlideDiagonalOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  DeltaX, DeltaY: integer;
  x, y: integer;
begin
  if percent = 0 then
    exit;
  DeltaX := Round ((Percent/100)*(Width-1));
  DeltaY := Round ((Percent/100)*(Height-1));
  if DeltaX > DeltaY then
  begin
    y := 0;
    for x := 0 to DeltaX do
    begin
      FadeMatrixLine (Matrix, Width, Height, 0, Y, Width - x -1, Height - 1, $FF);
      if y < DeltaY then
        Inc (y);
    end;
    y := 0;
    for x := 0 to DeltaX do
    begin
      FadeMatrixLine (Matrix, Width, Height, x, 0, Width, Height - y, $FF);
      if y < DeltaY then
        Inc (y);
    end;
  end
  else
  begin
    x := 0;
    for y := 0 to DeltaY do
    begin
      FadeMatrixLine (Matrix, Width, Height, x, 0, Width - 1, Height - y -1, $FF);
      if x < DeltaX then
        Inc (x);
    end;
    x := 0;
    for y := 0 to DeltaY do
    begin
      FadeMatrixLine (Matrix, Width, Height, 0, y, Width-x-1, height-1, $FF);
      if x < DeltaX then
        Inc (x);
    end;
  end;
end;

procedure FadeSlideDown(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  j: integer;
begin
  if percent = 0 then
    exit;
  for j := 0 to MulDiv(Percent,Height-1,100) do
    FadeMatrixLine (Matrix, Width, Height, 0, j, Width-1, j, $FF);
end;

procedure FadeSlideIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  j: integer;
begin
  if percent = 0 then
    exit;
  for j := Height-1 downto Height - 1 - MulDiv(Percent,Height div 2,100) do
    FadeMatrixLine (Matrix, Width, Height, 0, j, Width-1, j, $FF);
  for j := 0 to MulDiv(Percent,Height div 2,100) do
    FadeMatrixLine (Matrix, Width, Height, 0, j, Width-1, j, $FF);
end;

procedure FadeSlideOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  j: integer;
begin
  if percent = 0 then
    exit;
  for j := 0 to MulDiv(Percent,Height div 2,100) do
  begin
    FadeMatrixLine (Matrix, Width, Height, 0, Height div 2 - j, Width-1, Height div 2 - j, $FF);
    FadeMatrixLine (Matrix, Width, Height, 0, Height div 2 + j, Width-1, Height div 2 + j, $FF);
  end;
end;

procedure FadeSlideCrossOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  DeltaX, DeltaY: integer;
  Center: TPoint;
  i: integer;
begin
  if Percent = 0 then
    exit;
  DeltaX := MulDiv (Percent, Width*2, 100);
  DeltaY := MulDiv (Percent, Height*2, 100);
  Center.X := Width div 2;
  Center.Y := Height div 2;
  for i := -DeltaX div 4 to DeltaX div 4 do
    FadeMatrixLine (Matrix, Width, Height, Center.X+i, Center.Y-DeltaY,Center.X+i, Center.Y+DeltaY, $FF);
  for i := -DeltaY div 4 to DeltaY div 4 do
    FadeMatrixLine (Matrix, Width, Height, Center.X-DeltaX, Center.Y+i,Center.X+DeltaX, Center.Y+i, $FF);
end;

procedure FadeSlideCrossIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i,j: integer;
begin
  if Percent = 0 then
    exit;
  FadeSlideCrossOut (Matrix, Width, Height, 100-Percent);
  for i := 0 to Width-1 do
    for j := 0 to Height-1 do
      if Matrix[i+j*Width] = 0
      then
        Matrix[i+j*Width] := $FF
      else
        Matrix[i+j*Width] := 0;
end;

procedure FadeSlideRectOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  Delta: integer;
  Center: TPoint;
  i: integer;
  Alpha: integer;
  RealX,RealY: Real;
  x,y: integer;
begin
  if Percent = 0 then
    exit;
  if Width > Height then
  begin
    RealX := 1;
    Delta := MulDiv (Percent, Round (Width*1), 100);
    RealY := Height/Width;
  end
  else
  begin
    RealY := 1;
    Delta := MulDiv (Percent, Round (Height*1), 100);
    RealX:= Width/Height;
  end;
  Center.X := Width div 2;
  Center.Y := Height div 2;
  for i := 0 to Delta div 2 do
  begin
    Alpha := $FF;
    X := Round (RealX*i);
    Y := Round (RealY * i);
    FadeMatrixLine (Matrix, Width, Height, Center.X-x, Center.Y+y,Center.X-x, Center.Y-y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X-x, Center.Y-y,Center.X+x, Center.Y-y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X+x, Center.Y-y,Center.X+x, Center.Y+y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X+x, Center.Y+y,Center.X-x, Center.Y+y, Alpha);
  end;
end;

procedure FadeSlideRectIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i,j: integer;
begin
  if Percent = 0 then
    exit;
  FadeSlideRectOut (Matrix, Width, Height, 100-Percent);
  for i := 0 to Width-1 do
    for j := 0 to Height-1 do
      if Matrix[i+j*Width] = 0
      then
        Matrix[i+j*Width] := $FF
      else
        Matrix[i+j*Width] := 0;
end;

{ Smooth Effects ==============================================================}

procedure FadeSmoothDiagonal(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  Delta: integer;
  DeltaP: Real;
  x, y, i: integer;
  RealX,RealY: Real;
  Alpha: integer;
begin

  if Percent = 0 then
    exit;

  if Width > Height then
  begin
    RealX := 1;
    Delta := MulDiv (Percent, Round(Sqrt(Sqr(Height)+Sqr(Width))*2.5), 100);
    RealY := Height/Width;
  end
  else
  begin
    RealY := 1;
    Delta := MulDiv (Percent, Round(Sqrt(Sqr(Height)+Sqr(Width))*2.5), 100);
    RealX:= Width/Height;
  end;
  if Delta = 0 then
    DeltaP := (Percent*2)
  else
    DeltaP := (Percent*2)  / Delta;

  for i := 0 to Delta do
  begin
    Alpha := MulDiv (Round(Percent*2-i*DeltaP), $FF, 100);
    if Alpha > $FF then
      Alpha := $FF;
    if Percent = 100 then
      Alpha := $FF;
    x := Round (RealX*i);
    y := Round (RealY * i);
    FadeMatrixLine (Matrix, Width, Height, 0, y, x, 0, Alpha);
  end;
end;

procedure FadeSmoothDown(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  Delta: Integer;
  DeltaP: Real;
  j: Integer;
  Alpha: integer;
begin
  if Percent = 0 then
    exit;
  Delta := MulDiv (Round(Height*1.5), Percent, 100);
  if Delta = 0 then
    DeltaP := (Percent*2)
  else
    DeltaP := (Percent*2)  / Delta;
  for j := 0 to Delta do
  begin
    Alpha := MulDiv (Round(Percent*2-j*DeltaP), $FF, 100);
    if Alpha > $FF then
      Alpha := $FF;
    if Percent = 100 then
      Alpha := $FF;
    FadeMatrixLine (Matrix, Width, Height, 0, j, Width-1, j, Alpha);
  end;
end;

procedure FadeSmoothIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  Delta: Integer;
  DeltaP: Real;
  j: Integer;
  Alpha: integer;
begin
  if Percent = 0 then
    exit;
  Delta := MulDiv (Round(Height*0.8), Percent, 100);
  if Delta = 0 then
    DeltaP := (Percent*2)
  else
    DeltaP := (Percent*2)  / Delta;
  for j := 0 to Delta do
  begin
    Alpha := MulDiv (Round(Percent*2-j*DeltaP), $FF, 100);
    if Alpha > $FF then
      Alpha := $FF;
    if Percent = 100 then
      Alpha := $FF;
    if j <= Height div 2 then
    begin
      FadeMatrixLine (Matrix, Width, Height, 0, Height-j-1, Width-1, Height-j-1, Alpha);
      FadeMatrixLine (Matrix, Width, Height, 0, j, Width-1, j, Alpha);
    end;
  end;
end;

procedure FadeSmoothOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  Delta: Integer;
  DeltaP: Real;
  j: Integer;
  Alpha: integer;
begin
  if Percent = 0 then
    exit;
  Delta := MulDiv (Round(Height*0.8), Percent, 100);
  if Delta = 0 then
    DeltaP := (Percent*2)
  else
    DeltaP := (Percent*2)  / Delta;
  for j := 0 to Delta do
  begin
    Alpha := MulDiv (Round(Percent*2-j*DeltaP), $FF, 100);
    if Alpha > $FF then
      Alpha := $FF;
    if Percent = 100 then
      Alpha := $FF;
    if j <= Height div 2 then
    begin
      FadeMatrixLine (Matrix, Width, Height, 0, Height div 2 - j, Width-1, Height div 2 - j, Alpha);
      FadeMatrixLine (Matrix, Width, Height, 0, Height div 2 + j, Width-1, Height div 2 + j, Alpha);
    end;
  end;
end;

procedure FadeSmoothDiagonalIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  Delta,i: integer;
  DeltaX, DeltaY: real;
  x,y: real;
  DeltaP: real;
  Size: integer;
  Alpha:integer;
begin

  if percent = 0 then
    exit;

  if Height > Width then
  begin
    DeltaY := 1;
    DeltaX := Width/Height;
    Size:=Height;
  end
  else
  begin
    DeltaX := 1;
    DeltaY := Height/Width;
    Size:=Width;
  end;
  Delta := MulDiv (round(sqrt(sqr(Height)+sqr(Width))), Percent, 100);
  if Delta = 0 then
    DeltaP := (Percent*2)
  else
    DeltaP := (Percent*2)  / Delta;
  x := 0;
  y := 0;
  Delta := Round (Delta*2);

  for i := 0 to Delta do
  begin
    Alpha := MulDiv(Round(2*Percent-DeltaP*i/2), $FF, 100);
    if Alpha < 0 then
      Alpha := 0;
    if Alpha > $FF then
      Alpha := $FF;
    if Percent = 100 then
      Alpha := $FF;

    if i < Size then
    begin
      FadeMatrixLine (Matrix, Width, Height, 0, Height-1-Round(y), Round(x), Height-1, Alpha);
      FadeMatrixLine (Matrix, Width, Height, Width-Round(x), 0, Width-1, Round(y), Alpha);
    end;

    x := x + DeltaX;
    y := y + DeltaY;
  end;
end;

procedure FadeSmoothDiagonalOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  Delta,i: integer;
  DeltaX, DeltaY: real;
  x,y: real;
  DeltaP: real;
  Size: integer;
  Alpha:integer;
begin
  if percent = 0 then
    exit;

  if Height > Width then
  begin
    DeltaY := 1;
    DeltaX := Width/Height;
    Size:=Height;
  end
  else
  begin
    DeltaX := 1;
    DeltaY := Height/Width;
    Size:=Width;
  end;
  Delta := MulDiv (round(sqrt(sqr(Height)+sqr(Width))), Percent, 100);
  if Delta = 0 then
    DeltaP := (Percent*2)
  else
    DeltaP := (Percent*2)  / Delta;
  x := 0;
  y := 0;
  Delta := Round (Delta*2);

  for i := 0 to Delta do
  begin
    Alpha := MulDiv(Round(2*Percent-DeltaP*i/2), $FF, 100);
    if Alpha < 0 then
      Alpha := 0;
    if Alpha > $FF then
      Alpha := $FF;
    if Percent = 100 then
      Alpha := $FF;
    if i < Size then
    begin
      FadeMatrixLine (Matrix, Width, Height, 0, Height-1-Round(y), Width - Round(x), 0, Alpha);
      FadeMatrixLine (Matrix, Width, Height, Round(x), Height-1, Width-1, Round(y), Alpha);
    end;

    x := x + DeltaX;
    y := y + DeltaY;

  end;
end;

procedure FadeSmoothCrossOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  Delta: integer;
  Center: TPoint;
  i: integer;
  DeltaP: real;
  Alpha: integer;
  RealX,RealY: Real;
  x,y: integer;
begin
  if Percent = 0 then
    exit;

  if Width > Height then
  begin
    RealX := 1;
    Delta := MulDiv (Percent, Round (Width*2.5), 100);
    RealY := Height/Width;
  end
  else
  begin
    RealY := 1;
    Delta := MulDiv (Percent, Round (Height*2.5), 100);
    RealX:= Width/Height;
  end;
  Center.X := Width div 2;
  Center.Y := Height div 2;
  if (Delta div 2) = 0 then
    DeltaP := (Percent*2)
  else
    DeltaP := (Percent*2)  / (Delta div 2);
  for i := 0 to Delta div 2 do
  begin
    Alpha := MulDiv (Round(Percent*2-i*DeltaP), $FF, 100);
    if Alpha > $FF then
      Alpha := $FF;
    if Percent = 100 then
      Alpha := $FF;
    X := Round (RealX*i);
    Y := Round (RealY * i);
    FadeMatrixLine (Matrix, Width, Height, Center.X-x div 2, Center.Y+y,Center.X-x div 2, Center.Y+y div 2, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X-x div 2, Center.Y+y div 2,Center.X-x, Center.Y+y div 2, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X-x, Center.Y+y div 2,Center.X-x, Center.Y-y div 2, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X-x, Center.Y-y div 2,Center.X-x div 2, Center.Y-y div 2, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X-x div 2, Center.Y-y div 2,Center.X-x div 2, Center.Y-y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X-x div 2, Center.Y-y,Center.X+x div 2, Center.Y-y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X+x div 2, Center.Y-y,Center.X+x div 2, Center.Y-y div 2, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X+x div 2, Center.Y-y div 2,Center.X+x, Center.Y-y div 2, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X+x, Center.Y-y div 2,Center.X+x, Center.Y+y div 2, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X+x, Center.Y+y div 2,Center.X+x div 2, Center.Y+y div 2, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X+x div 2, Center.Y+y div 2,Center.X+x div 2, Center.Y+y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X+x div 2, Center.Y+y,Center.X-x div 2, Center.Y+y, Alpha);
  end;
end;

procedure FadeSmoothCrossIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i,j: integer;
begin
  if Percent = 0 then
    exit;
  FadeSmoothCrossOut (Matrix, Width, Height, 100-Percent);
  for i := 0 to Width-1 do
    for j := 0 to Height-1 do
      Matrix[i+j*Width] := $FF - Matrix[i+j*Width];
end;

procedure FadeSmoothRectOut(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  Delta: integer;
  Center: TPoint;
  i: integer;
  DeltaP: real;
  Alpha: integer;
  RealX,RealY: Real;
  x,y: integer;
begin
  if Percent = 0 then
    exit;
  if Width > Height then
  begin
    RealX := 1;
    Delta := MulDiv (Percent, Round (Width*1.6), 100);
    RealY := Height/Width;
  end
  else
  begin
    RealY := 1;
    Delta := MulDiv (Percent, Round (Height*1.6), 100);
    RealX:= Width/Height;
  end;
  Center.X := Width div 2;
  Center.Y := Height div 2;
  if (Delta div 2) = 0 then
    DeltaP := (Percent*2)
  else
    DeltaP := (Percent*2)  / (Delta div 2);
  for i := 0 to Delta div 2 do
  begin
    Alpha := MulDiv (Round(Percent*2-i*DeltaP), $FF, 100);
    if Alpha > $FF then
      Alpha := $FF;
    if Percent = 100 then
      Alpha := $FF;
    X := Round (RealX*i);
    Y := Round (RealY * i);
    FadeMatrixLine (Matrix, Width, Height, Center.X-x, Center.Y+y,Center.X-x, Center.Y-y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X-x, Center.Y-y,Center.X+x, Center.Y-y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X+x, Center.Y-y,Center.X+x, Center.Y+y, Alpha);
    FadeMatrixLine (Matrix, Width, Height, Center.X+x, Center.Y+y,Center.X-x, Center.Y+y, Alpha);
  end;
end;

procedure FadeSmoothRectIn(var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);
var
  i,j: integer;
begin
  if Percent = 0 then
    exit;
  FadeSmoothRectOut (Matrix, Width, Height, 100-Percent);
  for i := 0 to Width-1 do
    for j := 0 to Height-1 do
      Matrix[i+j*Width] := $FF - Matrix[i+j*Width];
end;

procedure TGRFadeTransitionParam.Init;
begin
  if Assigned(FOwner) then
  with FOwner^ do
  begin
    MatrixWidth := SourceImage.Width div Transition.Resolution div Transition.TileCount + 1;
    MatrixHeight := SourceImage.Height div Transition.Resolution div Transition.TileCount + 1;
    MatrixLen := MatrixWidth * MatrixHeight;
    GetMem(MatrixFade, MatrixLen);
    { Create copy for rotatation }
    if Transition.Rotation <> trNone then GetMem(CopyMatrixFade, MatrixLen);
  end;
end;

destructor TGRFadeTransitionParam.Destroy; 
begin
  //if Assigned(FOwner) then
  begin
    if Assigned(MatrixFade) then FreeMem(MatrixFade);
    //FreeMem(MatrixFade, MatrixLen);
    if Assigned(CopyMatrixFade) then FreeMem(CopyMatrixFade);
    //FreeMem(CopyMatrixFade, MatrixLen);
  end;
  inherited;
end;

constructor TGRBitmapTransitionParam.Create(aOwner: PGRTransitionRec = nil);
begin
  inherited Create(aOwner);
  //FBitmap := TBitmap.Create;
end;

destructor TGRBitmapTransitionParam.Destroy; 
begin
  //FBitmap.Free;
  inherited;
end;

procedure TGRBitmapTransitionParam.Init;
var
  MaskBitmap: TBitmap32;
  LMask: TBitmap32;
  i,j: integer;
begin
  if Assigned(FOwner) then
  with FOwner^ do
  begin
    MatrixLen := MatrixWidth * MatrixHeight;
    GetMem(MatrixFade, MatrixLen);
    GetMem(CopyMatrixFade, MatrixLen);

    { Copy from bitmap }
    MaskBitmap := TBitmap32.Create;
    LMask := TBitmap32.Create;
    try
      MaskBitmap.Assign(Transition.Bitmap);
      if Transition.Rotation in [trRotate90, trRotate270] then
      begin
        LMask.SetSize(MatrixHeight, MatrixWidth);
        MaskBitmap.DrawTo(LMask, MakeRect(0, 0, MatrixHeight, MatrixWidth));
      end
      else
      begin
        LMask.SetSize(MatrixWidth, MatrixHeight);
        MaskBitmap.DrawTo(LMask, MakeRect(0, 0, MatrixWidth, MatrixHeight));
      end;

      for i := 0 to MatrixWidth - 1 do
        for j := 0 to MatrixHeight - 1 do
        begin
          MatrixFade^[i + j * MatrixWidth] := LMask.Bits[i + j * MatrixWidth] and not $FFFFFF00;
          CopyMatrixFade^[i + j * MatrixWidth] := MatrixFade^[i + j * MatrixWidth];
        end;

    finally
      LMask.Free;
      MaskBitmap.Free;
    end;
  end;
end;

{procedure TGRBitmapTransitionParam.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;
}

procedure CalcFrameFade(aTransitionRec: TGRTransitionRec;
  Percent: byte);
var
  i, j: integer; { loop variables }
  DstRect: TRect;
begin
  { MatrixFade's Transition }
  with aTransitionRec, TGRFadeTransitionParam(Param) do
  begin
    { Clear matrix }
    if Transition.Rotation <> trNone then
    begin
      { Need use copymatrix for rotation }
      FillChar(CopyMatrixFade^, MatrixHeight * MatrixWidth, 0);
      if Transition.Rotation in [trRotate90, trRotate270] then
        TGRTransitionProcFade(ProcItem.Proc)(CopyMatrixFade^, MatrixHeight, MatrixWidth, Percent)
      else
        TGRTransitionProcFade(ProcItem.Proc)(CopyMatrixFade^, MatrixWidth, MatrixHeight, Percent);
    end
    else
    begin
      FillChar(MatrixFade^, MatrixHeight * MatrixWidth, 0);
      TGRTransitionProcFade(ProcItem.Proc)(MatrixFade^, MatrixWidth, MatrixHeight, Percent);
    end;

    { Matrix rotation }
    if Transition.Rotation = trRotate90 then
    begin
      for i := 0 to MatrixWidth - 1 do
        for j := 0 to MatrixHeight - 1 do
          MatrixFade^[i + (MatrixHeight - j - 1) * MatrixWidth] := CopyMatrixFade^[j + i * MatrixHeight];
    end;

    if Transition.Rotation = trRotate180 then
    begin
      for i := 0 to MatrixWidth - 1 do
        for j := 0 to MatrixHeight - 1 do
          MatrixFade^[(MatrixWidth - i - 1)  + (MatrixHeight - j - 1) * MatrixWidth] := CopyMatrixFade^[i + j * MatrixWidth];
    end;

    if Transition.Rotation = trRotate270 then
    begin
      for i := 0 to MatrixWidth - 1 do
        for j := 0 to MatrixHeight - 1 do
          MatrixFade^[(MatrixWidth - i - 1) + j * MatrixWidth] := CopyMatrixFade^[j + i * MatrixHeight];
    end;

    { Apply Matrix }
    for i := 0 to Transition.TileCount - 1 do
      for j := 0 to Transition.TileCount - 1 do
      begin
        DstRect := MakeRect(0, 0, MatrixWidth * Transition.Resolution, MatrixHeight * Transition.Resolution);
        OffsetRect(DstRect, i * RectWidth(DstRect), j * RectHeight(DstRect));

        StretchAlphaRect(DestImage.Bits^, DestImage.Width, DestImage.Height,
          DstRect.Left, DstRect.Top, RectWidth(DstRect), RectHeight(DstRect),
          0, 0, MatrixWidth, MatrixHeight, MatrixFade^);
      end;

    { Clear alpha }
    ClearAlphaFunc(DestImage.Bits, DestImage.Width * DestImage.Height, GRTransparent32);

    { Blending }
    DestImage.DrawMode := dmBlend;
    DestImage.DrawTo(ResultImage, 0, 0);
  end;
end;

procedure RegisterFadeTransition(
  const aName: string; 
  const aProc: TGRTransitionProcFade;
  const aTitle: string = ''; 
  aParamClass: TGRTransitionParamClass = nil
);
begin
  if (aParamClass = nil) or not aParamClass.inheritsFrom(TGRFadeTransitionParam) then aParamClass := TGRFadeTransitionParam;
  RegisterTransition(aName, 'Fade', TGRTransitionProc(@aProc), aTitle, aParamClass);
end;

procedure RegisterBitmapTransition(
  const aName: string; 
  const aProc: TGRTransitionProc;
  const aTitle: string = ''; 
  aParamClass: TGRTransitionParamClass = nil
);
begin
  if (aParamClass = nil) or not aParamClass.inheritsFrom(TGRBitmapTransitionParam) then 
    aParamClass := TGRBitmapTransitionParam;
  RegisterTransition(aName, 'Bitmap', TGRTransitionProc(@aProc), aTitle, aParamClass);
end;

procedure BitmapProc;
begin
end;

procedure CalcFrameBitmap(aTransitionRec: TGRTransitionRec;
  Percent: byte);
var
  i, j, Value: integer; { loop variables }
  DstRect: TRect;
begin
  //showmessage('ddd'+ inttostr(Percent));
  { Bitmap's Transition }
  with aTransitionRec, TGRBitmapTransitionParam(Param)  do
  begin
    { Change Matrix }
    for i := 0 to MatrixWidth - 1 do
      for j := 0 to MatrixHeight - 1 do
      begin
        { Read default value }
        if Transition.Rotation = trNone then
          Value := CopyMatrixFade^[i + j * MatrixWidth]
        else
          if Transition.Rotation = trRotate90 then
            Value := CopyMatrixFade^[j + i * MatrixHeight]
          else
            if Transition.Rotation = trRotate180 then
              Value := CopyMatrixFade^[(MatrixWidth - i - 1) + (MatrixHeight - j - 1) * MatrixWidth]
            else
              Value := CopyMatrixFade^[(MatrixHeight - j - 1) + i * MatrixHeight];

        { Calc new value }
        Inc(Value, MulDiv($1FE , Percent, 100));
        Dec(Value, $FF);

        if Value < 0 then Value := 0;
        if Value > $FF then Value := $FF;

        { Smoth level }
  //      if Value > 0 then Value := $FF;

        { Write value }
        MatrixFade^[i + j * MatrixWidth] := Value;
      end;

    { Apply Matrix }
    for i := 0 to Transition.TileCount - 1 do
      for j := 0 to Transition.TileCount - 1 do
      begin
        DstRect := MakeRect(0, 0, MatrixWidth * Transition.Resolution, MatrixHeight * Transition.Resolution);
        OffsetRect(DstRect, i * RectWidth(DstRect), j * RectHeight(DstRect));

        StretchAlphaRect(DestImage.Bits^, DestImage.Width, DestImage.Height,
          DstRect.Left, DstRect.Top, RectWidth(DstRect), RectHeight(DstRect),
          0, 0, MatrixWidth, MatrixHeight, MatrixFade^);
      end;

    { Clear alpha }
    ClearAlphaFunc(DestImage.Bits, DestImage.Width * DestImage.Height, GRTransparent32);

    { Blending }
    DestImage.DrawMode := dmBlend;
    DestImage.DrawTo(ResultImage, 0, 0);
    //DestImage.SaveToFile();
  end;
end;

initialization

  RegisterTransitionType('Fade', CalcFrameFade);
  {//TODO: it's not work yet.
  RegisterTransitionType('Bitmap', CalcFrameBitmap);
  RegisterBitmapTransition('Bitmap', BitmapProc);
  //}

  { Register ProcFade }
  RegisterFadeTransition('SmoothDiagonal', FadeSmoothDiagonal);
  RegisterFadeTransition('SmoothDiagonalIn', FadeSmoothDiagonalIn);
  RegisterFadeTransition('SmoothDiagonalOut', FadeSmoothDiagonalOut);
  RegisterFadeTransition('SmoothDown', FadeSmoothDown);
  RegisterFadeTransition('SmoothIn', FadeSmoothIn);
  RegisterFadeTransition('SmoothOut', FadeSmoothOut);
  RegisterFadeTransition('SmoothCrossOut', FadeSmoothCrossOut);
  RegisterFadeTransition('SmoothCrossIn', FadeSmoothCrossIn);
  RegisterFadeTransition('SmoothRectIn', FadeSmoothRectIn);
  RegisterFadeTransition('SmoothRectOut', FadeSmoothRectOut);

  RegisterFadeTransition('SlideDiagonal', FadeSlideDiagonal);
  RegisterFadeTransition('SlideDiagonalIn', FadeSlideDiagonalIn);
  RegisterFadeTransition('SlideDiagonalOut', FadeSlideDiagonalOut);
  RegisterFadeTransition('SlideDown', FadeSlideDown);
  RegisterFadeTransition('SlideIn', FadeSlideIn);
  RegisterFadeTransition('SlideOut', FadeSlideOut);
  RegisterFadeTransition('SlideCrossOut', FadeSlideCrossOut);
  RegisterFadeTransition('SlideCrossIn', FadeSlideCrossIn);
  RegisterFadeTransition('SlideRectOut', FadeSlideRectOut);
  RegisterFadeTransition('SlideRectIn', FadeSlideRectIn);

  RegisterFadeTransition('Fade', FadeMatrixFade);
  RegisterFadeTransition('FadeDiagonal', FadeMatrixDiagonal);
  RegisterFadeTransition('FadeDiagonalIn', FadeMatrixDiagonalIn);
  RegisterFadeTransition('FadeDiagonalOut', FadeMatrixDiagonalOut);
  RegisterFadeTransition('FadeDown', FadeMatrixDown);
  RegisterFadeTransition('FadeIn', FadeMatrixIn);
  RegisterFadeTransition('FadeOut', FadeMatrixOut);
  RegisterFadeTransition('FadeCrossOut', FadeMatrixCrossOut);
  RegisterFadeTransition('FadeCrossIn', FadeMatrixCrossIn);
  RegisterFadeTransition('FadeRectOut', FadeMatrixRectOut);
  RegisterFadeTransition('FadeRectIn', FadeMatrixRectIn);

  RegisterFadeTransition('Pixel', FadeMatrixPixel);
  RegisterFadeTransition('Pixel fade', FadeMatrixPixelFade);
  RegisterFadeTransition('Pixel line', FadeMatrixPixelLine);

  RegisterFadeTransition('CircleSlideOut', FadeCircleSlideOut);
  RegisterFadeTransition('CircleFadeOut', FadeCircleFadeOut);
  RegisterFadeTransition('CircleSmoothOut', FadeCircleSmoothOut);
  RegisterFadeTransition('CircleSlideIn', FadeCircleSlideIn);
  RegisterFadeTransition('CircleFadeIn', FadeCircleFadeIn);
  RegisterFadeTransition('CircleSmoothIn', FadeCircleSmoothIn);
  RegisterFadeTransition('CircleSlide', FadeCircleSlide);
  RegisterFadeTransition('CircleFade', FadeCircleFade);
  RegisterFadeTransition('CircleSmooth', FadeCircleSmooth);

  RegisterFadeTransition('DiamondSlideIn', FadeDiamondSlideIn);
  RegisterFadeTransition('DiamondFadeIn', FadeDiamondFadeIn);
  RegisterFadeTransition('DiamondSmoothIn', FadeDiamondSmoothIn);
  RegisterFadeTransition('DiamondSlideOut', FadeDiamondSlideOut);
  RegisterFadeTransition('DiamondFadeOut', FadeDiamondFadeOut);
  RegisterFadeTransition('DiamondSmoothOut', FadeDiamondSmoothOut);

  RegisterFadeTransition('RotateSlide', FadeRotateSlide);
  RegisterFadeTransition('RotateFade', FadeRotateFade);
  RegisterFadeTransition('RotateSmooth', FadeRotateSmooth);

  RegisterFadeTransition('Stream', FadeStream);
  RegisterFadeTransition('Plasma', FadePlasma);



finalization

end.
