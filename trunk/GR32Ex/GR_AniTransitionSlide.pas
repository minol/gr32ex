unit GR_AniTransitionSlide;

{$I GR32.inc}
{$T-,W-,X+,P+}


interface

uses
  GR32, GR32_Math
  , GR32_Blend
  , GR_LowLevel
  , GR_AniTransition
  ;

type
{ Slide Matrix }

  TGRPointSlide = record
    Alpha: Byte;
    X, Y: SmallInt;
  end;

  PGRMatrixSlide = ^TGRMatrixSlide;
  TGRMatrixSlide = array[0..0] of TGRPointSlide;

  TGRTransitionProcSlide = procedure (var Matrix: TGRMatrixSlide; Width, Height: integer; Percent: byte);

  TGRSlideTransitionParam = class(TGRTransitionParam)
  private
  protected
    MatrixWidth, MatrixHeight: integer;
    MatrixLen: integer;
    MatrixSlide: PGRMatrixSlide;
    procedure Init; override;
  public
    destructor Destroy; override;
  end;


procedure RegisterSlideTransition(
  const aName: string; 
  const aProc: TGRTransitionProcSlide;
  const aTitle: string = ''; 
  aParamClass: TGRTransitionParamClass = nil
);

implementation 

{ Slide Ani ==================================================================}

procedure SetSlideMatrix(var Matrix: TGRMatrixSlide; Width, Height: integer; x1,y1, x2, y2: integer; A: byte);
begin
{ Процедура установки матрицы TSlideAnimationMatrix с проверкой переполнения
  x1, y1 : устанавливаемая точка
  x2, y2 : источник }
  if (x1 < Width) and (x2 < Width) and (y2 < Height) and (y1 < Height) and (x1 >= 0) and (x2 >= 0) and (y1 >= 0) and (y2 >= 0) then
  begin
    with Matrix[y1 * Width + x1] do
    begin
      X := x2;
      Y := y2;
      Alpha := $FF
    end;
  end;
end;

{ SlideMatrix's Effects ======================================================}

procedure ProcSlideInterlace(var Matrix: TGRMatrixSlide; Width, Height: integer; Percent: byte);
var
  i, j: integer;
begin
  { Slider - interlaced line moving }
  if Percent = 0 then Exit;

  for i := 0 to Width - 1 do
    for j := 0 to Height - 1 do
      with Matrix[j * Width + i] do
      begin
        Alpha := $FF; //MulDiv(Percent, $FF, 100);
        if j mod 2 = 0 then
          X := i + MulDiv(100-Percent, Width, 100)
        else
          X := i - MulDiv(100-Percent, Width, 100);
        Y := j;
      end;
end;

procedure ProcSlideCrushIn(var Matrix: TGRMatrixSlide; Width, Height: integer; Percent: byte);
var
  i, j: integer;
  CenterX: integer;
begin
  { ZoomIn from Center line }
  if Percent = 0 then Exit;

  CenterX := Width div 2;

  for i := 0 to CenterX do
    for j := 0 to Height - 1 do
    begin
      with Matrix[j * Width + i] do
      begin
        Alpha := $FF; //MulDiv(Percent, $FF, 100);
        X := CenterX - Round((CenterX - i) / (Percent/100));
        Y := j;
      end;
      with Matrix[j * Width + i + (Width div 2)] do
      begin
        Alpha := $FF; //MulDiv(Percent, $FF, 100);
        X := CenterX + Round(i / (Percent/100));
        Y := j;
      end;
    end;
end;

procedure ProcSlideCorner(var Matrix: TGRMatrixSlide; Width, Height: integer; Percent: byte);
var
  i, j: integer;
  dx, dy : integer;
begin
  if Percent = 0 then Exit;

{С угла}
  dx := MulDiv (Width-1, Percent, 100);
  dy := MulDiv (Height-1, Percent, 100);

  for i := 0 to dx do
    for j := 0 to dy do
      SetSlideMatrix(Matrix, Width, Height, i, j, Width - dx -1 + i, Height - dy - 1 + j, $FF);
end;

procedure TGRSlideTransitionParam.Init;
begin
  if Assigned(FOwner) then
  with FOwner^ do
  begin
    MatrixWidth := SourceImage.Width div Transition.Resolution div Transition.TileCount + 1;
    MatrixHeight := SourceImage.Height div Transition.Resolution div Transition.TileCount + 1;
    MatrixLen := MatrixWidth * MatrixHeight * SizeOf(TGRPointSlide);
    GetMem(MatrixSlide, MatrixLen);
  end;
end;

destructor TGRSlideTransitionParam.Destroy; 
begin
  //if Assigned(FOwner) then
  begin
    if Assigned(MatrixSlide) then FreeMem(MatrixSlide);
    //FreeMem(MatrixSlide, MatrixLen);
  end;
  inherited;
end;

procedure CalcFrameSlide(aTransitionRec: TGRTransitionRec;
  Percent: byte);
var
  SourceColor, DestColor: PColor32;
  MatrixValue: TGRPointSlide;
  RepeatWidth, RepeatHeight: integer;
  MatrixX, MatrixY, TileX, TileY, ResX, ResY: integer; { loop variables }
  Sx, Sy: integer; { Source position }
  Dx, Dy: integer; { Dest position }
begin
  { MatrixSlide's Transition }
  with aTransitionRec, TGRSlideTransitionParam(Param) do
  begin
    FillChar(MatrixSlide^, MatrixLen, 0);

    { Perform matrix proc }
    if Transition.Rotation in [trRotate90, trRotate270] then
      TGRTransitionProcSlide(ProcItem.Proc)(MatrixSlide^, MatrixHeight, MatrixWidth, Percent)
    else
      TGRTransitionProcSlide(ProcItem.Proc)(MatrixSlide^, MatrixWidth, MatrixHeight, Percent);

    { }
    RepeatWidth := ResultImage.Width div Transition.TileCount + 1;
    RepeatHeight := ResultImage.Height div Transition.TileCount + 1;

    { Apply matrix }
    try
      for TileX := 0 to Transition.TileCount-1 do
        for TileY := 0 to Transition.TileCount-1 do
          for MatrixX := 0 to MatrixWidth - 1 do
            for MatrixY := 0 to MatrixHeight - 1 do
            begin
              for ResX := MatrixX * Transition.Resolution to (MatrixX + 1) * Transition.Resolution do
                for ResY := MatrixY * Transition.Resolution to (MatrixY + 1) * Transition.Resolution do
                begin
                  Sx := (TileX * RepeatWidth) + ResX;
                  Sy := (TileY * RepeatHeight) + ResY;

                  { Get matrix value}
                  case Transition.Rotation of
                    trRotate90:
                      begin
                        MatrixValue := MatrixSlide[MatrixY + MatrixHeight * (MatrixWidth - MatrixX - 1)];
                        Dx := (TileX * RepeatWidth) + (MatrixWidth - MatrixValue.Y - 1) * Transition.Resolution + (ResX - MatrixX * Transition.Resolution);
                        Dy := (TileY * RepeatHeight) + MatrixValue.X * Transition.Resolution + (ResY - MatrixY * Transition.Resolution);
                      end;
                    trRotate180:
                      begin
                        MatrixValue := MatrixSlide[(MatrixWidth - MatrixX - 1) + MatrixWidth * (MatrixHeight - MatrixY - 1)];
                        Dx := (TileX * RepeatWidth) + (MatrixWidth - MatrixValue.X - 1) * Transition.Resolution + (ResX - MatrixX * Transition.Resolution);
                        Dy := (TileY * RepeatHeight) + (MatrixHeight - MatrixValue.Y - 1) * Transition.Resolution + (ResY - MatrixY * Transition.Resolution);
                      end;
                    trRotate270:
                      begin
                        MatrixValue := MatrixSlide[(MatrixHeight - MatrixY - 1) + MatrixHeight * MatrixX];
                        Dx := (TileX * RepeatWidth) + MatrixValue.Y * Transition.Resolution + (ResX - MatrixX * Transition.Resolution);
                        Dy := (TileY * RepeatHeight) + (MatrixHeight - MatrixValue.X - 1) * Transition.Resolution + (ResY - MatrixY * Transition.Resolution);
                      end;
                    else
                      { None }
                      MatrixValue := MatrixSlide[MatrixX + MatrixWidth * MatrixY];
                      Dx := (TileX * RepeatWidth) + MatrixValue.X * Transition.Resolution + (ResX - MatrixX * Transition.Resolution);
                      Dy := (TileY * RepeatHeight) + MatrixValue.Y * Transition.Resolution + (ResY - MatrixY * Transition.Resolution);
                  end;

                  { Check source point use loop }
                  if Sx >= DestImage.Width then Continue;
                  if Sy >= DestImage.Height then Continue;

                  { Get color pointer }
                  SourceColor := ResultImage.PixelPtr[Sx, Sy];

                  { Check dest position (use direct value) }
                  if (Dx >= DestImage.Width) or (Dy >= DestImage.Height) or (Dx < 0) or (Dy < 0) then
                    Continue;

                  if (Dx < TileX * RepeatWidth) or (Dx > (TileX + 1) * RepeatWidth) or
                     (Dy < TileY * RepeatHeight) or (Dy > (TileY + 1) * RepeatHeight)
                  then
                    Continue;

                  { Get destination }
                  DestColor := DestImage.PixelPtr[Dx, Dy];

                  { Apply matrix }
                  if (DestColor^ and AlphaMask = 0) then
                    Continue // Transparent
                  else
                    if MatrixValue.Alpha = $FF then
                      SourceColor^ := DestColor^
                    else
                    begin
                      DestColor^ := DestColor^ and not AlphaMask;
                      DestColor^ := DestColor^ or (MatrixValue.Alpha shl 24);
                      SourceColor^ := BlendReg(DestColor^, SourceColor^);
                    end;
                end;
            end;
    finally
      EMMS;
    end;
  end;
end;

procedure RegisterSlideTransition(
  const aName: string; 
  const aProc: TGRTransitionProcSlide;
  const aTitle: string = ''; 
  aParamClass: TGRTransitionParamClass = nil
);
begin
  if aParamClass = nil then aParamClass := TGRSlideTransitionParam;
  RegisterTransition(aName, 'Slide', TGRTransitionProc(@aProc), aTitle, aParamClass);
end;

initialization
  RegisterTransitionType('Slide', CalcFrameSlide);

  { Register ProcSlide }
  RegisterSlideTransition('Interlace', ProcSlideInterlace);
  RegisterSlideTransition('CrushIn', ProcSlideCrushIn);
  RegisterSlideTransition('Corner', ProcSlideCorner);


finalization

end.
