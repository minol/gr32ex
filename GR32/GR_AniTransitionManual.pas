unit GR_AniTransitionManual;

{$I GR32.inc}
{$T-,W-,X+,P+}


interface

uses
  GR32, GR32_Math
  , GR_LowLevel
  , GR_AniTransition
  ;

type
{ Manual }

  TGRTransitionProcManual = procedure (SourceImage, DestImage: TBitmap32; Transition: TGRCustomTransition; Percent: byte);


procedure RegisterManualTransition(
  const aName: string; 
  const aProc: TGRTransitionProcManual;
  const aTitle: string = ''; 
  aParamClass: TGRTransitionParamClass = nil
);

implementation 

{ Manual proc =================================================================}

procedure ProcManualSlide(SourceImage, DestImage: TBitmap32; Transition: TGRCustomTransition; Percent: byte);
var
  Pos: integer;
begin
  DestImage.DrawMode := dmBlend;

  if Transition.Rotation = trNone then
  begin
    Pos := MulDiv(SourceImage.Width, Percent, 100);
    DestImage.DrawTo(SourceImage, Pos - SourceImage.Width, 0);
  end;

  if Transition.Rotation = trRotate90 then
  begin
    Pos := MulDiv(SourceImage.Height, Percent, 100);
    DestImage.DrawTo(SourceImage, 0, SourceImage.Height - Pos);
  end;

  if Transition.Rotation = trRotate180 then
  begin
    Pos := MulDiv(SourceImage.Width, Percent, 100);
    DestImage.DrawTo(SourceImage, SourceImage.Width - Pos, 0);
  end;

  if Transition.Rotation = trRotate270 then
  begin
    Pos := MulDiv(SourceImage.Height, Percent, 100);
    DestImage.DrawTo(SourceImage, 0, Pos - SourceImage.Height);
  end;
end;

procedure ProcManualPush(SourceImage, DestImage: TBitmap32; Transition: TGRCustomTransition; Percent: byte);
var
  Pos: integer;
begin
  DestImage.DrawMode := dmBlend;

  if Transition.Rotation = trNone then
  begin
    Pos := MulDiv(SourceImage.Width, Percent, 100);
    SourceImage.DrawTo(SourceImage, Pos, 0);
    DestImage.DrawTo(SourceImage, Pos - SourceImage.Width, 0);
  end;

  if Transition.Rotation = trRotate90 then
  begin
    Pos := MulDiv(SourceImage.Height, Percent, 100);
    SourceImage.DrawTo(SourceImage, 0, -Pos);
    DestImage.DrawTo(SourceImage, 0, SourceImage.Height - Pos);
  end;

  if Transition.Rotation = trRotate180 then
  begin
    Pos := MulDiv(SourceImage.Width, Percent, 100);
    SourceImage.DrawTo(SourceImage, -Pos, 0);
    DestImage.DrawTo(SourceImage, SourceImage.Width - Pos, 0);
  end;

  if Transition.Rotation = trRotate270 then
  begin
    Pos := MulDiv(SourceImage.Height, Percent, 100);
    SourceImage.DrawTo(SourceImage, 0, Pos);
    DestImage.DrawTo(SourceImage, 0, Pos - SourceImage.Height);
  end;
end;

procedure CalcFrameManual(aTransitionRec: TGRTransitionRec;
  Percent: byte);
begin
  { Manual Transition }
  with aTransitionRec do
  begin
    { Clear alpha }
    ClearAlphaFunc(DestImage.Bits, DestImage.Width * DestImage.Height, GRTransparent32);
    { Calc frame }
    TGRTransitionProcManual(ProcItem.Proc)(ResultImage, DestImage, Transition, Percent);
  end;
end;

procedure RegisterManualTransition(
  const aName: string; 
  const aProc: TGRTransitionProcManual;
  const aTitle: string; 
  aParamClass: TGRTransitionParamClass
);
begin
  RegisterTransition(aName, 'Manual', TGRTransitionProc(@aProc), aTitle, aParamClass);
end;

initialization
  RegisterTransitionType('Manual', CalcFrameManual);

  { Resigter Manual }
  RegisterManualTransition('Slide', ProcManualSlide);
  RegisterManualTransition('Push', ProcManualPush);


finalization

end.
