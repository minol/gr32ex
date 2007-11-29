{$APPTYPE CONSOLE}
program bmp2png;

uses
  SysUtils, Classes
  , Graphics
  , GR32
  , GR32_PNG
  , GR_FilterEx
  //, GR32_BitmapEx
  //, GR32_Graphics
  ;


const
  sMask = '_Mask';
  sUp = '_UP';
  sHot = '_Hot';
  sDown = '_Down';
  sButton = '_Btn';
var
  s, s1 , Filename: string;
  aBmp: TBitmap32;
  Dst, Dst2: TBitmap32;
  i, j: integer;
  w,h: integer;
  SrcRect: TRect;
begin
  If ParamCount >= 1 Then
  Begin
    Filename := ParamStr(1);
    aBmp := TBitmap32.Create;
    Dst := TBitmap32.Create;
   	Dst2 := TBitmap32.Create;
    try
      aBMP.LoadFromFile(Filename);
   	 	//ApplyTransparentColor(aBMP, aBMP[0,0]);
   	  //SaveBitmap32ToPNG(aBMP, Filename, false);
   	 	//exit;
      Delete(Filename, Length(Filename)-3, 4);
      w := aBMP.Width div 3;
      h := aBMP.Height div 2;
     	Dst2.SetSize(w,h);

    	//first split two bmp: button and button mask.
    	dst.SetSize(aBMP.Width, h);
    	for i := 0 to 1 do
    	Begin
        	SrcRect.Left := 0;
        	SrcRect.Top := i * h;
        	SrcRect.Right := aBMP.Width;
        	SrcRect.Bottom := SrcRect.Top + h;
        	aBMP.DrawTo(Dst, 0,0, SrcRect);
        	if i = 0 then s := sButton else s := sMask;
        	//Dst.DrawMode := dmTransparent;
        	//Dst.OuterColor := Dst[0,0];
        	if i = 1 then
        	  ApplyBlueChannelToAlpha(Dst, true)
        	;//else
        	 	ApplyTransparentColor(Dst, Dst[0,0]);

        	SaveBitmap32ToPNG(Dst, Filename + s + '.png', false);
        	for j := 0 to 2 do
        	begin
        	  SrcRect.Left := j * w;
        	  SrcRect.Top := 0;
        	  SrcRect.Right := SrcRect.Left + w;
        	  SrcRect.Bottom := SrcRect.Top + h;
        	  Dst.DrawTo(Dst2, 0, 0, SrcRect);
        	  case j of
        	    0: s1 := s + sUp;
        	    1: s1 := s + sHot;
        	    2: s1 := s + sDown;
        	  end;
        	  SaveBitmap32ToPNG(Dst2, Filename + s1 + '.png', false);
        	end;
        	//Dst.SaveToFile(Filename + s + '.bmp');
    	end;

      {for i := 0 to 2 do //w
        for j:= 0 to 1 do //h
        Begin
        	SrcRect.Left := i * w;
        	SrcRect.Top := j * h;
        	SrcRect.Right := SrcRect.Left + w;
        	SrcRect.Bottom := SrcRect.Top + h;
        	aBMP.DrawTo(Dst, 0,0, SrcRect);
        	if j = 0 then s := sButton else s := sMask;
        	case i of
        	  0: s := s + sUp;
        	  1: s := s + sHot;
        	  2: s := s + sDown;
        	end;
        	SaveBitmap32ToPNG(Dst, Filename + s + '.png', false);
        	//Dst.SaveToFile(Filename + s + '.bmp');
        end; //}
    finally
      aBmp.Free;
      Dst.Free;
      Dst2.Free;
    end;
  End; // If
  
end.
