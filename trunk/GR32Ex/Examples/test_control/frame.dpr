{$APPTYPE CONSOLE}
program frame;

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
  sFrame = '_frame';

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
    try
      aBMP.LoadFromFile(Filename);
      Delete(Filename, Length(Filename)-3, 4);
      w := aBMP.Width div 3;
      h := aBMP.Height div 3;
      Dst.SetSize(w, h);
      for i := 0 to 2 do
        for j := 0 to 2 do
        begin
        	case i of
        	  0: s := '_L';
        	  1: s :=  '_M';
        	  2: s :=  '_R';
        	end;
        	case j of
        	  0: s1 := s + 'T';
        	  1: s1 :=  s + 'M';
        	  2: s1 :=  s + 'B';
        	end;
        	SrcRect.Left := i * w;
        	SrcRect.Top := j * h;
        	SrcRect.Right := SrcRect.Left + w;
        	SrcRect.Bottom := SrcRect.Top + h;
        	aBMP.DrawTo(Dst, 0,0, SrcRect);
   	      SaveBitmap32ToPNG(Dst, Filename + s1 + '.png', false);
        end;
    finally
      aBmp.Free;
      Dst.Free;
    end;
  End; // If
  
end.
