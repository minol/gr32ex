{$APPTYPE CONSOLE}
program mask;

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
    try
      aBMP.LoadFromFile(Filename);
      Delete(Filename, Length(Filename)-3, 4);
      ApplyBWImage(aBMP);
      aBMP.SaveToFile(Filename + sMask + '.bmp');
    finally
      aBmp.Free;
    end;
  End; // If
  
end.
