{$APPTYPE CONSOLE}
program rot;

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
  //Dst, Dst2: TBitmap32;
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
      aBMP.Rotate180;
      SaveBitmap32ToPNG(aBMP, Filename + '1.png', false);
      ///aBMP.SaveToFile(Filename + '_1.png');
    finally
      aBmp.Free;
    end;
  End; // If
  
end.
