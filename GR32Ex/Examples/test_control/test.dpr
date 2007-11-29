{$APPTYPE CONSOLE}
program test;
//I:\Borland\ksdev\fxlib\sources\vcl
uses
  SysUtils, Classes
  , Graphics
  , GR32
  , GR32_System //TPerfTimer
  , GR32_PNG
  , GR32_Transforms
  , GR_FilterEx
  , GR_System
  //, GR_FastFx
  //, GR32_BitmapEx
  //, GR32_Graphics
  ;

type
  TTestObjC = object of TTestObj;
  TTestObj = object
  private
    FA: Integer;
  public
    procedure writelnA();
    property A: Integer read FA;
    
  end;

procedure TTestObj.writelnA;
begin
	writeln(A);
end;

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
  aTimer: TPerfTimer;
  aGRCounter: TGRCounter;
  AT: TAffineTransformation;
  testO: TTestObj;
begin
	testO.FA := 123;
	testO.writelnA;
  If ParamCount >= 1 Then
  Begin
    Filename := ParamStr(1);
    aBmp := TBitmap32.Create;
    Dst := TBitmap32.Create;
    aTimer := TPerfTimer.Create;
    AT := TAffineTransformation.Create;
    try
      writeln('Calibrate(ms):', aGRCounter.Calibrate);
      writeln('Frequency(ms):', aGRCounter.Frequency);
      aBMP.LoadFromFile(Filename);
      Dst.SetSizeFrom(aBMP);
      aTimer.Start;
      aGRCounter.Start;
      Rotate(aBMP, Dst, 33);
      aGRCounter.Stop;
      writeln('Rotate Used time(ms):', aTimer.ReadMilliseconds);
      writeln('Rotate Used time(ms):', aGRCounter.ReadMilliseconds);
      aTimer.Start;
      aGRCounter.Start;
      QuickRotoZoom(aBMP, Dst, 33);
      aGRCounter.Stop;
      writeln('QuickRotoZoom Used time(ms):', aTimer.ReadMilliseconds);
      writeln('QuickRotoZoom Used time(ms):', aGRCounter.ReadMilliseconds);
      aTimer.Start;
      AT.Rotate(aBMP.Width div 2, aBMP.Height div 2, 33);
      Transform(Dst, aBMP, AT);
      writeln('Transform Used time(ms):', aTimer.ReadMilliseconds);
   	 	//ApplyTransparentColor(aBMP, aBMP[0,0]);
      Delete(Filename, Length(Filename)-3, 4);
   	  SaveBitmap32ToPNG(Dst, Filename+'Rot.png', false);
   	  //writeln(Dst[0,0]);

        	//SaveBitmap32ToPNG(Dst, Filename + s + '.png', false);
        	//Dst.SaveToFile(Filename + s + '.bmp');
    finally
      aBmp.Free;
      Dst.Free;
      aTimer.Free;
      AT.Free;
    end;
  End; // If
  
end.
