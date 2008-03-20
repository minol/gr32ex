program test;

{$APPTYPE Console}

uses
  Windows, SysUtils, Classes
  , GpStructuredStorage
  ;

var
  vCF: IGpStructuredStorage;
  vB, vE: Int64;
  vStream, vDest: TStream;

begin
  vCF := CreateStructuredStorage;
  vCF.Initialize('test.stm', fmCreate);
  try
    vStream := TFileStream.Create('d:\setup.exe', fmOpenRead);
    try
      QueryPerformanceCounter(vB);
      vDest := vCF.OpenFile('\setup.exe', fmCreate);
      vDest.CopyFrom(vStream, 0);
      vDest.Free;
      QueryPerformanceCounter(vE);
    finally
      vStream.Free;
    end;
    WriteLn('Time:', vE-vB);
    QueryPerformanceFrequency(vB);
    WriteLn('QueryPerformanceFrequency:', vB);
  finally
  end;
end.
