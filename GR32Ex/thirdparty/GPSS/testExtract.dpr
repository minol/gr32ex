program testExtract;

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
  vCF.Initialize('test.stm', fmOpenRead);
  try
    vStream := TFileStream.Create('setup.exe', fmCreate);
    try
      QueryPerformanceCounter(vB);
      vDest := vCF.OpenFile('\setup.exe', fmOpenRead);
      vStream.CopyFrom(vDest, 0);
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
