program testExtract;

{$APPTYPE Console}

uses
  Windows, SysUtils, Classes
  , AbCompoundFile
  ;

var
  vCF: TAbCompoundFile;
  vB, vE: Int64;
  vStream: TFileStream;

begin
  vCF := TAbCompoundFile.Create('', '', 4096 * 1024);
  try
    vCF.Open('test.stm');
    vStream := TFileStream.Create('setup.exe', fmCreate);
    try
      QueryPerformanceCounter(vB);
      vCF.OpenFile('setup.exe', TStream(vStream));
      QueryPerformanceCounter(vE);
    finally
      vStream.Free;
    end;
    WriteLn('Time:', vE-vB);
  finally
    vCF.Free;
  end;
end.
