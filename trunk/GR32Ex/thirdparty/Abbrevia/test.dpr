program test;

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
  vCF := TAbCompoundFile.Create('test.stm', 'MyFF', 4096 * 1024);
  try
    vStream := TFileStream.Create('d:\setup.exe', fmOpenRead);
    try
      QueryPerformanceCounter(vB);
      vCF.AddFile('setup.exe', vStream, vStream.Size);
      QueryPerformanceCounter(vE);
    finally
      vStream.Free;
    end;
    WriteLn('Time:', vE-vB);
  finally
    vCF.Free;
  end;
end.
