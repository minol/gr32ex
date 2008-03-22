program t;

{$APPTYPE Console}
uses 
  Windows,
  Classes, SysUtils,
  CompDoc;


var
  vRS: TRootStorage;
  vSS: TStorageStream;
  vS: TFileStream;
  vB, vE: Int64;
begin
{  vRS := TRootStorage.Create(ExtractFilePath(ParamStr(0)) + 'tss.pak', amReadWrite, smExclusive, tmDirect, True);
  try
    QueryPerformanceCounter(vB);
    with TStorageStream.Create('qq.rar', vRS, amWrite, True) do
    try
      vS := TFileStream.Create('e:\qq.rar', fmOpenRead);
      try
        CopyFrom(vS, 0);
      finally
        vS.Free;
      end;
    finally
      Free;
    end;
  finally
    vRS.Free;
  end;
  QueryPerformanceCounter(vE);
  writeLn(vE-vB);
}

  vRS := TRootStorage.Create(ExtractFilePath(ParamStr(0)) + 'tss.pak', amRead, smExclusive, tmDirect, False);
  try
    QueryPerformanceCounter(vB);
    vSS:=TStorageStream.Create('qq.rar', vRS, amRead, False);
    with vSS do
    try
      vS := TFileStream.Create(ExtractFilePath(ParamStr(0)) +'qq.rar', fmCreate);
      try
        vS.CopyFrom(vSS, 0);
      finally
        vS.Free;
      end;
    finally
      Free;
    end;
  finally
    vRS.Free;
  end;
  QueryPerformanceCounter(vE);
  writeLn(vE-vB);
end.
