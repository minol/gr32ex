library GpStructuredStoragePacker;

uses
  Windows,
  SysUtils,
  Classes,
  wcxhead,
  GpStructuredStorage_WCX in 'GpStructuredStorage_WCX.pas';

exports
  OpenArchive,
  ReadHeader,
  ProcessFile,
  CloseArchive,
  SetChangeVolProc,
  SetProcessDataProc,
  GetPackerCaps,
  PackFiles,
  DeleteFiles,
  CanYouHandleThisFile;

{$E wcx}
{$R *.res}

begin
end.
