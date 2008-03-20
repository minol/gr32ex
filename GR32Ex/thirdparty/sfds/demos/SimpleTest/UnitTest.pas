unit UnitTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, SFDS;

type
  TTestForm = class(TForm)
    BtnTest: TButton;
    ProgressBar: TProgressBar;
    procedure BtnTestClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ProgressProc(Sender : TSFDSCustomSource; Progress, MaxProgress, ThisTime : Int64; var Cancel : Boolean; ArcName, StreamName : string);
    { Public declarations }
  end;

var
  TestForm: TTestForm;
  vB, vE: Int64;

implementation
uses sfds_compressorzlib, sfds_compressorBZip2;

{$R *.dfm}

procedure TTestForm.ProgressProc(Sender : TSFDSCustomSource; Progress, MaxProgress, ThisTime : Int64; var Cancel : Boolean; ArcName, StreamName : string);
begin
  ProgressBar.Max := MaxProgress;
  ProgressBar.Position := Progress;
  Application.ProcessMessages;
end;

procedure TTestForm.BtnTestClick(Sender: TObject);
var Writer : TSFDSFileWriter;
    Reader : TSFDSFileReader;
    FileName : string;
    I : Integer;
begin
  FileName := ExtractFilePath(Application.ExeName) + 'tmp_test_file.sfds';
  Writer := TSFDSFileWriter.Create(FileName);
  try
  {
   Writer.OpenStream('Buffer_Test_1', 0, cfNone);
   Writer.Write(PChar('Test Buffer 1')^, 13);
   Writer.OpenStream('Buffer_Test_2', 0, cfZlib);
   Writer.Write(PChar('Test Buffer 2')^, 13);
   Writer.OpenStream('Buffer_Test_3', 0, cfBzip2);
   Writer.Write(PChar('Test Buffer 3')^, 13);
   Writer.WriteFile(Application.ExeName, ExtractFileName(Application.ExeName), ProgressProc, 0, cfBzip2);
   }
   QueryPerformanceCounter(vB);
   Writer.WriteFile('d:\setup.exe', ExtractFileName(Application.ExeName), nil, 0, cfZlib);
   QueryPerformanceCounter(vE);
  finally
   Writer.Close;
   Writer.Free;
  end; 
  MessageDlg('Test file written successfully. time:'+ IntToStr(vE-vB) + #13#10 + 'Now testing.', mtInformation, [mbok], 0);
  Reader := TSFDSFileReader.Create(FileName, 0);
  try
   QueryPerformanceCounter(vB);
   Reader.ExtractFile(ExtractFileName(Application.ExeName), ExtractFilePath(ParamStr(0))+'Setup.exe');
   QueryPerformanceCounter(vE);
   {for I := 0 to Reader.FileCount - 1 do
      begin
        Reader.ExtractFile(I, '', True, ProgressProc);
      end; //}
  finally
   Reader.Free;
  end;
  MessageDlg('Read test: OK.'+ IntToStr(vE-vB), mtInformation, [mbok], 0);
end;

end.
