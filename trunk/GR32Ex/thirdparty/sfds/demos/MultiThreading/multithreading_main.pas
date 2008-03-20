unit multithreading_main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SFDS, StdCtrls, MD5;

type
  TForm1 = class(TForm)
    btnStart: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    btnStop: TButton;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TSFDSTestThread = class(TThread)
  private
    FCancel: Boolean;
    FBytesProcessed: Int64;
    FNumberOfErrors: Integer;
    FNumberOfChecksumsCalculated: Integer;
  public
    procedure Execute; override;
    property Cancel : Boolean read FCancel write FCancel;
    property BytesProcessed : Int64 read FBytesProcessed;
    property NumberOfErrors : Integer read FNumberOfErrors;
    property NumberOfChecksumsCalculated : Integer read FNumberOfChecksumsCalculated;
  end;

var
  Form1: TForm1;
  ThreadArray : array[0..9] of TSFDSTestThread;
  ThreadCount : Integer = 0;
  OpenedFile : string = '';

implementation

{$R *.dfm}

{ TSFDSTestThread }

procedure TSFDSTestThread.Execute;
var str : TStream;
    strsize : Int64;
    buf : Pointer;
    read : Integer;
    Digest, B : TMD5Digest;
    Context : TMD5Context;
begin
  Cancel := False;
  FNumberOfErrors := 0;
  FNumberOfChecksumsCalculated := 0;
  str := nil;
  if SFDS_OpenStreamIndirect(OpenedFile + '::test.dat', str, strsize) then
    begin
      GetMem(buf, 1024);
      try
        repeat
        if str.Position = 0 then
          begin
          if FBytesProcessed > 0 then
            begin
              Digest := MD5Final(Context);
              FNumberOfChecksumsCalculated := FNumberOfChecksumsCalculated + 1;
              B := SFDS_Find_ActiveReader_By_SFDSFileName(OpenedFile).FileEntrys.FindEntry('test.dat')^.MD5Digest;
              if not MD5DigestsEqual(Digest, B) then FNumberOfErrors := FNumberOfErrors + 1;
              FreeAndNil(str);
              SFDS_OpenStreamIndirect(OpenedFile + '::test.dat', str, strsize);
            end;
          MD5Init(Context);
          end;
        read := str.Read(buf^, 1024);
        if Read > 0 then MD5Update(Context, buf^, read);
        FBytesProcessed := FBytesProcessed + read;
        if (read < 1024) then str.Seek(0, soBeginning);
        until Cancel;
      finally
        FreeMem(buf, 1024); FreeAndNil(str);
      end;
    end;
end;

procedure TForm1.btnStartClick(Sender: TObject);
var I : Integer;
    T : TSFDSTestThread;
    R : TSFDSCustomReader;
    M : TMemoryStream;
    F : TFileStream;
begin
  CheckBox1.Enabled := False;
  btnStart.Enabled := False;
  if (CheckBox1.Checked = True) then
     begin
       if OpenedFile = '' then
       begin
         F := TFileStream.Create('mttestdata.sfds', fmOpenRead or fmShareDenyWrite);
         try
         M := TMemoryStream.Create;
         M.LoadFromStream(F);
         finally
           F.Free;
         end;
         R := TSFDSMemoryStreamReader.Create(M, 0, True);
         OpenedFile := R.FSFileName;
       end;
     end
     else
     begin
       if OpenedFile = '' then
       begin
         R := TSFDSFileReader.Create('mttestdata.sfds', 0);
         OpenedFile := R.FSFileName;
       end;
     end;
  ThreadCount := ComboBox1.ItemIndex + 1;
  for I := 0 to (ThreadCount - 1) do
      begin
        T := TSFDSTestThread.Create(False);
        T.Priority := tpLowest;
        ThreadArray[I] := T;
        //T.Execute;
      end;
  btnStop.Enabled := True;
end;

procedure TForm1.btnStopClick(Sender: TObject);
var I : Integer;
    s : string;
begin
s := 'Operated in file: "' + OpenedFile + '"' + #13#10 + 'on stream: "test.dat" (1.440.054 Bytes - zlib compressed)' + #13#10 + #13#10;
  if ThreadCount = 0 then Exit;
  for  I := (ThreadCount - 1) downto 0 do
    begin
      ThreadArray[I].Cancel := True;
      ThreadArray[I].WaitFor;
      Dec(ThreadCount);
      s := s + 'Thread ' + IntToStr(I + 1) + ': ' + IntToStr(ThreadArray[I].BytesProcessed) + ' bytes processed. (' + IntToStr(ThreadArray[I].NumberOfChecksumsCalculated) + ' Checksums Calculated; ' + IntToStr(ThreadArray[I].NumberOfErrors) + ' errors)' + #13#10;
      ThreadArray[I].Free;
      ThreadArray[I] := nil;
    end;
  btnStart.Enabled := True;
  btnStop.Enabled := False;
  ShowMessage(S);  
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Label2.Caption := 'Press "Start MultiThreading Test" to create a number of threads that read the same data from the same sfds file at the same time.'+#13#10+'When bored press "Stop" to see statistics.';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  btnStopClick(btnStop);
end;

end.
