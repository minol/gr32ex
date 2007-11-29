unit Debug_LE;

interface
uses
    Windows, Classes, SysUtils, Dialogs;

{ ������ ����������� ������� ��� ������� � ��.}

{ Secundomer : tSecundomer - ��������� �������
  �������������:
   Secundomer.Start;
   <... ��� ...>
   Secundomer.Stop; // ��� Secundomer.Pouse, ����� ����������� ���������
   [... ��� ..]
   ShowMessage(Secundomer.ResultString)

}
type

  tSecundomer = class(tObject)
  private
    fPoused: boolean;
    procedure SetPoused(const Value: boolean);
    function GetInSeconds: double;

  protected
    fStartTickCount  : integer;
    fResultTickCount : integer;
    fResultFormat: string;
    fResult : tDateTime;
    function GetRes: tDateTime;
    function GetResultString: string;
  public
    constructor Create; virtual;

    procedure Start; overload;
    procedure Stop;  // ������������� � ���������� ����������
    procedure Pouse; // ������������� ����������, �� ��������� �� ���������� ����� ����� ����� �����������
    procedure Reset; // c��������� ���������. ���������� ������� � ��� �� ���������
    procedure ShowResult;

    property Poused : boolean read fPoused write SetPoused;
    property Res : tDateTime read GetRes;
    property ResultString : string read GetResultString;
    property InSeconds : double read GetInSeconds;
    property ResultFormat : string read fResultFormat write fResultFormat;
  end;

  tDebug = class(tObject)
  private
    fFileSizeLimit: string;
    fEnabled: boolean;
    fTimeFormat: string;
    fReady: boolean;
    procedure SetFileSizeLimit(const Value: string);


  protected
    fFileName : string;
    fText : text;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Init;


    procedure CheckReady;
    procedure Print(sText : string); virtual;
    procedure Clear; virtual;
    property FileName : string read fFileName;
    property FileSizeLimit : string read fFileSizeLimit write SetFileSizeLimit; { TODO - ����������� ������������ ����� }
    property Enabled : boolean read fEnabled write fEnabled;
    property TimeFormat : string read fTimeFormat write fTimeFormat;
    property Ready : boolean read fReady;
  end;




var
  Secundomer : tSecundomer;
  Debug : tDebug;

implementation

uses Forms;

{ tSecundomer }

constructor tSecundomer.Create;
begin
  inherited Create;
  fResultFormat := 'hh:mm:ss.zzz';
  fResultTickCount := 0;
  fPoused := true;
  fStartTickCount := 0;
end;

function tSecundomer.GetInSeconds: double;
begin
  result := fResult*SecsPerDay;
end;

function tSecundomer.GetRes: tDateTime;
begin
  result := fResult;
end;

function tSecundomer.GetResultString: string;
begin
  DateTimeToString(result, fResultFormat, fResult);
end;

procedure tSecundomer.Pouse;
begin
  fPoused := true;
  fResultTickCount := GetTickCount;
  fResult := (fResultTickCount - fStartTickCount)/ MSecsPerDay;
end;

procedure tSecundomer.Reset;
begin
  fResultTickCount := 0;
  fPoused := true;
  fStartTickCount := 0;
end;

procedure tSecundomer.SetPoused(const Value: boolean);
begin
  fPoused := Value;
end;

procedure tSecundomer.ShowResult;
begin
  ShowMessage(GetResultString);
end;

procedure tSecundomer.Start;
begin
  fPoused := false;
  fStartTickCount := fStartTickCount + GetTickCount - fResultTickCount;

end;

procedure tSecundomer.Stop;
begin
  fResultTickCount := GetTickCount;
  fPoused := true;
  fResult := (fResultTickCount - fStartTickCount)/ MSecsPerDay;
  Reset;
end;

{ tDebug }

constructor tDebug.Create;
begin
  inherited;
  fEnabled := true;
  fFileName := ExtractFilePath(Application.ExeName) + '\debug.log';
  fTimeFormat := 'hh:mm:ss.zzz';

end;

procedure tDebug.Init;
begin
  Assign(fText, fFileName);
  rewrite(fText, fFileName);
  close(ftext);
end;

destructor tDebug.Destroy;
begin
  inherited;
end;

procedure tDebug.Clear;
begin
  CheckReady;
  rewrite(fText);
end;

procedure tDebug.Print(sText: string);
var
  sTime : string;
begin
  if fEnabled then
    begin
    CheckReady;
    append(fText);
    DateTimeToString(sTime, fTimeFormat, Now());
    writeln(fText,  sTime + ' ' + sText);
    close(fText);
    end;
end;

procedure tDebug.SetFileSizeLimit(const Value: string);
begin
  fFileSizeLimit := Value;
end;

procedure tDebug.CheckReady;
begin
  if not fReady then Init;
end;

initialization
  Secundomer := nil;
  Secundomer := tSecundomer.Create;

  Debug := nil;
  Debug := tDebug.Create;



finalization

  Secundomer.Free;
  Debug.Free;

end.
