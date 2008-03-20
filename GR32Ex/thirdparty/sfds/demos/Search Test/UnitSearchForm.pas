unit UnitSearchForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TFormSearchTest = class(TForm)
    Panel1: TPanel;
    FileListBox: TListBox;
    DisplayMemo: TRichEdit;
    BtnFindAll: TButton;
    BtnFindTest1: TButton;
    BtnFindTest2: TButton;
    BtnFindTest3: TButton;
    EditCustomSrc: TEdit;
    Button1: TButton;
    LabelInfo: TLabel;
    Panel2: TPanel;
    Label1: TLabel;
    ComboBoxStrMode: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FileListBoxClick(Sender: TObject);
    procedure BtnFindAllClick(Sender: TObject);
    procedure BtnFindTest1Click(Sender: TObject);
    procedure BtnFindTest2Click(Sender: TObject);
    procedure BtnFindTest3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBoxStrModeChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Search(FileName, SFDSFileName : string);
    { Public declarations }
  end;

var
  FormSearchTest: TFormSearchTest;

implementation
uses SFDS, SFDS_CompressorZLib, SFDS_CompressorBZip2;

{$R *.dfm}
const
InfoText = 'Search can be performed in all opened readers or in a specific one.' + #13#10 +
           'Strings with wildcards(* and ?) separated with commas are allowed.' + #13#10 +
           'Example:' + #13#10 +
           'images\*.jpg;images\*.png' + #13#10 +
           'images\01?5.jpg;images\01?5.png';

var D1 : TSFDSCustomReader = nil;
    D2 : TSFDSCustomReader = nil;
    D3 : TSFDSCustomReader = nil;

procedure UnloadDataFiles;
begin
  D3.Free;
  D2.Free;
  D1.Free;
end;

procedure LoadDataFiles(StreamAutorenameMode : TStreamAutorenameMode = sarmNone);
begin
 UnloadDataFiles;
 D1 := SFDS_CreateReader('test1.sfds', False, StreamAutorenameMode);
 D2 := SFDS_CreateReader('test2.sfds', False, StreamAutorenameMode);
 D3 := SFDS_CreateReader('test3.sfds', False, StreamAutorenameMode);
end;

procedure TFormSearchTest.FormCreate(Sender: TObject);
begin
 //Load all data files
 LoadDataFiles(TStreamAutorenameMode(ComboBoxStrMode.ItemIndex));
 BtnFindAllClick(BtnFindAll);

 LabelInfo.Caption := InfoText;
end;

procedure TFormSearchTest.Search(FileName, SFDSFileName: string);
var FDAT : TSFDSFileData;
    SH : PtrUInt;
begin
  FileListBox.Clear; DisplayMemo.Clear;
  SH := SFDS_FindFirst(FileName, SFDSFileName, FDAT);
  if SH = 0 then Exit;
  FileListBox.Items.Add(FDAT.SFDSFileName + '::' + FDAT.FileData.FileName);
  while SFDS_FindNext(SH, FDAT) = 0 do
    begin
    FileListBox.Items.Add(FDAT.SFDSFileName + '::' + FDAT.FileData.FileName);
    end;
  SFDS_FindClose(SH);
end;

procedure TFormSearchTest.FileListBoxClick(Sender: TObject);
var I : Integer;
    S : string;
    ST : TStream;
    SZ : Int64;
begin
  I := FileListBox.ItemIndex;
  if I = -1 then exit;
  S := FileListBox.Items.Strings[I];
  ST := nil;
  if not SFDS_OpenStreamIndirect(S, ST, SZ) then exit;
  DisplayMemo.Lines.LoadFromStream(ST);
  ST.Free;
end;

procedure TFormSearchTest.BtnFindAllClick(Sender: TObject);
begin
  Search('*.*', '');
end;

procedure TFormSearchTest.BtnFindTest1Click(Sender: TObject);
begin
  Search('*.*', 'test1.sfds');
end;

procedure TFormSearchTest.BtnFindTest2Click(Sender: TObject);
begin
  Search('*.rtf;sample 1*.txt', '');
end;

procedure TFormSearchTest.BtnFindTest3Click(Sender: TObject);
begin
  Search('sample 1?.txt', '');
end;

procedure TFormSearchTest.Button1Click(Sender: TObject);
begin
  Search(EditCustomSrc.Text, '');
end;

procedure TFormSearchTest.ComboBoxStrModeChange(Sender: TObject);
begin
 LoadDataFiles(TStreamAutorenameMode(ComboBoxStrMode.ItemIndex));
 FileListBox.Clear;
 BtnFindAllClick(BtnFindAll);
end;

end.
