unit BSFCForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ActnList, sfds, SFDS_PROPS, SFDS_Consts, ExtCtrls, StdCtrls,
  ImgList, FileCtrl, SFDS_CompressorZLib, SFDS_CompressorBZip2;

type
  TSFDSCreator = class(TForm)
    FileListView: TListView;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    CreateSFDSFile1: TMenuItem;
    Close1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Actions1: TMenuItem;
    AddFile1: TMenuItem;
    AddDirectory1: TMenuItem;
    ools1: TMenuItem;
    estSFDSFile1: TMenuItem;
    Options1: TMenuItem;
    InternalFileListCompression1: TMenuItem;
    iflcNone: TMenuItem;
    iflcZlib: TMenuItem;
    iflcBZip2: TMenuItem;
    AskForMetadata: TMenuItem;
    ActionList1: TActionList;
    ActExit: TAction;
    ActNew: TAction;
    ActClose: TAction;
    ActAddFile: TAction;
    ActAddDir: TAction;
    SaveFileDialog: TSaveDialog;
    ProgressBar: TProgressBar;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    LabelAct: TLabel;
    BtnFinish: TButton;
    ActTest: TAction;
    OpenDialogTest: TOpenDialog;
    ImageList: TImageList;
    ExtractSFDSFile1: TMenuItem;
    ActExtract: TAction;
    N2: TMenuItem;
    Extras1: TMenuItem;
    estdamagedsfdsfile1: TMenuItem;
    ActShowFileMeta: TAction;
    N3: TMenuItem;
    ShowSFDSfilemetadatainformation1: TMenuItem;
    N4: TMenuItem;
    PreviewBitmapfromSFDSfile1: TMenuItem;
    PercentPanel: TPanel;
    procedure ActExitExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActNewExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActCloseExecute(Sender: TObject);
    procedure ActCloseUpdate(Sender: TObject);
    procedure iflcClick(Sender: TObject);
    procedure ActAddFileExecute(Sender: TObject);
    procedure AskForMetadataClick(Sender: TObject);
    procedure BtnFinishClick(Sender: TObject);
    procedure ActTestExecute(Sender: TObject);
    procedure ActExtractExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure estdamagedsfdsfile1Click(Sender: TObject);
    procedure ActShowFileMetaExecute(Sender: TObject);
    procedure PreviewBitmapfromSFDSfile1Click(Sender: TObject);
  private
    { Private declarations }
  public
    IsFileOpened : Boolean;
    procedure ProgressProc(Sender : TSFDSCustomSource; Progress, MaxProgress, ThisTime : Int64; var Cancel : Boolean; ArcName, StreamName : string);
    procedure ShowMenu(Show : Boolean);
    procedure ExtractOrTest(Test : Boolean; FileName, DestDir : String);
    { Public declarations }
  end;

var
  SFDSCreator: TSFDSCreator;
  SFDSFileWriter : TSFDSFileWriter;
  MetaData : TSFDSFileExtraInfoList;
  InternalFileListCompression : Byte = 1;
  CancelAct : Boolean = False;
  YesToAll : Boolean = False;
  NoToAll  : Boolean = False;
  ReplaceFileDlgResult : Integer = 0;

const
  CompFmt : array[0..2] of string = ('None', 'ZLib (Deflate)', 'BZip2');
  CompLevel : array[clFastest..clMax] of string = ('Fastest', 'Default', 'Max');
  ID_YES = mrYes;
  ID_NO = mrNo;
  ID_YESTOALL = 18;
  ID_NOTOALL = 19;

implementation
uses AddFileDlgForm, ReplaceFileDlg;

{$R *.dfm}

{ This function solves for y in the equation "x is y%%% of z". }
function SolveForY(X, Z: Int64): Int64;
begin
  if Z = 0 then Result := 0
  else Result := Longint(Trunc( (X * 10000.0) / Z ));
end;

procedure TSFDSCreator.ActExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TSFDSCreator.FormCreate(Sender: TObject);
begin
  IsFileOpened := False;
  SFDSFileWriter := nil;
  MetaData := TSFDSFileExtraInfoList.Create;
end;

procedure TSFDSCreator.ActNewExecute(Sender: TObject);
var Title, Subject, Author, Keywords, Comments : String;
const EMPTYGUID : TGUID = '{00000000-0000-0000-0000-000000000000}';
begin
  if IsFileOpened then
     begin
       ActClose.Execute;
     end;
  if not SaveFileDialog.Execute then Exit;
  MetaData.Clear;
  if AskForMetadata.Checked then
     EditMetaData(MetaData, '', SaveFileDialog.FileName, EMPTYGUID, Title, Subject, Author, Keywords, Comments, Now);
  SFDSFileWriter := TSFDSFileWriter.Create(SaveFileDialog.FileName, False, aSfxNone, nil, Title, Subject, Author, 0, 'Basic SFDS File Creator', 0, Keywords, Comments, MetaData);
  SFDSFileWriter.InternalCompressionFormat := InternalFileListCompression;
  IsFileOpened := True;
end;

procedure TSFDSCreator.FormDestroy(Sender: TObject);
begin
  MetaData.Free;
  ActClose.Execute;
end;

procedure TSFDSCreator.ActCloseExecute(Sender: TObject);
begin
  if Assigned(SFDSFileWriter) then
     begin
     SFDSFileWriter.Free;
     SFDSFileWriter := nil;
     end;
  FileListView.Clear;
  IsFileOpened := False;
end;

procedure TSFDSCreator.ActCloseUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := IsFileOpened;
end;

procedure TSFDSCreator.iflcClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  InternalFileListCompression := TMenuItem(Sender).Tag;
  if Assigned(SFDSFileWriter) then SFDSFileWriter.InternalCompressionFormat := InternalFileListCompression;
end;

procedure TSFDSCreator.PreviewBitmapfromSFDSfile1Click(Sender: TObject);
begin
  ShowSFDSPropDialog(ExtractFilePath(Application.ExeName) + 'SFDS_With_Preview.sfds');
end;

procedure TSFDSCreator.ProgressProc(Sender : TSFDSCustomSource; Progress, MaxProgress, ThisTime : Int64; var Cancel : Boolean; ArcName, StreamName : string);
begin
  ProgressBar.Position := SolveForY(Progress, MaxProgress);
  PercentPanel.Caption := Format('%2f%%', [(ProgressBar.Position / 100)]);
  Application.ProcessMessages;
  Cancel := CancelAct;
end;

procedure TSFDSCreator.ActAddFileExecute(Sender: TObject);
var Dlg : TAddFileDlg;
    I : Integer;
    L : TListItem;
begin
  if not OpenDialog.Execute then Exit;
  CancelAct := False;
  Dlg := nil;
  try
   Dlg := TAddFileDlg.Create(Application);
   ProgressBar.Show; PercentPanel.Show;
   ProgressBar.Position := 0;
   ShowMenu(False);
   if OpenDialog.Files.Count > 1 then
      begin
      Dlg.SrcFile.Text := IntToStr(OpenDialog.Files.Count) + ' files selected.';
      Dlg.StrName.Enabled := False;
      Dlg.ButtonAdd.Caption := Dlg.ButtonAdd.Caption + '(s)';
      Dlg.ShowModal;
        for I := 0 to OpenDialog.Files.Count - 1 do
            begin
              if CancelAct then Break;
              LabelAct.Caption := 'Adding file: ' + ExtractFileName(OpenDialog.Files.Strings[I]);
              ProgressBar.Position := 0;
              SFDSFileWriter.WriteFile(OpenDialog.Files.Strings[I], ExtractFileName(OpenDialog.Files.Strings[I]), ProgressProc, StrToIntDef(Dlg.FileAttr.Text, 0), Dlg.CompressionFormatCombo.ItemIndex, TSFDSCompressionLevel(Dlg.CompressionTypeCombo.ItemIndex), csDefault, Dlg.ExtraDataStr.Text, StrToIntDef(Dlg.ExtraDataInt.Text, 0), True);
              L := FileListView.Items.Add;
              L.Caption := ExtractFileName(OpenDialog.Files.Strings[I]);
              L.SubItems.Add(IntToStr(SFDSFileWriter.OpenedStreamSize));
              L.SubItems.Add(IntToStr(StrToIntDef(Dlg.FileAttr.Text, 0)));
              L.SubItems.Add(CompFmt[Dlg.CompressionFormatCombo.ItemIndex]);
              if Dlg.CompressionFormatCombo.ItemIndex = 0 then L.SubItems.Add('Stored')
                 else L.SubItems.Add(CompLevel[TSFDSCompressionLevel(Dlg.CompressionTypeCombo.ItemIndex)]);
              L.SubItems.Add(Dlg.ExtraDataStr.Text);
              L.SubItems.Add(IntToStr(StrToIntDef(Dlg.ExtraDataInt.Text, 0)));
              L.Selected := True; L.MakeVisible(False);
            end;
      end
        else
          begin
          Dlg.SrcFile.Text := OpenDialog.FileName;
          Dlg.StrName.Text := ExtractFileName(OpenDialog.FileName);
          Dlg.ShowModal;
          LabelAct.Caption := 'Adding file: ' + ExtractFileName(OpenDialog.FileName);
          ProgressBar.Position := 0;
          SFDSFileWriter.WriteFile(OpenDialog.FileName, Dlg.StrName.Text, ProgressProc, StrToIntDef(Dlg.FileAttr.Text, 0), Dlg.CompressionFormatCombo.ItemIndex, TSFDSCompressionLevel(Dlg.CompressionTypeCombo.ItemIndex), csDefault, Dlg.ExtraDataStr.Text, StrToIntDef(Dlg.ExtraDataInt.Text, 0), True);
          L := FileListView.Items.Add;
          L.Caption := Dlg.StrName.Text;
          L.SubItems.Add(IntToStr(SFDSFileWriter.OpenedStreamSize));
          L.SubItems.Add(IntToStr(StrToIntDef(Dlg.FileAttr.Text, 0)));
          L.SubItems.Add(CompFmt[Dlg.CompressionFormatCombo.ItemIndex]);
          if Dlg.CompressionFormatCombo.ItemIndex = 0 then L.SubItems.Add('Stored')
             else L.SubItems.Add(CompLevel[TSFDSCompressionLevel(Dlg.CompressionTypeCombo.ItemIndex)]);
          L.SubItems.Add(Dlg.ExtraDataStr.Text);
          L.SubItems.Add(IntToStr(StrToIntDef(Dlg.ExtraDataInt.Text, 0)));
          L.Selected := True; L.MakeVisible(False);
          end;
  finally
   Dlg.Free;
   ProgressBar.Hide;
   PercentPanel.Hide;
   ShowMenu(True);
   LabelAct.Caption := '';
  end;
end;

procedure TSFDSCreator.AskForMetadataClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
end;

procedure TSFDSCreator.ShowMenu(Show : Boolean);
var I : Integer;
begin
  for I := 0 to MainMenu.Items.Count - 1 do
      MainMenu.Items.Items[I].Visible := Show;
  BtnFinish.Visible := not Show;
  LabelAct.Caption := '';
end;

procedure TSFDSCreator.BtnFinishClick(Sender: TObject);
begin
  CancelAct := True;
  ShowMenu(True);
  if not IsFileOpened then FileListView.Clear;
end;

procedure TSFDSCreator.ActTestExecute(Sender: TObject);
begin
  if IsFileOpened then
     if MessageDlg('A file is opened for writing. Close it (finish it)?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes
        then exit
        else ActClose.Execute;
  if not OpenDialogTest.Execute then Exit;
  ExtractOrTest(True, OpenDialogTest.FileName, '');
end;

function ShouldReplaceFile(FileName : string) : Boolean;
begin
  if YesToAll then begin result := True; Exit; end;
  if NoToAll then begin result := False; Exit; end;
  ReplaceFileDlgResult := ID_NO;
  try
    FormReplace.EditF.Text := FileName;
    FormReplace.ShowModal;
  except
    ReplaceFileDlgResult := ID_NO;
  end;
  case ReplaceFileDlgResult of
   ID_YES : result := True;
   ID_NO : result := False;
   ID_YESTOALL :
     begin
       YesToAll := True;
       result := True;
     end;
   ID_NOTOALL :
     begin
       NoToAll := True;
       result := False;
     end;
  else
    result := False;
  end;
end;

procedure TSFDSCreator.ExtractOrTest(Test : Boolean; FileName, DestDir : String);
var Pos : Int64;
    Reader : TSFDSFileReader;
    I : Integer;
    L : TListItem;
    Res : Boolean;
    ErrCount : Integer;
const
CTEST : array[boolean] of string = ('Extracting', 'Testing');
begin
  CancelAct := False;
  YesToAll := False;
  NoToAll := False;
  Reader := nil;

  if not SFDS_IsStorageFile(FileName, Pos) then
     begin
     MessageDlg('The selected file is not a SFDS valid file.', mtWarning, [mbOk], 0);
     Exit;
     end;
  ActClose.Execute;
  ShowMenu(False);
 try
  try
    Reader := TSFDSFileReader.Create(FileName, Pos);
    ProgressBar.Position := 0;
    ProgressBar.Show; PercentPanel.Show;
    //Also works:
    //Reader := TSFDSFileReader(SFDS_CreateReader(OpenDialogTest.FileName));
    ErrCount := 0;
    for I := 0 to Reader.FileCount - 1 do
        begin
          if CancelAct then Break;
          LabelAct.Caption := CTEST[Test] + ' file: ' + Reader.FileEntrys.Entrys[I]^.FileName;
          ProgressBar.Position := 0;
          if not Test then
             if FileExists(IncludeTrailingPathDelimiter(DestDir) + Reader.FileEntrys.Entrys[I]^.FileName) then
                if not ShouldReplaceFile(IncludeTrailingPathDelimiter(DestDir) + Reader.FileEntrys.Entrys[I]^.FileName) then
                   Continue;
//                   try
          Res := (Reader.ExtractFile(I , IncludeTrailingPathDelimiter(DestDir) + Reader.FileEntrys.Entrys[I]^.FileName, Test, ProgressProc, False) = E_OK);
//                   except
//                   res := false;
//                   end;
          L := FileListView.Items.Add;
          L.Caption := ExtractFileName(Reader.FileEntrys.Entrys[I]^.FileName);
          if Res then L.ImageIndex := 0  //OK
                 else L.ImageIndex := 1; //Failed
          if not res then Inc(ErrCount);
          L.SubItems.Add(IntToStr(Reader.FileEntrys.Entrys[I]^.FileSize));
          L.SubItems.Add(IntToStr(Reader.FileEntrys.Entrys[I]^.FileAttributes));
          L.SubItems.Add(CompFmt[Reader.FileEntrys.Entrys[I]^.CompressionFormat]);
          if Reader.FileEntrys.Entrys[I]^.CompressionFormat = 0 then L.SubItems.Add('Stored')
            else L.SubItems.Add(CompLevel[TSFDSCompressionLevel(Reader.FileEntrys.Entrys[I]^.CompressionLevel)]);
          L.SubItems.Add(Reader.FileEntrys.Entrys[I]^.ExtraDataStr);
          L.SubItems.Add(IntToStr(Reader.FileEntrys.Entrys[I]^.ExtraDataInt));
          L.Selected := True; L.MakeVisible(False);
        end;
        LabelAct.Caption := Format('Finished ' + CTEST[Test] + ' %s files. (%s error(s) found)', [IntToStr(Reader.FileCount), IntToStr(ErrCount)]);
  finally
  ProgressBar.Hide; PercentPanel.Hide;
  if Assigned(Reader) then Reader.Free;
  end;
 except
  ShowMenu(True);
  FileListView.Clear;
  raise;
 end;
end;

procedure TSFDSCreator.ActExtractExecute(Sender: TObject);
var Dir : String;
begin
  if IsFileOpened then
     if MessageDlg('A file is opened for writing. Close it (finish it)?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes
     then exit
     else ActClose.Execute;
  if not OpenDialogTest.Execute then Exit;
  if not SelectDirectory('Select destination directory...', '', Dir) then Exit;
  ExtractOrTest(False, OpenDialogTest.FileName, Dir);
end;

procedure TSFDSCreator.FormResize(Sender: TObject);
var Max, I : Integer;
begin
  Max := FileListView.ClientWidth;
  for I := FileListView.Columns.Count - 1 downto 1 do
      FileListView.Columns.Items[I].Width := ((10 * Max) div 100);
  FileListView.Columns.Items[0].Width := Max - ((FileListView.Columns.Count - 1) * ((10 * Max) div 100));
end;

procedure TSFDSCreator.estdamagedsfdsfile1Click(Sender: TObject);
begin
  ExtractOrTest(True, ExtractFilePath(Application.ExeName) + 'damaged.sfds', '');
end;

procedure TSFDSCreator.ActShowFileMetaExecute(Sender: TObject);
begin
 if not OpenDialogTest.Execute then exit;
 ShowSFDSPropDialog(OpenDialogTest.FileName);
end;

end.
