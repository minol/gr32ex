unit SFDS_PROPS;

{*****************************************************************************}
{                                                                             }
{  SFDS (Single File Data Storage) Extra Info Properties/Metadata Editor Unit }
{                                                                             }
{  For conditions of distribution and use, see LICENSE.TXT                    }
{                                                                             }
{*****************************************************************************}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, SFDS;

type
  TPropForm = class;
  TSFDSPropFormButtonClick = procedure(Sender : TPropForm; ButtonClicked : Integer) of Object;

  TPropForm = class(TForm)
    PageControl: TPageControl;
    TabSummary: TTabSheet;
    TabCustom: TTabSheet;
    BtnCancel: TButton;
    btnOk: TButton;
    BtnApply: TButton;
    TabStreams: TTabSheet;
    TabPreview: TTabSheet;
    ImagePreview: TImage;
    EditFileName: TLabeledEdit;
    EditGUID: TLabeledEdit;
    EditTitle: TLabeledEdit;
    EditSubject: TLabeledEdit;
    EditAuthor: TLabeledEdit;
    EditKeyWords: TLabeledEdit;
    EditComments: TMemo;
    LabelComments: TLabel;
    EditDateLastSaved: TLabeledEdit;
    ListStream: TListView;
    ListCustom: TListView;
    LabelProp: TLabel;
    EditCustomName: TLabeledEdit;
    EditCustomvalue: TLabeledEdit;
    EditCustomType: TComboBox;
    LabelCtype: TLabel;
    btnCustomAddmodify: TButton;
    btnCustomRemove: TButton;
    EditCustomBool: TRadioGroup;
    btnHelp: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure EditCustomTypeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EditCustomNameChange(Sender: TObject);
    procedure EditCustomBoolClick(Sender: TObject);
    procedure EditCustomvalueChange(Sender: TObject);
    procedure btnCustomAddmodifyClick(Sender: TObject);
    procedure btnCustomRemoveClick(Sender: TObject);
    procedure ListCustomSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormResize(Sender: TObject);
    procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
      MaxWidth, MaxHeight: Integer);
    procedure btnHelpClick(Sender: TObject);
  private
    FButClick: TSFDSPropFormButtonClick;
    FReadOnly: Boolean;
    FModified: Boolean;
    Updating : Boolean;
    { Private declarations }
  public
    CustomFields : TSFDSFileExtraInfoList;
    procedure Prepare;
    procedure SetFModified(V : boolean);
    procedure ClearCustomEdits(AndSetToYes : Boolean = False);
    procedure UpdateButtonsCaptions;
    function EditedMod(var FMod, FFound : Boolean) : Integer;
    procedure UpdateCustomFields;
    property OnButtonClick : TSFDSPropFormButtonClick read FButClick write FButClick;
    property ReadOnly : Boolean read FReadOnly write FReadOnly;
    property Modified : Boolean read FModified;
    { Public declarations }
  end;

  const
  ID_BUTCLICK_OK = 0;
  ID_BUTCLICK_CANCEL = 1;
  ID_BUTCLICK_APPLY = 2;
  ID_BUTCLICK_HELP = 3;

  {Show the properties of the FileName.}
  procedure ShowSFDSPropDialog(FileName : String; OnButtonClicked : TSFDSPropFormButtonClick = nil; ShowHelpBtn : Boolean = False);

  {Shows a dialog to let the user edit the Fields.}
  procedure EditCustomFields(var Fields : TSFDSFileExtraInfoList);

  {
  Shows a dialog to let the user edit the metadata of a SFDS file.
  DlgCaption is the caption of the dialog.
  FileName is the name of the file to which the metadata will be applied.
  GUID is your custom file GUID.
  Fields, Title, Subject, Author, Keywords, Comments are the actual metadata to be edited.
  DateSaved is the date the file was last saved.
  }
  procedure EditMetaData(var Fields : TSFDSFileExtraInfoList; const DlgCaption, FileName : string; const GUID : TGUID; var Title, Subject, Author, Keywords, Comments : string; const DateSaved : TDateTime);

implementation

{$R *.dfm}

  resourcestring
  SYESSTR = 'Yes';
  SNOSTR = 'No';
  SEDITCFLDS = 'Edit custom fields';
  SEDITMETA  = '%s Properties';
  SADD = 'Add';
  SMODIFY = 'Modify';
  SREMOVE = 'Remove';
  SNOMATCH = 'The value entered does not match with the specified type.  The value will be stored as text.';
  STYPETEXT = 'Text';
  STYPENR = 'Number';
  STYPEDATE = 'Date';
  STYPEYORN = 'Yes or No';
  STYPEBIN  = 'Binary';
  SBINARYNS = '<Binary Data>';
  SNOBINEDIT = 'This editor cannot save input text as binary metadata.' + #13#10 + 'Switching to text type.';

  const //don't localize
  SPREVIEWBMP = '$SFDS$PREVIEW$BMP$';
  SPREVIEWTRANSPARENT = '$SFDS$PREVIEW$TRANSPARENT$';

  const
  BDEFYESNO : array[boolean] of String = (SNOSTR, SYESSTR);

procedure EditMetaData(var Fields : TSFDSFileExtraInfoList; const DlgCaption, FileName : string; const GUID : TGUID; var Title, Subject, Author, Keywords, Comments : string; const DateSaved : TDateTime);
var
  PropForm: TPropForm;
  I : Integer;
  L : TListItem;
begin
    PropForm := nil;
  try
    PropForm := TPropForm.Create(Application);
    PropForm.Updating := True;
    PropForm.ReadOnly := False;
    PropForm.CustomFields.Assign(Fields);
    PropForm.TabSummary.TabVisible := True;
    PropForm.TabStreams.TabVisible := False;
    PropForm.TabPreview.TabVisible := False;
    if DlgCaption <> '' then PropForm.Caption := DlgCaption
                        else PropForm.Caption := Format(SEDITMETA, [ExtractFileName(FileName)]);
    PropForm.EditFileName.Text := FileName;
    PropForm.EditGUID.Text := GUIDToString(GUID);
    PropForm.EditDateLastSaved.Text := DateTimeToStr(DateSaved);
    PropForm.EditTitle.Text := Title;
    PropForm.EditSubject.Text := Subject;
    PropForm.EditAuthor.Text := Author;
    PropForm.EditKeyWords.Text := Keywords;
    PropForm.EditComments.Text := Comments;
     try
          PropForm.ListCustom.Clear;
          for I := 0 to PropForm.CustomFields.Count - 1 do
             begin
               L := PropForm.ListCustom.Items.Add;
               L.Caption := PropForm.CustomFields.ExtraInfoField[I]^.Name;
               case Integer(PropForm.CustomFields.ExtraInfoField[I]^.FieldType) of
                 0 : L.SubItems.Add(Pointer2String(PropForm.CustomFields.ExtraInfoField[I]^.ValueString, PropForm.CustomFields.ExtraInfoField[I]^.ValueStringSize));
                 1 : L.SubItems.Add(IntToStr(PropForm.CustomFields.ExtraInfoField[I]^.ValueNumber));
                 2 : L.SubItems.Add(DateToStr(PropForm.CustomFields.ExtraInfoField[I]^.ValueDate));
                 3 : L.SubItems.Add(BDEFYESNO[Boolean(PropForm.CustomFields.ExtraInfoField[I]^.ValueBoolean)]);
                 4 : L.SubItems.Add(SBINARYNS);
               end;
               L.SubItems.Add(PropForm.EditCustomType.Items.Strings[Integer(PropForm.CustomFields.ExtraInfoField[I]^.FieldType)]);
               L.SubItems.Add(IntToStr(Integer(PropForm.CustomFields.ExtraInfoField[I]^.FieldType)));
             end;
     finally
     end;
    PropForm.Prepare;
    PropForm.Updating := False;
    PropForm.BtnApply.Hide;
    if (PropForm.ShowModal = mrOK) and (PropForm.Modified) then
       begin
         Fields.Assign(PropForm.CustomFields);
         Title := PropForm.EditTitle.Text;
         Subject := PropForm.EditSubject.Text;
         Author := PropForm.EditAuthor.Text;
         Keywords := PropForm.EditKeyWords.Text;
         Comments := PropForm.EditComments.Text;
       end;
  finally
    PropForm.Free;
  end;
end;

procedure EditCustomFields(var Fields : TSFDSFileExtraInfoList);
var
  PropForm: TPropForm;
  I : Integer;
  L : TListItem;
begin
    PropForm := nil;
  try
    PropForm := TPropForm.Create(Application);
    PropForm.Updating := True;
    PropForm.ReadOnly := False;
    PropForm.CustomFields.Assign(Fields);
    PropForm.TabSummary.TabVisible := False;
    PropForm.TabStreams.TabVisible := False;
    PropForm.TabPreview.TabVisible := False;
    PropForm.Caption := SEDITCFLDS;
     try
          PropForm.ListCustom.Clear;
          for I := 0 to PropForm.CustomFields.Count - 1 do
             begin
               L := PropForm.ListCustom.Items.Add;
               L.Caption := PropForm.CustomFields.ExtraInfoField[I]^.Name;
               case Integer(PropForm.CustomFields.ExtraInfoField[I]^.FieldType) of
                 0 : L.SubItems.Add(Pointer2String(PropForm.CustomFields.ExtraInfoField[I]^.ValueString, PropForm.CustomFields.ExtraInfoField[I]^.ValueStringSize));
                 1 : L.SubItems.Add(IntToStr(PropForm.CustomFields.ExtraInfoField[I]^.ValueNumber));
                 2 : L.SubItems.Add(DateToStr(PropForm.CustomFields.ExtraInfoField[I]^.ValueDate));
                 3 : L.SubItems.Add(BDEFYESNO[Boolean(PropForm.CustomFields.ExtraInfoField[I]^.ValueBoolean)]);
                 4 : L.SubItems.Add(SBINARYNS);
               end;
               L.SubItems.Add(PropForm.EditCustomType.Items.Strings[Integer(PropForm.CustomFields.ExtraInfoField[I]^.FieldType)]);
               L.SubItems.Add(IntToStr(Integer(PropForm.CustomFields.ExtraInfoField[I]^.FieldType)));
             end;
     finally
     end;
    PropForm.Prepare;
    PropForm.Updating := False;
    PropForm.BtnApply.Hide;    
    if (PropForm.ShowModal = mrOK) and (PropForm.Modified) then
       begin
         Fields.Assign(PropForm.CustomFields);
       end;
  finally
    PropForm.Free;
  end;
end;

procedure ShowSFDSPropDialog(FileName : String; OnButtonClicked : TSFDSPropFormButtonClick = nil; ShowHelpBtn : Boolean = False);
var
  PropForm: TPropForm;
  R : TSFDSCustomReader;
  PO : Int64;
  I  : Integer;
  L  : TListItem;
  S : string;
  G : TGUID;
  ST : TStream;
  FF : string;
  K : Integer;
begin
    PropForm := nil; R := nil;
    if not SFDS_IsStorageFileEx(FileName, Po, G) then Exit;
  try
    PropForm := TPropForm.Create(Application);
    PropForm.Updating := True;
     try
      R := SFDS_CreateReader(FileName, True);
      PropForm.ListStream.Clear;
      for I := 0 to R.FileCount - 1 do
          begin
            L := PropForm.ListStream.Items.Add;
            L.Caption := R.FileEntrys.Entrys[I]^.FileName;
            L.SubItems.Add(IntToStr(R.FileEntrys.Entrys[I]^.FileSize));
            try
            if R.FileEntrys.Entrys[I]^.CompressionFormat <> 0 then
                if R.FileEntrys.Entrys[I]^.FileSize > 0 then //avoid division by zero when file size is zero
                   S := FloatToStr(Trunc(100 - (R.FileEntrys.Entrys[I]^.EndPosition - R.FileEntrys.Entrys[I]^.StartPosition)/(R.FileEntrys.Entrys[I]^.FileSize) * 100.0) + 1) + '%'
                   else S := '0%'
                  else //Not compressed
                    S := '0% (not compressed)';
            L.SubItems.Add(S);
            except
            end;
          end;
            PropForm.EditGUID.Text := GUIDToString(G);
            PropForm.EditTitle.Text := R.Title;
            PropForm.EditSubject.Text := R.Subject;
            PropForm.EditAuthor.Text := R.Author;
            PropForm.EditDateLastSaved.Text := DateToStr(R.TimeCreate) + ' ' + TimeToStr(R.TimeCreate);
            PropForm.EditKeyWords.Text := R.KeyWords;
            PropForm.EditComments.Text := R.Comments;
            PropForm.CustomFields.Assign(R.ExtraInfoFields);
         ST := nil;
       try
         FF := SPREVIEWBMP;
         if R.FileEntrys.FileExists(FF) then
            begin
            ST := R.OpenStream(FF, PO);
            PropForm.ImagePreview.Picture.Bitmap.LoadFromStream(ST);
            ST.Free;
            K := PropForm.CustomFields.IndexOfField(SPREVIEWTRANSPARENT);
            if K <> -1 then
               begin
                 if PropForm.CustomFields.ExtraInfoField[K].FieldType = EIBoolean then
                    begin
                       PropForm.ImagePreview.Transparent := PropForm.CustomFields.ExtraInfoField[K].ValueBoolean;
                    end;
               end;
            end;
       except
         ST.Free;
       end;
     finally
      R.Free;
     end;
     try
          PropForm.ListCustom.Clear;
          for I := 0 to PropForm.CustomFields.Count - 1 do
             begin
               L := PropForm.ListCustom.Items.Add;
               L.Caption := PropForm.CustomFields.ExtraInfoField[I]^.Name;
               case Integer(PropForm.CustomFields.ExtraInfoField[I]^.FieldType) of
                 0 : L.SubItems.Add(Pointer2String(PropForm.CustomFields.ExtraInfoField[I]^.ValueString, PropForm.CustomFields.ExtraInfoField[I]^.ValueStringSize));
                 1 : L.SubItems.Add(IntToStr(PropForm.CustomFields.ExtraInfoField[I]^.ValueNumber));
                 2 : L.SubItems.Add(DateToStr(PropForm.CustomFields.ExtraInfoField[I]^.ValueDate));
                 3 : L.SubItems.Add(BDEFYESNO[Boolean(PropForm.CustomFields.ExtraInfoField[I]^.ValueBoolean)]);
                 4 : L.SubItems.Add(SBINARYNS);
               end;
               L.SubItems.Add(PropForm.EditCustomType.Items.Strings[Integer(PropForm.CustomFields.ExtraInfoField[I]^.FieldType)]);
               L.SubItems.Add(IntToStr(Integer(PropForm.CustomFields.ExtraInfoField[I]^.FieldType)));
             end;
     finally
     end;
    PropForm.OnButtonClick := OnButtonClicked;
    PropForm.Caption := Format(SEDITMETA, [ExtractFileName(FileName)]);
    PropForm.EditFileName.Text := FileName;
    PropForm.Prepare;
    PropForm.Updating := False;
    PropForm.btnHelp.Visible := ShowHelpBtn;
    PropForm.ShowModal;
  finally
    PropForm.Free;
  end;
end;

procedure TPropForm.FormConstrainedResize(Sender: TObject; var MinWidth,
  MinHeight, MaxWidth, MaxHeight: Integer);
begin
  FormResize(Self);
end;

procedure TPropForm.FormCreate(Sender: TObject);
begin
  ClientHeight := 435;
  ClientWidth := 360;
  PageControl.ActivePageIndex := 0;
  ReadOnly := True;
  SetFModified(False);
  CustomFields := TSFDSFileExtraInfoList.Create;
  Updating := False;
  EditCustomBool.Items.Clear;
  EditCustomBool.Items.Add(SYESSTR);
  EditCustomBool.Items.Add(SNOSTR);
  EditCustomType.Clear;
  EditCustomType.Items.Add(STYPETEXT);
  EditCustomType.Items.Add(STYPENR);
  EditCustomType.Items.Add(STYPEDATE);
  EditCustomType.Items.Add(STYPEYORN);
  EditCustomType.Items.Add(STYPEBIN);  
  ClearCustomEdits(True);
end;

procedure TPropForm.btnOkClick(Sender: TObject);
begin
  UpdateCustomFields;
  if Assigned(OnButtonClick) then OnButtonClick(Self, ID_BUTCLICK_OK);
end;

procedure TPropForm.BtnCancelClick(Sender: TObject);
begin
  if Assigned(OnButtonClick) then OnButtonClick(Self, ID_BUTCLICK_CANCEL);
end;

procedure TPropForm.BtnApplyClick(Sender: TObject);
begin
  BtnApply.Enabled := False;
  UpdateCustomFields;
  if Assigned(OnButtonClick) then OnButtonClick(Self, ID_BUTCLICK_APPLY);
end;

procedure TPropForm.Prepare;
begin
  try
  Updating := True;
  TabPreview.TabVisible := Assigned(ImagePreview.Picture.Graphic) and (not ImagePreview.Picture.Graphic.Empty);
  EditTitle.ReadOnly := ReadOnly;
  EditSubject.ReadOnly := ReadOnly;
  EditAuthor.ReadOnly := ReadOnly;
  EditKeyWords.ReadOnly := ReadOnly;
  EditComments.ReadOnly := ReadOnly;
  if ReadOnly then
     begin
       EditTitle.ParentColor := True;
       EditSubject.ParentColor := True;
       EditAuthor.ParentColor := True;
       EditKeyWords.ParentColor := True;
       EditComments.ParentColor := True;
     end
     else
     begin
       EditTitle.Color := clWindow;
       EditSubject.Color := clWindow;
       EditAuthor.Color := clWindow;
       EditKeyWords.Color := clWindow;
       EditComments.Color := clWindow;
     end;
  EditCustomType.ItemIndex := 0;
  EditCustomTypeChange(EditCustomType);
  ListCustom.ClearSelection; //Removes the selection, leaving all items unselected.
  btnCustomAddmodify.Enabled := False;
  btnCustomRemove.Enabled := False;
  EditCustomName.Enabled := not ReadOnly;
  EditCustomvalue.Enabled := not ReadOnly;
  EditCustomType.Enabled := not ReadOnly;
  EditCustomBool.Enabled := not ReadOnly;
  LabelCtype.Enabled := not ReadOnly;
  finally
  Updating := False;
  end;
end;

procedure TPropForm.EditChange(Sender: TObject);
begin
  if not Updating then SetFModified(True);
end;

procedure TPropForm.EditCustomTypeChange(Sender: TObject);
var B : Boolean;
begin
  case EditCustomType.ItemIndex of
  0..1 : begin B := True; EditCustomvalue.Text := ''; end;
  2 : begin B := True; EditCustomvalue.Text := DateToStr(Now); end;
  3 : begin EditCustomvalue.Text := ''; B := False; end;
  4 : begin
        MessageDlg(SNOBINEDIT, mtWarning, [mbOk], 0);
        EditCustomType.ItemIndex := 0;
        EditCustomTypeChange(EditCustomType);
        Exit;
      end;
  else
  B := True;
  end;
  //EditCustomvalue.Visible := B;
  EditCustomvalue.TabStop := B;
  EditCustomBool.TabStop := not B;
  EditCustomBool.Visible := not B;
  {if EditCustomBool.Visible then
       EditCustomBool.ItemIndex := 0;}
  UpdateButtonsCaptions;
  try
  if Visible then ActiveControl := EditCustomName;
  except
  end;
end;

procedure TPropForm.FormDestroy(Sender: TObject);
begin
  CustomFields.Free;
end;

procedure TPropForm.FormResize(Sender: TObject);
begin
  ListCustom.Column[0].Width := (ListCustom.ClientWidth - 75) div 2;
  ListCustom.Column[1].Width := ListCustom.ClientWidth - 75 - ListCustom.Column[0].Width;
  ListStream.Column[0].Width := ((ListStream.ClientWidth - 75) div 5) * 4;
  ListStream.Column[1].Width := ListStream.ClientWidth - 75 - ListStream.Column[0].Width;
end;

function SSTRB(S : string) : Integer;
  begin
    result := 0;
    if SameText(S, SNOSTR) then result := 1;
  end;

function TPropForm.EditedMod(var FMod, FFound : Boolean) : Integer;
  var I : Integer;
      L : TListItem;
  begin
  FMod := False;
  FFound := False;
  result := -1;
    for I := 0 to ListCustom.Items.Count - 1 do
       begin
         L := ListCustom.Items.Item[I];
         if SameText(L.Caption, EditCustomName.Text) then
            begin
              result := I;
              FFound := True;
              FMod := StrToInt(L.SubItems.Strings[2]) <> EditCustomType.ItemIndex;
              if FMOD then Exit;
              case StrToInt(L.SubItems.Strings[2]) of
              0..2 : FMod := not (SameText(L.SubItems.Strings[0], EditCustomvalue.Text));
              3    : FMod := EditCustomBool.ItemIndex <> SSTRB(L.SubItems.Strings[0]);
              else
                FMod := not (SameText(L.SubItems.Strings[0], EditCustomvalue.Text));
              end;
            end;
       end;
  end;

procedure TPropForm.UpdateButtonsCaptions;
var FFound : boolean;
    FMod : Boolean;

begin
  EditedMod(FMod, FFound);
  if FFound then btnCustomAddmodify.Caption := SMODIFY
            else btnCustomAddmodify.Caption := SADD;
  btnCustomRemove.Enabled := FFound;
  btnCustomAddmodify.Enabled := (FMod or (not FFound)) and (EditCustomName.Text <> '');
  btnCustomRemove.Caption := SREMOVE;
end;

procedure TPropForm.EditCustomNameChange(Sender: TObject);
begin
  UpdateButtonsCaptions;
end;

procedure TPropForm.EditCustomBoolClick(Sender: TObject);
begin
  UpdateButtonsCaptions;
end;

procedure TPropForm.EditCustomvalueChange(Sender: TObject);
begin
  UpdateButtonsCaptions;
end;

procedure TPropForm.btnCustomAddmodifyClick(Sender: TObject);
var FMod, FFound : Boolean;
    AtPos : Integer;
    L : TListItem;
    B  : Boolean;
begin
  AtPos := EditedMod(FMod, FFound);
  try
  case EditCustomType.ItemIndex of
    1 : StrToInt64(EditCustomvalue.Text);
    2 : StrToDate(EditCustomvalue.Text);
  end;
  except
    case MessageDlg(SNOMATCH, mtWarning, [mbOK, mbCancel], 0) of
      mrOK : EditCustomType.ItemIndex := 0;
      mrCANCEL : Exit;
    end;
  end;
  if not FFound then
     begin
     L := ListCustom.Items.Add;
     L.SubItems.Add(''); L.SubItems.Add(''); L.SubItems.Add('');
     end
     else
     begin
     L := ListCustom.Items.Item[AtPos];
     end;

     L.Caption := EditCustomName.Text;
     case EditCustomBool.ItemIndex of
       0 : B := True;
       1 : B := False;
     else
       B := True;
     end;
     case EditCustomType.ItemIndex of
     0..2 : L.SubItems.Strings[0] := EditCustomvalue.Text;
     3    : L.SubItems.Strings[0] := BDEFYESNO[B];
     else
       L.SubItems.Strings[0] := EditCustomvalue.Text;
     end;
     L.SubItems.Strings[1] := EditCustomType.Items.Strings[EditCustomType.ItemIndex];
     L.SubItems.Strings[2] := IntToStr(EditCustomType.ItemIndex);
     SetFModified(True);
     ClearCustomEdits(True);
     ListCustom.ClearSelection;//Removes the selection, leaving all items unselected.  
end;

procedure TPropForm.btnCustomRemoveClick(Sender: TObject);
var L : TListItem;
    AtPos : Integer;
    FMod, FFound : Boolean;
begin
  SetFModified(True);
  AtPos := EditedMod(FMod, FFound);
  if not FFound then Exit;
  L := ListCustom.Items.Item[ATPos];
  if L = nil then Exit;
  L.Delete;
  ClearCustomEdits(True);
end;

procedure TPropForm.btnHelpClick(Sender: TObject);
begin
  if Assigned(OnButtonClick) then OnButtonClick(Self, ID_BUTCLICK_HELP);
end;

procedure TPropForm.ClearCustomEdits(AndSetToYes : Boolean = False);
begin
  EditCustomName.Text := '';
  EditCustomvalue.Text := '';
  EditCustomType.ItemIndex := 0;
  EditCustomTypeChange(EditCustomType);
  if AndSetToYes then EditCustomBool.ItemIndex := 0;
end;

procedure TPropForm.UpdateCustomFields;
var I : Integer;
    E : TSFDSFileExtraInfoField;
    L : TListItem;
begin
  if not Modified then Exit;
  CustomFields.Clear;
  for I := 0 to ListCustom.Items.Count - 1 do
      begin
        L := ListCustom.Items.Item[I];
        E.Name := L.Caption;
        E.FieldType := TSFDSFileExtraInfo(StrToIntDef(L.SubItems.Strings[2], 0));
        case E.FieldType of
         EIStr :
           begin
           String2Pointer(L.SubItems.Strings[0], E.ValueString);
           E.ValueStringSize := Length(L.SubItems.Strings[0]);
           end;
         EINumber : E.ValueNumber := StrToInt64Def(L.SubItems.Strings[0], 0);
         EIDate : E.ValueDate := StrToDateDef(L.SubItems.Strings[0], Now);
         EIBoolean : E.ValueBoolean := not Boolean(SSTRB(L.SubItems.Strings[0]));
        else
          continue;
        end;
        CustomFields.AddOrUpdateField(E);
      end;
end;

procedure TPropForm.ListCustomSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var L : TListItem;
begin
  L := ListCustom.Selected;
  if L = nil then
     Exit;
  if ReadOnly then Exit;
  EditCustomName.Text := L.Caption;
  EditCustomType.ItemIndex := StrToInt(L.SubItems.Strings[2]);
  EditCustomvalue.Text := '';
  EditCustomBool.ItemIndex := 0;
  case EditCustomType.ItemIndex of
  0..2 : EditCustomvalue.Text := L.SubItems.Strings[0];
  3    : EditCustomBool.ItemIndex := SSTRB(L.SubItems.Strings[0]);
  else
    EditCustomvalue.Text := L.SubItems.Strings[0];
  end;
  EditCustomTypeChange(EditCustomType);
end;

procedure TPropForm.SetFModified(V: boolean);
begin
  FModified := V;
  if FModified and (not ReadOnly)  and (not Updating) then BtnApply.Enabled := True;
end;

end.
