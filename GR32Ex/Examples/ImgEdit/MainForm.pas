unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ImgList, ActnList, ComCtrls,
  TntActnList, TntDialogs, TntStdCtrls,
  { TB2K }
  TB2Dock, TB2Toolbar, TB2Item, TB2ExtItems,
  TBXDkPanels,
  SpTBXSkins, SpTBXItem, SpTBXControls, SpTBXDkPanels, SpTBXTabs, SpTBXEditors,
  SpTBXCustomizer,
  {GR32}
  GR32,
  GR32_Layers,
  GR32_Image,
  GR_ImageEx,
  GR_Layers,
  { gettext }
  gnugettext;

type
  TfrmMain = class(TForm)
    ilMain: TImageList;
    dockTop: TSpTBXDock;
    SpTBXMultiDock1: TSpTBXMultiDock;
    SpTBXMultiDock2: TSpTBXMultiDock;
    dockBottom: TSpTBXDock;
    tbStandard: TSpTBXToolbar;
    tbMenuBar: TSpTBXToolbar;
    mFile: TSpTBXSubmenuItem;
    mEdit: TSpTBXSubmenuItem;
    mView: TSpTBXSubmenuItem;
    mHelp: TSpTBXSubmenuItem;
    mNew: TSpTBXItem;
    mOpen: TSpTBXItem;
    mSave: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    mExit: TSpTBXItem;
    mCut: TSpTBXItem;
    mCopy: TSpTBXItem;
    mPaste: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    mSelectAll: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    mFind: TSpTBXItem;
    TntActionList1: TTntActionList;
    aNew: TTntAction;
    aOpen: TTntAction;
    aSave: TTntAction;
    aExit: TTntAction;
    actCut: TTntAction;
    actCopy: TTntAction;
    actPaste: TTntAction;
    aSelectAll: TTntAction;
    aFind: TTntAction;
    aBold: TTntAction;
    aItalic: TTntAction;
    aUnderline: TTntAction;
    aLeftJustify: TTntAction;
    aRightJustify: TTntAction;
    aCentered: TTntAction;
    aBullets: TTntAction;
    aNumberedBullets: TTntAction;
    mLeftJustify: TSpTBXItem;
    mUnderline: TSpTBXItem;
    mItalic: TSpTBXItem;
    mBold: TSpTBXItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    mRightJustify: TSpTBXItem;
    mCentered: TSpTBXItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    mBullets: TSpTBXItem;
    mNumberedBullets: TSpTBXItem;
    mSidebar: TSpTBXSubmenuItem;
    mOptions: TSpTBXItem;
    mmHelp: TSpTBXItem;
    mToolbars: TSpTBXSubmenuItem;
    SpTBXPopupMenu1: TSpTBXPopupMenu;
    mStandardToolbar: TSpTBXItem;
    mFormattingToolbar: TSpTBXItem;
    mNavigationToolbar: TSpTBXItem;
    mCommandsLog: TSpTBXItem;
    mmmHelp: TSpTBXItem;
    mAbout: TSpTBXItem;
    mPrint: TSpTBXItem;
    aPrint: TTntAction;
    actSelectPointer: TTntAction;
    pGroupItem1: TTBGroupItem;
    SpTBXSeparatorItem10: TSpTBXSeparatorItem;
    pCustomize: TSpTBXItem;
    tbLayouts: TSpTBXToolbar;
    tLayoutSave: TSpTBXItem;
    SpTBXLabelItem1: TSpTBXLabelItem;
    tLayoutsToolbar: TSpTBXItem;
    tbxCustomizer: TSpTBXCustomizer;
    cPrint: TSpTBXItem;
    cLeftJustify: TSpTBXItem;
    cCentered: TSpTBXItem;
    cNumBullets: TSpTBXItem;
    cBullets: TSpTBXItem;
    cRightJustify: TSpTBXItem;
    cFavs: TSpTBXItem;
    cExit: TSpTBXItem;
    cSelectAll: TSpTBXItem;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    tabMain: TSpTBXTabControl;
    tSkins: TSpTBXSubmenuItem;
    SpTBXSkinGroupItem1: TSpTBXSkinGroupItem;
    pEmbeddedCustomize: TSpTBXItem;
    aCustomize: TTntAction;
    aEmbeddedCustomize: TTntAction;
    SpTBXSeparatorItem13: TSpTBXSeparatorItem;
    SpTBXSplitter1: TSpTBXSplitter;
    SpTBXSplitter2: TSpTBXSplitter;
    tLanguages: TSpTBXComboBox;
    TBControlItem1: TTBControlItem;
    tLayouts: TSpTBXComboBox;
    TBControlItem4: TTBControlItem;
    SpTBXStatusBar1: TSpTBXStatusBar;
    tbComponentPallete: TSpTBXToolbar;
    SpTBXTabItem1: TSpTBXTabItem;
    tbsDesign: TSpTBXTabSheet;
    actDel: TTntAction;
    procedure ActionsExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tLayoutSaveClick(Sender: TObject);
    procedure tLayoutsItemClick(Sender: TObject);
    procedure aCustomizeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tLanguagesItemClick(Sender: TObject);
    procedure actSelectPointerExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
  private
    { Private declarations }
    //procedure NotifyClipObjFree(Sender: TObject);
    procedure NotifyList(Sender: TLayerCollection; Action: TLayerListNotification; Layer: TCustomLayer; Index: Integer);
  protected
    FImageEditor: TImage32Editor;
    FClipObj: TGRLayerControl;
    procedure doSelectControl(Sender: TObject);
    procedure SetClipObj(const Value: TGRLayerControl);
  public
    { Public declarations }
    FAppPath: string;
    FIniPath: string;
    procedure FillLayoutList(CurrentLayout: string = '');
    constructor Create(aComponent: TComponent);override;
    
    property ClipObj: TGRLayerControl read FClipObj write SetClipObj;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  TntSystem, TntForms, GR_LayerEditors;

{$R *.dfm}

type
  TLayerCollectionAccess = class(TLayerCollection);
{ Form }

procedure TfrmMain.FormShow(Sender: TObject);
begin
  FAppPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  FIniPath := FAppPath + 'Options.ini';

  // Load the items positions and the last layout from the ini file
  tbxCustomizer.Load(FIniPath);

  // Load the layout list
  FillLayoutList('LastLayout');

  tbxCustomizer.MenuBar := tbMenuBar;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Save the items positions and the current layout to the Ini file
  tbxCustomizer.Save(FIniPath);
end;

{ Layouts UI }

procedure TfrmMain.FillLayoutList(CurrentLayout: string);
var
  I: integer;
begin
  // Fill the tLayouts combobox
  tLayouts.Items.Clear;
  for I := 0 to TBXCustomizer.Layouts.Count - 1 do
    tLayouts.Items.Add(TBXCustomizer.Layouts[I]);
  I := tLayouts.Items.IndexOf(CurrentLayout);
  if I > -1 then
    tLayouts.ItemIndex := I;
end;

procedure TfrmMain.tLayoutsItemClick(Sender: TObject);
begin
  if tLayouts.ItemIndex > -1 then
    TBXCustomizer.LoadLayout(FIniPath, tLayouts.Items[tLayouts.ItemIndex]);
end;

procedure TfrmMain.tLayoutSaveClick(Sender: TObject);
var
  S: string;
begin
  S := TntDialogs.WideInputBox(_('Save Layout'), _('Save current layout as:'), '');
  if S <> '' then begin
    TBXCustomizer.SaveLayout(FIniPath, S);
    FillLayoutList(S);
  end;
end;

{ Actions }

procedure TfrmMain.ActionsExecute(Sender: TObject);
begin
  //if Sender is TAction then
    //Memo1.Lines.Add(TAction(Sender).Caption + ' ' + _('Executed'));
end;

procedure TfrmMain.aCustomizeExecute(Sender: TObject);
begin
  TBXCustomizer.Show;
end;

{ Languages }

function MyWideCustomLoadResString(ResStringRec: PResStringRec; var Value: WideString): Boolean;
begin
  Result := True;
  Value := GnuGetText.LoadResStringW(ResStringRec);
end;

procedure SpDxGetTextInitialize(LanguageCode: string; AComponents: array of TComponent; ShellFont, UnicodeResourceStrings: Boolean);
// LanguageCode can be an ISO language code: 'en', 'es', 'ko'
// And also can be the ISO code plus a description: '[en] English', '[es] Spanish', '[ko] Korean'
var
  I, L: Integer;
begin
  // Get the ISO language code
  L := Length(LanguageCode);
  if (L > 2) and (LanguageCode[1] = '[') then begin
    I := Pos(']', LanguageCode);
    if (I > 0) then
      LanguageCode := Copy(LanguageCode, 2, I - 2);
  end;

  // Override Delphi's automatic ResourceString conversion to Ansi
  if UnicodeResourceStrings then begin
    TntSystem.InstallTntSystemUpdates;
    // Override TNT's LoadResString function
    // This is necessary because dxGetText uses a different
    // way to access the translated ResourceStrings.
    TntSystem.WideCustomLoadResString := MyWideCustomLoadResString;
  end;

  if ShellFont then begin
    if  (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) then
      DefFontData.Name := 'MS Shell Dlg 2'
    else
      DefFontData.Name := 'MS Shell Dlg';
  end;

  gnugettext.TP_GlobalIgnoreClassProperty(TAction,'Category');
  gnugettext.TP_GlobalIgnoreClassProperty(TControl,'HelpKeyword');
  gnugettext.TP_GlobalIgnoreClassProperty(TControl,'ImeName');
  gnugettext.TP_GlobalIgnoreClassProperty(TTntAction,'Category');
  gnugettext.TP_GlobalIgnoreClass(Graphics.TFont);
  gnugettext.TP_GlobalIgnoreClass(TSpTBXTabSheet);

  gnugettext.UseLanguage(LanguageCode);
  for I := Low(AComponents) to High(AComponents) do
    gnugettext.TranslateComponent(AComponents[I]);
end;

procedure SpDxGetTextChangeLanguage(LanguageCode: string; AComponents: array of TComponent);
// LanguageCode can be an ISO language code: 'en', 'es', 'ko'
// And also can be the ISO code plus a description: '[en] English', '[es] Spanish', '[ko] Korean'
var
  I, L: Integer;
  C: TComponent;
begin
  // Get the ISO language code
  L := Length(LanguageCode);
  if (L > 2) and (LanguageCode[1] = '[') then begin
    I := Pos(']', LanguageCode);
    if (I > 0) then
      LanguageCode := Copy(LanguageCode, 2, I - 2);
  end;

  if LanguageCode <> gnugettext.GetCurrentLanguage then begin
    gnugettext.UseLanguage(LanguageCode);
    for I := Low(AComponents) to High(AComponents) do begin
      C := AComponents[I];
      SpBeginUpdateAllToolbars(C);
      try
        gnugettext.ReTranslateComponent(C);
      finally
        SpEndUpdateAllToolbars(C);
      end;
    end;
  end;
end;

procedure TfrmMain.tLanguagesItemClick(Sender: TObject);
var
  I: integer;
begin
  I := tLanguages.ItemIndex;
  if I > -1 then begin
    tLanguages.Text := tLanguages.Items[I];
    // Change language and retranslate
    SpDxGetTextChangeLanguage(tLanguages.Text, [Self]);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SpDxGetTextInitialize('en', [Self], True, True);
  tLanguages.Items.LoadFromFile('langcodes.txt');
  tLanguages.ItemIndex := 1;
end;

constructor TfrmMain.Create(aComponent: TComponent);
var
  i: integer;
  vItem: TTBItem;
begin
  inherited;
  FImageEditor := TImage32Editor.Create(Self);
  FImageEditor.Parent := tbsDesign;
  FImageEditor.Align := alClient;
  TLayerCollectionAccess(FImageEditor.Layers).OnListNotify := NotifyList;

  with GLayerControlClasses.LockList do
  try
  //tbComponentPallete.
    for i := 0 to count -1 do
    begin
      vItem := TTBItem.Create(Self);
      vItem.ParentComponent := tbComponentPallete;
      //vItem.Action := aCustomize;
      vItem.Caption := TGRLayerControlClass(Items[i]).ClassName;
      vItem.Tag := Integer(Items[i]);
      vItem.OnClick := doSelectControl;
      tbComponentPallete.Items.Add(vitem);
    end;
  finally
    GLayerControlClasses.UnlockList;
  end;
end;

procedure TfrmMain.actSelectPointerExecute(Sender: TObject);
begin
  //select pt
end;

procedure TfrmMain.doSelectControl(Sender: TObject);
var
  vLayerControl: TGRLayerControl;
  //vEditorClass: TGRLayerEditorClass;
  P: TPoint;
begin
  if (Sender is TTBItem) then
    with Sender as TTBItem do
    begin
      if (tag <> 0) then
      begin
        with FImageEditor.GetViewportRect do
         P := FImageEditor.ControlToBitmap(Point((Right + Left) div 2, (Top + Bottom) div 2));
        vLayerControl := TGRLayerControlClass(tag).Create(FImageEditor.Layers);
        vLayerControl.Left := P.X;
        vLayerControl.Top := P.Y;
        ShowLayerControlEditor(vLayerControl);
        FImageEditor.Selection := vLayerControl;
      end;
    end;
end;

procedure TfrmMain.actDelExecute(Sender: TObject);
var
  vLayerControl: TGRLayerControl;
begin
  vLayerControl := TGRLayerControl(FImageEditor.Selection);
  if Assigned(vLayerControl) then
  begin
    vLayerControl.Free;
  end;
end;

procedure TfrmMain.actPasteExecute(Sender: TObject);
begin
  //
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  ClipObj := TGRLayerControl(FImageEditor.Selection);
end;

procedure TfrmMain.actCutExecute(Sender: TObject);
begin
  //
end;

procedure TfrmMain.NotifyList(Sender: TLayerCollection; Action: TLayerListNotification; Layer: TCustomLayer; Index: Integer);
begin
  if (Action = lnCleared) or ((Action = lnLayerDeleted) and (FClipObj = Layer)) then
    FClipObj := nil;
end;

procedure TfrmMain.SetClipObj(const Value: TGRLayerControl);
begin
  if FClipObj <> Value then
  begin
    FClipObj := Value;
  end;
end;

end.
