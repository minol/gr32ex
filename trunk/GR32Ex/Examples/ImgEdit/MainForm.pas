unit MainForm;

{$I Setting.inc}

{$IFNDEF Designtime_Supports}
{$Message Fatal 'pls define Designtime_Supports first'}
{$ENDIF}

interface

{.$DEFINE SynEdit_Supports}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ImgList, ActnList, ComCtrls,
  Jpeg,
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
  GR32_PNG,
  GR_Layers, 
  GR_StdLayers,
  GR_ImageEx,
  GR_LayerRegisters,
  { SynEdit }
  {$IFDEF SynEdit_Supports}
  SynEdit, SynEditTypes, SynHighlighterPas,
  SynEditRegexSearch, SynEditSearch, SynEditMiscClasses, SynEditHighlighter, SynHighlighterDfm,
  {$ENDIF}
  { gettext }
  gnugettext;

type
  TfrmMain = class(TForm)
    ilMain: TImageList;
    dockTop: TSpTBXDock;
    dockLeft: TSpTBXMultiDock;
    dockRight: TSpTBXMultiDock;
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
    mSelectAll: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    mDel: TSpTBXItem;
    ActionList: TTntActionList;
    actNew: TTntAction;
    actOpen: TTntAction;
    actSave: TTntAction;
    actExit: TTntAction;
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
    pmCustomize: TSpTBXPopupMenu;
    mStandardToolbar: TSpTBXItem;
    mFormattingToolbar: TSpTBXItem;
    mCommandsLog: TSpTBXItem;
    mmmHelp: TSpTBXItem;
    mAbout: TSpTBXItem;
    mPrint: TSpTBXItem;
    actPrint: TTntAction;
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
    aCustomize: TTntAction;
    aEmbeddedCustomize: TTntAction;
    SpTBXSeparatorItem13: TSpTBXSeparatorItem;
    sptLeft: TSpTBXSplitter;
    SpTBXSplitter2: TSpTBXSplitter;
    tLanguages: TSpTBXComboBox;
    TBControlItem1: TTBControlItem;
    tLayouts: TSpTBXComboBox;
    TBControlItem4: TTBControlItem;
    SpTBXStatusBar1: TSpTBXStatusBar;
    tbComponentPallete: TSpTBXToolbar;
    tbiDesign: TSpTBXTabItem;
    tbsDesign: TSpTBXTabSheet;
    actDel: TTntAction;
    pnlInspector: TSpTBXDockablePanel;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    pmLayer: TSpTBXPopupMenu;
    Button1: TButton;
    Button2: TButton;
    tbiSource: TSpTBXTabItem;
    tbsSource: TSpTBXTabSheet;
    tbiScript: TSpTBXTabItem;
    tbsScript: TSpTBXTabSheet;
    tbTest: TSpTBXToolbar;
    actRun: TTntAction;
    SpTBXItem3: TSpTBXItem;
    actBringToFront: TTntAction;
    actSendToBack: TTntAction;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
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
    procedure actExitExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actPasteUpdate(Sender: TObject);
    procedure actDelUpdate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure tabMainActiveTabChanging(Sender: TObject; TabIndex,
      NewTabIndex: Integer; var Allow: Boolean);
    procedure actRunExecute(Sender: TObject);
    procedure actBringToFrontExecute(Sender: TObject);
    procedure actSendToBackExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
  private
    { Private declarations }
    FOldLayerListNotification: TLayerListNotifyEvent;
    procedure NotifyList(Sender: TLayerCollection; Action: TLayerListNotification; Layer: TCustomLayer; Index: Integer);
  protected
    FImageEditor: TImage32Editor;
    mmoSource: {$IFDEF SynEdit_Supports}TSynEdit{$ELSE}TMemo{$ENDIF};
    mmoScript: {$IFDEF SynEdit_Supports}TSynEdit{$ELSE}TMemo{$ENDIF};
    {$IFDEF SynEdit_Supports}
    FSynDfm: TSynDfmSyn;
    {$ENDIF}
    FClipObj: TGRLayer;
    FIsSourceChanged: Boolean;
    FAppPath: string;
    FIniPath: string;
    procedure DoCreateLayer(Sender: TObject);
    procedure DoObjSourceChanged(Sender: TObject);
    procedure SetClipObj(const Value: TGRLayer);
  public
    { Public declarations }
    procedure FillLayoutList(CurrentLayout: string = '');
    constructor Create(aComponent: TComponent);override;
    destructor Destroy; override;
    
    property ClipObj: TGRLayer read FClipObj write SetClipObj;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  TntSystem, TntForms, GR_LayerEditors, GR_LayerInspector
  , TestForm
  ;

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
  tLanguages.ItemIndex := 1;
  tLanguagesItemClick(nil);
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
  if S <> '' then 
  begin
    TBXCustomizer.SaveLayout(FIniPath, S);
    FillLayoutList(S);
  end;
end;

{ Actions }

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
  if (L > 2) and (LanguageCode[1] = '[') then 
  begin
    I := Pos(']', LanguageCode);
    if (I > 0) then
      LanguageCode := Copy(LanguageCode, 2, I - 2);
  end;

  // Override Delphi's automatic ResourceString conversion to Ansi
  if UnicodeResourceStrings then 
  begin
    TntSystem.InstallTntSystemUpdates;
    // Override TNT's LoadResString function
    // This is necessary because dxGetText uses a different
    // way to access the translated ResourceStrings.
    TntSystem.WideCustomLoadResString := MyWideCustomLoadResString;
  end;

  if ShellFont then 
  begin
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
  if (L > 2) and (LanguageCode[1] = '[') then 
  begin
    I := Pos(']', LanguageCode);
    if (I > 0) then
      LanguageCode := Copy(LanguageCode, 2, I - 2);
  end;

  if LanguageCode <> gnugettext.GetCurrentLanguage then 
  begin
    gnugettext.UseLanguage(LanguageCode);
    for I := Low(AComponents) to High(AComponents) do 
    begin
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
  if I > -1 then
  begin
    tLanguages.Text := tLanguages.Items[I];
    // Change language and retranslate
    SpDxGetTextChangeLanguage(tLanguages.Text, [Self]);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SpDxGetTextInitialize('en', [Self], True, True);
  tLanguages.Items.LoadFromFile('lang.ini');
end;

constructor TfrmMain.Create(aComponent: TComponent);
var
  i: integer;
  vItem: TTBItem;
  vBmp: TBitmap;
begin
  inherited;
  FImageEditor := TImage32Editor.Create(Self);
  FImageEditor.Parent := tbsDesign;
  FImageEditor.Align := alClient;
  FOldLayerListNotification := TLayerCollectionAccess(FImageEditor.Layers).OnListNotify;
  TLayerCollectionAccess(FImageEditor.Layers).OnListNotify := NotifyList;
  FImageEditor.PopupMenu := pmLayer;
  with GLayerInspector do
  begin
    Parent := pnlOptions;
    BorderStyle := bsNone;
    Left:=0;
    Top:=0;
    Align := alClient;
    Editor := FImageEditor;
    Visible := True;
  end;

  with GLayerClasses.LockList do
  try
    //##tbComponentPallete.
    for i := 0 to count -1 do
    if TGRLayerClass(Items[i]).IsVisibleInEditor then
    begin
      vItem := TSpTBXItem.Create(Self);
      vItem.ParentComponent := tbComponentPallete;

      //vRes := FindResource(HInstance, PChar(TGRLayerClass(Items[i]).ClassName), RT_BITMAP);
      //if vRes <> 0 then
      begin
        vBmp := TBitmap.Create;
        try
         try
          vBmp.LoadFromResourceName(HInstance, TGRLayerClass(Items[i]).ClassName);
          if not vBmp.Empty then
          begin
            vItem.Images := ilMain;
            vItem.ImageIndex := ilMain.Add(vBmp, nil);
          end;
         except
         end;
        finally
          vBmp.Free;
        end;
      end;
      vItem.Caption := TSDPlayingLayerClass(Items[i]).ClassName;
      vItem.Tag := Integer(Items[i]);
      vItem.OnClick := DoCreateLayer;
      tbComponentPallete.Items.Add(vitem);
    end;
  finally
    GLayerClasses.UnlockList;
  end;
 {$IFDEF SynEdit_Supports}
  FSynDfm := TSynDfmSyn.Create(Self);
 {$ENDIF}
  mmoSource := {$IFDEF SynEdit_Supports}TSynEdit{$ELSE}TMemo{$ENDIF}.Create(Self);
  with mmoSource do
  begin
    Parent := tbsSource;
    Align := alClient;
    OnChange := DoObjSourceChanged;
    {$IFDEF SynEdit_Supports}
    Highlighter := FSynDfm;
    {$ENDIF}
  end;
  mmoScript := {$IFDEF SynEdit_Supports}TSynEdit{$ELSE}TMemo{$ENDIF}.Create(Self);
  with mmoScript do
  begin
    Parent := tbsScript;
    Align := alClient;
  end;
end;
destructor TfrmMain.Destroy;
begin
  TLayerCollectionAccess(FImageEditor.Layers).OnListNotify := FOldLayerListNotification;
  inherited;
end;

procedure TfrmMain.actSelectPointerExecute(Sender: TObject);
begin
  //select pt
end;

procedure TfrmMain.DoCreateLayer(Sender: TObject);
var
  vLayer: TGRPropertyLayer;
begin
  if (Sender is TTBItem) then
    with Sender as TTBItem do
    begin
      if (tag <> 0) then
      begin
        vLayer := FImageEditor.CreateLayer(TGRLayerClass(tag))
      end;
    end;
end;

procedure TfrmMain.actDelExecute(Sender: TObject);
begin
  FImageEditor.RemoveSelectedLayer;
end;

procedure TfrmMain.actPasteExecute(Sender: TObject);
begin
  if Assigned(ClipObj) then
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  ClipObj := TGRLayer(FImageEditor.Selection);
end;

procedure TfrmMain.actCutExecute(Sender: TObject);
begin
  //
end;

procedure TfrmMain.NotifyList(Sender: TLayerCollection; Action: TLayerListNotification; Layer: TCustomLayer; Index: Integer);
begin
  if (Action = lnCleared) or ((Action = lnLayerDeleted) and (FClipObj = Layer)) then
    FClipObj := nil;
  if Assigned(FOldLayerListNotification) then
    FOldLayerListNotification(Sender, Action, Layer, Index);
end;

procedure TfrmMain.SetClipObj(const Value: TGRLayer);
begin
  if FClipObj <> Value then
  begin
    FClipObj := Value;
  end;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
begin
  with dlgSave do
  begin
    if Execute then
      FImageEditor.SaveToFile(FileName);
  end;
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
begin
  with dlgOpen do
  begin
    if Execute then
      FImageEditor.LoadFromFile(FileName);
  end;
end;

procedure TfrmMain.actNewExecute(Sender: TObject);
begin
  FImageEditor.Clear;
end;

procedure TfrmMain.actPasteUpdate(Sender: TObject);
begin
  //
end;

procedure TfrmMain.actDelUpdate(Sender: TObject);
begin
 actDel.Enabled := Assigned(FImageEditor.Selection);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  if Assigned(FImageEditor.Selection) then
    //TGRLayer(FImageEditor.Selection).Top := 0;
    
    with TGRLayerAnimator_Sample.Create(TGRLayer(FImageEditor.Selection), 100) do
    begin
      FreeOnTerminate := True;
      Start;
    end; //}
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  if Assigned(FImageEditor.Selection) then
    //TGRLayer(FImageEditor.Selection).Top := 0;

    {with TGRLayerAnimator_Sample.Create(TGRLayer(FImageEditor.Selection), 100) do
    begin
      FreeOnTerminate := True;
      Start;
    end; //}
end;

procedure TfrmMain.tabMainActiveTabChanging(Sender: TObject; TabIndex,
  NewTabIndex: Integer; var Allow: Boolean);
begin
  if NewTabIndex = tabMain.Items.IndexOf(tbiSource) then
  begin
    mmoSource.Lines.Text := FImageEditor.ToString;
    FIsSourceChanged := False;
  end
  else if NewTabIndex = tabMain.Items.IndexOf(tbiDesign) then
  begin
    if FIsSourceChanged then
    begin
      try
        FImageEditor.LoadFromString(mmoSource.Lines.Text);
      except
        On E: Exception do ShowMessage(E.Message);
      end;
      FIsSourceChanged := False;
    end;
  end
end;

procedure TfrmMain.DoObjSourceChanged(Sender: TObject);
begin
  FIsSourceChanged := True;
end;

procedure TfrmMain.actRunExecute(Sender: TObject);
begin
  frmTest.PlayingDesktop.Assign(FImageEditor);
  LayersRemoveClasses(frmTest.Image.layers, [TGRRubberBandLayer, TGRGridLayer]);
  frmTest.Show;
end;

procedure TfrmMain.actBringToFrontExecute(Sender: TObject);
begin
  if Assigned(FImageEditor.Selection) then
    FImageEditor.Selection.BringToFront;
end;

procedure TfrmMain.actSendToBackExecute(Sender: TObject);
begin
  if Assigned(FImageEditor.Selection) then
    FImageEditor.Selection.SendToBack;
end;

procedure TfrmMain.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actBringToFront.Enabled := Assigned(FImageEditor.Selection);
  actSendToBack.Enabled := Assigned(FImageEditor.Selection);
end;

initialization
  //RegisterLayer(TGRLayer);
end.
