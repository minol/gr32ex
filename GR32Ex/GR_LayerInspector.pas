
{ the layer inspector editor : you need the jvcl to compile.}
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is GR_LayerInspector
 *
 * The Initial Developer of the Original Code is Riceball LEE
 * Portions created by Riceball LEE are Copyright (C) 2008
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_LayerInspector;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Dialogs, ExtDlgs
  , JvInspector
  , GR32
  , GR32_Image
  , GR32_Layers
  , GR32_ExtLayers
  , GR_ImageEx
  ;

type
  //the selected control changed, the inspector should be known.
  //when the LayerSelector changed then the editor should change the selection too.
  TGRLayerInspector = class(TForm)
  protected
    FLayerSelector: TComboBox;
    FInspector: TJvInspector;
    FInspectorBorlandPainter: TJvInspectorBorlandPainter;
    FInspectorDotNETPainter: TJvInspectorDotNETPainter;
    FEditor: TImage32Editor;

    procedure InspectorAfterItemCreate(Sender: TObject; const Item: TJvCustomInspectorItem);
    procedure DoSelectionChanged(Sender: TObject);
    procedure DoLayerSelectorChanged(Sender: TObject);
    procedure RefreshLayerSelector;
    procedure AddObjectToInspector(const Parent: TJvCustomInspectorItem; const Ctrl: TControl);

    procedure SetEditor(const Value: TImage32Editor);
  public
    class function Execute(const aEditor: TImage32Editor): TGRLayerInspector;
    constructor Create(aComponent: TComponent);override;
    property CurrentLayer: TCustomLayer read GetCurrentLayer;
    property Editor: TImage32Editor read FEditor write SetEditor;
  end;

function GLayerInspector: TGRLayerInspector;

implementation

uses
  JclRTTI
  //, JVCLVer
  , JvInspExtraEditors
  ;

{$R *.dfm}

var
  FLayerInspector: TGRLayerInspector;

function GLayerInspector: TGRLayerInspector;
begin
  if not Assigned(FLayerInspector) then
    TGRLayerInspector.Create(nil);
  Result := FLayerInspector;
end;

{ TGRLayerInspector }

procedure TGRLayerInspector.SetEditor(const Value: TImage32Editor);
begin
  if FEditor <> Value then
  begin
    //if Assigned(FEditor) then
    FEditor := Value;
    if Assigned(FEditor) then
    begin
      FEditor.OnSelectionChanged := DoSelectionChanged;
    end;
    RefreshLayerSelector;
  end;
end;

class procedure TGRLayerInspector.Execute(const aEditor: TImage32Editor);
begin
  if not Assigned(FLayerInspector) then
    FLayerInspector := Create(nil);
  FLayerInspector.Editor := aEditor;
  FLayerInspector.Show;
end;

constructor TGRLayerInspector.Create(aComponent: TComponent);
begin
  Assert(FLayerInspector = nil);
  inherited;
  FLayerInspector := Self;

  FLayerSelector := TComboBox.Create(Self);
  FLayerSelector.Parent := Self;
  FLayerSelector.Align := alTop;
  FLayerSelector.Style := csDropDownList;
  FLayerSelector.OnChange := DoLayerSelectorChanged;

  FInspectorBorlandPainter:= TJvInspectorBorlandPainter.Create(Self);
  FInspectorDotNETPainter := TJvInspectorDotNETPainter.Create(Self);

  FInspector := TJvInspector.Create(Self);
  FInspector.Parent := Self;
  FInspector.Align := alClient;
  FInspector.Painter := FInspectorBorlandPainter;
  FInspector.AfterItemCreate  := InspectorAfterItemCreate;
end;

procedure TGRLayerInspector.AddObjectToInspector(const Parent: TJvCustomInspectorItem; const aObj: TObject);
var
  InspCat: TJvInspectorCustomCategoryItem;
  M: TNotifyEvent;
begin
  InspCat := TJvInspectorCustomCategoryItem.Create(Parent, nil);
  if aObj is TControl then
    InspCat.DisplayName := TControl(aObj).Name + ': '
  else if aObj is TCustomLayerEx then
    InspCat.DisplayName := TCustomLayerEx(aObj).Name + ': '
  else
    InspCat.DisplayName := '';

  InspCat.DisplayName := InspCat.DisplayName + aObj.ClassName;

  TJvInspectorPropData.New(InspCat, aObj);
end;

procedure TGRLayerInspector.DoLayerSelectorChanged(Sender: TObject);
//var
  //vInspCat: TJvInspectorCustomCategoryItem;
begin
  FInspector.SaveValues;
  FInspector.Clear;
  if (FLayerSelector.ItemIndex >= 0) and (FEditor.Selection <> FLayerSelector.Items.Objects[FLayerSelector.ItemIndex]) then
    FEditor.Selection := FLayerSelector.Items.Objects[FLayerSelector.ItemIndex];
  
  if Assigned(FEditor.Selection) then 
  begin
    //vInspCat := TJvInspectorCustomCategoryItem.Create(FInspector.Root, nil);
    //vInspCat.DisplayName := '';
    //vInspCat.SortKind := iskNone;
    AddObjectToInspector(FInspector.Root, FEditor.Selection);
  end;
end;

procedure TGRLayerInspector.DoSelectionChanged(Sender: TObject);
begin
  FLayerSelector.ItemIndex := FLayerSelector.Items.IndexOfObject(FEditor.Selection);
  DoLayerSelectorChanged(FLayerSelector);
end;

procedure TGRLayerInspector.InspectorAfterItemCreate(Sender: TObject; const Item: TJvCustomInspectorItem);
begin
  if Item is TJvInspectorBooleanItem then
    TJvInspectorBooleanItem(Item).ShowAsCheckbox := True;
end;

procedure TGRLayerInspector.RefreshLayerSelector;
var
  i: integer;
begin
  FLayerSelector.Items.Clear;
  if Assigned(FEditor) then with FEditor do
  begin
    for i  := 0 to Layers.Count - 1 do
    begin
      FLayerSelector.Items.AddObject(Layers[i].ClassName, Layers[i]);
    end;
    FLayerSelector.ItemIndex := FLayerSelector.Items.IndexOfObject(FEditor.Selection);
    DoLayerSelectorChanged(FLayerSelector);
  end;
end;

initialization
  FLayerInspector := nil;

  TJvInspectorAlignItem.RegisterAsDefaultItem;
  TJvInspectorAnchorsItem.RegisterAsDefaultItem;
  TJvInspectorColorItem.RegisterAsDefaultItem;
  TJvInspectorTImageIndexItem.RegisterAsDefaultItem;
finalization
  FreeAndNil(FLayerInspector);
end.
