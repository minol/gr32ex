
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
    FOldLayerListNotify: TLayerListNotifyEvent;
    FLayerSelector: TComboBox;
    FInspector: TJvInspector;
    FInspectorBorlandPainter: TJvInspectorBorlandPainter;
    FInspectorDotNETPainter: TJvInspectorDotNETPainter;
    FEditor: TImage32Editor;

    procedure InspectorAfterItemCreate(Sender: TObject; Item: TJvCustomInspectorItem);
    procedure DoSelectionChanged(Sender: TObject);
    procedure DoLayerSelectorChanged(Sender: TObject);
    procedure DoLayerListNotify(Sender: TLayerCollection; Action: TLayerListNotification;
      Layer: TCustomLayer; Index: Integer);
    procedure RefreshLayerSelector;
    procedure AddObjectToInspector(const Parent: TJvCustomInspectorItem; const aObj: TObject);

    procedure SetEditor(const Value: TImage32Editor);
  public
    class function Execute(const aEditor: TImage32Editor): TGRLayerInspector;
    constructor Create(aComponent: TComponent);override;
    destructor Destroy;override;
    //property CurrentLayer: TCustomLayer read GetCurrentLayer;
    property Editor: TImage32Editor read FEditor write SetEditor;
  end;

  TJvInspectorFloatPointItem = class(TJvInspectorCustomCompoundItem)
  private
    FX: TJvInspectorFloatItem;
    FY: TJvInspectorFloatItem;
  protected
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
  end;

function GLayerInspector: TGRLayerInspector;

implementation

uses
  JclRTTI
  //, JVCLVer
  , JvInspExtraEditors
  ;

{$R *.dfm}

type
  TLayerCollectionAccess = class(TLayerCollection);

var
  FLayerInspector: TGRLayerInspector;

function GLayerInspector: TGRLayerInspector;
begin
  if not Assigned(FLayerInspector) then
    FLayerInspector := TGRLayerInspector.Create(nil);
  Result := FLayerInspector;
end;

{ TGRLayerInspector }

class function TGRLayerInspector.Execute(const aEditor: TImage32Editor): TGRLayerInspector;
begin
  Result := GLayerInspector();
  with Result do
  begin
    Editor := aEditor;
    Show;
  end;
end;

constructor TGRLayerInspector.Create(aComponent: TComponent);
begin
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

destructor TGRLayerInspector.Destroy;
begin
  if FLayerInspector = Self then
    FLayerInspector := nil;
  inherited;
end;

procedure TGRLayerInspector.AddObjectToInspector(const Parent: TJvCustomInspectorItem; const aObj: TObject);
var
  InspCat: TJvInspectorCustomCategoryItem;
begin
  InspCat := TJvInspectorCustomCategoryItem.Create(Parent, nil);
  if aObj is TControl then
    InspCat.DisplayName := TControl(aObj).Name + ': '
  else if aObj is TGRCustomLayer then
    InspCat.DisplayName := TGRCustomLayer(aObj).Name + ': '
  else
    InspCat.DisplayName := '';

  InspCat.DisplayName := InspCat.DisplayName + aObj.ClassName;

  TJvInspectorPropData.New(InspCat, aObj);
end;

procedure TGRLayerInspector.DoLayerListNotify(Sender: TLayerCollection; Action: TLayerListNotification;
      Layer: TCustomLayer; Index: Integer);
var
  i: integer;
begin
  case Action of
    lnLayerDeleted:
      begin
        if FInspector.InspectObject = Layer then FInspector.Clear;
        i := FLayerSelector.Items.IndexOfObject(Layer);
        if i >= 0 then FLayerSelector.Items.Delete(i);
      end;
    lnLayerAdded, lnLayerInserted:
      FLayerSelector.Items.AddObject(Layer.ClassName, Layer);
    lnCleared:
      FInspector.Clear
  end;

  RefreshLayerSelector;
  if Assigned(FOldLayerListNotify) then
    FOldLayerListNotify(Sender, Action, Layer, Index);
end;

procedure TGRLayerInspector.DoLayerSelectorChanged(Sender: TObject);
//var
  //vInspCat: TJvInspectorCustomCategoryItem;
begin
  FInspector.SaveValues;
  FInspector.Clear;
  if (FLayerSelector.ItemIndex >= 0) and (FEditor.Selection <> FLayerSelector.Items.Objects[FLayerSelector.ItemIndex]) then
    FEditor.Selection := TGRTransformationLayer(FLayerSelector.Items.Objects[FLayerSelector.ItemIndex]);
  
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

procedure TGRLayerInspector.InspectorAfterItemCreate(Sender: TObject; Item: TJvCustomInspectorItem);
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

procedure TGRLayerInspector.SetEditor(const Value: TImage32Editor);
begin
  if FEditor <> Value then
  begin
    if Assigned(FEditor) then
    begin
      FEditor.OnSelectionChanged := nil;
      TLayerCollectionAccess(FEditor.Layers).OnListNotify := FOldLayerListNotify;
      FOldLayerListNotify := nil;
    end;
    FEditor := Value;
    if Assigned(FEditor) then
    begin
      FEditor.OnSelectionChanged := DoSelectionChanged;
      FOldLayerListNotify := TLayerCollectionAccess(FEditor.Layers).OnListNotify;
      TLayerCollectionAccess(FEditor.Layers).OnListNotify := DoLayerListNotify;
    end;
    RefreshLayerSelector;
  end;
end;

{ TJvInspectorFloatPointItem }
constructor TJvInspectorFloatPointItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  SingleNameUseFirstCol := True;
  FX := TJvInspectorFloatItem.Create(Self, AData);
  FY := TJvInspectorFloatItem.Create(Self, AData);
  AddColumnPrim(FX);
  AddColumnPrim(FY);
end;

initialization
  FLayerInspector := nil;

  with TJvCustomInspectorData.ItemRegister do
  begin
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorFloatPointItem, TypeInfo(TFloatPoint)));
  end;

  TJvInspectorAlignItem.RegisterAsDefaultItem;
  TJvInspectorAnchorsItem.RegisterAsDefaultItem;
  TJvInspectorColorItem.RegisterAsDefaultItem;
  TJvInspectorTImageIndexItem.RegisterAsDefaultItem;
finalization
  FreeAndNil(FLayerInspector);
end.
