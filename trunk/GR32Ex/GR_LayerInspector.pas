
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
  , GR_JvInspector
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
		procedure InspectorDataValueChanged(Sender: TObject;
			Data: TJvCustomInspectorData);
    procedure DoSelectionChanged(Sender: TObject);
    procedure DoLayerSelectorChanged(Sender: TObject);
    procedure DoLayerListNotify(Sender: TLayerCollection; Action: TLayerListNotification;
      Layer: TCustomLayer; Index: Integer);
    procedure RefreshLayerSelector;
    procedure AddObjectToInspector(const Parent: TJvCustomInspectorItem; const aObj: TObject);
    procedure AddEventsToInspector(const Parent: TJvCustomInspectorItem; const aObj: TObject);
    procedure AddSettingObjectToInspector;

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

  TGRInspectorEventItem = class(TJvCustomInspectorItem)
    class procedure RegisterAsDefaultItem;
  protected
    function GetDisplayValue: string; override;
    procedure GetValueList(const Strings: TStrings); override;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
    procedure Edit; override;
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;


function GLayerInspector: TGRLayerInspector;


resourcestring
  rsSkew = 'Skew';
  rsPivot = 'Pivot';
  rsScaling = 'Scaling';
  rsEvents = 'Events';
  rsSettings = 'Settings';
  rsScriptBehavior = 'ScriptBehavior';


implementation

uses
  JclRTTI
  //, JVCLVer
  , JvInspExtraEditors
  ;

{$R *.dfm}

const
  cNameClassSeperator = ': ';

type
  TLayerCollectionAccess = class(TLayerCollection);
  TGRTransformationLayerAccess = class(TGRTransformationLayer);

var
  FLayerInspector: TGRLayerInspector;

function GLayerInspector: TGRLayerInspector;
begin
  if not Assigned(FLayerInspector) then
    FLayerInspector := TGRLayerInspector.Create(nil);
  Result := FLayerInspector;
end;

procedure CreateInspFloatPointItem(const aName: string; const Parent: TJvCustomInspectorItem; const aPoint: TFloatPoint);
var
  vInspCompCat: TJvInspectorCustomCategoryItem;
  vCompItem: TJvInspectorCompoundItem;
begin
    vInspCompCat := TJvInspectorCustomCategoryItem.Create(Parent, nil);
    vInspCompCat.DisplayName := aName;
    vInspCompCat.Expanded := True;
    vCompItem := TJvInspectorCompoundItem.Create(vInspCompCat, nil);
    //vCompItem.SingleName := False;
    vCompItem.AddColumn(TJvInspectorVarData.New(vCompItem, 'X', TypeInfo(TFloat), @aPoint.X));
    vCompItem.AddColumn(TJvInspectorVarData.New(vCompItem, 'Y', TypeInfo(TFloat), @aPoint.Y));
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
  if not Assigned(FLayerInspector) then
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
  FInspector.OnDataValueChanged := InspectorDataValueChanged;
end;

destructor TGRLayerInspector.Destroy;
begin
  if FLayerInspector = Self then
    FLayerInspector := nil;
  inherited;
end;

procedure TGRLayerInspector.AddEventsToInspector(const Parent: TJvCustomInspectorItem; const aObj: TObject);
var
  vInspEventCat: TJvInspectorCustomCategoryItem;
  //vE: TNotifyEvent;
  vM: TMethod;
  vEventStrs, vBehaviors: TStringList;
  i, j: integer;
begin
  if aObj is TGRCustomLayer then
  begin
    vEventStrs := TStringList.Create;
    try
      TGRCustomLayer(aObj).GetRegisteredEvents(vEventStrs);
      if vEventStrs.Count > 0 then
      begin
        vInspEventCat := TJvInspectorCustomCategoryItem.Create(Parent, nil);
        vInspEventCat.DisplayName := rsEvents;
        for i := 0 to vEventStrs.Count - 1 do
        with TJvInspectorPropData.New(vInspEventCat, aObj, vEventStrs.Strings[i]) {as TJvInspectorTMethodItem} do
        begin
          {
          AddInstance(aObj, 'Self');
          vBehaviors := TStringList.Create;
          try
            TGRCustomLayer(aObj).GetRegisteredBehaviors(vBehaviors);
            for j := 0 to vBehaviors.Count - 1 do
            begin
              vM.Code := vBehaviors.Objects[j];
              vM.Data := aObj;
              AddMethod(vM, vBehaviors.Strings[j]);
            end;
          finally
            vBehaviors.Free;
          end;
          //}
        end;
        vInspEventCat.Expanded := True;
      end;
    finally
      vEventStrs.Free;
    end;
  end;
end;

procedure TGRLayerInspector.AddObjectToInspector(const Parent: TJvCustomInspectorItem; const aObj: TObject);
var
  vInspCat: TJvInspectorCustomCategoryItem;
begin
  vInspCat := TJvInspectorCustomCategoryItem.Create(Parent, nil);
  if aObj is TControl then
    vInspCat.DisplayName := TControl(aObj).Name + cNameClassSeperator
  else if aObj is TGRCustomLayer then
    vInspCat.DisplayName := TGRCustomLayer(aObj).Name + cNameClassSeperator
  else
    vInspCat.DisplayName := '';

  vInspCat.DisplayName := vInspCat.DisplayName + aObj.ClassName;

  TJvInspectorPropData.New(vInspCat, aObj);

  if aObj is TGRTransformationLayer then
  begin
    with TGRTransformationLayerAccess(aObj) do
    begin
      CreateInspFloatPointItem(rsSkew, vInspCat, FSkew);
      CreateInspFloatPointItem(rsScaling, vInspCat, FScaling);
      CreateInspFloatPointItem(rsPivot, vInspCat, FPivotPoint);
    end;
  end;

  if aObj is TGRCustomLayer then
  begin
    AddEventsToInspector(vInspCat, aObj);
  end;

end;

procedure TGRLayerInspector.AddSettingObjectToInspector;
var
  vInspCat: TJvInspectorCustomCategoryItem;
begin
  vInspCat := TJvInspectorCustomCategoryItem.Create(FInspector.Root, nil);
  vInspCat.DisplayName := rsSettings;
  TJvInspectorPropData.New(vInspCat, Editor, 'Bitmap');
  TJvInspectorPropData.New(vInspCat, Editor, 'BitmapAlign');
  TJvInspectorPropData.New(vInspCat, Editor, 'Scale');
  TJvInspectorPropData.New(vInspCat, Editor, 'ScaleMode');
  TJvInspectorPropData.New(vInspCat, Editor, 'ShowHint');
  //TJvInspectorPropData.New(vInspCat, Editor);
  vInspCat.Expanded := True;
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

procedure TGRLayerInspector.InspectorDataValueChanged(
  Sender: TObject; Data: TJvCustomInspectorData);
var
  i: integer;
begin
  if SameText(data.Name , 'Name') then
    with FLayerSelector do
    begin
      i := ItemIndex;
      Items[i] := data.AsString + cNameClassSeperator + TObject(Items.Objects[i]).ClassName;
      Refresh;
      ItemIndex := i;
    end
  else if (data.Name = 'X') or (data.Name = 'Y') then
    with FLayerSelector do
    begin
      i := ItemIndex;
      with TGRTransformationLayerAccess(Items.Objects[i]) do
      begin
        Changed;
        DoChange;
      end;
    end;
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
      if (Layers[i] is TGRCustomLayer) and not (Layers[i] is TGRRubberBandLayer) then
      begin
        FLayerSelector.Items.AddObject(TGRCustomLayer(Layers[i]).Name+cNameClassSeperator+Layers[i].ClassName, Layers[i]);
      end;
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

{ TGRInspectorEventItem }
class procedure TGRInspectorEventItem.RegisterAsDefaultItem;
begin
  TJvCustomInspectorData.ItemRegister.Add(
    TJvInspectorTypeInfoRegItem.Create(
      TGRInspectorEventItem, TypeInfo(TGRNotifyEventStr)));
end;

function TGRInspectorEventItem.GetDisplayValue: string;
begin
  Result := Data.AsString;
  if (Length(Result) >= 2) and (Result[1] = '<') then
    Result := rsScriptBehavior;
end;

procedure TGRInspectorEventItem.GetValueList(const Strings: TStrings);
var
  vInstance: TSDPlayingLayer;
begin
  if (Data is TJvInspectorPropData) then
  begin
    vInstance := TSDPlayingLayer(TJvInspectorPropData(Data).Instance);
    if vInstance is TSDPlayingLayer then
    begin
      vInstance.GetRegisteredBehaviors(Strings);
    end;
  end;
end;

procedure TGRInspectorEventItem.SetDisplayValue(const Value: string);
begin
  if Value = rsScriptBehavior then 
  begin
    if Data.AsString = '' then 
    begin
      Data.AsString := '<>';
      Edit;
    end;
  end
  else
    Data.AsString := Value;
end;

procedure TGRInspectorEventItem.SetFlags(const Value: TInspectorItemFlags);
var
  NewValue: TInspectorItemFlags;
begin
  NewValue := Value + [iifValueList, iifAutoUpdate];
  inherited SetFlags(NewValue);
end;

procedure TGRInspectorEventItem.Edit;
var
  s: string;
begin
  s := Data.AsString;
  if (Length(s) >= 2) and (s[1] = '<')  then
  begin
    System.Delete(s,1, 1);
    System.Delete(s, Length(s), 1);
    s := Trim(TdlgScriptEdit.Execute(s));
    s := '<' + s + '>';
    Data.AsString := s;
  end;
end;

procedure TGRInspectorEventItem.EditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (ssDouble in Shift) then
    Edit;
end;

initialization
  FLayerInspector := nil;
  TGRInspectorEventItem.RegisterAsDefaultItem;


  {with TJvCustomInspectorData.ItemRegister do
  begin
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorFloatPointItem, TypeInfo(TFloatPoint)));
  end; //}

finalization
  FreeAndNil(FLayerInspector);
end.
