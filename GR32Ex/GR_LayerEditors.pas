
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
 * The Original Code is GR_LayerEditors
 *
 * The Initial Developer of the Original Code is Riceball LEE
 * Portions created by Riceball LEE are Copyright (C) 2008
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_LayerEditors;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Dialogs, ExtDlgs
  , GR32
  , GR32_Image
  , GR_Layers
  ;

type
  TGRLayerEditorClass = class of TGRLayerEditor;
  TGRLayerEditor = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    shpLine: TBevel;
    dlgOpenPicture: TOpenPictureDialog;
    btnOpenPic: TSpeedButton;
    edtLeft: TLabeledEdit;
    edtTop: TLabeledEdit;
    procedure btnOKClick(Sender: TObject);
    procedure btnOpenPicClick(Sender: TObject);
    procedure NumberKeyPressOnly(Sender: TObject; var Key: Char);
  protected
    FLayerControl: TGRLayerControl;
    FImage: TImage32;
    procedure SetLayerControl(const Value: TGRLayerControl); virtual;
    { Private declarations }
  public
    { Public declarations }
    class function LayerControlClass: TGRLayerControlClass; virtual;
    class function Execute(aLayerControl: TGRLayerControl): Boolean;
    constructor Create(aComponent: TComponent);override;
    property LayerControl: TGRLayerControl read FLayerControl write SetLayerControl;
  end;

procedure RegisterLayerControlEditor(const aEditor: TGRLayerEditorClass);
function GetLayerControlEditorClass(const aLayer: TGRLayerControl): TGRLayerEditorClass;

implementation

{$R *.dfm}

var
  FLayerControlEditorClasses: TList;

function GetLayerControlEditorClass(const aLayer: TGRLayerControl): TGRLayerEditorClass;
var
 i: integer;
begin
  with FLayerControlEditorClasses do
    for i := Count -1 downto 0 do
    begin
      Result := TGRLayerEditorClass(Items[i]);
      if aLayer.InheritsFrom(Result.LayerControlClass) then
        exit;
    end;
  Result := nil;
end;

procedure RegisterLayerControlEditor(const aEditor: TGRLayerEditorClass);
begin
  with FLayerControlEditorClasses do
    if IndexOf(aEditor) < 0 then
      Add(aEditor);
end;

{ TGRLayerEditor }

procedure TGRLayerEditor.SetLayerControl(const Value: TGRLayerControl);
begin
  FLayerControl := Value;
  FImage.Bitmap.Assign(FLayerControl.Bitmap);
  edtLeft.Text := IntToStr(FLayerControl.Left);
  edtTop.Text := IntToStr(FLayerControl.Top);
end;

procedure TGRLayerEditor.btnOKClick(Sender: TObject);
begin
 //save the properties to layer control.
  FLayerControl.Bitmap.Assign(FImage.Bitmap);
  FLayerControl.Left := StrToInt(edtLeft.Text);
  FLayerControl.Top := StrToInt(edtTop.Text);
end;

class function TGRLayerEditor.Execute(
  aLayerControl: TGRLayerControl): Boolean;
begin
  with Create(nil) do
  try
    LayerControl := aLayerControl;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

procedure TGRLayerEditor.btnOpenPicClick(Sender: TObject);
begin
  with dlgOpenPicture do
    if Execute then
    begin
      FImage.Bitmap.LoadFromFile(FileName);
    end;
end;

class function TGRLayerEditor.LayerControlClass: TGRLayerControlClass;
begin
  Result := TGRLayerControl;
end;

procedure TGRLayerEditor.NumberKeyPressOnly(Sender: TObject; var Key: Char);
begin
  if not (Key in ['0'..'9']) then key := #0;
end;

constructor TGRLayerEditor.Create(aComponent: TComponent);
begin
  inherited;
  FImage := TImage32.Create(Self);
  FImage.Parent := Self;
  FImage.Width := edtLeft.Left - 5;
  FImage.Height := shpLine.Top - 5;
end;

initialization
  FLayerControlEditorClasses := TList.Create;
  //RegisterLayerControlEditor(TGRLayerEditor);
finalization
  FreeAndNil(FLayerControlEditorClasses);
end.
