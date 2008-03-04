
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

resourcestring
  rsNoLayerEditorError = 'no editor error';

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
    edtName: TLabeledEdit;
    procedure btnOKClick(Sender: TObject);
    procedure btnOpenPicClick(Sender: TObject);
    procedure NumberKeyPressOnly(Sender: TObject; var Key: Char);
  protected
    FLayer: TGRLayer;
    FImage: TImage32;
    procedure SetLayer(const Value: TGRLayer); virtual;
    { Private declarations }
  public
    { Public declarations }
    class function LayerClass: TGRLayerClass; virtual;
    class function Execute(aLayer: TGRLayer): Boolean;
    constructor Create(aComponent: TComponent);override;
    property Layer: TGRLayer read FLayer write SetLayer;
  end;

procedure RegisterLayerEditor(const aEditor: TGRLayerEditorClass);
function GetLayerEditorClass(const aLayer: TGRLayer): TGRLayerEditorClass;
function ShowLayerEditor(const aLayer: TGRLayer): Boolean;

implementation

{$R *.dfm}

var
  FLayerEditorClasses: TList;

function GetLayerEditorClass(const aLayer: TGRLayer): TGRLayerEditorClass;
var
 i: integer;
begin
  with FLayerEditorClasses do
    for i := Count -1 downto 0 do
    begin
      Result := TGRLayerEditorClass(Items[i]);
      if aLayer.InheritsFrom(Result.LayerClass) then
        exit;
    end;
  Result := nil;
end;

procedure RegisterLayerEditor(const aEditor: TGRLayerEditorClass);
begin
  with FLayerEditorClasses do
    if IndexOf(aEditor) < 0 then
      Add(aEditor);
end;

function ShowLayerEditor(const aLayer: TGRLayer): Boolean;
var
  vEditorClass: TGRLayerEditorClass;
begin
  vEditorClass := GetLayerEditorClass(aLayer);
  Result := Assigned(vEditorClass);
  if Result then
  begin
    vEditorClass.execute(aLayer);
  end
  else
    showmessage(rsNoLayerEditorError);
end;

{ TGRLayerEditor }

procedure TGRLayerEditor.SetLayer(const Value: TGRLayer);
begin
  FLayer := Value;
  FImage.Bitmap.Assign(FLayer.Bitmap);
  edtName.Text := FLayer.Name;
  edtLeft.Text := IntToStr(FLayer.Left);
  edtTop.Text := IntToStr(FLayer.Top);
end;

procedure TGRLayerEditor.btnOKClick(Sender: TObject);
begin
 //save the properties to layer control.
  FLayer.Bitmap.Assign(FImage.Bitmap);
  FLayer.Name := edtName.Text;
  FLayer.Left := StrToInt(edtLeft.Text);
  FLayer.Top := StrToInt(edtTop.Text);
end;

class function TGRLayerEditor.Execute(
  aLayer: TGRLayer): Boolean;
begin
  with Create(nil) do
  try
    Layer := aLayer;
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

class function TGRLayerEditor.LayerClass: TGRLayerClass;
begin
  Result := TGRLayer;
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
  FLayerEditorClasses := TList.Create;
  //RegisterLayerEditor(TGRLayerEditor);
finalization
  FreeAndNil(FLayerEditorClasses);
end.
