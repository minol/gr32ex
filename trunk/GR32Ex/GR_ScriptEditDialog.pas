
{ you need the SynEdit to compile.}
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
 * The Original Code is GR_ScriptEditDialog
 *
 * The Initial Developer of the Original Code is Riceball LEE
 * Portions created by Riceball LEE are Copyright (C) 2008
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_ScriptEditDialog;

{$DEFINE SynEdit_Supports}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls,
  Dialogs
  {$IFDEF SynEdit_Supports}
  , SynEdit, SynEditTypes, SynHighlighterPas,
  SynEditRegexSearch, SynEditSearch, SynEditMiscClasses, SynEditHighlighter, SynHighlighterDfm
  {$ENDIF}
  ;

type
  TdlgScriptEdit = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
  private
    function GetScript: string;
    procedure SetScript(const Value: string);
  protected
    FScriptEdit: {$IFDEF SynEdit_Supports}TSynEdit{$ELSE}TMemo{$ENDIF};
  public
    { Public declarations }
    class function Execute(const aScript: string): string;
    constructor Create(aOwner: TComponent);override;
    property Script: string read GetScript write SetScript;
  end;


implementation

{$R *.dfm}

class function TdlgScriptEdit.Execute(const aScript: string): string;
begin
  With Create(nil) do
  try
    Script := aScript;
    if ShowModal = mrOk then
      Result := Script
    else
      Result := aScript;
  finally
    Free;
  end;
end;

procedure TdlgScriptEdit.FormShow(Sender: TObject);
begin
  //
end;

constructor TdlgScriptEdit.Create(aOwner: TComponent);
begin
  inherited;
  FScriptEdit := {$IFDEF SynEdit_Supports}TSynEdit{$ELSE}TMemo{$ENDIF}.Create(Self);
  with FScriptEdit do
  begin
    Parent := Self;
    Left := 1;
    Top := 1;
    Width := Self.ClientWidth - 2;
    Height := btnOk.Top - 2;
    Anchors := [akTop, akLeft, akRight, akBottom];
    TabOrder := 0;
  end;
  btnOK.Anchors := [akLeft, akBottom];
  btnCancel.Anchors := [akLeft, akBottom];
end;

function TdlgScriptEdit.GetScript: string;
begin
  Result := FScriptEdit.Lines.Text;
end;

procedure TdlgScriptEdit.SetScript(const Value: string);
begin
  FScriptEdit.Lines.Text := Value;
end;

end.
