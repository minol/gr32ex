unit GR_URLPackedFile;

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
 * The Original Code is GR_URLPackedFile
 *
 * The Initial Developer of the Original Code is
 * Riceball LEE
 *
 * Portions created by Riceball LEE are Copyright (C) 2007-2008
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I Setting.inc}

uses
  SysUtils, Classes
  , GR_URL
  ;

type

  TGRPackedFileConnection = class(TGRResourceConnection)
  protected
    function UpdateURL(const Value: string): Boolean; override;
    function iGetHeader(const aInfo: TStrings): TGRResourceResult; override;
    function iGet(const aStream: TStream): TGRResourceResult; override;
    function iPut(const aStream: TStream): TGRResourceResult; override;
    function iDelete(): TGRResourceResult; override;
  public
    class function Supports: TGRResourceSupports; override;
    //list this connection supports protocols, seperate by ";"
    //eg, http;https
    class function Protocols: string; override;
  end;

implementation

uses
  uMeStrUtils;

{ TGRPackedFileConnection }
class function TGRResourceConnection.Supports: TGRResourceSupports;
begin
  Result := [rsfGet, rsfPut, rsfDelete, rsfHead];
end;

class function TGRResourceConnection.Protocols: string;
begin
  Result := 'pack';
end;

function TGRResourceConnection.UpdateURL(const Value: string): Boolean;
begin
end;

function TGRResourceConnection.iGetHeader(const aInfo: TStrings): TGRResourceResult;
begin
end;

function TGRResourceConnection.iGet(const aStream: TStream): TGRResourceResult;
begin
end;

function TGRResourceConnection.iPut(const aStream: TStream): TGRResourceResult;
begin
end;

function TGRResourceConnection.iDelete(): TGRResourceResult;
begin
end;

initialization
  TGRPackedFileConnection.Register;
finalization
end.
