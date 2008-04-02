unit GR_URL;

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
 * The Original Code is GR_URL
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
  ;

type
  {$Z4}
  TGRResourceResult = (rrOk, rrNoSuchProtocol, rrNoSupports, rrNoSuchResource, rrNoPermission, rrUnkownError);
  {$Z1}
  TGRResourceSupport = (rsfGet, rsfPut, rsfDelete, rsfHead);
  TGRResourceSupports = set of TGRResourceSupport;

  TGRRequestDone = procedure (const Sender : TObject;
                              const RqType : TGRResourceSupport;
                              const Error  : TGRResourceResult) of object;

  TGRResourceConnection = class;

  TGRResourceConnectionClass = class of TGRResourceConnection;

  TGRResourceLocator = class
  protected
  public
  end;

  TGRResourceConnection = class
  protected
    //the current protocol.
    FProtocol: string;
    FURL: string;

    procedure SetURL(const aURL: string);
    class function IndexOfConnectionClass(const aURL: string): Integer;

    function UpdateURL(Var Value: string): Boolean; virtual;
    function iGetHeader(const aInfo: TStrings): TGRResourceResult; virtual; abstract;
    function iGet(const aStream: TStream): TGRResourceResult; virtual; abstract;
    function iPut(const aStream: TStream): TGRResourceResult; virtual; abstract;
    function iDelete(): TGRResourceResult; virtual; abstract;
  public
    class function CreateConnection(const aURL: string): TGRResourceConnection;
    class function Supports: TGRResourceSupports; virtual; abstract;
    //list this connection supports protocols, seperate by ";"
    //eg, http;https
    class function Protocols: string; virtual; abstract;
    class procedure Register;
    //test whether this Protocol can be processed.
    class function CanProcessed(const aProtocol: string): Boolean;
    constructor Create(const aURL: string = ''); virtual;

    {
      Return the ResourceInfo in aInfo: AttributeName=value
        Such as:
        Size=XX
        Date=XX
        Revision=
    }
    function GetResourceHeader(const aInfo: TStrings): TGRResourceResult; overload;
    function GetResource(const aStream: TStream): TGRResourceResult; overload;
    function PutResource(const aStream: TStream): TGRResourceResult; overload;
    function DeleteResource(): TGRResourceResult; overload;

    class function GetResourceHeader(const aURL: string; const aInfo: TStrings): TGRResourceResult; overload;
    class function GetResource(const aURL: string; const aStream: TStream): TGRResourceResult; overload;
    class function PutResource(const aURL: string; const aStream: TStream): TGRResourceResult; overload;
    class function DeleteResource(const aURL: string): TGRResourceResult; overload;

    property URL: string read FURL write SetURL;
    property Protocol: string read FProtocol;
  end;

  { Summary: the abstract file resource. }
  {
    the file resource MUST like this: "Protocol://[user:passwd@][/folder/../]filename:/folder/../rsource.txt"
    pack://user:pwd@/test/aFile.pak:/folder/test.txt
  }
  TGRCustomFileConnection = class(TGRResourceConnection)
  protected
    FFileName: string;
    FUserName: string;
    FPassword: string;

    function UpdateURL(Var Value: string): Boolean; override;
  end;

{ the registered resource connections classes }
function GResourceConnectionClasses: TList;

implementation

uses
  uMeStrUtils;

var
  FConnectionClasses: TList;

function GResourceConnectionClasses: TList;
begin
  if not Assigned(FConnectionClasses) then
    FConnectionClasses := TList.Create;
  Result := FConnectionClasses;
end;

{ TGRResourceLocator }

{ TGRResourceConnection }
class function TGRResourceConnection.CreateConnection(const aURL: string): TGRResourceConnection;
var
  i: Integer;
begin
  i := IndexOfConnectionClass(aURL);
  if i >= 0 then
  begin
    Result := TGRResourceConnectionClass(GResourceConnectionClasses.Items[i]).Create(aURL);
  end
  else
    Result := nil;
end;

class function TGRResourceConnection.CanProcessed(const aProtocol: string): Boolean;
var
  s: string;
begin
  s := Protocols;
  Result := False;
  while (s <> '') and not Result do
  begin
    Result := AnsiCompareText(StrFetch(s, ';', True), aProtocol) = 0;
  end;
end;

class function TGRResourceConnection.IndexOfConnectionClass(const aURL: string): Integer;
var
  vProtocol: string;
begin
  Result := AnsiPos(';', aURL);
  if Result > 0 then
  begin
    vProtocol := Copy(aURL, 1, Result - 1);
    with GResourceConnectionClasses do
      for Result := 0 to Count -1 do
        if AnsiCompareText(vProtocol, TGRResourceConnectionClass(Items[Result]).Protocols) = 0 then
          exit;
  end;
  Result := -1;
end;

class procedure TGRResourceConnection.Register;
begin
  with GResourceConnectionClasses do
    if IndexOf(ClassType) < 0 then
      Add(ClassType);
end;

constructor TGRResourceConnection.Create(const aURL: string = '');
begin
  inherited Create;
  URL := aURL;
end;

procedure TGRResourceConnection.SetURL(const aURL: string);
var
  s: string;
begin
  if FURL <> aURL then
  begin
    s := aURL;
    if UpdateURL(s) then
      FURL := aURL;
  end;
end;

function TGRResourceConnection.GetResourceHeader(const aInfo: TStrings): TGRResourceResult;
begin
  if rsfHead in Supports then
    Result := iGetHeader(aInfo)
  else
    Result := rrNoSupports;
end;

function TGRResourceConnection.GetResource(const aStream: TStream): TGRResourceResult;
begin
  if rsfGet in Supports then
    Result := iGet(aStream)
  else
    Result := rrNoSupports;
end;

function TGRResourceConnection.PutResource(const aStream: TStream): TGRResourceResult;
begin
  if rsfPut in Supports then
    Result := iPut(aStream)
  else
    Result := rrNoSupports;
end;

function TGRResourceConnection.DeleteResource(): TGRResourceResult;
begin
  if rsfDelete in Supports then
    Result := iDelete()
  else
    Result := rrNoSupports;
end;

class function TGRResourceConnection.GetResourceHeader(const aURL: string; const aInfo: TStrings): TGRResourceResult;
var
  vConn: TGRResourceConnection;
begin
  vConn := CreateConnection(aURL);
  if Assigned(vConn) then
  try
    Result := vConn.GetResourceHeader(aInfo);
  finally
    vConn.Free;
  end
  else
    Result := rrNoSuchProtocol;  
end;

class function TGRResourceConnection.GetResource(const aURL: string; const aStream: TStream): TGRResourceResult;
var
  vConn: TGRResourceConnection;
begin
  vConn := CreateConnection(aURL);
  if Assigned(vConn) then
  try
    Result := vConn.GetResource(aStream);
  finally
    vConn.Free;
  end
  else
    Result := rrNoSuchProtocol;  
end;

class function TGRResourceConnection.PutResource(const aURL: string; const aStream: TStream): TGRResourceResult;
var
  vConn: TGRResourceConnection;
begin
  vConn := CreateConnection(aURL);
  if Assigned(vConn) then
  try
    Result := vConn.PutResource(aStream);
  finally
    vConn.Free;
  end
  else
    Result := rrNoSuchProtocol;  
end;

class function TGRResourceConnection.DeleteResource(const aURL: string): TGRResourceResult;
var
  vConn: TGRResourceConnection;
begin
  vConn := CreateConnection(aURL);
  if Assigned(vConn) then
  try
    Result := vConn.DeleteResource();
  finally
    vConn.Free;
  end
  else
    Result := rrNoSuchProtocol;  
end;

function TGRResourceConnection.UpdateURL(Var Value: string): Boolean;
begin
  FProtocol := StrFetch(Value, ':', True);
  Result := (FProtocol <> '') and CanProcessed(FProtocol);
  //if not Result then FProtocol := '';
end;

{ TGRCustomFileConnection }
function TGRCustomFileConnection.UpdateURL(Var Value: string): Boolean;
Var
  s: string;
begin
  Result := inherited UpdateURL(Value);
  if Result then
  begin
    s := StrRFetch(Value, ':', True);
    Result := s <> '';
    if Result then
    begin
      FFileName := s;
      s := StrFetch(FFileName, '@', True);
      FUserName := s;
      FPassword := StrFetch(FUserName, ':', True);
    end
    else
      Result := False;
  end;
end;

initialization
finalization
  FreeAndNil(FConnectionClasses);
end.
