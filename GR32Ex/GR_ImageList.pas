
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
 * The Original Code is GR_ImageList
 *
 * The Initial Developer of the Original Code is Riceball LEE
 * Portions created by Riceball LEE are Copyright (C) 2004-2008
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_ImageList;

interface

{$I Setting.inc}

uses
{$IFDEF CLX}
  Qt, Types, QControls, QGraphics, QConsts,
  {$IFDEF LINUX}Libc,{$ENDIF}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
{$ELSE}
  Windows, Messages, Controls, Graphics,
{$ENDIF}
  Classes, SysUtils
  , SyncObjs
  //, GR32
  , GR_System
  ;

const
  opChanged = 2;

type
  TGRPictureItemClass = class of TGRPictureItem;
  { Summary A Picture container designed to be inserted into TGRPictureCollection }
  {
    Usage:
      with PictureItem.Picture do
      try
        ....
      fianlly
        //delete the picture from memory if no Cached Setting.
        PictureItem.UnUse;
      end;
  }
  TGRPictureItem = class(TGRBufferItem)
  protected
    FPicture: TPicture;
    procedure SetPicture(const Value: TPicture);
  protected
    function GetPicture: TPicture;
    procedure LoadBuffer; override;
    procedure ReleaseBuffer; override;
    function DoURLChanged(const aURL: string): Boolean;override;
    procedure PictureChanged(Sender: TObject);
  public
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

  published
    property Picture: TPicture read GetPicture write SetPicture;
    property Name;
    property URL;
    property Cached;
  end;
  
  { Summary A collection of TGRGraphicItem objects }
  TGRPictureCollection = class(TCollection)
  protected
    FOwner: TPersistent;
    function GetItem(Index: Integer): TGRPictureItem;
    procedure SetItem(Index: Integer; Value: TGRPictureItem);
  protected
    function GetOwner: TPersistent; override;
    procedure PictureChanged(const Sender: TGRPictureItem);
  public
    constructor Create(AOwner: TPersistent; ItemClass: TGRPictureItemClass);
    function Add: TGRPictureItem;
    function Find(const aName: string): TGRPictureItem;
    property Items[Index: Integer]: TGRPictureItem read GetItem write SetItem;
      default;
  end;
  
  TGRPictureChangedEvent = procedure (const Sender: TGRPictureItem) of object;
  { Summary A component that stores TGRGraphicCollection }
  TGRPictureList = class(TComponent)
  protected
    FNotifyList: TList;
    FOnPictureChanged: TGRPictureChangedEvent;
    FPictureCollection: TGRPictureCollection;
    function GetPicture(const Index: Integer): TGRPictureItem;
    procedure SetPicture(const Index: Integer; const Value: TGRPictureItem);
    procedure SetPictures(const Value: TGRPictureCollection);

    procedure PictureChanged(const Sender: TGRPictureItem);
    procedure ClearNotifyList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ChangeNotification(const aComponent: TComponent);
    procedure RemoveChangeNotification(const aComponent: TComponent);

    property Picture[const Index: Integer]: TGRPictureItem read GetPicture write SetPicture; default;
    //property PictureByName[const Name: string]: TGRPictureItem read Find;
    property OnPictureChanged: TGRPictureChangedEvent read FOnPictureChanged write FOnPictureChanged;
  published
    property Pictures: TGRPictureCollection read FPictureCollection write SetPictures;
  end;
  
implementation

//uses Math, TypInfo, GR32_System;

type
  TComponentAccess = class(TComponent);
  //TBitmap32Access = class(TBitmap32);

{ TGRPictureItem }
constructor TGRPictureItem.Create(aCollection: TCollection);
begin
  inherited;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
end;

destructor TGRPictureItem.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TGRPictureItem.Assign(Source: TPersistent);
begin
  if Source is TGRPictureItem then
  begin
    Picture := TGRPictureItem(Source).Picture;
  end
  else
    inherited;
end;

function TGRPictureItem.DoURLChanged(const aURL: string): Boolean;
begin
  //TODO
  Result := inherited DoURLChanged(aURL);
end;

procedure TGRPictureItem.LoadBuffer;
begin
end;

procedure TGRPictureItem.PictureChanged(Sender: TObject);
begin
  if Collection is TGRPictureCollection then
    TGRPictureCollection(Collection).PictureChanged(Self);
end;

procedure TGRPictureItem.ReleaseBuffer; 
begin
end;

function TGRPictureItem.GetPicture: TPicture;
begin
  Use;
  Result := FPicture;
end;

procedure TGRPictureItem.SetPicture(const Value: TPicture);
begin
  if FPicture <> Value then
    FPicture.Assign(Value);
end;

{ TGRPictureCollection }
constructor TGRPictureCollection.Create(AOwner: TPersistent; ItemClass:
  TGRPictureItemClass);
begin
  inherited Create(ItemClass);
  FOwner := AOwner;
end;

function TGRPictureCollection.Add: TGRPictureItem;
begin
  Result := TGRPictureItem(inherited Add);
end;

function TGRPictureCollection.Find(const aName: string): TGRPictureItem;
var
  I: Integer;
begin
  Result := nil;
  
  For i := 0 to Count -1 do
  begin
    if Items[i].Name = aName then
    begin
      Result := Items[i];
      Exit;
    end;
  end;
end;

function TGRPictureCollection.GetItem(Index: Integer): TGRPictureItem;
begin
  Result := TGRPictureItem(inherited GetItem(Index));
end;

function TGRPictureCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TGRPictureCollection.PictureChanged(const Sender: TGRPictureItem);
begin
  if FOwner is TGRPictureList then
    TGRPictureList(FOwner).PictureChanged(Sender);
end;

procedure TGRPictureCollection.SetItem(Index: Integer; Value: TGRPictureItem);
begin
  inherited SetItem(Index, Value);
end;

{ TGRPictureList }
constructor TGRPictureList.Create(AOwner: TComponent);
begin
  inherited;
  FPictureCollection := TGRPictureCollection.Create(Self, TGRPictureItem);
  FNotifyList := TList.Create;
end;

destructor TGRPictureList.Destroy;
begin
  ClearNotifyList;
  FNotifyList.Free;
  FPictureCollection.Free;
  inherited;
end;

procedure TGRPictureList.ClearNotifyList;
var
  i: Integer;
  vItem: TComponent;
begin
  for i := 0 to FNotifyList.Count - 1 do
  begin
    vItem := FNotifyList.Items[i];
    if vItem is TComponent then
      vItem.RemoveFreeNotification(Self);
  end;
  FNotifyList.Clear;
end;

procedure TGRPictureList.ChangeNotification(const aComponent: TComponent);
begin
  if Assigned(aComponent) and (FNotifyList.IndexOf(aComponent) < 0) then
    FNotifyList.Add(aComponent);
end;

procedure TGRPictureList.RemoveChangeNotification(const aComponent: TComponent);
begin
  FNotifyList.Remove(aComponent);
end;

function TGRPictureList.GetPicture(const Index: Integer): TGRPictureItem;
begin
  Result := FPictureCollection.Items[Index];
end;

procedure TGRPictureList.PictureChanged(const Sender: TGRPictureItem);
var
  i: Integer;
begin
  for i := 0 to FNotifyList.Count - 1 do
  begin
    TComponentAccess(FNotifyList.Items[i]).Notification(TComponent(Sender), TOperation(opChanged));
  end;  
end;

procedure TGRPictureList.SetPicture(const Index: Integer; const Value: TGRPictureItem);
begin
  FPictureCollection.Items[Index] := Value;
end;

procedure TGRPictureList.SetPictures(const Value: TGRPictureCollection);
begin
  if FPictureCollection <> Value then
    FPictureCollection.Assign(Value);
end;

end.
