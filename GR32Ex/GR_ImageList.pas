
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
  , GR32
  , GR_System
  ;

const
  opChanged = 2;

type
  TGRPictureIndex = type Integer;
  TGRCustomPictureItem = class;
  TGRPictureChangedEvent = procedure (const Sender: TGRCustomPictureItem) of object;

  TGRPictureItemClass = class of TGRCustomPictureItem;
  TGRPictureCollectionClass = class of TGRCustomPictureCollection;

  { Summary A Picture container designed to be inserted into TGRCustomPictureCollection }
  {
    Usage:
      with PictureItem.GetPicture do
      try
        ....
      fianlly
        //delete the picture from memory if no Cached Setting.
        PictureItem.UnUse;
      end;
  }
  TGRCustomPictureItem = class(TGRBufferItem)
  protected
    FIsPictureStored: Boolean;
  protected
    procedure LoadBuffer; override;
    procedure ReleaseBuffer; override;
    function DoURLChanged(const aURL: string): Boolean;override;
    procedure PictureChanged(Sender: TObject);
    function GetIsPictureStored: Boolean;
  public
    procedure Assign(Source: TPersistent); override;

  published
    {  Summary: whether need store the Picture to the local DFM stream when URL is not empty. }
    property IsPictureStored: Boolean read FIsPictureStored write FIsPictureStored;
    property Name;
    { Summary the URL address of the Picture. }
    property URL;
    property Cached;
  end;
  
  TGRPictureItem = class(TGRCustomPictureItem)
  protected
    FPicture: TPicture;
    procedure SetPicture(const Value: TPicture);
  public
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetPicture: TPicture;
  published
    property Picture: TPicture read FPicture write SetPicture stored GetIsPictureStored;
  end;

  TGRBitmapItem = class(TGRCustomPictureItem)
  protected
    FPicture: TBitmap32;
    procedure SetPicture(const Value: TBitmap32);
  public
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetPicture: TBitmap32;
  published
    property Picture: TBitmap32 read FPicture write SetPicture stored GetIsPictureStored;
  end;

  { Summary: A collection of TGRPictureItem objects }
  TGRCustomPictureCollection = class(TCollection)
  protected
    FOwner: TPersistent;
    FOnPictureChanged: TGRPictureChangedEvent;
    function GetItem(Index: Integer): TGRCustomPictureItem;
    procedure SetItem(Index: Integer; Value: TGRCustomPictureItem);
  protected
    function GetOwner: TPersistent; override;
    procedure PictureChanged(const Sender: TGRCustomPictureItem);

    property OnPictureChanged: TGRPictureChangedEvent read FOnPictureChanged write FOnPictureChanged;
  public
    constructor Create(AOwner: TPersistent);virtual;
    function Add: TGRCustomPictureItem;
    function Find(const aName: string): TGRCustomPictureItem;
    class function ItemClass: TGRPictureItemClass; virtual;abstract;

    property Items[Index: Integer]: TGRCustomPictureItem read GetItem write SetItem; default;
  end;
  
  TGRPictureCollection = class(TGRCustomPictureCollection)
  protected
  public
    class function ItemClass: TGRPictureItemClass; override;
  end;

  { Summary: A collection of TGRBitmapItem objects }
  TGRBitmapCollection = class(TGRCustomPictureCollection)
  public
    class function ItemClass: TGRPictureItemClass; override;
  end;
  
  { Summary A component that stores the TGRGraphicCollection }
  TGRCustomPictureList = class(TComponent)
  protected
    FNotifyList: TList;
    FOnPictureChanged: TGRPictureChangedEvent;
    FPictureCollection: TGRCustomPictureCollection;
    function GetPicture(const Index: Integer): TGRCustomPictureItem;
    procedure SetPicture(const Index: Integer; const Value: TGRCustomPictureItem);
    procedure SetPictures(const Value: TGRCustomPictureCollection);

    procedure PictureChanged(const Sender: TGRCustomPictureItem);
    procedure ClearNotifyList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ChangeNotification(const aComponent: TComponent);
    procedure RemoveChangeNotification(const aComponent: TComponent);
    class function CollectionClass: TGRPictureCollectionClass; virtual;abstract;

    property Picture[const Index: Integer]: TGRCustomPictureItem read GetPicture write SetPicture; default;
    //property PictureByName[const Name: string]: TGRCustomPictureItem read Find;
    property OnPictureChanged: TGRPictureChangedEvent read FOnPictureChanged write FOnPictureChanged;
  published
    property Pictures: TGRCustomPictureCollection read FPictureCollection write SetPictures;
  end;

  TGRPictureList = class(TGRCustomPictureList)
  public
    class function CollectionClass: TGRPictureCollectionClass; override;
  end;
  
  TGRBitmapList = class(TGRCustomPictureList)
  public
    class function CollectionClass: TGRPictureCollectionClass; override;
  end;

implementation

//uses Math, TypInfo, GR32_System;

type
  TComponentAccess = class(TComponent);
  //TBitmap32Access = class(TBitmap32);

{ TGRCustomPictureItem }
procedure TGRCustomPictureItem.Assign(Source: TPersistent);
begin
  if Source is TGRCustomPictureItem then
  begin
    IsPictureStored := TGRCustomPictureItem(Source).IsPictureStored;
  end;
  inherited;
end;

function TGRCustomPictureItem.DoURLChanged(const aURL: string): Boolean;
begin
  //TODO
  Result := inherited DoURLChanged(aURL);
end;

function TGRCustomPictureItem.GetIsPictureStored: Boolean;
begin
  Result := FIsPictureStored or (URL = '');
end;

procedure TGRCustomPictureItem.LoadBuffer;
begin
end;

procedure TGRCustomPictureItem.PictureChanged(Sender: TObject);
begin
  if Collection is TGRCustomPictureCollection then
    TGRCustomPictureCollection(Collection).PictureChanged(Self);
end;

procedure TGRCustomPictureItem.ReleaseBuffer; 
begin
end;

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
  end;
  inherited;
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

{ TGRBitmapItem }
constructor TGRBitmapItem.Create(aCollection: TCollection);
begin
  inherited;
  FPicture := TBitmap32.Create;
  FPicture.OnChange := PictureChanged;
end;

destructor TGRBitmapItem.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TGRBitmapItem.Assign(Source: TPersistent);
begin
  if Source is TGRBitmapItem then
  begin
    Picture := TGRBitmapItem(Source).Picture;
  end;
  inherited;
end;

function TGRBitmapItem.GetPicture: TBitmap32;
begin
  Use;
  Result := FPicture;
end;

procedure TGRBitmapItem.SetPicture(const Value: TBitmap32);
begin
  if FPicture <> Value then
    FPicture.Assign(Value);
end;

{ TGRCustomPictureCollection }
constructor TGRCustomPictureCollection.Create(AOwner: TPersistent);
begin
  inherited Create(ItemClass);
  FOwner := AOwner;
end;

function TGRCustomPictureCollection.Add: TGRCustomPictureItem;
begin
  Result := TGRCustomPictureItem(inherited Add);
end;

function TGRCustomPictureCollection.Find(const aName: string): TGRCustomPictureItem;
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

function TGRCustomPictureCollection.GetItem(Index: Integer): TGRCustomPictureItem;
begin
  Result := TGRCustomPictureItem(inherited GetItem(Index));
end;

function TGRCustomPictureCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TGRCustomPictureCollection.PictureChanged(const Sender: TGRCustomPictureItem);
begin
  if Assigned(FOnPictureChanged) then
    FOnPictureChanged(Sender);
  //if FOwner is TGRCustomPictureList then
    //TGRCustomPictureList(FOwner).PictureChanged(Sender);
end;

procedure TGRCustomPictureCollection.SetItem(Index: Integer; Value: TGRCustomPictureItem);
begin
  inherited SetItem(Index, Value);
end;

{ TGRPictureCollection }
class function TGRPictureCollection.ItemClass: TGRPictureItemClass;
begin
  Result := TGRPictureItem;
end;

{ TGRBitmapCollection }
class function TGRBitmapCollection.ItemClass: TGRPictureItemClass;
begin
  Result := TGRBitmapItem;
end;

{ TGRCustomPictureList }
constructor TGRCustomPictureList.Create(AOwner: TComponent);
begin
  inherited;
  FPictureCollection := CollectionClass.Create(Self);
  FPictureCollection.OnPictureChanged := PictureChanged;
  FNotifyList := TList.Create;
end;

destructor TGRCustomPictureList.Destroy;
begin
  ClearNotifyList;
  FNotifyList.Free;
  FPictureCollection.Free;
  inherited;
end;

procedure TGRCustomPictureList.ClearNotifyList;
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

procedure TGRCustomPictureList.ChangeNotification(const aComponent: TComponent);
begin
  if Assigned(aComponent) and (FNotifyList.IndexOf(aComponent) < 0) then
    FNotifyList.Add(aComponent);
end;

procedure TGRCustomPictureList.RemoveChangeNotification(const aComponent: TComponent);
begin
  FNotifyList.Remove(aComponent);
end;

function TGRCustomPictureList.GetPicture(const Index: Integer): TGRCustomPictureItem;
begin
  Result := FPictureCollection.Items[Index];
end;

procedure TGRCustomPictureList.PictureChanged(const Sender: TGRCustomPictureItem);
var
  i: Integer;
begin
  if Assigned(FOnPictureChanged) then
    FOnPictureChanged(Sender);
  for i := 0 to FNotifyList.Count - 1 do
  begin
    TComponentAccess(FNotifyList.Items[i]).Notification(TComponent(Sender), TOperation(opChanged));
  end;
end;

procedure TGRCustomPictureList.SetPicture(const Index: Integer; const Value: TGRCustomPictureItem);
begin
  FPictureCollection.Items[Index] := Value;
end;

procedure TGRCustomPictureList.SetPictures(const Value: TGRCustomPictureCollection);
begin
  if FPictureCollection <> Value then
    FPictureCollection.Assign(Value);
end;

{ TGRPictureList }
class function TGRPictureList.CollectionClass: TGRPictureCollectionClass;
begin
  Result := TGRPictureCollection;
end;

{ TGRBitmapList }
class function TGRBitmapList.CollectionClass: TGRPictureCollectionClass;
begin
  Result := TGRBitmapCollection;
end;

end.
