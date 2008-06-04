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
 * The Original Code is GR_ImageEx
 *
 * The Initial Developer of the Original Code is Riceball LEE
 * Portions created by Riceball LEE are Copyright (C) 2004-2007
 * All Rights Reserved.
 *
 * Contributor(s):
 *   Michael Haralabos (Transparent TImage32 )
 *   Riceball LEE(Mouse and keyboard events supports)
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_ImageEx;


interface

{$I GR32.inc}

uses
  {$IFDEF Debug}
  CnDebug,
  {$ENDIF}
{$IFDEF CLX}
  Qt, Types, QControls, QGraphics, QForms, QConsts,
  {$IFDEF LINUX}Libc,{$ENDIF}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
{$ELSE}
  Windows, Messages, Controls, Graphics, Forms, 
  //Dialogs,
{$ENDIF}
  Classes, SysUtils, Menus
  , GR32_Image, GR32_Layers, GR32
  , GR_Layers
  ;

type
  TFixupReferenceProc = procedure (const aName: string; const aObj: TObject) of object;
  PFixupReferenceInfo = ^ TFixupReferenceInfo;
  TFixupReferenceInfo = record
    Name: string;
    Proc: TFixupReferenceProc;
  end;
  TFixupReferences = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  TImage32Ex = class(TImage32)
  protected
    FCurrentFocusedLayer: TGRLayer;
    //collects can focused layers(TabStop = true)
    FFocusLayers: TList;
    FIsLoading: Boolean;
    FFixupReferences: TFixupReferences;
    FTransparent: Boolean;
    FPopupMenu: TPopupMenu;

    procedure SetTransparent(const Value: Boolean);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure DoLayerChanged(Sender: TLayerCollection; Action: TLayerListNotification; Layer: TCustomLayer; Index: Integer);
    function GetFixupReferences: TFixupReferences;

    procedure ReadData(aReader: TReader);
    procedure WriteData(aWriter: TWriter);
  protected
    FIsDragging: Boolean;
    FLastShift: TShiftState;
    FLastMouseDownPos: TPoint;
{$IFDEF CLX}
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
{$ELSE}
    procedure MouseEnter; override;
    procedure MouseLeave; override;
{$ENDIF}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); overload; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); overload; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); overload; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure WndProc(var Message: TMessage);override;

    procedure DefineProperties(Filer: TFiler); override;
    procedure DoPopupMenu(const X, Y: Integer);virtual;

    property FixupReferences: TFixupReferences read GetFixupReferences;
  public
    constructor Create(aOwner: TComponent);override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;
    procedure ExecClearBackgnd(Dest: TBitmap32; StageNum: Integer); override;
    { Summary: Load the user interface description(binary dfm) from stream }
    procedure LoadFromStream(const aStream: TStream);virtual;
    { Summary: save the user interface description(binary dfm) to stream }
    procedure SaveToStream(const aStream: TStream);virtual;
    { Summary: Get the layer index by layer name. return -1 means no such named layer. }
    function IndexOf(const aName: string): Integer;
    { Summary: Load the user interface description(binary dfm) from file }
    procedure LoadFromFile(const aFileName: string);
    { Summary: save the user interface description(binary dfm) to file }
    procedure SaveToFile(const aFileName: string);
    { Summary: Load the user interface description from text(dfm) }
    procedure LoadFromString(const s: string);
    { Summary: convert the user interface description to text(dfm) }
    function SaveToString: string;
    procedure AddFixupReference(const aName: string; const aProc: TFixupReferenceProc);
    function GetScaledSize(const aViewportWidth, aViewportHeight: Integer): TSize;

    property IsLoading: Boolean read FIsLoading;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
  published
    property Enabled;
    property Transparent: Boolean read FTransparent write SetTransparent;
  end;

  TImage32Editor = class(TImage32Ex)
  protected
    FRubberBand: TGRRubberBandLayer;
    FSelection: TGRTransformationLayer;
    FPopupMenu: TPopupMenu;
    FOnSelectionChanged: TNotifyEvent;

    procedure SetSelection(Value: TGRTransformationLayer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer); reintroduce; overload;override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    procedure LoadFromStream(const aStream: TStream);override;
    procedure RemoveSelectedLayer();
    function CreateLayer(const aClass: TGRLayerClass): TGRPropertyLayer;
    procedure Clear;

    property Selection: TGRTransformationLayer read FSelection write SetSelection;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;

procedure Register;

resourcestring
  rsUnNamed = 'Unnamed';

implementation

uses
  RTLConsts,
  uMeInjector,
  GR_LayerEditors;

type
  TLayerAccess = class(TCustomLayer);
  TLayerCollectionAccess = class(TLayerCollection);
  TExtRubberBandLayerAccess = class(TGRRubberBandLayer);
  TReaderAccess = class(TReader);
  TWriterAccess = class(TWriter);
  TGRLayerAccess = class(TGRLayer);

  TMyShiftState  = (ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble);
  TMyShiftStates = set of TMyShiftState;

  TGRLayerWriter = class(TWriter)
  protected
    function FindMethodName(Method: TMethod): string; override;
  end;

  TGRLayerReader = class(TReader)
  protected
    FCurrentPropInfo: Pointer;
    FCurrentInstance: string;
    FFixupList: TStrings;
    function GetFixupList: TStrings;
    function FindMethodInstance(Root: TComponent; const MethodName: string): TMethod;override;
    procedure ReadPropValue(Instance: TPersistent; PropInfo: Pointer);
  public
    destructor Destroy; override;
    property FixupList: TStrings read GetFixupList;
  end;

//Not double click in shift state
function IsMouseButtonDown(Shift: TMyShiftStates; Button: TMyShiftState): Boolean;
begin
  Result := (Button in Shift) and not (ssDouble in Shift);
end;

{ TGRLayerWriter }
function TGRLayerWriter.FindMethodName(Method: TMethod): string;
var
  vObj: TObject;
begin
  vObj := Method.Data;
  if vObj is TGRCustomLayer then
  begin
    Result := vObj.MethodName(Method.Code);
    if Result <> '' then
      Result := TGRCustomLayer(vObj).Name + '.' + Result;
  end
  else 
    Result := inherited FindMethodName(Method);
end;

{ TGRLayerReader }
destructor TGRLayerReader.Destroy;
begin
  FreeAndNil(FFixupList);
  inherited;
end;

function TGRLayerReader.FindMethodInstance(Root: TComponent; const MethodName: string): TMethod;
var
  i: Integer;
  vObjName, vMethodName: string;
begin
  i := Pos('.', MethodName);
  if i > 0 then
  begin
    vObjName := Copy(MethodName, 1, i-1);
    vMethodName := Copy(MethodName, i+1, Length(MethodName));
    if Root is TImage32Ex then
    begin
      i := TImage32Ex(Root).IndexOf(vObjName);
      if i >= 0 then
      begin
        Result.Data := TImage32Ex(Root).Layers[i];
        Result.Code := TImage32Ex(Root).Layers[i].MethodAddress(vMethodName);
        if Result.Code = nil then 
          raise EReadError.CreateRes(@SInvalidPropertyValue);
      end
      else
        FixupList.AddObject(FCurrentInstance+'='+MethodName, FCurrentPropInfo);
      Exit;
    end;
  end
  else begin
    vObjName := '';
    vMethodName := MethodName;
  end;
  Result := inherited FindMethodInstance(Root, MethodName);
end;

function TGRLayerReader.GetFixupList: TStrings;
begin
  if not Assigned(FFixupList) then
    FFixupList := TStringList.Create;
  Result := FFixupList;
end;

var
  FOldReadPropValueProc: procedure(const aSelf: TObject; Instance: TPersistent; PropInfo: Pointer) = nil;
  vReadPropValueInjector: TMeInjector;

procedure TGRLayerReader.ReadPropValue(Instance: TPersistent; PropInfo: Pointer);
begin
  if Self is TGRLayerReader then
  begin
    FCurrentPropInfo := PropInfo;
    if Instance is TGRCustomLayer then
      FCurrentInstance := TGRCustomLayer(Instance).Name
    else
      FCurrentInstance := '';
  end;
  if Assigned(FOldReadPropValueProc) then
    FOldReadPropValueProc(Self, Instance, PropInfo);
end;

{ TImage32Ex }
constructor TImage32Ex.Create(aOwner: TComponent);
begin
  inherited;
  FTransparent := False;
  FFocusLayers := TList.Create;
  TLayerCollectionAccess(Layers).OnListNotify := DoLayerChanged;
end;

destructor TImage32Ex.Destroy;
begin
  FreeAndNil(FFixupReferences);
  FreeAndNil(FFocusLayers);
  inherited;
end;

procedure AssignLayers(const aSrc, aDest: TLayerCollection);
var
  I: Integer;
  vItem: TCustomLayer;
begin
  aDest.Clear;
  TLayerCollectionAccess(aDest).BeginUpdate;
  try
    for I := 0 to aSrc.Count - 1 do
    begin
      vItem := aSrc.Items[I];
      if (vItem is TGRRubberBandLayer) or (vItem is TGRGridLayer) then
        continue;
      if (vItem is TGRPropertyLayer) and not TGRPropertyLayer(vItem).CanStored then
        continue;
      aDest.Add(TLayerClass(vItem.ClassType)).Assign(vItem);
    end;
  finally
    TLayerCollectionAccess(aDest).EndUpdate;
  end;
end;

procedure TImage32Ex.AddFixupReference(const aName: string; const aProc: TFixupReferenceProc);
var
  v: PFixupReferenceInfo;
begin
  if FIsLoading then
  begin
  New(v);
    v.Name := aName;
    v.Proc := aProc;
    FixupReferences.Add(v);
  end;
end;

procedure TImage32Ex.Assign(Source: TPersistent);
begin
  BeginUpdate;
  FIsLoading := True;
  try
    if Source is TImage32Ex then
    begin
        AssignLayers(TImage32Ex(Source).Layers, Layers);
        FTransparent := TImage32Ex(Source).FTransparent;
        Scale := TImage32Ex(Source).Scale;
        ScaleMode := TImage32Ex(Source).ScaleMode;
        BitmapAlign := TImage32Ex(Source).BitmapAlign;
        Bitmap.Assign(TImage32Ex(Source).Bitmap);
        Invalidate;
    end;
  finally
    FIsLoading := False;
    EndUpdate;
  end;
end;

function SortTabOrder(Item1, Item2: TGRLayerAccess): Integer;
begin
  Result := Item2.TabOrder - Item1.TabOrder;
end;
 
procedure TImage32Ex.DoLayerChanged(Sender: TLayerCollection; Action: TLayerListNotification; Layer: TCustomLayer; Index: Integer);
var
  i: integer;
begin
  if Layer is TGRLayer then
  begin
    case Action of
      lnTabStopChanged:
      begin
        i := FFocusLayers.IndexOf(Layer);
        if TGRLayerAccess(Layer).TabStop then
        begin
          if i < 0 then FFocusLayers.Add(Layer);
        end
        else
          if i >= 0 then FFocusLayers.Delete(i);
      end;
      lnTabOrderChanged:
      begin
        i := FFocusLayers.IndexOf(Layer);
        if i >=0 then
        begin
          FFocusLayers.Sort(@SortTabOrder);
        end;
      end;
      lnLayerDeleted:
      begin
        i := FFocusLayers.IndexOf(Layer);
        if i >= 0 then FFocusLayers.Delete(i);
      end;
      lnCleared:
        FFocusLayers.Clear;
    end; //case
  end;
end;

procedure TImage32Ex.DoPopupMenu(const X, Y: Integer);
begin
  if Assigned(FPopupMenu) then
    FPopupMenu.Popup(X, Y);
end;

procedure TImage32Ex.LoadFromStream(const aStream: TStream);
var
  //vReader: TGRReader;
  vLayer: TGRCustomLayer;
  vObjName, vMethodName: string;
  vM: TMethod;
  i, j: integer;
  v: PFixupReferenceInfo;
begin
  FIsLoading := True;
  try
    FixupReferences.Clear;
    aStream.ReadComponent(Self);
    with FFixupReferences do if Count > 0 then
    begin
      for i := 0 to Count - 1 do
      begin
        v := Items[i];
        if Assigned(v) then
        begin
          j := Self.IndexOf(v.Name);
          Assert(j >= 0, 'the '+ v.Name + ' can not FixupReferences!');
          if j >= 0 then
            v.Proc(v.Name, Layers[j]);
        end;
      end;
    end;
  finally
    FIsLoading := False;
  end;
  {
  vReader := TGRReader.Create(aStream, 4096);
  try
    vReader.ReadRootComponent(Self);
    if vReader.FixupList.Count > 0 then
    begin
      with vReader do for i := 0 to FixupList.Count - 1 do
      begin
        j := IndexOf(FixupList.Names[i]);
        if j >= 0 then
        begin
          vLayer := Layers[j];
          vMethodName := FixupList.ValueFromIndex[i];
          j := Pos('.', vMethodName);
          if j > 0 then
          begin
            vObjName := Copy(vMethodName, 1, i -1);
            Delete(vMethodName, 1, i);
            j := IndexOf(vObjName);
            if j >= 0 then
            begin
              vM.Data := Layers[j];
              vM.Code := Layers[j].MethodAddress(vMethodName);
              if vM.Code <> nil then SetMethodProp(vLayer, FixupList.Objects[i], vM);
            end;
          end;
        end;
      end;
    end;
  finally
    vReader.Free;
  end; //}
end;

procedure TImage32Ex.SaveToStream(const aStream: TStream);
//var
  //vWriter: TGRWriter;
begin
	aStream.WriteComponent(Self);
	{
  vWriter := TGRWriter.Create(aStream, 4096);
  try
    vWriter.WriteDescendent(Self, nil);
  finally
    vWriter.Free;
  end; //}
end;

procedure TImage32Ex.LoadFromFile(const aFileName: string);
var
  vStream: TFileStream;
begin
  vStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(vStream);
  finally
    vStream.Free;
  end;
end;

procedure TImage32Ex.SaveToFile(const aFileName: string);
var
  vStream: TFileStream;
begin
  vStream := TFileStream.Create(aFileName, fmCreate or fmShareDenyWrite);
  try
    SaveToStream(vStream);
  finally
    vStream.Free;
  end;
  //SaveStrToFile(aFileName+'.txt', ComponentToStr(Self));
end;

procedure TImage32Ex.LoadFromString(const s: string);
var
  vStrStream:TStringStream;
  vBinStream: TMemoryStream;
begin
  vStrStream := TStringStream.Create(s);
  try
    vBinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(vStrStream, vBinStream);
      vBinStream.Seek(0, soFromBeginning);
      LoadFromStream(vBinStream);
    finally
      vBinStream.Free;
    end;
  finally
    vStrStream.Free;
  end;
end;

function TImage32Ex.SaveToString: string;
begin
    Result := ComponentToStr(Self);
end;

procedure TImage32Ex.DefineProperties(Filer: TFiler);
  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TImage32Ex)
    else
      Result := Layers.Count > 0;
  end;
begin
  inherited;
  Filer.DefineProperty('Layers', ReadData, WriteData, DoWrite);
end;

procedure TImage32Ex.ExecClearBackgnd(Dest: TBitmap32; StageNum: Integer);
var
  P: TPoint;
  SaveIndex: Integer;
begin
  if FTransparent and Assigned(Parent) and
     not (Assigned(Bitmap) and (BitmapAlign = baTile)) then
  begin
    SaveIndex := SaveDC(Dest.Handle);
    GetViewportOrgEx(Dest.Handle, P);
    SetViewportOrgEx(Dest.Handle, P.X - Left, P.Y - Top, nil);
    IntersectClipRect(Dest.Handle, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
    Parent.Perform(WM_ERASEBKGND, Dest.Handle, 0);
    Parent.Perform(WM_PAINT, Dest.Handle, 0);
    RestoreDC(Dest.Handle, SaveIndex);
  end
  else
    inherited;
end;

procedure TImage32Ex.ReadData(aReader: TReader);
var
  vItem: TGRCustomLayer;
  vLayerClass: TGRLayerClass;
  vS: string;
begin
  TLayerCollectionAccess(Layers).BeginUpdate;
  with TReaderAccess(aReader) do
  try
    //if not EndOfList then 
    Layers.MouseListener := nil;
    Layers.Clear;
    Assert(NextValue = vaCollection, 'NOT Layers Collection');
    ReadValue; //skip vaCollection flag
    while not EndOfList do
    begin
      if NextValue in [vaInt8, vaInt16, vaInt32] then ReadInteger;
      ReadListBegin;
      vS := ReadStr;
      vS := ReadString;
      vLayerClass := GetLayerClass(vS);

      Assert(Assigned(vLayerClass),  vS + ' not Registered');

      vItem := vLayerClass.Create(Layers);
      while not EndOfList do ReadProperty(vItem);
      ReadListEnd;
    end;
    ReadListEnd;
  finally
    TLayerCollectionAccess(Layers).EndUpdate;
  end;
end;

procedure TImage32Ex.WriteData(aWriter: TWriter);
var
  I: Integer;
  OldAncestor: TPersistent;
begin
  TLayerCollectionAccess(Layers).BeginUpdate;
  with TWriterAccess(aWriter) do
  try
    OldAncestor := Ancestor;
    Ancestor := nil;
    WriteValue(vaCollection);
    for I := 0 to Layers.Count - 1 do
    begin
      if (Layers[I] is TGRRubberBandLayer) or (Layers[I] is TGRGridLayer) then
        continue;
      if (Layers[I] is TGRPropertyLayer) and not TGRPropertyLayer(Layers[I]).CanStored then
        continue;
      WriteListBegin;
      WriteStr('Class');
      WriteString(Layers[I].ClassName);
      WriteProperties(Layers[I]);
      WriteListEnd;
    end;
    WriteListEnd;
  finally
    aWriter.Ancestor := OldAncestor;
    TLayerCollectionAccess(Layers).EndUpdate;
  end;
end;

function TImage32Ex.IndexOf(const aName: string): Integer;
begin
  Result := IndexOfLayers(Layers, aName);
end;

function TImage32Ex.GetFixupReferences: TFixupReferences;
begin
  if not Assigned(FFixupReferences) then
    FFixupReferences := TFixupReferences.Create;
  Result := FFixupReferences;
end;

function TImage32Ex.GetScaledSize(const aViewportWidth, aViewportHeight: Integer): TSize;
var
  Mode: TScaleMode;
  RScaleX, RScaleY: Single;
begin
  with Result do
  begin
    if Bitmap.Empty or (Width = 0) or (Height = 0) then
    begin
      Cx := 0;
      Cy := 0;
      Exit;
    end;

    // check for optimal modes as these are compounds of the other modes.
    case ScaleMode of
      smOptimal:
        if (Bitmap.Width > aViewportWidth) or (Bitmap.Height > aViewportHeight) then
          Mode := smResize
        else
          Mode := smNormal;
      smOptimalScaled:
        if (Round(Bitmap.Width * ScaleX) > aViewportWidth) or
          (Round(Bitmap.Height * ScaleY) > aViewportHeight) then
          Mode := smResize
        else
          Mode := smScale;
    else
      Mode := ScaleMode;
    end;

    case Mode of
      smNormal:
        begin
          Cx := Bitmap.Width;
          Cy := Bitmap.Height;
        end;
      smStretch:
        begin
          Cx := aViewportWidth;
          Cy := aViewportHeight;
        end;
      smResize:
        begin
          Cx := Bitmap.Width;
          Cy := Bitmap.Height;
          RScaleX := aViewportWidth / Cx;
          RScaleY := aViewportHeight / Cy;
          if (RScaleX >= RScaleY) and (Cy > Cx) then
          begin
            Cx := Round(Cx * RScaleY);
            Cy := aViewportHeight;
          end
          else
          begin
            Cx := aViewportWidth;
            Cy := Round(Cy * RScaleX);
          end;
        end;
    else // smScale
      begin
        Cx := Round(Bitmap.Width * ScaleX);
        Cy := Round(Bitmap.Height * ScaleY);
      end;
    end;
    if Cx <= 0 then Cx := 0;
    if Cy <= 0 then Cy := 0;
  end;
end;

procedure TImage32Ex.SetPopupMenu(const Value: TPopupMenu);
begin
  if Value <> FPopupMenu then
  begin
    if Assigned(FPopupMenu) then
      FPopupMenu.RemoveFreeNotification(Self);
    FPopupMenu := Value;
    if Assigned(FPopupMenu) then
      FPopupMenu.FreeNotification(Self);
  end;
end;

procedure TImage32Ex.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

{$IFDEF CLX}
procedure TImage32Ex.MouseEnter(AControl: TControl);
{$ELSE}
procedure TImage32Ex.MouseEnter;
{$ENDIF}
begin
  if (Layers.MouseEvents) and (Layers.MouseListener <> nil) and not Layers.MouseListener.MouseInControl then
    TLayerAccess(Layers.MouseListener).MouseEnter;
  inherited;
end;

{$IFDEF CLX}
procedure TImage32Ex.MouseLeave(AControl: TControl);
{$ELSE}
procedure TImage32Ex.MouseLeave;
{$ENDIF}
begin
  if (Layers.MouseEvents) and (Layers.MouseListener <> nil) and Layers.MouseListener.MouseInControl then
    TLayerAccess(Layers.MouseListener).MouseLeave;
  inherited;
end;

procedure TImage32Ex.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vP: TPoint;
begin
  FLastShift := Shift;
  FLastMouseDownPos.X := X;
  FLastMouseDownPos.Y := Y;
  if not FIsDragging then
  begin
    inherited;
    if Assigned(FPopupMenu) and IsMouseButtonDown(TMyShiftStates(Shift), ssRight) then
    begin
      vP.X := X;
      vP.Y := Y;
      vP := ClientToScreen(vP);
      with vP do DoPopupMenu(X, Y);
    end;
  end;
end;

const
  sc_DragMove=$F012; //61458

procedure TImage32Ex.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not FIsDragging then
  begin
    FIsDragging := IsMouseButtonDown(TMyShiftStates(Shift), ssLeft) and IsMouseButtonDown(TMyShiftStates(FLastShift), ssLeft) 
    and ((Abs(X - FLastMouseDownPos.X) >=2) or (Abs(Y - FLastMouseDownPos.Y) >=2));
  end;
  if FIsDragging and (Parent is TCustomForm) then
  begin
  	//no the mouseUp will be disabled!!
    ReleaseCapture;
    SendMessage(Parent.Handle, WM_SYSCOMMAND, sc_DragMove, 0);
  end;
  inherited;
end;

procedure TImage32Ex.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not FIsDragging then
    inherited;
  //when ReleaseCapture this would be never execute.
  FIsDragging := False;
end;

procedure TImage32Ex.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) then
  begin
    if (AComponent = FPopupMenu)  then
      FPopupMenu := nil
  end;
  inherited;
end;

procedure TImage32Ex.WMKillFocus(var Message: TMessage);
begin
  DoExit;
  inherited;
end;

procedure TImage32Ex.KeyDown(var Key: Word; Shift: TShiftState); 
begin
  If (Layers.MouseListener <> nil) and Layers.KeyEvents Then
  Begin
    TLayerAccess(Layers.MouseListener).KeyDown(Key, Shift);
  End; // If
  inherited;
end;

procedure TImage32Ex.KeyPress(var Key: Char); 
begin
  If (Layers.MouseListener <> nil) and Layers.KeyEvents Then
  Begin
    TLayerAccess(Layers.MouseListener).KeyPress(Key);
  End; // If
  inherited;
end;

procedure TImage32Ex.KeyUp(var Key: Word; Shift: TShiftState); 
begin
  If (Layers.MouseListener <> nil) and Layers.KeyEvents Then
  Begin
    TLayerAccess(Layers).KeyUp(Key, Shift);
  End; // If
  inherited;
end;

procedure TImage32Ex.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_MOUSEFIRST..WM_MOUSELAST: if IsControlMouseMsg(TWMMouse(Message)) then
    begin
      //first pass it to self to prevent from be processed by children  .
      Dispatch(Message);
      //exit;
    end
    //else ShowMessage(IntToStr(TWMMouse(Message).XPos))
    ;
  end;

  inherited;
  If (Layers.MouseListener <> nil) and Layers.MessageEvents Then
  Begin
    TLayerAccess(Layers.MouseListener).WndProc(Message);
  End; // If
end;

{ TImage32Editor }

function TImage32Editor.CreateLayer(const aClass: TGRLayerClass): TGRPropertyLayer;
var
  P: TPoint;
begin
  with GetViewportRect do
    P := ControlToBitmap(Point((Right + Left) div 2, (Top + Bottom) div 2));
  Result := aClass.Create(Layers);
  if Result is TGRPropertyLayer then with TGRPropertyLayer(Result) do
  begin
    Left := P.X;
    Top := P.Y;
    if TGRLayerEditor.Execute(TGRPropertyLayer(Result)) then
    begin
      Selection := TGRPropertyLayer(Result);
      Result.Name := CreateUniqueDefaultName(Layers);
    end
    else
      FreeAndNil(Result);
    end;
end;

procedure TImage32Editor.Clear;
begin
  Selection := nil;
  FRubberBand := nil;
  Layers.Clear;
end;

procedure TImage32Editor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_ESCAPE: if Assigned(FRubberBand) and Assigned(FSelection) then
      FRubberBand.Cancel;
  end;
end;
procedure TImage32Editor.LoadFromStream(const aStream: TStream);
begin
  inherited;
  FSelection := nil;
end;

procedure TImage32Editor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if (Layer is TGRTransformationLayer) then
  begin
    Selection := TGRTransformationLayer(Layer);
    if Assigned(FRubberBand) then
    begin
      Layers.MouseListener := nil;
      Layer := TLayerCollectionAccess(Layers).MouseDown(Button, Shift, X, Y);
    end;
  end
  else
  begin
    Selection := nil;
  end;
  inherited MouseDown(Button, Shift, X, Y, Layer);
 {$IFDEF Debug}
  //if Assigned(Layer) then sendDebug('Image32Ex.MouseDown Layer=' + Layer.ClassName);
 {$ENDIF}
end;

procedure TImage32Editor.RemoveSelectedLayer;
var
  vSelected: TGRPositionLayer;
begin
  vSelected := FSelection;
  if Assigned(vSelected) then
  begin
    Selection := nil;
    vSelected.Free;
  end;
end;

procedure TImage32Editor.SetSelection(Value: TGRTransformationLayer);
begin
  if Value is TGRRubberBandLayer then exit;
  if Value <> FSelection then
  begin
    if FRubberBand <> nil then
    begin
      FRubberBand.ChildLayer := nil;
      Invalidate;
    end;

    FSelection := Value;

    if Value <> nil then
    begin
 {$IFDEF Debug}
  sendDebug('SetSelection=' + Value.ClassName);
 {$ENDIF}
      //Value.BringToFront;
      if FRubberBand = nil then
      begin
        FRubberBand := TGRRubberBandLayer.Create(Layers);
        //FRubberBand.Options := [rboAllowMove, rboShowFrame];
      end;
      //else 
        FRubberBand.BringToFront;
      //FRubberBand.Index := Value.Index + 1;
      FRubberBand.ChildLayer := Value;
    end;
    if Assigned(FOnSelectionChanged) then FOnSelectionChanged(Self);
  end;
end;

procedure TFixupReferences.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Assigned(Ptr) and (Action = lnDeleted) then
    FreeMem(Ptr);
end;

procedure Register;
begin
  RegisterComponents('Graphics32', [TImage32Ex, TImage32Editor]);
end;

initialization
  //if vReadPropValueInjector.InjectProcedure(@TReader.ReadPropValue, @TGRReader.ReadPropValue) then
    //@FOldReadPropValueProc := vReadPropValueInjector.OriginalProc

finalization
  //vReadPropValueInjector.Enabled := False;
end.

