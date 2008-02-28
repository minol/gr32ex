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
{$IFDEF CLX}
  Qt, Types, QControls, QGraphics, QForms, QConsts,
  {$IFDEF LINUX}Libc,{$ENDIF}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
{$ELSE}
  Windows, Messages, Controls, Graphics, Forms, 
  //Dialogs,
{$ENDIF}
  Classes, SysUtils, Menus
  GR32_Image, GR32_Layers, GR32
  , GR32_ExtLayers
  ;

type
  TImage32Ex = class(TImage32)
  private
    FTransparent: Boolean;

    procedure SetTransparent(const Value: Boolean);
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;

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

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure WndProc(var Message: TMessage);override;

    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure ExecClearBackgnd(Dest: TBitmap32; StageNum: Integer); override;
    procedure LoadFromStream(const aStream: TStream);virtual;
    procedure SaveToStream(const aStream: TStream);virtual;
    procedure LoadFromFile(const aFileName: string);
    procedure SaveToFile(const aFileName: string);
  published
    property Enabled;
    property Transparent: Boolean read FTransparent write SetTransparent;
  end;

  TImage32Editor = class(TImage32Ex)
  protected
    FRubberBand: TExtRubberBandLayer;
    FSelection: TTransformationLayer;
    FPopupMenu: TPopupMenu;

    procedure SetSelection(Value: TTransformationLayer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer); reintroduce; overload;override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    procedure LoadFromStream(const aStream: TStream);override;
    procedure RemoveSelectedLayer();

    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property Selection: TTransformationLayer read FSelection write SetSelection;
  end;

procedure Register;

implementation

uses
  GR_Layers;

type
  TLayerHack = class(TCustomLayer);
  TLayerCollectionAccess = class(TLayerCollection);
  TExtRubberBandLayerAccess = class(TExtRubberBandLayer);
  TReaderAccess = class(TReader);
  TWriterAccess = class(TWriter);

  TMyShiftState  = (ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble);
  TMyShiftStates = set of TMyShiftState;

//Not double click in shift state
function IsMouseButtonDown(Shift: TMyShiftStates; Button: TMyShiftState): Boolean;
begin
  Result := (Button in Shift) and not (ssDouble in Shift);
end;

{ TImage32Ex }
procedure TImage32Ex.LoadFromStream(const aStream: TStream);
{var
  vReader: TReader;
  //}
begin
  aStream.ReadComponent(Self);
{  vReader := TReader.Create(aStream, 4096);
  try
    vReader.ReadRootComponent(Self);
  finally
    vReader.Free;
  end; //}
end;

procedure TImage32Ex.SaveToStream(const aStream: TStream);
{var
  vWriter: TWriter;
  //}
begin
  aStream.WriteComponent(Self);
{  vWriter := TWriter.Create(aStream, 4096);
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

function ComponentToStr(Component: TComponent): string;
var
  BinStream:TMemoryStream;
  StrStream: TStringStream;
  s: string;
begin
  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create(s);
    try
      BinStream.WriteComponent(Component);
      BinStream.Seek(0, soFromBeginning);
      //try
      ObjectBinaryToText(BinStream, StrStream);
      //except
      //  on E:Exception do
      //end;
      StrStream.Seek(0, soFromBeginning);
      Result:= StrStream.DataString;
    finally
      StrStream.Free;

    end;
  finally
    BinStream.Free
  end;
end;

procedure SaveStrToFile(const aFileName, s: string);
begin
  with TStringList.Create do
  try
    Text := s;
    SaveToFile(aFileName);
  finally
    Free;
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
  SaveStrToFile(aFileName+'.txt', ComponentToStr(Self));
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

procedure TImage32Ex.ReadData(aReader: TReader);
var
  vItem: TGRLayerControl;
  vLayerControlClass: TGRLayerControlClass;
  vS: string;
begin
  TLayerCollectionAccess(Layers).BeginUpdate;
  with TReaderAccess(aReader) do
  try
    //if not EndOfList then 
    Layers.Clear;
    Assert(NextValue = vaCollection, 'NOT Layers Collection');
    ReadValue; //skip vaCollection flag
    while not EndOfList do
    begin
      if NextValue in [vaInt8, vaInt16, vaInt32] then ReadInteger;
      ReadListBegin;
      vS := ReadStr;
      vS := ReadString;
      vLayerControlClass := GetPlayingLayerControlClass(vS);

      Assert(Assigned(vLayerControlClass),  vS + ' not Registered');

      vItem := vLayerControlClass.Create(Layers);
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
      if (Layers[I] is TExtRubberBandLayer) or (Layers[I] is TGridLayer) then
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
    TLayerHack(Layers.MouseListener).MouseEnter;
  inherited;
end;

{$IFDEF CLX}
procedure TImage32Ex.MouseLeave(AControl: TControl);
{$ELSE}
procedure TImage32Ex.MouseLeave;
{$ENDIF}
begin
  if (Layers.MouseEvents) and (Layers.MouseListener <> nil) and Layers.MouseListener.MouseInControl then
    TLayerHack(Layers.MouseListener).MouseLeave;
  inherited;
end;

procedure TImage32Ex.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FLastShift := Shift;
  FLastMouseDownPos.X := X;
  FLastMouseDownPos.Y := Y;
  if not FIsDragging then
    inherited;
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

procedure TImage32Ex.WMKillFocus(var Message: TMessage);
begin
  DoExit;
  inherited;
end;

procedure TImage32Ex.KeyDown(var Key: Word; Shift: TShiftState); 
begin
  If (Layers.MouseListener <> nil) and Layers.KeyEvents Then
  Begin
    TLayerHack(Layers.MouseListener).KeyDown(Key, Shift);
  End; // If
  inherited;
end;

procedure TImage32Ex.KeyPress(var Key: Char); 
begin
  If (Layers.MouseListener <> nil) and Layers.KeyEvents Then
  Begin
    TLayerHack(Layers.MouseListener).KeyPress(Key);
  End; // If
  inherited;
end;

procedure TImage32Ex.KeyUp(var Key: Word; Shift: TShiftState); 
begin
  If (Layers.MouseListener <> nil) and Layers.KeyEvents Then
  Begin
    TLayerHack(Layers).KeyUp(Key, Shift);
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
    TLayerHack(Layers.MouseListener).WndProc(Message);
  End; // If
end;

{ TImage32Editor }

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
  if (Layer is TTransformationLayer) then
  begin
    Selection := TTransformationLayer(Layer);
    if Assigned(FRubberBand) then
    begin
      Layers.MouseListener := nil;
      Layer := TLayerCollectionAccess(Layers).MouseDown(Button, Shift, X, Y);
    end;
  end
  else
  begin
    Selection := nil;
    inherited MouseDown(Button, Shift, X, Y, Layer);
  end;
 {$IFDEF Debug}
  if Assigned(Layer) then sendDebug('Image32Ex.MouseDown Layer=' + Layer.ClassName);
 {$ENDIF}
end;

procedure TImage32Editor.RemoveSelectedLayer;
var
  vSelected: TTransformationLayer;
begin
  vSelected := FSelection;
  if Assigned(vSelected) then
  begin
    Selection := nil;
    vSelected.Free;
  end;
end;

procedure TImage32Editor.SetSelection(Value: TTransformationLayer);
begin
  if Value is TExtRubberBandLayer then exit;
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
        FRubberBand := TExtRubberBandLayer.Create(Layers);
        FRubberBand.Options := [rboAllowMove, rboShowFrame];
      end;
      //else 
        FRubberBand.BringToFront;
      //FRubberBand.Index := Value.Index + 1;
      FRubberBand.ChildLayer := Value;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Graphics32', [TImage32Ex, TImage32Editor]);
end;

end.

