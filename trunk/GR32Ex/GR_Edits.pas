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
 * The Original Code is GR_Edits
 *
 * The Initial Developer of the Original Code is Riceball LEE
 * Portions created by Riceball LEE are Copyright (C) 2004-2007
 * All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_Edits;

{$I Setting.inc}

{.$define debug}
{$define MBCSSUPPORT}

interface

uses
  {$ifdef Debug}
  DbugIntf,
  {$endif}
  Messages, {$IFDEF LINUX} WinUtils, {$ENDIF} Windows,
  SysUtils, Classes, Graphics, Controls, Forms, CommCtrl
  , Menus, Imm
  , Clipbrd
  , GR32
  , GR_Graphics
  , GR_GraphUtils
  , GR_Controls
  , GR_FilterEx
  , SimpleTimer
  ;

type 
  TEditCharCase = (ecNormal, ecUpperCase, ecLowerCase);
  
type
  TGRAbstractEdit = class(TGRBGCustomControl)
  private
    FAutoSelect: Boolean;
    FCharCase: TEditCharCase;
    FCursorPos: Integer;
    FCursorShowing: Boolean;
    FCursorTimer: TSimpleTimer;
    FCursorXPos: Integer;
    FFirstDisplayChar: Integer;
    FHideSelection: Boolean;
    FMaxLength: Integer;
    FOnChange: TNotifyEvent;
    FPasswordChar: Char;
    FReadOnly: Boolean;
    FSelPoint1: Integer;
    FSelPoint2: Integer;
    FText: string;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure DoBlink(Sender: TObject);
    function GetCanUndo: Boolean;
    function GetModified: Boolean;
    procedure MenuEvent(Sender: TObject);
    procedure SetCharCase(const Value: TEditCharCase);
    procedure SetCursorPos(Value: Integer);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetModified(const Value: Boolean);
    procedure SetPasswordChar(const Value: Char);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetSelText(const Value: string);
  protected
    FIsFocused: Boolean;
    procedure BeforePaintBuffer(aBitmap32: TBitmap32); override;
    procedure CalcFirstDisplayChar; virtual;
    function CanAutoSize(var NewWidth: Integer; var NewHeight: Integer):
      Boolean; override;
    { Summary the text changed. }
    procedure Change; dynamic;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure DoDefaultPopupMenu(const PopupMenu: TPopupMenu); override;
    { Summary Respond to receiving input focus. }
    procedure DoEnter; override;
    { Summary Respond to losing input focus }
    procedure DoExit; override;
    procedure DoFrameChanged(Sender: TObject); override;
    procedure DrawCursor(aImage: TBitmap32;  XPos: Integer; CurrentCharPos:
      Integer); virtual; abstract;
    procedure DrawText(aImage: TBitmap32; XPos, YPos: Integer; Value: string;
      Selected: Boolean); virtual; abstract;
    function GetCharX(CharIndex: integer): Integer;
    function GetSelLength: Integer; virtual;
    function GetSelStart: Integer; virtual;
    function GetSelText: string; virtual;
    function GetTextHeight: Integer; virtual; abstract;
    function GetTextWidth(Index: Integer; Count: Integer = 1; Value: string =
      ''): Integer; virtual; abstract;
    function HitTest(XPos, YPos: Integer): TPoint; virtual;
    function InternalPaintBuffer(aBitmap32: TBitmap32): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure SetMaxLength(Value: Integer); virtual;
    procedure SetSelLength(Value: Integer); virtual;
    procedure SetSelStart(Value: Integer); virtual;
    procedure SetText(Value: string); virtual;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMImeComposition(var Msg: TMessage); message WM_IME_COMPOSITION;
    procedure WMImeStartComposition(var Msg: TMessage); message
      WM_IME_STARTCOMPOSITION;
    property AutoSelect: Boolean read FAutoSelect write FAutoSelect default
      True;
    property CharCase: TEditCharCase read FCharCase write SetCharCase default
      ecNormal;
    property Color default clWindow;
    property CursorPos: Integer read FCursorPos write SetCursorPos;
    property FirstDisplayChar: Integer read FFirstDisplayChar;
    property HideSelection: Boolean read FHideSelection write SetHideSelection
      default False;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property ParentColor default False;
    property PasswordChar: Char read FPasswordChar write SetPasswordChar
      default #0;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Text: string read FText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure ClearSelection;
    procedure ClearUndo;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    function Focused: Boolean; override;
    function GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer; virtual;
    procedure PasteFromClipboard;
    procedure SelectAll;
    procedure SetSelTextBuf(Buffer: PChar);
    procedure Undo;
    property CanUndo: Boolean read GetCanUndo;
    property Modified: Boolean read GetModified write SetModified;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: string read GetSelText write SetSelText;
  published
    property CaptionFont;
  end;
  
  TGRCustomEdit = class(TGRAbstractEdit)
  protected
    procedure DrawCursor(aImage: TBitmap32;  XPos: Integer; CurrentCharPos:
      Integer); override;
    procedure DrawText(aImage: TBitmap32; XPos, YPos: Integer; Value: string;
      Selected: Boolean); override;
    function GetTextWidth(Index: Integer; Count: Integer = 1; Value: string =
      ''): Integer; override;
    procedure Loaded; override;
  public
    function GetTextHeight: Integer; override;
  end;
  
  TGREdit = class(TGRCustomEdit)
  public
    property Background;
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property CharCase;
    property Color;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property HelpContext;
    property HideSelection;
    property Hint;
    property MaxLength;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Text;
    property Visible;
  end;
  

implementation

const
  cMenuUndo = 1;
  cMenuCut = 2;
  cMenuCopy = 3;
  cMenuPaste = 4;
  cMenuDelete = 5;
  cMenuSelectAll = 6;

constructor TGRAbstractEdit.Create(AOwner: TComponent);
begin
  inherited;
  FCursorTimer := TSimpleTimer.Create;
  with FCursorTimer do
  begin
    Enabled := False;
    Interval := 500;
    OnTimer := DoBlink;
  end;
  FAutoSelect := True;
  AutoSize := True;
  FCharCase := ecNormal;
  FHideSelection := False;
  FMaxLength := 0;
  FPasswordChar := #0;
  ParentColor := False;
  FReadOnly := False;
  FText := Name;
  FCursorPos := 0;
  FFirstDisplayChar := 1;
  FSelPoint1 := 0;
  FSelPoint2 := 0;
  Color := clWindow;
  Width := 100;
  Cursor := crIBeam;
  ControlStyle := ControlStyle + [csCaptureMouse];
end;

destructor TGRAbstractEdit.Destroy;
begin
  FCursorTimer.Free;
  inherited;
end;

procedure TGRAbstractEdit.BeforePaintBuffer(aBitmap32: TBitmap32);
begin
  inherited BeforePaintBuffer(aBitmap32);
  {aBitmap32.Font := CaptionFont;
  if Color <> clNone then
    aBitmap32.Clear(Color32(Color));
  //}
end;

procedure TGRAbstractEdit.CalcFirstDisplayChar;
var
  Value: string;
begin
  if PasswordChar = #0 then
    Value := Text
  else
    Value := StringOfChar(PasswordChar, Length(Text));
  if Value <> '' then
  begin
    if FFirstDisplayChar > CursorPos then
    begin
      FFirstDisplayChar := CursorPos;
      if FFirstDisplayChar < 1 then
        FFirstDisplayChar := 1;
    end;
    //I := FFirstDisplayChar;
    while GetTextWidth(FFirstDisplayChar, FCursorPos - FFirstDisplayChar + 1, Value) >=
      (Width - GetLeftBorderSize - GetRightBorderSize) do
    begin
      {$ifdef MBCSSUPPORT}
      if ByteType(Value, FFirstDisplayChar) = mbLeadByte then
      repeat
        Inc(FFirstDisplayChar);
      until ByteType(Value, FFirstDisplayChar) = mbTrailByte;
      {$endif}
      Inc(FFirstDisplayChar);
    end;
    {$ifdef debug}
    //SendDebug(Value+' FFirstDisplayChar:'+IntToStr(FFirstDisplayChar));
    {$endif}
    if FFirstDisplayChar > Length(Value) then
      FFirstDisplayChar := Length(Value);
    if FFirstDisplayChar <= 0 then FFirstDisplayChar := 1;
  end;
end;

function TGRAbstractEdit.CanAutoSize(var NewWidth: Integer; var NewHeight:
  Integer): Boolean;
begin
  NewHeight := GetBorderHeight + GetTextHeight;
  Result := NewHeight > 0;
end;

procedure TGRAbstractEdit.Change;
begin
  FCursorTimer.Enabled := False;
  FCursorShowing := True;
  FCursorTimer.Enabled := True;
  InvalidateSelfBuffer;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TGRAbstractEdit.Clear;
begin
  Text := '';
end;

procedure TGRAbstractEdit.ClearSelection;
begin
  SelText := '';
end;

procedure TGRAbstractEdit.ClearUndo;
begin
end;

procedure TGRAbstractEdit.CMEnter(var Message: TCMEnter);
begin
  DoEnter;
  inherited;
end;

procedure TGRAbstractEdit.CMExit(var Message: TCMExit);
begin
  DoExit;
  inherited;
end;

procedure TGRAbstractEdit.CMFontChanged(var Message: TMessage);
begin
  if AutoSize then AdjustSize;
  Change;
end;

procedure TGRAbstractEdit.CopyToClipboard;
begin
  if SelLength > 0 then Clipboard.AsText := SelText;
end;

procedure TGRAbstractEdit.CutToClipboard;
begin
  if ReadOnly then Exit;
  CopyToClipboard;
  SelText := '';
end;

procedure TGRAbstractEdit.DoBlink(Sender: TObject);
begin
  FCursorShowing := not FCursorShowing;
  InvalidateSelfBuffer;
end;

procedure TGRAbstractEdit.DoDefaultPopupMenu(const PopupMenu: TPopupMenu);
  
  function AddMenu(const Caption: string; Enabled: Boolean): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := Caption;
    Result.Enabled := Enabled;
    Result.OnClick := MenuEvent;
    PopupMenu.Items.Add(Result);
  end;
  
begin
  inherited;
  with AddMenu('Undo', CanUndo) do Tag := cMenuUndo;
  AddMenu('-', False);
  with AddMenu('Cut', not ReadOnly) do Tag := cMenuCut;
  with AddMenu('Copy', SelLength > 0) do Tag := cMenuCopy;
  with AddMenu('Paste', (not ReadOnly) and (Clipboard.AsText <> '')) do Tag := cMenuPaste;
  with AddMenu('Delete', (not ReadOnly) and (SelLength > 0)) do Tag := cMenuDelete;
  AddMenu('-', False);
  with AddMenu('Select All', Length(FText) > 0) do Tag := cMenuSelectAll;
end;

procedure TGRAbstractEdit.DoEnter;
begin
  inherited DoEnter;
    {$ifdef Debug}
    //SendDebug('DoEnter');
    {$endif}
  
  FIsFocused := True;
  FCursorShowing := True;
  FCursorTimer.Enabled := True;
  SelStart := 0;
  if AutoSelect then
    SelLength := Length(Text)
  else
    SelLength := 0;
  InvalidateSelfBuffer;
end;

procedure TGRAbstractEdit.DoExit;
begin
    {$ifdef Debug}
    //SendDebug('DoExit');
    {$endif}
  
  FIsFocused := False;
  inherited DoExit;
  FCursorTimer.Enabled := False;
  SelStart := 0;
  SelLength := 0;
  InvalidateSelfBuffer;
end;

procedure TGRAbstractEdit.DoFrameChanged(Sender: TObject);
begin
  if AutoSize then AdjustSize;
  inherited DoFrameChanged(Sender);
end;

function TGRAbstractEdit.Focused: Boolean;
begin
  Result := inherited Focused;
  //Result := FIsFocused;
end;

function TGRAbstractEdit.GetCanUndo: Boolean;
begin
  Result := False;
end;

function TGRAbstractEdit.GetCharX(CharIndex: integer): Integer;
var
  s: string;
  I: Integer;
begin
  Result := GetLeftBorderSize;
  if (CharIndex > 0) and (CharIndex <= Length(Text)) then
  begin
    //s := Copy(Text, 1, CharIndex);
    //I := 1;
    I := GetTextWidth(1, CharIndex);
    Inc(Result, I);
  end;
end;

function TGRAbstractEdit.GetModified: Boolean;
begin
  Result := False;
end;

function TGRAbstractEdit.GetSelLength: Integer;
begin
  if FSelPoint1 > FSelPoint2 then
    Result := FSelPoint1 - FSelPoint2
  else
    Result := FSelPoint2 - FSelPoint1;
end;

function TGRAbstractEdit.GetSelStart: Integer;
begin
  if FSelPoint1 < FSelPoint2 then
    Result := FSelPoint1
  else
    Result := FSelPoint2;
end;

function TGRAbstractEdit.GetSelText: string;
begin
  Result := '';
  if SelLength > 0 then Result := Copy(FText, SelStart+1, SelLength);
end;

function TGRAbstractEdit.GetSelTextBuf(Buffer: PChar; BufSize: Integer):
  Integer;
begin
  if SelLength > BufSize then Result := BufSize
  else
    Result := SelLength;
  if SelLength > 0 then Move(SelText[1], Buffer^, Result);
end;

function TGRAbstractEdit.HitTest(XPos, YPos: Integer): TPoint;
var
  I, CurrentX, RightBorderEdge, CharWidth: Integer;
begin
  Result.X := -1;
  Result.Y := 0;
  if Length(Text) = 0 then Exit;
  
  CurrentX := GetLeftBorderSize;
  RightBorderEdge := Width - GetRightBorderSize;
  I := FirstDisplayChar - 1;
  //J := 1;
  while I <= Length(FText) do
  begin
    if PasswordChar <> #0 then
    begin
      CharWidth := GetTextWidth(1,1,'*');
    end
    else
      CharWidth := GetTextWidth(I);
    if CurrentX + CharWidth + (CharWidth shr 1) >= XPos then
    begin
      Result.X := I;
      Break;
    end;
  
    Inc(CurrentX, CharWidth);
    if CurrentX >= (RightBorderEdge) then Break;
    Inc(I);
  end;
  if Result.X < 0 then Result.X := Length(Text);
end;

function TGRAbstractEdit.InternalPaintBuffer(aBitmap32: TBitmap32): Boolean;
var
  C: {$ifdef MBCSSUPPORT} String {$else} Char {$endif};
  I: Integer;
  XPos, YPos, RightBorder: Integer;
  J: Integer;
begin
  YPos := GetTopBorderSize;
  XPos := GetLeftBorderSize;
  FCursorXPos := XPos;
  RightBorder := Width - GetRightBorderSize;
  I := FFirstDisplayChar;
  while I <= Length(Text) do
  //for I := FFirstDisplayChar to Length(Text) do
  begin
    if PasswordChar = #0 then
    begin
    {$ifdef MBCSSUPPORT}
      j := I;
      if ByteType(Text, J) = mbLeadByte then
      begin
        C := '';
        while (J<=Length(Text)) and (ByteType(Text, J) <> mbTrailByte) do
        begin
          C := C + Text[J];
          Inc(J);
        end;
        C := C + Text[J];
    {$ifdef debug}
    {SendDebug('IPaint.Text=' + Text);
    SendDebug('IPaint.Char=' + C);
    SendDebug('IPaint.ByteType='+ IntToStr(Integer(ByteType(Text, J))));
    SendDebug('IPaint.mbTrail='+ IntToStr(Integer(mbTrailByte)));
    SendDebug('IPaint.mbLead='+ IntToStr(Integer(mbLeadByte)));
    //}
    {$endif}
      end
      else
    {$endif}
      C := Text[I]
    end
    else
      C := PasswordChar;
    DrawText(aBitmap32, XPos, YPos, C, not HideSelection and (SelLength > 0) and
    (I > SelStart) and (I <= SelStart + SelLength));
    //if I = CursorPos then CursorXPos := XPos;
    Inc(XPos, aBitmap32.TextWidth(C));
    if XPos >= RightBorder then Break;
    //Minor Changed by Riceball.
    Inc(I, Length(C));
  end;
  //Added by Riceball.
  if CursorPos > 0 then
  begin
    j := CursorPos;
    C := Copy(FText, FFirstDisplayChar, J - FFirstDisplayChar+1);
    FCursorXPos := FCursorXPos + aBitmap32.TextWidth(C);
    {$ifdef debug}
    //SendDebug('IPaint.Text=' + Text);
    SendDebug('IPaint.Char=' + C);
    SendDebug('IPaint.CursorPos='+ IntToStr(J));
    //SendDebug('IPaint.ByteType='+ IntToStr(Integer(ByteType(Text, J))));
    //SendDebug('IPaint.mbTrail='+ IntToStr(Integer(mbTrailByte)));
    //SendDebug('IPaint.mbLead='+ IntToStr(Integer(mbLeadByte)));
    //}
    {$endif}
    {C := '';
  
    if ByteType(Text, J) = mbLeadByte then
    repeat
      C := C + Text[J];
      Inc(J);
    until (J > Length(Text)) or (ByteType(Text, J) = mbTrailByte);
    C := C + Text[J];//}
    FCursorXPos := FCursorXPos {+ aBitmap32.TextWidth(C)} - 1;
  end;
  
  if Focused and FCursorShowing then
    if (Text = '') or (CursorPos = 0) then
      DrawCursor(aBitmap32, FCursorXPos, 0)
    else if PasswordChar = #0 then
      DrawCursor(aBitmap32, FCursorXPos, -1{CursorPos})
    else
      DrawCursor(aBitmap32, FCursorXPos, -1);
  //aBitmap32.ResetAlpha;
  //if Assigned(OnPaint) then OnPaint(Self);
  Result := inherited InternalPaintBuffer(aBitmap32);
end;

procedure TGRAbstractEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  OrigCursorPos: Integer;
begin
  inherited;
  
    {$ifdef Debug}
    //SendDebug('Cur:'+IntToStr(CursorPos));
    {$endif}
  
  case Key of
    VK_Home: CursorPos := 0;
    VK_Left: if CursorPos > 0 then
      begin
        repeat
          OrigCursorPos := CursorPos;
          {$IFDEF xMBCSSUPPORT}
          if ByteType(FText, FCursorPos) = mbTrailByte then
          repeat
            Dec(FCursorPos);
          until ByteType(FText, FCursorPos) = mbLeadByte;
          {$ENDIF}
          CursorPos := FCursorPos - 1;
        until (CursorPos = OrigCursorPos) or not (ssCTRL in Shift) or
          (FText[CursorPos] = ' ');
        {if (ssShift in Shift) and (OrigCursorPos > CursorPos) then
        begin
          //SelStart := CursorPos;
          SelLength := OrigCursorPos - CursorPos + 2;
        end; //}
      end;
  
    VK_Right: if CursorPos < Length(FText) then
      begin
        repeat
          OrigCursorPos := CursorPos;
          {$IFDEF xMBCSSUPPORT}
          if ByteType(FText, CursorPos + 1) = mbLeadByte then
          begin
            CursorPos := CursorPos + 1;
            while (CursorPos < Length(FText))
              and (ByteType(FText, CursorPos + 1) <> mbTrailByte) do
              CursorPos := CursorPos + 1
          end;
          {$ENDIF}
            CursorPos := CursorPos + 1;
        until (CursorPos = OrigCursorPos) or not (ssCTRL in Shift) or
          (FText[CursorPos] = ' ');
  
        {if (ssShift in Shift) and (CursorPos > OrigCursorPos) then
        begin
          //SelStart := OrigCursorPos;
          SelLength := CursorPos - OrigCursorPos;
        end; //}
      end;
  
    VK_End: CursorPos := Length(Text);
  
    VK_Back:
      if not ReadOnly and (CursorPos > 0) then
      begin
        if SelLength > 0 then
          SelText := ''
        else
        begin
          {$IFDEF xMBCSSUPPORT}
          OrigCursorPos := CursorPos;
          if ByteType(FText, OrigCursorPos) = mbTrailByte then
          begin
            repeat
              Dec(OrigCursorPos);
            until ByteType(FText, OrigCursorPos) = mbLeadByte;
            Dec(OrigCursorPos);
            Delete(FText, CursorPos - 1, CursorPos-OrigCursorPos);
            CursorPos := OrigCursorPos;
          end
          else
          {$ENDIF}
          begin
            {$IFDEF MBCSSUPPORT}
            OrigCursorPos := CursorPos;
            CursorPos := CursorPos - 1;
            if OrigCursorPos - CursorPos >= 2 then
              Delete(FText, OrigCursorPos-1, OrigCursorPos - CursorPos)
            else
              Delete(FText, OrigCursorPos, 1);
            {$else}
            Delete(FText, CursorPos, 1);
            CursorPos := CursorPos - 1;
            {$ENDIF}
          end;
          Change;
        end;
      end;
  
    VK_Delete:
      if not ReadOnly then
      begin
        if ssCTRL in Shift then
          CutToClipboard
        else
        if SelLength > 0 then
          SelText := ''
        else
        if CursorPos < Length(Text) then
        begin
          {$IFDEF MBCSSUPPORT}
          OrigCursorPos := CursorPos + 1;
          if ByteType(FText, OrigCursorPos) = mbLeadByte then
          begin
            repeat
              //Delete(FText, CursorPos + 1, 2)
              Inc(OrigCursorPos);
            until ByteType(FText, OrigCursorPos) = mbTrailByte;
            //Inc(OrigCursorPos);
            Delete(FText, CursorPos + 1, OrigCursorPos - CursorPos);
          end
          else
          {$ENDIF}
            Delete(FText, CursorPos + 1, 1);
          Change;
        end;
      end;
  
    VK_Insert: if ssCTRL in Shift then PasteFromClipboard;
  
    Ord('x'), Ord('X'): if ssCTRL in Shift then CutToClipboard;
    Ord('c'), Ord('C'): if ssCTRL in Shift then CopyToClipboard;
    Ord('v'), Ord('V'): if ssCTRL in Shift then PasteFromClipboard;
  end;
end;

procedure TGRAbstractEdit.KeyPress(var Key: Char);
begin
  if ReadOnly then Exit;
  if Key < #32 then Exit;
  if (MaxLength > 0) and (Length(Text) = MaxLength) then Exit;
  case CharCase of
    ecNormal: SelText := Key;
    ecUpperCase: SelText := Upcase(Key);
    ecLowerCase: SelText := LowerCase(Key)[1];
  end;
  Change;
  inherited;
end;

procedure TGRAbstractEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
end;

procedure TGRAbstractEdit.MenuEvent(Sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    cMenuUndo: Undo;
    cMenuCut: CutToClipboard;
    cMenuCopy: CopyToClipboard;
    cMenuPaste: PasteFromClipboard;
    cMenuDelete: ClearSelection;
    cMenuSelectAll:
      begin
        SelStart := 0;
        SelLength := Length(FText);
      end;
  end;
end;

procedure TGRAbstractEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  SetFocus;
  if Button = mbLeft then
  begin
    {$ifdef MBCSSUPPORT}
    X := HitTest(X, Y).X;
    if ByteType(FText, X + 1) = mbTrailByte then
      CursorPos := X + 1
    else
      CursorPos := X;
    {$else}
    CursorPos := HitTest(X, Y).X;
    {$endif}
    SelStart := CursorPos;
    SelLength := 0;
  end;
end;

procedure TGRAbstractEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
  {$ifdef MBCSSUPPORT}
  begin
    X := HitTest(X, Y).X;
    if ByteType(FText, X + 1) = mbTrailByte then
      CursorPos := X + 1
    else
      CursorPos := X;
  end;
  {$else}
  CursorPos := HitTest(X, Y).X;
  {$endif}
end;

procedure TGRAbstractEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

procedure TGRAbstractEdit.PasteFromClipboard;
begin
  SelText := Clipboard.AsText;
end;

procedure TGRAbstractEdit.SelectAll;
begin
  SelStart := 0;
  SelLength := Length(FText);
end;

procedure TGRAbstractEdit.SetCharCase(const Value: TEditCharCase);
begin
  FCharCase := Value;
end;

procedure TGRAbstractEdit.SetCursorPos(Value: Integer);
var
  OldCursorPos, Distance: Integer;
begin
  if Value < 0 then Value := 0;
  if Value > Length(Text) then Value := Length(Text);
  FCursorTimer.Enabled := False;
  FCursorShowing := True;
  FCursorTimer.Enabled := True;
  
  OldCursorPos := CursorPos;
  FCursorPos := Value;
  
  {$IFDEF MBCSSUPPORT}
  //Value := OldCursorPos;
  if OldCursorPos > FCursorPos then
  begin
    //move to left
    if ByteType(FText, FCursorPos) = mbLeadByte then
    begin
      repeat
        Dec(FCursorPos);
      until (ByteType(FText, FCursorPos) <> mbLeadByte);
      //Dec(FCursorPos);
    end;
  end
  else begin
    {$ifdef debug}
    //senddebug(FText+' SetCurPos:'+IntToStr(Integer(ByteType(FText, FCursorPos))));
    {$endif}
    //move to right
    if ByteType(FText, FCursorPos) = mbLeadByte then
    begin
      repeat
        Inc(FCursorPos);
      until ByteType(FText, FCursorPos) = mbTrailByte;
    end;
    //}
  end;
  {$endif}
  
  if not (ssShift in ShiftState) and not (mbLeft in MouseButtons) then
  begin
    FSelPoint1 := FCursorPos;
    FSelPoint2 := FCursorPos;
    SelStart := FCursorPos;
  end
  else
  begin
    Distance := FCursorPos - OldCursorPos;
    if OldCursorPos = FSelPoint2 then
      Inc(FSelPoint2, Distance)
    else
      Inc(FSelPoint1, Distance);
    {$ifdef Debug}
    //SendDebug('FSelPoint2:'+ IntToStr(FSelPoint2));
    //SendDebug('FSelPoint1:'+ IntToStr(FSelPoint1));
    {$endif}
  end;
  
  CalcFirstDisplayChar;
  InvalidateSelfBuffer;
end;

procedure TGRAbstractEdit.SetHideSelection(const Value: Boolean);
begin
  FHideSelection := Value;
end;

procedure TGRAbstractEdit.SetMaxLength(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FMaxLength := Value;
  if Length(Text) > Value then Text := Copy(Text, 1, Value);
end;

procedure TGRAbstractEdit.SetModified(const Value: Boolean);
begin
end;

procedure TGRAbstractEdit.SetPasswordChar(const Value: Char);
begin
  if FPasswordChar <> Value then
  begin
    FPasswordChar := Value;
    InvalidateSelfBuffer;
  end;
end;

procedure TGRAbstractEdit.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TGRAbstractEdit.SetSelLength(Value: Integer);
begin
  if ReadOnly then Exit;
  if FSelPoint2 > FSelPoint1 then
    FSelPoint2 := FSelPoint1 + Value
  else
    FSelPoint1 := FSelPoint2 + Value;
  if SelLength + SelStart > Length(Text) then
    SelLength := Length(Text) - SelStart;
  if SelLength < 0 then SelLength := 0;
  InvalidateSelfBuffer;
end;

procedure TGRAbstractEdit.SetSelStart(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value > Length(Text) then Value := Length(Text);
  FSelPoint1 := Value;
  FSelPoint2 := Value;
  Invalidate;
end;

procedure TGRAbstractEdit.SetSelText(const Value: string);
begin
  if ReadOnly then Exit;
  if SelLength = 0 then
  begin
    Insert(Value, FText, CursorPos + 1);
    CursorPos := SelStart + Length(Value);
  end
  else
  begin
    Delete(FText, SelStart + 1, SelLength);
    Insert(Value, FText, SelStart + 1);
    CursorPos := SelStart + Length(Value);
  end;
  FSelPoint1 := CursorPos;
  FSelPoint2 := FSelPoint1;
  if MaxLength > 0 then FText := Copy(FText, 1, MaxLength);
  Change;
    {$ifdef Debug}
    //SendDebug(FText);
    {$endif}
end;

procedure TGRAbstractEdit.SetSelTextBuf(Buffer: PChar);
begin
end;

procedure TGRAbstractEdit.SetText(Value: string);
begin
  if MaxLength > 0 then
    FText := Copy(Value, 1, MaxLength)
  else
    FText := Value;
  SelLength := 0;
  SelStart := 0;
  CursorPos := Length(Text);
  CalcFirstDisplayChar;
  Change;
  Invalidate;
end;

procedure TGRAbstractEdit.Undo;
begin
end;

procedure TGRAbstractEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := dlgc_WantArrows or DLGC_WANTCHARS;
end;

procedure TGRAbstractEdit.WMImeComposition(var Msg: TMessage);
  
  var
    IMC: HIMC;
    Buff: WideString;
    i: integer;
  
begin
  if Msg.lParam and GCS_RESULTSTR <> 0 then
  begin
    IMC := ImmGetContext(Handle);
    if IMC <> 0 then
    begin
      try
        { Get the result string }
        SetLength(Buff, ImmGetCompositionStringW(IMC, GCS_RESULTSTR, nil, 0) div SizeOf(WideChar));
        ImmGetCompositionStringW(IMC, GCS_RESULTSTR, PWideChar(Buff), Length(Buff) * SizeOf(WideChar));
      finally
        ImmReleaseContext(Handle, IMC);
      end;
  
      //for i := 1 to length(Buff) do
        SelText :=Buff;
  
      Msg.Result := 0;
      Exit;
    end;
  end;
  
  inherited ;
end;

procedure TGRAbstractEdit.WMImeStartComposition(var Msg: TMessage);
  
  var
    IMC: HIMC;
    LogFont: TLogFont;
    CF: TCompositionForm;
  
begin
  inherited ;
  IMC := ImmGetContext(Handle);
  if IMC <> 0 then
  begin
    if Assigned(CaptionFont) then
    begin
      GetObject(CaptionFont.Handle, SizeOf(TLogFont), @LogFont);
      ImmSetCompositionFont(IMC, @LogFont);
    end;
  
    CF.dwStyle := CFS_RECT;
    CF.rcArea  := Rect(0, 0, Width, Height);
    CF.ptCurrentPos := Point(FCursorXPos, CF.rcArea.Top);
    ImmSetCompositionWindow(IMC, @CF);
    ImmReleaseContext(Handle, IMC);
  end;
end;

procedure TGRCustomEdit.DrawCursor(aImage: TBitmap32;  XPos: Integer;
  CurrentCharPos: Integer);
var
  Handled: Boolean;
  CharWidth, LineHeight: Integer;
  s: string;
begin
    {$ifdef Debug}
    //SendDebug('DCursor');
    {$endif}
  Handled := False;
  //if Assigned(OnDrawCursor) then OnDrawCursor(Self, XPos, CurrentChar, Handled);
  //if not Handled then
  with aImage do
    begin
      //Font.Assign(Self.Font);
      s := '';
      if CurrentCharPos > 0 then
      begin
        {$ifdef MBCSSUPPORT}
        if ByteType(Text, CurrentCharPos) = mbLeadByte then
        repeat
          s := s + Text[CurrentCharPos];
          Inc(CurrentCharPos);
        until (CurrentCharPos > Length(Text)) or (ByteType(Text, CurrentCharPos) = mbTrailByte);
        {$endif}
        s := s + Text[CurrentCharPos];
        CharWidth := TextWidth(s) - 1;
      end
      else
        CharWidth := 0;
      PenColor := Color32(Self.CaptionFont.Color);
      //PenColor := clWhite32;
      //PenStyle := psSolid;
      //Pen.Width := 2;
  
      LineHeight := GetTextHeight;
      CurrentCharPos := GetBottomBorderSize;
      if LineHeight > Height - CurrentCharPos - 1 then
        LineHeight := Height - CurrentCharPos - 1;
  
      CurrentCharPos := GetTopBorderSize;
      //VertLineS(0, 0, 12, clWhite32);
      //VertLineS(1, 0, 12, clWhite32);
      XPos := XPos + CharWidth;
      MoveTo(XPos, CurrentCharPos);
      LineToS(XPos, CurrentCharPos + LineHeight);
    end;
end;

procedure TGRCustomEdit.DrawText(aImage: TBitmap32; XPos, YPos: Integer; Value:
  string; Selected: Boolean);
var
  Handled: Boolean;
  LSize: TSize;
begin
  //Handled := False;
  //if Assigned(OnDrawText) then OnDrawText(Self, XPos, YPos, Value, Selected, Handled);
  
  //if not Handled then
  with aImage do
  begin
    if Selected then
    begin
      aImage.Font.Color := clHighlightText;
      LSize := aImage.TextExtent(Value);
      aImage.FillRectTS(XPos, YPos, XPos+LSize.cX, YPos+LSize.cY, Color32(clHighlight));
    end;
    CaptionFont.RenderText(aImage, XPos, YPos, Value);
    //}
    {//Font.Assign(Self.Font);
    if Selected then
    begin
      Canvas.Brush.Style := Graphics.bsSolid;
      Canvas.Brush.Color := clHighlight;
      Font.Color := clHighlightText;
    end
    else
      Canvas.Brush.Style := bsClear;
    //Font.Color := clblack32;
    Canvas.Font.Assign(Font);
    Canvas.TextOut(XPos, YPos, Value);
    //}
  end;
end;

function TGRCustomEdit.GetTextHeight: Integer;
var
  TxtWidth: Integer;
begin
  Result := 12;
  if Parent = nil then Exit;
  with TControlCanvas.Create do
    try
      Control := Parent;
      Font.Assign(Self.Font);
      Result := TextHeight('Wg');
      {if Assigned(OnMeasureText) then
      begin
        TxtWidth := TextWidth('G');
        OnMeasureText(Self, TxtWidth, Result);
      end;//}
    finally
      Free;
    end;
end;

function TGRCustomEdit.GetTextWidth(Index: Integer; Count: Integer = 1; Value:
  string = ''): Integer;
var
  TxtHeight: Integer;
  LChar: string;
begin
  Result := 0;
  if Parent = nil then Exit;
  if Value = '' then Value := Text;
  if Count > Length(Value) then Count := Length(Value);
  with TControlCanvas.Create do
    try
      Control := Parent;
      Font.Assign(Self.CaptionFont);
      {$ifdef MBCSSUPPORT}
      if ByteType(Value, Count) <> mbSingleByte then
      begin
        //LChar := '';
        while (Count <= Length(Value)) and (ByteType(Value, Count) <> mbTrailByte) do
        begin
          //LChar := LChar + Value[Index];
          Inc(Count);
        end;
        //LChar := LChar + Value[Index];
        //Result := TextWidth(LChar);
      end;
      //else
      {$endif}
      Value := Copy(Value, Index, Count);
      Result := TextWidth(Value);
      {if Assigned(OnMeasureText) then
      begin
        //Minor Changed by Riceball.
        TxtHeight := TextHeight('Wg');
        OnMeasureText(Self, Result, TxtHeight);
      end;//}
    finally
      Free;
    end;
end;

procedure TGRCustomEdit.Loaded;
begin
  inherited;
  CursorPos := 0;
end;


end.
