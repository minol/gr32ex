unit GR_JvInspector;

interface

uses
  Windows, SysUtils, Classes, Graphics, Dialogs, ExtDlgs, Forms
  , Controls //MouseType
  , GR32
  , GR32_Resamplers
  , GR_BitmapEx
  , JvInspector, TypInfo
  , JclRTTI
  , JvInspExtraEditors
  ;

type
  TInspectorPictureItem = class(TJvInspectorClassItem)
    class procedure RegisterAsDefaultItem;
    //class procedure UnregisterAsDefaultItem;
  protected
    FFilename: string;
    function ExecutePictureDialog: Boolean;
    procedure Edit; override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  end;

  TInspectorBitmap32Item = class(TJvInspectorClassItem)
    class procedure RegisterAsDefaultItem;
    //class procedure UnregisterAsDefaultItem;
  protected
    FFilename: string;
    function ExecutePictureDialog: Boolean;
    procedure Edit; override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  TInspectorBitmap32ExItem = class(TInspectorBitmap32Item)
    class procedure RegisterAsDefaultItem;
    //class procedure UnregisterAsDefaultItem;
  protected
    procedure Edit; override;
  end;

  TInspectorColorItemEx = class(TJvInspectorColorItem)
    class procedure RegisterAsDefaultItem;
  private
    function GetPropColor: TColor;
    procedure SetPropColor(const Value: TColor);
    //class procedure UnregisterAsDefaultItem;
  protected
    FDialogColor: TColor;
    function ExecuteColorDialog: Boolean;
    procedure Edit; override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
    property PropColor: TColor read GetPropColor write SetPropColor;
  end;

  TJvTMethodEditEvent = procedure(const Sender: TObject) of object;

  TJvInspectorTMethodItemEx = class(TJvInspectorTMethodItem)
    class procedure RegisterAsDefaultItem;
    //class procedure UnregisterAsDefaultItem;
  protected
    FOnEdit: TJvTMethodEditEvent;

    procedure ExecuteTMethodEdit; virtual;

    procedure Edit; override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  public
    property OnEdit: TJvTMethodEditEvent read FOnEdit write FOnEdit;
  end;

  //For TGRResamplerClassName Resampler Class Name in Bitmap32Ex
  TJvInspectorResamplerClassNameItem = class(TJvCustomInspectorItem)
    class procedure RegisterAsDefaultItem;
  protected
    function GetDisplayValue: string; override;
    procedure GetValueList(const aStrings: TStrings); override;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  end;


  {
  TInspectorCursorItem = class(TJvInspectorEnumItem)
    class procedure RegisterAsDefaultItem;
  protected
    function GetDisplayValue: string; override;
    procedure GetValueList(const Strings: TStrings); override;
    procedure SetDisplayValue(const Value: string); override;
  end; //}

procedure RegisterInspectorItems;


implementation

uses
  JvFullColorDialogs;

procedure RegisterInspectorItems;
begin
  TInspectorPictureItem.RegisterAsDefaultItem;
  TInspectorBitmap32Item.RegisterAsDefaultItem;
  TInspectorBitmap32ExItem.RegisterAsDefaultItem;
  TInspectorColorItemEx.RegisterAsDefaultItem;

  TJvInspectorAlignItem.RegisterAsDefaultItem;
  TJvInspectorAnchorsItem.RegisterAsDefaultItem;
  //TJvInspectorColorItem.RegisterAsDefaultItem;
  TJvInspectorTImageIndexItem.RegisterAsDefaultItem;
end;

{ TInspectorPictureItem }

class procedure TInspectorPictureItem.RegisterAsDefaultItem;
begin
  TJvCustomInspectorData.ItemRegister.Add(
    TJvInspectorTypeInfoRegItem.Create(
      TInspectorPictureItem, TypeInfo(TPicture)));
//  TJvInspectorMultiPropData.ItemRegister.Add(
//    TJvInspectorTypeInfoRegItem.Create(
//      TInspectorPictureItem, TypeInfo(TPicture)));
end;

function TInspectorPictureItem.ExecutePictureDialog: Boolean;
begin
  with TOpenPictureDialog.Create(GetParentForm(Inspector)) do
    try
      Result := Execute;
      FFilename := Filename;
    finally
      Free;
    end;
end;

procedure TInspectorPictureItem.Edit;
begin
  if ExecutePictureDialog then
    TPicture(Data.AsOrdinal).LoadFromFile(FFilename);
end;

procedure TInspectorPictureItem.SetFlags(const Value: TInspectorItemFlags);
var
  NewValue: TInspectorItemFlags;
begin
  NewValue := Value + [ iifEditButton, iifEditFixed ];
  inherited SetFlags(NewValue);
end;

{ TInspectorBitmap32Item }

class procedure TInspectorBitmap32Item.RegisterAsDefaultItem;
begin
  TJvCustomInspectorData.ItemRegister.Add(
    TJvInspectorTypeInfoRegItem.Create(
      TInspectorBitmap32Item, TypeInfo(TBitmap32)));
end;

function TInspectorBitmap32Item.ExecutePictureDialog: Boolean;
begin
  with TOpenPictureDialog.Create(GetParentForm(Inspector)) do
    try
      Result := Execute;
      FFilename := Filename;
    finally
      Free;
    end;
end;

procedure TInspectorBitmap32Item.Edit;
begin
  if ExecutePictureDialog then
    TBitmap32(Data.AsOrdinal).LoadFromFile(FFilename);
end;

procedure TInspectorBitmap32Item.EditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (ssDouble in Shift) then
    Edit;
end;

procedure TInspectorBitmap32Item.SetFlags(const Value: TInspectorItemFlags);
var
  NewValue: TInspectorItemFlags;
begin
  NewValue := Value + [ iifEditButton, iifEditFixed ];
  inherited SetFlags(NewValue);
end;

{ TInspectorBitmap32ExItem }
class procedure TInspectorBitmap32ExItem.RegisterAsDefaultItem;
begin
  TJvCustomInspectorData.ItemRegister.Add(
    TJvInspectorTypeInfoRegItem.Create(
      TInspectorBitmap32ExItem, TypeInfo(TBitmap32Ex)));
end;

procedure TInspectorBitmap32ExItem.Edit;
begin
  if ExecutePictureDialog then
    TBitmap32Ex(Data.AsOrdinal).LoadFromFile(FFilename);
end;


{ TInspectorColorItemEx }

var
  SharedColorDialog: TColorDialog;

  function ColorDialog: TColorDialog;
  begin
    if SharedColorDialog = nil then
    begin
      //  TJvFullColorDialog.Create(Application)
      SharedColorDialog := TColorDialog.Create(Application);
      SharedColorDialog.Options := [ cdFullOpen, cdAnyColor ];
    end;
    Result := SharedColorDialog;
  end;

class procedure TInspectorColorItemEx.RegisterAsDefaultItem;
begin
  TJvCustomInspectorData.ItemRegister.Add(
    TJvInspectorTypeInfoRegItem.Create(
      TInspectorColorItemEx, TypeInfo(TColor)));
end;

function TInspectorColorItemEx.GetPropColor: TColor;
begin
  Result := Data.AsOrdinal;
end;

procedure TInspectorColorItemEx.SetPropColor(const Value: TColor);
begin
  Data.AsOrdinal := Value;
end;

function TInspectorColorItemEx.ExecuteColorDialog: Boolean;
begin
  with ColorDialog do
  begin
    Color := PropColor;
    Result := Execute;
    if Result then
      PropColor := Color;
  end;
end;

procedure TInspectorColorItemEx.Edit;
begin
  ExecuteColorDialog;
end;

procedure TInspectorColorItemEx.SetFlags(const Value: TInspectorItemFlags);
var
  NewValue: TInspectorItemFlags;
begin
  NewValue := Value + [ iifEditButton, iifEditFixed ];
  inherited SetFlags(NewValue);
end;

{ TJvInspectorTMethodItemEx }
procedure TJvInspectorTMethodItemEx.ExecuteTMethodEdit;
begin
  if Assigned(FOnEdit) then 
    FOnEdit(Self);
end;

procedure TJvInspectorTMethodItemEx.Edit;
begin
  ExecuteTMethodEdit;
end;

procedure TJvInspectorTMethodItemEx.SetFlags(const Value: TInspectorItemFlags);
var
  NewValue: TInspectorItemFlags;
begin
  NewValue := Value + [iifEditButton];
  inherited SetFlags(NewValue);
end;

class procedure TJvInspectorTMethodItemEx.RegisterAsDefaultItem;
begin
  TJvCustomInspectorData.ItemRegister.Add(
    TJvInspectorTypeKindRegItem.Create(
      TJvInspectorTMethodItemEx, tkMethod));
end;

{ TJvInspectorResamplerClassNameItem }
class procedure TJvInspectorResamplerClassNameItem.RegisterAsDefaultItem;
begin
  TJvCustomInspectorData.ItemRegister.Add(
    TJvInspectorTypeInfoRegItem.Create(
      TJvInspectorResamplerClassNameItem, TypeInfo(TGRResamplerClassName)));
end;

function TJvInspectorResamplerClassNameItem.GetDisplayValue: string;
begin
  Result := Data.AsString;
end;

procedure TJvInspectorResamplerClassNameItem.GetValueList(const aStrings: TStrings);
var
  i: Integer;
begin
  if (Data is TJvInspectorPropData) then
  begin
    aStrings.Clear;
    if Assigned(ResamplerList) then
    with ResamplerList do
      for i := 0 to Count - 1 do
      begin
        if Assigned(Items[i]) then
          aStrings.Add(Items[i].ClassName);
      end;
  end;
end;

procedure TJvInspectorResamplerClassNameItem.SetDisplayValue(const Value: string);
begin
  Data.AsString := Value;
end;

procedure TJvInspectorResamplerClassNameItem.SetFlags(const Value: TInspectorItemFlags);
var
  NewValue: TInspectorItemFlags;
begin
  NewValue := Value + [iifValueList, iifAutoUpdate];
  inherited SetFlags(NewValue);
end;

initialization
  RegisterInspectorItems;

finalization

end.

