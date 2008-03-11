unit GR_JvInspector;

interface

uses
  Windows, SysUtils, Classes, Graphics, Dialogs, ExtDlgs, Forms
  , GR32
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

procedure RegisterInspectorItems;


implementation

uses
	JvFullColorDialogs;

procedure RegisterInspectorItems;
begin
	TInspectorPictureItem.RegisterAsDefaultItem;
	TInspectorBitmap32Item.RegisterAsDefaultItem;
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
//	TJvInspectorMultiPropData.ItemRegister.Add(
//		TJvInspectorTypeInfoRegItem.Create(
//			TInspectorPictureItem, TypeInfo(TPicture)));
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

procedure TInspectorBitmap32Item.SetFlags(const Value: TInspectorItemFlags);
var
	NewValue: TInspectorItemFlags;
begin
	NewValue := Value + [ iifEditButton, iifEditFixed ];
	inherited SetFlags(NewValue);
end;

{ TInspectorColorItemEx }

var
	SharedColorDialog: TColorDialog;

	function ColorDialog: TColorDialog;
	begin
		if SharedColorDialog = nil then
		begin
			//	TJvFullColorDialog.Create(Application)
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


initialization
  RegisterInspectorItems;

finalization

end.

