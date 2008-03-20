unit unit_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SFDS, ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var W : TSFDSFileWriter;
    MetaData : TSFDSFileExtraInfoList;
    Item : TSFDSFileExtraInfoField;
    I, CSize : Integer;
    F : TFileStream;
    S : String;
    R : TSFDSFileReader;
begin
ProgressBar1.Position := 0;
MetaData := TSFDSFileExtraInfoList.Create();
  for I := 0 to 1000 do //String Metadata
    begin
      Item.Name := 'StringName #' + IntToStr(I);
      Item.FieldType := EIStr;
      String2Pointer('StringValue #' + IntToStr(I), Item.ValueString);
      Item.ValueStringSize := Length('StringValue #' + IntToStr(I));
      ProgressBar1.Position := ProgressBar1.Position + 1;
      MetaData.AddOrUpdateField(Item);
      FreeMem(Item.ValueString, Item.ValueStringSize);
    end;
  for I := 0 to 1000 do //Number Metadata
    begin
      Item.Name := 'NumberName #' + IntToStr(I);
      Item.FieldType := EINumber;
      Item.ValueNumber := I;
      ProgressBar1.Position := ProgressBar1.Position + 1;
      MetaData.AddOrUpdateField(Item);
    end;
  for I := 0 to 1000 do //Date Metadata
    begin
      Item.Name := 'DateName #' + IntToStr(I);
      Item.FieldType := EIDate;
      Item.ValueDate := Now;
      ProgressBar1.Position := ProgressBar1.Position + 1;
      MetaData.AddOrUpdateField(Item);
    end;
  for I := 0 to 1000 do //Boolean Metadata
    begin
      Item.Name := 'BooleanName #' + IntToStr(I);
      Item.FieldType := EIBoolean;
      Item.ValueBoolean := Boolean(Random(2));
      ProgressBar1.Position := ProgressBar1.Position + 1;
      MetaData.AddOrUpdateField(Item);
    end;
  F := TFileStream.Create('SFDS.PNG', fmOpenRead or fmShareDenyWrite);
  CSize := F.Size;
  SetLength(S, CSize);
  F.Read(PChar(S)^, CSize);
  for I := 0 to 1000 do //Binary Metadata
    begin
      Item.Name := 'BinaryName #' + IntToStr(I);
      Item.FieldType := EIBinary;
      String2Pointer(S, Item.ValueBinary);
      Item.ValueBinarySize := CSize;
      ProgressBar1.Position := ProgressBar1.Position + 1;
      MetaData.AddOrUpdateField(Item);
      FreeMem(Item.ValueBinary, Item.ValueBinarySize);
    end;
  F.Free;  
  W := TSFDSFileWriter.Create('TEST.sfds', False, aSfxNone, nil, '', '', '', 0, '', 0, '', '', MetaData);
MetaData.Free;
  W.Free;
  R := TSFDSFileReader.Create('TEST.sfds', 0);
  Application.MessageBox('File Loaded Click Ok to unload.', 'Loaded', MB_OK);
  R.Free;
end;

end.
