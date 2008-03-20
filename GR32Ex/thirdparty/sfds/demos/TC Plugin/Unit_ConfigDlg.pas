unit Unit_ConfigDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TConfigDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    ComboInt: TComboBox;
    Label2: TLabel;
    ComboFile: TComboBox;
    Label3: TLabel;
    ComboLevel: TComboBox;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    Title: TEdit;
    Dir: TEdit;
    Label5: TLabel;
    SFX: TComboBox;
    Label6: TLabel;
    ComboStrategy: TComboBox;
    procedure SFXChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Update__;
    { Public declarations }
  end;

implementation
uses TcPlgFuncs, SFDS;

{$R *.dfm}

procedure TConfigDlg.Update__;
var I : Integer;
begin
  ComboInt.Clear;
  ComboFile.Clear;
  ComboInt.Items.Add('Store');
  ComboFile.Items.Add('Store');
  for I := 0 to SFDS_CompressionFormatCount - 1 do
      begin
       ComboInt.Items.Add(SFDS_CompressionFormat(I)^.FormatName);
       ComboFile.Items.Add(SFDS_CompressionFormat(I)^.FormatName);
      end;
  ComboInt.ItemIndex := ComboInt.Items.IndexOf(InternalFileListCompressionFormat);
  if ComboInt.ItemIndex = -1 then ComboInt.ItemIndex := 0;
  ComboFile.ItemIndex := ComboFile.Items.IndexOf(CompFormat);
  if ComboFile.ItemIndex = -1 then ComboFile.ItemIndex := 0;
  ComboLevel.ItemIndex := Integer(CompLevel);
  ComboStrategy.ItemIndex := Integer(CompressionStrategy);
  Title.Text := Title_;
  Dir.Text := Dir_;
end;

procedure TConfigDlg.SFXChange(Sender: TObject);
begin
  Label4.Enabled := SFX.ItemIndex > 0;
  Label5.Enabled := SFX.ItemIndex > 0;
  Title.Enabled := SFX.ItemIndex > 0;
  Dir.Enabled := SFX.ItemIndex > 0;
end;

end.
