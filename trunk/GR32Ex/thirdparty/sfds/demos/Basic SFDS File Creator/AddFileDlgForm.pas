unit AddFileDlgForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TAddFileDlg = class(TForm)
    Label1: TLabel;
    SrcFile: TEdit;
    Label2: TLabel;
    StrName: TEdit;
    FileAtrr: TLabel;
    FileAttr: TEdit;
    Label3: TLabel;
    ExtraDataStr: TEdit;
    Label4: TLabel;
    ExtraDataInt: TEdit;
    CompressionFormatCombo: TComboBox;
    label6: TLabel;
    Label5: TLabel;
    CompressionTypeCombo: TComboBox;
    ButtonAdd: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AddFileDlg: TAddFileDlg;

implementation

{$R *.dfm}

end.
