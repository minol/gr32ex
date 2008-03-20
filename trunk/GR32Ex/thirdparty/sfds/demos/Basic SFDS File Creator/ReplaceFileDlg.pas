unit ReplaceFileDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormReplace = class(TForm)
    Label1: TLabel;
    EditF: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormReplace: TFormReplace;

implementation
uses BSFCForm;

{$R *.dfm}

procedure TFormReplace.Button1Click(Sender: TObject);
begin
  ReplaceFileDlgResult := ID_YES;
end;

procedure TFormReplace.Button2Click(Sender: TObject);
begin
  ReplaceFileDlgResult := ID_NO;
end;

procedure TFormReplace.Button3Click(Sender: TObject);
begin
  ReplaceFileDlgResult := ID_YESTOALL;
end;

procedure TFormReplace.Button4Click(Sender: TObject);
begin
  ReplaceFileDlgResult := ID_NOTOALL;
end;

end.
