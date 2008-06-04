
{
Summary the Image run-time test.
@author Riceball LEE(lixeuyu@snda.com)
@revision 0;1
}
unit TestForm;

{$I Setting.inc}

{$DEFINE Debug}

interface

uses
 {$IFDEF Debug}
 DbugIntf,
 {$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs
  , GR32_Image
  , GR_ImageEx
  , GR_Layers
  ;

type
  TfrmTest = class(TForm)
    procedure FormShow(Sender: TObject);
  private
  protected
    FImage: TImage32Ex;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure DoFormConstraint(Sender: TObject);
  public
    { Public declarations }
    constructor Create(aOwner: TComponent);override;
    property Image: TImage32Ex read FImage;
  end;

var
  frmTest: TfrmTest;

implementation

{$R *.dfm}

{ TfrmTest }
constructor TfrmTest.Create(aOwner: TComponent);
begin
  inherited;
  FImage := TImage32Ex.Create(Self);
  with FImage do
  begin
    Parent := Self;
    Align := alClient;
    OnBitmapResize := DoFormConstraint;
  end;
  OnCanResize := FormCanResize;
end;

procedure TfrmTest.DoFormConstraint(Sender: TObject);
var
  vScaledSize: TSize;
begin
  if not Image.Bitmap.Empty then
  begin
    vScaledSize := Image.GetScaledSize(ClientWidth, ClientHeight);
    ClientWidth := vScaledSize.cx;
    ClientHeight := vScaledSize.cy;
  end;
  {
  with Image.Bitmap do 
  if not Empty then
  begin
    vScaledSize := Image.GetBitmapSize();
    case Image.ScaleMode of
      smNormal:
      begin
        Constraints.MinWidth := Width;
        Constraints.MaxWidth := Width;
        Constraints.MinHeight := Height;
        Constraints.MaxHeight := Height;
      end;
      smScale:
      begin
        Constraints.MinWidth := vScaledSize.cx;
        Constraints.MaxWidth := vScaledSize.cx;
        Constraints.MinHeight := vScaledSize.cy;
        Constraints.MaxHeight := vScaledSize.cy;
      end;
      smOptimal:
      begin
        Constraints.MinWidth := 0;
        Constraints.MaxWidth := Width;
        Constraints.MinHeight := 0;
        Constraints.MaxHeight := Height;
      end;
      smOptimalScaled:
      begin
        Constraints.MinWidth := 0;
        Constraints.MaxWidth := vScaledSize.cx;
        Constraints.MinHeight := 0;
        Constraints.MaxHeight := vScaledSize.cy;
      end;
      //smStretch:
      //smOptimal, smOptimalScaled:
      else begin
        Constraints.MinWidth := 0;
        Constraints.MaxWidth := 0;
        Constraints.MinHeight := 0;
        Constraints.MaxHeight := 0;
      end;
    end; //case
  end; //if}
end;

procedure TfrmTest.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
var
  vScaledSize: TSize;
begin
  if not Image.Bitmap.Empty {and (Image.ScaleMode in [smResize, smOptimal, smOptimalScaled])} then
  begin
    vScaledSize := Image.GetScaledSize(NewWidth, NewHeight);
  {$IFDEF DEBUG}
   //SendDebug('CanResize:  vScaledSize.cx='+IntToStr(vScaledSize.cx)+' vScaledSize.cy='+ IntToStr(vScaledSize.cy)+', NewWidth='+IntToStr(NewWidth)+' NewHeight='+ IntToStr(NewHeight));
  {$ENDIF}
    NewWidth  := vScaledSize.cx;
    NewHeight := vScaledSize.cy;
  end; //if
  {$IFDEF DEBUG}
   //SendDebug('CanResize:  vScaledSize.cx='+IntToStr(vScaledSize.cx)+' vScaledSize.cy='+ IntToStr(vScaledSize.cy)+', NewWidth='+IntToStr(NewWidth)+' NewHeight='+ IntToStr(NewHeight));
   //OutputDebugString(PChar('CanResize:  vScaledSize.cx='+IntToStr(vScaledSize.cx)+' vScaledSize.cy='+ IntToStr(vScaledSize.cy)+', NewWidth='+IntToStr(NewWidth)+' NewHeight='+ IntToStr(NewHeight)));
   //writeln('Width='+IntToStr(NewWidth)+' Height='+ IntToStr(NewHeight));
  {$ENDIF}
end;

procedure TfrmTest.FormShow(Sender: TObject);
begin
  DoFormConstraint(nil);
end;

procedure TfrmTest.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_Escape then 
    Close
  else
    inherited;
end;

initialization
end.
