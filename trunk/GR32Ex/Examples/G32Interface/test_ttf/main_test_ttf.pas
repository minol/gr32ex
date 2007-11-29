unit main_test_ttf;
{$I G32i.inc}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, G32_Interface,
  StdCtrls, ExtCtrls, GR32_Transforms, Buttons, ComCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    FontDialog: TFontDialog;
    btnSelectFont: TButton;
    sbMain: TStatusBar;
    btnDrawMany: TButton;
    Edit1: TEdit;
    trackAngle: TTrackBar;
    lbAngle: TLabel;
    lbXSkew: TLabel;
    trackXSkew: TTrackBar;
    Label2: TLabel;
    trackYSkew: TTrackBar;
    lbYSkew: TLabel;
    lbXScale: TLabel;
    trackXScale: TTrackBar;
    lbYScale: TLabel;
    trackYScale: TTrackBar;
    GroupBox1: TGroupBox;
    cbAntialising: TCheckBox;
    cbFilling: TCheckBox;
    btnClearBitmap: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnSelectFontClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnDrawManyClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure cbAntialisingClick(Sender: TObject);
    procedure cbFillingClick(Sender: TObject);
    procedure trackAngleChange(Sender: TObject);
    procedure btnClearBitmapClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    fText : widestring;
    fBuffer : tBitmap32Ex;
    fDrawningOptions : tPolygonDrawOptions;
    fAT : TAffineTransformation;
    procedure RenderFittedText(fBuffer: tBitmap32Ex);
  public
    { Public declarations }
    procedure UpdateOptions;
    procedure UpdateTransform;
  end;

var
  fmMain: TfmMain;

implementation

uses GR32, GR32_Polygons, Math
{$IFDEF DEBUG_OPTIMIZATION}, Debug_LE {$ENDIF}
;

{$R *.DFM}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Self.Color := clBlack;
  fBuffer := tBitmap32Ex.Create;
  fBuffer.Font := FontDialog.Font;
  fText := Edit1.text;

  UpdateOptions;
  

  Application.HintPause := 100;
  fAT  := TAffineTransformation.Create;

  fBuffer.SetSize(Self.ClientWidth, Self.ClientHeight);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  fBuffer.Free;
  fAT.Free;
end;

procedure TfmMain.FormPaint(Sender: TObject);

begin
  
  fBuffer.BeginUpdate;
  fBuffer.FillRect(0, 0, fBuffer.Width, fBuffer.Height, clBlack32);

  fBuffer.Font.Color := clLime;
  { draw simple text }
  fBuffer.Textout(100, 40, fText);
  { draw antialised text with transormation }
  fBuffer.RenderTextExW(Fixed(100), Fixed(400), fText, clLime32, fDrawningOptions);
  //gRenderTextExW(fBuffer, fBuffer.Font.Handle, Fixed(100), Fixed(400), fText, clLime32, fDrawningOptions, fAT.Matrix );
  //gRenderTextEx(fBuffer, fBuffer.Font.Handle, Fixed(100), Fixed(400), fText, clLime32, fDrawningOptions, fAT.Matrix );
  { draw antialised text with transformation along curve }
  RenderFittedText(fBuffer);

  fBuffer.EndUpdate;
  { рисуем буфер }
  fBuffer.DrawTo(Canvas.Handle, 0, 0);
end;

procedure TfmMain.RenderFittedText(fBuffer: tBitmap32Ex);
var
  FP : tArrayOfFixedPoint;
begin
  { define curve }
  setLength(FP, 7);
  FP[0] := FixedPoint(100, 200);
  FP[1] := FixedPoint(200, 150);
  FP[2] := FixedPoint(300, 100);
  FP[3] := FixedPoint(400, 200);
  FP[4] := FixedPoint(500, 300);
  FP[5] := FixedPoint(600, 300);
  FP[6] := FixedPoint(700, 200);

  fBuffer.PolyBezier(FP, clGreen32, fDrawningOptions and pdoAntialising, false);
  fBuffer.RenderFittedTextW(fText, clRed32, fDrawningOptions, FP);
  FP := nil;
end;

procedure TfmMain.FormResize(Sender: TObject);
begin
  fBuffer.SetSize(Self.ClientWidth, Self.ClientHeight);
end;

procedure TfmMain.btnSelectFontClick(Sender: TObject);
begin
  if FontDialog.Execute then
    begin
    fBuffer.Font := FontDialog.Font;
    Repaint;
    end;
end;

procedure TfmMain.btnRefreshClick(Sender: TObject);
begin
  Repaint;
end;

procedure TfmMain.btnDrawManyClick(Sender: TObject);
const
  {$IFDEF DEBUG_OPTIMIZATION}
     N = 1000;
  {$ELSE}
     N = 100;
   {$ENDIF}
var

  i : integer;
begin


  fBuffer.FillRect(0, 0, fBuffer.Width, fBuffer.Height, clBlack32);
  Screen.Cursor := crHourGlass;
  {$IFDEF DEBUG_OPTIMIZATION} Secundomer.Start; {$ENDIF}
    for I := 0 to N do
      begin
      fBuffer.RenderTextEx(
        Fixed(Random(Width - 40)),
        Fixed(Random(Height - 40)),
        'line#' + IntToStr(Random(100)),
        Color32(Random(255), Random(255), Random(255), Random(255)),
        fDrawningOptions);
     end;
  {$IFDEF DEBUG_OPTIMIZATION}Secundomer.Stop; {$ENDIF}
  Screen.Cursor := crDefault;
  fBuffer.DrawTo(Canvas.Handle, 0, 0);
  {$IFDEF DEBUG_OPTIMIZATION} Secundomer.ShowResult; {$ENDIF}
end;

procedure TfmMain.Edit1Change(Sender: TObject);
begin
  if fText <> Edit1.text then
    begin
    fText := Edit1.text;
    FormPaint(Self);
    end;
end;

procedure TfmMain.UpdateOptions;
begin
  fDrawningOptions := 0;
  if cbAntialising.Checked then fDrawningOptions := pdoFloat;
  if cbFilling.Checked then fDrawningOptions := fDrawningOptions or pdoFilling;
end;

procedure TfmMain.cbAntialisingClick(Sender: TObject);
begin
  UpdateOptions;
  FormPaint(Self);
end;


procedure TfmMain.cbFillingClick(Sender: TObject);
begin
  UpdateOptions;
  FormPaint(Self);
end;

procedure TfmMain.trackAngleChange(Sender: TObject);
begin
  lbAngle.Caption := format('angle (%d)' ,[trackAngle.Position]);

  lbXSkew.Caption := format('xSkew (%8.2f)' ,[trackXSkew.Position/5]);
  lbYSkew.Caption := format('ySkew (%8.2f)' ,[trackYSkew.Position/5]);

  lbXScale.Caption := format('xScale (%8.2f)' ,[trackXScale.Position/5]);
  lbYScale.Caption := format('yScale (%8.2f)' ,[trackYScale.Position/5]);

  UpdateTransform;
  FormPaint(Self);
end;

procedure TfmMain.UpdateTransform;
var
  DegAngle : integer;
  xSkew, ySkew : single;
  xScale, yScale : single;
begin
   fAT.Clear;
   fAT.Matrix[1, 1] := -1; { потому что буквы рисуются перевёрнутыми }

   xScale := trackXScale.Position/5;
   yScale := trackyScale.Position/5;
   fAT.Scale(xScale, yScale);

   xSkew := trackXSkew.Position/5;
   ySkew := trackYSkew.Position/5;
   fAT.Skew(xSkew, ySkew);

   DegAngle := trackAngle.Position;
   fAT.Rotate(0,0, DegAngle);


   fBuffer.SelectFontTransform(fAT.Matrix);
end;

procedure TfmMain.btnClearBitmapClick(Sender: TObject);
begin
  fBuffer.FillRect(0, 0, fBuffer.Width, fBuffer.Height, clBlack32);
  fBuffer.DrawTo(Canvas.Handle, 0, 0);  
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  UpdateTransform;
end;

end.

