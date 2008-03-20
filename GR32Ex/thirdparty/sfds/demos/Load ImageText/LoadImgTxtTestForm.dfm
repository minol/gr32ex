object Form1: TForm1
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Loading Test'
  ClientHeight = 233
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 8
    Top = 8
    Width = 300
    Height = 150
  end
  object LabelTxt: TLabel
    Left = 8
    Top = 168
    Width = 12
    Height = 13
    Caption = '***'
  end
  object Button1: TButton
    Left = 320
    Top = 8
    Width = 75
    Height = 22
    Caption = 'Load'
    TabOrder = 0
    OnClick = Button1Click
  end
end
