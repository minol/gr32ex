object TestForm: TTestForm
  Left = 192
  Top = 107
  Caption = 'TestForm'
  ClientHeight = 47
  ClientWidth = 323
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BtnTest: TButton
    Left = 16
    Top = 16
    Width = 89
    Height = 25
    Caption = 'Test'
    TabOrder = 0
    OnClick = BtnTestClick
  end
  object ProgressBar: TProgressBar
    Left = 120
    Top = 16
    Width = 185
    Height = 16
    TabOrder = 1
  end
end
