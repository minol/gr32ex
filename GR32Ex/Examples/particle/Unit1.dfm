object Form1: TForm1
  Left = 198
  Top = 114
  Width = 367
  Height = 242
  Caption = 'the Particle Animation Effects - Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage32
    Left = 176
    Top = 8
    Width = 153
    Height = 121
    align = alClient
  end
  object OpenPlay: TButton
    Left = 8
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Open - Play'
    TabOrder = 1
  end
  object Snapshot: TButton
    Left = 176
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Snapshot'
    TabOrder = 2
  end
  object CallBack: TCheckBox
    Left = 176
    Top = 168
    Width = 97
    Height = 17
    Caption = 'Callback Event'
    TabOrder = 3
  end
  object OpenDialog: TOpenDialog
    Left = 16
    Top = 80
  end
end
