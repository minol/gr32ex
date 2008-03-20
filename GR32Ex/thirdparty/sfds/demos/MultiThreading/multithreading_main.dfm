object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'MultiThreading Safe'
  ClientHeight = 123
  ClientWidth = 461
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 188
    Top = 11
    Width = 94
    Height = 13
    Alignment = taRightJustify
    Caption = 'Number of threads:'
  end
  object Label2: TLabel
    Left = 8
    Top = 70
    Width = 445
    Height = 13
    Caption = '...'
    Constraints.MaxWidth = 445
    Constraints.MinWidth = 445
    WordWrap = True
  end
  object btnStart: TButton
    Left = 8
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Start MultiThreading Test'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object ComboBox1: TComboBox
    Left = 288
    Top = 8
    Width = 43
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 2
    TabOrder = 1
    Text = '3'
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9'
      '10')
  end
  object btnStop: TButton
    Left = 8
    Top = 39
    Width = 145
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 2
    OnClick = btnStopClick
  end
  object CheckBox1: TCheckBox
    Left = 187
    Top = 30
    Width = 270
    Height = 17
    Caption = 'SFDS Memory Stream Reader Test (else FileStream)'
    TabOrder = 3
  end
end
