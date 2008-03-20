object FormReplace: TFormReplace
  Left = 211
  Top = 107
  BorderStyle = bsDialog
  Caption = 'File exists. Replace?'
  ClientHeight = 88
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 8
    Width = 47
    Height = 13
    Caption = 'FileName:'
  end
  object EditF: TEdit
    Left = 10
    Top = 24
    Width = 360
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
  end
  object Button1: TButton
    Left = 33
    Top = 56
    Width = 75
    Height = 22
    Caption = '&Yes'
    ModalResult = 6
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 113
    Top = 56
    Width = 75
    Height = 22
    Caption = '&No'
    Default = True
    ModalResult = 7
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 193
    Top = 56
    Width = 75
    Height = 22
    Caption = 'Yes To &All'
    ModalResult = 1
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 273
    Top = 56
    Width = 75
    Height = 22
    Caption = 'No &To All'
    ModalResult = 2
    TabOrder = 4
    OnClick = Button4Click
  end
end
