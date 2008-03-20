object AddFileDlg: TAddFileDlg
  Left = 246
  Top = 247
  BorderStyle = bsDialog
  Caption = 'Add File...'
  ClientHeight = 214
  ClientWidth = 429
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
    Left = 16
    Top = 16
    Width = 56
    Height = 13
    Caption = 'Source File:'
  end
  object Label2: TLabel
    Left = 16
    Top = 64
    Width = 67
    Height = 13
    Caption = 'Stream Name:'
  end
  object FileAtrr: TLabel
    Left = 16
    Top = 112
    Width = 66
    Height = 13
    Caption = 'File Attributes:'
  end
  object Label3: TLabel
    Left = 112
    Top = 112
    Width = 63
    Height = 13
    Caption = 'ExtraDataStr:'
  end
  object Label4: TLabel
    Left = 344
    Top = 112
    Width = 62
    Height = 13
    Caption = 'ExtraDataInt:'
  end
  object label6: TLabel
    Left = 16
    Top = 160
    Width = 98
    Height = 13
    Caption = 'Compression Format:'
  end
  object Label5: TLabel
    Left = 136
    Top = 160
    Width = 90
    Height = 13
    Caption = 'Compression Type:'
  end
  object SrcFile: TEdit
    Left = 16
    Top = 32
    Width = 393
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
    Text = 'SrcFile'
  end
  object StrName: TEdit
    Left = 16
    Top = 80
    Width = 393
    Height = 21
    TabOrder = 1
    Text = '<default>'
  end
  object FileAttr: TEdit
    Left = 16
    Top = 128
    Width = 65
    Height = 21
    TabOrder = 2
    Text = '0'
  end
  object ExtraDataStr: TEdit
    Left = 112
    Top = 128
    Width = 217
    Height = 21
    TabOrder = 3
  end
  object ExtraDataInt: TEdit
    Left = 344
    Top = 128
    Width = 65
    Height = 21
    TabOrder = 4
    Text = '0'
  end
  object CompressionFormatCombo: TComboBox
    Left = 16
    Top = 176
    Width = 97
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 1
    TabOrder = 5
    Text = 'ZLib (Deflate)'
    Items.Strings = (
      'None'
      'ZLib (Deflate)'
      'BZip2')
  end
  object CompressionTypeCombo: TComboBox
    Left = 136
    Top = 176
    Width = 97
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 2
    TabOrder = 6
    Text = 'Max'
    Items.Strings = (
      'Fastest'
      'Default'
      'Max')
  end
  object ButtonAdd: TButton
    Left = 312
    Top = 176
    Width = 99
    Height = 22
    Caption = 'Add File'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
end
