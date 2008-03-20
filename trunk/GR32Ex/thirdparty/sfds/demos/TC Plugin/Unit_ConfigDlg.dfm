object ConfigDlg: TConfigDlg
  Left = 211
  Top = 107
  BorderStyle = bsDialog
  Caption = 'SFDS Configuration'
  ClientHeight = 250
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 260
    Top = 215
    Width = 75
    Height = 22
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 340
    Top = 215
    Width = 75
    Height = 22
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 10
    Top = 16
    Width = 200
    Height = 220
    Caption = 'Compression'
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 163
      Height = 13
      Caption = 'Internal file list compression format:'
    end
    object Label2: TLabel
      Left = 16
      Top = 72
      Width = 113
      Height = 13
      Caption = 'File compression format:'
    end
    object Label3: TLabel
      Left = 16
      Top = 168
      Width = 106
      Height = 13
      Caption = 'File compression level:'
    end
    object Label6: TLabel
      Left = 16
      Top = 120
      Width = 105
      Height = 13
      Caption = 'Compression Strategy:'
    end
    object ComboInt: TComboBox
      Left = 16
      Top = 40
      Width = 169
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'None')
    end
    object ComboFile: TComboBox
      Left = 16
      Top = 88
      Width = 169
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      Items.Strings = (
        'None')
    end
    object ComboLevel: TComboBox
      Left = 16
      Top = 184
      Width = 169
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 2
      TabOrder = 2
      Text = 'Maximum'
      Items.Strings = (
        'Fastest'
        'Default'
        'Maximum')
    end
    object ComboStrategy: TComboBox
      Left = 16
      Top = 136
      Width = 169
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 3
      Text = 'Default'
      Items.Strings = (
        'Default'
        'Filtered'
        'Huffman'
        'RLE'
        'Fixed')
    end
  end
  object GroupBox2: TGroupBox
    Left = 224
    Top = 16
    Width = 225
    Height = 190
    Caption = 'SFX'
    TabOrder = 3
    object Label4: TLabel
      Left = 16
      Top = 48
      Width = 65
      Height = 13
      Caption = 'Window Title:'
      Enabled = False
    end
    object Label5: TLabel
      Left = 16
      Top = 88
      Width = 82
      Height = 13
      Caption = 'Default Directory:'
      Enabled = False
    end
    object Title: TEdit
      Left = 16
      Top = 64
      Width = 190
      Height = 21
      Enabled = False
      TabOrder = 0
    end
    object Dir: TEdit
      Left = 16
      Top = 104
      Width = 190
      Height = 21
      Enabled = False
      TabOrder = 1
    end
    object SFX: TComboBox
      Left = 16
      Top = 24
      Width = 190
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 2
      Text = 'None'
      OnChange = SFXChange
      Items.Strings = (
        'None'
        'SFX win32 (Method 1)'
        'SFX win32 (Method 2)')
    end
  end
end
