object FormSearchTest: TFormSearchTest
  Left = 244
  Top = 66
  Caption = 'Search Test'
  ClientHeight = 466
  ClientWidth = 542
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 550
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 542
    Height = 417
    Align = alClient
    TabOrder = 0
    DesignSize = (
      542
      417)
    object LabelInfo: TLabel
      Left = 8
      Top = 345
      Width = 9
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = '---'
      ExplicitTop = 344
    end
    object FileListBox: TListBox
      Left = 8
      Top = 8
      Width = 281
      Height = 273
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
      OnClick = FileListBoxClick
    end
    object DisplayMemo: TRichEdit
      Left = 296
      Top = 8
      Width = 233
      Height = 273
      Anchors = [akTop, akRight, akBottom]
      TabOrder = 1
    end
    object BtnFindAll: TButton
      Left = 8
      Top = 289
      Width = 121
      Height = 22
      Anchors = [akLeft, akBottom]
      Caption = '*.* from all sfds files'
      TabOrder = 2
      OnClick = BtnFindAllClick
    end
    object BtnFindTest1: TButton
      Left = 136
      Top = 289
      Width = 121
      Height = 22
      Anchors = [akLeft, akBottom]
      Caption = '*.* from test1.sfds'
      TabOrder = 3
      OnClick = BtnFindTest1Click
    end
    object BtnFindTest2: TButton
      Left = 264
      Top = 289
      Width = 121
      Height = 22
      Anchors = [akLeft, akBottom]
      Caption = '*.rtf;sample 1*.txt'
      TabOrder = 4
      OnClick = BtnFindTest2Click
    end
    object BtnFindTest3: TButton
      Left = 392
      Top = 289
      Width = 121
      Height = 22
      Anchors = [akLeft, akBottom]
      Caption = 'sample 1?.txt'
      TabOrder = 5
      OnClick = BtnFindTest3Click
    end
    object EditCustomSrc: TEdit
      Left = 136
      Top = 316
      Width = 377
      Height = 21
      Anchors = [akLeft, akBottom]
      TabOrder = 6
      Text = 'sample 1?.*;sample 2?.*;'
    end
    object Button1: TButton
      Left = 8
      Top = 316
      Width = 121
      Height = 22
      Anchors = [akLeft, akBottom]
      Caption = 'Custom search'
      TabOrder = 7
      OnClick = Button1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 417
    Width = 542
    Height = 49
    Align = alBottom
    TabOrder = 1
    object Label1: TLabel
      Left = 143
      Top = 14
      Width = 242
      Height = 13
      Alignment = taRightJustify
      Caption = 'Auto-rename stream mode (when loading SFDS file)'
    end
    object ComboBoxStrMode: TComboBox
      Left = 408
      Top = 12
      Width = 107
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'None'
      OnChange = ComboBoxStrModeChange
      Items.Strings = (
        'None'
        'System'
        'Windows'
        'UNIX')
    end
  end
end
