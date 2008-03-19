object Form1: TForm1
  Left = 444
  Top = 395
  Caption = 'Form1'
  ClientHeight = 426
  ClientWidth = 683
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    683
    426)
  PixelsPerInch = 96
  TextHeight = 13
  object lbLog: TListBox
    Left = 176
    Top = 8
    Width = 497
    Height = 410
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 11
  end
  object btnFullTest: TButton
    Left = 8
    Top = 8
    Width = 161
    Height = 25
    Caption = 'Full test'
    TabOrder = 0
    OnClick = btnFullTestClick
  end
  object btnTestStorageCreation: TButton
    Left = 48
    Top = 40
    Width = 123
    Height = 25
    Caption = 'Storage creation'
    TabOrder = 1
    OnClick = btnTestStorageCreationClick
  end
  object btnTestFlatFileSystem: TButton
    Left = 48
    Top = 72
    Width = 123
    Height = 25
    Caption = 'Flat file system'
    TabOrder = 2
    OnClick = btnTestFlatFileSystemClick
  end
  object btnTestFragmentedFiles: TButton
    Left = 48
    Top = 136
    Width = 123
    Height = 25
    Caption = 'Fragmented files'
    TabOrder = 4
    OnClick = btnTestFragmentedFilesClick
  end
  object btnTestFolders: TButton
    Left = 48
    Top = 168
    Width = 123
    Height = 25
    Caption = 'Folders'
    TabOrder = 5
    OnClick = btnTestFoldersClick
  end
  object btnTestBigAndSmall: TButton
    Left = 48
    Top = 104
    Width = 123
    Height = 25
    Caption = 'Big and small'
    TabOrder = 3
    OnClick = btnTestBigAndSmallClick
  end
  object btnTestTruncation: TButton
    Left = 48
    Top = 200
    Width = 123
    Height = 25
    Caption = 'Storage truncation'
    TabOrder = 6
    OnClick = btnTestTruncationClick
  end
  object btnTestExists: TButton
    Left = 48
    Top = 232
    Width = 123
    Height = 25
    Caption = 'Exists'
    TabOrder = 7
    OnClick = btnTestExistsClick
  end
  object btnTestExceptions: TButton
    Left = 48
    Top = 392
    Width = 123
    Height = 25
    Caption = 'Exceptions'
    TabOrder = 10
    OnClick = btnTestExceptionsClick
  end
  object btnTestMovingAndDeleting: TButton
    Left = 48
    Top = 296
    Width = 123
    Height = 25
    Caption = 'Moving && Deleting'
    TabOrder = 9
    OnClick = btnTestMovingAndDeletingClick
  end
  object btnTestEnumerating: TButton
    Left = 48
    Top = 264
    Width = 123
    Height = 25
    Caption = 'Enumerating'
    TabOrder = 8
    OnClick = btnTestEnumeratingClick
  end
  object btnTestAttributes: TButton
    Left = 48
    Top = 328
    Width = 123
    Height = 25
    Caption = 'Attributes'
    TabOrder = 12
    OnClick = btnTestAttributesClick
  end
  object btnTestCompacting: TButton
    Left = 48
    Top = 360
    Width = 123
    Height = 25
    Caption = 'Compacting'
    TabOrder = 13
    OnClick = btnTestCompactingClick
  end
end
