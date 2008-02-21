object GRLayerEditor: TGRLayerEditor
  Left = 245
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Dialog'
  ClientHeight = 214
  ClientWidth = 313
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  DesignSize = (
    313
    214)
  PixelsPerInch = 96
  TextHeight = 13
  object shpLine: TBevel
    Left = 8
    Top = 173
    Width = 297
    Height = 9
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
  end
  object btnOpenPic: TSpeedButton
    Left = 134
    Top = 138
    Width = 58
    Height = 22
    Caption = 'Browser'
    OnClick = btnOpenPicClick
  end
  object btnOK: TButton
    Left = 79
    Top = 180
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 159
    Top = 180
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edtLeft: TLabeledEdit
    Left = 136
    Top = 20
    Width = 121
    Height = 21
    EditLabel.Width = 19
    EditLabel.Height = 13
    EditLabel.Caption = 'Left'
    TabOrder = 2
    OnKeyPress = NumberKeyPressOnly
  end
  object edtTop: TLabeledEdit
    Left = 132
    Top = 64
    Width = 121
    Height = 21
    EditLabel.Width = 18
    EditLabel.Height = 13
    EditLabel.Caption = 'Top'
    TabOrder = 3
    OnKeyPress = NumberKeyPressOnly
  end
  object dlgOpenPicture: TOpenPictureDialog
    Left = 112
    Top = 8
  end
end
