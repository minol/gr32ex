object PropForm: TPropForm
  Left = 365
  Top = 59
  ActiveControl = ListCustom
  BorderIcons = [biSystemMenu]
  Caption = 'PropForm'
  ClientHeight = 436
  ClientWidth = 392
  Color = clBtnFace
  Constraints.MinHeight = 470
  Constraints.MinWidth = 390
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnConstrainedResize = FormConstrainedResize
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    392
    436)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 376
    Height = 391
    ActivePage = TabCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSummary: TTabSheet
      Caption = 'Summary'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        368
        363)
      object LabelComments: TLabel
        Left = 14
        Top = 259
        Width = 52
        Height = 13
        Alignment = taRightJustify
        Caption = 'Comments:'
      end
      object EditFileName: TLabeledEdit
        Left = 72
        Top = 16
        Width = 275
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 47
        EditLabel.Height = 13
        EditLabel.Caption = 'FileName:'
        LabelPosition = lpLeft
        LabelSpacing = 5
        ParentColor = True
        ReadOnly = True
        TabOrder = 0
      end
      object EditGUID: TLabeledEdit
        Left = 72
        Top = 48
        Width = 275
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 30
        EditLabel.Height = 13
        EditLabel.Caption = 'GUID:'
        LabelPosition = lpLeft
        LabelSpacing = 5
        ParentColor = True
        ReadOnly = True
        TabOrder = 1
      end
      object EditTitle: TLabeledEdit
        Left = 72
        Top = 80
        Width = 275
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 23
        EditLabel.Height = 13
        EditLabel.Caption = 'Title:'
        LabelPosition = lpLeft
        LabelSpacing = 5
        ParentColor = True
        ReadOnly = True
        TabOrder = 2
        OnChange = EditChange
      end
      object EditSubject: TLabeledEdit
        Left = 72
        Top = 112
        Width = 275
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 39
        EditLabel.Height = 13
        EditLabel.Caption = 'Subject:'
        LabelPosition = lpLeft
        LabelSpacing = 5
        ParentColor = True
        ReadOnly = True
        TabOrder = 3
        OnChange = EditChange
      end
      object EditAuthor: TLabeledEdit
        Left = 72
        Top = 144
        Width = 275
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 34
        EditLabel.Height = 13
        EditLabel.Caption = 'Author:'
        LabelPosition = lpLeft
        LabelSpacing = 5
        ParentColor = True
        ReadOnly = True
        TabOrder = 4
        OnChange = EditChange
      end
      object EditKeyWords: TLabeledEdit
        Left = 72
        Top = 224
        Width = 275
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 49
        EditLabel.Height = 13
        EditLabel.Caption = 'Keywords:'
        LabelPosition = lpLeft
        LabelSpacing = 5
        ParentColor = True
        ReadOnly = True
        TabOrder = 5
        OnChange = EditChange
      end
      object EditComments: TMemo
        Left = 72
        Top = 256
        Width = 275
        Height = 82
        Anchors = [akLeft, akTop, akRight, akBottom]
        ParentColor = True
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 6
        OnChange = EditChange
      end
      object EditDateLastSaved: TLabeledEdit
        Left = 120
        Top = 176
        Width = 227
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 83
        EditLabel.Height = 13
        EditLabel.Caption = 'Date Last Saved:'
        LabelPosition = lpLeft
        LabelSpacing = 5
        ParentColor = True
        ReadOnly = True
        TabOrder = 7
      end
    end
    object TabCustom: TTabSheet
      Caption = 'Custom'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        368
        363)
      object LabelProp: TLabel
        Left = 24
        Top = 112
        Width = 50
        Height = 13
        Alignment = taRightJustify
        Caption = 'Properties:'
      end
      object LabelCtype: TLabel
        Left = 46
        Top = 53
        Width = 27
        Height = 13
        Alignment = taRightJustify
        Caption = 'Type:'
      end
      object ListCustom: TListView
        Left = 80
        Top = 112
        Width = 270
        Height = 226
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Name'
            Width = 90
          end
          item
            Caption = 'Value'
            Width = 90
          end
          item
            AutoSize = True
            Caption = 'Type'
            MaxWidth = 75
          end
          item
            Width = 0
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnSelectItem = ListCustomSelectItem
      end
      object EditCustomName: TLabeledEdit
        Left = 80
        Top = 24
        Width = 160
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 31
        EditLabel.Height = 13
        EditLabel.Caption = 'Name:'
        LabelPosition = lpLeft
        LabelSpacing = 7
        TabOrder = 1
        OnChange = EditCustomNameChange
      end
      object EditCustomvalue: TLabeledEdit
        Left = 80
        Top = 76
        Width = 160
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 30
        EditLabel.Height = 13
        EditLabel.Caption = 'Value:'
        LabelPosition = lpLeft
        LabelSpacing = 7
        TabOrder = 3
        OnChange = EditCustomvalueChange
      end
      object EditCustomType: TComboBox
        Left = 80
        Top = 50
        Width = 160
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 2
        OnChange = EditCustomTypeChange
        Items.Strings = (
          'Text'
          'Number'
          'Date'
          'Yes or No')
      end
      object btnCustomAddmodify: TButton
        Left = 251
        Top = 24
        Width = 100
        Height = 22
        Anchors = [akTop, akRight]
        Caption = 'Add'
        TabOrder = 5
        OnClick = btnCustomAddmodifyClick
      end
      object btnCustomRemove: TButton
        Left = 251
        Top = 50
        Width = 100
        Height = 22
        Anchors = [akTop, akRight]
        Caption = 'Remove'
        TabOrder = 6
        OnClick = btnCustomRemoveClick
      end
      object EditCustomBool: TRadioGroup
        Left = 80
        Top = 72
        Width = 160
        Height = 33
        Anchors = [akLeft, akTop, akRight]
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Yes'
          'No')
        TabOrder = 4
        OnClick = EditCustomBoolClick
      end
    end
    object TabStreams: TTabSheet
      Caption = 'Streams'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        368
        363)
      object ListStream: TListView
        Left = 3
        Top = 3
        Width = 360
        Height = 355
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Stream Name'
            Width = 200
          end
          item
            Caption = 'Size'
            Width = 70
          end
          item
            AutoSize = True
            Caption = 'Ratio'
            MaxWidth = 75
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object TabPreview: TTabSheet
      Caption = 'Preview'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ImagePreview: TImage
        Left = 0
        Top = 0
        Width = 368
        Height = 363
        Align = alClient
        Center = True
        Proportional = True
        ExplicitWidth = 336
        ExplicitHeight = 362
      end
    end
  end
  object BtnCancel: TButton
    Left = 218
    Top = 406
    Width = 80
    Height = 22
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = BtnCancelClick
  end
  object btnOk: TButton
    Left = 132
    Top = 406
    Width = 80
    Height = 22
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
  end
  object BtnApply: TButton
    Left = 304
    Top = 405
    Width = 80
    Height = 22
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    Enabled = False
    TabOrder = 3
    OnClick = BtnApplyClick
  end
  object btnHelp: TButton
    Left = 8
    Top = 405
    Width = 75
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = '&Help'
    TabOrder = 4
    Visible = False
    OnClick = btnHelpClick
  end
end
