object fmMain: TfmMain
  Left = 26
  Top = 103
  Width = 957
  Height = 688
  Caption = 'True Type Font testing'
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  ShowHint = True
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 759
    Top = 0
    Width = 190
    Height = 640
    Align = alRight
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object lbAngle: TLabel
      Left = 16
      Top = 72
      Width = 48
      Height = 14
      Caption = 'Angle: (0)'
    end
    object lbXSkew: TLabel
      Left = 13
      Top = 112
      Width = 34
      Height = 14
      Caption = 'xSkew'
    end
    object Label2: TLabel
      Left = 24
      Top = 128
      Width = 48
      Height = 14
      Caption = 'Angle: (0)'
    end
    object lbYSkew: TLabel
      Left = 16
      Top = 152
      Width = 34
      Height = 14
      Caption = 'ySkew'
    end
    object lbXScale: TLabel
      Left = 16
      Top = 189
      Width = 33
      Height = 14
      Caption = 'xScale'
    end
    object lbYScale: TLabel
      Left = 15
      Top = 230
      Width = 33
      Height = 14
      Caption = 'yScale'
    end
    object btnSelectFont: TButton
      Left = 8
      Top = 40
      Width = 169
      Height = 25
      Caption = 'Select font...'
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnSelectFontClick
    end
    object btnDrawMany: TButton
      Left = 8
      Top = 272
      Width = 171
      Height = 25
      Caption = 'Draw Many Labels'
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnDrawManyClick
    end
    object Edit1: TEdit
      Left = 8
      Top = 8
      Width = 164
      Height = 25
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      Text = '÷–Œƒ≤‚ ‘ Test text ABCdefGHIKLMNOPqrstTUVWXYZ'
      OnChange = Edit1Change
    end
    object trackAngle: TTrackBar
      Left = 8
      Top = 88
      Width = 169
      Height = 25
      Max = 180
      Min = -180
      Frequency = 10
      SelEnd = 90
      SelStart = -90
      TabOrder = 3
      ThumbLength = 10
      OnChange = trackAngleChange
    end
    object trackXSkew: TTrackBar
      Left = 8
      Top = 128
      Width = 169
      Height = 25
      Max = 50
      Min = -50
      Frequency = 2
      TabOrder = 4
      ThumbLength = 10
      OnChange = trackAngleChange
    end
    object trackYSkew: TTrackBar
      Left = 8
      Top = 168
      Width = 169
      Height = 25
      Max = 50
      Min = -50
      Frequency = 2
      TabOrder = 5
      ThumbLength = 10
      OnChange = trackAngleChange
    end
    object trackXScale: TTrackBar
      Left = 8
      Top = 205
      Width = 169
      Height = 25
      Max = 50
      Min = 1
      Frequency = 2
      Position = 5
      TabOrder = 6
      ThumbLength = 10
      OnChange = trackAngleChange
    end
    object trackYScale: TTrackBar
      Left = 8
      Top = 246
      Width = 169
      Height = 25
      Max = 50
      Min = 1
      Frequency = 2
      Position = 5
      TabOrder = 7
      ThumbLength = 10
      OnChange = trackAngleChange
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 312
      Width = 145
      Height = 89
      Caption = 'Drawning Options'
      TabOrder = 8
      object cbAntialising: TCheckBox
        Left = 24
        Top = 24
        Width = 89
        Height = 17
        Caption = 'Antialising'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = cbAntialisingClick
      end
      object cbFilling: TCheckBox
        Left = 24
        Top = 48
        Width = 105
        Height = 17
        Caption = 'Filling'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = cbFillingClick
      end
    end
    object btnClearBitmap: TButton
      Left = 8
      Top = 408
      Width = 89
      Height = 25
      Caption = '&Clear'
      TabOrder = 9
      OnClick = btnClearBitmapClick
    end
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 640
    Width = 949
    Height = 21
    Panels = <
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end>
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -27
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Options = [fdForceFontExist, fdScalableOnly]
    Left = 122
    Top = 135
  end
end
