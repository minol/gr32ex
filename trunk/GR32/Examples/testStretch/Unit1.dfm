object Form1: TForm1
  Left = 198
  Top = 114
  Width = 800
  Height = 600
  Caption = 'The Benchmark...'
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 566
    Align = alLeft
    TabOrder = 0
    DesignSize = (
      201
      566)
    object LabelWait: TLabel
      Left = 8
      Top = 196
      Width = 185
      Height = 13
      AutoSize = False
      Caption = 'Please wait, Test is runing...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
    end
    object Memo1: TMemo
      Left = 1
      Top = 1
      Width = 199
      Height = 192
      Align = alTop
      TabOrder = 0
    end
    object Button4: TButton
      Left = 8
      Top = 220
      Width = 185
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'test'
      TabOrder = 4
      OnClick = Button4Click
    end
  end
  object Dest: TImage32
    Left = 201
    Top = 0
    Width = 591
    Height = 566
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 1
  end
  object Src: TBitmap32List
    Bitmaps = <
      item
        Bitmap.ResamplerClassName = 'TNearestResampler'
      end>
    Left = 8
    Top = 8
  end
end
