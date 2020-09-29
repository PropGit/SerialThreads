object Form1: TForm1
  Left = 1576
  Top = 135
  Width = 340
  Height = 857
  Caption = 'Serial Thread Tester'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    324
    819)
  PixelsPerInch = 96
  TextHeight = 13
  object PortLabel: TLabel
    Left = 240
    Top = 16
    Width = 22
    Height = 13
    Caption = 'Port:'
  end
  object BaudLabel: TLabel
    Left = 136
    Top = 16
    Width = 28
    Height = 13
    Caption = 'Baud:'
  end
  object BuffSizeLabel: TLabel
    Left = 8
    Top = 16
    Width = 54
    Height = 13
    Caption = 'Buffer Size:'
  end
  object SkipLabel: TLabel
    Left = 26
    Top = 72
    Width = 80
    Height = 13
    Caption = 'Skip First Line(s):'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object EndOfTemplateDelayLabel: TLabel
    Left = 26
    Top = 96
    Width = 152
    Height = 13
    Caption = 'End of Template Delay (> X ms):'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object MatchingLinesProcessedLabel: TLabel
    Left = 8
    Top = 136
    Width = 128
    Height = 13
    Caption = 'Matching Lines Processed:'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 124
    Width = 305
    Height = 2
  end
  object BuffLabel: TLabel
    Left = 8
    Top = 160
    Width = 31
    Height = 13
    Caption = 'Buffer:'
  end
  object PortEdit: TEdit
    Left = 266
    Top = 13
    Width = 47
    Height = 21
    TabOrder = 2
    Text = 'COM8'
  end
  object PortButton: TButton
    Left = 240
    Top = 91
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Open Port'
    TabOrder = 6
    OnClick = PortButtonClick
  end
  object RxMemo: TMemo
    Left = 8
    Top = 184
    Width = 308
    Height = 625
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clWhite
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object BaudEdit: TEdit
    Left = 168
    Top = 13
    Width = 57
    Height = 21
    TabOrder = 1
    Text = '2000000'
    OnExit = BaudEditExit
  end
  object BuffSizeEdit: TEdit
    Left = 66
    Top = 13
    Width = 57
    Height = 21
    TabOrder = 0
    Text = '256'
    OnExit = BuffSizeEditExit
  end
  object TemplateCheckBox: TCheckBox
    Left = 8
    Top = 46
    Width = 225
    Height = 17
    Caption = 'Parse from Template (First Burst of Lines)'
    TabOrder = 3
    OnClick = TemplateCheckBoxClick
  end
  object SkipEdit: TEdit
    Left = 110
    Top = 69
    Width = 19
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
    Text = '2'
    OnExit = BuffSizeEditExit
  end
  object EndOfTemplateDelayEdit: TEdit
    Left = 182
    Top = 93
    Width = 43
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
    Text = '750'
  end
  object MatchingLinesProcessedEdit: TEdit
    Left = 141
    Top = 133
    Width = 57
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 8
    Text = '0'
    OnExit = BuffSizeEditExit
  end
  object BuffProgressBar: TProgressBar
    Left = 43
    Top = 160
    Width = 273
    Height = 17
    Min = 0
    Max = 100
    TabOrder = 9
  end
end
