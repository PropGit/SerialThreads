object Form1: TForm1
  Left = 1576
  Top = 135
  Width = 340
  Height = 545
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
    507)
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
  object PortEdit: TEdit
    Left = 266
    Top = 13
    Width = 47
    Height = 21
    TabOrder = 3
    Text = 'COM8'
  end
  object PortButton: TButton
    Left = 240
    Top = 67
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Open Port'
    TabOrder = 0
    OnClick = PortButtonClick
  end
  object RxMemo: TMemo
    Left = 8
    Top = 104
    Width = 308
    Height = 393
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clWhite
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object BaudEdit: TEdit
    Left = 168
    Top = 13
    Width = 57
    Height = 21
    TabOrder = 2
    Text = '2000000'
    OnExit = BaudEditExit
  end
  object BuffSizeEdit: TEdit
    Left = 66
    Top = 13
    Width = 57
    Height = 21
    TabOrder = 1
    Text = '256'
    OnExit = BuffSizeEditExit
  end
  object TemplateCheckBox: TCheckBox
    Left = 8
    Top = 46
    Width = 225
    Height = 17
    Caption = 'Parse from Template (First Burst of Lines)'
    TabOrder = 5
    OnClick = TemplateCheckBoxClick
  end
  object SkipEdit: TEdit
    Left = 110
    Top = 69
    Width = 27
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 6
    Text = '2'
    OnExit = BuffSizeEditExit
  end
end
