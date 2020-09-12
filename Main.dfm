object Form1: TForm1
  Left = 1598
  Top = 135
  Width = 318
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
    302
    507)
  PixelsPerInch = 96
  TextHeight = 13
  object PortLabel: TLabel
    Left = 112
    Top = 72
    Width = 22
    Height = 13
    Caption = 'Port:'
  end
  object BaudLabel: TLabel
    Left = 8
    Top = 72
    Width = 28
    Height = 13
    Caption = 'Baud:'
  end
  object BuffSizeLabel: TLabel
    Left = 8
    Top = 40
    Width = 54
    Height = 13
    Caption = 'Buffer Size:'
  end
  object PortEdit: TEdit
    Left = 138
    Top = 69
    Width = 47
    Height = 21
    TabOrder = 3
    Text = 'COM8'
  end
  object PortButton: TButton
    Left = 218
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
    Top = 96
    Width = 286
    Height = 401
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object BaudEdit: TEdit
    Left = 40
    Top = 69
    Width = 57
    Height = 21
    TabOrder = 2
    Text = '2000000'
    OnExit = BaudEditExit
  end
  object BuffSizeEdit: TEdit
    Left = 66
    Top = 37
    Width = 57
    Height = 21
    TabOrder = 1
    Text = '256'
    OnExit = BuffSizeEditExit
  end
end
