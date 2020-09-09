object Form1: TForm1
  Left = 1699
  Top = 135
  Width = 217
  Height = 545
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 56
    Top = 8
    Width = 22
    Height = 13
    Caption = 'Port:'
  end
  object PortEdit: TEdit
    Left = 88
    Top = 5
    Width = 57
    Height = 21
    TabOrder = 0
    Text = 'COM8'
  end
  object PortButton: TButton
    Left = 64
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Open Port'
    TabOrder = 1
    OnClick = PortButtonClick
  end
  object RxMemo: TMemo
    Left = 8
    Top = 96
    Width = 185
    Height = 401
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
