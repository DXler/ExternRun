object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Test'
  ClientHeight = 531
  ClientWidth = 826
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo2: TMemo
    Left = 8
    Top = 311
    Width = 810
    Height = 211
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 8
    Top = 94
    Width = 810
    Height = 211
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Start extern program'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 36
    Width = 129
    Height = 25
    Caption = 'Send command'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 147
    Top = 39
    Width = 671
    Height = 21
    TabOrder = 2
  end
  object Edit2: TEdit
    Left = 147
    Top = 10
    Width = 671
    Height = 21
    TabOrder = 3
  end
  object Button3: TButton
    Left = 8
    Top = 67
    Width = 129
    Height = 25
    Caption = 'Abort extern program'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 147
    Top = 67
    Width = 208
    Height = 25
    Caption = 'Set Environment (show with "set")'
    TabOrder = 5
    OnClick = Button4Click
  end
end
