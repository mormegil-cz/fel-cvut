object frmNewGame: TfrmNewGame
  Left = 396
  Top = 340
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Quoridor — New Game'
  ClientHeight = 85
  ClientWidth = 260
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblPlayer1: TLabel
    Left = 8
    Top = 8
    Width = 41
    Height = 13
    Caption = 'Player 1:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblPlayer2: TLabel
    Left = 8
    Top = 32
    Width = 41
    Height = 13
    Caption = 'Player 2:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object btnHuman1: TSpeedButton
    Left = 181
    Top = 3
    Width = 23
    Height = 22
    GroupIndex = 1
    Down = True
    Flat = True
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
      77777777777CC0877777777777CCCC087777777777CCCC008777777777CCCC88
      7777777777CCCC777777777777CCCC77777777777CCCCCC7777777777CCCCCC7
      777777777CCCCCC7777777777CCCCCC7777777777778877777777777778FF877
      77777777778FF877777777777778877777777777777777777777}
    OnClick = btnPlayerTypeClick
  end
  object btnCPU1: TSpeedButton
    Left = 205
    Top = 3
    Width = 23
    Height = 22
    GroupIndex = 1
    Flat = True
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777770F87777
      777777770708887777778770800F887877778708080008887777808088800F88
      7877880888880008887780888888800F88F888F8888888000888770F88888880
      0F887788F8888888000877770F888888800F777788F8888888007777770F8888
      888777777788F8888877777777770F8887777777777788F87777}
    OnClick = btnPlayerTypeClick
  end
  object btnHuman2: TSpeedButton
    Left = 181
    Top = 27
    Width = 23
    Height = 22
    GroupIndex = 2
    Flat = True
    OnClick = btnPlayerTypeClick
  end
  object btnCPU2: TSpeedButton
    Left = 205
    Top = 27
    Width = 23
    Height = 22
    GroupIndex = 2
    Down = True
    Flat = True
    OnClick = btnPlayerTypeClick
  end
  object editName1: TEdit
    Left = 56
    Top = 4
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Player 1'
  end
  object editName2: TEdit
    Left = 56
    Top = 28
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'Player 2'
  end
  object btnStart: TButton
    Left = 88
    Top = 56
    Width = 75
    Height = 25
    Caption = 'GO!'
    Default = True
    TabOrder = 2
    OnClick = btnStartClick
  end
  object editCPULevel1: TCSpinEdit
    Left = 228
    Top = 3
    Width = 33
    Height = 22
    TabStop = True
    Enabled = False
    MaxValue = 5
    MinValue = 1
    ParentColor = False
    TabOrder = 3
    Value = 2
  end
  object editCPULevel2: TCSpinEdit
    Left = 228
    Top = 27
    Width = 33
    Height = 22
    TabStop = True
    MaxValue = 5
    MinValue = 1
    ParentColor = False
    TabOrder = 4
    Value = 2
  end
end
