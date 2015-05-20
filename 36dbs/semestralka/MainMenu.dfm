object MainForm: TMainForm
  Left = 192
  Top = 107
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'ATC Infosystem'
  ClientHeight = 122
  ClientWidth = 254
  Color = clBtnFace
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
  object boxPilot: TGroupBox
    Left = 0
    Top = 0
    Width = 121
    Height = 57
    Caption = '&Pilot actions'
    TabOrder = 0
    object btnNewFlightPlan: TBitBtn
      Left = 16
      Top = 24
      Width = 89
      Height = 25
      Caption = 'New &flight plan'
      TabOrder = 0
      OnClick = btnNewFlightPlanClick
    end
  end
  object boxService: TGroupBox
    Left = 0
    Top = 64
    Width = 121
    Height = 57
    Caption = '&Service actions'
    TabOrder = 1
    object btnNewPilot: TBitBtn
      Left = 16
      Top = 24
      Width = 89
      Height = 25
      Caption = 'New pi&lot'
      TabOrder = 0
      OnClick = btnNewPilotClick
    end
  end
  object boxATC: TGroupBox
    Left = 128
    Top = 0
    Width = 121
    Height = 121
    Caption = '&ATC'
    TabOrder = 2
    object btnViewPlan: TBitBtn
      Left = 16
      Top = 24
      Width = 89
      Height = 25
      Caption = '&View flight plan'
      TabOrder = 0
      OnClick = btnViewPlanClick
    end
    object btnPilots: TBitBtn
      Left = 16
      Top = 56
      Width = 89
      Height = 25
      Caption = 'Pilo&ts'
      TabOrder = 1
      OnClick = btnPilotsClick
    end
  end
  object Database: TDatabase
    AliasName = 'IBLocal'
    DatabaseName = 'IBLocal'
    LoginPrompt = False
    Params.Strings = (
      'SERVER NAME=c:\Dokumenty\Petr\Skola\DBS\Semestrálka\DBS.gdb'
      'USER NAME=OPS$KADLECP2'
      'PASSWORD=password')
    SessionName = 'Default'
    Top = 56
  end
end
