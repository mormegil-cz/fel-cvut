object frmMain: TfrmMain
  Left = 192
  Top = 107
  Width = 696
  Height = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Graf: TPaintBox
    Left = 0
    Top = 0
    Width = 503
    Height = 434
    Align = alClient
    OnPaint = GrafPaint
  end
  object Panel1: TPanel
    Left = 503
    Top = 0
    Width = 185
    Height = 434
    Align = alRight
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 55
      Height = 13
      Caption = 'Generation:'
    end
    object lblGeneration: TLabel
      Left = 80
      Top = 8
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label2: TLabel
      Left = 8
      Top = 40
      Width = 30
      Height = 13
      Caption = 'Value:'
    end
    object lblValue: TLabel
      Left = 80
      Top = 40
      Width = 20
      Height = 13
      Caption = 'N/A'
    end
    object Label3: TLabel
      Left = 8
      Top = 24
      Width = 26
      Height = 13
      Caption = 'Time:'
    end
    object lblTime: TLabel
      Left = 80
      Top = 24
      Width = 42
      Height = 13
      Caption = '00:00:00'
    end
    object lblLastChange: TLabel
      Left = 136
      Top = 8
      Width = 12
      Height = 13
      Caption = '(0)'
    end
    object Label4: TLabel
      Left = 8
      Top = 80
      Width = 53
      Height = 13
      Caption = 'Elite count:'
    end
    object lblElite: TLabel
      Left = 80
      Top = 80
      Width = 12
      Height = 13
      Caption = '20'
    end
    object Label5: TLabel
      Left = 8
      Top = 96
      Width = 64
      Height = 13
      Caption = 'INV mutation:'
    end
    object lblInvMutation: TLabel
      Left = 80
      Top = 96
      Width = 12
      Height = 13
      Caption = '30'
    end
    object Label7: TLabel
      Left = 8
      Top = 112
      Width = 71
      Height = 13
      Caption = 'SWP mutation:'
    end
    object lblSwapMutation: TLabel
      Left = 80
      Top = 112
      Width = 6
      Height = 13
      Caption = '5'
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 434
    Width = 688
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
end
