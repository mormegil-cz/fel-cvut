object frmAddPilot: TfrmAddPilot
  Left = 192
  Top = 107
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'New pilot'
  ClientHeight = 109
  ClientWidth = 267
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 31
    Height = 13
    Caption = '&Name:'
    FocusControl = editName
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 58
    Height = 13
    Caption = '&Personal ID:'
  end
  object Label3: TLabel
    Left = 8
    Top = 56
    Width = 54
    Height = 13
    Caption = '&License ID:'
    FocusControl = editLicense
  end
  object editName: TDBEdit
    Left = 72
    Top = 4
    Width = 185
    Height = 21
    DataField = 'JMENO'
    DataSource = srcPilots
    TabOrder = 0
  end
  object editLicense: TDBEdit
    Left = 72
    Top = 52
    Width = 73
    Height = 21
    DataField = 'CISLOLETPRUKAZU'
    DataSource = srcPilots
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 8
    Top = 80
    Width = 75
    Height = 25
    Caption = 'O&K'
    Default = True
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 184
    Top = 80
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object editID: TDBEdit
    Left = 72
    Top = 28
    Width = 73
    Height = 21
    DataField = 'RODCISLO'
    DataSource = srcPilots
    MaxLength = 11
    TabOrder = 1
  end
  object srcPilots: TDataSource
    DataSet = tblPilots
    Left = 200
    Top = 32
  end
  object tblPilots: TTable
    DatabaseName = 'IBLocal'
    TableName = 'PILOT'
    Left = 232
    Top = 32
  end
end
