object frmPilotList: TfrmPilotList
  Left = 192
  Top = 107
  Width = 544
  Height = 375
  Caption = 'Pilots'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gridPilots: TDBGrid
    Left = 0
    Top = 0
    Width = 536
    Height = 348
    Align = alClient
    DataSource = srcPilots
    Options = [dgTitles, dgColumnResize, dgColLines, dgRowSelect]
    ReadOnly = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object srcPilots: TDataSource
    DataSet = tblPilots
    Left = 408
    Top = 16
  end
  object tblPilots: TTable
    DatabaseName = 'IBLocal'
    TableName = 'PILOT'
    Left = 440
    Top = 16
    object tblPilotsRODCISLO: TStringField
      DisplayLabel = 'Personal ID'
      FieldName = 'RODCISLO'
      Required = True
      FixedChar = True
      Size = 11
    end
    object tblPilotsCISLOLETPRUKAZU: TIntegerField
      DisplayLabel = 'Flight license ID'
      FieldName = 'CISLOLETPRUKAZU'
      Required = True
    end
    object tblPilotsJMENO: TStringField
      DisplayLabel = 'Name'
      FieldName = 'JMENO'
      Required = True
      Size = 100
    end
  end
end
