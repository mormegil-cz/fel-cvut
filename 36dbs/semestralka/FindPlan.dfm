object frmFindPlan: TfrmFindPlan
  Left = 219
  Top = 113
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Flight plans'
  ClientHeight = 424
  ClientWidth = 420
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
  object Label1: TLabel
    Left = 16
    Top = 48
    Width = 113
    Height = 13
    Caption = '7 - Aircraft identification:'
  end
  object Label2: TLabel
    Left = 16
    Top = 74
    Width = 68
    Height = 13
    Caption = '8 - Flight rules:'
  end
  object Label3: TLabel
    Left = 128
    Top = 74
    Width = 64
    Height = 13
    Caption = 'Type of flight:'
  end
  object Label4: TLabel
    Left = 16
    Top = 100
    Width = 52
    Height = 13
    Caption = '9. Number:'
  end
  object Label5: TLabel
    Left = 120
    Top = 100
    Width = 74
    Height = 13
    Caption = 'Type of aircraft:'
  end
  object Label6: TLabel
    Left = 264
    Top = 100
    Width = 129
    Height = 13
    Caption = 'Wake turbulence category:'
  end
  object Label7: TLabel
    Left = 16
    Top = 126
    Width = 71
    Height = 13
    Caption = '10. Equipment:'
  end
  object Label8: TLabel
    Left = 96
    Top = 126
    Width = 27
    Height = 13
    Caption = '/SSR'
  end
  object Label9: TLabel
    Left = 16
    Top = 152
    Width = 121
    Height = 13
    Caption = '13. Departure aerodrome:'
  end
  object Label10: TLabel
    Left = 264
    Top = 152
    Width = 26
    Height = 13
    Caption = 'Time:'
  end
  object Label11: TLabel
    Left = 16
    Top = 178
    Width = 90
    Height = 13
    Caption = '15. Cruising speed:'
  end
  object Label12: TLabel
    Left = 184
    Top = 178
    Width = 29
    Height = 13
    Caption = 'Level:'
  end
  object Label13: TLabel
    Left = 16
    Top = 206
    Width = 32
    Height = 13
    Caption = 'Route:'
  end
  object Label14: TLabel
    Left = 16
    Top = 328
    Width = 127
    Height = 13
    Caption = '16. Destination aerodrome:'
  end
  object Label15: TLabel
    Left = 272
    Top = 328
    Width = 24
    Height = 13
    Caption = 'EET:'
  end
  object Label16: TLabel
    Left = 120
    Top = 352
    Width = 21
    Height = 13
    Caption = 'Altn:'
  end
  object Label17: TLabel
    Left = 272
    Top = 352
    Width = 44
    Height = 13
    Caption = '2nd altn::'
  end
  object Label18: TLabel
    Left = 16
    Top = 376
    Width = 101
    Height = 13
    Caption = '18. Other information:'
  end
  object Label19: TLabel
    Left = 16
    Top = 400
    Width = 83
    Height = 13
    Caption = 'Pilot in command:'
  end
  object DBComboPilot: TDBText
    Left = 112
    Top = 400
    Width = 68
    Height = 13
    AutoSize = True
    Color = clBtnFace
    DataField = 'Kapitan_Jmeno'
    DataSource = srcPlan
    ParentColor = False
  end
  object DBComboAircraftID: TDBText
    Left = 136
    Top = 48
    Width = 92
    Height = 13
    AutoSize = True
    Color = clBtnFace
    DataField = 'LETADLOP_ID'
    DataSource = srcPlan
    ParentColor = False
  end
  object DBComboDepart: TDBText
    Left = 144
    Top = 152
    Width = 80
    Height = 13
    AutoSize = True
    Color = clBtnFace
    DataField = 'OdletZ_Jmeno'
    DataSource = srcPlan
    ParentColor = False
  end
  object DBComboDestin: TDBText
    Left = 152
    Top = 328
    Width = 78
    Height = 13
    AutoSize = True
    Color = clBtnFace
    DataField = 'Pristani_Jmeno'
    DataSource = srcPlan
    ParentColor = False
  end
  object editDepartTime: TDBText
    Left = 296
    Top = 152
    Width = 72
    Height = 13
    AutoSize = True
    Color = clBtnFace
    DataField = 'CASODLETU'
    DataSource = srcPlan
    ParentColor = False
  end
  object comboRules: TDBText
    Left = 96
    Top = 74
    Width = 17
    Height = 13
    Color = clBtnFace
    DataField = 'PREDPIS'
    DataSource = srcPlan
    ParentColor = False
  end
  object txtPlanNum: TDBText
    Left = 160
    Top = 16
    Width = 65
    Height = 17
    DataField = 'SCHVALCISLO'
    DataSource = srcPlan
  end
  object DBText1: TDBText
    Left = 216
    Top = 400
    Width = 42
    Height = 13
    AutoSize = True
    Color = clBtnFace
    DataField = 'KAPITANL_CISLOLETPRUKAZU'
    DataSource = srcPlan
    ParentColor = False
  end
  object DBNavigator: TDBNavigator
    Left = 16
    Top = 8
    Width = 135
    Height = 25
    DataSource = srcPlan
    VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbRefresh]
    ConfirmDelete = False
    TabOrder = 0
  end
  object gridRoute: TDBGrid
    Left = 56
    Top = 208
    Width = 353
    Height = 112
    DataSource = srcRoute
    Options = []
    ReadOnly = True
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'NAZEV'
        Width = 330
        Visible = True
      end>
  end
  object srcPilot: TDataSource
    DataSet = qPilot
    Left = 392
    Top = 8
  end
  object qPilot: TQuery
    DatabaseName = 'IBLocal'
    SQL.Strings = (
      'select * from Pilot')
    Left = 392
    Top = 40
  end
  object qNavPoint: TQuery
    DatabaseName = 'IBLocal'
    SQL.Strings = (
      'SELECT * FROM NavObjekt')
    Left = 360
    Top = 40
  end
  object srcNavPoint: TDataSource
    DataSet = qNavPoint
    Left = 360
    Top = 8
  end
  object srcPlan: TDataSource
    DataSet = tPlan
    Left = 328
    Top = 8
  end
  object tPlan: TTable
    AfterScroll = tPlanAfterScroll
    DatabaseName = 'IBLocal'
    ReadOnly = True
    TableName = 'LETOVYPLAN'
    Left = 328
    Top = 40
    object tPlanCASODLETU: TDateTimeField
      FieldName = 'CASODLETU'
      Required = True
    end
    object tPlanPREDPIS: TStringField
      FieldName = 'PREDPIS'
      Required = True
      FixedChar = True
      Size = 1
    end
    object tPlanSCHVALCISLO: TIntegerField
      FieldName = 'SCHVALCISLO'
      Required = True
    end
    object tPlanLETADLOP_ID: TStringField
      FieldName = 'LETADLOP_ID'
      Required = True
      FixedChar = True
      Size = 10
    end
    object tPlanODLETZ_NAVOBJEK_ID: TStringField
      DisplayWidth = 100
      FieldKind = fkLookup
      FieldName = 'OdletZ_Jmeno'
      LookupDataSet = qNavPoint
      LookupKeyFields = 'ID'
      LookupResultField = 'NAZEV'
      KeyFields = 'ODLETZ_NAVOBJEK_ID'
      LookupCache = True
      Required = True
      FixedChar = True
      Size = 100
      Lookup = True
    end
    object tPlanPRISTANI_NAVOBJEK_ID: TStringField
      DisplayWidth = 100
      FieldKind = fkLookup
      FieldName = 'Pristani_Jmeno'
      LookupDataSet = qNavPoint
      LookupKeyFields = 'ID'
      LookupResultField = 'NAZEV'
      KeyFields = 'PRISTANI_NAVOBJEK_ID'
      LookupCache = True
      Required = True
      FixedChar = True
      Size = 100
      Lookup = True
    end
    object tPlanODLETZ_NAVOBJEK_ID2: TStringField
      FieldName = 'ODLETZ_NAVOBJEK_ID'
      Required = True
      FixedChar = True
      Size = 5
    end
    object tPlanPRISTANI_NAVOBJEK_ID2: TStringField
      FieldName = 'PRISTANI_NAVOBJEK_ID'
      Required = True
      FixedChar = True
      Size = 5
    end
    object tPlanKAPITANL_CISLOLETPRUKAZU2: TIntegerField
      FieldName = 'KAPITANL_CISLOLETPRUKAZU'
      Required = True
    end
    object tPlanKapitan_Jmeno: TStringField
      FieldKind = fkLookup
      FieldName = 'Kapitan_Jmeno'
      LookupDataSet = qPilot
      LookupKeyFields = 'CISLOLETPRUKAZU'
      LookupResultField = 'JMENO'
      KeyFields = 'KAPITANL_CISLOLETPRUKAZU'
      Size = 100
      Lookup = True
    end
  end
  object qRoute: TQuery
    DatabaseName = 'IBLocal'
    SQL.Strings = (
      
        'SELECT PoradiVPlanu,Navigacn_NavObjek_ID,ID,Nazev FROM NavObjekt' +
        ',PlanPresBod WHERE Navigacn_NavObjek_ID=ID AND LetovyPl_SchvalCi' +
        'slo=:PlanNum ORDER BY PoradiVPlanu')
    Left = 288
    Top = 40
    ParamData = <
      item
        DataType = ftInteger
        Name = 'PlanNum'
        ParamType = ptInput
      end>
  end
  object srcRoute: TDataSource
    DataSet = qRoute
    Left = 288
    Top = 8
  end
end
