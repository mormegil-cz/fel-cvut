object frmNewPlan: TfrmNewPlan
  Left = 201
  Top = 44
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'New flight plan'
  ClientHeight = 509
  ClientWidth = 530
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
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 113
    Height = 13
    Caption = '7 - Aircraft identification:'
  end
  object Label2: TLabel
    Left = 16
    Top = 42
    Width = 68
    Height = 13
    Caption = '8 - Flight rules:'
  end
  object Label3: TLabel
    Left = 128
    Top = 42
    Width = 64
    Height = 13
    Caption = 'Type of flight:'
  end
  object Label4: TLabel
    Left = 16
    Top = 68
    Width = 52
    Height = 13
    Caption = '9. Number:'
  end
  object Label5: TLabel
    Left = 120
    Top = 68
    Width = 74
    Height = 13
    Caption = 'Type of aircraft:'
  end
  object Label6: TLabel
    Left = 264
    Top = 68
    Width = 129
    Height = 13
    Caption = 'Wake turbulence category:'
  end
  object Label7: TLabel
    Left = 16
    Top = 94
    Width = 71
    Height = 13
    Caption = '10. Equipment:'
  end
  object Label8: TLabel
    Left = 240
    Top = 94
    Width = 27
    Height = 13
    Caption = '/SSR'
  end
  object Label9: TLabel
    Left = 16
    Top = 120
    Width = 121
    Height = 13
    Caption = '13. Departure aerodrome:'
  end
  object Label10: TLabel
    Left = 312
    Top = 120
    Width = 26
    Height = 13
    Caption = 'Time:'
  end
  object Label11: TLabel
    Left = 16
    Top = 146
    Width = 90
    Height = 13
    Caption = '15. Cruising speed:'
  end
  object Label12: TLabel
    Left = 184
    Top = 146
    Width = 29
    Height = 13
    Caption = 'Level:'
  end
  object Label13: TLabel
    Left = 16
    Top = 174
    Width = 32
    Height = 13
    Caption = 'Route:'
  end
  object Label14: TLabel
    Left = 16
    Top = 296
    Width = 127
    Height = 13
    Caption = '16. Destination aerodrome:'
  end
  object Label15: TLabel
    Left = 312
    Top = 296
    Width = 24
    Height = 13
    Caption = 'EET:'
  end
  object Label16: TLabel
    Left = 120
    Top = 320
    Width = 21
    Height = 13
    Caption = 'Altn:'
  end
  object Label17: TLabel
    Left = 312
    Top = 320
    Width = 44
    Height = 13
    Caption = '2nd altn::'
  end
  object Label18: TLabel
    Left = 16
    Top = 344
    Width = 101
    Height = 13
    Caption = '18. Other information:'
  end
  object Label19: TLabel
    Left = 16
    Top = 448
    Width = 83
    Height = 13
    Caption = 'Pilot in command:'
  end
  object editEquipment: TEdit
    Left = 88
    Top = 90
    Width = 153
    Height = 21
    TabOrder = 6
  end
  object comboSSR: TComboBox
    Left = 269
    Top = 90
    Width = 33
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 7
    Items.Strings = (
      'N'
      'A'
      'C'
      'X'
      'P'
      'I'
      'S')
  end
  object Panel1: TPanel
    Left = 80
    Top = 64
    Width = 17
    Height = 21
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 3
    object lblNumber: TLabel
      Left = 1
      Top = 1
      Width = 15
      Height = 19
      Align = alClient
      Layout = tlCenter
    end
  end
  object Panel2: TPanel
    Left = 200
    Top = 64
    Width = 33
    Height = 21
    BevelOuter = bvLowered
    TabOrder = 4
    object DBtxtAircraftType: TDBText
      Left = 1
      Top = 4
      Width = 42
      Height = 13
      DataSource = srcAircftType
      Transparent = True
    end
  end
  object Panel3: TPanel
    Left = 400
    Top = 64
    Width = 17
    Height = 21
    BevelOuter = bvLowered
    TabOrder = 5
    object DBtxtTurbulence: TDBText
      Left = 1
      Top = 4
      Width = 42
      Height = 13
      DataSource = srcAircftType
      Transparent = True
    end
  end
  object editDepartTime: TMaskEdit
    Left = 344
    Top = 116
    Width = 97
    Height = 21
    EditMask = '#0.#0.0000, #0:00;1;_'
    MaxLength = 17
    TabOrder = 9
    Text = '  .  .    ,   :  '
  end
  object comboSpeedUnits: TComboBox
    Left = 112
    Top = 142
    Width = 33
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 10
    Items.Strings = (
      'K'
      'N'
      'M')
  end
  object editSpeed: TMaskEdit
    Left = 144
    Top = 142
    Width = 33
    Height = 21
    EditMask = '0000;1; '
    MaxLength = 4
    TabOrder = 11
    Text = '    '
  end
  object boxLevelUnits: TComboBox
    Left = 216
    Top = 142
    Width = 33
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 12
    OnChange = boxLevelUnitsChange
    Items.Strings = (
      'F'
      'A'
      'S'
      'M')
  end
  object editLevel: TMaskEdit
    Left = 248
    Top = 142
    Width = 33
    Height = 21
    EditMask = '0000;1; '
    MaxLength = 4
    TabOrder = 13
    Text = '    '
  end
  object boxRoute: TListBox
    Left = 56
    Top = 174
    Width = 169
    Height = 113
    ItemHeight = 13
    TabOrder = 14
    OnClick = boxRouteClick
    OnDblClick = boxRouteDblClick
  end
  object btnAddNavPoint: TButton
    Left = 232
    Top = 174
    Width = 75
    Height = 25
    Caption = 'Add'
    Enabled = False
    TabOrder = 15
    OnClick = btnAddNavPointClick
  end
  object btnRemoveNavPoint: TButton
    Left = 232
    Top = 264
    Width = 75
    Height = 25
    Caption = 'Remove'
    Enabled = False
    TabOrder = 18
    OnClick = btnRemoveNavPointClick
  end
  object editEET: TMaskEdit
    Left = 344
    Top = 292
    Width = 33
    Height = 21
    EditMask = '#0:00;1;_'
    MaxLength = 5
    TabOrder = 21
    Text = '  :  '
  end
  object memoOtherInfo: TMemo
    Left = 120
    Top = 344
    Width = 353
    Height = 89
    TabOrder = 24
  end
  object btnAcknowledge: TButton
    Left = 16
    Top = 480
    Width = 75
    Height = 25
    Caption = 'Acknowledge'
    TabOrder = 27
    OnClick = btnAcknowledgeClick
  end
  object btnCancel: TButton
    Left = 104
    Top = 480
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 28
    OnClick = btnCancelClick
  end
  object DBComboPilot: TDBLookupComboBox
    Left = 112
    Top = 444
    Width = 145
    Height = 21
    DataField = 'KAPITANL_CISLOLETPRUKAZU'
    DataSource = srcPlan
    KeyField = 'CISLOLETPRUKAZU'
    ListField = 'JMENO'
    ListSource = srcPilot
    TabOrder = 25
  end
  object Panel4: TPanel
    Left = 264
    Top = 444
    Width = 217
    Height = 21
    BevelOuter = bvLowered
    TabOrder = 26
    object DBtxtPilotInfo: TDBText
      Left = 1
      Top = 4
      Width = 216
      Height = 13
      DataField = 'CISLOLETPRUKAZU'
      DataSource = srcPilot
      Transparent = True
    end
  end
  object DBComboAircraftID: TDBLookupComboBox
    Left = 136
    Top = 12
    Width = 145
    Height = 21
    DataField = 'LETADLOP_ID'
    DataSource = srcPlan
    KeyField = 'ID'
    ListField = 'ID'
    ListSource = srcRegAircft
    TabOrder = 0
  end
  object comboRules: TDBComboBox
    Left = 88
    Top = 38
    Width = 33
    Height = 21
    Style = csDropDownList
    DataField = 'PREDPIS'
    DataSource = srcPlan
    ItemHeight = 13
    Items.Strings = (
      'I'
      'V'
      'Y'
      'Z')
    TabOrder = 1
  end
  object comboFlightType: TDBComboBox
    Left = 200
    Top = 38
    Width = 33
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Items.Strings = (
      'S'
      'N'
      'G'
      'M'
      'X')
    TabOrder = 2
  end
  object DBComboDepart: TDBLookupComboBox
    Left = 144
    Top = 116
    Width = 153
    Height = 21
    DataField = 'ODLETZ_NAVOBJEK_ID'
    DataSource = srcPlan
    KeyField = 'ID'
    ListField = 'NAZEV'
    ListSource = srcAirport
    TabOrder = 8
  end
  object DBComboDestin: TDBLookupComboBox
    Left = 144
    Top = 292
    Width = 161
    Height = 21
    DataField = 'PRISTANI_NAVOBJEK_ID'
    DataSource = srcPlan
    KeyField = 'ID'
    ListField = 'NAZEV'
    ListSource = srcAirport
    TabOrder = 20
  end
  object DBComboAltn: TDBLookupComboBox
    Left = 144
    Top = 316
    Width = 161
    Height = 21
    KeyField = 'ID'
    ListField = 'NAZEV'
    ListSource = srcAirport
    TabOrder = 22
  end
  object DBComboAltn2: TDBLookupComboBox
    Left = 360
    Top = 316
    Width = 161
    Height = 21
    KeyField = 'ID'
    ListField = 'NAZEV'
    ListSource = srcAirport
    TabOrder = 23
  end
  object boxNavPoints: TListBox
    Left = 312
    Top = 174
    Width = 169
    Height = 113
    ItemHeight = 13
    TabOrder = 19
    OnClick = boxNavPointsClick
    OnDblClick = boxNavPointsDblClick
  end
  object btnMoveUp: TButton
    Left = 232
    Top = 208
    Width = 25
    Height = 17
    Caption = 'á'
    Enabled = False
    Font.Charset = SYMBOL_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Wingdings'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentFont = False
    TabOrder = 16
    OnClick = btnMoveUpClick
  end
  object btnMoveDown: TButton
    Left = 232
    Top = 232
    Width = 25
    Height = 17
    Caption = 'â'
    Enabled = False
    Font.Charset = SYMBOL_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Wingdings'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentFont = False
    TabOrder = 17
    OnClick = btnMoveDownClick
  end
  object srcAircftType: TDataSource
    DataSet = qAircftType
    Left = 368
  end
  object srcRegAircft: TDataSource
    DataSet = qRegAircft
    Left = 400
  end
  object srcAirport: TDataSource
    DataSet = qAirport
    Left = 464
  end
  object srcPilot: TDataSource
    DataSet = qPilot
    Left = 496
  end
  object qAircftType: TQuery
    DatabaseName = 'IBLocal'
    SQL.Strings = (
      'select * from TypLetadla')
    Left = 368
    Top = 32
  end
  object qAirport: TQuery
    DatabaseName = 'IBLocal'
    SQL.Strings = (
      
        'SELECT Nazev,ID,NavObjek_ID FROM Letiste,NavObjekt WHERE ID=NavO' +
        'bjek_ID')
    Left = 464
    Top = 32
  end
  object qPilot: TQuery
    DatabaseName = 'IBLocal'
    SQL.Strings = (
      'select * from Pilot')
    Left = 496
    Top = 32
  end
  object qNavPoint: TQuery
    DatabaseName = 'IBLocal'
    SQL.Strings = (
      
        'SELECT Nazev,ID,NavObjek_ID FROM NavigacniBod,NavObjekt WHERE ID' +
        '=NavObjek_ID')
    Left = 432
    Top = 32
  end
  object srcNavPoint: TDataSource
    DataSet = qNavPoint
    Left = 432
  end
  object qRegAircft: TQuery
    DatabaseName = 'IBLocal'
    SQL.Strings = (
      'select * from RegLetadlo')
    Left = 400
    Top = 32
  end
  object srcPlan: TDataSource
    DataSet = tPlan
    Left = 336
  end
  object tPlan: TTable
    DatabaseName = 'IBLocal'
    TableName = 'LETOVYPLAN'
    Left = 336
    Top = 32
  end
  object qMaxSchvalCislo: TQuery
    DatabaseName = 'IBLocal'
    SQL.Strings = (
      'select MAX(SchvalCislo) MaxCislo from LetovyPlan')
    Left = 464
    Top = 64
  end
  object tPlanPresBod: TTable
    DatabaseName = 'IBLocal'
    TableName = 'PLANPRESBOD'
    Left = 496
    Top = 64
  end
end
