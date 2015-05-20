object frmMain: TfrmMain
  Left = 213
  Top = 195
  BorderStyle = bsDialog
  Caption = 'Delphi Component Help Creator'
  ClientHeight = 206
  ClientWidth = 493
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 493
    Height = 206
    ActivePage = tabFiles
    Align = alClient
    MultiLine = True
    TabOrder = 0
    object tabFiles: TTabSheet
      Caption = '&Files'
      object Label1: TLabel
        Left = 58
        Top = 20
        Width = 53
        Height = 13
        Caption = '&Source file:'
      end
      object Label2: TLabel
        Left = 15
        Top = 44
        Width = 96
        Height = 13
        Caption = 'Destination &RTF file:'
      end
      object Label3: TLabel
        Left = 16
        Top = 68
        Width = 95
        Height = 13
        Caption = 'Destination &HPJ file:'
      end
      object btnStart: TButton
        Left = 392
        Top = 144
        Width = 75
        Height = 25
        Caption = '&Start!'
        Default = True
        Enabled = False
        TabOrder = 3
        OnClick = btnStartClick
      end
      object editSource: TEdit
        Left = 120
        Top = 16
        Width = 353
        Height = 21
        TabOrder = 0
        OnChange = editSourceChange
      end
      object editRTF: TEdit
        Left = 120
        Top = 40
        Width = 353
        Height = 21
        TabOrder = 1
      end
      object editHPJ: TEdit
        Left = 120
        Top = 64
        Width = 353
        Height = 21
        TabOrder = 2
      end
    end
    object tabSearchPath: TTabSheet
      Caption = 'Search &path'
      ImageIndex = 1
      object boxSearchPaths: TListBox
        Left = 8
        Top = 8
        Width = 413
        Height = 129
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
        OnClick = boxSearchPathsClick
      end
      object btnAdd: TButton
        Left = 428
        Top = 144
        Width = 57
        Height = 21
        Anchors = [akRight, akBottom]
        Caption = '&Add'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = btnAddClick
      end
      object btnMoveUp: TButton
        Left = 428
        Top = 8
        Width = 57
        Height = 21
        Anchors = [akRight, akBottom]
        Caption = '&Up'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = btnMoveUpClick
      end
      object btnMoveDown: TButton
        Left = 428
        Top = 32
        Width = 57
        Height = 21
        Anchors = [akRight, akBottom]
        Caption = '&Down'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        OnClick = btnMoveDownClick
      end
      object btnDelete: TButton
        Left = 428
        Top = 64
        Width = 57
        Height = 21
        Anchors = [akRight, akBottom]
        Caption = 'De&lete'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        OnClick = btnDeleteClick
      end
      object editDirectory: TEdit
        Left = 8
        Top = 144
        Width = 409
        Height = 21
        TabOrder = 1
        OnChange = editDirectoryChange
      end
    end
    object tabAbout: TTabSheet
      Caption = '&About'
      ImageIndex = 2
      object Label4: TLabel
        Left = 105
        Top = 45
        Width = 275
        Height = 23
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Delphi Component Help Creator'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
      end
      object Label5: TLabel
        Left = 208
        Top = 80
        Width = 56
        Height = 13
        Alignment = taCenter
        Anchors = [akLeft, akRight]
        Caption = 'version 0.1'
      end
      object Label6: TLabel
        Left = 160
        Top = 104
        Width = 147
        Height = 13
        Alignment = taCenter
        Anchors = [akLeft, akRight]
        Caption = 'Copyright © DragonSoft, 2001'
      end
    end
  end
end
