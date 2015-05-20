object frmGameMain: TfrmGameMain
  Left = 191
  Top = 107
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Quoridor'
  ClientHeight = 456
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object GameGrid: TDrawGrid
    Left = 0
    Top = 0
    Width = 489
    Height = 337
    ColCount = 9
    DefaultColWidth = 20
    DefaultRowHeight = 20
    DefaultDrawing = False
    FixedCols = 0
    RowCount = 9
    FixedRows = 0
    ScrollBars = ssNone
    TabOrder = 0
    OnClick = GameGridClick
    OnDrawCell = GameGridDrawCell
    OnKeyDown = FormKeyDown
    OnMouseMove = GameGridMouseMove
  end
  object pnlGameInfo: TPanel
    Left = 648
    Top = 0
    Width = 40
    Height = 437
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object lblBarriers1: TLabel
      Left = 8
      Top = 0
      Width = 18
      Height = 19
      Caption = '10'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblBarriers0: TLabel
      Left = 8
      Top = 312
      Width = 18
      Height = 19
      Caption = '10'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 437
    Width = 688
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
end
