object FormDatabase: TFormDatabase
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'FormDatabase'
  ClientHeight = 379
  ClientWidth = 718
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pcMain: TPageControl
    Left = 8
    Top = 8
    Width = 697
    Height = 337
    ActivePage = tsResults
    TabOrder = 0
    object tsConnect: TTabSheet
      Caption = 'Conex'#227'o'
      object rgDBType: TRadioGroup
        Left = 10
        Top = 10
        Width = 175
        Height = 80
        Caption = 'Tipo do banco de dados'
        Items.Strings = (
          'PostgreSql'
          'Firebird')
        TabOrder = 0
      end
      object lbeServerHost: TLabeledEdit
        Left = 200
        Top = 25
        Width = 215
        Height = 21
        EditLabel.Width = 80
        EditLabel.Height = 13
        EditLabel.Caption = 'Host do Servidor'
        TabOrder = 1
      end
      object lbeServerPort: TLabeledEdit
        Left = 425
        Top = 25
        Width = 75
        Height = 21
        EditLabel.Width = 84
        EditLabel.Height = 13
        EditLabel.Caption = 'Porta do Servidor'
        NumbersOnly = True
        TabOrder = 2
      end
      object lbeDatabase: TLabeledEdit
        Left = 200
        Top = 70
        Width = 300
        Height = 21
        EditLabel.Width = 70
        EditLabel.Height = 13
        EditLabel.Caption = 'Base de dados'
        TabOrder = 3
      end
      object lbeUserName: TLabeledEdit
        Left = 200
        Top = 110
        Width = 145
        Height = 21
        EditLabel.Width = 36
        EditLabel.Height = 13
        EditLabel.Caption = 'Usu'#225'rio'
        TabOrder = 4
      end
      object lbePassword: TLabeledEdit
        Left = 355
        Top = 110
        Width = 145
        Height = 21
        EditLabel.Width = 30
        EditLabel.Height = 13
        EditLabel.Caption = 'Senha'
        PasswordChar = '*'
        TabOrder = 5
      end
      object btnConnect: TButton
        Left = 8
        Top = 96
        Width = 100
        Height = 25
        Caption = 'Conectar'
        TabOrder = 6
        OnClick = btnConnectClick
      end
      object btnDisconnect: TButton
        Left = 8
        Top = 127
        Width = 100
        Height = 25
        Caption = 'Desconectar'
        Enabled = False
        TabOrder = 7
        OnClick = btnDisconnectClick
      end
    end
    object tsSQL: TTabSheet
      Caption = 'SQL'
      ImageIndex = 1
      object pnSqlTopBar: TPanel
        Left = 0
        Top = 0
        Width = 689
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 80
        ExplicitTop = 48
        ExplicitWidth = 185
        object btnSqlExec: TButton
          Left = 8
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Executar'
          TabOrder = 0
          OnClick = btnSqlExecClick
        end
      end
      object reSQL: TRichEdit
        Left = 0
        Top = 41
        Width = 689
        Height = 268
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Courier New'
        Font.Style = []
        HideSelection = False
        HideScrollBars = False
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 1
        Zoom = 100
      end
    end
    object tsResults: TTabSheet
      Caption = 'Resultado'
      ImageIndex = 2
      object synResult: TSynEdit
        Left = 0
        Top = 40
        Width = 689
        Height = 269
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        CodeFolding.GutterShapeSize = 11
        CodeFolding.CollapsedLineColor = clGrayText
        CodeFolding.FolderBarLinesColor = clGrayText
        CodeFolding.IndentGuidesColor = clGray
        CodeFolding.IndentGuides = True
        CodeFolding.ShowCollapsedLine = False
        CodeFolding.ShowHintMark = True
        UseCodeFolding = False
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Highlighter = SynJSONSyn1
        FontSmoothing = fsmNone
        ExplicitLeft = 56
        ExplicitTop = 48
        ExplicitWidth = 200
        ExplicitHeight = 150
      end
      object pnResultsTopBar: TPanel
        Left = 0
        Top = 0
        Width = 689
        Height = 40
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object lblResultTime: TLabel
          Left = 119
          Top = 15
          Width = 62
          Height = 13
          Caption = 'lblResultTime'
        end
        object btnSaveAs: TButton
          Left = 10
          Top = 9
          Width = 100
          Height = 25
          Caption = 'Salvar Como..'
          TabOrder = 0
          OnClick = btnSaveAsClick
        end
      end
    end
  end
  object SynJSONSyn1: TSynJSONSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 260
    Top = 120
  end
  object SaveDialog: TSaveDialog
    Filter = 'Arquivo JSON|*.json'
    Left = 52
    Top = 104
  end
end
