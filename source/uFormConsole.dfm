object FormConsole: TFormConsole
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Console'
  ClientHeight = 531
  ClientWidth = 889
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
  object reOutput: TRichEdit
    Left = 0
    Top = 60
    Width = 889
    Height = 471
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    HideSelection = False
    HideScrollBars = False
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    Zoom = 100
  end
  object pnTopBar: TPanel
    Left = 0
    Top = 0
    Width = 889
    Height = 60
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lbeCommandLine: TLabeledEdit
      Left = 10
      Top = 25
      Width = 250
      Height = 21
      EditLabel.Width = 86
      EditLabel.Height = 13
      EditLabel.Caption = 'Linha de comando'
      TabOrder = 0
      Text = 'cmd.exe /c dir c:\'
    end
    object btnExecute: TButton
      Left = 272
      Top = 23
      Width = 75
      Height = 25
      Caption = 'Executar'
      TabOrder = 1
      OnClick = btnExecuteClick
    end
    object cbxRemoveAdministrativePermissions: TCheckBox
      Left = 128
      Top = 2
      Width = 200
      Height = 17
      Caption = 'Remover permiss'#245'es de administrador'
      TabOrder = 2
    end
  end
  object TimerCheck: TTimer
    Enabled = False
    Interval = 250
    OnTimer = TimerCheckTimer
    Left = 344
    Top = 168
  end
end
