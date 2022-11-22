object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 299
  ClientWidth = 635
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
  object btnConsoleRichEdit: TButton
    Left = 10
    Top = 10
    Width = 159
    Height = 25
    Caption = 'Console (RichEdit)'
    TabOrder = 0
    OnClick = btnConsoleRichEditClick
  end
  object btnDatabase: TButton
    Left = 10
    Top = 41
    Width = 159
    Height = 25
    Caption = 'Base de dados'
    TabOrder = 1
    OnClick = btnDatabaseClick
  end
  object btnXml: TButton
    Left = 10
    Top = 72
    Width = 159
    Height = 25
    Caption = 'Xml'
    TabOrder = 2
    OnClick = btnXmlClick
  end
end
