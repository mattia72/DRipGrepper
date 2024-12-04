object AppSettingsForm: TAppSettingsForm
  Left = 0
  Top = 0
  Caption = 'AppSettingsForm'
  ClientHeight = 139
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 537
    Height = 139
    Align = alClient
    TabOrder = 0
    object grpFontColors: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 529
      Height = 55
      Align = alTop
      Caption = 'Fonts and Colors'
      TabOrder = 0
    end
    object grpDeveloper: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 65
      Width = 529
      Height = 64
      Align = alTop
      Caption = 'Developer'
      TabOrder = 1
      object chDebugTrace: TCheckBox
        Left = 16
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Debug trace'
        TabOrder = 0
      end
      object chExpertMode: TCheckBox
        Left = 16
        Top = 39
        Width = 97
        Height = 17
        Caption = 'Expert Mode'
        TabOrder = 1
      end
    end
  end
end
