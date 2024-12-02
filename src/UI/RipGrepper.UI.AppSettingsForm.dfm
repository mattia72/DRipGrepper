object AppSettingsForm: TAppSettingsForm
  Left = 0
  Top = 0
  Caption = 'AppSettingsForm'
  ClientHeight = 388
  ClientWidth = 836
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 836
    Height = 388
    Align = alClient
    TabOrder = 0
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
  inline ColorSelectorFrame1: TColorSelectorFrame
    Left = 8
    Top = 96
    Width = 408
    Height = 27
    TabOrder = 1
    inherited LabelText: TLabel
      StyleElements = [seFont, seClient, seBorder]
    end
    inherited cbBackground: TColorBox
      StyleElements = [seFont, seClient, seBorder]
    end
    inherited cbForeground: TColorBox
      StyleElements = [seFont, seClient, seBorder]
    end
  end
end
