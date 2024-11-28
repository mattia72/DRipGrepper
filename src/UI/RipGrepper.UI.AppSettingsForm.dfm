object AppSettingsForm: TAppSettingsForm
  Left = 0
  Top = 0
  Caption = 'AppSettingsForm'
  ClientHeight = 441
  ClientWidth = 624
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
    Width = 624
    Height = 441
    Align = alClient
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 75
      Width = 127
      Height = 15
      Caption = 'Match color foreground'
    end
    object Label2: TLabel
      Left = 304
      Top = 75
      Width = 67
      Height = 15
      Caption = 'background:'
    end
    object ColorBox1: TColorBox
      Left = 149
      Top = 72
      Width = 145
      Height = 22
      TabOrder = 0
    end
    object chDebugTrace: TCheckBox
      Left = 16
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Debug trace'
      TabOrder = 1
    end
    object chExpertMode: TCheckBox
      Left = 16
      Top = 39
      Width = 97
      Height = 17
      Caption = 'Expert Mode'
      TabOrder = 2
    end
    object ColorBox2: TColorBox
      Left = 377
      Top = 72
      Width = 145
      Height = 22
      TabOrder = 3
    end
  end
end
