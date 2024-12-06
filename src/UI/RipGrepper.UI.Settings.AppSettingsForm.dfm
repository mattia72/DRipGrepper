object AppSettingsForm: TAppSettingsForm
  Left = 0
  Top = 0
  Caption = 'AppSettingsForm'
  ClientHeight = 196
  ClientWidth = 522
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
    Width = 522
    Height = 196
    Align = alClient
    TabOrder = 0
    object grpDeveloper: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 514
      Height = 188
      Align = alClient
      Caption = 'Developer'
      TabOrder = 0
      DesignSize = (
        514
        188)
      object chDebugTrace: TCheckBox
        Left = 16
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Debug trace'
        TabOrder = 0
      end
      object chExpertMode: TCheckBox
        Left = 16
        Top = 47
        Width = 97
        Height = 17
        Caption = 'Expert Mode'
        TabOrder = 1
      end
      object lbledtIniFilePath: TLabeledEdit
        Left = 16
        Top = 152
        Width = 495
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 64
        EditLabel.Height = 15
        EditLabel.Caption = 'Ini File Path:'
        Enabled = False
        TabOrder = 2
        Text = ''
      end
      object lbledtRgExePath: TLabeledEdit
        Left = 16
        Top = 107
        Width = 495
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 64
        EditLabel.Height = 15
        EditLabel.Caption = 'Rg.exe Path:'
        Enabled = False
        TabOrder = 3
        Text = ''
      end
    end
  end
end
