object ExtensionSettingsForm: TExtensionSettingsForm
  Left = 0
  Top = 0
  Caption = 'ExtensionSettingsForm'
  ClientHeight = 312
  ClientWidth = 566
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object pnlMiddle: TPanel
    Left = 0
    Top = 0
    Width = 566
    Height = 312
    Align = alClient
    TabOrder = 0
    object grpShortcuts: TGroupBox
      Left = 24
      Top = 40
      Width = 417
      Height = 105
      Caption = 'Shortcuts'
      TabOrder = 0
      object lblOpenWith: TLabel
        Left = 36
        Top = 28
        Width = 60
        Height = 15
        Caption = 'Open With '
      end
      object lblSearch: TLabel
        Left = 16
        Top = 53
        Width = 85
        Height = 15
        Caption = 'Search Selected:'
      end
      object hkedtOpenWidth: THotKey
        Left = 114
        Top = 22
        Width = 121
        Height = 23
        HotKey = 32833
        TabOrder = 0
      end
      object HotKey1: THotKey
        Left = 114
        Top = 51
        Width = 121
        Height = 23
        HotKey = 32833
        TabOrder = 1
      end
    end
  end
end
