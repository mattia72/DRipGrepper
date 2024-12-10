object ExtensionSettingsForm: TExtensionSettingsForm
  Left = 0
  Top = 0
  Caption = 'ExtensionSettingsForm'
  ClientHeight = 185
  ClientWidth = 599
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 15
  object pnlMiddle: TPanel
    Left = 0
    Top = 0
    Width = 599
    Height = 185
    Align = alClient
    TabOrder = 0
    DesignSize = (
      599
      185)
    object grpShortcuts: TGroupBox
      Left = 14
      Top = 10
      Width = 575
      Height = 91
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Shortcuts'
      TabOrder = 0
      object lblOpenWith: TLabel
        Left = 36
        Top = 28
        Width = 60
        Height = 15
        Caption = 'Open With:'
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
      object hkedtSearchSelected: THotKey
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
