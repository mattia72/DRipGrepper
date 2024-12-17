object ColorSettingsForm: TColorSettingsForm
  Left = 0
  Top = 0
  Caption = 'Fonts and Colors'
  ClientHeight = 63
  ClientWidth = 660
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 15
  object grpFontColors: TGroupBox
    Left = 0
    Top = 0
    Width = 660
    Height = 63
    Align = alClient
    TabOrder = 0
    object Panel1: TPanel
      Left = 2
      Top = 20
      Width = 656
      Height = 41
      Align = alBottom
      TabOrder = 0
      object Button1: TButton
        Left = 290
        Top = 10
        Width = 96
        Height = 25
        Caption = 'Load Defaults'
        TabOrder = 0
        OnClick = Button1Click
      end
    end
  end
end
