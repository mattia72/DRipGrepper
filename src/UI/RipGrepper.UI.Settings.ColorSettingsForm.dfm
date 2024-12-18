object ColorSettingsForm: TColorSettingsForm
  Left = 0
  Top = 0
  Caption = 'Fonts and Colors'
  ClientHeight = 155
  ClientWidth = 660
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 15
  object pnlBottom: TPanel
    Left = 0
    Top = 114
    Width = 660
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    StyleElements = [seFont, seClient]
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
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 660
    Height = 114
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    StyleElements = [seFont, seClient]
    object grpFontColors: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 654
      Height = 108
      Align = alClient
      Caption = 'Fonts && Colors'
      TabOrder = 0
    end
  end
end
