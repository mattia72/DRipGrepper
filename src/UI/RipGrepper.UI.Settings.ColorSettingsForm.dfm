object ColorSettingsForm: TColorSettingsForm
  Left = 0
  Top = 0
  Caption = 'Fonts and Colors'
  ClientHeight = 177
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
    Top = 136
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
    Height = 136
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
      Height = 130
      Align = alClient
      Caption = 'Fonts && Colors'
      TabOrder = 0
      object ScrollBox1: TScrollBox
        Left = 2
        Top = 17
        Width = 650
        Height = 111
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 0
      end
    end
  end
end
