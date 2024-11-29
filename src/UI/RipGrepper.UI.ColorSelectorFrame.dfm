object ColorSelectorFrame: TColorSelectorFrame
  Left = 0
  Top = 0
  Width = 590
  Height = 27
  TabOrder = 0
  object LabelText: TLabel
    Left = 3
    Top = 3
    Width = 49
    Height = 15
    Alignment = taRightJustify
    Caption = 'LabelText'
  end
  object cbBackground: TColorBox
    AlignWithMargins = True
    Left = 157
    Top = 0
    Width = 39
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbPrettyNames, cbCustomColors]
    TabOrder = 0
    OnChange = cbBackgroundChange
  end
  object cbForeground: TColorBox
    AlignWithMargins = True
    Left = 112
    Top = 0
    Width = 39
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbPrettyNames, cbCustomColors]
    TabOrder = 1
    OnChange = cbForegroundChange
  end
  object ExampleText: TStaticText
    AlignWithMargins = True
    Left = 58
    Top = 4
    Width = 48
    Height = 19
    Alignment = taCenter
    BorderStyle = sbsSingle
    Caption = 'Example'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 2
    Transparent = False
    StyleElements = [seClient, seBorder]
    OnDblClick = ExampleTextDblClick
  end
  object cbBold: TCheckBox
    Left = 202
    Top = 3
    Width = 46
    Height = 17
    Caption = 'Bold'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
  end
  object cbItalic: TCheckBox
    Left = 254
    Top = 3
    Width = 46
    Height = 17
    Caption = 'Italic'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsItalic]
    ParentFont = False
    TabOrder = 4
  end
  object cbUnderline: TCheckBox
    Left = 306
    Top = 3
    Width = 77
    Height = 17
    Caption = 'UnderLine'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsUnderline]
    ParentFont = False
    TabOrder = 5
  end
  object cbStrikeOut: TCheckBox
    Left = 389
    Top = 3
    Width = 74
    Height = 17
    Caption = 'StrikeOut'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsStrikeOut]
    ParentFont = False
    TabOrder = 6
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Left = 493
    Top = 3
  end
end
