object ColorSelectorFrame: TColorSelectorFrame
  Left = 0
  Top = 0
  Width = 458
  Height = 25
  TabOrder = 0
  object LabelText: TLabel
    Left = 119
    Top = 3
    Width = 49
    Height = 15
    Alignment = taRightJustify
    Caption = 'LabelText'
  end
  object cbBackground: TColorBox
    AlignWithMargins = True
    Left = 273
    Top = 0
    Width = 40
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbPrettyNames, cbCustomColors]
    TabOrder = 0
    OnChange = cbBackgroundChange
  end
  object cbForeground: TColorBox
    AlignWithMargins = True
    Left = 228
    Top = 0
    Width = 40
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbPrettyNames, cbCustomColors]
    TabOrder = 1
    OnChange = cbForegroundChange
  end
  object ExampleText: TStaticText
    AlignWithMargins = True
    Left = 174
    Top = 1
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
    Left = 318
    Top = 3
    Width = 30
    Height = 17
    Caption = 'B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = cbBoldClick
  end
  object cbItalic: TCheckBox
    Left = 355
    Top = 3
    Width = 30
    Height = 17
    Caption = 'I'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsItalic]
    ParentFont = False
    TabOrder = 4
    OnClick = cbItalicClick
  end
  object cbUnderline: TCheckBox
    Left = 388
    Top = 3
    Width = 30
    Height = 17
    Caption = 'U'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsUnderline]
    ParentFont = False
    TabOrder = 5
    OnClick = cbUnderlineClick
  end
  object cbStrikeOut: TCheckBox
    Left = 425
    Top = 3
    Width = 30
    Height = 17
    Caption = 'S'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsStrikeOut]
    ParentFont = False
    TabOrder = 6
    OnClick = cbStrikeOutClick
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Left = 5
    Top = 65531
  end
end
