object ColorSelectorFrame: TColorSelectorFrame
  Left = 0
  Top = 0
  Width = 524
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
    Hint = 'Background Color'
    NoneColorColor = clWindow
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnChange = cbBackgroundChange
  end
  object cbForeground: TColorBox
    AlignWithMargins = True
    Left = 228
    Top = 0
    Width = 40
    Height = 22
    Hint = 'Foreground Color'
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
    ParentShowHint = False
    ShowHint = True
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
    TabOrder = 6
    Transparent = False
    StyleElements = []
    OnDblClick = ExampleTextDblClick
  end
  object cbBold: TCheckBox
    Left = 318
    Top = 3
    Width = 23
    Height = 17
    Hint = 'Bold'
    Caption = 'B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = cbBoldClick
  end
  object cbItalic: TCheckBox
    Left = 347
    Top = 3
    Width = 23
    Height = 17
    Hint = 'Italic'
    Caption = 'I'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsItalic]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = cbItalicClick
  end
  object cbUnderline: TCheckBox
    Left = 376
    Top = 3
    Width = 23
    Height = 17
    Hint = 'Underscore'
    Caption = 'U'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = cbUnderlineClick
  end
  object cbStrikeOut: TCheckBox
    Left = 405
    Top = 3
    Width = 41
    Height = 17
    Hint = 'Strikeout'
    Caption = 'S'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsStrikeOut]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
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
