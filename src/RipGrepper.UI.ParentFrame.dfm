object ParentFrame: TParentFrame
  Left = 0
  Top = 0
  Width = 851
  Height = 490
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  Align = alClient
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ParentFont = False
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  inline TopFrame: TRipGrepperTopFrame
    Left = 0
    Top = 0
    Width = 851
    Height = 31
  end
  inline MainFrame: TRipGrepperMiddleFrame
    Left = 0
    Top = 31
    Width = 851
    Height = 429
    TabOrder = 1
  end
  inline BottomFrame: TRipGrepperBottomFrame
    Left = 0
    Top = 460
    Width = 851
    Height = 30
    Margins.Left = 20
    Margins.Top = 20
    Margins.Right = 20
    Margins.Bottom = 20
    Align = alBottom
    TabOrder = 2
  end
end
