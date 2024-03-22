object AllFrames: TAllFrames
  Left = 0
  Top = 0
  Width = 1453
  Height = 712
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  Align = alClient
  TabOrder = 0
  PixelsPerInch = 96
  inline TopFrame: TRipGrepperTopFrame
    Left = 0
    Top = 0
    Width = 1453
    Height = 60
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont =True
    TabOrder = 2
  end
  inline BottomFrame: TRipGrepperBottomFrame
    Left = 0
    Top = 652
    Width = 1453
    Height = 60
    Margins.Left = 20
    Margins.Top = 20
    Margins.Right = 20
    Margins.Bottom = 20
    Align = alBottom
    TabOrder = 0
  end
  inline MainFrame: TRipGrepperMainFrame
    Left = 0
    Top = 60
    Width = 1453
    Height = 592
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
    ParentFont =True
    TabOrder = 1
  end
end
