object ParentFrame: TParentFrame
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
  inline TopFrame: TRipGrepperTopFrame
    Left = 0
    Top = 0
    Width = 1453
    Height = 31
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alTop
    TabOrder = 0
  end
  inline MainFrame: TRipGrepperMainFrame
    Left = 0
    Top = 31
    Width = 1453
    Height = 651
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    TabOrder = 1
  end
  inline BottomFrame: TRipGrepperBottomFrame
    Left = 0
    Top = 682
    Width = 1453
    Height = 30
    Margins.Left = 20
    Margins.Top = 20
    Margins.Right = 20
    Margins.Bottom = 20
    Align = alBottom
    TabOrder = 2
  end
end
