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
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alTop
    TabOrder = 0
  end
  inline MainFrame: TRipGrepperMiddleFrame
    Left = 0
    Top = 31
    Width = 851
    Height = 429
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    TabOrder = 1
    inherited panelMain: TPanel
      Width = 845
      Height = 423
      inherited SplitView1: TSplitView
        Height = 423
        inherited Splitter1: TSplitter
          Height = 423
        end
        inherited PanelHistory: TPanel
          Height = 423
          inherited ListBoxSearchHistory: TListBox
            Height = 405
          end
        end
        inherited PanelResult: TPanel
          Height = 423
          inherited VstResult: TVirtualStringTree
            Height = 415
          end
        end
      end
    end
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
    inherited pnlBottom: TPanel
      Width = 851
      Height = 30
      inherited StatusBar1: TStatusBar
        Width = 845
        Height = 27
      end
      inherited ActivityIndicator1: TActivityIndicator
        Left = 557
      end
    end
  end
end
