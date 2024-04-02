object ParentFrame: TParentFrame
  Left = 0
  Top = 0
  Width = 640
  Height = 480
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
    Width = 640
    Height = 31
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alTop
    TabOrder = 0
    inherited ToolBar1: TToolBar
      Width = 634
    end
  end
  inline MainFrame: TRipGrepperMiddleFrame
    Left = 0
    Top = 31
    Width = 640
    Height = 419
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    TabOrder = 1
    inherited panelMain: TPanel
      Width = 634
      Height = 413
      inherited SplitView1: TSplitView
        Height = 413
        inherited Splitter1: TSplitter
          Height = 413
        end
        inherited PanelHistory: TPanel
          Height = 413
          inherited ListBoxSearchHistory: TListBox
            Height = 395
          end
        end
        inherited PanelResult: TPanel
          Height = 413
          inherited VstResult: TVirtualStringTree
            Height = 405
          end
        end
      end
    end
  end
  inline BottomFrame: TRipGrepperBottomFrame
    Left = 0
    Top = 450
    Width = 640
    Height = 30
    Margins.Left = 20
    Margins.Top = 20
    Margins.Right = 20
    Margins.Bottom = 20
    Align = alBottom
    TabOrder = 2
    inherited pnlBottom: TPanel
      Width = 640
      inherited StatusBar1: TStatusBar
        Width = 640
      end
      inherited ActivityIndicator1: TActivityIndicator
        Left = 346
      end
    end
  end
end
