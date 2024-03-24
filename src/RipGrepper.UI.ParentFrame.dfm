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
    inherited ToolBar1: TToolBar
      Width = 1447
    end
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
    inherited panelMain: TPanel
      Width = 1447
      Height = 645
      inherited SplitView1: TSplitView
        Height = 645
        inherited Splitter1: TSplitter
          Height = 645
        end
        inherited PanelHistory: TPanel
          Height = 645
          inherited ListBoxSearchHistory: TListBox
            Height = 627
          end
        end
        inherited PanelResult: TPanel
          Height = 645
        end
      end
    end
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
    inherited pnlBottom: TPanel
      Width = 1453
    end
  end
end
