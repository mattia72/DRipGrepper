object Frames: TFrames
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
  PixelsPerInch = 240
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
    inherited pnlBottom: TPanel
      Width = 1453
      Height = 60
      Margins.Left = 20
      Margins.Top = 20
      Margins.Right = 20
      Margins.Bottom = 20
      inherited StatusBar1: TStatusBar
        Width = 1453
        Height = 60
        Margins.Left = 20
        Margins.Top = 20
        Margins.Right = 200
        Margins.Bottom = 20
        Panels = <
          item
            Width = 750
          end
          item
            Alignment = taCenter
            Text = 'READY'
            Width = 438
          end
          item
            Alignment = taRightJustify
            Text = 'RipGrepper v1.0.0      '
            Width = 938
          end>
      end
      inherited ActivityIndicator1: TActivityIndicator
        Left = 1388
        Top = 10
      end
    end
    inherited ActionList: TActionList
      Left = 898
      Top = 3
    end
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
    Font.Height = -30
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    inherited panelMain: TPanel
      Width = 1437
      Height = 576
      inherited SplitView1: TSplitView
        Height = 560
        inherited Splitter1: TSplitter
          Height = 560
        end
        inherited PanelHistory: TPanel
          Height = 560
          inherited ListBoxSearchHistory: TListBox
            Height = 542
          end
        end
        inherited PanelResult: TPanel
          Height = 560
          inherited ListViewResult: TListView
            Height = 542
          end
        end
      end
    end
  end
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
    Font.Height = -30
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    inherited ToolBar1: TToolBar
      Width = 1437
    end
    inherited ImageListButtons: TImageList
      Left = 1122
    end
  end
end
