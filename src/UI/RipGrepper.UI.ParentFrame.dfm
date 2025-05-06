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
  TabOrder = 0
  inline TopFrame: TRipGrepperTopFrame
    Left = 0
    Top = 0
    Width = 851
    Height = 31
    Align = alTop
    ParentBackground = False
    TabOrder = 2
    StyleElements = [seFont, seBorder]
    inherited pnlTop: TPanel
      Width = 851
      Height = 31
      inherited tbarConfig: TToolBar
        Left = 815
        Height = 25
      end
    end
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
    ParentBackground = False
    TabOrder = 1
    inherited panelMain: TPanel
      Width = 851
      Height = 429
      inherited SplitView1: TSplitView
        Height = 429
        inherited Splitter1: TSplitter
          Height = 429
        end
        inherited PanelHistory: TPanel
          Height = 429
          inherited MiddleLeftFrame1: TMiddleLeftFrame
            Height = 429
            inherited Panel1: TPanel
              Height = 429
              inherited VstHistory: TVirtualStringTree
                Height = 423
                DefaultText = ''
              end
            end
          end
        end
        inherited PanelResult: TPanel
          Height = 429
          inherited VstResult: TVirtualStringTree
            Height = 423
          end
        end
      end
    end
    inherited PopupMenuHistory: TPopupMenu
      Images = MainFrame.MiddleLeftFrame1.SVGIconImageList1
    end
  end
  inline BottomFrame: TRipGrepperBottomFrame
    Left = 0
    Top = 460
    Width = 851
    Height = 30
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alBottom
    TabOrder = 0
    inherited pnlBottom: TPanel
      Width = 851
      Height = 30
      inherited StatusBar1: TStatusBar
        Width = 845
        Height = 27
      end
      inherited ActivityIndicator1: TActivityIndicator
        Left = 365
        Top = 3
      end
    end
  end
end
