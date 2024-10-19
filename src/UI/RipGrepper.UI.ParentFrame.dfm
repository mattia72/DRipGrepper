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
    Align = alTop
    TabOrder = 2
    inherited tbarConfig: TToolBar
      Left = 815
      Height = 25
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
          inherited MiddleLeftFrame1: TMiddleLeftFrame
            Height = 421
            inherited VstHistory: TVirtualStringTree
              Height = 415
              Columns = <
                item
                  Position = 0
                  Text = 'Search'
                  Width = 179
                end
                item
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coSmartResize, coAllowFocus, coEditable, coStyleColor]
                  Position = 1
                  Text = 'Replace'
                  Width = 519
                end>
              DefaultText = ''
            end
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
