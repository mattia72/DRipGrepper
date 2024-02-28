object RipGrepperBottomFrame: TRipGrepperBottomFrame
  Left = 0
  Top = 0
  Width = 1600
  Height = 60
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  Align = alBottom
  TabOrder = 0
  PixelsPerInch = 240
  object pnlBottom: TPanel
    Left = 0
    Top = 0
    Width = 1600
    Height = 60
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      1600
      60)
    object StatusBar1: TStatusBar
      AlignWithMargins = True
      Left = 8
      Top = -11
      Width = 1512
      Height = 63
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 80
      Margins.Bottom = 8
      Action = ActionStatusBar
      Panels = <
        item
          Width = 500
        end
        item
          Alignment = taCenter
          Text = 'READY'
          Width = 175
        end
        item
          Alignment = taRightJustify
          Text = 'RipGrepper v1.0.0      '
          Width = 375
        end>
    end
    object ActivityIndicator1: TActivityIndicator
      Left = 1535
      Top = 0
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Anchors = [akRight, akBottom]
      IndicatorSize = aisSmall
    end
  end
  object ActionList: TActionList
    Left = 807
    Top = 65526
    object ActionStatusBar: TAction
      OnUpdate = ActionStatusBarUpdate
    end
  end
end
