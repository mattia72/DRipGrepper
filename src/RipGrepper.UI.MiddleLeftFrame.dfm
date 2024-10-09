object MiddleLeftFrame: TMiddleLeftFrame
  Left = 0
  Top = 0
  Width = 609
  Height = 211
  TabOrder = 0
  object VstHistory: TVirtualStringTree
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 603
    Height = 205
    Align = alClient
    Colors.BorderColor = 15987699
    Colors.DisabledColor = clGray
    Colors.DropMarkColor = 15385233
    Colors.DropTargetColor = 15385233
    Colors.DropTargetBorderColor = 15385233
    Colors.FocusedSelectionColor = 15385233
    Colors.FocusedSelectionBorderColor = 15385233
    Colors.GridLineColor = 15987699
    Colors.HeaderHotColor = clBlack
    Colors.HotColor = clBlack
    Colors.SelectionRectangleBlendColor = 15385233
    Colors.SelectionRectangleBorderColor = 15385233
    Colors.SelectionTextColor = clBlack
    Colors.TreeLineColor = 9471874
    Colors.UnfocusedColor = clGray
    Colors.UnfocusedSelectionColor = clWhite
    Colors.UnfocusedSelectionBorderColor = clWhite
    Header.AutoSizeIndex = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible, hoFullRepaintOnResize, hoHeaderClickAutoSort, hoAutoResizeInclCaption]
    HintMode = hmHint
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    TreeOptions.PaintOptions = [toShowBackground, toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect, toSiblingSelectConstraint]
    OnBeforeCellPaint = VstHistoryBeforeCellPaint
    OnFreeNode = VstHistoryFreeNode
    OnGetText = VstHistoryGetText
    OnPaintText = VstHistoryPaintText
    OnGetHintKind = VstHistoryGetHintKind
    OnGetHint = VstHistoryGetHint
    OnNodeClick = VstHistoryNodeClick
    OnNodeDblClick = VstHistoryNodeDblClick
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'Search'
        Width = 599
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coSmartResize, coAllowFocus, coEditable, coStyleColor]
        Position = 1
        Text = 'Replace'
        Width = 519
      end>
    DefaultText = ''
  end
  object ActionList: TActionList
    Left = 251
    Top = 30
    object ActionHistoryDelete: TAction
      Caption = 'Delete'
      ImageIndex = 2
      OnExecute = ActionHistoryDeleteExecute
      OnUpdate = ActionHistoryDeleteUpdate
    end
    object ActionHistoryDeleteAll: TAction
      Caption = 'Delete All'
      ImageIndex = 3
      OnExecute = ActionHistoryDeleteAllExecute
      OnUpdate = ActionHistoryDeleteAllUpdate
    end
    object ActionCopyCmdLineToClipboard: TAction
      Caption = 'Copy Command Line'
      ImageIndex = 0
    end
    object ActionOpenSearchForm: TAction
      Caption = 'Open Search Form...'
      ImageIndex = 0
      OnExecute = ActionOpenSearchFormExecute
    end
  end
end
