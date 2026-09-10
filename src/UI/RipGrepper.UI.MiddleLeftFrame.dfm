object MiddleLeftFrame: TMiddleLeftFrame
  Left = 0
  Top = 0
  Width = 609
  Height = 211
  ParentBackground = False
  TabOrder = 0
  StyleElements = [seFont, seClient]
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 609
    Height = 211
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    StyleElements = [seFont, seClient]
    object VstHistory: TVirtualStringTree
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 603
      Height = 205
      Align = alClient
      DragMode = dmAutomatic
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
      Images = SVGIconImageList1
      ParentShowHint = False
      PopupMenu = PopupMenuHistory
      ShowHint = True
      TabOrder = 0
      TreeOptions.PaintOptions = [toShowBackground, toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toFullRowSelect, toSiblingSelectConstraint, toAlwaysSelectNode]
      OnBeforeCellPaint = VstHistoryBeforeCellPaint
      OnDragAllowed = VstHistoryDragAllowed
      OnDragDrop = VstHistoryDragDrop
      OnDragOver = VstHistoryDragOver
      OnFreeNode = VstHistoryFreeNode
      OnGetText = VstHistoryGetText
      OnPaintText = VstHistoryPaintText
      OnGetHintKind = VstHistoryGetHintKind
      OnGetImageIndex = VstHistoryGetImageIndex
      OnGetHint = VstHistoryGetHint
      OnLoadTree = VstHistoryLoadTree
      OnNodeClick = VstHistoryNodeClick
      OnNodeDblClick = VstHistoryNodeDblClick
      OnSaveTree = VstHistorySaveTree
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
  end
  object ActionList: TActionList
    Images = SVGIconImageList1
    Left = 251
    Top = 30
    object ActionHistoryDelete: TAction
      Caption = 'Delete'
      Hint = 'Delete Selected Item'
      ImageIndex = 4
      ImageName = 'close-alt'
      ShortCut = 46
      OnExecute = ActionHistoryDeleteExecute
      OnUpdate = ActionHistoryDeleteUpdate
    end
    object ActionHistoryDeleteAll: TAction
      Caption = 'Delete All'
      ImageIndex = 5
      ImageName = 'close-all'
      ShortCut = 8238
      OnExecute = ActionHistoryDeleteAllExecute
      OnUpdate = ActionHistoryDeleteAllUpdate
    end
    object ActionCopyCmdLineToClipboard: TAction
      Caption = 'Copy Command Line'
      ImageIndex = 0
      ImageName = 'clippy'
      OnExecute = ActionCopyCmdLineToClipboardExecute
    end
    object ActionOpenSearchForm: TAction
      Caption = 'Open Search Form...'
      ImageIndex = 3
      ImageName = 'search'
      OnExecute = ActionOpenSearchFormExecute
    end
    object ActionSave: TAction
      Caption = 'Save History List...'
      OnExecute = ActionSaveExecute
    end
    object ActionLoad: TAction
      Caption = 'Load History List...'
      OnExecute = ActionLoadExecute
    end
  end
  object PopupMenuHistory: TPopupMenu
    Images = SVGIconImageList1
    Left = 255
    Top = 113
    object pmOpenSearchForm: TMenuItem
      Action = ActionOpenSearchForm
      Default = True
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object pmCopyCommandLine: TMenuItem
      Action = ActionCopyCmdLineToClipboard
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object pmSave: TMenuItem
      Action = ActionSave
    end
    object pmLoad: TMenuItem
      Action = ActionLoad
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object pmHistoryDelete: TMenuItem
      Action = ActionHistoryDelete
    end
    object pmHistoryDeleteAll: TMenuItem
      Action = ActionHistoryDeleteAll
    end
  end
  object SVGIconImageList1: TSVGIconVirtualImageList
    ImageCollection = SVGIconDataModule.SVGIconImageCollection1
    Images = <
      item
        CollectionName = 'clippy'
        Name = 'clippy'
      end
      item
        CollectionName = 'rocket'
        Name = 'rocket'
      end
      item
        CollectionName = 'trash'
        Name = 'trash'
      end
      item
        CollectionName = 'search'
        Name = 'search'
      end
      item
        CollectionName = 'close-alt'
        Name = 'close-alt'
      end
      item
        CollectionName = 'close-all'
        Name = 'close-all'
      end>
    Scaled = True
    Left = 400
    Top = 96
  end
  object SaveDialog1: TSaveDialog
    Left = 344
    Top = 40
  end
  object OpenDialog1: TOpenDialog
    Left = 485
    Top = 55
  end
end
