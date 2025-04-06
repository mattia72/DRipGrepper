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
      PopupMenu = PopupMenuHistory
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
      ImageIndex = 2
      ImageName = 'trash'
      ShortCut = 46
      OnExecute = ActionHistoryDeleteExecute
      OnUpdate = ActionHistoryDeleteUpdate
    end
    object ActionHistoryDeleteAll: TAction
      Caption = 'Delete All'
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
      Caption = 'Save...'
      OnExecute = ActionSaveExecute
      Visible = False
    end
    object ActionLoad: TAction
      Caption = 'Load...'
      Visible = False
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
  object SVGIconImageList1: TSVGIconImageList
    SVGIconItems = <
      item
        IconName = 'clippy'
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<path fill-rule="evenodd" clip-' +
          'rule="evenodd" d="M7.00024 13.9916H4.00024V4.99159H12.0002V6.991' +
          '59H13.0002V4.49159L12.5002 3.99159H11.0002V2.99159H10.0002C9.998' +
          '01 2.46201 9.78583 1.95494 9.41024 1.58159C9.03552 1.20908 8.528' +
          '62 1 8.00024 1C7.47187 1 6.96497 1.20908 6.59024 1.58159C6.21466' +
          ' 1.95494 6.00248 2.46201 6.00024 2.99159H4.94024V3.99159H3.50024' +
          'L3.00024 4.49159V14.4916L3.50024 14.9916H7.00024V13.9916ZM7.0002' +
          '4 2.79159C7.03722 2.59305 7.13344 2.41037 7.27623 2.26757C7.4190' +
          '3 2.12478 7.60171 2.02856 7.80024 1.99159C7.99525 1.95352 8.1971' +
          '6 1.97441 8.38024 2.05159C8.56424 2.12151 8.72163 2.24743 8.8302' +
          '4 2.41159C8.96125 2.60413 9.02095 2.83635 8.99905 3.0682C8.97714' +
          ' 3.30006 8.875 3.51699 8.71024 3.68159C8.54565 3.84634 8.32871 3' +
          '.94848 8.09686 3.97039C7.86501 3.9923 7.63278 3.93259 7.44024 3.' +
          '80159C7.27609 3.69298 7.15017 3.53558 7.08024 3.35159C7.00024 3.' +
          '17651 6.97246 2.98206 7.00024 2.79159ZM14.0803 12.2516L13.0003 1' +
          '3.3416V7.99158H12.0003V13.3316L10.9203 12.2516L10.2103 12.9616L1' +
          '2.1503 14.8916H12.8603L14.7903 12.9616L14.0803 12.2516ZM8.16024 ' +
          '8.09155H8.87024L10.8002 10.0216L10.0902 10.7316L9.01025 9.65155V' +
          '14.9916H8.01025V9.64155L6.93025 10.7316L6.22025 10.0216L8.16024 ' +
          '8.09155Z" fill="#424242"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'rocket'
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<path fill-rule="evenodd" clip-' +
          'rule="evenodd" d="M14.4912 1C10.8928 1.0045 7.83757 2.98269 5.65' +
          '659 5H1.50024L1.00024 5.5V8.5L1.14724 8.854L2.1382 9.84496L2.139' +
          '24 9.854L6.13924 13.854L6.14825 13.855L7.14725 14.854L7.50025 15' +
          'H10.5002L11.0002 14.5V10.346C13.0193 8.16839 14.9962 5.11301 14.' +
          '9922 1.5L14.4912 1ZM2.00024 6H4.64331C3.59271 7.10401 2.83194 8.' +
          '11996 2.41868 8.71094L2.00024 8.293V6ZM7.70025 14L7.28073 13.576' +
          '6C7.87285 13.1655 8.89019 12.4085 9.99525 11.3611V14H7.70025ZM6.' +
          '55724 12.856L3.13624 9.437C4.12824 8 8.37924 2.355 13.9782 2.016' +
          'C13.6522 7.628 7.99124 11.869 6.55724 12.856ZM4.00024 15V14H2.00' +
          '024V12H1.00024V15H4.00024ZM10.7478 7.33284C10.9124 7.08628 11.00' +
          '02 6.79647 11.0002 6.50001C11.0002 6.3026 10.9613 6.10714 10.885' +
          '6 5.92483C10.8099 5.74251 10.699 5.57693 10.5591 5.43758C10.4193' +
          ' 5.29822 10.2534 5.18784 10.0708 5.11275C9.88824 5.03766 9.69265' +
          ' 4.99935 9.49524 5.00001C9.19879 5.001 8.90927 5.08981 8.66326 5' +
          '.25523C8.41724 5.42065 8.22576 5.65526 8.113 5.92944C8.00023 6.2' +
          '0361 7.97124 6.50506 8.02968 6.7957C8.08812 7.08634 8.23137 7.35' +
          '314 8.44135 7.56242C8.65133 7.7717 8.91861 7.91407 9.20944 7.971' +
          '54C9.50028 8.02902 9.80162 7.99902 10.0754 7.88534C10.3492 7.771' +
          '67 10.5832 7.57941 10.7478 7.33284Z" fill="#424242"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'trash'
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<path fill-rule="evenodd" clip-' +
          'rule="evenodd" d="M10.0002 3H12.0002H13.0002V4H12.0002V13L11.000' +
          '2 14H4.00024L3.00024 13V4H2.00024V3H5.00024V2C5.00024 1.73478 5.' +
          '10555 1.48038 5.29309 1.29285C5.48063 1.10531 5.73503 1 6.00024 ' +
          '1H9.00024C9.26546 1 9.51986 1.10531 9.7074 1.29285C9.89493 1.480' +
          '38 10.0002 1.73478 10.0002 2V3ZM9.00024 2H6.00024V3H9.00024V2ZM4' +
          '.00024 13H11.0002V4H4.00024V13ZM6.00024 5H5.00024V12H6.00024V5ZM' +
          '7.00024 5H8.00024V12H7.00024V5ZM9.00024 5H10.0002V12H9.00024V5Z"' +
          ' fill="#424242"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'search'
        SVGText = 
          '<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<path d="M15.2502 1.02546e-06C1' +
          '3.6607 -0.000791296 12.1048 0.457574 10.7697 1.32007C9.43447 2.1' +
          '8256 8.37681 3.4124 7.724 4.8617C7.07118 6.31099 6.85102 7.91801' +
          ' 7.08984 9.4895C7.32867 11.061 8.01628 12.5301 9.07019 13.72L1.0' +
          '0024 22.88L2.12024 23.88L10.1703 14.76C11.2057 15.5693 12.4195 1' +
          '6.1196 13.7106 16.365C15.0017 16.6104 16.3328 16.5437 17.5929 16' +
          '.1707C18.853 15.7976 20.0058 15.1288 20.9552 14.2201C21.9046 13.' +
          '3114 22.6232 12.1891 23.0511 10.9465C23.4791 9.70396 23.6041 8.3' +
          '7703 23.4155 7.07642C23.227 5.77581 22.7304 4.53915 21.9673 3.46' +
          '924C21.2041 2.39933 20.1964 1.52711 19.0281 0.925416C17.8597 0.3' +
          '23719 16.5644 0.00991516 15.2502 0.0100108V1.02546e-06ZM15.2502 ' +
          '15C13.9152 15 12.6102 14.6041 11.5001 13.8624C10.3901 13.1207 9.' +
          '52493 12.0665 9.01404 10.8331C8.50315 9.59973 8.36943 8.24248 8.' +
          '62988 6.93311C8.89033 5.62373 9.53329 4.42106 10.4773 3.47705C11' +
          '.4213 2.53305 12.624 1.89009 13.9333 1.62964C15.2427 1.36919 16.' +
          '6 1.5029 17.8334 2.01379C19.0668 2.52469 20.121 3.38985 20.8627 ' +
          '4.49988C21.6044 5.60991 22.0002 6.91498 22.0002 8.25C22.0002 10.' +
          '0402 21.2891 11.7571 20.0232 13.023C18.7573 14.2888 17.0405 15 1' +
          '5.2502 15Z" fill="#424242"/>'#13#10'</svg>'#13#10
      end>
    Scaled = True
    Left = 400
    Top = 96
  end
  object SaveDialog1: TSaveDialog
    Left = 344
    Top = 40
  end
end
