object RipGrepperTopFrame: TRipGrepperTopFrame
  Left = 0
  Top = 0
  Width = 817
  Height = 26
  Align = alTop
  ParentBackground = False
  TabOrder = 0
  StyleElements = [seFont, seBorder]
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 817
    Height = 26
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    object tbarConfig: TToolBar
      AlignWithMargins = True
      Left = 781
      Top = 3
      Width = 33
      Height = 20
      Align = alRight
      AutoSize = True
      ButtonHeight = 23
      ButtonWidth = 25
      Caption = 'tbarConfig'
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = SvgImgLstTopFrame
      TabOrder = 2
      object ToolButton5: TToolButton
        Left = 0
        Top = 0
        Width = 8
        Caption = 'ToolButton5'
        ImageName = 'Search\view-refresh-symbolic'
        Style = tbsSeparator
      end
      object tbConfigure: TToolButton
        Left = 8
        Top = 0
        Hint = 'Open Settings...'
        Action = ActionConfig
        Caption = 'Settings'
        ParentShowHint = False
        ShowHint = True
      end
    end
    object tbarResult: TToolBar
      AlignWithMargins = True
      Left = 190
      Top = 0
      Width = 457
      Height = 23
      Align = alNone
      AutoSize = True
      ButtonHeight = 23
      ButtonWidth = 25
      Caption = 'tbarResult'
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = SvgImgLstTopFrame
      TabOrder = 0
      object ToolButton7: TToolButton
        Left = 0
        Top = 0
        Width = 8
        Caption = 'ToolButton7'
        ImageName = 'Search\view-refresh-symbolic'
        Style = tbsSeparator
      end
      object tbExpandCollapse: TToolButton
        Left = 8
        Top = 0
        Action = ActionExpandCollapse
      end
      object ToolButton2: TToolButton
        Left = 33
        Top = 0
        Width = 8
        Caption = 'ToolButton2'
        ImageName = 'multimedia-equalizer-symbolic'
        Style = tbsSeparator
      end
      object tbShowRelativePath: TToolButton
        Left = 41
        Top = 0
        Action = ActionShowRelativePath
      end
      object tbShowDateColumns: TToolButton
        Left = 66
        Top = 0
        Action = ActionShowDateColumns
        Style = tbsDropDown
        DropdownMenu = PopupMenuDateColumns
      end
      object ToolButton10: TToolButton
        Left = 91
        Top = 0
        Width = 8
        Caption = 'ToolButton10'
        Style = tbsSeparator
      end
      object tbAlternateRowColors: TToolButton
        Left = 99
        Top = 0
        Action = ActionAlternateRowColors
      end
      object tbShowFileIcon: TToolButton
        Left = 124
        Top = 0
        Action = ActionShowFileIcons
      end
      object tbIndentLines: TToolButton
        Left = 149
        Top = 0
        Action = ActionIndentLine
      end
      object ToolButton4: TToolButton
        Left = 174
        Top = 0
        Width = 8
        Caption = 'ToolButton4'
        ImageName = 'Search\view-refresh-symbolic'
        Style = tbsSeparator
      end
      object edtFilter: THistoryButtonedEdit
        Left = 149
        Top = 0
        Width = 121
        Height = 23
        Hint = 'Filter Results'
        TabStop = False
        Images = SvgImgLstTopFrame
        ParentShowHint = False
        PopupMenu = PopupMenuFilterMode
        RightButton.ImageIndex = 13
        RightButton.ImageName = 'filter-outline'
        RightButton.Visible = True
        ShowHint = True
        TabOrder = 0
        TextHint = 'Filter...'
        OnChange = edtFilterChange
        OnKeyDown = edtFilterKeyDown
        OnRightButtonClick = edtFilterRightButtonClick
      end
      object ToolButton9: TToolButton
        Left = 270
        Top = 0
        Width = 8
        Caption = 'ToolButton9'
        ImageName = 'multimedia-equalizer-symbolic'
        Style = tbsSeparator
      end
      object edtReplace: THistoryButtonedEdit
        Left = 311
        Top = 0
        Width = 121
        Height = 23
        Hint = 'Replace Matches (Right-Click to Change)'
        TabStop = False
        Images = SvgImgLstTopFrame
        ParentShowHint = False
        PopupMenu = PopupMenuReplace
        RightButton.ImageIndex = 14
        RightButton.ImageName = 'file-replace-outline'
        RightButton.Visible = True
        ShowHint = True
        TabOrder = 1
        TextHint = 'Replace...'
        OnChange = edtReplaceChange
        OnKeyDown = edtReplaceKeyDown
        OnRightButtonClick = edtReplaceRightButtonClick
      end
      object tbCheckAllResults: TToolButton
        Left = 242
        Top = 0
        Action = ActionCheckAllResults
      end
      object tbSaveReplacement: TToolButton
        Left = 251
        Top = 0
        Action = ActionSaveReplacement
      end
      object ToolButton6: TToolButton
        Left = 267
        Top = 0
        Width = 8
        Caption = 'ToolButton6'
        ImageName = 'multimedia-equalizer-symbolic'
        Style = tbsSeparator
      end
      object tbOpenWith: TToolButton
        Left = 275
        Top = 0
        Action = ActionOpenWith
        ImageIndex = 17
        ImageName = 'rocket'
      end
    end
    object tbarSearch: TToolBar
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 124
      Height = 23
      Align = alNone
      AutoSize = True
      ButtonHeight = 23
      ButtonWidth = 25
      Caption = 'tbarSearch'
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = SvgImgLstTopFrame
      TabOrder = 1
      object ToolButton8: TToolButton
        Left = 0
        Top = 0
        Width = 8
        Caption = 'ToolButton8'
        ImageIndex = 25
        Style = tbsSeparator
      end
      object tbShowSearchForm: TToolButton
        Left = 8
        Top = 0
        Action = ActionShowSearchForm
      end
      object tbRefreshSearch: TToolButton
        Left = 33
        Top = 0
        Action = ActionRefreshSearch
      end
      object ToolButton3: TToolButton
        Left = 58
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageName = 'edit-find-replace-symbolic'
        Style = tbsSeparator
      end
      object tbAbortSearch: TToolButton
        Left = 66
        Top = 0
        Action = ActionAbortSearch
      end
      object ToolButton1: TToolButton
        Left = 91
        Top = 0
        Width = 8
        Caption = 'ToolButton1'
        ImageName = 'format-justify-right-symbolic'
        Style = tbsSeparator
      end
      object tbCopyCmdLine: TToolButton
        AlignWithMargins = True
        Left = 99
        Top = 0
        Action = ActionCmdLineCopy
      end
    end
  end
  object ActionList: TActionList
    Images = SvgImgLstTopFrame
    Left = 1418
    Top = 65531
    object ActionSetFileFilterMode: TAction
      Category = 'Filter'
      Caption = 'Filter File'
      Checked = True
      OnExecute = ActionSetFileFilterModeExecute
    end
    object ActionSetTextFilterMode: TAction
      Category = 'Filter'
      Caption = 'Filter Text'
      OnExecute = ActionSetTextFilterModeExecute
    end
    object ActionSetDateFilterMode: TAction
      Category = 'Filter'
      Caption = 'Filter Date'
      OnExecute = ActionSetDateFilterModeExecute
    end
    object ActionSetFilterModeCaseSensitive: TAction
      Category = 'Filter'
      Caption = 'Match &Case'
      OnExecute = ActionSetFilterModeCaseSensitiveExecute
    end
    object ActionSetFilterModeRegex: TAction
      Category = 'Filter'
      Caption = 'Use Regex'
      OnExecute = ActionSetFilterModeRegexExecute
    end
    object ActionSearch: TAction
      Category = 'Search'
      Caption = 'Search'
      ImageName = 'Search\edit-find'
      OnExecute = ActionSearchExecute
    end
    object ActionShowSearchForm: TAction
      Category = 'Search'
      Caption = 'Search...'
      Hint = 'Search...'
      ImageIndex = 0
      ImageName = 'magnify'
      ShortCut = 16467
      OnExecute = ActionShowSearchFormExecute
    end
    object ActionSaveAllReplacement: TAction
      Enabled = False
      Hint = 'Save All Replacement'
      ImageIndex = 32
      ShortCut = 12371
      Visible = False
    end
    object ActionRefreshSearch: TAction
      Category = 'Search'
      Caption = 'Refresh'
      Hint = 'Refresh Results'
      ImageIndex = 1
      ImageName = 'sync-alt'
      ShortCut = 116
      OnExecute = ActionRefreshSearchExecute
      OnUpdate = ActionRefreshSearchUpdate
    end
    object ActionAbortSearch: TAction
      Category = 'Search'
      Caption = 'Abort'
      Hint = 'Abort Running Search'
      ImageIndex = 2
      ImageName = 'stop-circle-outline'
      ShortCut = 16411
      OnExecute = ActionAbortSearchExecute
    end
    object ActionCmdLineCopy: TAction
      Category = 'Search'
      Caption = 'ActionCmdLineCopy'
      Hint = 'Copy Command Line to Clipboard'
      ImageIndex = 3
      ImageName = 'clipboard-outline'
      ShortCut = 16451
      OnExecute = ActionCmdLineCopyExecute
    end
    object ActionExpandCollapse: TAction
      Category = 'View'
      Caption = 'Expand'
      GroupIndex = 1
      Hint = '[B]Expand [/B]Results'
      ImageIndex = 4
      ImageName = 'arrow-expand-vertical'
      OnExecute = ActionExpandCollapseExecute
    end
    object ActionConfig: TAction
      Category = 'Config'
      Caption = 'Config'
      GroupIndex = 3
      Hint = 'Open Config...'
      ImageIndex = 18
      ImageName = 'settings'
      OnExecute = ActionConfigExecute
    end
    object ActionShowRelativePath: TAction
      Category = 'View'
      Caption = 'ActionShowRelativePath'
      GroupIndex = 1
      Hint = 'Show Full or Relative Path'
      ImageIndex = 6
      ImageName = 'full-path'
      OnExecute = ActionShowRelativePathExecute
    end
    object ActionShowFileIcons: TAction
      Category = 'View'
      Caption = 'Show File Icons'
      GroupIndex = 1
      Hint = 'Show File Icons'
      ImageIndex = 9
      ImageName = 'file-image-outline'
      OnExecute = ActionShowFileIconsExecute
    end
    object ActionAlternateRowColors: TAction
      Category = 'View'
      Caption = 'Alternate'
      GroupIndex = 1
      Hint = 'Alternate Row Colors'
      ImageIndex = 8
      ImageName = 'texture'
      OnExecute = ActionAlternateRowColorsExecute
    end
    object ActionIndentLine: TAction
      Category = 'View'
      Caption = 'Indent'
      GroupIndex = 1
      Hint = 'Indent Matched Lines'
      ImageIndex = 10
      ImageName = 'format-align-left'
      OnExecute = ActionIndentLineExecute
    end
    object ActionShowDateColumns: TAction
      Category = 'View'
      Caption = 'Show Date Columns'
      GroupIndex = 1
      Hint = 'Show/Hide Date Columns'
      ImageIndex = 19
      ImageName = 'calendar-clock'
      OnExecute = ActionShowDateColumnsExecute
    end
    object ActionOpenWith: TAction
      Caption = 'Open with...'
      Hint = 'Open With...'
      ImageName = 'rocket-launch-outline'
      OnExecute = ActionOpenWithExecute
      OnUpdate = ActionOpenWithUpdate
    end
    object ActionAlignToolbars: TAction
      Caption = 'Align Toolbars'
      OnExecute = ActionAlignToolbarsExecute
    end
    object ActionSearchInResult: TAction
      Category = 'Search'
      Caption = 'Find...'
      Enabled = False
      Hint = 'Find in Result Tree (Find Next F3)'
      ImageName = 'Result\edit-find-replace'
      ShortCut = 114
      Visible = False
      OnExecute = ActionSearchInResultExecute
    end
    object ActionSaveReplacement: TAction
      Hint = 'Save Selected Replacement'
      ImageIndex = 16
      ImageName = 'content-save-all-outline'
      ShortCut = 16467
      OnExecute = ActionSaveReplacementExecute
      OnUpdate = ActionSaveReplacementUpdate
    end
    object ActionCheckAllResults: TAction
      Caption = 'Check All'
      Hint = 'Check/Uncheck All Result Rows for Replace'
      ImageIndex = 20
      ImageName = 'checkbox-multiple-marked-outline'
      ShortCut = 49217
      OnExecute = ActionCheckAllResultsExecute
      OnUpdate = ActionCheckAllResultsUpdate
    end
    object ActionReplaceCaseSensitive: TAction
      Caption = 'Match &Case'
      OnExecute = ActionReplaceCaseSensitiveExecute
    end
    object ActionReplaceUseRegex: TAction
      Caption = 'Use &Regex'
      OnExecute = ActionReplaceUseRegexExecute
    end
  end
  object PopupMenuToolbar: TPopupMenu
    Left = 721
    Top = 65534
    object AlignToolbar1: TMenuItem
      Action = ActionAlignToolbars
    end
  end
  object PopupMenuDateColumns: TPopupMenu
    Left = 696
    Top = 65534
    object miShowModifiedDate: TMenuItem
      Caption = 'Modified'
      Checked = True
      OnClick = miShowModifiedDateClick
    end
    object miShowCreationDate: TMenuItem
      Caption = 'Created'
      OnClick = miShowCreationDateClick
    end
    object miShowLastAccessDate: TMenuItem
      Caption = 'Accessed'
      OnClick = miShowLastAccessDateClick
    end
  end
  object PopupMenuFilterMode: TPopupMenu
    Left = 746
    Top = 65534
    object miSetFileFilterMode: TMenuItem
      Action = ActionSetFileFilterMode
      RadioItem = True
    end
    object miSetTextFilterMode: TMenuItem
      Action = ActionSetTextFilterMode
      RadioItem = True
    end
    object miSetDateFilterMode: TMenuItem
      Action = ActionSetDateFilterMode
      RadioItem = True
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miFilterModeCaseSensitive: TMenuItem
      Action = ActionSetFilterModeCaseSensitive
    end
    object miFilterModeUseRegex: TMenuItem
      Action = ActionSetFilterModeRegex
    end
  end
  object PopupMenuReplace: TPopupMenu
    Left = 696
    Top = 65534
    object mniCaseSensitive: TMenuItem
      Action = ActionReplaceCaseSensitive
    end
    object mniUseRegex: TMenuItem
      Action = ActionReplaceUseRegex
    end
  end
  object SvgImgLstTopFrame: TSVGIconVirtualImageList
    ImageCollection = SVGIconDataModule.SVGIconImageCollection1
    Images = <
      item
        CollectionName = 'magnify'
        Name = 'magnify'
      end
      item
        CollectionName = 'sync-alt'
        Name = 'sync-alt'
      end
      item
        CollectionName = 'stop-circle-outline'
        Name = 'stop-circle-outline'
      end
      item
        CollectionName = 'clipboard-outline'
        Name = 'clipboard-outline'
      end
      item
        CollectionName = 'arrow-expand-vertical'
        Name = 'arrow-expand-vertical'
      end
      item
        CollectionName = 'arrow-collapse-vertical'
        Name = 'arrow-collapse-vertical'
      end
      item
        CollectionName = 'full-path'
        Name = 'full-path'
      end
      item
        CollectionName = 'relative-path'
        Name = 'relative-path'
      end
      item
        CollectionName = 'texture'
        Name = 'texture'
      end
      item
        CollectionName = 'file-image-outline'
        Name = 'file-image-outline'
      end
      item
        CollectionName = 'format-align-left'
        Name = 'format-align-left'
      end
      item
        CollectionName = 'format-align-right'
        Name = 'format-align-right'
      end
      item
        CollectionName = 'filter'
        Name = 'filter'
      end
      item
        CollectionName = 'filter-outline'
        Name = 'filter-outline'
      end
      item
        CollectionName = 'file-replace-outline'
        Name = 'file-replace-outline'
      end
      item
        CollectionName = 'file-replace'
        Name = 'file-replace'
      end
      item
        CollectionName = 'content-save-all-outline'
        Name = 'content-save-all-outline'
      end
      item
        CollectionName = 'rocket'
        Name = 'rocket'
      end
      item
        CollectionName = 'settings'
        Name = 'settings'
      end
      item
        CollectionName = 'calendar-clock'
        Name = 'calendar-clock'
      end
      item
        CollectionName = 'checkbox-multiple-marked-outline'
        Name = 'checkbox-multiple-marked-outline'
      end
      item
        CollectionName = 'checkbox-multiple-blank-outline'
        Name = 'checkbox-multiple-blank-outline'
      end>
    FixedColor = clGrayText
    AntiAliasColor = clBtnShadow
    Scaled = True
    Left = 651
  end
end
