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
    Caption = 'pnlTop'
    Color = clWindow
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
      TabOrder = 0
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
        Action = ActionConfig
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
      TabOrder = 1
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
      object tbAlternateRowColors: TToolButton
        Left = 66
        Top = 0
        Action = ActionAlternateRowColors
      end
      object tbShowFileIcon: TToolButton
        Left = 91
        Top = 0
        Action = ActionShowFileIcons
      end
      object tbIndentLines: TToolButton
        Left = 116
        Top = 0
        Action = ActionIndentLine
      end
      object ToolButton4: TToolButton
        Left = 141
        Top = 0
        Width = 8
        Caption = 'ToolButton4'
        ImageName = 'Search\view-refresh-symbolic'
        Style = tbsSeparator
      end
      object edtFilter: TButtonedEdit
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
      object edtReplace: TButtonedEdit
        Left = 278
        Top = 0
        Width = 121
        Height = 23
        Hint = 'Replace Matches (right-click to change)'
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
      object tbSaveReplacement: TToolButton
        Left = 399
        Top = 0
        Action = ActionSaveReplacement
      end
      object ToolButton6: TToolButton
        Left = 424
        Top = 0
        Width = 8
        Caption = 'ToolButton6'
        ImageName = 'multimedia-equalizer-symbolic'
        Style = tbsSeparator
      end
      object tbOpenWith: TToolButton
        Left = 432
        Top = 0
        Action = ActionOpenWith
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
      TabOrder = 2
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
      OnUpdate = ActionShowSearchFormUpdate
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
      ImageName = 'sync'
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
      OnUpdate = ActionAbortSearchUpdate
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
      OnExecute = ActionShowRelativePathExecute
    end
    object ActionShowFileIcons: TAction
      Category = 'View'
      Caption = 'Show File  Icons'
      GroupIndex = 1
      Hint = 'Show File  Icons'
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
    object ActionOpenWith: TAction
      Caption = 'Open with...'
      Hint = 'Open With...'
      ImageIndex = 17
      ImageName = 'rocket-launch-outline'
      OnExecute = ActionOpenWithExecute
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
  object SvgImgLstTopFrame: TSVGIconImageList
    SVGIconItems = <
      item
        IconName = 'magnify'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M9.5,3A6.5,6.5 0 0,1 16,9.5C16,11.11 15.41,12.59 14.44,13.7' +
          '3L14.71,14H15.5L20.5,19L19,20.5L14,15.5V14.71L13.73,14.44C12.59,' +
          '15.41 11.11,16 9.5,16A6.5,6.5 0 0,1 3,9.5A6.5,6.5 0 0,1 9.5,3M9.' +
          '5,5C7,5 5,7 5,9.5C5,12 7,14 9.5,14C12,14 14,12 14,9.5C14,7 12,5 ' +
          '9.5,5Z" /></svg>'
        GrayScale = True
      end
      item
        IconName = 'sync'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M12,18A6,6 0 0,1 6,12C6,11 6.25,10.03 6.7,9.2L5.24,7.74C4.4' +
          '6,8.97 4,10.43 4,12A8,8 0 0,0 12,20V23L16,19L12,15M12,4V1L8,5L12' +
          ',9V6A6,6 0 0,1 18,12C18,13 17.75,13.97 17.3,14.8L18.76,16.26C19.' +
          '54,15.03 20,13.57 20,12A8,8 0 0,0 12,4Z" /></svg>'
      end
      item
        IconName = 'stop-circle-outline'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A' +
          '10,10 0 0,0 12,2M12,4C16.41,4 20,7.59 20,12C20,16.41 16.41,20 12' +
          ',20C7.59,20 4,16.41 4,12C4,7.59 7.59,4 12,4M9,9V15H15V9" /></svg' +
          '>'
      end
      item
        IconName = 'clipboard-outline'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M19,3H14.82C14.4,1.84 13.3,1 12,1C10.7,1 9.6,1.84 9.18,3H5A' +
          '2,2 0 0,0 3,5V19A2,2 0 0,0 5,21H19A2,2 0 0,0 21,19V5A2,2 0 0,0 1' +
          '9,3M12,3A1,1 0 0,1 13,4A1,1 0 0,1 12,5A1,1 0 0,1 11,4A1,1 0 0,1 ' +
          '12,3M7,7H17V5H19V19H5V5H7V7Z" /></svg>'
        GrayScale = True
      end
      item
        IconName = 'arrow-expand-vertical'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M13,9V15H16L12,19L8,15H11V9H8L12,5L16,9H13M4,2H20V4H4V2M4,2' +
          '0H20V22H4V20Z" /></svg>'
      end
      item
        IconName = 'arrow-collapse-vertical'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M4,12H20V14H4V12M4,9H20V11H4V9M16,4L12,8L8,4H11V1H13V4H16M8' +
          ',19L12,15L16,19H13V22H11V19H8Z" /></svg>'
      end
      item
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" xmlns="http://ww' +
          'w.w3.org/2000/svg">'#13#10'  <g fill="none" stroke="#000" stroke-width' +
          '="1">'#13#10'    <!-- Relative path ...\... -->'#13#10'    <text x="0" y="12' +
          '" font-family="Mono" font-size="8">C : \</text>'#13#10'   </g>'#13#10'</svg>'
        GrayScale = True
      end
      item
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" xmlns="http://ww' +
          'w.w3.org/2000/svg">'#13#10'  <g fill="none" stroke="#000" stroke-width' +
          '="1">'#13#10'    <!-- Relative path ...\... -->'#13#10'    <text x="0" y="12' +
          '" font-family="Arial" font-size="8">. . . \</text>'#13#10'   </g>'#13#10'</s' +
          'vg>'
        GrayScale = True
      end
      item
        IconName = 'texture'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M9.29,21H12.12L21,12.12V9.29M19,21C19.55,21 20.05,20.78 20.' +
          '41,20.41C20.78,20.05 21,19.55 21,19V17L17,21M5,3A2,2 0 0,0 3,5V7' +
          'L7,3M11.88,3L3,11.88V14.71L14.71,3M19.5,3.08L3.08,19.5C3.17,19.8' +
          '5 3.35,20.16 3.59,20.41C3.84,20.65 4.15,20.83 4.5,20.92L20.93,4.' +
          '5C20.74,3.8 20.2,3.26 19.5,3.08Z" /></svg>'
      end
      item
        IconName = 'file-image-outline'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M14,2L20,8V20A2,2 0 0,1 18,22H6A2,2 0 0,1 4,20V4A2,2 0 0,1 ' +
          '6,2H14M18,20V9H13V4H6V20H18M17,13V19H7L12,14L14,16M10,10.5A1.5,1' +
          '.5 0 0,1 8.5,12A1.5,1.5 0 0,1 7,10.5A1.5,1.5 0 0,1 8.5,9A1.5,1.5' +
          ' 0 0,1 10,10.5Z" /></svg>'
      end
      item
        IconName = 'format-align-left'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M3,3H21V5H3V3M3,7H15V9H3V7M3,11H21V13H3V11M3,15H15V17H3V15M' +
          '3,19H21V21H3V19Z" /></svg>'
      end
      item
        IconName = 'format-align-right'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M3,3H21V5H3V3M9,7H21V9H9V7M3,11H21V13H3V11M9,15H21V17H9V15M' +
          '3,19H21V21H3V19Z" /></svg>'
      end
      item
        IconName = 'filter'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M14,12V19.88C14.04,20.18 13.94,20.5 13.71,20.71C13.32,21.1 ' +
          '12.69,21.1 12.3,20.71L10.29,18.7C10.06,18.47 9.96,18.16 10,17.87' +
          'V12H9.97L4.21,4.62C3.87,4.19 3.95,3.56 4.38,3.22C4.57,3.08 4.78,' +
          '3 5,3V3H19V3C19.22,3 19.43,3.08 19.62,3.22C20.05,3.56 20.13,4.19' +
          ' 19.79,4.62L14.03,12H14Z" /></svg>'
      end
      item
        IconName = 'filter-outline'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M15,19.88C15.04,20.18 14.94,20.5 14.71,20.71C14.32,21.1 13.' +
          '69,21.1 13.3,20.71L9.29,16.7C9.06,16.47 8.96,16.16 9,15.87V10.75' +
          'L4.21,4.62C3.87,4.19 3.95,3.56 4.38,3.22C4.57,3.08 4.78,3 5,3V3H' +
          '19V3C19.22,3 19.43,3.08 19.62,3.22C20.05,3.56 20.13,4.19 19.79,4' +
          '.62L15,10.75V19.88M7.04,5L11,10.06V15.58L13,17.58V10.05L16.96,5H' +
          '7.04Z" /></svg>'
      end
      item
        IconName = 'file-replace-outline'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M14,3L12,1H4A2,2 0 0,0 2,3V15A2,2 0 0,0 4,17H11V19L15,16L11' +
          ',13V15H4V3H14M21,10V21A2,2 0 0,1 19,23H8A2,2 0 0,1 6,21V19H8V21H' +
          '19V12H14V7H8V13H6V7A2,2 0 0,1 8,5H16L21,10Z" /></svg>'
      end
      item
        IconName = 'file-replace'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M14,12H19.5L14,6.5V12M8,5H15L21,11V21A2,2 0 0,1 19,23H8C6.8' +
          '9,23 6,22.1 6,21V18H11V20L15,17L11,14V16H6V7A2,2 0 0,1 8,5M13.5,' +
          '3H4V16H6V18H4A2,2 0 0,1 2,16V3A2,2 0 0,1 4,1H11.5L13.5,3Z" /></s' +
          'vg>'
      end
      item
        IconName = 'content-save-all-outline'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M1 7H3V21H17V23H3C1.9 23 1 22.11 1 21V7M19 1H7C5.89 1 5 1.9' +
          ' 5 3V17C5 18.1 5.89 19 7 19H21C22.1 19 23 18.1 23 17V5L19 1M21 1' +
          '7H7V3H18.17L21 5.83V17M14 10C12.34 10 11 11.34 11 13S12.34 16 14' +
          ' 16 17 14.66 17 13 15.66 10 14 10M8 4H17V8H8V4Z" /></svg>'
      end
      item
        IconName = 'rocket-launch-outline'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M13.13 22.19L11.5 18.36C13.07 17.78 14.54 17 15.9 16.09L13.' +
          '13 22.19M5.64 12.5L1.81 10.87L7.91 8.1C7 9.46 6.22 10.93 5.64 12' +
          '.5M19.22 4C19.5 4 19.75 4 19.96 4.05C20.13 5.44 19.94 8.3 16.66 ' +
          '11.58C14.96 13.29 12.93 14.6 10.65 15.47L8.5 13.37C9.42 11.06 10' +
          '.73 9.03 12.42 7.34C15.18 4.58 17.64 4 19.22 4M19.22 2C17.24 2 1' +
          '4.24 2.69 11 5.93C8.81 8.12 7.5 10.53 6.65 12.64C6.37 13.39 6.56' +
          ' 14.21 7.11 14.77L9.24 16.89C9.62 17.27 10.13 17.5 10.66 17.5C10' +
          '.89 17.5 11.13 17.44 11.36 17.35C13.5 16.53 15.88 15.19 18.07 13' +
          'C23.73 7.34 21.61 2.39 21.61 2.39S20.7 2 19.22 2M14.54 9.46C13.7' +
          '6 8.68 13.76 7.41 14.54 6.63S16.59 5.85 17.37 6.63C18.14 7.41 18' +
          '.15 8.68 17.37 9.46C16.59 10.24 15.32 10.24 14.54 9.46M8.88 16.5' +
          '3L7.47 15.12L8.88 16.53M6.24 22L9.88 18.36C9.54 18.27 9.21 18.12' +
          ' 8.91 17.91L4.83 22H6.24M2 22H3.41L8.18 17.24L6.76 15.83L2 20.59' +
          'V22M2 19.17L6.09 15.09C5.88 14.79 5.73 14.47 5.64 14.12L2 17.76V' +
          '19.17Z" /></svg>'
      end
      item
        IconName = 'settings'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M19.59,15.5L17.77,14.2C18.07,13.12 18.09,11.95 17.77,10.78L' +
          '19.59,9.5L18.14,7L16.11,7.92C15.32,7.12 14.32,6.5 13.15,6.21L12.' +
          '95,4H10.05L9.85,6.21C8.68,6.5 7.68,7.12 6.89,7.92L4.86,7L3.41,9.' +
          '5L5.23,10.78C4.91,11.95 4.93,13.12 5.23,14.2L3.41,15.5L4.86,18L6' +
          '.89,17.07C7.68,17.86 8.68,18.46 9.85,18.77L10.05,21H12.95L13.15,' +
          '18.77C14.32,18.46 15.32,17.86 16.11,17.07L18.14,18L19.59,15.5M13' +
          '.5,3C13.77,3 14,3.2 14,3.46L14.18,5.5C14.94,5.78 15.62,6.19 16.2' +
          '3,6.68L18.08,5.81C18.31,5.69 18.6,5.77 18.74,6L20.74,9.5C20.88,9' +
          '.71 20.8,10 20.58,10.15L18.91,11.32C19.04,12.12 19.03,12.91 18.9' +
          '1,13.68L20.58,14.85C20.8,15 20.88,15.29 20.74,15.5L18.74,19C18.6' +
          ',19.21 18.31,19.29 18.08,19.17L16.23,18.31C15.62,18.8 14.94,19.2' +
          ' 14.18,19.5L14,21.5C14,21.79 13.77,22 13.5,22H9.5C9.22,22 9,21.7' +
          '8 9,21.5L8.82,19.5C8.06,19.2 7.38,18.8 6.77,18.31L4.92,19.17C4.6' +
          '9,19.29 4.4,19.21 4.26,19L2.26,15.5C2.12,15.29 2.2,15 2.42,14.85' +
          'L4.09,13.68C3.97,12.91 3.96,12.12 4.09,11.32L2.42,10.15C2.2,10 2' +
          '.12,9.71 2.26,9.5L4.26,6C4.4,5.77 4.69,5.69 4.92,5.81L6.77,6.68C' +
          '7.38,6.19 8.06,5.78 8.82,5.5L9,3.46C9,3.2 9.23,3 9.5,3H13.5M11.5' +
          ',9C13.43,9 15,10.57 15,12.5C15,14.43 13.43,16 11.5,16C9.57,16 8,' +
          '14.43 8,12.5C8,10.57 9.57,9 11.5,9M11.5,10C10.12,10 9,11.12 9,12' +
          '.5C9,13.88 10.12,15 11.5,15C12.88,15 14,13.88 14,12.5C14,11.12 1' +
          '2.88,10 11.5,10Z" /></svg>'
      end>
    AntiAliasColor = clBtnShadow
    GrayScale = True
    Scaled = True
    Left = 651
  end
end
