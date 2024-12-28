object RipGrepperTopFrame: TRipGrepperTopFrame
  Left = 0
  Top = 0
  Width = 817
  Height = 26
  Align = alTop
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
      Hint = 'Replace Resulted Items'
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
      ImageIndex = 0
      ImageName = 'Search\edit-find'
      OnExecute = ActionSearchExecute
    end
    object ActionShowSearchForm: TAction
      Category = 'Search'
      Caption = 'Search...'
      Hint = 'Search...'
      ImageIndex = 0
      ImageName = 'Search\edit-find'
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
      ImageName = 'Search\media-playback-stop-symbolic'
      ShortCut = 16411
      OnExecute = ActionAbortSearchExecute
      OnUpdate = ActionAbortSearchUpdate
    end
    object ActionCmdLineCopy: TAction
      Category = 'Search'
      Caption = 'ActionCmdLineCopy'
      Hint = 'Copy Command Line to Clipboard'
      ImageIndex = 3
      ImageName = 'Search\edit-paste-symbolic'
      ShortCut = 16451
      OnExecute = ActionCmdLineCopyExecute
    end
    object ActionExpandCollapse: TAction
      Category = 'View'
      Caption = 'Expand'
      GroupIndex = 1
      Hint = '[B]Expand [/B]Results'
      ImageIndex = 4
      ImageName = 'Result\expand-all'
      OnExecute = ActionExpandCollapseExecute
    end
    object ActionConfig: TAction
      Category = 'Config'
      Caption = 'Config'
      GroupIndex = 3
      Hint = 'Open Config...'
      ImageIndex = 18
      ImageName = 'system-run-symbolic'
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
      ImageName = 'format-list-bulleted-type'
      OnExecute = ActionShowFileIconsExecute
    end
    object ActionAlternateRowColors: TAction
      Category = 'View'
      Caption = 'Alternate'
      GroupIndex = 1
      Hint = 'Alternate Row Colors'
      ImageIndex = 8
      ImageName = 'Result\format-justify-fill-symbolic'
      OnExecute = ActionAlternateRowColorsExecute
    end
    object ActionIndentLine: TAction
      Category = 'View'
      Caption = 'Indent'
      GroupIndex = 1
      Hint = 'Indent Matched Lines'
      ImageIndex = 10
      ImageName = 'Result\format-justify-left-symbolic'
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
      ImageName = 'document-save-all'
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
    Left = 731
    Top = 65534
    object AlignToolbar1: TMenuItem
      Action = ActionAlignToolbars
    end
  end
  object PopupMenuFilterMode: TPopupMenu
    Left = 731
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
    Left = 681
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
        IconName = 'Search\edit-find'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16">'#13#10'  ' +
          '<defs id="defs3051">'#13#10'    <style type="text/css" id="current-col' +
          'or-scheme">'#13#10'      .ColorScheme-Text {'#13#10'        color:#363636;'#13#10 +
          '      }'#13#10'      </style>'#13#10'  </defs>'#13#10' <path style="fill:currentCo' +
          'lor;fill-opacity:1;stroke:none" '#13#10'     d="M 6.5 2 C 4.007 2 2 4.' +
          '01 2 6.5 C 2 8.993 4.01 11 6.5 11 C 7.5636432 11 8.5263409 10.61' +
          '8801 9.2949219 10.005859 L 13.292969 14.003906 L 14 13.296875 L ' +
          '10.001953 9.2988281 C 10.617604 8.529048 11 7.565338 11 6.5 C 11' +
          ' 4.007 8.99 2 6.5 2 z M 6.5 3 C 8.439 3 10 4.561 10 6.5 C 10 8.4' +
          '39 8.439 10 6.5 10 C 4.561 10 3 8.439 3 6.5 C 3 4.561 4.561 3 6.' +
          '5 3 z "'#13#10'     class="ColorScheme-Text"'#13#10'     />'#13#10'</svg>'#13#10
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
        IconName = 'Search\media-playback-stop-symbolic'
        SVGText = 
          '<svg width="16" height="16" enable-background="new" version="1.1' +
          '" xmlns="http://www.w3.org/2000/svg">'#13#10' <path d="m8 0a8 8 0 0 0-' +
          '8 8 8 8 0 0 0 8 8 8 8 0 0 0 8-8 8 8 0 0 0-8-8zm-0.095703 1a6.999' +
          '9 6.9999 0 0 1 0.0019531 0 6.9999 6.9999 0 0 1 0.09375 0 6.9999 ' +
          '6.9999 0 0 1 7 7 6.9999 6.9999 0 0 1-7 7 6.9999 6.9999 0 0 1-7-7' +
          ' 6.9999 6.9999 0 0 1 6.9043-7zm-1.9043 4c-0.55399 0-1 0.44601-1 ' +
          '1v4c0 0.55399 0.44601 1 1 1h4c0.55399 0 1-0.44601 1-1v-4c0-0.553' +
          '99-0.44601-1-1-1h-4z" fill="#363636" stroke-width=".99998"/>'#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'Search\edit-paste-symbolic'
        SVGText = 
          '<svg width="16" height="16" enable-background="new" version="1.1' +
          '" xmlns="http://www.w3.org/2000/svg">'#13#10' <path d="m8 0c-1.1046 0-' +
          '2 0.89543-2 2h-1-0.93945-0.060547v0.0058594c-1.113 0.032581-2 0.' +
          '93345-2 2.0547v8.8789c0 1.142 0.91853 2.0605 2.0605 2.0605h7.878' +
          '9c1.142 0 2.0605-0.91853 2.0605-2.0605v-8.8789c0-1.142-0.91853-2' +
          '.0605-2.0605-2.0605h-0.93945-1c0-0.138-0.00525-0.27625-0.03125-0' +
          '.40625-0.19615-0.93036-1.0179-1.5956-1.9688-1.5938zm-0.011719 1c' +
          '0.0039062-2.289e-5 0.0078126-2.289e-5 0.011719 0 0.414 0 0.75425' +
          ' 0.266 0.90625 0.625 0.056891 0.11716 0.088816 0.24486 0.09375 0' +
          '.375-6.1e-5 0.88813-1.0716 1.3352-1.7031 0.71094-0.63157-0.62426' +
          '-0.19666-1.7005 0.69141-1.7109zm-3.9883 2h1 1v1s-1 0-1 1h6c0-0.7' +
          '5-0.56275-0.953-0.84375-1h-0.15625v-1h1 1c0.554 0 1 0.446 1 1v9c' +
          '0 0.554-0.446 1-1 1h-8c-0.554 0-1-0.446-1-1v-9c0-0.554 0.446-1 1' +
          '-1z" fill="#363636"/>'#13#10'</svg>'#13#10
        GrayScale = True
      end
      item
        IconName = 'Result\expand-all'
        SVGText = 
          '<svg viewBox="0 0 16 16" xmlns="http://www.w3.org/2000/svg">'#13#10'  ' +
          '  <style'#13#10'        type="text/css"'#13#10'        id="current-color-sch' +
          'eme">'#13#10'        .ColorScheme-Text {'#13#10'            color:#363636;'#13#10 +
          '        }'#13#10'    </style>'#13#10'    <path d="M2 4v1h12V4zm.707 3L2 7.70' +
          '7l6 6 6-6L13.293 7 8 12.293 2.707 7z" class="ColorScheme-Text" f' +
          'ill="currentColor"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Result\collapse-all'
        SVGText = 
          '<svg viewBox="0 0 16 16" xmlns="http://www.w3.org/2000/svg">'#13#10'  ' +
          '  <style'#13#10'        type="text/css"'#13#10'        id="current-color-sch' +
          'eme">'#13#10'        .ColorScheme-Text {'#13#10'            color:#363636;'#13#10 +
          '        }'#13#10'    </style>'#13#10'    <path class="ColorScheme-Text" d="M' +
          '2 4v1h12V4zm6 2.293l-6 6 .707.707L8 7.707 13.293 13l.707-.707z" ' +
          'fill="currentColor"/>'#13#10'</svg>'#13#10
      end
      item
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" xmlns="http://ww' +
          'w.w3.org/2000/svg">'#13#10'  <g fill="none" stroke="#000" stroke-width' +
          '="1">'#13#10'    <!-- Relative path ...\... -->'#13#10'    <text x="1" y="10' +
          '" font-family="Mono" font-size="7">C : \</text>'#13#10'   </g>'#13#10'</svg>'
        GrayScale = True
      end
      item
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" xmlns="http://ww' +
          'w.w3.org/2000/svg">'#13#10'  <g fill="none" stroke="#000" stroke-width' +
          '="1">'#13#10'    <!-- Relative path ...\... -->'#13#10'    <text x="1" y="8"' +
          ' font-family="Arial" font-size="7">. . . \</text>'#13#10'   </g>'#13#10'</sv' +
          'g>'
        GrayScale = True
      end
      item
        IconName = 'Result\format-justify-fill-symbolic'
        SVGText = 
          '<svg height="16" width="16" xmlns="http://www.w3.org/2000/svg">'#13 +
          #10'  <g color="#363636" transform="translate(-633 -77)">'#13#10'    <rec' +
          't fill="#FFFFFF" height="2" overflow="visible" style="marker:non' +
          'e" width="14" x="634" y="78"/>'#13#10'    <rect fill="#A0A0A0" height=' +
          '"2" overflow="visible" style="marker:none" width="14" x="634" y=' +
          '"80"/>'#13#10'    <rect fill="#FFFFFF" height="2" overflow="visible" s' +
          'tyle="marker:none" width="14" x="634" y="82"/>'#13#10'    <rect fill="' +
          '#A0A0A0" height="2" overflow="visible" style="marker:none" width' +
          '="14" x="634" y="84"/>'#13#10'    <rect fill="#FFFFFF" height="2" over' +
          'flow="visible" style="marker:none" width="14" x="634" y="86"/>'#13#10 +
          '    <rect fill="#A0A0A0" height="2" overflow="visible" style="ma' +
          'rker:none" width="14" x="634" y="88"/>'#13#10'    <rect fill="#FFFFFF"' +
          ' height="2" overflow="visible" style="marker:none" width="14" x=' +
          '"634" y="90"/>'#13#10'  </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'format-list-bulleted-type'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">'#13#10'  ' +
          '<path d="M5,9.5L7.5,14H2.5L5,9.5M3,4H7V8H3V4M5,20A2,2 0 0,0 7,18' +
          'A2,2 0 0,0 5,16A2,2 0 0,0 3,18A2,2 0 0,0 5,20M9,5V7H21V5H9M9,19H' +
          '21V17H9V19M9,13H21V11H9V13Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Result\format-justify-left-symbolic'
        SVGText = 
          '<svg height='#39'16'#39' width='#39'16'#39' xmlns='#39'http://www.w3.org/2000/svg'#39'>'#13 +
          #10'    <g color='#39'#363636'#39' transform='#39'translate(-613 -77)'#39'>'#13#10'      ' +
          '  <rect fill='#39'#363636'#39' height='#39'1'#39' overflow='#39'visible'#39' style='#39'mark' +
          'er:none'#39' width='#39'14'#39' x='#39'614'#39' y='#39'78'#39'/>'#13#10'        <rect fill='#39'#36363' +
          '6'#39' height='#39'1'#39' overflow='#39'visible'#39' style='#39'marker:none'#39' width='#39'10'#39' ' +
          'x='#39'614'#39' y='#39'81'#39'/>'#13#10'        <rect fill='#39'#363636'#39' height='#39'1'#39' overfl' +
          'ow='#39'visible'#39' style='#39'marker:none'#39' width='#39'10'#39' x='#39'614'#39' y='#39'87'#39'/>'#13#10'  ' +
          '      <rect fill='#39'#363636'#39' height='#39'1'#39' overflow='#39'visible'#39' style='#39 +
          'marker:none'#39' width='#39'14'#39' x='#39'614'#39' y='#39'90'#39'/>'#13#10'        <rect fill='#39'#3' +
          '63636'#39' height='#39'1'#39' overflow='#39'visible'#39' style='#39'marker:none'#39' width='#39 +
          '14'#39' x='#39'614'#39' y='#39'84'#39'/>'#13#10'        '#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Result\format-justify-right-symbolic'
        SVGText = 
          '<svg height='#39'16'#39' width='#39'16'#39' xmlns='#39'http://www.w3.org/2000/svg'#39'>'#13 +
          #10'    <g color='#39'#363636'#39' transform='#39'translate(-593 -77)'#39'>'#13#10'      ' +
          '  <rect fill='#39'#363636'#39' height='#39'1'#39' overflow='#39'visible'#39' style='#39'mark' +
          'er:none'#39' width='#39'14'#39' x='#39'594'#39' y='#39'78'#39'/>'#13#10'        <rect fill='#39'#36363' +
          '6'#39' height='#39'1'#39' overflow='#39'visible'#39' style='#39'marker:none'#39' width='#39'10'#39' ' +
          'x='#39'598'#39' y='#39'81'#39'/>'#13#10'        <rect fill='#39'#363636'#39' height='#39'1'#39' overfl' +
          'ow='#39'visible'#39' style='#39'marker:none'#39' width='#39'10'#39' x='#39'598'#39' y='#39'87'#39'/>'#13#10'  ' +
          '      <rect fill='#39'#363636'#39' height='#39'1'#39' overflow='#39'visible'#39' style='#39 +
          'marker:none'#39' width='#39'14'#39' x='#39'594'#39' y='#39'90'#39'/>'#13#10'        <rect fill='#39'#3' +
          '63636'#39' height='#39'1'#39' overflow='#39'visible'#39' style='#39'marker:none'#39' width='#39 +
          '14'#39' x='#39'594'#39' y='#39'84'#39'/>'#13#10'        '#13#10'    </g>'#13#10'</svg>'#13#10
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
        IconName = 'document-save-all'
        SVGText = 
          '<svg version="1.1" viewBox="0 0 16 16" xmlns="http://www.w3.org/' +
          '2000/svg">'#13#10' <defs>'#13#10'  <style id="current-color-scheme" type="te' +
          'xt/css">.ColorScheme-Text {'#13#10'        color:#363636;'#13#10'      }</st' +
          'yle>'#13#10' </defs>'#13#10' <path d="m4 1c-1.108 0-2 0.892-2 2h1c0-0.554 0.' +
          '446-1 1-1h6.5859l3.4141 3.4141v6.5859c0 0.554-0.446 1-1 1 0 0.34' +
          '745-0.05617 0.679-0.13867 1h0.13867c1.108 0 2-0.892 2-2v-7l-4-4h' +
          '-6z" style="fill:currentColor" class="ColorScheme-Text"/>'#13#10' <pat' +
          'h d="m2 3c-1.108 0-2 0.892-2 2v9c0 1.108 0.892 2 2 2h9c1.108 0 2' +
          '-0.892 2-2v-7l-4-4h-7zm0 1h1v3c0 1.108 0.892 2 2 2h2c1.108 0 2-0' +
          '.892 2-2v-2.5859l3 3v6.5859c0 0.554-0.446 1-1 1v-2c0-1.108-0.892' +
          '-2-2-2h-5c-1.108 0-2 0.892-2 2v2c-0.554 0-1-0.446-1-1v-9c0-0.554' +
          ' 0.446-1 1-1zm2 0h2v4h-1c-0.554 0-1-0.446-1-1zm0 8h5c0.554 0 1 0' +
          '.446 1 1v2h-7v-2c0-0.554 0.446-1 1-1z" style="fill:currentColor"' +
          ' class="ColorScheme-Text"/>'#13#10'</svg>'#13#10
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
        IconName = 'system-run-symbolic'
        SVGText = 
          '<svg width="16" height="16" enable-background="new" version="1.1' +
          '" xmlns="http://www.w3.org/2000/svg">'#13#10' <g fill="#363636">'#13#10'  <p' +
          'ath d="M7.903 1.478s-.148.05-.499.283c-.429.287-.67.235-.954.156' +
          'l-.002.002a2.547 2.547 0 0 0-.16-.087V1.83c-.22-.195-.396-.37-.3' +
          '9-.886.006-.422-.033-.572-.033-.572a1.107 1.107 0 0 0-.364-.107s' +
          '-.214-.023-.378.029c0 0-.07.139-.152.553-.101.506-.309.64-.565.7' +
          '85v.003a2.668 2.668 0 0 0-.175.052l-.002-.002c-.294.018-.541.019' +
          '-.902-.35C3.033 1.031 2.9.953 2.9.953a1.101 1.101 0 0 0-.333.18s' +
          '-.167.136-.247.289c0 0 .05.147.283.498.287.43.235.671.156.955l.0' +
          '02.002c-.03.052-.06.105-.087.16H2.67c-.195.22-.37.395-.886.389-.' +
          '421-.005-.572.034-.572.034-.085.15-.107.364-.107.364s-.023.214.0' +
          '29.378c0 0 .139.07.553.152.506.101.64.309.785.565h.002c.015.059.' +
          '033.117.052.175l-.002.002c.018.294.019.54-.35.901-.303.295-.382.' +
          '429-.382.429.047.166.182.333.182.333s.135.167.288.247c0 0 .148-.' +
          '05.498-.284.43-.286.671-.234.955-.155l.002-.002c.053.03.106.06.1' +
          '6.087v.003c.22.195.395.37.389.885-.005.422.033.573.033.573.15.08' +
          '5.364.107.364.107s.214.023.379-.03c0 0 .07-.138.152-.552.1-.506.' +
          '309-.64.565-.785v-.003c.058-.015.116-.033.174-.052l.002.002c.294' +
          '-.018.541-.019.901.35.295.303.429.381.429.381.166-.046.333-.181.' +
          '333-.181s.167-.136.247-.289c0 0-.049-.147-.283-.498-.287-.429-.2' +
          '34-.67-.156-.954l-.002-.002c.031-.053.06-.106.087-.16h.003c.195-' +
          '.22.37-.396.886-.39.422.006.572-.033.572-.033.085-.15.107-.364.1' +
          '07-.364s.023-.214-.029-.379c0 0-.139-.07-.553-.152-.506-.1-.64-.' +
          '308-.785-.565h-.002a2.538 2.538 0 0 0-.052-.174l.002-.002c-.018-' +
          '.294-.019-.541.35-.902.303-.294.381-.428.381-.428-.046-.166-.181' +
          '-.333-.181-.333s-.135-.168-.288-.247zM6.26 5.696a1.87 1.87 0 1 1' +
          '-2.355-2.908A1.87 1.87 0 0 1 6.26 5.696z" enable-background="new' +
          '"/>'#13#10'  <path d="M12.326 6.472s-.158.113-.492.521c-.409.5-.717.52' +
          '5-1.085.535l-.002.003a3.202 3.202 0 0 0-.222-.046v-.003c-.336-.1' +
          '52-.61-.297-.79-.916-.148-.506-.25-.672-.25-.672-.21-.046-.474.0' +
          '05-.474.005s-.264.052-.441.174c0 0-.033.191.02.716.064.642-.135.' +
          '878-.388 1.145v.004c-.065.04-.127.082-.19.126l-.002-.002c-.344.1' +
          '29-.64.22-1.205-.09-.463-.253-.652-.298-.652-.298-.181.115-.331.' +
          '338-.331.338s-.15.223-.19.435c0 0 .113.158.52.492.5.408.526.716.' +
          '536 1.084l.003.002a3.36 3.36 0 0 0-.046.223h-.002c-.153.335-.297' +
          '.609-.916.79-.506.147-.672.25-.672.25-.047.21.004.473.004.473s.0' +
          '51.264.174.442c0 0 .19.032.716-.02.641-.065.877.135 1.144.388h.0' +
          '03a3.4 3.4 0 0 0 .126.189l-.002.003c.13.345.22.64-.09 1.206-.253' +
          '.462-.298.651-.298.651.116.182.339.332.339.332s.223.15.435.19c0 ' +
          '0 .158-.113.492-.521.408-.5.716-.525 1.084-.535l.002-.003c.074.0' +
          '18.148.034.223.046v.004c.335.152.609.297.79.916.147.506.249.672.' +
          '249.672.21.046.474-.005.474-.005s.264-.052.442-.174c0 0 .032-.19' +
          '1-.02-.716-.064-.642.136-.878.389-1.145l-.001-.003c.064-.04.127-' +
          '.082.19-.126l.002.002c.345-.129.64-.22 1.206.09.462.253.651.298.' +
          '651.298.182-.116.332-.338.332-.338s.15-.223.19-.435c0 0-.112-.15' +
          '9-.52-.492-.5-.409-.526-.717-.536-1.085l-.002-.002c.018-.074.034' +
          '-.148.046-.222h.003c.153-.336.297-.61.916-.79.506-.148.672-.25.6' +
          '72-.25.046-.21-.005-.474-.005-.474s-.051-.264-.173-.442c0 0-.192' +
          '-.032-.717.02-.641.065-.878-.134-1.145-.388h-.003a3.164 3.164 0 ' +
          '0 0-.126-.188l.002-.003c-.129-.345-.22-.64.09-1.206.253-.462.298' +
          '-.652.298-.652-.116-.181-.338-.332-.338-.332s-.223-.15-.435-.19z' +
          'm-.423 5.643A2.338 2.338 0 1 1 8.026 9.5a2.338 2.338 0 0 1 3.877' +
          ' 2.615z" enable-background="new"/>'#13#10' </g>'#13#10'</svg>'#13#10
      end>
    AntiAliasColor = clBtnShadow
    GrayScale = True
    Scaled = True
    Left = 136
  end
end
