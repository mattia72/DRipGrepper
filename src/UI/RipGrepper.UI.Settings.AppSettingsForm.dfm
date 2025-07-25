object AppSettingsForm: TAppSettingsForm
  Left = 0
  Top = 0
  Caption = 'AppSettingsForm'
  ClientHeight = 413
  ClientWidth = 514
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnShow = FormShow
  TextHeight = 15
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 514
    Height = 413
    Align = alClient
    TabOrder = 0
    DesignSize = (
      510
      409)
    object lblRgExePath: TLabel
      Left = 16
      Top = 9
      Width = 141
      Height = 15
      Caption = 'Path to ripgrep executable:'
    end
    object lblVersion: TLabel
      Left = 16
      Top = 56
      Width = 41
      Height = 15
      Caption = 'Version:'
    end
    object btnedtRgExePath: TButtonedEdit
      Left = 16
      Top = 27
      Width = 479
      Height = 23
      Hint = 'Full path to rg.exe.'
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      Images = SVGIconImageList1
      LeftButton.DisabledImageIndex = 1
      LeftButton.DisabledImageName = 'sync'
      LeftButton.ImageIndex = 1
      LeftButton.ImageName = 'sync'
      LeftButton.Visible = True
      ReadOnly = True
      RightButton.DisabledImageIndex = 0
      RightButton.DisabledImageName = 'folder-opened'
      RightButton.ImageIndex = 0
      RightButton.ImageName = 'folder-opened'
      RightButton.Visible = True
      TabOrder = 0
      Text = 'Path to rg.exe...'
      TextHint = 'Path to rg.exe...'
      OnEnter = btnedtRgExePathEnter
      OnExit = btnedtRgExePathExit
      OnLeftButtonClick = btnedtRgExePathLeftButtonClick
      OnRightButtonClick = btnedtRgExePathRightButtonClick
    end
    object Memo1: TMemo
      Left = 16
      Top = 74
      Width = 479
      Height = 63
      Hint = 'Output of "rg.exe --version".'
      Anchors = [akLeft, akTop, akRight]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlightText
      Font.Height = -12
      Font.Name = 'Consolas'
      Font.Style = []
      Lines.Strings = (
        'line 1'
        'line 2'
        'line 3'
        'line 4')
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 1
    end
    object grpSettings: TGroupBox
      Left = 11
      Top = 143
      Width = 484
      Height = 252
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Settings'
      TabOrder = 2
      DesignSize = (
        484
        252)
      object Label2: TLabel
        Left = 19
        Top = 24
        Width = 116
        Height = 15
        Caption = 'Copy command shell:'
      end
      object Label3: TLabel
        Left = 19
        Top = 52
        Width = 116
        Height = 15
        Caption = 'Combo history count:'
      end
      object cmbCopyCmdShell: TComboBox
        Left = 142
        Top = 20
        Width = 99
        Height = 23
        Hint = 
          'Select shell format to copy the RipGrep command to the clipboard' +
          '.'
        ItemIndex = 0
        TabOrder = 0
        Text = 'PowerShell '
        Items.Strings = (
          'PowerShell '
          'DOS')
      end
      object grpSaveLoad: TGroupBox
        Left = 16
        Top = 79
        Width = 456
        Height = 162
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Save/Load behaviour'
        TabOrder = 2
        DesignSize = (
          456
          162)
        object lblSearches: TLabel
          Left = 184
          Top = 80
          Width = 45
          Height = 15
          Caption = 'searches'
        end
        object rgModeLoadSeraches: TRadioGroup
          AlignWithMargins = True
          Left = 10
          Top = 17
          Width = 436
          Height = 91
          Hint = 
            'All searches '#8211' Loads all previously saved searches.'#10#10#13#10'Only (re)' +
            'executed searches '#8211' Loads only searches you'#8217've actually executed' +
            '.'#10#10#13#10'Last NUM searches '#8211' Loads the latest NUM saved searches.'
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Constraints'
          Items.Strings = (
            'All searches'
            'Only (re)executed searches'
            'Last')
          TabOrder = 0
          StyleElements = [seFont, seClient]
        end
        object seSearchHistoryCount: TSpinEdit
          Left = 79
          Top = 77
          Width = 99
          Height = 24
          Hint = 'Number of history length in search history panel.'
          MaxValue = 500
          MinValue = 1
          TabOrder = 1
          Value = 10
        end
        object cbSaveResults: TCheckBox
          Left = 3
          Top = 119
          Width = 222
          Height = 17
          Hint = 'Save results of ripgrep search'
          Caption = 'Save / Load search results'
          TabOrder = 2
        end
        object cbLoadLastSearchHistories: TCheckBox
          Left = 3
          Top = 142
          Width = 222
          Height = 17
          Hint = 'Automatically load saved search history items on startup.'
          Caption = 'Load search history on startup'
          TabOrder = 3
        end
      end
      object seCmbHistoryCount: TSpinEdit
        Left = 141
        Top = 49
        Width = 99
        Height = 24
        Hint = 'Number of history length in combo boxes.'
        MaxValue = 100
        MinValue = 10
        TabOrder = 1
        Value = 10
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 115
    Top = 77
  end
  object ActionList1: TActionList
    Left = 306
    Top = 76
    object ActionOpenFileDialog: TAction
      Caption = 'ActionOpenFileDialog'
      ImageIndex = 2
    end
  end
  object SVGIconImageList1: TSVGIconImageList
    SVGIconItems = <
      item
        IconName = 'folder-opened'
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<path d="M1.50024 14H12.5002L12' +
          '.9802 13.63L15.6102 6.63L15.1302 6H14.0002V3.5L13.5002 3H7.71021' +
          'L6.85022 2.15002L6.50024 2H1.50024L1.00024 2.5V13.5L1.50024 14ZM' +
          '2.00024 3H6.29028L7.15027 3.84998L7.50024 4H13.0002V6H8.50024L8.' +
          '15027 6.15002L7.29028 7H3.50024L3.03027 7.33997L2.03027 10.42L2.' +
          '00024 3ZM12.1302 13H2.19019L3.86023 8H7.50024L7.85022 7.84998L8.' +
          '71021 7H14.5002L12.1302 13Z" fill="#424242"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'sync'
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<g clip-path="url(#clip0)">'#13#10'<p' +
          'ath fill-rule="evenodd" clip-rule="evenodd" d="M2.00607 8.26691L' +
          '0.780244 9.50003L0.000244141 8.73003L2.09024 6.66003L2.85024 6.6' +
          '7003L4.94024 8.79003L4.18024 9.55003L3.01372 8.36995C3.20304 10.' +
          '9586 5.36325 13 8.00024 13C9.91087 13 11.5712 11.9283 12.4129 10' +
          '.3533L13.2262 10.9499C12.1961 12.7709 10.2417 14 8.00024 14C4.77' +
          '597 14 2.14571 11.4568 2.00607 8.26691ZM12.9963 7.80051L11.7602 ' +
          '6.55005L11.0002 7.31005L13.0902 9.42005L13.8502 9.43005L15.9402 ' +
          '7.36005L15.1902 6.60005L13.9963 7.78004C13.8805 4.56823 11.2403 ' +
          '2 8.00024 2C5.83751 2 3.94204 3.14427 2.88622 4.86043L3.69588 5.' +
          '45436C4.56671 3.98506 6.16844 3 8.00024 3C10.6949 3 12.8916 5.13' +
          '157 12.9963 7.80051Z" fill="#424242"/>'#13#10'</g>'#13#10'<defs>'#13#10'<clipPath ' +
          'id="clip0">'#13#10'<rect width="16" height="16" fill="white" transform' +
          '="translate(0.000244141)"/>'#13#10'</clipPath>'#13#10'</defs>'#13#10'</svg>'#13#10
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
      end>
    Scaled = True
    Left = 384
    Top = 328
  end
end
