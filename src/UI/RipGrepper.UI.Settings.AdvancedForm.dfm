object AdvancedForm: TAdvancedForm
  Left = 0
  Top = 0
  Caption = 'Advanced'
  ClientHeight = 221
  ClientWidth = 524
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
    Width = 524
    Height = 221
    Align = alClient
    TabOrder = 0
    DesignSize = (
      520
      217)
    object grpAdvanced: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 508
      Height = 178
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Advanced'
      TabOrder = 0
      DesignSize = (
        508
        178)
      object Label1: TLabel
        Left = 16
        Top = 126
        Width = 129
        Height = 15
        Caption = 'Configuaration file path:'
      end
      object chExpertMode: TCheckBox
        Left = 16
        Top = 22
        Width = 97
        Height = 24
        Hint = 'In expert mode, rg.exe can be freely parameterized.'
        Caption = 'Expert mode'
        TabOrder = 0
      end
      object btnedtIniFilePath: TButtonedEdit
        Left = 16
        Top = 147
        Width = 483
        Height = 23
        Hint = 
          'Full path to the settings file. It can be opened using the launc' +
          'h icon.'
        Anchors = [akLeft, akTop, akRight]
        Color = clInfoBk
        Images = SVGIconImageList1
        LeftButton.DisabledImageIndex = 1
        LeftButton.DisabledImageName = 'sync'
        LeftButton.Hint = 'Refresh...'
        LeftButton.ImageIndex = 1
        LeftButton.ImageName = 'sync'
        LeftButton.Visible = True
        ReadOnly = True
        RightButton.DisabledImageIndex = 2
        RightButton.DisabledImageName = 'rocket'
        RightButton.Hint = 'Open With...'
        RightButton.ImageIndex = 2
        RightButton.ImageName = 'rocket'
        RightButton.Visible = True
        TabOrder = 2
        Text = 'Path to ini file...'
        TextHint = 'Path to ini file...'
        OnEnter = btnedtIniFilePathEnter
        OnExit = btnedtIniFilePathExit
        OnLeftButtonClick = btnedtIniFilePathLeftButtonClick
        OnRightButtonClick = btnedtIniFilePathRightButtonClick
      end
      object gbTrace: TGroupBox
        Left = 16
        Top = 52
        Width = 480
        Height = 68
        Hint = 'Debug trace can be viewed in a debug viewer eg. DebugView++.'
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Debug trace filters'
        TabOrder = 1
        object chEnd: TCheckBox
          Left = 385
          Top = 20
          Width = 75
          Height = 17
          Caption = 'End'
          TabOrder = 5
        end
        object chBegin: TCheckBox
          Left = 308
          Top = 20
          Width = 75
          Height = 17
          Caption = 'Begin'
          TabOrder = 4
        end
        object chError: TCheckBox
          Left = 3
          Top = 20
          Width = 75
          Height = 17
          Caption = 'Error'
          TabOrder = 0
        end
        object chWarning: TCheckBox
          Left = 79
          Top = 20
          Width = 75
          Height = 17
          Caption = 'Warning'
          TabOrder = 1
        end
        object chInfo: TCheckBox
          Left = 155
          Top = 20
          Width = 75
          Height = 17
          Caption = 'Info'
          TabOrder = 2
        end
        object chRegex: TCheckBox
          Left = 3
          Top = 43
          Width = 75
          Height = 17
          Caption = 'Regex'
          TabOrder = 7
          OnClick = chRegexClick
        end
        object edtRegex: TEdit
          Left = 79
          Top = 42
          Width = 121
          Height = 23
          TabOrder = 6
          TextHint = 'Regex'
        end
        object chVerbose: TCheckBox
          Left = 232
          Top = 20
          Width = 75
          Height = 17
          Caption = 'Verbose'
          TabOrder = 3
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 250
    Top = 112
  end
  object ActionList1: TActionList
    Left = 316
    Top = 116
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
    Top = 108
  end
end
