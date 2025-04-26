object ExtensionSettingsForm: TExtensionSettingsForm
  Left = 0
  Top = 0
  Caption = 'ExtensionSettingsForm'
  ClientHeight = 273
  ClientWidth = 636
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 15
  object pnlMiddle: TPanel
    Left = 0
    Top = 0
    Width = 636
    Height = 273
    Align = alClient
    TabOrder = 0
    DesignSize = (
      636
      273)
    object grpShortcuts: TGroupBox
      Left = 17
      Top = 8
      Width = 602
      Height = 100
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Shortcuts'
      TabOrder = 0
      object lblOpenWith: TLabel
        Left = 36
        Top = 28
        Width = 67
        Height = 15
        Caption = 'Open with...:'
      end
      object lblSearch: TLabel
        Left = 16
        Top = 53
        Width = 84
        Height = 15
        Caption = 'Search selected:'
      end
      object hkedtOpenWidth: THotKey
        Left = 114
        Top = 22
        Width = 121
        Height = 23
        HotKey = 32833
        TabOrder = 0
      end
      object hkedtSearchSelected: THotKey
        Left = 114
        Top = 51
        Width = 121
        Height = 23
        HotKey = 32833
        TabOrder = 1
      end
    end
    object grpInstallation: TGroupBox
      Left = 17
      Top = 114
      Width = 602
      Height = 118
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Install as Delphi IDE Extension'
      TabOrder = 1
      DesignSize = (
        602
        118)
      object cmbDelphiVersions: TComboBox
        Left = 16
        Top = 26
        Width = 145
        Height = 23
        TabOrder = 0
        Text = 'cmbDelphiVersions'
        OnChange = cmbDelphiVersionsChange
      end
      object btnInstallPackage: TButton
        Left = 510
        Top = 55
        Width = 78
        Height = 21
        Action = ActionExtensionInstall
        Anchors = [akRight, akBottom]
        TabOrder = 1
      end
      object btnedtDllPath: TButtonedEdit
        Left = 16
        Top = 55
        Width = 488
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        Color = clInfoBk
        Images = SVGIconImageList1
        LeftButton.ImageIndex = 0
        LeftButton.ImageName = 'sync'
        LeftButton.Visible = True
        ReadOnly = True
        RightButton.DisabledImageIndex = 1
        RightButton.DisabledImageName = 'folder-opened'
        RightButton.ImageIndex = 1
        RightButton.ImageName = 'folder-opened'
        RightButton.Visible = True
        TabOrder = 2
        Text = 'Dll path...'
        TextHint = 'Rg.exe path...'
        OnRightButtonClick = btnedtDllPathRightButtonClick
      end
    end
  end
  object ActionList1: TActionList
    Images = SVGIconImageList1
    Left = 488
    Top = 40
    object ActionExtensionInstall: TAction
      Caption = 'Install...'
      OnExecute = ActionExtensionInstallExecute
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 384
    Top = 40
  end
  object SVGIconImageList1: TSVGIconImageList
    SVGIconItems = <
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
        IconName = 'file'
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<path fill-rule="evenodd" clip-' +
          'rule="evenodd" d="M10.5702 1.14L13.8502 4.44L14.0002 4.8V14.5L13' +
          '.5002 15H2.50024L2.00024 14.5V1.5L2.50024 1H10.2202L10.5702 1.14' +
          'ZM10.0002 5H13.0002L10.0002 2V5ZM3.00024 2V14H13.0002V6H9.50024L' +
          '9.00024 5.5V2H3.00024ZM11.0002 7H5.00024V8H11.0002V7ZM5.00024 9H' +
          '11.0002V10H5.00024V9ZM11.0002 11H5.00024V12H11.0002V11Z" fill="#' +
          '424242"/>'#13#10'</svg>'#13#10
      end>
    Scaled = True
    Left = 320
    Top = 98
  end
end
