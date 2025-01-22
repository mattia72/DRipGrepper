object ExtensionSettingsForm: TExtensionSettingsForm
  Left = 0
  Top = 0
  Caption = 'ExtensionSettingsForm'
  ClientHeight = 292
  ClientWidth = 572
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
    Width = 572
    Height = 292
    Align = alClient
    TabOrder = 0
    object grpShortcuts: TGroupBox
      Left = 1
      Top = 1
      Width = 570
      Height = 91
      Align = alTop
      Caption = 'Shortcuts'
      TabOrder = 0
      object lblOpenWith: TLabel
        Left = 36
        Top = 28
        Width = 60
        Height = 15
        Caption = 'Open With:'
      end
      object lblSearch: TLabel
        Left = 16
        Top = 53
        Width = 85
        Height = 15
        Caption = 'Search Selected:'
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
      Left = 1
      Top = 92
      Width = 570
      Height = 118
      Align = alTop
      Caption = 'Install as Delphi IDE Extension'
      TabOrder = 1
      DesignSize = (
        570
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
        Left = 480
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
        Width = 458
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        Color = clInfoBk
        Images = SVGIconImageList1
        LeftButton.ImageIndex = 0
        LeftButton.ImageName = 'sync'
        LeftButton.Visible = True
        ReadOnly = True
        RightButton.ImageIndex = 2
        RightButton.ImageName = 'file-document-outline'
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
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M12,18A6,6 0 0,1 6,12C6,11 6.25,10.03 6.7,9.2L5.24,7.74C4.4' +
          '6,8.97 4,10.43 4,12A8,8 0 0,0 12,20V23L16,19L12,15M12,4V1L8,5L12' +
          ',9V6A6,6 0 0,1 18,12C18,13 17.75,13.97 17.3,14.8L18.76,16.26C19.' +
          '54,15.03 20,13.57 20,12A8,8 0 0,0 12,4Z" /></svg>'
      end
      item
        IconName = 'folder-open-outline'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M6.1,10L4,18V8H21A2,2 0 0,0 19,6H12L10,4H4A2,2 0 0,0 2,6V18' +
          'A2,2 0 0,0 4,20H19C19.9,20 20.7,19.4 20.9,18.5L23.2,10H6.1M19,18' +
          'H6L7.6,12H20.6L19,18Z" /></svg>'
      end
      item
        IconName = 'file-document-outline'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M6,2A2,2 0 0,0 4,4V20A2,2 0 0,0 6,22H18A2,2 0 0,0 20,20V8L1' +
          '4,2H6M6,4H13V9H18V20H6V4M8,12V14H16V12H8M8,16V18H13V16H8Z" /></s' +
          'vg>'
      end>
    Scaled = True
    Left = 320
    Top = 98
  end
end
