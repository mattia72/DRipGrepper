inherited AppSettingsForm: TAppSettingsForm
  Caption = 'AppSettingsForm'
  ClientHeight = 476
  ClientWidth = 514
  Color = clBtnFace
  ShowHint = True
  OnShow = FormShow
  TextHeight = 15
  inherited PanelBottom: TPanel
    Top = 431
    Width = 514
    TabOrder = 1
  end
  object ScrollBox1: TScrollBox [1]
    Left = 0
    Top = 0
    Width = 514
    Height = 431
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 0
    DesignSize = (
      514
      431)
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
      Width = 494
      Height = 23
      Hint = 'Full Path to rg.exe'
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
      Width = 494
      Height = 63
      Hint = 'Output of "rg.exe --version"'
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
      Width = 499
      Height = 263
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Settings'
      TabOrder = 2
      DesignSize = (
        499
        263)
      object Label2: TLabel
        Left = 20
        Top = 23
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
        Hint = 'Select Shell Format to Copy the RipGrep Command to the Clipboard'
        ItemIndex = 0
        TabOrder = 0
        Text = 'PowerShell '
        Items.Strings = (
          'PowerShell '
          'DOS')
      end
      object grpSaveLoad: TGroupBox
        AlignWithMargins = True
        Left = 16
        Top = 83
        Width = 454
        Height = 168
        Margins.Left = 20
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Startup behaviour'
        TabOrder = 2
        DesignSize = (
          454
          168)
        object rgModeLoadSeraches: TRadioGroup
          AlignWithMargins = True
          Left = 15
          Top = 45
          Width = 418
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
            'Last                       searches')
          TabOrder = 0
          StyleElements = [seFont, seClient]
          OnClick = rgModeLoadSerachesClick
        end
        object seSearchHistoryCount: TSpinEdit
          Left = 68
          Top = 105
          Width = 52
          Height = 24
          Hint = 'Maximum Number of Entries in Search History Panel'
          MaxValue = 500
          MinValue = 1
          TabOrder = 1
          Value = 10
        end
        object cbSaveResults: TCheckBox
          Left = 15
          Top = 142
          Width = 273
          Height = 17
          Hint = 'Save Results of RipGrep Search'
          Caption = 'Load found files and matches from last session'
          TabOrder = 2
          OnClick = cbSaveResultsClick
        end
        object cbLoadLastSearchHistories: TCheckBox
          Left = 15
          Top = 22
          Width = 222
          Height = 17
          Hint = 'Automatically Load Saved Search History Items on Startup'
          Caption = 'Load search history from last session'
          TabOrder = 3
          OnClick = cbLoadLastSearchHistoriesClick
        end
      end
      object seCmbHistoryCount: TSpinEdit
        Left = 141
        Top = 49
        Width = 99
        Height = 24
        Hint = 'Maximum Number of Entries in Combo Boxes'
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
  object SVGIconImageList1: TSVGIconVirtualImageList
    ImageCollection = SVGIconDataModule.SVGIconImageCollection1
    Images = <
      item
        CollectionName = 'folder-opened'
        Name = 'folder-opened'
      end
      item
        CollectionName = 'sync'
        Name = 'sync'
      end
      item
        CollectionName = 'rocket'
        Name = 'rocket'
      end>
    Scaled = True
    Left = 384
    Top = 328
  end
end
