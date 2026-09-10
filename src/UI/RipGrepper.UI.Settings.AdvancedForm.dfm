inherited AdvancedForm: TAdvancedForm
  Caption = 'Advanced'
  ClientHeight = 355
  ClientWidth = 559
  Color = clBtnFace
  ShowHint = True
  OnShow = FormShow
  TextHeight = 15
  inherited PanelBottom: TPanel
    Top = 310
    Width = 559
    TabOrder = 1
    inherited btnOk: TButton
      Left = 267
    end
    inherited btnCancel: TButton
      Left = 348
    end
  end
  object ScrollBox1: TScrollBox [1]
    Left = 0
    Top = 0
    Width = 559
    Height = 310
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 0
    DesignSize = (
      559
      310)
    object grpAdvanced: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 558
      Height = 293
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Advanced'
      TabOrder = 0
      DesignSize = (
        558
        293)
      object Label1: TLabel
        Left = 16
        Top = 27
        Width = 123
        Height = 15
        Caption = 'Configuration file path:'
      end
      object chExpertMode: TCheckBox
        Left = 16
        Top = 77
        Width = 220
        Height = 24
        Hint = 
          'In Expert mode, rg.exe can be freely parameterized.'#13#10'Applies to ' +
          'new searches only. History items use their own stored setting.'#13#10 +
          'Can also be toggled in the Search Form.'
        Caption = 'Expert mode (for new searches)'
        TabOrder = 0
      end
      object btnedtIniFilePath: TButtonedEdit
        Left = 16
        Top = 48
        Width = 530
        Height = 23
        Hint = 
          'Full Path to the Settings File. It Can Be Opened Using the Launc' +
          'h Icon'
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
        AlignWithMargins = True
        Left = 16
        Top = 107
        Width = 530
        Height = 174
        Hint = 'Debug Trace Can Be Viewed in a Debug Viewer e.g. DebugView++'
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Debug trace'
        TabOrder = 1
        DesignSize = (
          530
          174)
        object lblTraceOutput: TLabel
          Left = 19
          Top = 84
          Width = 45
          Height = 15
          Caption = 'Trace to:'
        end
        object lblLogFilePath: TLabel
          Left = 19
          Top = 110
          Width = 69
          Height = 15
          Caption = 'Log file path:'
        end
        object lblLogCreation: TLabel
          Left = 19
          Top = 139
          Width = 82
          Height = 15
          Caption = 'Creation mode:'
        end
        object chEnd: TCheckBox
          Left = 402
          Top = 30
          Width = 75
          Height = 17
          Caption = 'End'
          TabOrder = 5
        end
        object chBegin: TCheckBox
          Left = 325
          Top = 30
          Width = 75
          Height = 17
          Caption = 'Begin'
          TabOrder = 4
        end
        object chError: TCheckBox
          Left = 20
          Top = 30
          Width = 75
          Height = 17
          Caption = 'Error'
          TabOrder = 0
        end
        object chWarning: TCheckBox
          Left = 96
          Top = 30
          Width = 75
          Height = 17
          Caption = 'Warning'
          TabOrder = 1
        end
        object chInfo: TCheckBox
          Left = 172
          Top = 30
          Width = 75
          Height = 17
          Caption = 'Info'
          TabOrder = 2
        end
        object chRegex: TCheckBox
          Left = 20
          Top = 53
          Width = 75
          Height = 23
          Caption = 'Regex'
          TabOrder = 7
          OnClick = chRegexClick
        end
        object edtRegex: TEdit
          Left = 110
          Top = 53
          Width = 121
          Height = 23
          TabOrder = 6
          TextHint = 'Regex'
        end
        object chVerbose: TCheckBox
          Left = 249
          Top = 30
          Width = 75
          Height = 17
          Caption = 'Verbose'
          TabOrder = 3
        end
        object chTraceToDebugView: TCheckBox
          Left = 110
          Top = 83
          Width = 110
          Height = 17
          Hint = 
            'Output trace messages via OutputDebugString (use DebugView++ to ' +
            'see them)'
          Caption = 'Debug Viewer'
          TabOrder = 8
        end
        object chTraceToFile: TCheckBox
          Left = 226
          Top = 83
          Width = 50
          Height = 17
          Hint = 'Write trace messages to a log file'
          Caption = 'File'
          TabOrder = 9
          OnClick = chTraceToFileClick
        end
        object edtLogFilePath: TButtonedEdit
          Left = 110
          Top = 106
          Width = 409
          Height = 23
          Hint = 'Full path to the log file'
          Anchors = [akLeft, akTop, akRight]
          Images = SVGIconImageList1
          RightButton.Hint = 'Browse...'
          RightButton.ImageIndex = 0
          RightButton.ImageName = 'folder-opened'
          RightButton.Visible = True
          TabOrder = 10
          TextHint = 'Default: application directory'
          OnRightButtonClick = edtLogFilePathRightButtonClick
        end
        object cmbLogCreation: TComboBox
          Left = 110
          Top = 136
          Width = 180
          Height = 23
          Hint = 'How the log file is created on application start'
          Style = csDropDownList
          TabOrder = 11
        end
        object btnClearLogFile: TButton
          Left = 298
          Top = 134
          Width = 110
          Height = 25
          Hint = 'Clear the log file (delete all content)'
          Caption = 'Clear log file'
          ImageIndex = 3
          ImageName = 'outline-restore-from-trash'
          Images = SVGIconImageList1
          TabOrder = 12
          OnClick = btnClearLogFileClick
        end
      end
    end
  end
  inherited SVGImageListBottomPanel: TSVGIconVirtualImageList
    Left = 370
    Top = 3
  end
  object OpenDialog1: TOpenDialog
    Left = 290
    Top = 65533
  end
  object ActionList1: TActionList
    Left = 436
    Top = 1
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
      end
      item
        CollectionName = 'outline-restore-from-trash'
        Name = 'outline-restore-from-trash'
      end>
    Scaled = True
    Left = 499
    Top = 8
  end
end
