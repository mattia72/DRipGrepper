object fmFeedbackWizard: TfmFeedbackWizard
  Left = 285
  Top = 155
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'GExperts Feedback Wizard'
  ClientHeight = 515
  ClientWidth = 701
  Color = clBtnFace
  Constraints.MinHeight = 414
  Constraints.MinWidth = 554
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 14
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 701
    Height = 83
    Align = alTop
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    object pnlHeader: TPanel
      Left = 0
      Top = 0
      Width = 701
      Height = 43
      Align = alTop
      BevelOuter = bvNone
      Color = 6488064
      TabOrder = 0
      DesignSize = (
        701
        43)
      object lblFeeedback: TLabel
        Left = 9
        Top = 8
        Width = 680
        Height = 29
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'GExperts Feedback Wizard'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWhite
        Font.Height = -24
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
    end
    object pnlDescription: TPanel
      Left = 0
      Top = 43
      Width = 701
      Height = 40
      Align = alClient
      BevelInner = bvRaised
      BevelOuter = bvNone
      BorderWidth = 1
      FullRepaint = False
      TabOrder = 1
      object lblDescription: TLabel
        Left = 2
        Top = 2
        Width = 697
        Height = 36
        Align = alClient
        Alignment = taCenter
        Caption = 'Feedback Type'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Layout = tlCenter
      end
    end
  end
  object pnlContent: TPanel
    Left = 0
    Top = 83
    Width = 701
    Height = 396
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object Splitter: TSplitter
      Left = 493
      Top = 0
      Width = 5
      Height = 396
      Cursor = crHSplit
      Align = alRight
      AutoSnap = False
      Beveled = True
      MinSize = 100
    end
    object pgeInfo: TPageControl
      Left = 498
      Top = 0
      Width = 203
      Height = 396
      ActivePage = tshInfoHelp
      Align = alRight
      MultiLine = True
      TabIndex = 0
      TabOrder = 0
      object tshInfoHelp: TTabSheet
        Caption = 'Help'
        object mmoInfoHelp: TMemo
          Left = 0
          Top = 0
          Width = 195
          Height = 367
          Align = alClient
          Color = 15858687
          HideSelection = False
          ReadOnly = True
          TabOrder = 0
          OnEnter = InfoMemoEnter
        end
      end
      object tshInfoExample: TTabSheet
        Caption = 'Example'
        ImageIndex = 1
        object mmoInfoExample: TMemo
          Left = 0
          Top = 0
          Width = 195
          Height = 367
          Align = alClient
          Color = 15858687
          ReadOnly = True
          TabOrder = 0
          OnEnter = InfoMemoEnter
        end
      end
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 493
      Height = 396
      Align = alClient
      BorderWidth = 1
      FullRepaint = False
      TabOrder = 1
      object pgeMain: TPageControl
        Left = 2
        Top = 2
        Width = 489
        Height = 392
        ActivePage = tshType
        Align = alClient
        TabIndex = 0
        TabOrder = 0
        OnResize = pgeMainResize
        object tshType: TTabSheet
          Caption = 'Feedback Type'
          object l_BugReportUrl: TLabel
            Left = 40
            Top = 176
            Width = 107
            Height = 14
            Caption = 'http://bla.blub.com'
          end
          object l_FeatureUrl: TLabel
            Left = 40
            Top = 224
            Width = 107
            Height = 14
            Caption = 'http://bla.blub.com'
          end
          object l_Verify: TLabel
            Left = 8
            Top = 8
            Width = 472
            Height = 16
            Caption = 
              'Please verify you are using the latest GExperts release before s' +
              'ending in feedback.'
            Font.Charset = ANSI_CHARSET
            Font.Color = clRed
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object l_URL: TLabel
            Left = 8
            Top = 32
            Width = 184
            Height = 14
            Caption = 'https://gexperts.dummzeuch.de/'
          end
          object rb_BugReport: TRadioButton
            Left = 16
            Top = 152
            Width = 113
            Height = 17
            Caption = '&Bug Report'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = rgpFeedbackTypeClick
          end
          object rb_FeatureRequest: TRadioButton
            Left = 16
            Top = 200
            Width = 113
            Height = 17
            Caption = '&Feature Request'
            TabOrder = 1
            OnClick = rgpFeedbackTypeClick
          end
        end
        object tshDescription: TTabSheet
          Caption = 'Bug Description'
          ImageIndex = 1
          object mmoDescription: TMemo
            Left = 0
            Top = 0
            Width = 481
            Height = 363
            Align = alClient
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
        object tshBugDetails: TTabSheet
          Caption = 'Bug Details'
          ImageIndex = 2
          object gbxBugDetails: TGroupBox
            Left = 8
            Top = 6
            Width = 305
            Height = 129
            Caption = ' Bug Details '
            TabOrder = 0
            object lblPercent: TLabel
              Left = 70
              Top = 48
              Width = 78
              Height = 14
              Caption = '% of the &time'
              FocusControl = edtPercent
            end
            object chkProjectSpecific: TCheckBox
              Left = 13
              Top = 98
              Width = 288
              Height = 17
              Caption = 'T&he bug is project specific'
              TabOrder = 3
            end
            object chkMultipleMachines: TCheckBox
              Left = 13
              Top = 74
              Width = 288
              Height = 17
              Caption = 'The bug occurs on &multiple machines'
              TabOrder = 2
            end
            object edtPercent: TEdit
              Left = 26
              Top = 44
              Width = 39
              Height = 22
              MaxLength = 3
              TabOrder = 1
              Text = '100'
              OnKeyPress = edtPercentKeyPress
            end
            object chkReproducible: TCheckBox
              Left = 13
              Top = 21
              Width = 288
              Height = 17
              Caption = 'The bug is &reproducible'
              Checked = True
              State = cbChecked
              TabOrder = 0
              OnClick = chkReproducibleClick
            end
          end
        end
        object tshBugSteps: TTabSheet
          Caption = 'Bug Duplication Steps'
          ImageIndex = 3
          object mmoBugSteps: TMemo
            Left = 0
            Top = 0
            Width = 481
            Height = 363
            Align = alClient
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
        object tshConfiguration: TTabSheet
          Caption = 'Configuration Data to Report'
          ImageIndex = 5
          object gbxConfigurationData: TGroupBox
            Left = 8
            Top = 2
            Width = 305
            Height = 188
            Caption = ' Configuration Data to Report '
            TabOrder = 0
            object chkGExpertsVer: TCheckBox
              Left = 16
              Top = 20
              Width = 280
              Height = 17
              Caption = 'GExperts &Version'
              Checked = True
              Enabled = False
              State = cbChecked
              TabOrder = 0
            end
            object chkOS: TCheckBox
              Left = 16
              Top = 60
              Width = 280
              Height = 17
              Caption = '&Operating System'
              Checked = True
              Enabled = False
              State = cbChecked
              TabOrder = 2
            end
            object chkExperts: TCheckBox
              Left = 16
              Top = 100
              Width = 280
              Height = 17
              Caption = 'Installed E&xperts'
              Checked = True
              State = cbChecked
              TabOrder = 4
            end
            object chkIdeVer: TCheckBox
              Left = 16
              Top = 40
              Width = 280
              Height = 17
              Caption = '&IDE Version'
              Checked = True
              Enabled = False
              State = cbChecked
              TabOrder = 1
            end
            object chkPackages: TCheckBox
              Left = 16
              Top = 120
              Width = 280
              Height = 17
              Caption = 'Inst&alled Packages'
              Checked = True
              State = cbChecked
              TabOrder = 5
            end
            object chkLocaleKeyboard: TCheckBox
              Left = 16
              Top = 140
              Width = 280
              Height = 17
              Caption = 'Locale/&Keyboard Information'
              Checked = True
              State = cbChecked
              TabOrder = 6
            end
            object chkCpu: TCheckBox
              Left = 16
              Top = 160
              Width = 280
              Height = 17
              Caption = 'CP&U Information'
              Checked = True
              State = cbChecked
              TabOrder = 7
            end
            object chkGExpertsSettings: TCheckBox
              Left = 16
              Top = 80
              Width = 280
              Height = 17
              Caption = '&GExperts Settings'
              Checked = True
              State = cbChecked
              TabOrder = 3
            end
          end
        end
        object tshReport: TTabSheet
          Caption = 'Completed Bug Report'
          ImageIndex = 4
          object mmoReport: TMemo
            Left = 0
            Top = 0
            Width = 364
            Height = 363
            Align = alClient
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
          object pnlReportButtons: TPanel
            Left = 364
            Top = 0
            Width = 117
            Height = 363
            Align = alRight
            BevelOuter = bvNone
            TabOrder = 1
            object btnSave: TButton
              Left = 5
              Top = 46
              Width = 110
              Height = 27
              Caption = '&Save Report...'
              TabOrder = 1
              OnClick = btnSaveClick
            end
            object btnCopy: TButton
              Left = 5
              Top = 6
              Width = 110
              Height = 27
              Caption = 'Copy to &Clipboard'
              TabOrder = 0
              OnClick = btnCopyClick
            end
            object btnEmail: TButton
              Left = 5
              Top = 86
              Width = 110
              Height = 27
              Caption = 'Open &Browser'
              TabOrder = 2
              OnClick = btnEmailClick
            end
          end
        end
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 479
    Width = 701
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 2
    OnResize = pnlButtonsResize
    object pnlButtonsCenter: TPanel
      Left = 152
      Top = 3
      Width = 289
      Height = 30
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      object btnPrev: TButton
        Left = 11
        Top = 3
        Width = 74
        Height = 25
        Action = actPrev
        TabOrder = 0
      end
      object btnNext: TButton
        Left = 94
        Top = 3
        Width = 73
        Height = 25
        Action = actNext
        Default = True
        TabOrder = 1
      end
      object btnCancel: TButton
        Left = 205
        Top = 3
        Width = 74
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        TabOrder = 2
        OnClick = btnCancelClick
      end
    end
  end
  object Actions: TActionList
    Left = 475
    Top = 328
    object actPrev: TAction
      Caption = '<  &Prev'
      OnExecute = actPrevExecute
      OnUpdate = actPrevUpdate
    end
    object actNext: TAction
      Caption = '&Next  >'
      OnExecute = actNextExecute
      OnUpdate = actNextUpdate
    end
  end
  object dlgSaveReport: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 512
    Top = 328
  end
end
