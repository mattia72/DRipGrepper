object FormComponentTest: TFormComponentTest
  Left = 0
  Top = 0
  Caption = 'DRipGrepper Component Test Application'
  ClientHeight = 599
  ClientWidth = 751
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 751
    Height = 599
    Align = alClient
    TabOrder = 0
    object pnlHistoryTest: TPanel
      Left = 1
      Top = 1
      Width = 749
      Height = 145
      Align = alTop
      TabOrder = 0
      object lblHistoryTest: TLabel
        Left = 16
        Top = 16
        Width = 263
        Height = 13
        Caption = 'THistoryButtonedEdit Test (Design && Runtime)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object btnAddToHistory: TButton
        Left = 16
        Top = 72
        Width = 105
        Height = 25
        Caption = 'Add to History'
        TabOrder = 0
        OnClick = btnAddToHistoryClick
      end
      object btnClearHistory: TButton
        Left = 127
        Top = 72
        Width = 105
        Height = 25
        Caption = 'Clear History'
        TabOrder = 1
        OnClick = btnClearHistoryClick
      end
      object btnShowHistory: TButton
        Left = 238
        Top = 72
        Width = 105
        Height = 25
        Caption = 'Show History'
        TabOrder = 2
        OnClick = btnShowHistoryClick
      end
      object ntfyngspndt1: TNotifyingSpinEdit
        Left = 384
        Top = 74
        Width = 41
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 0
      end
      object optnchckbxspn1: TOptionCheckBoxSpin
        Left = 464
        Top = 74
        Width = 185
        Height = 29
        BevelOuter = bvNone
        Caption = 'lhjgljkg'
        Enabled = True
        TabOrder = 4
      end
      object optnchckbxcmb1: TOptionCheckBoxCombo
        Left = 464
        Top = 109
        Width = 185
        Height = 28
        BevelOuter = bvNone
        Caption = 'asdfasd'
        Enabled = True
        TabOrder = 5
      end
    end
    object pnlNotifyingControls: TPanel
      Left = 1
      Top = 146
      Width = 749
      Height = 145
      Align = alTop
      TabOrder = 1
      object lblNotifyingTest: TLabel
        Left = 16
        Top = 16
        Width = 189
        Height = 13
        Caption = 'Notifying Controls Test (Runtime)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblSpinEdit: TLabel
        Left = 33
        Top = 48
        Width = 88
        Height = 13
        Caption = 'Notifying SpinEdit:'
      end
      object lblComboBox: TLabel
        Left = 20
        Top = 78
        Width = 101
        Height = 13
        Caption = 'Notifying ComboBox:'
      end
      object lblCheckBox: TLabel
        Left = 24
        Top = 108
        Width = 97
        Height = 13
        Caption = 'Notifying CheckBox:'
      end
    end
    object pnlOptionControls: TPanel
      Left = 1
      Top = 291
      Width = 749
      Height = 95
      Align = alTop
      TabOrder = 2
      object lblOptionControls: TLabel
        Left = 16
        Top = 16
        Width = 176
        Height = 13
        Caption = 'Option Controls Test (Runtime)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object memoLog: TMemo
      Left = 1
      Top = 466
      Width = 749
      Height = 132
      Align = alBottom
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 3
    end
    object pnlRuntimeControls: TPanel
      Left = 1
      Top = 392
      Width = 749
      Height = 74
      Align = alBottom
      TabOrder = 4
      object btnTestRuntimeCreation: TButton
        Left = 16
        Top = 16
        Width = 185
        Height = 25
        Caption = 'Create Runtime Control'
        TabOrder = 0
        OnClick = btnTestRuntimeCreationClick
      end
      object lbledt1: TLabeledEdit
        Left = 528
        Top = 24
        Width = 121
        Height = 21
        EditLabel.Width = 32
        EditLabel.Height = 13
        EditLabel.Caption = 'lkjhlkjh'
        TabOrder = 1
        Text = 'ljlkjj'
      end
    end
  end
end
