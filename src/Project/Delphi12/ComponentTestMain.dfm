object FormComponentTest: TFormComponentTest
  Left = 0
  Top = 0
  Caption = 'DRipGrepper Component Test Application'
  ClientHeight = 599
  ClientWidth = 982
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
    Width = 982
    Height = 599
    Align = alClient
    TabOrder = 0
    object pnlHistoryTest: TPanel
      Left = 1
      Top = 1
      Width = 980
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
    end
    object pnlNotifyingControls: TPanel
      Left = 1
      Top = 146
      Width = 980
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
      Width = 980
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
      Width = 980
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
      Top = 243
      Width = 980
      Height = 223
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
        Left = 296
        Top = 32
        Width = 121
        Height = 21
        EditLabel.Width = 68
        EditLabel.Height = 13
        EditLabel.Caption = 'TLabeled Edit:'
        TabOrder = 1
        Text = 'ljlkjj'
      end
      object LabeledComboBox1: TLabeledComboBox
        Left = 296
        Top = 80
        Width = 145
        Height = 21
        TabOrder = 2
        Text = 'LabeledComboBox1'
      end
      object chckbxcmbx1: TCheckBoxComboBox
        Left = 512
        Top = 16
        Width = 145
        Height = 21
        TabOrder = 3
        Text = 'chckbxcmbx1'
        CheckBox.Left = 412
        CheckBox.Top = 12
        CheckBox.Width = 80
        CheckBox.Height = 30
        CheckBox.Caption = 'ComboBox checkbox'
        CheckBox.TabOrder = 4
        CheckBox.WordWrap = True
        CheckBoxPosition = cpLeft
        CheckBoxSpacing = 20
      end
      object chckbxspndt1: TCheckBoxSpinEdit
        Left = 512
        Top = 72
        Width = 121
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 5
        Value = 0
        CheckBox.Left = 512
        CheckBox.Top = 52
        CheckBox.Width = 97
        CheckBox.Height = 17
        CheckBox.Caption = 'chckbxspndt1'
        CheckBox.TabOrder = 6
      end
    end
  end
end
