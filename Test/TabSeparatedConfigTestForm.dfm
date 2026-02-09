object TabSeparatedConfigTestMainForm: TTabSeparatedConfigTestMainForm
  Left = 0
  Top = 0
  Caption = 'Tab Separated Config Form - Test Application'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 124
    Height = 15
    Caption = 'Input Data (Tab-delimited):'
  end
  object Label2: TLabel
    Left = 24
    Top = 264
    Width = 41
    Height = 15
    Caption = 'Results:'
  end
  object Label3: TLabel
    Left = 24
    Top = 192
    Width = 153
    Height = 15
    Caption = 'Column Headers (comma-separated):'
  end
  object Label4: TLabel
    Left = 24
    Top = 500
    Width = 740
    Height = 45
    Caption = 
      'Instructions:'#13#10'1. Enter tab-separated data in the input field (' +
      'one row per line)'#13#10'2. Optionally enable Test Action to see the ' +
      'test button in action'#13#10'3. Click "Open Tab Config Form" to test ' +
      'the form'
    WordWrap = True
  end
  object btnOpenForm: TButton
    Left = 600
    Top = 213
    Width = 176
    Height = 32
    Caption = 'Open Tab Config Form'
    TabOrder = 0
    OnClick = btnOpenFormClick
  end
  object memoResults: TMemo
    Left = 24
    Top = 285
    Width = 752
    Height = 192
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object memoInput: TMemo
    Left = 24
    Top = 45
    Width = 752
    Height = 128
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object chkEnableTestAction: TCheckBox
    Left = 24
    Top = 223
    Width = 161
    Height = 17
    Caption = 'Enable Test Action'
    TabOrder = 3
  end
  object edtHeaders: TEdit
    Left = 200
    Top = 189
    Width = 576
    Height = 23
    TabOrder = 4
    Text = 'Name,Email,Position'
  end
  object btnTestAction: TButton
    Left = 208
    Top = 218
    Width = 120
    Height = 25
    Action = ActionTest
    TabOrder = 5
  end
  object ActionList1: TActionList
    Left = 712
    Top = 16
    object ActionTest: TAction
      Caption = 'Test Action'
      OnExecute = ActionTestExecute
    end
  end
end
