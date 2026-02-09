object HistoryEditTestForm: THistoryEditTestForm
  Left = 0
  Top = 0
  Caption = 'THistoryButtonedEdit Test'
  ClientHeight = 400
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 20
    Top = 8
    Width = 130
    Height = 13
    Caption = 'THistoryButtonedEdit Test:'
  end
  object Button1: TButton
    Left = 20
    Top = 60
    Width = 100
    Height = 25
    Caption = 'Show History'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 130
    Top = 60
    Width = 100
    Height = 25
    Caption = 'Clear History'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 20
    Top = 100
    Width = 460
    Height = 280
    Lines.Strings = (
      'Instructions:'
      '1. Type text in the edit control'
      '2. Press ENTER to add to history'
      '3. Use UP/DOWN arrows to navigate history'
      '4. Click "Show History" to see current items'
      '5. Click "Clear History" to reset')
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
