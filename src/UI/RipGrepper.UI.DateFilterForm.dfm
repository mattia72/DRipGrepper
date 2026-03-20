object DateFilterForm: TDateFilterForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Date Filter'
  ClientHeight = 260
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    380
    260)
  PixelsPerInch = 96
  TextHeight = 15
  object rgDateTimeType: TRadioGroup
    Left = 16
    Top = 8
    Width = 345
    Height = 50
    Caption = 'Timestamp Type'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Last Modified'
      'Created'
      'Last Accessed')
    TabOrder = 0
    OnClick = rgDateTimeTypeClick
  end
  object chkDateFrom: TCheckBox
    Left = 16
    Top = 74
    Width = 60
    Height = 17
    Caption = 'From:'
    TabOrder = 1
    OnClick = chkDateFromClick
  end
  object dtpDateFrom: TDateTimePicker
    Left = 85
    Top = 70
    Width = 140
    Height = 23
    Enabled = False
    TabOrder = 2
  end
  object dtpTimeFrom: TDateTimePicker
    Left = 235
    Top = 70
    Width = 100
    Height = 23
    Kind = dtkTime
    Enabled = False
    TabOrder = 3
  end
  object chkDateTo: TCheckBox
    Left = 16
    Top = 108
    Width = 60
    Height = 17
    Caption = 'To:'
    TabOrder = 4
    OnClick = chkDateToClick
  end
  object dtpDateTo: TDateTimePicker
    Left = 85
    Top = 104
    Width = 140
    Height = 23
    Enabled = False
    TabOrder = 5
  end
  object dtpTimeTo: TDateTimePicker
    Left = 235
    Top = 104
    Width = 100
    Height = 23
    Kind = dtkTime
    Enabled = False
    TabOrder = 6
  end
  object lblHint: TLabel
    Left = 16
    Top = 140
    Width = 345
    Height = 15
    Font.Color = clGrayText
    Font.Style = [fsItalic]
    ParentFont = False
    Visible = False
  end
  object btnOk: TButton
    Left = 110
    Top = 220
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object btnCancel: TButton
    Left = 195
    Top = 220
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object btnClear: TButton
    Left = 280
    Top = 220
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 9
    OnClick = btnClearClick
  end
end
