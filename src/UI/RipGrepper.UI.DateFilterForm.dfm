inherited DateFilterForm: TDateFilterForm
  BorderStyle = bsDialog
  Caption = 'Date Filter'
  ClientHeight = 201
  ClientWidth = 361
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 361
    Height = 156
    Align = alClient
    Color = clBtnHighlight
    ParentBackground = False
    TabOrder = 0
    object lblHint: TLabel
      Left = 16
      Top = 64
      Width = 319
      Height = 15
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsItalic]
      ParentFont = False
      Visible = False
    end
    object chkDateFrom: TCheckBox
      Left = 16
      Top = 89
      Width = 60
      Height = 17
      Caption = 'From:'
      TabOrder = 0
      OnClick = chkDateFromClick
    end
    object chkDateTo: TCheckBox
      Left = 16
      Top = 118
      Width = 60
      Height = 17
      Caption = 'To:'
      TabOrder = 1
      OnClick = chkDateToClick
    end
    object dtpDateFrom: TDateTimePicker
      Left = 85
      Top = 85
      Width = 140
      Height = 23
      Date = 46104.000000000000000000
      Time = 0.453621620370540800
      Enabled = False
      TabOrder = 2
    end
    object dtpDateTo: TDateTimePicker
      Left = 85
      Top = 114
      Width = 140
      Height = 23
      Date = 46104.000000000000000000
      Time = 0.453621620370540800
      Enabled = False
      TabOrder = 3
    end
    object dtpTimeFrom: TDateTimePicker
      Left = 235
      Top = 85
      Width = 100
      Height = 23
      Date = 46104.000000000000000000
      Time = 0.453621620370540800
      Enabled = False
      Kind = dtkTime
      TabOrder = 4
    end
    object dtpTimeTo: TDateTimePicker
      Left = 235
      Top = 114
      Width = 100
      Height = 23
      Date = 46104.000000000000000000
      Time = 0.453621620370540800
      Enabled = False
      Kind = dtkTime
      TabOrder = 5
    end
    object rgDateTimeType: TRadioGroup
      Left = 16
      Top = 8
      Width = 319
      Height = 50
      Caption = 'Timestamp Type'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'Modified'
        'Created'
        'Accessed')
      TabOrder = 6
      OnClick = rgDateTimeTypeClick
    end
  end
  inherited PanelBottom: TPanel
  end
end
