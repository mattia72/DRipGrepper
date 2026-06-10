inherited ColorSettingsForm: TColorSettingsForm
  Caption = 'Fonts and Colors'
  ClientHeight = 508
  ClientWidth = 660
  Color = clBtnFace
  OnShow = FormShow
  TextHeight = 15
  inherited PanelBottom: TPanel
    Top = 463
    Width = 660
    TabOrder = 3
  end
  object pnlBottom: TPanel [1]
    Left = 0
    Top = 422
    Width = 660
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    StyleElements = [seFont, seClient]
    object btnLoadDefaults: TButton
      Left = 290
      Top = 10
      Width = 96
      Height = 25
      Caption = 'Load Defaults'
      TabOrder = 0
      OnClick = btnLoadDefaultsClick
    end
  end
  object pnlTop: TPanel [2]
    Left = 0
    Top = 266
    Width = 660
    Height = 156
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    StyleElements = [seFont, seClient]
    object grpFontColors: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 654
      Height = 150
      Align = alClient
      Caption = 'Fonts && Colors'
      TabOrder = 0
      object ScrollBox1: TScrollBox
        Left = 2
        Top = 17
        Width = 650
        Height = 131
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 0
      end
    end
  end
  object pnlThemeRow: TPanel [3]
    Left = 0
    Top = 0
    Width = 660
    Height = 266
    Align = alTop
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 2
    object rgTheme: TRadioGroup
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 654
      Height = 78
      Align = alTop
      Caption = 'Theme'
      Items.Strings = (
        'Light'
        'Dark'
        'System')
      TabOrder = 0
      OnClick = rgThemeClick
    end
    object grpDateColumns: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 152
      Width = 654
      Height = 109
      Align = alTop
      Caption = 'Date Columns'
      TabOrder = 1
      object lblDateFormat: TLabel
        Left = 16
        Top = 24
        Width = 66
        Height = 15
        Caption = 'Date format:'
      end
      object cmbDateFormat: TComboBox
        Left = 105
        Top = 20
        Width = 200
        Height = 23
        Hint = 
          'Date format for the timestamp columns. Uses Delphi FormatDateTim' +
          'e syntax.'
        TabOrder = 0
        Items.Strings = (
          'yyyy-mm-dd hh:nn:ss'
          'dd.mm.yyyy hh:nn:ss'
          'mm/dd/yyyy hh:nn:ss'
          'yyyy-mm-dd'
          'dd.mm.yyyy'
          'dd/mm/yyyy hh:nn')
      end
      object cbShowModifiedDateColumn: TCheckBox
        Left = 16
        Top = 49
        Width = 200
        Height = 17
        Hint = 'Show the "Modified" timestamp column in the result tree'
        Caption = 'Show "Modified" column'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object cbShowCreationDateColumn: TCheckBox
        Left = 16
        Top = 65
        Width = 200
        Height = 17
        Hint = 'Show the "Created" timestamp column in the result tree'
        Caption = 'Show "Created" column'
        TabOrder = 2
      end
      object cbShowLastAccessDateColumn: TCheckBox
        Left = 16
        Top = 82
        Width = 200
        Height = 17
        Hint = 'Show the "Accessed" timestamp column in the result tree'
        Caption = 'Show "Accessed" column'
        TabOrder = 3
      end
    end
    object grpFileColors: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 87
      Width = 654
      Height = 59
      Align = alTop
      Caption = 'File Colors'
      TabOrder = 2
      object cbShowFileErrorColor: TCheckBox
        Left = 16
        Top = 19
        Width = 280
        Height = 17
        Hint = 'Use error color for files that do not exist'
        Caption = 'Error color for missing files'
        TabOrder = 0
      end
      object cbShowFileWarningColor: TCheckBox
        Left = 16
        Top = 35
        Width = 280
        Height = 17
        Hint = 
          'Use warning color for files outside the project path (Extension ' +
          'only)'
        Caption = 'Warning color for files outside project'
        TabOrder = 1
      end
    end
  end
end
