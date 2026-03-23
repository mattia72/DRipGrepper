inherited ColorSettingsForm: TColorSettingsForm
  Left = 0
  Top = 0
  Caption = 'Fonts and Colors'
  ClientHeight = 272
  ClientWidth = 660
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 15
  object pnlBottom: TPanel
    Left = 0
    Top = 231
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
  object pnlTop: TPanel
    Left = 0
    Top = 121
    Width = 660
    Height = 110
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
      Height = 104
      Align = alClient
      Caption = 'Fonts && Colors'
      TabOrder = 0
      object ScrollBox1: TScrollBox
        Left = 2
        Top = 17
        Width = 650
        Height = 85
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 0
      end
    end
  end
  object pnlThemeRow: TPanel
    Left = 0
    Top = 0
    Width = 660
    Height = 121
    Align = alTop
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 2
    object rgTheme: TRadioGroup
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 298
      Height = 115
      Align = alLeft
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
      Left = 307
      Top = 3
      Width = 350
      Height = 115
      Align = alClient
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
        Top = 50
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
        Top = 71
        Width = 200
        Height = 17
        Hint = 'Show the "Created" timestamp column in the result tree'
        Caption = 'Show "Created" column'
        TabOrder = 2
      end
      object cbShowLastAccessDateColumn: TCheckBox
        Left = 16
        Top = 92
        Width = 200
        Height = 17
        Hint = 'Show the "Accessed" timestamp column in the result tree'
        Caption = 'Show "Accessed" column'
        TabOrder = 3
      end
    end
  end
end
