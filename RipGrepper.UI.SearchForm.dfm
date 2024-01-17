object SearchDialogForm: TSearchDialogForm
  Left = 0
  Top = 0
  Caption = 'RipGrepper Search...'
  ClientHeight = 174
  ClientWidth = 690
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object pnlSearch: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 684
    Height = 168
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = -180
    ExplicitTop = 0
    ExplicitWidth = 779
    ExplicitHeight = 159
    DesignSize = (
      684
      168)
    object gbSearch: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 678
      Height = 162
      Align = alClient
      Caption = 'Search'
      TabOrder = 2
      ExplicitWidth = 773
      ExplicitHeight = 153
      DesignSize = (
        678
        162)
      object lblParams: TLabel
        AlignWithMargins = True
        Left = 16
        Top = 85
        Width = 62
        Height = 15
        Caption = 'Parameters:'
      end
      object lblPaths: TLabel
        AlignWithMargins = True
        Left = 16
        Top = 30
        Width = 40
        Height = 15
        Caption = 'Path(s):'
      end
      object lblText: TLabel
        AlignWithMargins = True
        Left = 16
        Top = 56
        Width = 24
        Height = 15
        Caption = 'Text:'
      end
      object cmbParameters: TComboBox
        Left = 84
        Top = 82
        Width = 543
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'cmbSearchDir'
        ExplicitWidth = 638
      end
      object cmbSearchDir: TComboBox
        Left = 84
        Top = 24
        Width = 543
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'cmbSearchDir'
      end
      object cmbSearchText: TComboBox
        Left = 84
        Top = 53
        Width = 543
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = 'cmbSearchDir'
        ExplicitWidth = 638
      end
    end
    object btnConfig: TButton
      AlignWithMargins = True
      Left = 636
      Top = 85
      Width = 26
      Height = 24
      Anchors = [akTop, akRight]
      ImageAlignment = iaCenter
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      ExplicitLeft = 731
    end
    object btnSearch: TButton
      AlignWithMargins = True
      Left = 513
      Top = 122
      Width = 75
      Height = 21
      Anchors = [akTop, akRight]
      Default = True
      TabOrder = 1
      ExplicitLeft = 608
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 594
      Top = 122
      Width = 75
      Height = 21
      Anchors = [akTop, akRight]
      Cancel = True
      TabOrder = 3
      ExplicitLeft = 689
    end
  end
  object ActionList1: TActionList
    Images = ImageList1
    Left = 328
    Top = 120
  end
  object ImageList1: TImageList
    Left = 424
    Top = 120
  end
end
