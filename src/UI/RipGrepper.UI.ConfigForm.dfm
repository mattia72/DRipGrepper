object ConfigForm: TConfigForm
  Left = 0
  Top = 0
  Width = 546
  Height = 478
  VertScrollBar.Range = 330
  VertScrollBar.Visible = False
  Caption = 'DRipGrepper Config'
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
    Top = 391
    Width = 530
    Height = 48
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      530
      48)
    object btn_Save: TButton
      AlignWithMargins = True
      Left = 344
      Top = 14
      Width = 75
      Height = 25
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Action = ActionOk
      Anchors = [akRight, akBottom]
      TabOrder = 0
    end
    object btn_Cancel: TButton
      AlignWithMargins = True
      Left = 435
      Top = 14
      Width = 75
      Height = 25
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Action = ActionCancel
      Anchors = [akRight, akBottom]
      TabOrder = 1
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 530
    Height = 391
    Align = alClient
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 528
      Height = 389
      Align = alClient
      MultiLine = True
      TabOrder = 0
    end
  end
  object ActionList1: TActionList
    Left = 232
    Top = 128
    object ActionOk: TAction
      Caption = 'Ok'
      OnExecute = ActionOkExecute
    end
    object ActionCancel: TAction
      Caption = 'Cancel'
      OnExecute = ActionCancelExecute
    end
  end
end
