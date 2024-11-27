object ConfigForm: TConfigForm
  Left = 0
  Top = 0
  Caption = 'DRipGrepper Config'
  ClientHeight = 330
  ClientWidth = 525
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 525
    Height = 280
    Align = alClient
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 280
    Width = 525
    Height = 50
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    DesignSize = (
      525
      50)
    object btn_Save: TButton
      AlignWithMargins = True
      Left = 339
      Top = 16
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
      Left = 430
      Top = 16
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
