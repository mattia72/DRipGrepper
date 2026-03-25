inherited ConfigForm: TConfigForm
  VertScrollBar.Visible = False
  Caption = 'DRipGrepper Config'
  ClientHeight = 258
  ClientWidth = 410
  Position = poOwnerFormCenter
  ShowHint = True
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  inherited PanelBottom: TPanel
    inherited btnOk: TButton
      Action = ActionOk
    end
    inherited btnCancel: TButton
      Action = ActionCancel
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 410
    Height = 210
    Align = alClient
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 408
      Height = 208
      Align = alClient
      MultiLine = True
      TabOrder = 0
      OnChange = PageControl1Change
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
