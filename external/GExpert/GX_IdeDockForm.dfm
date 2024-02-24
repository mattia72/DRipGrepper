inherited DockableForm: TDockableForm
  Left = 182
  Top = 242
  Width = 201
  Height = 106
  HorzScrollBar.Increment = 17
  VertScrollBar.Increment = 7
  BorderStyle = bsSizeToolWin
  Caption = 'DockableForm'
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  OnGetSiteInfo = FormGetSiteInfo
  PixelsPerInch = 96
  TextHeight = 13
  object DockActionList: TActionList
    Left = 8
    Top = 48
    object DockableCmd: TAction
      Caption = '&Dockable'
      Checked = True
      OnExecute = DockableCmdExecute
      OnUpdate = DockableCmdUpdate
    end
    object StayOnTopCmd: TAction
      Caption = '&Stay on Top'
      OnExecute = StayOnTopCmdExecute
      OnUpdate = StayOnTopCmdUpdate
    end
    object ZoomWindowCmd: TAction
      Caption = 'Zoom Window'
      Visible = False
      OnExecute = ZoomWindowCmdExecute
    end
  end
end
