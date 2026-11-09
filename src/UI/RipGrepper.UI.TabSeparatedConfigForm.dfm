inherited TabSeparatedConfigForm: TTabSeparatedConfigForm
  Left = 756
  Top = 291
  Caption = 'Configure Data'
  ClientHeight = 303
  ClientWidth = 397
  Position = poDesigned
  ExplicitWidth = 413
  ExplicitHeight = 342
  TextHeight = 15
  inherited PanelBottom: TPanel
    Top = 258
    Width = 397
    inherited btnOk: TButton
      Action = ActionOk
    end
    inherited btnCancel: TButton
      Action = ActionCancel
    end
  end
  object pnlMain: TPanel [1]
    Left = 0
    Top = 0
    Width = 397
    Height = 258
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitHeight = 253
    object ToolBar1: TToolBar
      AlignWithMargins = True
      Left = 8
      Top = 3
      Width = 381
      Height = 22
      Margins.Left = 8
      Margins.Right = 8
      AutoSize = True
      Caption = 'ToolBar1'
      Images = SVGIconImageList1
      TabOrder = 0
      object tbPlus: TToolButton
        Left = 0
        Top = 0
        Action = ActionAdd
      end
      object tbMinus: TToolButton
        Left = 23
        Top = 0
        Action = ActionRemove
      end
      object tbUp: TToolButton
        Left = 46
        Top = 0
        Action = ActionMoveUp
      end
      object tbDown: TToolButton
        Left = 69
        Top = 0
        Action = ActionMoveDown
      end
      object tbTestRun: TToolButton
        Left = 92
        Top = 0
        Action = ActionTest
      end
    end
    object VstData: TVirtualStringTree
      AlignWithMargins = True
      Left = 3
      Top = 31
      Width = 391
      Height = 219
      Align = alClient
      Colors.BorderColor = 15987699
      Colors.DisabledColor = clGray
      Colors.DropMarkColor = 15385233
      Colors.DropTargetColor = 15385233
      Colors.DropTargetBorderColor = 15385233
      Colors.FocusedSelectionColor = 15385233
      Colors.FocusedSelectionBorderColor = 15385233
      Colors.GridLineColor = 15987699
      Colors.HeaderHotColor = clBlack
      Colors.HotColor = clBlack
      Colors.SelectionRectangleBlendColor = 15385233
      Colors.SelectionRectangleBorderColor = 15385233
      Colors.SelectionTextColor = clBlack
      Colors.TreeLineColor = 9471874
      Colors.UnfocusedColor = clGray
      Colors.UnfocusedSelectionColor = clWhite
      Colors.UnfocusedSelectionBorderColor = clWhite
      Header.AutoSizeIndex = 0
      Header.Height = 15
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      TabOrder = 1
      OnDblClick = VstDataDblClick
      OnFreeNode = VstDataFreeNode
      OnGetText = VstDataGetText
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      Columns = <>
    end
  end
  object ActionListConfig: TActionList
    Images = SVGIconImageList1
    Left = 232
    Top = 152
    object ActionAdd: TAction
      Hint = 'Add Entry'
      ImageIndex = 4
      ImageName = 'add'
      OnExecute = ActionAddExecute
    end
    object ActionRemove: TAction
      Hint = 'Remove Entry'
      ImageIndex = 5
      ImageName = 'remove'
      OnExecute = ActionRemoveExecute
      OnUpdate = ActionRemoveUpdate
    end
    object ActionTest: TAction
      Hint = 'Test'
      ImageIndex = 3
      ImageName = 'rocket'
      OnExecute = ActionTestExecute
      OnUpdate = ActionTestUpdate
    end
    object ActionMoveDown: TAction
      Hint = 'Move Entry Down'
      ImageIndex = 2
      ImageName = 'arrow-down'
      OnExecute = ActionMoveDownExecute
      OnUpdate = ActionMoveDownUpdate
    end
    object ActionMoveUp: TAction
      Hint = 'Move Entry Up'
      ImageIndex = 1
      ImageName = 'arrow-up'
      OnExecute = ActionMoveUpExecute
      OnUpdate = ActionMoveUpUpdate
    end
    object ActionOk: TAction
      Caption = 'Ok'
      OnExecute = ActionOkExecute
    end
    object ActionCancel: TAction
      Caption = 'Cancel'
      OnExecute = ActionCancelExecute
    end
  end
  object SVGIconImageList1: TSVGIconVirtualImageList
    Images = <
      item
        CollectionIndex = 52
        CollectionName = 'reply'
        Name = 'reply'
      end
      item
        CollectionIndex = 53
        CollectionName = 'arrow-up'
        Name = 'arrow-up'
      end
      item
        CollectionIndex = 54
        CollectionName = 'arrow-down'
        Name = 'arrow-down'
      end
      item
        CollectionIndex = 18
        CollectionName = 'rocket'
        Name = 'rocket'
      end
      item
        CollectionIndex = 55
        CollectionName = 'add'
        Name = 'add'
      end
      item
        CollectionIndex = 56
        CollectionName = 'remove'
        Name = 'remove'
      end
      item
        CollectionIndex = 48
        CollectionName = 'folder-opened'
        Name = 'folder-opened'
      end>
    ImageCollection = SVGIconDataModule.SVGIconImageCollection1
    Left = 128
    Top = 152
  end
end
