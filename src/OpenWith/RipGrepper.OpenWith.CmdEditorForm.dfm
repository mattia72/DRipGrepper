inherited OpenWithCommandEditor: TOpenWithCommandEditor
  Caption = 'OpenWithCommandEditor'
  ClientHeight = 429
  ClientWidth = 468
  OnShow = FormShow
  TextHeight = 15
  inherited PanelBottom: TPanel
    Top = 384
    Width = 468
    TabOrder = 1
    inherited btnOk: TButton
      Left = 312
      Top = 14
      Action = ActionOk
    end
    inherited btnCancel: TButton
      Left = 393
      Top = 14
      Action = ActionCancel
    end
  end
  object Panel1: TPanel [1]
    Left = 0
    Top = 0
    Width = 468
    Height = 384
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    object GroupBox1: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 462
      Height = 230
      Align = alTop
      Caption = 'Settings'
      TabOrder = 0
      DesignSize = (
        462
        230)
      object Label1: TLabel
        AlignWithMargins = True
        Left = 10
        Top = 72
        Width = 322
        Height = 20
        Margins.Left = 50
        Margins.Top = 15
        Margins.Right = 8
        Margins.Bottom = 8
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Command:'
      end
      object Label2: TLabel
        AlignWithMargins = True
        Left = 10
        Top = 22
        Width = 322
        Height = 20
        Margins.Left = 50
        Margins.Top = 15
        Margins.Right = 8
        Margins.Bottom = 8
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Caption:'
      end
      object Label4: TLabel
        AlignWithMargins = True
        Left = 10
        Top = 173
        Width = 322
        Height = 20
        Margins.Left = 50
        Margins.Top = 15
        Margins.Right = 8
        Margins.Bottom = 8
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Description:'
      end
      object Label3: TLabel
        AlignWithMargins = True
        Left = 10
        Top = 120
        Width = 322
        Height = 20
        Margins.Left = 50
        Margins.Top = 15
        Margins.Right = 8
        Margins.Bottom = 8
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Parameters:'
      end
      object btnOpenFile: TButton
        Left = 428
        Top = 91
        Width = 25
        Height = 25
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Action = ActionOpenFileDialog
        Align = alCustom
        Anchors = [akTop, akRight]
        ImageAlignment = iaCenter
        Images = SVGIconImageList1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object edtCmdPath: TEdit
        Left = 10
        Top = 92
        Width = 415
        Height = 23
        Margins.Left = 50
        Margins.Top = 8
        Margins.Right = 125
        Margins.Bottom = 0
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object edtDescr: TEdit
        Left = 10
        Top = 193
        Width = 415
        Height = 23
        Margins.Left = 50
        Margins.Top = 8
        Margins.Right = 125
        Margins.Bottom = 0
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object edtLabel: TEdit
        Left = 10
        Top = 42
        Width = 415
        Height = 23
        Margins.Left = 50
        Margins.Top = 8
        Margins.Right = 125
        Margins.Bottom = 0
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
      end
      object edtParameters: TEdit
        Left = 10
        Top = 140
        Width = 415
        Height = 23
        Margins.Left = 50
        Margins.Top = 8
        Margins.Right = 125
        Margins.Bottom = 0
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
      end
    end
    object GroupBox2: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 239
      Width = 462
      Height = 141
      Margins.Bottom = 4
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Help'
      TabOrder = 1
      object RichEdit1: TRichEdit
        AlignWithMargins = True
        Left = 5
        Top = 20
        Width = 452
        Height = 116
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        Lines.Strings = (
          'File Path and Location Placeholders:'
          ''
          '<DIR>: Directory (search path)'
          '<FILE>: File path'
          '<ROW>: Line number'
          '<COL>: Column number'
          ''
          'Example Program Parameters:'
          ''
          'notepad.exe "<FILE>"'
          'code.exe --reuse-window "<DIR>" --goto "<FILE>:<LINE>:<COL>"'
          'notepad++.exe "<FILE>" -n<LINE> -c<COL>'
          'explorer.exe /select,"<FILE>"'
          'nvim-qt.exe -c "+normal <LINE>G<COL>l" -- "<FILE>"'
          '')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        WantTabs = True
        WordWrap = False
      end
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
    Left = 343
    Top = 288
  end
  object ActionList1: TActionList
    Images = SVGIconImageList1
    Left = 406
    Top = 285
    object ActionOk: TAction
      Caption = 'Ok'
      OnExecute = ActionOkExecute
    end
    object ActionCancel: TAction
      Caption = 'Cancel'
      OnExecute = ActionCancelExecute
    end
    object ActionOpenFileDialog: TAction
      Hint = 'Explore File...'
      ImageIndex = 6
      ImageName = 'folder-opened'
      OnExecute = ActionOpenFileDialogExecute
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 381
    Top = 324
  end
end
