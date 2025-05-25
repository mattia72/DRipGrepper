object AboutForm: TAboutForm
  Left = 0
  Top = 0
  Caption = 'Fonts and Colors'
  ClientHeight = 306
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 15
  object lblLatestRelease1: TLabel
    Left = 141
    Top = 54
    Width = 73
    Height = 15
    Caption = 'Latest release:'
    Visible = False
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 0
    Width = 554
    Height = 16
    Align = alTop
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    StyleElements = [seFont, seClient]
  end
  object pnlTop: TPanel
    Left = 0
    Top = 16
    Width = 554
    Height = 290
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    StyleElements = [seFont, seClient]
    object ScrollBox1: TScrollBox
      Left = 0
      Top = 0
      Width = 554
      Height = 290
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 0
      DesignSize = (
        554
        290)
      object lblLatestRelease: TLabel
        Left = 133
        Top = 54
        Width = 73
        Height = 15
        Caption = 'Latest release:'
        Visible = False
      end
      object Memo1: TMemo
        Left = 3
        Top = 80
        Width = 551
        Height = 207
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object lnkLatestUrl: TLinkLabel
        Left = 212
        Top = 52
        Width = 46
        Height = 19
        Caption = 'v4.6.2-b'
        TabOrder = 1
        Visible = False
        OnLinkClick = lnkLatestUrlLinkClick
      end
      object pnlTitle: TPanel
        Left = 0
        Top = 0
        Width = 554
        Height = 33
        Align = alTop
        Caption = 'DripGrepper'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI Semibold'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
      end
    end
    object btnCheckUpdate: TButton
      Left = 0
      Top = 49
      Width = 126
      Height = 25
      Action = ActionCheckUpdate
      TabOrder = 1
    end
  end
  object ActionList1: TActionList
    Left = 392
    Top = 120
    object ActionCheckUpdate: TAction
      Caption = 'Check for Update...'
      OnExecute = ActionCheckUpdateExecute
    end
  end
end
