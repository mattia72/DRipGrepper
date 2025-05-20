object AboutForm: TAboutForm
  Left = 0
  Top = 0
  Caption = 'Fonts and Colors'
  ClientHeight = 286
  ClientWidth = 516
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
    Top = 0
    Width = 516
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
    Width = 516
    Height = 270
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    StyleElements = [seFont, seClient]
    object grpFontColors: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 510
      Height = 264
      Align = alClient
      Caption = 'Release'
      TabOrder = 0
      object ScrollBox1: TScrollBox
        Left = 2
        Top = 17
        Width = 506
        Height = 245
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 0
        DesignSize = (
          506
          245)
        object lblLatestRelease: TLabel
          Left = 138
          Top = 11
          Width = 73
          Height = 15
          Caption = 'Latest release:'
          Visible = False
        end
        object Memo1: TMemo
          Left = 3
          Top = 36
          Width = 500
          Height = 206
          Anchors = [akLeft, akTop, akRight, akBottom]
          Lines.Strings = (
            'Memo1')
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object lnkLatestUrl: TLinkLabel
          Left = 217
          Top = 11
          Width = 46
          Height = 19
          Caption = 'v4.6.2-b'
          TabOrder = 1
          Visible = False
          OnLinkClick = lnkLatestUrlLinkClick
        end
      end
      object btnCheckUpdate: TButton
        Left = 3
        Top = 21
        Width = 126
        Height = 25
        Action = ActionCheckUpdate
        TabOrder = 1
      end
    end
  end
  object ActionList1: TActionList
    Left = 350
    Top = 100
    object ActionCheckUpdate: TAction
      Caption = 'Check for Update...'
      OnExecute = ActionCheckUpdateExecute
    end
  end
  object RESTClient1: TRESTClient
    Accept = 'application/json, text/plain; q=0.9, text/html;q=0.8,'
    AcceptCharset = 'utf-8, *;q=0.8'
    BaseURL = 'https://api.github.com/repos/mattia72/DripGrepper/releases'
    Params = <>
    SynchronizedEvents = False
    Left = 130
    Top = 105
  end
  object RESTRequest1: TRESTRequest
    Client = RESTClient1
    Params = <
      item
        Name = 'GetLastRelease'
        ContentTypeStr = 'application/json'
      end>
    Response = RESTResponse1
    SynchronizedEvents = False
    Left = 250
    Top = 100
  end
  object RESTResponse1: TRESTResponse
    ContentType = 'application/json'
    Left = 50
    Top = 100
  end
end
