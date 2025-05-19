object AboutForm: TAboutForm
  Left = 0
  Top = 0
  Caption = 'Fonts and Colors'
  ClientHeight = 230
  ClientWidth = 430
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
    Top = 189
    Width = 430
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    StyleElements = [seFont, seClient]
    object btnCheckUpdate: TButton
      Left = 0
      Top = 6
      Width = 126
      Height = 25
      Action = ActionCheckUpdate
      TabOrder = 0
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 430
    Height = 189
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    StyleElements = [seFont, seClient]
    object grpFontColors: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 424
      Height = 183
      Align = alClient
      Caption = 'Release'
      TabOrder = 0
      object ScrollBox1: TScrollBox
        Left = 2
        Top = 17
        Width = 420
        Height = 164
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 0
        object Memo1: TMemo
          Left = 3
          Top = 15
          Width = 418
          Height = 146
          Lines.Strings = (
            'Memo1')
          TabOrder = 0
        end
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
    BaseURL = 'https://github.com/mattia72/DRipGrepper/releases/latest'
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
