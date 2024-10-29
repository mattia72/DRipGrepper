object fmAbout: TfmAbout
  Left = 285
  Top = 198
  ActiveControl = btnClose
  BorderStyle = bsDialog
  Caption = 'About GExperts'
  ClientHeight = 289
  ClientWidth = 617
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object lblGExperts: TLabel
    Left = 346
    Top = 14
    Width = 140
    Height = 22
    Alignment = taCenter
    AutoSize = False
    Caption = 'GExperts'
  end
  object lblVersion: TLabel
    Left = 224
    Top = 32
    Width = 385
    Height = 22
    Alignment = taCenter
    AutoSize = False
    Caption = 'Version ?.??'
  end
  object lblWebPage: TLabel
    Left = 368
    Top = 72
    Width = 184
    Height = 14
    Alignment = taCenter
    Caption = 'https://gexperts.dummzeuch.de/'
  end
  object lblProjectLeader: TLabel
    Left = 275
    Top = 91
    Width = 84
    Height = 14
    Alignment = taRightJustify
    Caption = 'Project Leader:'
  end
  object lblContributors: TLabel
    Left = 255
    Top = 112
    Width = 104
    Height = 14
    Alignment = taRightJustify
    Caption = 'Major Contributors:'
  end
  object lblProjectLeaderName: TLabel
    Left = 368
    Top = 91
    Width = 85
    Height = 14
    Caption = 'Thomas Mueller'
  end
  object lblWebSite: TLabel
    Left = 304
    Top = 72
    Width = 55
    Height = 14
    Alignment = taRightJustify
    Caption = 'Web Site:'
  end
  object lblPreRelease1: TLabel
    Left = 241
    Top = 19
    Width = 101
    Height = 20
    Alignment = taCenter
    AutoSize = False
    Caption = 'Pre-Release'
    Visible = False
  end
  object lblPreRelease2: TLabel
    Left = 489
    Top = 19
    Width = 101
    Height = 20
    Alignment = taCenter
    AutoSize = False
    Caption = 'Pre-Release'
    Visible = False
  end
  object lblIdeVersion: TLabel
    Left = 224
    Top = 56
    Width = 385
    Height = 14
    Alignment = taCenter
    AutoSize = False
    Caption = 'IDE version goes here'
  end
  object mmoBuildDetails: TMemo
    Left = 224
    Top = 240
    Width = 273
    Height = 49
    Alignment = taCenter
    BorderStyle = bsNone
    Color = clBtnFace
    Ctl3D = False
    Lines.Strings = (
      'Experimental build from http://www.domain.com.  '
      'Please report all bugs to email@domain.com.')
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 3
    Visible = False
  end
  object btnClose: TButton
    Left = 504
    Top = 256
    Width = 105
    Height = 26
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object pnlLogo: TPanel
    Left = 8
    Top = 8
    Width = 217
    Height = 273
    BevelOuter = bvLowered
    TabOrder = 2
    object imgLogo: TImage
      Left = 9
      Top = 9
      Width = 201
      Height = 252
      Center = True
    end
  end
  object btnEmail: TButton
    Left = 248
    Top = 256
    Width = 213
    Height = 26
    Caption = '&Send a Bug Report/Suggestion'
    TabOrder = 0
    OnClick = btnEmailClick
  end
  object mmoContributors: TMemo
    Left = 368
    Top = 112
    Width = 241
    Height = 129
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    Ctl3D = False
    Lines.Strings = (
      'ArentJan Banck'
      'Erik Berry'
      'Jim Campbell'
      'Primoz Gabrijelcic'
      'Ulrich Gerhardt'
      'Benjamin Fournier'
      'John Hansen'
      'Taz Higgins'
      'Stefan Hoffmeister'
      'Rick Hollerich'
      'Achim Kalwa'
      'Per-Eric Larsson'
      'Piotr Likus'
      'Ray Lischner'
      'Gerald Nunn'
      'Alex Petrov'
      'Puthoon'
      'Mahdi Safsafi'
      'Egbert van Nes'
      'Martin Waldenburg')
    ParentCtl3D = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object tim_Scroll: TTimer
    OnTimer = tim_ScrollTimer
    Left = 296
    Top = 144
  end
end
