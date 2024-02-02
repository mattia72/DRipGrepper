object fmGxMessageBox: TfmGxMessageBox
  Left = 501
  Top = 221
  BorderStyle = bsDialog
  Caption = 'GExperts Message'
  ClientHeight = 244
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object p_Buttons: TPanel
    Left = 0
    Top = 203
    Width = 409
    Height = 41
    Align = alBottom
    TabOrder = 0
  end
  object p_Top: TPanel
    Left = 0
    Top = 0
    Width = 409
    Height = 203
    Align = alClient
    TabOrder = 1
    object mmoMessage: TMemo
      Left = 1
      Top = 1
      Width = 407
      Height = 167
      Align = alClient
      Lines.Strings = (
        '')
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object p_Chkbox: TPanel
      Left = 1
      Top = 168
      Width = 407
      Height = 34
      Align = alBottom
      TabOrder = 1
      object chkNeverShowAgain: TCheckBox
        Left = 9
        Top = 1
        Width = 397
        Height = 32
        Anchors = [akLeft, akRight, akBottom]
        Caption = '&Never show this message again'
        TabOrder = 0
      end
      object p_ChkLeft: TPanel
        Left = 1
        Top = 1
        Width = 8
        Height = 32
        Align = alLeft
        TabOrder = 1
      end
    end
  end
end
