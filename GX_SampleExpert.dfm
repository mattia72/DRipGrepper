object fmGxSampleExpertForm: TfmGxSampleExpertForm
  Left = 381
  Top = 212
  BorderStyle = bsDialog
  Caption = 'GExperts Sample Expert'
  ClientHeight = 359
  ClientWidth = 741
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -30
  Font.Name = 'Tahoma'
  Font.Style = []
  PixelsPerInch = 240
  TextHeight = 36
  object lblNote: TLabel
    Left = 20
    Top = 30
    Width = 683
    Height = 38
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Alignment = taCenter
    AutoSize = False
    Caption = 'Well, this is just a sample. Not a lot here...'
  end
  object lblData: TLabel
    Left = 45
    Top = 120
    Width = 373
    Height = 36
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Alignment = taRightJustify
    Caption = 'Data is saved to the registry'
  end
  object btnOK: TButton
    Left = 160
    Top = 210
    Width = 188
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object edtData: TEdit
    Left = 450
    Top = 110
    Width = 233
    Height = 44
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 380
    Top = 210
    Width = 188
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
