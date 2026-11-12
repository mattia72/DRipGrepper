object BaseForm: TBaseForm
  Left = 0
  Top = 0
  ClientHeight = 300
  ClientWidth = 400
  Color = clBtnHighlight
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object PanelBottom: TPanel
    Left = 0
    Top = 255
    Width = 400
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    ParentBackground = False
    TabOrder = 0
    object btnOk: TButton
      AlignWithMargins = True
      Left = 234
      Top = 10
      Width = 75
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 6
      Margins.Bottom = 0
      Align = alRight
      Caption = 'OK'
      Default = True
      ImageIndex = 0
      ImageName = 'ok'
      Images = SVGImageListBottomPanel
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 315
      Top = 10
      Width = 75
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ImageIndex = 1
      ImageName = 'close'
      Images = SVGImageListBottomPanel
      ModalResult = 2
      TabOrder = 1
    end
  end
  object SVGImageListBottomPanel: TSVGIconVirtualImageList
    ImageCollection = SVGIconDataModule.SVGIconImageCollection1
    Images = <
      item
        CollectionName = 'ok'
        Name = 'ok'
      end
      item
        CollectionName = 'close'
        Name = 'close'
      end>
    Scaled = True
    Left = 200
    Top = 8
  end
end
