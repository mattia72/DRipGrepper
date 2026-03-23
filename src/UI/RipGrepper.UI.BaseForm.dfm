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
    Color = clBtnFace
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      400
      45)
    object btnOk: TButton
      Left = 234
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ImageIndex = 0
      ImageName = 'ok'
      Images = SVGImageListBottomPanel
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 315
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ImageIndex = 1
      ImageName = 'close'
      Images = SVGImageListBottomPanel
      ModalResult = 2
      TabOrder = 1
    end
  end
  object SVGImageListBottomPanel: TSVGIconImageList
    SVGIconItems = <
      item
        IconName = 'ok'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M21,7L9,19L3.5,13.5L4.91,12.09L9,16.17L19.59,5.59L21,7Z" ' +
          '/></svg>'
      end
      item
        IconName = 'close'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><pat' +
          'h d="M19,6.41L17.59,5L12,10.59L6.41,5L5,6.41L10.59,12L5,17.59L' +
          '6.41,19L12,13.41L17.59,19L19,17.59L13.41,12L19,6.41Z" /></svg>'
      end>
    Scaled = True
    Left = 200
    Top = 8
  end
end
