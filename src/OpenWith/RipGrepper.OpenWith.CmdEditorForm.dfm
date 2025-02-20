object OpenWithCommandEditor: TOpenWithCommandEditor
  Left = 0
  Top = 0
  Caption = 'OpenWithCommandEditor'
  ClientHeight = 361
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 472
    Height = 361
    Align = alClient
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      472
      361)
    object btn_Save: TButton
      AlignWithMargins = True
      Left = 297
      Top = 326
      Width = 75
      Height = 25
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Action = ActionOk
      Anchors = [akRight, akBottom]
      TabOrder = 0
    end
    object btn_Cancel: TButton
      AlignWithMargins = True
      Left = 388
      Top = 326
      Width = 75
      Height = 25
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Action = ActionCancel
      Anchors = [akRight, akBottom]
      TabOrder = 1
    end
    object GroupBox1: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 464
      Height = 195
      Align = alTop
      Caption = 'Settings'
      TabOrder = 2
      DesignSize = (
        464
        195)
      object Label1: TLabel
        AlignWithMargins = True
        Left = 10
        Top = 72
        Width = 326
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
        Width = 326
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
        Top = 122
        Width = 326
        Height = 20
        Margins.Left = 50
        Margins.Top = 15
        Margins.Right = 8
        Margins.Bottom = 8
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Description:'
      end
      object btnOpenFile: TButton
        Left = 432
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
      object edtCmdLine: TEdit
        Left = 10
        Top = 92
        Width = 419
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
        Top = 142
        Width = 419
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
        Width = 419
        Height = 23
        Margins.Left = 50
        Margins.Top = 8
        Margins.Right = 125
        Margins.Bottom = 0
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
      end
    end
    object GroupBox2: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 203
      Width = 464
      Height = 117
      Margins.Bottom = 40
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Help'
      TabOrder = 3
      object ListBox1: TListBox
        AlignWithMargins = True
        Left = 5
        Top = 20
        Width = 454
        Height = 92
        Align = alClient
        ItemHeight = 15
        Items.Strings = (
          'notepad.exe "<FILE>"'
          'code.exe --reuse-window "<DIR>" --goto "<FILE>:<LINE>:<COL>"'
          'notepad++.exe "<FILE>" -n<LINE> -c<COL>'
          'explorer.exe /select,"<FILE>"'
          'nvim-qt.exe "<FILE>" -- -c "+normal <LINE>G<COL>l"')
        TabOrder = 0
      end
    end
  end
  object SVGIconImageList1: TSVGIconImageList
    SVGIconItems = <
      item
        IconName = 'reply'
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<path fill-rule="evenodd" clip-' +
          'rule="evenodd" d="M6.30674 2.14648L2.28674 6.16649V6.87359L6.306' +
          '74 10.8936L7.01385 10.1865L3.80735 6.97998H5.6903C8.50346 6.9799' +
          '8 10.2952 7.58487 11.3956 8.70855C12.4977 9.83407 13.0103 11.585' +
          '7 13.0103 14.13V14.48H14.0103V14.13C14.0103 11.4843 13.4829 9.41' +
          '09 12.11 8.00891C10.7354 6.60509 8.61714 5.97998 5.6903 5.97998H' +
          '3.88746L7.01385 2.85359L6.30674 2.14648Z" fill="#424242"/>'#13#10'</sv' +
          'g>'#13#10
      end
      item
        IconName = 'arrow-up'
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<path fill-rule="evenodd" clip-' +
          'rule="evenodd" d="M13.8538 6.99999L8.85384 2H8.14673L3.14673 6.9' +
          '9999L3.85384 7.7071L8.00027 3.56066V14H9.00027V3.56066L13.1467 7' +
          '.7071L13.8538 6.99999Z" fill="#424242"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'arrow-down'
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<path fill-rule="evenodd" clip-' +
          'rule="evenodd" d="M3.14667 9.00001L8.14665 14L8.85376 14L13.8538' +
          ' 9.00001L13.1467 8.2929L9.00021 12.4393L9.00022 2.00001L8.00022 ' +
          '2.00001L8.00022 12.4393L3.85378 8.2929L3.14667 9.00001Z" fill="#' +
          '424242"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'rocket'
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<path fill-rule="evenodd" clip-' +
          'rule="evenodd" d="M14.4912 1C10.8928 1.0045 7.83757 2.98269 5.65' +
          '659 5H1.50024L1.00024 5.5V8.5L1.14724 8.854L2.1382 9.84496L2.139' +
          '24 9.854L6.13924 13.854L6.14825 13.855L7.14725 14.854L7.50025 15' +
          'H10.5002L11.0002 14.5V10.346C13.0193 8.16839 14.9962 5.11301 14.' +
          '9922 1.5L14.4912 1ZM2.00024 6H4.64331C3.59271 7.10401 2.83194 8.' +
          '11996 2.41868 8.71094L2.00024 8.293V6ZM7.70025 14L7.28073 13.576' +
          '6C7.87285 13.1655 8.89019 12.4085 9.99525 11.3611V14H7.70025ZM6.' +
          '55724 12.856L3.13624 9.437C4.12824 8 8.37924 2.355 13.9782 2.016' +
          'C13.6522 7.628 7.99124 11.869 6.55724 12.856ZM4.00024 15V14H2.00' +
          '024V12H1.00024V15H4.00024ZM10.7478 7.33284C10.9124 7.08628 11.00' +
          '02 6.79647 11.0002 6.50001C11.0002 6.3026 10.9613 6.10714 10.885' +
          '6 5.92483C10.8099 5.74251 10.699 5.57693 10.5591 5.43758C10.4193' +
          ' 5.29822 10.2534 5.18784 10.0708 5.11275C9.88824 5.03766 9.69265' +
          ' 4.99935 9.49524 5.00001C9.19879 5.001 8.90927 5.08981 8.66326 5' +
          '.25523C8.41724 5.42065 8.22576 5.65526 8.113 5.92944C8.00023 6.2' +
          '0361 7.97124 6.50506 8.02968 6.7957C8.08812 7.08634 8.23137 7.35' +
          '314 8.44135 7.56242C8.65133 7.7717 8.91861 7.91407 9.20944 7.971' +
          '54C9.50028 8.02902 9.80162 7.99902 10.0754 7.88534C10.3492 7.771' +
          '67 10.5832 7.57941 10.7478 7.33284Z" fill="#424242"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'add'
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<path d="M14.0004 7V8H8.00037V1' +
          '4H7.00037V8H1.00037V7H7.00037V1H8.00037V7H14.0004Z" fill="#42424' +
          '2"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'remove'
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<path d="M15.0002 8H1.00024V7H1' +
          '5.0002V8Z" fill="#424242"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'folder-opened'
        SVGText = 
          '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmln' +
          's="http://www.w3.org/2000/svg">'#13#10'<path d="M1.50024 14H12.5002L12' +
          '.9802 13.63L15.6102 6.63L15.1302 6H14.0002V3.5L13.5002 3H7.71021' +
          'L6.85022 2.15002L6.50024 2H1.50024L1.00024 2.5V13.5L1.50024 14ZM' +
          '2.00024 3H6.29028L7.15027 3.84998L7.50024 4H13.0002V6H8.50024L8.' +
          '15027 6.15002L7.29028 7H3.50024L3.03027 7.33997L2.03027 10.42L2.' +
          '00024 3ZM12.1302 13H2.19019L3.86023 8H7.50024L7.85022 7.84998L8.' +
          '71021 7H14.5002L12.1302 13Z" fill="#424242"/>'#13#10'</svg>'#13#10
      end>
    Scaled = True
    Left = 391
    Top = 128
  end
  object ActionList1: TActionList
    Images = SVGIconImageList1
    Left = 430
    Top = 125
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
    Left = 437
    Top = 44
  end
end
