unit GX_MessageBox;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  GX_BaseForm, GX_MemoEscFix;

type
  TMemo = class(TMemoEscFix)
  end;

type
  TGxMsgBoxAdaptor = class(TObject)
  protected
    // General optional data passed into ShowGxMessageBox that can be
    // used to customize the error message, etc.
    FData: string;
    // Returns the message text shown in the message text
    function GetMessage: string; virtual; abstract;
    // Returns caption of the message box
    function GetCaption: string; virtual;
    // Returns the set of buttons that the message box
    // should feature.
    function GetButtons: TMsgDlgButtons; virtual;
    // Button to be used as the default
    function GetDefaultButton: TMsgDlgBtn; virtual;
    // Return a unique identifier for this dialog; this
    // is used for storage in the registry.
    // By default, this is the content of Self.ClassName
    function GetUniqueIdentifier: string;
    // Mark this message box as suppressed; by default,
    // state is stored in the registry.
    procedure DoPermanentlySuppress;
    // Returns whether this message box is permanently
    // suppressed.
    function IsPermanentlySuppressed: Boolean;
    // Returns whether the message box should be shown.
    function ShouldShow: Boolean; virtual;
    function AllowSuppress: Boolean; virtual;
  public
    class function ConfigurationKey: string;
  end;

  TGxQuestionBoxAdaptor = class(TGxMsgBoxAdaptor)
  protected
    function GetButtons: TMsgDlgButtons; override;
    function GetDefaultButton: TMsgDlgBtn; override;
  end;

  TfmGxMessageBox = class(TfmBaseForm)
    p_Buttons: TPanel;
    p_Top: TPanel;
    mmoMessage: TMemo;
    p_Chkbox: TPanel;
    chkNeverShowAgain: TCheckBox;
    p_ChkLeft: TPanel;
  public
    constructor Create(_Owner: TComponent); override;
  end;

  TGxMsgBoxAdaptorClass = class of TGxMsgBoxAdaptor;

function ShowGxMessageBox(AdaptorClass: TGxMsgBoxAdaptorClass; const Data: string = ''): TModalResult;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Consts,
  GX_ConfigurationInfo, u_dzVclUtils;

const
  MsgDlgResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0 {$IFDEF GX_VER200_up}, mrClose{$ENDIF});

  MsgDlgButtonCaptions: array[TMsgDlgBtn] of string = (
   SMsgDlgYes, SNoButton, SMsgDlgOK, SMsgDlgCancel, SMsgDlgAbort, SMsgDlgRetry,
   SMsgDlgIgnore, SMsgDlgAll, SMsgDlgNoToAll, SMsgDlgYesToAll, SMsgDlgHelp {$IFDEF GX_VER200_up}, SMsgDlgClose{$ENDIF});

function ShowGxMessageBox(AdaptorClass: TGxMsgBoxAdaptorClass; const Data: string): TModalResult;
var
  Adaptor: TGxMsgBoxAdaptor;
  frm: TfmGxMessageBox;

  procedure CreateButtons;
  const
    SingleButtonWidth = 75;
    SingleButtonHeight = 25;
    Spacing = 8;
  var
    BtnType, DefaultBtn, CancelBtn: TMsgDlgBtn;
    DialogButtons: TMsgDlgButtons;
    ButtonRowWidth, NextButonXPos: Integer;
    CurrentButton: TButton;
  begin
    // Calculate the width of all buttons together
    ButtonRowWidth := 0;
    DialogButtons := Adaptor.GetButtons;
    for BtnType := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if BtnType in DialogButtons then
        Inc(ButtonRowWidth, SingleButtonWidth + Spacing);
    Dec(ButtonRowWidth, Spacing);
    if ButtonRowWidth > frm.ClientWidth then
      frm.ClientWidth := ButtonRowWidth;

    DefaultBtn := Adaptor.GetDefaultButton;

    if mbCancel in DialogButtons then
      CancelBtn := mbCancel
    else if mbNo in DialogButtons then
      CancelBtn := mbNo
    else
      CancelBtn := mbOK;

    NextButonXPos := (frm.p_Buttons.ClientWidth - ButtonRowWidth) div 2;

    for BtnType := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do begin
      if BtnType in DialogButtons then begin
        CurrentButton := TButton.Create(frm);
        CurrentButton.Caption := MsgDlgButtonCaptions[BtnType];
        CurrentButton.ModalResult := MsgDlgResults[BtnType];
        CurrentButton.Parent := frm.p_Buttons;
        CurrentButton.TabOrder := 999;
        CurrentButton.SetBounds(NextButonXPos, Spacing, SingleButtonWidth, SingleButtonHeight);
        if BtnType = DefaultBtn then begin
          CurrentButton.Default := True;
          frm.ActiveControl := CurrentButton;
        end;
        if BtnType = CancelBtn then
          CurrentButton.Cancel := True;
        Inc(NextButonXPos, SingleButtonWidth + Spacing);
      end;
    end;
  end;

begin
  Adaptor := AdaptorClass.Create;
  Adaptor.FData := Data;
  try
    Result := MsgDlgResults[Adaptor.GetDefaultButton];
    if Adaptor.IsPermanentlySuppressed then
      Exit;
    if Adaptor.ShouldShow then begin
      frm := TfmGxMessageBox.Create(nil);
      try
        frm.Caption := Adaptor.GetCaption;
        frm.chkNeverShowAgain.Enabled := Adaptor.AllowSuppress;
        frm.mmoMessage.Lines.Text := Adaptor.GetMessage;
        CreateButtons;
        frm.InitDpiScaler;
        Result := frm.ShowModal;
        if Adaptor.AllowSuppress and frm.chkNeverShowAgain.Checked then
          Adaptor.DoPermanentlySuppress;
      finally
        FreeAndNil(frm);
      end;
    end;
  finally
    FreeAndNil(Adaptor);
  end;
end;

{ TGxMsgBoxAdaptor }

function TGxMsgBoxAdaptor.GetCaption: string;
begin
  Result := 'GExperts Message';
end;

function TGxMsgBoxAdaptor.GetUniqueIdentifier: string;
begin
  Result := Self.ClassName;
end;

procedure TGxMsgBoxAdaptor.DoPermanentlySuppress;
var
  Settings: IExpertSettings;
begin
  if not AllowSuppress then
    Exit;
  Settings := ConfigInfo.GetExpertSettings(ConfigurationKey);
  Settings.WriteBool(Self.ClassName, True); // Do not localize
end;

function TGxMsgBoxAdaptor.IsPermanentlySuppressed: Boolean;
var
  Settings: IExpertSettings;
begin
  Settings := ConfigInfo.GetExpertSettings(ConfigurationKey);
  Result := Settings.ReadBool(Self.ClassName, False); // Do not localize
end;

function TGxMsgBoxAdaptor.ShouldShow: Boolean;
begin
  Result := True;
end;

function TGxMsgBoxAdaptor.GetButtons: TMsgDlgButtons;
begin
  Result := [mbOK];
end;

function TGxMsgBoxAdaptor.GetDefaultButton: TMsgDlgBtn;
const
  DefaultButton = mbOK;
begin
  Result := DefaultButton;

{$IFOPT D+}
  if not (DefaultButton in GetButtons) then
    SendDebugError('Message box "' + Self.ClassName + '" has a default button that is not available');
{$ENDIF D+}
end;

function TGxMsgBoxAdaptor.AllowSuppress: Boolean;
begin
  Result := True;
end;

class function TGxMsgBoxAdaptor.ConfigurationKey: string;
begin
  Result := 'Misc' + PathDelim + 'SuppressedMessages';
end;

{ TGxQuestionBoxAdaptor }

function TGxQuestionBoxAdaptor.GetButtons: TMsgDlgButtons;
begin
  Result := [mbYes, mbNo];
end;

function TGxQuestionBoxAdaptor.GetDefaultButton: TMsgDlgBtn;
begin
  Result := mbYes;
end;

{ TfmGxMessageBox }

constructor TfmGxMessageBox.Create(_Owner: TComponent);
begin
  inherited;
  TPanel_BevelNone([p_Buttons, p_Top, p_Chkbox, p_ChkLeft]);
end;

end.
