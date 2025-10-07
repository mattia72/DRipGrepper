unit RipGrepper.OpenWith.CmdListForm;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	Vcl.StdCtrls,
	Vcl.ExtCtrls,
	System.Actions,
	Vcl.ActnList,
	Vcl.ComCtrls,
	System.ImageList,
	Vcl.ImgList,
	RipGrepper.Settings.AppSettings,
	RipGrepper.UI.DpiScaler,
	RipGrepper.Settings.OpenWithSettings,
	RipGrepper.Helper.UI.DarkMode,
	SVGIconImageListBase,
	SVGIconImageList,
	RipGrepper.Tools.FileUtils;

type
	TOpenWithCmdList = class(TForm)
		lbCommands : TListView;
		pnl_Bottom : TPanel;
		btn_Save : TButton;
		btn_Cancel : TButton;
		alActions : TActionList;
		ActionOk : TAction;
		ActionCancel : TAction;
		ImageListIcons : TImageList;
		btnView : TButton;
		ActionSwitchView : TAction;
		btnConfig : TButton;
		ActionShowConfig : TAction;
		pnlMain : TPanel;
		lblHint2 : TLabel;
		Memo1 : TMemo;
		lblHint1 : TLabel;
		pnl_Right : TPanel;
		pnl_Top : TPanel;
		SVGIconImageList1 : TSVGIconImageList;
		procedure ActionCancelExecute(Sender : TObject);
		procedure ActionShowConfigExecute(Sender : TObject);
		procedure ActionOkExecute(Sender : TObject);
		procedure ActionSwitchViewExecute(Sender : TObject);
		procedure ActionSwitchViewUpdate(Sender : TObject);
		procedure FormResize(Sender : TObject);
		procedure FormShow(Sender : TObject);
		procedure lbCommandsDblClick(Sender : TObject);
		procedure lbCommandsKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);

		private
			FColorTheme : string;
			FScaledIcons : TCustomImageList;
			FDpiScaler : TRipGrepperDpiScaler;
			FMemoLineMargin : Integer;
			FOrigMemoHeight : Integer;
			FOrigTopPanelHeight : Integer;
			FSettings : TOpenWithSettings;
			FThemeHandler : TThemeHandler;
			FViewStyleIndex : Integer;

			procedure CreateScaledIcons(const bUpdateScaler : Boolean = False);
			class function GetEnabledCmds(const _settings : TOpenWithSettings) : TArray<TCommandItem>;
			function GetFileNameFromCfg(const _ci : TCommandItem): string;
			function GetThemeHandler : TThemeHandler;
			function GetViewStyleIndex : Integer;
			procedure SaveOrigHeights;
			procedure SetMemoHeightByLineCount;
			property ThemeHandler : TThemeHandler read GetThemeHandler;
			property ViewStyleIndex : Integer read GetViewStyleIndex;

		public
			constructor Create(_owner : TComponent; const _settings : TOpenWithSettings; const _colorTheme : string); reintroduce;
			destructor Destroy(); override;
			class function CreateAndShow(const _settings : TOpenWithSettings; const _colorTheme : string) : string;
			procedure InitCtrlsTexts;
			procedure LoadEnbledCmds;
	end;

const
	LISTVIEW_TYPES : TArray<TViewStyle> = [vsList, vsIcon, vsReport, vsSmallIcon];
	LISTVIEW_TYPE_TEXTS : TArray<string> = ['List', 'Icon', 'Report', 'SmallIcon'];

implementation

uses
	Winapi.ShellAPI,
	System.IOUtils,
	RipGrepper.OpenWith.ConfigForm,
	System.Math,
	RipGrepper.OpenWith.Constants,
	RipGrepper.Tools.DebugUtils;

{$R *.dfm}

constructor TOpenWithCmdList.Create(_owner : TComponent; const _settings : TOpenWithSettings; const _colorTheme : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithCmdList.Create');
	inherited Create(_owner);
	FDpiScaler := TRipGrepperDpiScaler.Create(self);
	lbCommands.items.Clear;

	ImageListIcons.ColorDepth := TColorDepth.cd32Bit;
	FViewStyleIndex := 0;
	FSettings := _settings;
	FColorTheme := _colorTheme;
	// FSettings.ReLoad; TODO: AlreadyRead should be set
	FSettings.ReadFile; // we should read ini every time, it can be overwritten by another instance...
	dbgMsg.MsgFmt('FSettings: %s', [FSettings.ToString]);

	SaveOrigHeights;
	InitCtrlsTexts;

    ThemeHandler.Init(_colorTheme);
end;

destructor TOpenWithCmdList.Destroy();
begin
	FDpiScaler.Free;
	inherited Destroy();
end;

procedure TOpenWithCmdList.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
end;

procedure TOpenWithCmdList.ActionShowConfigExecute(Sender : TObject);
begin
	TOpenWithConfigForm.CreateAndShow(self, FSettings, FColorTheme);
	LoadEnbledCmds();
	CreateScaledIcons(True);
end;

procedure TOpenWithCmdList.ActionOkExecute(Sender : TObject);
begin
	FSettings.StoreToPersister;
	ModalResult := mrOk;
end;

procedure TOpenWithCmdList.ActionSwitchViewExecute(Sender : TObject);
begin
	lbCommands.ViewStyle := LISTVIEW_TYPES[ViewStyleIndex];
end;

procedure TOpenWithCmdList.ActionSwitchViewUpdate(Sender : TObject);
begin
	var
	idx := IfThen((FViewStyleIndex + 1) <= (Length(LISTVIEW_TYPES) - 1), FViewStyleIndex + 1, 0);
	ActionSwitchView.ImageIndex := idx + 2;
	ActionSwitchView.Hint := 'Change View ' + LISTVIEW_TYPE_TEXTS[idx];
end;

class function TOpenWithCmdList.CreateAndShow(const _settings : TOpenWithSettings; const _colorTheme : string) : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithCmdList.CreateAndShow');

	var
	form := TOpenWithCmdList.Create(nil, _settings, _colorTheme);
	try
		// form.InitCtrlsTexts();
		form.LoadEnbledCmds();
		form.CreateScaledIcons();

		if (mrOk = form.ShowModal()) then begin
			if form.lbCommands.ItemIndex > -1 then begin
				Result := form.lbCommands.Items[form.lbCommands.ItemIndex].SubItems[0];
			end;
		end;

	finally
		form.Free;
	end;

end;

procedure TOpenWithCmdList.CreateScaledIcons(const bUpdateScaler : Boolean = False);
begin
	if bUpdateScaler or not Assigned(FScaledIcons) or not Assigned(FDpiScaler) then begin
		FDpiScaler.Free;
		FDpiScaler := TRipGrepperDpiScaler.Create(Self, false);
		FDpiScaler.ScaleImageListToActDpi(ImageListIcons);
	end;

	lbCommands.SmallImages := ImageListIcons;
	lbCommands.LargeImages := ImageListIcons;
end;

procedure TOpenWithCmdList.FormResize(Sender : TObject);
begin
	SetMemoHeightByLineCount;
end;

procedure TOpenWithCmdList.FormShow(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithCmdList.FormShow');
	if lbCommands.GetCount > 0 then begin
		lbCommands.ItemIndex := 0;
	end;
	ActiveControl := lbCommands;
end;

class function TOpenWithCmdList.GetEnabledCmds(const _settings : TOpenWithSettings) : TArray<TCommandItem>;
var
	arrCmd : TArray<string>;
	ci : TCommandItem;
	i : Integer;
	sCmds : string;
begin
	Result := [];

	i := 0;
	repeat
		sCmds := _settings.Command[i];
		TDebugUtils.DebugMessage((Format('OpenWithFunc.GetSelectedCmd %d: "%s" ', [i, sCmds])));

		if sCmds = '' then begin
			break
		end;

		arrCmd := [''];
		arrCmd := sCmds.Split([SEPARATOR]);
		ci := TCommandItem.New(arrCmd);

		if ci.IsActive then begin
			Result := Result + [ci];
		end;

		inc(i);
	until (i = MAX_COMMAND_NUM);
end;

function TOpenWithCmdList.GetFileNameFromCfg(const _ci : TCommandItem) : string;
var
	sFileName : string;
	sPath : string;
begin
	sFileName := _ci.CommandLine.ExePath;
	sPath := ExtractFileDir(sFileName);
	if sPath.IsEmpty then begin
		TFileUtils.FindExecutable(sFileName, sPath);
		sFileName := sPath;
	end;
	Result := sFileName;
end;

function TOpenWithCmdList.GetThemeHandler : TThemeHandler;
begin
	if not Assigned(FThemeHandler) then begin
		FThemeHandler := TThemeHandler.Create(self);
	end;
	Result := FThemeHandler;
end;

function TOpenWithCmdList.GetViewStyleIndex : Integer;
begin
	FViewStyleIndex := IfThen(FViewStyleIndex < Length(LISTVIEW_TYPES) - 1, FViewStyleIndex + 1);
	// skip report
	FViewStyleIndex := IfThen(FViewStyleIndex = 2, FViewStyleIndex + 1, FViewStyleIndex);
	Result := (FViewStyleIndex mod Length(LISTVIEW_TYPES));
end;

procedure TOpenWithCmdList.InitCtrlsTexts;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithCmdList.InitCtrlsTexts');
	lblHint1.Caption := 'Open';
	dbgMsg.MsgFmt('FSettings.TestFile: %s', [FSettings.TestFile.ToString]);
	Memo1.Text := Format('%s', [FSettings.TestFile.ToString]);
	Memo1.Hint := Memo1.Text;
	lblHint2.Caption := 'with...';
end;

procedure TOpenWithCmdList.lbCommandsDblClick(Sender : TObject);
begin
	ActionOkExecute(Sender);
end;

procedure TOpenWithCmdList.lbCommandsKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
	if Key = VK_RETURN then begin
		ActionOkExecute(Sender);
	end;
end;

procedure TOpenWithCmdList.LoadEnbledCmds();
var
	sfi : TSHFileInfo;
	icon : TIcon;
	item : TListItem;
	sFileName : string;
begin

	icon := TIcon.Create;
	try
		lbCommands.Items.Clear();
		for var ci in GetEnabledCmds(FSettings) do begin
			sFileName := GetFileNameFromCfg(ci);
			if sFileName.IsEmpty then
				continue;

			item := lbCommands.Items.Add();

			item.Caption := ci.Caption;

			SHGetFileInfo(PChar(sFileName), 0, sfi, SizeOf(TSHFileInfo), SHGFI_LARGEICON or SHGFI_ICON);
			icon.Handle := sfi.hIcon;

			item.ImageIndex := ImageListIcons.AddIcon(icon);
			item.Subitems.Add(ci.CommandLine.AsString);
			item.Subitems.Add(ci.Description);

			TDebugUtils.DebugMessage((Format('TOpenWithCmdList.LoadEnbledCmds cmd: %d %s ', [item.ImageIndex, sFileName])));
			DestroyIcon(sfi.hIcon);
		end;

	finally
		icon.Free;
	end;
end;

procedure TOpenWithCmdList.SaveOrigHeights;
begin
	// if FOrigMemoHeight = 0 then begin
	FOrigMemoHeight := Memo1.Height;
	FMemoLineMargin := FOrigMemoHeight - Abs(Memo1.Font.Height);
	FOrigTopPanelHeight := pnl_Top.Height;
	// end;
end;

procedure TOpenWithCmdList.SetMemoHeightByLineCount;
var
	lineHeight : Integer;
	lineCount : Integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithCmdList.SetMemoHeightByLineCount');

	// SaveOrigHeights;

	lineCount := Memo1.Lines.Count;
	dbgMsg.MsgFmt('lineCount: %d', [lineCount]);
	if lineCount > 1 then begin
		lineHeight := Abs(Memo1.Font.Height) * (lineCount - 1);
		Memo1.Height := FOrigMemoHeight + lineHeight + FMemoLineMargin;
		pnl_Top.Height := FOrigTopPanelHeight + lineHeight + FMemoLineMargin;
	end else begin
		Memo1.Height := FOrigMemoHeight;
		pnl_Top.Height := FOrigTopPanelHeight;
	end;
	dbgMsg.MsgFmt('Memo1.Height: %d, pnl_Top.Height: %d', [Memo1.Height, pnl_Top.Height]);
end;

end.
