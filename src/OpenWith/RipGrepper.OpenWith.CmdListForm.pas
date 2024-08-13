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
	RipGrepper.Common.Settings.Misc,
	RipGrepper.UI.DpiScaler,
	u_dzDpiScaleUtils,
	RipGrepper.Common.Settings.RipGrepperOpenWithSettings;

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
		ImageListButtons : TImageList;
		pnlMain : TPanel;
		lblHint : TLabel;
		procedure ActionCancelExecute(Sender : TObject);
		procedure ActionShowConfigExecute(Sender : TObject);
		procedure ActionOkExecute(Sender : TObject);
		procedure ActionSwitchViewExecute(Sender : TObject);
		procedure ActionSwitchViewUpdate(Sender : TObject);
		procedure FormShow(Sender : TObject);
		procedure lbCommandsDblClick(Sender : TObject);

		private
			FDpiScaler : TRipGrepperDpiScaler;
			FImageScaler : TImageListScaler;
			FSettings : TRipGrepperOpenWithSettings;
			FViewStyleIndex : Integer;

			procedure CreateScaledIcons(const bUpdateScaler : Boolean = False);
			class function GetEnabledCmds(const _settings : TRipGrepperOpenWithSettings) : TArray<string>;
			function GetFileNameFromCfg(const _configText : string) : string;
			function GetViewStyleIndex : Integer;
			property ViewStyleIndex : Integer read GetViewStyleIndex;

		protected
		public
			class var FScaledIcons : TImageList;
			constructor Create(AOwner : TComponent; const ASettings : TRipGrepperOpenWithSettings); reintroduce;
			destructor Destroy(); override;
			class function CreateAndShow(const _settings : TRipGrepperOpenWithSettings) : string;
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
	RipGrepper.Tools.DebugUtils,
	u_dzVclUtils,
	RipGrepper.Tools.FileUtils;

{$R *.dfm}

constructor TOpenWithCmdList.Create(AOwner : TComponent; const ASettings : TRipGrepperOpenWithSettings);
begin
	inherited Create(AOwner);
	FDpiScaler := TRipGrepperDpiScaler.Create(self);
	lbCommands.items.Clear;

	ImageListIcons.ColorDepth := TColorDepth.cd32Bit;
	FViewStyleIndex := 0;
	FSettings := ASettings;
end;

destructor TOpenWithCmdList.Destroy();
begin
	FDpiScaler.Free;
	FImageScaler.Free;
	inherited Destroy();
end;

procedure TOpenWithCmdList.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
end;

procedure TOpenWithCmdList.ActionShowConfigExecute(Sender : TObject);
begin
	TOpenWithConfigForm.CreateAndShow(FSettings);
	LoadEnbledCmds();
	CreateScaledIcons(True);
end;

procedure TOpenWithCmdList.ActionOkExecute(Sender : TObject);
begin
	FSettings.Store;
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

class function TOpenWithCmdList.CreateAndShow(const _settings : TRipGrepperOpenWithSettings) : string;
begin
	var
	form := TOpenWithCmdList.Create(nil, _settings);

	try
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
	if bUpdateScaler or not Assigned(FImageScaler) then begin
		FImageScaler.Free;
		FImageScaler := TImageListScaler.Create(Self, ImageListIcons);
	end;
	FScaledIcons := FImageScaler.GetScaledList(FDpiScaler.ActualDPI);
	lbCommands.SmallImages := FScaledIcons;
	lbCommands.LargeImages := FScaledIcons;
end;

procedure TOpenWithCmdList.FormShow(Sender : TObject);
begin
	if lbCommands.GetCount > 0 then begin
		lbCommands.ItemIndex := 0;
	end;
	lblHint.Caption := Format('Open %s(%d:%d) with ...',
		[ExtractRelativePath(FSettings.TestFile.DirPath + '\', FSettings.TestFile.FileName), FSettings.TestFile.Column,
		FSettings.TestFile.Row]);
end;

class function TOpenWithCmdList.GetEnabledCmds(const _settings : TRipGrepperOpenWithSettings) : TArray<string>;
var arrCmd : TArray<string>;
	i : Integer; sCmds : string;
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
		if (arrCmd[0].ToUpper() = 'TRUE') then begin
			Result := Result + [arrCmd[1]];
		end;

		inc(i);
	until (i = MAX_COMMAND_NUM);

end;

function TOpenWithCmdList.GetFileNameFromCfg(const _configText : string) : string;
var sFileName : string; sPath : string;
begin
	sFileName := TFileUtils.ParseCommand(_configText).ExePath;
	sPath := ExtractFileDir(sFileName);
	if sPath.IsEmpty then begin
		TFileUtils.FindExecutable(sFileName, sPath);
		sFileName := sPath;
	end;
	Result := sFileName;
end;

function TOpenWithCmdList.GetViewStyleIndex : Integer;
begin
	FViewStyleIndex := IfThen(FViewStyleIndex < Length(LISTVIEW_TYPES) - 1, FViewStyleIndex + 1);
	// skip report
	FViewStyleIndex := IfThen(FViewStyleIndex = 2, FViewStyleIndex + 1, FViewStyleIndex);
	Result := (FViewStyleIndex mod Length(LISTVIEW_TYPES));
end;

procedure TOpenWithCmdList.lbCommandsDblClick(Sender : TObject);
begin
	ActionOkExecute(Sender);
end;

procedure TOpenWithCmdList.LoadEnbledCmds();
var sfi : TSHFileInfo; icon : TIcon; item : TListItem; sFileName : string;
begin

	icon := TIcon.Create;
	try
		lbCommands.Items.Clear();
		for var configText in GetEnabledCmds(FSettings) do begin
			sFileName := GetFileNameFromCfg(configText);
			if sFileName.IsEmpty then
				continue;

			item := lbCommands.Items.Add();

			SHGetFileInfo(PChar(sFileName), 0, sfi, SizeOf(sfi), SHGFI_DISPLAYNAME);
			item.Caption := sfi.szDisplayName;

			SHGetFileInfo(PChar(sFileName), 0, sfi, SizeOf(TSHFileInfo), SHGFI_LARGEICON or SHGFI_ICON);
			icon.Handle := sfi.hIcon;

			item.ImageIndex := ImageListIcons.AddIcon(icon);
			item.Subitems.Add(configText);

			TDebugUtils.DebugMessage((Format('TOpenWithCmdList.LoadEnbledCmds cmd: %d %s ', [item.ImageIndex, sFileName])));
			DestroyIcon(sfi.hIcon);
		end;

	finally
		icon.Free;
	end;
end;

end.
