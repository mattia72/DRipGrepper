unit RipGrepper.OpenWith.ConfigForm;

interface

uses
	System.Classes,
	System.Actions,
	Vcl.StdCtrls,
	Vcl.Controls,
	Vcl.ExtCtrls,
	Vcl.ActnList,
	Vcl.CheckLst,
	Vcl.ImgList,
	Vcl.Dialogs,
	Vcl.Forms,
	System.ImageList,
	RipGrepper.Settings.AppSettings,
	RipGrepper.UI.DpiScaler,
	RipGrepper.Settings.OpenWithSettings,
	System.IniFiles,
	Vcl.ComCtrls,
	Vcl.ToolWin,
	RipGrepper.UI.SettingsFormBase,
	SVGIconImageListBase,
	SVGIconImageList,
	RipGrepper.Helper.UI.DarkMode,
	RipGrepper.Tools.FileUtils;

type
	TCheckBoxState = (csbNone, csbTrue, csbFalse);

	TOpenWithConfigForm = class(TSettingsBaseForm, ISettingsForm)

		var
			ActionListConfig : TActionList;
			pnlBottom : TPanel;
			ActionMoveUp : TAction;
			ActionMoveDown : TAction;
			ActionTest : TAction;
			OpenDialog1 : TOpenDialog;
			ActionOpenFileDlg : TAction;
			ActionModify : TAction;
			ActionAdd : TAction;
			ActionRemove : TAction;
			pnlMain : TPanel;
			ActionOk : TAction;
			ActionCancel : TAction;
			ToolBar1 : TToolBar;
			tbPlus : TToolButton;
			tbMinus : TToolButton;
			tbUp : TToolButton;
			tbDown : TToolButton;
			tbTestRun : TToolButton;
			SVGIconImageList1 : TSVGIconImageList;
			lvCommands : TListView;
			btn_Save : TButton;
			btn_Cancel : TButton;
			procedure ActionAddExecute(Sender : TObject);
			procedure ActionCancelExecute(Sender : TObject);
			procedure ActionModifyExecute(Sender : TObject);
			procedure ActionMoveDownExecute(Sender : TObject);
			procedure ActionMoveDownUpdate(Sender : TObject);
			procedure ActionMoveUpExecute(Sender : TObject);
			procedure ActionMoveUpUpdate(Sender : TObject);
			procedure ActionOkExecute(Sender : TObject);
			procedure ActionRemoveExecute(Sender : TObject);
			procedure ActionRemoveUpdate(Sender : TObject);
			procedure ActionTestExecute(Sender : TObject);
			procedure ActionTestUpdate(Sender : TObject);
			procedure lvCommandsDblClick(Sender : TObject);

		private
			FColorTheme : string;
			FDpiScaler : TRipGrepperDpiScaler;
			FOpenWithSettings : TOpenWithSettings;
			FThemeHandler : TThemeHandler;
			procedure AddOrSetCommandItem(const _ci : TCommandItem; _li : TListItem = nil);
			procedure ExchangeItems(_lv : TListView; const i, j : Integer);
			function GetThemeHandler : TThemeHandler;
			procedure SetSelectedItem(const _idx : integer);
			property ThemeHandler : TThemeHandler read GetThemeHandler;

		protected
		public
			{ Public-Deklarationen }
			constructor Create(AOwner : TComponent; const _settings : TOpenWithSettings; const _colorTheme : string); reintroduce;
			destructor Destroy; override;
			class procedure CreateAndShow(const _settings : TOpenWithSettings; const _colorTheme : string);
			procedure ReadSettings; override;
			procedure WriteSettings; override;

	end;

implementation

uses
	System.SysUtils,
	Vcl.Clipbrd,
	Winapi.Windows,
	Winapi.ShellAPI,
	RipGrepper.OpenWith.Constants,
	RipGrepper.OpenWith.Runner,
	RipGrepper.OpenWith.Params,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Helper.UI,
	System.RegularExpressions,
	RipGrepper.OpenWith.CmdEditorForm,
	ArrayEx,
	RipGrepper.Settings.SettingsDictionary,
	RipGrepper.Settings.SettingVariant;

const
	IDX_DESCRIPTION = 1;
	IDX_COMMAND_LINE = 0;

	{$R *.dfm}

constructor TOpenWithConfigForm.Create(AOwner : TComponent; const _settings : TOpenWithSettings; const _colorTheme : string);
begin
	inherited Create(AOwner, _settings, _colorTheme);
	FDpiScaler := TRipGrepperDpiScaler.Create(self);
	lvCommands.MultiSelect := True; // we need this for working SelCount
	FOpenWithSettings := _settings;
	FOpenWithSettings.ReadIni; // we should read ini every time, it can be overwritten by another instance...
	ReadSettings;
	FColorTheme := _colorTheme;
	ThemeHandler.Init(_colorTheme);
end;

destructor TOpenWithConfigForm.Destroy;
begin
	FDpiScaler.Free;
	inherited;
end;

procedure TOpenWithConfigForm.ActionAddExecute(Sender : TObject);
var
	ci : TCommandItem;
	li : TListItem;
	arrEx : TArrayEx<string>;
begin
	inherited;
	li := lvCommands.Selected;
	if Assigned(li) then begin
		ci.Caption := li.Caption;
		arrEx := li.SubItems.ToStringArray;
		ci.CommandLine := TCommandLineRec.ParseCommand(arrEx.SafeItem[0]);
		ci.Description := arrEx.SafeItem[1];
	end;
	ci := TOpenWithCommandEditor.CreateAndShow(self, ci, FColorTheme);
	AddOrSetCommandItem(ci);
end;

procedure TOpenWithConfigForm.ActionCancelExecute(Sender : TObject);
begin
	OnCancel();
end;

procedure TOpenWithConfigForm.ActionModifyExecute(Sender : TObject);
begin
	inherited;
	var
	item := lvCommands.Items[lvCommands.ItemIndex];
	var
		ci : TCommandItem := TCommandItem.New(item.Caption, item.SubItems[IDX_COMMAND_LINE]);
	AddOrSetCommandItem(ci, item);
end;

procedure TOpenWithConfigForm.ActionMoveDownExecute(Sender : TObject);
begin
	inherited;
	var
	idx := lvCommands.ItemIndex;
	ExchangeItems(lvCommands, idx, idx + 1);
	SetSelectedItem(idx + 1);
end;

procedure TOpenWithConfigForm.ActionMoveDownUpdate(Sender : TObject);
begin
	inherited;
	ActionMoveDown.Enabled := (lvCommands.SelCount = 1) and (lvCommands.ItemIndex < lvCommands.GetCount - 1);
end;

procedure TOpenWithConfigForm.ActionMoveUpExecute(Sender : TObject);
begin
	inherited;
	var
	idx := lvCommands.ItemIndex;
	ExchangeItems(lvCommands, idx, idx - 1);
	SetSelectedItem(idx - 1);
end;

procedure TOpenWithConfigForm.ActionMoveUpUpdate(Sender : TObject);
begin
	inherited;
	ActionMoveUp.Enabled := (lvCommands.SelCount = 1) and (lvCommands.ItemIndex > 0);
end;

procedure TOpenWithConfigForm.ActionOkExecute(Sender : TObject);
begin
	OnOk();
end;

procedure TOpenWithConfigForm.ActionRemoveExecute(Sender : TObject);
begin
	inherited;
	lvCommands.DeleteSelected;
end;

procedure TOpenWithConfigForm.ActionRemoveUpdate(Sender : TObject);
begin
	inherited;
	ActionRemove.Enabled := (lvCommands.SelCount = 1);
end;

procedure TOpenWithConfigForm.ActionTestExecute(Sender : TObject);
var
	owp : TOpenWithParams;
begin
	inherited;
	owp := FOpenWithSettings.TestFile; // GxOtaGetCurrentSourceFile;
	TOpenWithRunner.RunEditorCommand(lvCommands.Items[lvCommands.ItemIndex].SubItems[IDX_COMMAND_LINE], owp);
end;

procedure TOpenWithConfigForm.ActionTestUpdate(Sender : TObject);
begin
	inherited;
	ActionTest.Enabled := (lvCommands.SelCount = 1) and (not FOpenWithSettings.TestFile.IsEmpty);
end;

procedure TOpenWithConfigForm.AddOrSetCommandItem(const _ci : TCommandItem; _li : TListItem = nil);
var
	item : TListItem;
begin
	if _ci.Caption.IsEmpty then begin
		Exit;
	end;

	lvCommands.Items.BeginUpdate;
	try
		if Assigned(_li) then begin
			item := _li;
		end else begin
			item := lvCommands.Items.Add;
		end;
		item.Caption := _ci.Caption;
		item.SubItems.Clear;
		item.SubItems.Add(_ci.CommandLine.AsString);
		item.SubItems.Add(_ci.Description);
		item.Checked := _ci.IsActive;
	finally
		lvCommands.Items.EndUpdate;
	end;
end;

class procedure TOpenWithConfigForm.CreateAndShow(const _settings : TOpenWithSettings; const _colorTheme : string);
begin
	// write ini file content
	_settings.UpdateFile;
	var
	form := TOpenWithConfigForm.Create(nil, _settings, _colorTheme);
	try
		form.ShowModal;
	finally
		form.Free;
		// re read content
		_settings.ReLoadFromDisk;
	end;
end;

function TOpenWithConfigForm.GetThemeHandler : TThemeHandler;
begin
	if not Assigned(FThemeHandler) then begin
		FThemeHandler := TThemeHandler.Create(self);
	end;
	Result := FThemeHandler;
end;

procedure TOpenWithConfigForm.ReadSettings;
var
	arr : TArray<string>;
	ci : TCommandItem;
	listCmdsFromSettings : TStringList;
	i : integer;
begin
	inherited ReadSettings;
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithConfigForm.ReadSettings');

	listCmdsFromSettings := TStringList.Create;
	try
		i := 0;
		repeat
			var
			sCmd := FOpenWithSettings.Command[i];
			dbgMsg.MsgFmt('sCmd:%s ', [sCmd]);
			if sCmd = '' then begin
				break;
			end;
			listCmdsFromSettings.Add(sCmd);
			inc(i);
		until (i = MAX_COMMAND_NUM);

		if listCmdsFromSettings.Count <> 0 then begin
			lvCommands.Items.Clear;
		end;

		for var sCmd : string in listCmdsFromSettings do begin
			dbgMsg.MsgFmt('%s ', [sCmd]);
			arr := sCmd.Split([SEPARATOR]); // TAB
			if Length(arr) > 1 then begin
				ci := TCommandItem.New(arr);
				AddOrSetCommandItem(ci);
				dbgMsg.MsgFmt('"%s" "%s"', [arr[0], arr[1]]);
			end;
		end;
	finally
		listCmdsFromSettings.Free;
	end;
end;

procedure TOpenWithConfigForm.WriteSettings;
var
	item : TListItem;
	settings : string;
	sCmd : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithConfigForm.WriteSettings');
	settings := '';
	FOpenWithSettings.ClearCommandList; // so deleted entries will be recognized
	for var i := 0 to lvCommands.Items.Count - 1 do begin
		item := lvCommands.Items[i];
		sCmd := item.Subitems[IDX_COMMAND_LINE].Replace(SEPARATOR, '', [rfReplaceAll]);
		dbgMsg.Msg(Format('%s', [sCmd]));
		settings := Format('%s' + SEPARATOR + '%s' + SEPARATOR + '%s' + SEPARATOR + '%s',
			{ } [BoolToStr(item.Checked and TOpenWithCommandEditor.CheckCommand(sCmd), true),
			{ } item.Caption, // caption
			{ } sCmd, // command line
			{ } item.SubItems[IDX_DESCRIPTION]]); // descr
		FOpenWithSettings.Command[i] := settings;

		dbgMsg.Msg(Format('%s', [FOpenWithSettings.Command[i]]));
	end;
	// after ClearCommandList recreate settings...
	FOpenWithSettings.SettingsDict.CreateSetting(OPENWITH_COMMAND_KEY,
		{ } FOpenWithSettings.CommandListSetting,
		{ } FOpenWithSettings.PersisterFactory);

	// inherited WriteSettings; it's not eonugh
	FOpenWithSettings.ForceWriteToIni; // save always
end;

procedure TOpenWithConfigForm.lvCommandsDblClick(Sender : TObject);
var
	ci : TCommandItem;
	item : TListItem;
	arrEx : TArrayEx<string>;
begin
	inherited;
	item := lvCommands.Items[lvCommands.ItemIndex];
	ci.Caption := item.Caption;
	arrEx := item.SubItems.ToStringArray;
	ci.CommandLine := TCommandLineRec.ParseCommand(arrEx.SafeItem[0]);
	ci.Description := arrEx.SafeItem[1];
	ci := TOpenWithCommandEditor.CreateAndShow(self, ci, FColorTheme);
	AddOrSetCommandItem(ci, item);
	TDebugUtils.DebugMessage((Format('TOpenWithConfigForm.lvCommandsDblClick SelectCount %d', [lvCommands.SelCount])));
end;

procedure TOpenWithConfigForm.ExchangeItems(_lv : TListView; const i, j : Integer);
var
	li : TListItem;
begin
	_lv.Items.BeginUpdate;
	try
		li := TListItem.Create(_lv.Items);
		li.Assign(_lv.Items.Item[i]);
		_lv.Items.Item[i].Assign(_lv.Items.Item[j]);
		_lv.Items.Item[j].Assign(li);
		li.Free;
	finally
		_lv.Items.EndUpdate
	end;
end;

procedure TOpenWithConfigForm.SetSelectedItem(const _idx : integer);
begin
	lvCommands.Selected := nil;
	lvCommands.ItemIndex := _idx;
	// lvCommands.Items[_idx].Focused := True;
end;

end.
