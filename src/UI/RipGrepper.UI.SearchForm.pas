unit RipGrepper.UI.SearchForm;

interface

uses

	Winapi.Messages,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.StdCtrls,
	Vcl.ExtCtrls,
	System.ImageList,
	Vcl.ImgList,
	System.Actions,
	Vcl.ActnList,
	RipGrepper.Settings.RipGrepperSettings,
	Vcl.StdActns,
	Vcl.Dialogs,
	RipGrepper.Common.Constants,
	RipGrepper.UI.DpiScaler,
	GX_BaseForm,
	Vcl.ButtonGroup,
	Vcl.Buttons,
	Vcl.ToolWin,
	Vcl.ComCtrls,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Settings.RipGrepParameterSettings,
	Vcl.Samples.Spin,
	RipGrepper.Helper.Types,
	RipGrepper.Settings.SearchFormSettings,
	RipGrepper.Common.Interfaces;

type
	TRipGrepperSearchDialogForm = class(TForm)
		pnlMiddle : TPanel;
		lblParams : TLabel;
		lblPaths : TLabel;
		cmbOptions : TComboBox;
		cmbSearchDir : TComboBox;
		cmbSearchText : TComboBox;
		btnConfig : TButton;
		btnSearch : TButton;
		btnCancel : TButton;
		ImageList1 : TImageList;
		ActionList : TActionList;
		ActionSearch : TAction;
		ActionCancel : TAction;
		ActionShowRipGrepOptionsForm : TAction;
		btnSearchFolder : TButton;
		ActionSearchFolder : TAction;
		btnSearchFile : TButton;
		ActionSearchFile : TAction;
		pnlBottom : TPanel;
		ActionAddParamMatchCase : TAction;
		ActionAddParamWord : TAction;
		ActionAddParamRegex : TAction;
		toolbarSearchTextOptions : TToolBar;
		tbIgnoreCase : TToolButton;
		tbMatchWord : TToolButton;
		tbUseRegex : TToolButton;
		gbExpert : TGroupBox;
		gbOptionsFilters : TGroupBox;
		lblFileMasks : TLabel;
		cmbFileMasks : TComboBox;
		Label1 : TLabel;
		memoCommandLine : TMemo;
		btnCopyToClipBoard : TButton;
		ActionCopyToClipboard : TAction;
		btnHelpFileMask : TButton;
		ActionShowFileMaskHelp : TAction;
		btnRGOptionsHelp : TButton;
		ActionShowRGOptionsHelp : TAction;
		gbOptionsOutput : TGroupBox;
		cbRgParamHidden : TCheckBox;
		cbRgParamNoIgnore : TCheckBox;
		cbRgParamPretty : TCheckBox;
		cbRgParamContext : TCheckBox;
		seContextLineNum : TSpinEdit;
		rbExtensionOptions : TRadioGroup;
		gbFileFilters : TGroupBox;
		gbPath : TGroupBox;
		btnShowInLines : TButton;
		ActionShowInLines : TAction;
		lblHintHelper : TLabel;
		cbRgParamEncoding : TCheckBox;
		cmbRgParamEncoding : TComboBox;
		btnSetAsDefault : TButton;
		ActionSetAsDefault : TAction;
		pnlTop : TPanel;
		TabControl1 : TTabControl;
		cmbReplaceText : TComboBox;
		btnRGReplaceHelp : TButton;
		ActionShowRGReplaceOptionHelp : TAction;
		procedure ActionAddParamMatchCaseExecute(Sender : TObject);
		procedure ActionAddParamMatchCaseUpdate(Sender : TObject);
		procedure ActionAddParamRegexExecute(Sender : TObject);
		procedure ActionAddParamRegexUpdate(Sender : TObject);
		procedure ActionAddParamWordExecute(Sender : TObject);
		procedure ActionAddParamWordUpdate(Sender : TObject);
		procedure ActionCancelExecute(Sender : TObject);
		procedure ActionCopyToClipboardExecute(Sender : TObject);
		procedure ActionSearchFolderExecute(Sender : TObject);
		procedure ActionShowRipGrepOptionsFormExecute(Sender : TObject);
		procedure ActionSearchExecute(Sender : TObject);
		procedure ActionSearchFileExecute(Sender : TObject);
		procedure ActionSetAsDefaultExecute(Sender : TObject);
		procedure ActionShowFileMaskHelpExecute(Sender : TObject);
		procedure ActionShowInLinesExecute(Sender : TObject);
		procedure ActionShowRGOptionsHelpExecute(Sender : TObject);
		procedure ActionShowRGReplaceOptionHelpExecute(Sender : TObject);
		procedure cbRgParamContextClick(Sender : TObject);
		procedure cbRgParamEncodingClick(Sender : TObject);
		procedure cbRgParamHiddenClick(Sender : TObject);
		procedure cbRgParamNoIgnoreClick(Sender : TObject);
		procedure cbRgParamPrettyClick(Sender : TObject);
		procedure cmbFileMasksChange(Sender : TObject);
		procedure cmbFileMasksExit(Sender : TObject);
		procedure cmbFileMasksSelect(Sender : TObject);
		procedure cmbOptionsChange(Sender : TObject);
		procedure cmbOptionsSelect(Sender : TObject);
		procedure cmbReplaceTextChange(Sender : TObject);
		procedure cmbRgParamEncodingChange(Sender : TObject);
		procedure cmbSearchDirChange(Sender : TObject);
		procedure cmbSearchTextChange(Sender : TObject);
		procedure ShowOptionsForm;
		procedure FormClose(Sender : TObject; var Action : TCloseAction);
		procedure FormResize(Sender : TObject);
		procedure FormShow(Sender : TObject);
		procedure ToggleExpertMode;
		procedure rbExtensionOptionsClick(Sender : TObject);
		procedure seContextLineNumChange(Sender : TObject);
		procedure TabControl1Change(Sender : TObject);

		private
			FbExtensionOptionsSkipClick : Boolean;
			FHistItemObj : IHistoryItemObject;
			FDpiScaler : TRipGrepperDpiScaler;
			FExpertGroupHeight : Integer;
			FParamsSetByGui : TGuiSearchTextParams;
			FCbClickEventEnabled : Boolean;
			FcmbOptionsOldText : string;
			FOptionsFiltersOrigHeight : Integer;
			FOptionsOutputOrigTop : Integer;
			FpnlMiddleOrigHeight : Integer;
			FOrigSearchFormSettings : TSearchFormSettings;
			FSettings : TRipGrepperSettings;
			FTopPanelOrigHeight : Integer;
			FOrigHeight : integer;
			function GetSelectedPaths(const _fdo : TFileDialogOptions) : string;
			procedure WriteInitialSettingsToCtrls;
			procedure ButtonDown(const _searchOption : EGuiOption; _tb : TToolButton; const _bNotMatch : Boolean = False); overload;
			function GetFullHeight(_ctrl : TControl) : integer;
			function IsOptionSet(const _sParamRegex : string; const _sParamValue : string = '') : Boolean;
			procedure ProcessControl(_ctrl : TControl; _imgList : TImageList);
			procedure RemoveNecessaryOptionsFromCmbOptionsText;
			procedure SetComboItemsAndText(_cmb : TComboBox; const _argName : string; const _items : TStrings;
				const _separator : string = ' ');
			procedure SetComboItemsFromOptions(_cmb : TComboBox; const _argMaskRegex : string; const _items : TStrings);
			procedure UpdateCmbOptionsAndMemoCommandLine;
			procedure StoreHistoriesAsCmbEntries;
			procedure WriteCtrlsToRipGrepParametersSettings;
			procedure WriteCtrlsToSettings(const _bDefaultOnly : Boolean = False);
			procedure UpdateCheckBoxesByRgOptions;
			procedure UpdateCheckBoxesBySettings;
			function CheckAndCorrectMultiLine(const _str : TMultiLineString) : string;
			procedure CheckVsCodeRipGrep;
			function GetFullHeights : integer;
			function HasHistItemObjWithResult : Boolean;
			function GetInIDESelectedText : string;
			procedure LoadExtensionSearchSettings;
			procedure LoadInitialSettings;
			procedure SetCmbSearchPathText(const _sPath : string);
			procedure SetExpertGroupSize;
			class procedure SetReplaceText(_settings : TRipGrepperSettings; const _replaceText : string);
			procedure SetReplaceTextSetting(const _replaceText : string);
			procedure ShowReplaceCtrls(const _bShow : Boolean);
			procedure UpdateButtonsBySettings;
			procedure UpdateCmbsOnIDEContextChange;
			procedure UpdateFileMasksInFileMasks;
			procedure UpdateFileMasksInOptions; overload;
			procedure UpdateHeight;
			procedure WriteOptionCtrlToRipGrepParametersSetting;
			procedure WriteSearchFormSettingsToCtrls();

		public
			constructor Create(AOwner : TComponent; const _settings : TRipGrepperSettings; const _histObj : IHistoryItemObject);
				reintroduce; virtual;
			destructor Destroy; override;
			function IsReplaceMode : Boolean;
			procedure LoadDefaultSettings;

			class function ShowSearchForm(_owner : TComponent; _settings : TRipGrepperSettings; _histObj : IHistoryItemObject) : integer;

			procedure UpdateMemoCommandLine(const _bSkipReadCtrls : Boolean = False);
			procedure UpdateCtrls(_ctrlChanged : TControl);
	end;

var
	RipGrepperSearchDialogForm : TRipGrepperSearchDialogForm;

implementation

uses
	RipGrepper.Helper.UI,
	RipGrepper.Tools.ProcessUtils,
	System.UITypes,
	RipGrepper.UI.RipGrepOptionsForm,
	System.SysUtils,
	System.RegularExpressions,
	System.Math,
	ArrayEx,
	RipGrepper.CommandLine.Builder,
	Vcl.Clipbrd,
	RipGrepper.CommandLine.OptionHelper,
	RipGrepper.Tools.DebugUtils,
	Winapi.ShellAPI,
	System.StrUtils,
	{$IFNDEF STANDALONE} RipGrepper.Common.IOTAUtils, {$ENDIF}
	RipGrepper.Tools.FileUtils,
	System.IOUtils,
	Winapi.Windows,
	RipGrepper.Settings.AppSettings,
	RipGrepper.Settings.ExtensionSettings,
	RipGrepper.CommandLine.OptionStrings;

{$R *.dfm}

constructor TRipGrepperSearchDialogForm.Create(AOwner : TComponent; const _settings : TRipGrepperSettings;
	const _histObj : IHistoryItemObject);
begin
	inherited Create(AOwner);
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.Create');

	FSettings := _settings;
	FHistItemObj := _histObj;

	LoadInitialSettings;
	dbgMsg.Msg(FSettings.SearchFormSettings.ToLogString);
	dbgMsg.Msg('gui params=' + FParamsSetByGui.ToLogString);

	FDpiScaler := TRipGrepperDpiScaler.Create(self);

	FExpertGroupHeight := gbExpert.Height;
	FOptionsFiltersOrigHeight := gbOptionsFilters.Height;
	FOptionsOutputOrigTop := gbOptionsOutput.Top;
	FpnlMiddleOrigHeight := pnlMiddle.Height;
	FTopPanelOrigHeight := pnlTop.Height;
	FOrigHeight := Height;

	toolbarSearchTextOptions.AutoSize := False; // else shrinked as extension
	cmbOptions.AutoComplete := False; // so we know the old value after change
end;

destructor TRipGrepperSearchDialogForm.Destroy;
begin
	if not HasHistItemObjWithResult then begin
		FParamsSetByGui.Free;
	end;
	FOrigSearchFormSettings.Free;
	FDpiScaler.Free;
	inherited Destroy;
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamMatchCaseExecute(Sender : TObject);
begin
	FParamsSetByGui.SetOrReset(EGuiOption.soMatchCase);
	UpdateCmbOptionsAndMemoCommandLine();
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamMatchCaseUpdate(Sender : TObject);
begin
	ButtonDown(EGuiOption.soMatchCase, tbIgnoreCase);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamRegexExecute(Sender : TObject);
begin
	FParamsSetByGui.SetOrReset(EGuiOption.soUseRegex);
	UpdateCmbOptionsAndMemoCommandLine();
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamRegexUpdate(Sender : TObject);
begin
	ButtonDown(EGuiOption.soUseRegex, tbUseRegex);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamWordExecute(Sender : TObject);
begin
	FParamsSetByGui.SetOrReset(EGuiOption.soMatchWord);
	UpdateCmbOptionsAndMemoCommandLine();
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamWordUpdate(Sender : TObject);
begin
	ButtonDown(EGuiOption.soMatchWord, tbMatchWord);
end;

procedure TRipGrepperSearchDialogForm.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
end;

procedure TRipGrepperSearchDialogForm.ActionCopyToClipboardExecute(Sender : TObject);
begin
	ClipBoard.AsText := memoCommandLine.Text;
end;

procedure TRipGrepperSearchDialogForm.ActionSearchFolderExecute(Sender : TObject);
var
	selectedDirs : string;
begin
	selectedDirs := GetSelectedPaths([fdoAllowMultiSelect, fdoPickfolders]);

	if selectedDirs <> '' then begin
		SetCmbSearchPathText(selectedDirs);
		UpdateCtrls(cmbSearchDir);
	end;
end;

procedure TRipGrepperSearchDialogForm.ActionShowRipGrepOptionsFormExecute(Sender : TObject);
begin
	ShowOptionsForm();
end;

procedure TRipGrepperSearchDialogForm.ActionSearchExecute(Sender : TObject);
begin
	WriteCtrlsToSettings(False);
	ModalResult := mrOk;
end;

procedure TRipGrepperSearchDialogForm.ActionSearchFileExecute(Sender : TObject);
var
	selectedFiles : string;
begin
	selectedFiles := GetSelectedPaths([fdoAllowMultiSelect]);

	if selectedFiles <> '' then begin
		SetCmbSearchPathText(selectedFiles);
		UpdateCtrls(cmbSearchDir);
	end;
end;

procedure TRipGrepperSearchDialogForm.ActionSetAsDefaultExecute(Sender : TObject);
begin
	WriteCtrlsToSettings(True);
	FSettings.StoreAsDefaultsToDict();
	FSettings.UpdateIniFile;
end;

procedure TRipGrepperSearchDialogForm.ActionShowFileMaskHelpExecute(Sender : TObject);
begin
	ShellExecute(0, 'OPEN', PChar(WWW_LINK_GLOBBING_HELP), '', '', SW_SHOWNORMAL);
end;

procedure TRipGrepperSearchDialogForm.ActionShowInLinesExecute(Sender : TObject);
var
	str : string;
begin
	if ActionShowInLines.Hint = SHOW_CMD_IN_SEPARATE_LINES then begin
		str := memoCommandLine.Text;
		memoCommandLine.Text := string.Join(CRLF, str.Split([' ']));
		ActionShowInLines.Hint := SHOW_CMD_IN_ONE_LINE;
	end else begin
		str := memoCommandLine.Text;
		memoCommandLine.Text := str.Replace(CRLF, ' ', [rfReplaceAll]);
		ActionShowInLines.Hint := SHOW_CMD_IN_SEPARATE_LINES;
	end;;
end;

procedure TRipGrepperSearchDialogForm.ActionShowRGOptionsHelpExecute(Sender : TObject);
begin
	ShellExecute(0, 'OPEN', PChar(WWW_LINK_RG_MAN_PAGE), '', '', SW_SHOWNORMAL);
end;

procedure TRipGrepperSearchDialogForm.ActionShowRGReplaceOptionHelpExecute(Sender : TObject);
begin
	ShellExecute(0, 'OPEN', PChar(WWW_LINK_RG_REPLACE_MAN_PAGE), '', '', SW_SHOWNORMAL);
end;

procedure TRipGrepperSearchDialogForm.ProcessControl(_ctrl : TControl; _imgList : TImageList);
var
	i : integer;
begin
	if _ctrl is TButton then begin
		(_ctrl as TButton).Images := _imgList;
	end;

	if _ctrl is TWinControl then begin
		for i := 0 to (_ctrl as TWinControl).ControlCount - 1 do
			ProcessControl((_ctrl as TWinControl).Controls[i], _imgList);
	end;
end;

procedure TRipGrepperSearchDialogForm.ShowOptionsForm;
begin
	var
	frm := TRipGrepOptionsForm.Create(self, FSettings.RipGrepParameters);
	try
		if (mrOk = frm.ShowModal) then begin
			FSettings.RipGrepParameters.RgExeOptions.RemoveOption(
				{ } string.Join('|', RG_NECESSARY_PARAMS + RG_GUI_SET_PARAMS));
			cmbOptions.Text := FSettings.RipGrepParameters.RgExeOptions.AsString;
			UpdateCtrls(cmbOptions);
		end;
	finally
		frm.Free;
	end;
end;

procedure TRipGrepperSearchDialogForm.FormClose(Sender : TObject; var Action : TCloseAction);
begin
	if ModalResult <> mrCancel then begin
		if HasHistItemObjWithResult then begin
			FSettings.SearchFormSettings.StoreSearchSettings(False);
			FHistItemObj.SearchFormSettings.Copy(FSettings.SearchFormSettings);
			FHistItemObj.SearchFormSettings.LoadFromDict();
			FHistItemObj.RipGrepArguments.Clear;
			FHistItemObj.RipGrepArguments.Assign(FSettings.GetRipGrepArguments());
			FHistItemObj.GuiSearchTextParams.Copy(FParamsSetByGui);
			FSettings.SearchFormSettings.Copy(FOrigSearchFormSettings);
			FSettings.SearchFormSettings.LoadFromDict();
		end;
		FSettings.StoreHistories();
		FSettings.UpdateIniFile;
	end;
end;

procedure TRipGrepperSearchDialogForm.FormShow(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.FormShow');
	WriteInitialSettingsToCtrls;
	LoadExtensionSearchSettings;

	CheckVsCodeRipGrep;

	WriteCtrlsToRipGrepParametersSettings;
	UpdateCmbOptionsAndMemoCommandLine;
	UpdateCheckBoxesByRgOptions();

	ShowReplaceCtrls(IsReplaceMode());
	UpdateHeight();
	ActiveControl := cmbSearchText;
end;

function TRipGrepperSearchDialogForm.GetSelectedPaths(const _fdo : TFileDialogOptions) : string;
var
	dlg : TFileOpenDialog;
begin
	dlg := TFileOpenDialog.Create(nil);
	try
		dlg.DefaultFolder := 'C:\';
		// dlg. := 'All files (*.*)|*.*';
		dlg.Options := dlg.Options + _fdo;
		if dlg.Execute(Handle) then begin
			dlg.Files.Delimiter := SEARCH_PATH_SEPARATOR;
			Result := dlg.Files.DelimitedText;
		end;
	finally
		dlg.Free;
	end;
end;

procedure TRipGrepperSearchDialogForm.WriteInitialSettingsToCtrls;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.WriteInitialSettingsToCtrls');
	dbgMsg.MsgFmt('FSettings.IsAlreadyRead=%s', [BoolToStr(FSettings.IsAlreadyRead)]);

	SetComboItemsAndText(cmbSearchDir, RG_ARG_SEARCH_PATH, FSettings.SearchPathsHistory, ';');
	SetComboItemsAndText(cmbSearchText, RG_ARG_SEARCH_TEXT, FSettings.SearchTextsHistory);
	SetComboItemsAndText(cmbReplaceText, RG_ARG_REPLACE_TEXT, FSettings.ReplaceTextsHistory);

	SetComboItemsFromOptions(cmbFileMasks, RG_PARAM_REGEX_GLOB, FSettings.FileMasksHistory);
	// Set available encodings...
	SetComboItemsAndText(cmbRgParamEncoding, RG_PARAM_REGEX_ENCODING, FSettings.AppSettings.EncodingItems);

	UpdateButtonsBySettings;
	UpdateCheckBoxesBySettings();
	SetComboItemsAndText(cmbOptions, RG_ARG_OPTIONS, FSettings.ExpertOptionHistory);

	FcmbOptionsOldText := cmbOptions.Text;
end;

procedure TRipGrepperSearchDialogForm.ButtonDown(const _searchOption : EGuiOption; _tb : TToolButton; const _bNotMatch : Boolean = False);
begin
	if (_bNotMatch) then begin
		_tb.Down := not(_searchOption in FParamsSetByGui.SearchOptions);
	end else begin
		_tb.Down := _searchOption in FParamsSetByGui.SearchOptions;
	end;
end;

procedure TRipGrepperSearchDialogForm.cbRgParamContextClick(Sender : TObject);
begin
	if not FCbClickEventEnabled then
		Exit;

	seContextLineNum.Enabled := cbRgParamContext.Checked;
	FSettings.SearchFormSettings.Context := IfThen(seContextLineNum.Enabled, seContextLineNum.Value);
	UpdateCtrls(cbRgParamContext);
end;

procedure TRipGrepperSearchDialogForm.cbRgParamHiddenClick(Sender : TObject);
begin
	if not FCbClickEventEnabled then
		Exit;

	FSettings.SearchFormSettings.Hidden := cbRgParamHidden.Checked;
	UpdateCtrls(cbRgParamHidden);
end;

procedure TRipGrepperSearchDialogForm.cbRgParamNoIgnoreClick(Sender : TObject);
begin
	if not FCbClickEventEnabled then
		Exit;

	FSettings.SearchFormSettings.NoIgnore := cbRgParamNoIgnore.Checked;
	UpdateCtrls(cbRgParamNoIgnore);
end;

procedure TRipGrepperSearchDialogForm.cbRgParamPrettyClick(Sender : TObject);
begin
	if not FCbClickEventEnabled then
		Exit;

	FSettings.SearchFormSettings.Pretty := cbRgParamPretty.Enabled and cbRgParamPretty.Checked;
	UpdateCtrls(cbRgParamPretty);
end;

procedure TRipGrepperSearchDialogForm.cmbFileMasksChange(Sender : TObject);
begin
	UpdateCtrls(cmbFileMasks);
end;

procedure TRipGrepperSearchDialogForm.cmbFileMasksExit(Sender : TObject);
begin
	UpdateCtrls(cmbFileMasks);
end;

procedure TRipGrepperSearchDialogForm.cmbFileMasksSelect(Sender : TObject);
begin
	UpdateCtrls(cmbFileMasks);
end;

procedure TRipGrepperSearchDialogForm.cmbOptionsSelect(Sender : TObject);
begin
	UpdateCtrls(cmbOptions);
end;

procedure TRipGrepperSearchDialogForm.cmbSearchDirChange(Sender : TObject);
begin
	UpdateCtrls(cmbSearchDir);
end;

procedure TRipGrepperSearchDialogForm.cmbSearchTextChange(Sender : TObject);
begin
	cmbSearchText.Text := CheckAndCorrectMultiLine(cmbSearchText.Text);
	UpdateCtrls(cmbSearchText);
end;

procedure TRipGrepperSearchDialogForm.ToggleExpertMode;
begin
	FSettings.AppSettings.ExpertMode := not FSettings.AppSettings.ExpertMode;
	UpdateHeight();
end;

function TRipGrepperSearchDialogForm.GetFullHeight(_ctrl : TControl) : integer;
begin
	Result := _ctrl.Margins.Top + _ctrl.Height + _ctrl.Margins.Bottom;
end;

function TRipGrepperSearchDialogForm.IsOptionSet(const _sParamRegex : string; const _sParamValue : string = '') : Boolean;
begin
	Result := FParamsSetByGui.RgOptions.IsOptionSet(_sParamRegex, _sParamValue);
end;

procedure TRipGrepperSearchDialogForm.RemoveNecessaryOptionsFromCmbOptionsText;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.RemoveNecessaryOptionsFromCmbOptionsText');
	dbgMsg.Msg('ExpertOptions=' + FParamsSetByGui.ExpertOptions.AsString);

	// Remove necessary options
	FParamsSetByGui.ExpertOptions.RemoveOptions(RG_NECESSARY_PARAMS + RG_GUI_SET_PARAMS);
	cmbOptions.Text := FParamsSetByGui.ExpertOptions.AsString;
	dbgMsg.Msg('cmbOptions.Text=' + cmbOptions.Text);

	WriteOptionCtrlToRipGrepParametersSetting;

	dbgMsg.Msg('ExpertOptions=' + FParamsSetByGui.ExpertOptions.AsString);
end;

procedure TRipGrepperSearchDialogForm.SetComboItemsAndText(_cmb : TComboBox; const _argName : string; const _items : TStrings;
	const _separator : string = ' ');
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.SetComboItemsAndText');

	_cmb.Items.Assign(_items);
	if HasHistItemObjWithResult then begin
		_cmb.Text := string.Join(_separator, FHistItemObj.RipGrepArguments.GetValues(_argName));
		dbgMsg.MsgFmt('%s=%s', [_cmb.Name, _cmb.Text]);
	end else begin
		_cmb.ItemIndex := 0;
		dbgMsg.MsgFmt('idx:%d, %s=%s', [_cmb.ItemIndex, _cmb.Name, _cmb.Text]);
	end;
end;

procedure TRipGrepperSearchDialogForm.SetComboItemsFromOptions(_cmb : TComboBox; const _argMaskRegex : string; const _items : TStrings);
var
	params : TArray<string>;
begin
	_cmb.Items.Assign(_items);
	if HasHistItemObjWithResult then begin
		params := FHistItemObj.RipGrepArguments.GetValues(RG_ARG_OPTIONS);
		_cmb.Text := TCommandLineBuilder.GetFileMasksDelimited(string.Join(' ', params));
	end else begin
		_cmb.Text := _cmb.Items[0];
	end;
	TDebugUtils.DebugMessageFormat('TRipGrepperSearchDialogForm.SetComboItemsFromOptions: %s - %s', [_cmb.Name, _cmb.Text]);
end;

procedure TRipGrepperSearchDialogForm.UpdateCmbOptionsAndMemoCommandLine;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.UpdateCmbOptionsAndMemoCommandLine');
	dbgMsg.Msg('FParamsSetByGui=' + FParamsSetByGui.ToString);
	RemoveNecessaryOptionsFromCmbOptionsText;
	dbgMsg.Msg('RgOptions=' + string.Join(' ', FParamsSetByGui.RgOptions.AsString));
	UpdateMemoCommandLine(True); // RgExeOptions may changed
	dbgMsg.Msg('FParamsSetByGui=' + FParamsSetByGui.ToString);
end;

procedure TRipGrepperSearchDialogForm.StoreHistoriesAsCmbEntries;
begin
	TItemInserter.AddTextToItemsIfNotContains(cmbOptions);
	if cmbSearchDir.Enabled then begin
		TItemInserter.AddTextToItemsIfNotContains(cmbSearchDir);
	end;
	TItemInserter.AddTextToItemsIfNotContains(cmbSearchText);
	TItemInserter.AddTextToItemsIfNotContains(cmbReplaceText);
	TItemInserter.AddTextToItemsIfNotContains(cmbFileMasks);
end;

procedure TRipGrepperSearchDialogForm.WriteCtrlsToRipGrepParametersSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.WriteCtrlsToRipGrepParametersSettings');

	dbgMsg.Msg('FParamsSetByGui=' + FParamsSetByGui.ToString);
	FParamsSetByGui.SearchText := cmbSearchText.Text;
	FParamsSetByGui.IsReplaceMode := IsReplaceMode;
	if IsReplaceMode then begin
		SetReplaceTextSetting(cmbReplaceText.Text);
		FParamsSetByGui.SetRgOptionWithValue(RG_PARAM_REGEX_REPLACE, FParamsSetByGui.ReplaceText, True);
	end else begin
		FParamsSetByGui.SetRgOption(RG_PARAM_REGEX_REPLACE, True);
		FParamsSetByGui.ReplaceText := '';
	end;
	dbgMsg.Msg('ReplaceText=' + FParamsSetByGui.ReplaceText);

	FSettings.RipGrepParameters.SearchPath := cmbSearchDir.Text;
	dbgMsg.Msg('SearchPath=' + cmbSearchDir.Text);

	FSettings.RipGrepParameters.FileMasks := cmbFileMasks.Text;
	dbgMsg.Msg('FileMasks=' + cmbFileMasks.Text);

	FParamsSetByGui.SetRgOption(RG_PARAM_REGEX_HIDDEN, not cbRgParamHidden.Checked);
	FParamsSetByGui.SetRgOption(RG_PARAM_REGEX_NO_IGNORE, not cbRgParamNoIgnore.Checked);

	FParamsSetByGui.SetRgOption(RG_PARAM_REGEX_PRETTY, not(cbRgParamPretty.Enabled and cbRgParamPretty.Checked));
	if cbRgParamContext.Checked then begin
		if seContextLineNum.Text = '' then begin
			seContextLineNum.Text := '0';
		end;
		FParamsSetByGui.SetRgOptionWithValue(RG_PARAM_REGEX_CONTEXT, seContextLineNum.Text, True);
	end else begin
		FParamsSetByGui.SetRgOption(RG_PARAM_REGEX_CONTEXT, True);
	end;

	if cbRgParamEncoding.Checked then begin
		if cmbRgParamEncoding.Text = '' then begin
			cmbRgParamEncoding.Text := cmbRgParamEncoding.Items[0];
		end;
		FSettings.SearchFormSettings.Encoding := cmbRgParamEncoding.Text;
		FParamsSetByGui.SetRgOptionWithValue(RG_PARAM_REGEX_ENCODING, cmbRgParamEncoding.Text, True);
	end else begin
		FParamsSetByGui.SetRgOption(RG_PARAM_REGEX_ENCODING, True);
		FSettings.SearchFormSettings.Encoding := '';
	end;

	if Fsettings.AppSettings.ExpertMode then begin
		WriteOptionCtrlToRipGrepParametersSetting;
	end;

	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.WriteCtrlsToRipGrepParametersSettings: end ' + FParamsSetByGui.ToString);
end;

procedure TRipGrepperSearchDialogForm.WriteCtrlsToSettings(const _bDefaultOnly : Boolean = False);
begin
	StoreHistoriesAsCmbEntries();

	FSettings.SearchPathsHistory := cmbSearchDir.Items;
	FSettings.SearchTextsHistory := cmbSearchText.Items;
	FSettings.ReplaceTextsHistory := cmbReplaceText.Items;
	FSettings.ExpertOptionHistory := cmbOptions.Items;
	FSettings.FileMasksHistory := cmbFileMasks.Items;

	WriteCtrlsToRipGrepParametersSettings;
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.WriteCtrlsToSettings: set GuiSearchTextParams=' + FParamsSetByGui.ToString);
	FSettings.RipGrepParameters.GuiSearchTextParams.Copy(FParamsSetByGui);

	if _bDefaultOnly then begin
		FSettings.RipGrepParameters.GuiSearchTextParams.StoreAsDefaultsToDict;
		FSettings.SearchFormSettings.StoreAsDefaultsToDict;
		FSettings.CopyDefaultsToValues();
	end;

	FSettings.RebuildArguments();
end;

procedure TRipGrepperSearchDialogForm.UpdateCheckBoxesByRgOptions;
var
	sVal : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.UpdateCheckBoxesByRgOptions');

	FCbClickEventEnabled := False;
	try
		cbRgParamHidden.Checked := IsOptionSet(RG_PARAM_REGEX_HIDDEN);
		cbRgParamNoIgnore.Checked := IsOptionSet(RG_PARAM_REGEX_NO_IGNORE);
		cbRgParamPretty.Checked := IsOptionSet(RG_PARAM_REGEX_PRETTY);

		cbRgParamContext.Checked := FParamsSetByGui.RgOptions.GetOptionValue(RG_PARAM_REGEX_CONTEXT, sVal);
		seContextLineNum.Enabled := cbRgParamContext.Checked;
		seContextLineNum.Text := IfThen(seContextLineNum.Enabled, sVal, '0');

		cbRgParamEncoding.Checked := FParamsSetByGui.RgOptions.GetOptionValue(RG_PARAM_REGEX_ENCODING, sVal);
		cmbRgParamEncoding.Enabled := cbRgParamEncoding.Checked;
		cmbRgParamEncoding.Text := IfThen(cmbRgParamEncoding.Enabled, sVal, '');

		sVal := '';
		var
		bReplaceMode := FParamsSetByGui.RgOptions.GetOptionValue(RG_PARAM_REGEX_REPLACE, sVal);
		TabControl1.TabIndex := IfThen(bReplaceMode, 1, 0);
		cmbReplaceText.Text := TOptionStrings.MaybeDeQuoteIfQuoted(sVal);

	finally
		FCbClickEventEnabled := True;
	end;
	dbgMsg.MsgFmt('Hidden %s NoIgnore %s Pretty %s',
		{ } [BoolToStr(cbRgParamHidden.Checked),
		{ } BoolToStr(cbRgParamNoIgnore.Checked),
		{ } BoolToStr(cbRgParamPretty.Checked)]);
end;

procedure TRipGrepperSearchDialogForm.UpdateCheckBoxesBySettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.UpdateCheckBoxesBySettings');

	FCbClickEventEnabled := False;
	try
		WriteSearchFormSettingsToCtrls();
	finally
		FCbClickEventEnabled := True;
	end;
	dbgMsg.MsgFmt('cbHidden %s cbNoIgnore %s cbPretty %s',
		{ } [BoolToStr(cbRgParamHidden.Checked),
		{ } BoolToStr(cbRgParamNoIgnore.Checked),
		{ } BoolToStr(cbRgParamPretty.Checked)]);
end;

procedure TRipGrepperSearchDialogForm.UpdateMemoCommandLine(const _bSkipReadCtrls : Boolean = False);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.UpdateMemoCommandLine');
	dbgMsg.Msg('FParamsSetByGui= ' + FParamsSetByGui.ToLogString);

	if not _bSkipReadCtrls then begin
		dbgMsg.Msg('not SkipReadCtrls');
		WriteCtrlsToRipGrepParametersSettings;
	end;

	FSettings.RipGrepParameters.GuiSearchTextParams.Copy(FParamsSetByGui);
	FSettings.RebuildArguments();
	memoCommandLine.Text := FSettings.RipGrepParameters.GetCommandLine();
	FParamsSetByGui.Copy(FSettings.RipGrepParameters.GuiSearchTextParams);
	dbgMsg.Msg('FParamsSetByGui= ' + FParamsSetByGui.ToLogString);
end;

procedure TRipGrepperSearchDialogForm.UpdateCtrls(_ctrlChanged : TControl);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.UpdateCtrls');
	dbgMsg.Msg('ctrl ' + _ctrlChanged.Name);

	if cmbSearchText = _ctrlChanged then begin
		UpdateMemoCommandLine(); // UpdateCtrls
	end else if (cmbReplaceText = _ctrlChanged)
	{ } or (TabControl1 = _ctrlChanged) then begin
		UpdateMemoCommandLine(); // UpdateCtrls
	end else if cmbSearchDir = _ctrlChanged then begin
		UpdateMemoCommandLine(); // UpdateCtrls
	end else if cmbFileMasks = _ctrlChanged then begin
		UpdateFileMasksInOptions();
		UpdateMemoCommandLine(); // UpdateCtrls
	end else if (cbRgParamHidden = _ctrlChanged)
	{ } or (cbRgParamNoIgnore = _ctrlChanged)
	{ } or (cbRgParamPretty = _ctrlChanged)
	{ } or (cbRgParamContext = _ctrlChanged)
	{ } or (seContextLineNum = _ctrlChanged)
	{ } or (cbRgParamEncoding = _ctrlChanged)
	{ } or (cmbRgParamEncoding = _ctrlChanged)
	{ } then begin
		UpdateMemoCommandLine(); // this should be done first! UpdateCtrls
	end else if cmbOptions = _ctrlChanged then begin
		UpdateMemoCommandLine(); // this should be done first! UpdateCtrls
		UpdateFileMasksInFileMasks();
		UpdateCheckBoxesByRgOptions(); // UpdateCtrs(cmbControls)
	end;
end;

procedure TRipGrepperSearchDialogForm.cbRgParamEncodingClick(Sender : TObject);
begin
	if not FCbClickEventEnabled then
		Exit;

	cmbRgParamEncoding.Enabled := cbRgParamEncoding.Checked;
	FSettings.SearchFormSettings.Encoding := IfThen(cmbRgParamEncoding.Enabled, cmbRgParamEncoding.Text);
	UpdateCtrls(cbRgParamEncoding);
end;

function TRipGrepperSearchDialogForm.CheckAndCorrectMultiLine(const _str : TMultiLineString) : string;
begin
	Result := '';
	if (_str.IsMultiLine) then begin
		TMsgBox.ShowWarning('Multiline string not supported.' + CRLF + 'Only first line will be searched.');
		// Save in ini not implemented for multiline strings
	end;
	Result := _str.GetLine(0);
end;

procedure TRipGrepperSearchDialogForm.CheckVsCodeRipGrep;
var
	sRgPath : string;
	sVsDir : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.CheckVsCodeRipGrep');

	sVsDir := TFileUtils.GetVsCodeDir;
	if not sVsDir.IsEmpty then begin
		sVsDir := TFileUtils.ShortToLongPath(sVsDir.Remove(sVsDir.Length - '\bin'.Length));

		TFileUtils.FindExecutable(FSettings.RipGrepParameters.RipGrepPath, sRgPath);
		// Rg in VSCode doesn't support --pretty
		cbRgParamPretty.Enabled := not TFileUtils.ShortToLongPath(sRgPath).StartsWith(sVsDir, True);
		if not cbRgParamPretty.Enabled then begin
			lblHintHelper.Caption := '';
			lblHintHelper.AutoSize := False;
			lblHintHelper.SetBounds(cbRgParamPretty.BoundsRect.Left, cbRgParamPretty.BoundsRect.Top, cbRgParamPretty.BoundsRect.Width,
				cbRgParamPretty.BoundsRect.Height);
			lblHintHelper.Hint := 'rg.exe in VSCode doesn''t support --pretty';
			lblHintHelper.ShowHint := True;
			lblHintHelper.Visible := True;
			dbgMsg.Msg(lblHintHelper.Hint);
		end;
	end;
end;

procedure TRipGrepperSearchDialogForm.cmbOptionsChange(Sender : TObject);
begin
	UpdateCtrls(cmbOptions);
end;

procedure TRipGrepperSearchDialogForm.cmbReplaceTextChange(Sender : TObject);
begin
	cmbReplaceText.Text := CheckAndCorrectMultiLine(cmbReplaceText.Text);
	UpdateCtrls(cmbReplaceText);
end;

procedure TRipGrepperSearchDialogForm.cmbRgParamEncodingChange(Sender : TObject);
begin
	FSettings.SearchFormSettings.Encoding := IfThen(cmbRgParamEncoding.Enabled, cmbRgParamEncoding.Text);
	UpdateCtrls(cmbRgParamEncoding);
end;

procedure TRipGrepperSearchDialogForm.FormResize(Sender : TObject);
begin
	inherited;
	// var ctrlBackup := ActiveControl;
	// ActiveControl := nil;
	SetExpertGroupSize();
	// ActiveControl := ctrlBackup; // TODO: after resize every edit is selected
end;

function TRipGrepperSearchDialogForm.GetFullHeights() : integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.GetFullHeights()');
	Result :=
	{ } GetFullHeight(pnlTop) +
	{ } pnlMiddle.Margins.Top +
	{ } GetFullHeight(gbOptionsFilters) +
	{ } GetFullHeight(gbOptionsOutput) +
	{ } pnlMiddle.Margins.Bottom +
	{ } GetFullHeight(pnlBottom);
	dbgMsg.Msg('Result=' + Result.ToString);
end;

function TRipGrepperSearchDialogForm.HasHistItemObjWithResult : Boolean;
begin
	Result := Assigned(FHistItemObj) and (FHistItemObj.HasResult);
end;

procedure TRipGrepperSearchDialogForm.rbExtensionOptionsClick(Sender : TObject);
begin
	{$IFDEF STANDALONE}
	var
	bStandalone := True;
	{$ELSE}
	var
	bStandalone := False;
	{$ENDIF}
	if bStandalone or FbExtensionOptionsSkipClick then begin
		Exit;
	end;
	UpdateCmbsOnIDEContextChange();
	WriteCtrlsToRipGrepParametersSettings();
	UpdateCmbOptionsAndMemoCommandLine();
end;

procedure TRipGrepperSearchDialogForm.seContextLineNumChange(Sender : TObject);
begin
	FSettings.SearchFormSettings.Context := IfThen(seContextLineNum.Enabled, seContextLineNum.Value);
	UpdateCtrls(seContextLineNum);
end;

function TRipGrepperSearchDialogForm.GetInIDESelectedText : string;
var
	selectedText : TMultiLineString;
begin
	{$IFNDEF STANDALONE}
	IOTAUtils.GxOtaGetActiveEditorTextAsMultilineString(selectedText, True);
	{$ENDIF}
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.GetInIDESelectedText: ' + selectedText);

	selectedText := string(selectedText).Trim();
	Result := CheckAndCorrectMultiLine(selectedText);
end;

function TRipGrepperSearchDialogForm.IsReplaceMode : Boolean;
begin
	Result := TabControl1.TabIndex = 1;
end;

procedure TRipGrepperSearchDialogForm.LoadDefaultSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.LoadDefaultSettings');
	// FSettings.LoadDefault;

	// TODO set only if it was saved before!
	SetCmbSearchPathText(IfThen(FSettings.RipGrepParameters.SearchPath.IsEmpty, cmbSearchDir.Text, FSettings.RipGrepParameters.SearchPath));
	dbgMsg.Msg('cmbSearchDir=' + cmbSearchDir.Text);
	{$IFDEF STANDALONE}
	var
	bStandalone := True;
	{$ELSE}
	var
	bStandalone := False;
	{$ENDIF}
	if not bStandalone then begin
		var
		cic := FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext;
		dbgMsg.Msg('IDEContext=' + cic.ToLogString);
		rbExtensionOptions.ItemIndex := Integer(cic.IDEContext);
		UpdateCmbsOnIDEContextChange();
		dbgMsg.Msg('cmbSearchDir=' + cmbSearchDir.Text);
	end;

	cmbFileMasks.Text := IfThen(FSettings.RipGrepParameters.FileMasks.IsEmpty, cmbFileMasks.Text, FSettings.RipGrepParameters.FileMasks);
	dbgMsg.Msg('cmbFileMasks.Text=' + cmbFileMasks.Text);
	FParamsSetByGui.SearchOptions := FSettings.RipGrepParameters.GuiSearchTextParams.SearchOptions;
	UpdateCheckBoxesBySettings();
end;

procedure TRipGrepperSearchDialogForm.LoadExtensionSearchSettings;
var
	extSearchSettings : TRipGrepperExtensionContext;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.LoadExtensionSearchSettings');
	{$IFDEF STANDALONE}
	var
	bStandalone := True;
	{$ELSE}
	var
	bStandalone := False;
	{$ENDIF}
	if bStandalone then begin
		Exit;
	end;

	dbgMsg.MsgFmt('ExtensionSettings.IsAlreadyRead=%s', [
		{ } BoolToStr(FSettings.SearchFormSettings.ExtensionSettings.IsAlreadyRead)]);
	var
	selectedText := GetInIDESelectedText;

	{$IFNDEF STANDALONE}
	extSearchSettings := FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext;
	if not HasHistItemObjWithResult then begin
		if not selectedText.IsEmpty then begin
			cmbSearchText.Text := selectedText;
			dbgMsg.Msg('SelectedText=' + selectedText);
		end;
	end;
	extSearchSettings.ActiveFile := IOTAUTils.GxOtaGetCurrentSourceFile();
	dbgMsg.Msg('ActiveFile=' + extSearchSettings.ActiveFile);

	extSearchSettings.ProjectFiles := IOTAUTils.GetProjectFiles();
	dbgMsg.MsgFmt('ProjectFiles.Count=', [Length(extSearchSettings.ProjectFiles)]);
	extSearchSettings.OpenFiles := IOTAUTils.GetOpenedEditBuffers();
	dbgMsg.MsgFmt('OpenFiles.Count=', [Length(extSearchSettings.OpenFiles)]);
	var
	ap := (IOTAUTils.GxOtaGetCurrentProject);
	if Assigned(ap) then begin
		extSearchSettings.ActiveProject := ap.FileName;
		dbgMsg.Msg('ActiveProject=' + extSearchSettings.ActiveProject);
	end;

	dbgMsg.Msg('CurrentIDEContext:' + extSearchSettings.ToLogString);
	FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext := extSearchSettings;
	{$ENDIF}
	FbExtensionOptionsSkipClick := True;
	rbExtensionOptions.ItemIndex := Integer(extSearchSettings.IDEContext);
	UpdateCmbsOnIDEContextChange();
	FbExtensionOptionsSkipClick := False;
end;

procedure TRipGrepperSearchDialogForm.LoadInitialSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.LoadInitialSettings');
	if HasHistItemObjWithResult then begin
		FParamsSetByGui := FHistItemObj.GuiSearchTextParams;
		if Assigned(FHistItemObj.SearchFormSettings) then begin
			FOrigSearchFormSettings := TSearchFormSettings.Create;
			dbgMsg.Msg('Hist: ' + FHistItemObj.SearchFormSettings.ToLogString);

			FOrigSearchFormSettings.Copy(FHistItemObj.SearchFormSettings);
			FSettings.SearchFormSettings.Copy(FHistItemObj.SearchFormSettings);
			FSettings.LoadFromDict();
		end;
	end else begin
		if (not FSettings.SearchFormSettings.IsAlreadyRead) then begin
			dbgMsg.ErrorMsg('SearchFormSettings.IsAlreadyRead');
		end;
		FSettings.CopyDefaultsToValues;

		FParamsSetByGui := TGuiSearchTextParams.Create(TRipGrepParameterSettings.INI_SECTION);
		FParamsSetByGui.ReadIni;
		FParamsSetByGui.LoadDefaultsFromDict;
	end;
end;

procedure TRipGrepperSearchDialogForm.SetCmbSearchPathText(const _sPath : string);
begin
	cmbSearchDir.Text := _sPath;
	TDebugUtils.Msg('cmbSearchDir.Text=' + cmbSearchDir.Text);
end;

procedure TRipGrepperSearchDialogForm.SetExpertGroupSize();
begin
	var
	iexpertHeight := Height - GetFullHeights();
	gbExpert.Visible := FSettings.AppSettings.ExpertMode and (iexpertHeight > 0);
	if gbExpert.Visible then begin
		gbExpert.Top := gbOptionsOutput.Top + gbOptionsOutput.Height + gbOptionsOutput.Margins.Bottom;
		gbExpert.Height := iexpertHeight;
	end;
end;

class procedure TRipGrepperSearchDialogForm.SetReplaceText(_settings : TRipGrepperSettings; const _replaceText : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.SetReplaceText');

	_settings.LastReplaceText := _replaceText;
	if _settings.IsReplaceMode and _replaceText.IsEmpty then begin
		_settings.LastReplaceText := QuotedStr('');
	end;
	dbgMsg.Msg('LastReplaceText=' + _settings.LastReplaceText);
end;

procedure TRipGrepperSearchDialogForm.SetReplaceTextSetting(const _replaceText : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.SetReplaceText');

	FParamsSetByGui.ReplaceText := _replaceText;
	if FParamsSetByGui.IsReplaceMode and _replaceText.IsEmpty then begin
		FParamsSetByGui.ReplaceText := QuotedStr('');
	end;
	dbgMsg.Msg('LastReplaceText=' + FParamsSetByGui.ReplaceText);
end;

procedure TRipGrepperSearchDialogForm.ShowReplaceCtrls(const _bShow : Boolean);
begin
	cmbReplaceText.Visible := _bShow;
	ActionShowRGReplaceOptionHelp.Visible := _bShow;
end;

class function TRipGrepperSearchDialogForm.ShowSearchForm(_owner : TComponent; _settings : TRipGrepperSettings;
	_histObj : IHistoryItemObject) : integer;
var
	frm : TRipGrepperSearchDialogForm;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.ShowSearchForm');
	frm := TRipGrepperSearchDialogForm.Create(_owner, _settings, _histObj);
	try
		Result := frm.ShowModal();
		if mrOk = Result then begin
			_settings.LastSearchText := frm.cmbSearchText.Text;
			SetReplaceText(_settings, frm.cmbReplaceText.Text);
		end else begin
			_settings.LastSearchText := _histObj.SearchText;
			SetReplaceText(_settings, _histObj.ReplaceText);
			dbgMsg.MsgFmtIf(_histObj.SearchText <> _histObj.GuiSearchTextParams.SearchText,
				{ } 'ERROR? _histObj.SearchText=%s <> GuiSearchTextParams=%s',
				[_histObj.SearchText, _histObj.GuiSearchTextParams.SearchText]);
		end;
		dbgMsg.Msg('LastSearchText=' + _settings.LastSearchText);
	finally
		frm.Free;
	end;
end;

procedure TRipGrepperSearchDialogForm.TabControl1Change(Sender : TObject);
begin
	ShowReplaceCtrls(IsReplaceMode());
	UpdateHeight;
	UpdateCtrls(TabControl1);
end;

procedure TRipGrepperSearchDialogForm.UpdateButtonsBySettings;
var
	s : string;
begin
	ButtonDown(EGuiOption.soMatchCase, tbIgnoreCase);
	ButtonDown(EGuiOption.soMatchWord, tbMatchWord);
	if tbMatchWord.Down then begin
		s := cmbSearchText.Text;
		TCommandLineBuilder.RemoveWordBoundaries(s);
		cmbSearchText.Text := s;
	end;
	ButtonDown(EGuiOption.soUseRegex, tbUseRegex);
end;

procedure TRipGrepperSearchDialogForm.UpdateCmbsOnIDEContextChange;
var
	rgec : TRipGrepperExtensionContext;
begin
	{$IFDEF STANDALONE}
	var
	bStandalone := True;
	{$ELSE}
	var
	bStandalone := False;
	{$ENDIF}
	if not bStandalone then begin
		var
		dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.UpdateCmbsOnIDEContextChange');

		rgec := FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext;
		dbgMsg.Msg(rgec.ToLogString);

		cmbSearchDir.Enabled := False;
		case rbExtensionOptions.ItemIndex of
			EXT_SEARCH_ACTIVE_FILE : begin
				SetCmbSearchPathText(rgec.ActiveFile);
			end;
			EXT_SEARCH_PROJECT_FILES : begin
				SetCmbSearchPathText(string.Join(SEARCH_PATH_SEPARATOR, rgec.ProjectFiles).Trim([SEARCH_PATH_SEPARATOR]));
			end;
			EXT_SEARCH_OPEN_FILES : begin
				SetCmbSearchPathText(string.Join(SEARCH_PATH_SEPARATOR, rgec.OpenFiles).Trim([SEARCH_PATH_SEPARATOR]));
			end;
			EXT_SEARCH_GIVEN_PATH : begin
				cmbSearchDir.Enabled := True;
				SetComboItemsAndText(cmbSearchDir, RG_ARG_SEARCH_PATH, FSettings.SearchPathsHistory, SEARCH_PATH_SEPARATOR);
			end;
			else begin
				raise Exception.Create('Extension IDE Context not supported');
			end;
		end;
		FSettings.RipGrepParameters.SearchPath := cmbSearchDir.Text;
		rgec.IDEContext := ERipGrepperExtensionContext(rbExtensionOptions.ItemIndex);
		FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext := rgec;
		dbgMsg.Msg(rgec.ToLogString);
	end;
end;

procedure TRipGrepperSearchDialogForm.UpdateFileMasksInFileMasks;
begin
	cmbFileMasks.Text := TCommandLineBuilder.GetFileMasksDelimited(FParamsSetByGui.RgOptions.AsString);
end;

procedure TRipGrepperSearchDialogForm.UpdateFileMasksInOptions;
begin
	FParamsSetByGui.RgOptions.UpdateFileMasks(cmbFileMasks.Text);
	FSettings.RipGrepParameters.FileMasks := cmbFileMasks.Text;
end;

procedure TRipGrepperSearchDialogForm.UpdateHeight;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.UpdateHeight');
	// Height := FOrigHeight;
	dbgMsg.Msg('Height=' + Height.ToString);
	if cmbReplaceText.Visible then begin
		pnlTop.Height := FTopPanelOrigHeight;
	end else begin
		pnlTop.Height := FTopPanelOrigHeight - GetFullHeight(cmbReplaceText);
	end;
	dbgMsg.Msg('pnlTop.Height=' + pnlTop.Height.ToString);
	pnlMiddle.Top := pnlTop.Height;
	{$IFDEF STANDALONE}
	var
	bStandalone := True;
	{$ELSE}
	var
	bStandalone := False;
	{$ENDIF}
	if bStandalone then begin
		rbExtensionOptions.Enabled := False;
		rbExtensionOptions.Visible := False;
		var
		extensionSpace := GetFullHeight(rbExtensionOptions);
		dbgMsg.Msg('extensionSpace=' + extensionSpace.ToString);

		gbOptionsFilters.Height := FOptionsFiltersOrigHeight - extensionSpace;
		gbOptionsOutput.Top := FOptionsOutputOrigTop - extensionSpace;

		pnlMiddle.Height := FpnlMiddleOrigHeight - extensionSpace;
		dbgMsg.Msg('pnlMiddle.Height=' + pnlMiddle.Height.ToString);
	end;

	var
	iHeight := GetFullHeights;
	Constraints.MinHeight := iHeight;

	if FSettings.AppSettings.ExpertMode then begin
		iHeight := iHeight + gbExpert.Height; // expertHeight will be changed by parent height.
	end;

	Height := iHeight;
	dbgMsg.Msg('Height=' + Height.ToString);
	SetExpertGroupSize();
end;

procedure TRipGrepperSearchDialogForm.WriteOptionCtrlToRipGrepParametersSetting;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.WriteOptionCtrlToRipGrepParametersSetting');
	FParamsSetByGui.ExpertOptions := TOptionStrings.New(cmbOptions.Text);
	dbgMsg.Msg('FParamsSetByGui.ExpertOptions=' + FParamsSetByGui.ExpertOptions.AsString);
end;

procedure TRipGrepperSearchDialogForm.WriteSearchFormSettingsToCtrls;
var
	searchFormSettings : TSearchFormSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.WriteSearchFormSettingsToCtrls');

	TabControl1.TabIndex := IfThen(FParamsSetByGui.IsReplaceMode, 1, 0);
	cmbReplaceText.Text := FParamsSetByGui.ReplaceText;

	searchFormSettings := FSettings.SearchFormSettings;

	dbgMsg.MsgFmt('searchFormSettings(%p).IsAlreadyRead=%s', [@searchFormSettings, BoolToStr(searchFormSettings.IsAlreadyRead)]);
	cbRgParamHidden.Checked := searchFormSettings.Hidden;
	dbgMsg.MsgFmt('cbRgParamHidden.Checked %s', [BoolToStr(cbRgParamHidden.Checked)]);
	cbRgParamNoIgnore.Checked := searchFormSettings.NoIgnore;
	dbgMsg.MsgFmt('cbRgParamNoIgnore.Checked %s', [BoolToStr(cbRgParamNoIgnore.Checked)]);
	cbRgParamPretty.Checked := searchFormSettings.Pretty;
	dbgMsg.MsgFmt('cbRgParamPretty.Checked %s', [BoolToStr(cbRgParamPretty.Checked)]);

	cbRgParamContext.Checked := searchFormSettings.Context <> 0;
	seContextLineNum.Enabled := cbRgParamContext.Checked;
	seContextLineNum.Value := searchFormSettings.Context;
	dbgMsg.MsgFmt('seContextLineNum.Value=%d', [seContextLineNum.Value]);

	cbRgParamEncoding.Checked := searchFormSettings.Encoding <> '';
	cmbRgParamEncoding.Enabled := cbRgParamEncoding.Checked;
	cmbRgParamEncoding.Text := searchFormSettings.Encoding;
	dbgMsg.Msg('cmbRgParamEncoding.Text=' + cmbRgParamEncoding.Text);
end;

end.
