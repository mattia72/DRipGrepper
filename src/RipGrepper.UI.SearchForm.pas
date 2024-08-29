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
	RipGrepper.Common.Settings.RipGrepperSettings,
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
	RipGrepper.Common.Settings.RipGrepParameterSettings,
	Vcl.Samples.Spin,
	RipGrepper.Helper.Types,
	RipGrepper.Common.Settings.RipGrepperSearchFormSettings,
    RipGrepper.Common.Interfaces;

type
	TRipGrepperSearchDialogForm = class(TForm)
		pnlSearch : TPanel;
		gbSearch : TGroupBox;
		lblParams : TLabel;
		lblPaths : TLabel;
		lblText : TLabel;
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
		ToolBar1 : TToolBar;
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
		procedure cmbRgParamEncodingChange(Sender : TObject);
		procedure cmbSearchDirChange(Sender : TObject);
		procedure cmbSearchTextChange(Sender : TObject);
		procedure ShowOptionsForm;
		procedure FormClose(Sender : TObject; var Action : TCloseAction);
		procedure FormShow(Sender : TObject);
		procedure ToggleExpertMode;
		procedure rbExtensionOptionsClick(Sender : TObject);
		procedure seContextLineNumChange(Sender : TObject);

		private
			FbExtensionOptionsSkipClick : Boolean;
			FHistItemObj : IHistoryItemObject;
			FDpiScaler : TRipGrepperDpiScaler;
			FExpertGroupHeight : Integer;
			FGuiSetSearchParams : TGuiSearchTextParams;
			FCbClickEventEnabled : Boolean;
			FcmbOptionsOldText : string;
			FOrigRipGrepperSearchFormSettings : TRipGrepperSearchFormSettings;
			FSettings : TRipGrepperSettings;

			function GetSelectedPaths(const _fdo : TFileDialogOptions) : string;
			procedure LoadSettings;
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
			procedure UpdateCheckBoxesBySettings(const _searchFormSettings : TRipGrepperSearchFormSettings);
			procedure AlignExpertGroupBox;
			function CheckAndCorrectMultiLine(const _str : TMultiLineString) : string;
			procedure ChecVsCodeRipGrep;
			function HasHistItemObj : Boolean;
			function GetInIDESelectedText : string;
			procedure LoadExtensionSearchSettings;
			procedure UpdateButtonsBySettings;
			procedure UpdateCmbsOnContextChange;
			procedure UpdateFileMasksInFileMasks;
			function UpdateFileMasksInOptions(const sOptions, sMasks : string) : string; overload;
			procedure UpdateFileMasksInOptions; overload;
			procedure UpdateHeight;
			procedure WriteOptionCtrlToRipGrepParametersSetting;
			procedure WriteSearchFormSettingsToCtrls(const _searchFormSettings : TRipGrepperSearchFormSettings);

		public
			constructor Create(AOwner : TComponent; const _settings : TRipGrepperSettings; const _histObj :IHistoryItemObject); reintroduce; virtual;
			destructor Destroy; override;
			procedure LoadDefaultSettings;

			class function ShowSearchForm(_owner : TComponent; _settings : TRipGrepperSettings; _histObj : IHistoryItemObject): integer;

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
	RipGrepper.Common.IOTAUtils,
	RipGrepper.Tools.FileUtils,
	System.IOUtils,
	Winapi.Windows,
	RipGrepper.Common.Settings.Misc;

{$R *.dfm}

constructor TRipGrepperSearchDialogForm.Create(AOwner : TComponent; const _settings : TRipGrepperSettings;
	const _histObj :IHistoryItemObject);
begin
	inherited Create(AOwner);
	FSettings := _settings;
	FHistItemObj := _histObj;
	if HasHistItemObj then begin
		TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.Create: set hist obj');
		FGuiSetSearchParams := FHistItemObj.GuiSearchTextParams;
		if Assigned(FHistItemObj.RipGrepperSearchFormSettings) then begin
			FOrigRipGrepperSearchFormSettings := TRipGrepperSearchFormSettings.Create;
			FOrigRipGrepperSearchFormSettings.Copy(FHistItemObj.RipGrepperSearchFormSettings);
			FSettings.RipGrepperSearchFormSettings.Copy(FHistItemObj.RipGrepperSearchFormSettings);
		end;
	end else begin
		FGuiSetSearchParams := TGuiSearchTextParams.Create();
	end;

	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.Create: gui params=' + FGuiSetSearchParams.GetAsString);
	FDpiScaler := TRipGrepperDpiScaler.Create(self);
	FExpertGroupHeight := gbExpert.Height;
	cmbOptions.AutoComplete := False; // so we know the old value after change
end;

destructor TRipGrepperSearchDialogForm.Destroy;
begin
	if not HasHistItemObj then begin
		FGuiSetSearchParams.Free;
	end;
	FOrigRipGrepperSearchFormSettings.Free;
	FDpiScaler.Free;
	inherited Destroy;
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamMatchCaseExecute(Sender : TObject);
begin
	FGuiSetSearchParams.SetOrReset(EGuiOption.soMatchCase);
	UpdateCmbOptionsAndMemoCommandLine();
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamMatchCaseUpdate(Sender : TObject);
begin
	ButtonDown(EGuiOption.soMatchCase, tbIgnoreCase);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamRegexExecute(Sender : TObject);
begin
	FGuiSetSearchParams.SetOrReset(EGuiOption.soUseRegex);
	UpdateCmbOptionsAndMemoCommandLine();
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamRegexUpdate(Sender : TObject);
begin
	ButtonDown(EGuiOption.soUseRegex, tbUseRegex);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamWordExecute(Sender : TObject);
begin
	FGuiSetSearchParams.SetOrReset(EGuiOption.soMatchWord);
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
		cmbSearchDir.Text := selectedDirs;
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
		cmbSearchDir.Text := selectedFiles;
		UpdateCtrls(cmbSearchDir);
	end;
end;

procedure TRipGrepperSearchDialogForm.ActionSetAsDefaultExecute(Sender : TObject);
begin
	WriteCtrlsToSettings(True);

	FSettings.StoreAsDefault();
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
			cmbOptions.Text := TGuiSearchTextParams.RemoveRgExeOptions(
				{ } FSettings.RipGrepParameters.RgExeOptions, string.Join('|', RG_NECESSARY_PARAMS + RG_GUI_SET_PARAMS));
			UpdateCtrls(cmbOptions);
		end;
	finally
		frm.Free;
	end;
end;

procedure TRipGrepperSearchDialogForm.FormClose(Sender : TObject; var Action : TCloseAction);
begin
	if HasHistItemObj then begin
		FHistItemObj.RipGrepperSearchFormSettings.Copy(FSettings.RipGrepperSearchFormSettings);
		FHistItemObj.RipGrepArguments.Clear;
		FHistItemObj.RipGrepArguments.Assign(FSettings.GetRipGrepArguments());
		FHistItemObj.GuiSearchTextParams.Copy(FGuiSetSearchParams);
		FSettings.RipGrepperSearchFormSettings.Copy(FOrigRipGrepperSearchFormSettings);
	end;
	FSettings.UpdateIniFile;
end;

procedure TRipGrepperSearchDialogForm.FormShow(Sender : TObject);
begin
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.FormShow');

	LoadSettings;
	LoadExtensionSearchSettings;

	if not HasHistItemObj then begin
		LoadDefaultSettings();
	end;

	ChecVsCodeRipGrep;

	WriteCtrlsToRipGrepParametersSettings;

	UpdateCmbOptionsAndMemoCommandLine;
	UpdateCheckBoxesByRgOptions();

	AlignExpertGroupBox();
	UpdateHeight();
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

procedure TRipGrepperSearchDialogForm.LoadSettings;
begin
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.LoadSettings');

	SetComboItemsAndText(cmbSearchDir, RG_ARG_SEARCH_PATH, FSettings.SearchPathsHistory, ';');
	SetComboItemsAndText(cmbSearchText, RG_ARG_SEARCH_TEXT, FSettings.SearchTextsHistory);
	SetComboItemsFromOptions(cmbFileMasks, RG_PARAM_REGEX_GLOB, FSettings.FileMasksHistory);
	// Set available encodings...
	SetComboItemsAndText(cmbRgParamEncoding, RG_PARAM_REGEX_ENCODING, FSettings.RipGrepperSettings.EncodingItems);

	UpdateButtonsBySettings;

	UpdateCheckBoxesBySettings(FSettings.RipGrepperSearchFormSettings);

	SetComboItemsAndText(cmbOptions, RG_ARG_OPTIONS, FSettings.RipGrepOptionsHistory);
	FcmbOptionsOldText := cmbOptions.Text;
end;

procedure TRipGrepperSearchDialogForm.ButtonDown(const _searchOption : EGuiOption; _tb : TToolButton; const _bNotMatch : Boolean = False);
begin
	if (_bNotMatch) then begin
		_tb.Down := not(_searchOption in FGuiSetSearchParams.SearchOptions);
	end else begin
		_tb.Down := _searchOption in FGuiSetSearchParams.SearchOptions;
	end;
end;

procedure TRipGrepperSearchDialogForm.cbRgParamContextClick(Sender : TObject);
begin
	if not FCbClickEventEnabled then
		Exit;

	seContextLineNum.Enabled := cbRgParamContext.Checked;
	FSettings.RipGrepperSearchFormSettings.Context := IfThen(seContextLineNum.Enabled, seContextLineNum.Value);
	UpdateCtrls(cbRgParamContext);
end;

procedure TRipGrepperSearchDialogForm.cbRgParamHiddenClick(Sender : TObject);
begin
	if not FCbClickEventEnabled then
		Exit;

	FSettings.RipGrepperSearchFormSettings.Hidden := cbRgParamHidden.Checked;
	UpdateCtrls(cbRgParamHidden);
end;

procedure TRipGrepperSearchDialogForm.cbRgParamNoIgnoreClick(Sender : TObject);
begin
	if not FCbClickEventEnabled then
		Exit;

	FSettings.RipGrepperSearchFormSettings.NoIgnore := cbRgParamNoIgnore.Checked;
	UpdateCtrls(cbRgParamNoIgnore);
end;

procedure TRipGrepperSearchDialogForm.cbRgParamPrettyClick(Sender : TObject);
begin
	if not FCbClickEventEnabled then
		Exit;

	FSettings.RipGrepperSearchFormSettings.Pretty := cbRgParamPretty.Enabled and cbRgParamPretty.Checked;
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
	FSettings.RipGrepperSettings.ExpertMode := not FSettings.RipGrepperSettings.ExpertMode;
	AlignExpertGroupBox();
	UpdateHeight();
end;

function TRipGrepperSearchDialogForm.GetFullHeight(_ctrl : TControl) : integer;
begin
	Result := _ctrl.Margins.Top + _ctrl.Height + _ctrl.Margins.Bottom;
end;

function TRipGrepperSearchDialogForm.IsOptionSet(const _sParamRegex : string; const _sParamValue : string = '') : Boolean;
begin
	Result := TOptionsHelper.IsOptionSet(FGuiSetSearchParams.RgOptions, _sParamRegex, _sParamValue);
end;

procedure TRipGrepperSearchDialogForm.RemoveNecessaryOptionsFromCmbOptionsText;
begin
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.RemoveNecessaryOptionsFromCmbOptionsText: start' +
		FGuiSetSearchParams.RgAdditionalOptions);

	// Remove necessary options
	cmbOptions.Text := TGuiSearchTextParams.RemoveRgExeOptions(
		{ } FGuiSetSearchParams.RgAdditionalOptions, string.Join(ARRAY_SEPARATOR, RG_NECESSARY_PARAMS + RG_GUI_SET_PARAMS));
	WriteOptionCtrlToRipGrepParametersSetting;

	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.RemoveNecessaryOptionsFromCmbOptionsText: end' +
		FGuiSetSearchParams.RgAdditionalOptions);
end;

procedure TRipGrepperSearchDialogForm.SetComboItemsAndText(_cmb : TComboBox; const _argName : string; const _items : TStrings;
	const _separator : string = ' ');
begin
	_cmb.Items.Assign(_items);
	if HasHistItemObj then begin
		_cmb.Text := string.Join(_separator, FHistItemObj.RipGrepArguments.GetValues(_argName));
	end else begin
		_cmb.ItemIndex := 0;
	end;
	TDebugUtils.DebugMessageFormat('TRipGrepperSearchDialogForm.SetComboItemsAndText: %s - %s', [_cmb.Name, _cmb.Text]);
end;

procedure TRipGrepperSearchDialogForm.SetComboItemsFromOptions(_cmb : TComboBox; const _argMaskRegex : string; const _items : TStrings);
var
	params : TArray<string>;
begin
	_cmb.Items.Assign(_items);
	if HasHistItemObj then begin
		params := FHistItemObj.RipGrepArguments.GetValues(RG_ARG_OPTIONS);
		_cmb.Text := TCommandLineBuilder.GetFileMasksDelimited(string.Join(' ', params));
	end else begin
		_cmb.Text := _cmb.Items[0];
	end;
	TDebugUtils.DebugMessageFormat('TRipGrepperSearchDialogForm.SetComboItemsFromOptions: %s - %s', [_cmb.Name, _cmb.Text]);
end;

procedure TRipGrepperSearchDialogForm.UpdateCmbOptionsAndMemoCommandLine;
begin
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.UpdateCmbOptionsAndMemoCommandLine: start ' + FGuiSetSearchParams.ToString);
	RemoveNecessaryOptionsFromCmbOptionsText;

	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.UpdateCmbOptionsAndMemoCommandLine:  ' + string.Join(' ',
		FGuiSetSearchParams.RgOptions));

	UpdateMemoCommandLine(True); // RgExeOptions may changed
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.UpdateCmbOptionsAndMemoCommandLine: end ' + FGuiSetSearchParams.ToString);
end;

procedure TRipGrepperSearchDialogForm.StoreHistoriesAsCmbEntries;
begin
	TItemInserter.AddTextToItemsIfNotContains(cmbOptions);
	if cmbSearchDir.Enabled then begin
		TItemInserter.AddTextToItemsIfNotContains(cmbSearchDir);
	end;
	TItemInserter.AddTextToItemsIfNotContains(cmbSearchText);
	TItemInserter.AddTextToItemsIfNotContains(cmbFileMasks);
end;

procedure TRipGrepperSearchDialogForm.WriteCtrlsToRipGrepParametersSettings;
begin
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.WriteCtrlsToRipGrepParametersSettings: start ' + FGuiSetSearchParams.ToString);
	FGuiSetSearchParams.SearchText := cmbSearchText.Text;
	FSettings.RipGrepParameters.SearchPath := cmbSearchDir.Text;
	FSettings.RipGrepParameters.FileMasks := cmbFileMasks.Text;

	FGuiSetSearchParams.SetRgOptions(RG_PARAM_REGEX_HIDDEN, not cbRgParamHidden.Checked);
	FGuiSetSearchParams.SetRgOptions(RG_PARAM_REGEX_NO_IGNORE, not cbRgParamNoIgnore.Checked);
	FGuiSetSearchParams.SetRgOptions(RG_PARAM_REGEX_IGNORE_CASE, cbRgParamNoIgnore.Checked);

	FGuiSetSearchParams.SetRgOptions(RG_PARAM_REGEX_PRETTY, not(cbRgParamPretty.Enabled and cbRgParamPretty.Checked));
	if cbRgParamContext.Checked then begin
		if seContextLineNum.Text = '' then begin
			seContextLineNum.Text := '0';
		end;
		FGuiSetSearchParams.SetRgOptionsWithValue(RG_PARAM_REGEX_CONTEXT, seContextLineNum.Text, True);
	end else begin
		FGuiSetSearchParams.SetRgOptions(RG_PARAM_REGEX_CONTEXT, True);
	end;

	if cbRgParamEncoding.Checked then begin
		if cmbRgParamEncoding.Text = '' then begin
			cmbRgParamEncoding.Text := cmbRgParamEncoding.Items[0];
		end;
		FSettings.RipGrepperSearchFormSettings.Encoding := cmbRgParamEncoding.Text;
		FGuiSetSearchParams.SetRgOptionsWithValue(RG_PARAM_REGEX_ENCODING, cmbRgParamEncoding.Text, True);
	end else begin
		FGuiSetSearchParams.SetRgOptions(RG_PARAM_REGEX_ENCODING, True);
		FSettings.RipGrepperSearchFormSettings.Encoding := '';
	end;

	if Fsettings.RipGrepperSettings.ExpertMode then begin
		WriteOptionCtrlToRipGrepParametersSetting;
	end;
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.WriteCtrlsToRipGrepParametersSettings: end ' + FGuiSetSearchParams.ToString);
end;

procedure TRipGrepperSearchDialogForm.WriteCtrlsToSettings(const _bDefaultOnly : Boolean = False);
begin
	StoreHistoriesAsCmbEntries();

	FSettings.SearchPathsHistory := cmbSearchDir.Items;
	FSettings.SearchTextsHistory := cmbSearchText.Items;
	FSettings.RipGrepOptionsHistory := cmbOptions.Items;
	FSettings.FileMasksHistory := cmbFileMasks.Items;

	WriteCtrlsToRipGrepParametersSettings;
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.WriteCtrlsToSettings: set GuiSearchTextParams=' + FGuiSetSearchParams.ToString);
	FSettings.RipGrepParameters.GuiSearchTextParams.Copy(FGuiSetSearchParams);

	if _bDefaultOnly then begin
		FSettings.CopyDefaultsToValues();
	end;

	FSettings.RebuildArguments();
end;

procedure TRipGrepperSearchDialogForm.UpdateCheckBoxesByRgOptions;
var
	sVal : string;
begin
	FCbClickEventEnabled := False;
	try
		cbRgParamHidden.Checked := IsOptionSet(RG_PARAM_REGEX_HIDDEN);
		cbRgParamNoIgnore.Checked := IsOptionSet(RG_PARAM_REGEX_NO_IGNORE);
		cbRgParamPretty.Checked := IsOptionSet(RG_PARAM_REGEX_PRETTY);

		cbRgParamContext.Checked := TOptionsHelper.GetOptionValueFromOptions(FGuiSetSearchParams.RgOptions, RG_PARAM_REGEX_CONTEXT, sVal);
		seContextLineNum.Enabled := cbRgParamContext.Checked;
		seContextLineNum.Text := IfThen(seContextLineNum.Enabled, sVal, '0');

		cbRgParamEncoding.Checked := TOptionsHelper.GetOptionValueFromOptions(FGuiSetSearchParams.RgOptions, RG_PARAM_REGEX_ENCODING, sVal);
		cmbRgParamEncoding.Enabled := cbRgParamEncoding.Checked;
		cmbRgParamEncoding.Text := IfThen(cmbRgParamEncoding.Enabled, sVal, '');

	finally
		FCbClickEventEnabled := True;
	end;
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.UpdateCheckBoxesByRgOptions: ' + Format('Hidden %s NoIgnore %s Pretty %s',
		[BoolToStr(cbRgParamHidden.Checked), BoolToStr(cbRgParamNoIgnore.Checked), BoolToStr(cbRgParamPretty.Checked)]));
end;

procedure TRipGrepperSearchDialogForm.UpdateCheckBoxesBySettings(const _searchFormSettings : TRipGrepperSearchFormSettings);
begin
	FCbClickEventEnabled := False;
	try
		WriteSearchFormSettingsToCtrls(_searchFormSettings);
	finally
		FCbClickEventEnabled := True;
	end;
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.UpdateCheckBoxesBySettings: ' + Format('Hidden %s NoIgnore %s Pretty %s',
		[BoolToStr(cbRgParamHidden.Checked), BoolToStr(cbRgParamNoIgnore.Checked), BoolToStr(cbRgParamPretty.Checked)]));
end;

procedure TRipGrepperSearchDialogForm.UpdateMemoCommandLine(const _bSkipReadCtrls : Boolean = False);
begin
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.UpdateMemoCommandLine: start ' + FGuiSetSearchParams.ToString);

	if not _bSkipReadCtrls then
		WriteCtrlsToRipGrepParametersSettings;

	FSettings.RipGrepParameters.GuiSearchTextParams.Copy(FGuiSetSearchParams);
	FSettings.RebuildArguments();
	memoCommandLine.Text := FSettings.RipGrepParameters.GetCommandLine();
	FGuiSetSearchParams.Copy(FSettings.RipGrepParameters.GuiSearchTextParams);
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.UpdateMemoCommandLine: end ' + FGuiSetSearchParams.ToString);

end;

procedure TRipGrepperSearchDialogForm.UpdateCtrls(_ctrlChanged : TControl);
begin
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.UpdateCtrls: ctrl ' + _ctrlChanged.Name);

	if cmbSearchText = _ctrlChanged then begin
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

procedure TRipGrepperSearchDialogForm.AlignExpertGroupBox;
begin
	if FSettings.RipGrepperSettings.ExpertMode then begin
		gbExpert.Height := FExpertGroupHeight;
		gbExpert.Caption := CAPTION_GRPBX_EXPERT_MODE;
		// gbExpert.Font.Style := gbExpert.Font.Style - [fsBold, fsUnderline];
	end else begin
		gbExpert.Height := 0;
		gbExpert.Visible := False;
		// gbExpert.Caption := 'Show ' + CAPTION_GRPBX_EXPERT_MODE;
		// gbExpert.Font.Style := gbExpert.Font.Style + [fsBold, fsUnderline];
	end;
end;

procedure TRipGrepperSearchDialogForm.cbRgParamEncodingClick(Sender : TObject);
begin
	if not FCbClickEventEnabled then
		Exit;

	cmbRgParamEncoding.Enabled := cbRgParamEncoding.Checked;
	FSettings.RipGrepperSearchFormSettings.Encoding := IfThen(cmbRgParamEncoding.Enabled, cmbRgParamEncoding.Text);
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

procedure TRipGrepperSearchDialogForm.ChecVsCodeRipGrep;
begin
	var
		sVsDir : string := TFileUtils.GetVsCodeDir;
	if not sVsDir.IsEmpty then begin
		sVsDir := TFileUtils.ShortToLongPath(sVsDir.Remove(sVsDir.Length - '\bin'.Length));

		var
			sRgPath : string;
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
		end;
	end;
end;

procedure TRipGrepperSearchDialogForm.cmbOptionsChange(Sender : TObject);
begin
	UpdateCtrls(cmbOptions);
end;

procedure TRipGrepperSearchDialogForm.cmbRgParamEncodingChange(Sender : TObject);
begin
	FSettings.RipGrepperSearchFormSettings.Encoding := IfThen(cmbRgParamEncoding.Enabled, cmbRgParamEncoding.Text);
	UpdateCtrls(cmbRgParamEncoding);
end;

function TRipGrepperSearchDialogForm.HasHistItemObj : Boolean;
begin
	Result := Assigned(FHistItemObj);
end;

procedure TRipGrepperSearchDialogForm.rbExtensionOptionsClick(Sender : TObject);
begin
	if IOTAUTils.IsStandAlone or FbExtensionOptionsSkipClick then begin
		Exit;
	end;
	UpdateCmbsOnContextChange();
	WriteCtrlsToRipGrepParametersSettings();
	UpdateCmbOptionsAndMemoCommandLine();
end;

procedure TRipGrepperSearchDialogForm.seContextLineNumChange(Sender : TObject);
begin
	FSettings.RipGrepperSearchFormSettings.Context := IfThen(seContextLineNum.Enabled, seContextLineNum.Value);
	UpdateCtrls(seContextLineNum);
end;

function TRipGrepperSearchDialogForm.GetInIDESelectedText : string;
var
	selectedText : TMultiLineString;
begin
	IOTAUtils.GxOtaGetActiveEditorTextAsMultilineString(selectedText, True);
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.GetInIDESelectedText: ' + selectedText);

	selectedText := string(selectedText).Trim();
	Result := CheckAndCorrectMultiLine(selectedText);
end;

procedure TRipGrepperSearchDialogForm.LoadDefaultSettings;
begin
	FSettings.LoadDefault;
	// TODO set only if it was saved before!
	cmbSearchDir.Text := IfThen(FSettings.RipGrepParameters.SearchPath.IsEmpty, cmbSearchDir.Text, FSettings.RipGrepParameters.SearchPath);
	cmbFileMasks.Text := IfThen(FSettings.RipGrepParameters.FileMasks.IsEmpty, cmbFileMasks.Text, FSettings.RipGrepParameters.FileMasks);
	FGuiSetSearchParams.SearchOptions := FSettings.RipGrepParameters.GuiSearchTextParams.SearchOptions;
	UpdateCheckBoxesBySettings(FSettings.RipGrepperSearchFormSettings);
end;

procedure TRipGrepperSearchDialogForm.LoadExtensionSearchSettings;
var
	extSearchSettings : TRipGrepperExtensionContext;
	selectedText : string;
begin
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;

	extSearchSettings := FSettings.ExtensionSettings.CurrentSearchSettings;
	if not HasHistItemObj then begin
		selectedText := GetInIDESelectedText;
		if not selectedText.IsEmpty then begin
			cmbSearchText.Text := selectedText;
			TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.LoadExtensionSearchSettings SelectedText=' + selectedText);
		end;
	end;
	extSearchSettings.ActiveFile := IOTAUTils.GxOtaGetCurrentSourceFile();

	extSearchSettings.ProjectFiles := IOTAUTils.GetProjectFiles();
	extSearchSettings.OpenFiles := IOTAUTils.GetOpenedEditBuffers();
	extSearchSettings.ActiveProject := (IOTAUTils.GxOtaGetCurrentProject).FileName;

	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.LoadExtensionSearchSettings CurrentSearchSettings:' + extSearchSettings.ToString);
	FSettings.ExtensionSettings.CurrentSearchSettings := extSearchSettings;

	FbExtensionOptionsSkipClick := True;
	rbExtensionOptions.ItemIndex := Integer(extSearchSettings.Context);
	UpdateCmbsOnContextChange();
	FbExtensionOptionsSkipClick := False;
end;

class function TRipGrepperSearchDialogForm.ShowSearchForm(_owner : TComponent; _settings : TRipGrepperSettings;
	_histObj : IHistoryItemObject) : integer;
var
	frm : TRipGrepperSearchDialogForm;
begin
	frm := TRipGrepperSearchDialogForm.Create(_owner, _settings, _histObj);
	try
		Result := frm.ShowModal();
		if mrOk = Result then begin
			_settings.LastSearchText := frm.cmbSearchText.Text
		end else begin
			_settings.LastSearchText := '';
		end;
	finally
		frm.Free;
	end;
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

procedure TRipGrepperSearchDialogForm.UpdateCmbsOnContextChange;
var
	rgec : TRipGrepperExtensionContext;
begin
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	rgec := FSettings.ExtensionSettings.CurrentSearchSettings;
	case rbExtensionOptions.ItemIndex of
		EXT_SEARCH_ACTIVE_FILE : begin
			cmbSearchDir.Enabled := False;
			cmbSearchDir.Text := rgec.ActiveFile;
		end;
		EXT_SEARCH_PROJECT_FILES : begin
			cmbSearchDir.Enabled := False;
			cmbSearchDir.Text := string.Join(SEARCH_PATH_SEPARATOR, rgec.ProjectFiles).Trim([SEARCH_PATH_SEPARATOR]);
		end;
		EXT_SEARCH_OPEN_FILES : begin
			cmbSearchDir.Enabled := False;
			cmbSearchDir.Text := string.Join(SEARCH_PATH_SEPARATOR, rgec.OpenFiles).Trim([SEARCH_PATH_SEPARATOR]);
		end;

		EXT_SEARCH_GIVEN_PATH : begin
			cmbSearchDir.Enabled := True;
			SetComboItemsAndText(cmbSearchDir, RG_ARG_SEARCH_PATH, FSettings.SearchPathsHistory, SEARCH_PATH_SEPARATOR);
		end;
	end;
	rgec.Context := ERipGrepperExtensionContext(rbExtensionOptions.ItemIndex);
	FSettings.ExtensionSettings.CurrentSearchSettings := rgec;
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.rbExtensionOptionsClick: ' + rgec.ToString);
end;

procedure TRipGrepperSearchDialogForm.UpdateFileMasksInFileMasks;
begin
	cmbFileMasks.Text := TCommandLineBuilder.GetFileMasksDelimited(FGuiSetSearchParams.RgOptions);
end;

function TRipGrepperSearchDialogForm.UpdateFileMasksInOptions(const sOptions, sMasks : string) : string;
var
	oldMaskOptions : TArrayEx<string>;
	newMaskOptions : string;
begin
	Result := sOptions;
	oldMaskOptions := TCommandLineBuilder.GetFileMaskParamsFromOptions(sOptions);
	newMaskOptions := TCommandLineBuilder.GetFileMaskParamsFromDelimitedText(sMasks, ';');

	Result := TOptionsHelper.RemoveAllParams(sOptions, RG_PARAM_REGEX_GLOB);
	Result := Result + ' ' + newMaskOptions.Trim([' ']);
end;

procedure TRipGrepperSearchDialogForm.UpdateFileMasksInOptions;
begin
	FGuiSetSearchParams.RgOptions := UpdateFileMasksInOptions(FGuiSetSearchParams.RgOptions, cmbFileMasks.Text);
	FSettings.RipGrepParameters.FileMasks := cmbFileMasks.Text;
end;

procedure TRipGrepperSearchDialogForm.UpdateHeight;
begin
	if IOTAUTils.IsStandAlone then begin
		rbExtensionOptions.Enabled := False;
		rbExtensionOptions.Visible := False;
		var
		shift := (rbExtensionOptions.Height + 2 * rbExtensionOptions.Margins.Bottom);
		// Margins.Top is 0
		gbOptionsFilters.Height := gbOptionsFilters.Height - shift;
		gbOptionsOutput.Top := gbOptionsOutput.Top - shift;
		gbExpert.Height := gbExpert.Height + shift;
		pnlSearch.Height := pnlSearch.Height - shift;
	end;

	var
	iHeight :=
	{ } GetFullHeight(gbSearch) +
	{ } GetFullHeight(gbOptionsFilters) +
	{ } GetFullHeight(gbOptionsOutput) +
	{ } GetFullHeight(pnlSearch) - pnlSearch.Height +
	{ } GetFullHeight(pnlBottom);

	if gbExpert.Visible then begin
		iHeight := iHeight + GetFullHeight(gbExpert);
	end else begin
		iHeight := iHeight + 2 * gbOptionsOutput.Margins.Bottom; // Margins.Top is 0
	end;

	Constraints.MinHeight := iHeight;
	Height := iHeight;
end;

procedure TRipGrepperSearchDialogForm.WriteOptionCtrlToRipGrepParametersSetting;
begin
	FGuiSetSearchParams.RgAdditionalOptions := cmbOptions.Text;
end;

procedure TRipGrepperSearchDialogForm.WriteSearchFormSettingsToCtrls(const _searchFormSettings : TRipGrepperSearchFormSettings);
begin
	cbRgParamHidden.Checked := _searchFormSettings.Hidden;
	cbRgParamNoIgnore.Checked := _searchFormSettings.NoIgnore;
	cbRgParamPretty.Checked := _searchFormSettings.Pretty;

	cbRgParamContext.Checked := _searchFormSettings.Context <> 0;
	seContextLineNum.Enabled := cbRgParamContext.Checked;
	seContextLineNum.Value := _searchFormSettings.Context;

	cbRgParamEncoding.Checked := _searchFormSettings.Encoding <> '';
	cmbRgParamEncoding.Enabled := cbRgParamEncoding.Checked;
	cmbRgParamEncoding.Text := _searchFormSettings.Encoding;
end;

end.
