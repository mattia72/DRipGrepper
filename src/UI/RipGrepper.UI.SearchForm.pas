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
	Vcl.ButtonGroup,
	Vcl.Buttons,
	Vcl.ToolWin,
	Vcl.ComCtrls,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Settings.RipGrepParameterSettings,
	Vcl.Samples.Spin,
	RipGrepper.Helper.Types,
	RipGrepper.Settings.SearchFormSettings,
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.SimpleTypes,
	SVGIconImageListBase,
	SVGIconImageList,
	RipGrepper.Helper.UI.DarkMode,
	Spring,
	ArrayEx,
	RipGrepper.Settings.SettingVariant,
	Vcl.ControlList,
	RipGrepper.UI.Settings.ExtensionContexPanel,
	RipGrepper.Common.IDEContextValues,
	RipGrepper.UI.SearchForm.CtrlValueProxy,
	RipGrepper.UI.CustomCheckOptions,
	RipGrepper.UI.RgOptionsPanel;

const
	RG_OPTIONS_PADDING_LEFT = 4;
	RG_OPTIONS_PADDING_TOP = 4;

type
	EMemoTextFormat = (mtfOneLine, mtfSeparateLines);

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
		tbMatchCase : TToolButton;
		tbMatchWord : TToolButton;
		tbUseRegex : TToolButton;
		gbExpert : TGroupBox;
		gbOptionsFilters : TGroupBox;
		lblFileMasks : TLabel;
		cmbFileMasks : TComboBox;
		lblCmdLine : TLabel;
		memoCommandLine : TMemo;
		btnCopyToClipBoard : TButton;
		ActionCopyToClipboard : TAction;
		btnHelpFileMask : TButton;
		ActionShowFileMaskHelp : TAction;
		btnRGOptionsHelp : TButton;
		ActionShowRGOptionsHelp : TAction;
		gbOptionsOutput : TGroupBox;
		pnlPath : TPanel;
		btnShowInLines : TButton;
		ActionShowInLines : TAction;
		pnlTop : TPanel;
		TabControl1 : TTabControl;
		cmbReplaceText : TComboBox;
		btnRGReplaceHelp : TButton;
		ActionShowRGReplaceOptionHelp : TAction;
		SVGIconImageList1 : TSVGIconImageList;
		ToolButton1 : TToolButton;
		ToolButton2 : TToolButton;
		pnlRgFilterOptions : TPanel;
		pnlRgOutputOptions : TPanel;
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
		procedure ActionShowFileMaskHelpExecute(Sender : TObject);
		procedure ActionShowInLinesExecute(Sender : TObject);
		procedure ActionShowRGOptionsHelpExecute(Sender : TObject);
		procedure ActionShowRGReplaceOptionHelpExecute(Sender : TObject);

		procedure cmbFileMasksChange(Sender : TObject);
		procedure cmbFileMasksExit(Sender : TObject);
		procedure cmbFileMasksSelect(Sender : TObject);
		procedure cmbOptionsChange(Sender : TObject);
		procedure cmbOptionsSelect(Sender : TObject);
		procedure cmbReplaceTextChange(Sender : TObject);
		procedure cmbSearchDirChange(Sender : TObject);
		procedure cmbSearchTextChange(Sender : TObject);
		procedure cmbSearchTextKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
		procedure ShowOptionsForm;
		procedure FormClose(Sender : TObject; var Action : TCloseAction);
		procedure FormResize(Sender : TObject);
		procedure FormShow(Sender : TObject);
		procedure OnContextChange(Sender : TObject; _icv : IIDEContextValues);

		procedure TabControl1Change(Sender : TObject);

		strict private
			FExtensionContextPanel : TExtensionContexPanel;
			FRgFilterOptionsPanel : TRgFilterOptionsPanel;
			FRgOutputOptionsPanel : TRgOutputOptionsPanel;
			FAppSettingsPanel : TAppOptionsPanel;
			cbRgParamHidden : TCheckBox;
			cbRgParamNoIgnore : TCheckBox;
			cbRgParamEncoding : TCheckBox;
			cmbRgParamEncoding : TComboBox;
			// Output options panel controls
			cbRgParamPretty : TCheckBox;
			cbRgParamContext : TCheckBox;
			seContextLineNum : TSpinEdit;
			cmbOutputFormat : TComboBox;

			// Base heights for normal mode (the base for all calculations)
			FExtensionContextPanelNormalHeight : Integer;
			FFormNormalHeight : Integer;

			// Height differences for expert mode (relative to normal mode)
			FExtensionContextPanelExpertHeightDiff : Integer;
			FFormExpertHeightDiff : Integer;

			// Common orig heights
			FRgFilterOptionsPanelOrigHeight : Integer;
			FRgOutputOptionsPanelOrigHeight : Integer;
			FpnlPathOrigHeight : Integer;
			FOrigHeight : integer;
			FIsKeyboardInput : Boolean;
			// proxy between settings and ctrls
			FCtrlProxy : TSearchFormCtrlValueProxy;
			FSettings : TRipGrepperSettings;
			FHistItemObj : IHistoryItemObject;
			// FDpiScaler : TRipGrepperDpiScaler;

			FSettingsProxy : IShared<TGuiSearchTextParams>;

			FbExtensionOptionsSkipClick : Boolean;
			FCbClickEventEnabled : Boolean;
			FMemoTextFormat : EMemoTextFormat;
			FOptionsOutputOrigTop : Integer;
			FpnlMiddleOrigHeight : Integer;
			FOrigSearchFormSettings : TSearchFormSettings;
			FTopPanelOrigHeight : Integer;
			FShowing : Boolean;
			FThemeHandler : TThemeHandler;

			procedure ApplyExpertModeLayout();
			procedure ApplyNormalModeLayout();
			procedure UpdateLayoutForMode();
			procedure CalculateAndStoreBaseHeights();
			procedure CalculateNormalModeHeights();
			procedure CalculateExpertModeHeights();
			function GetSelectedPaths(const _initialDir : string; const _fdo : TFileDialogOptions) : string;
			procedure WriteCtrlProxyToCtrls();
			procedure ButtonDown(const _searchOption : EGuiOption; _tb : TToolButton; const _bNotMatch : Boolean = False); overload;
			procedure ChangeHistoryItems(_cmb : TComboBox; var _items : TArrayEx<string>);
			function GetFullHeight(_ctrl : TControl) : integer;
			function IsOptionSet(const _sParamRegex : string; const _sParamValue : string = '') : Boolean;
			procedure ProcessControl(_ctrl : TControl; _imgList : TImageList);
			procedure RemoveNecessaryOptionsFromCmbOptionsText;
			procedure SetComboItemsAndText(_cmb : TComboBox; _txt : string; const _lst : TArrayEx<string>);
			procedure SetComboItemsFromOptions(_cmb : TComboBox; const _argMaskRegex : string; const _items : TArrayEx<string>);
			procedure UpdateCmbOptionsAndMemoCommandLine;
			procedure StoreCmbHistorieItems();
			procedure WriteCtrlsToRipGrepParametersSettings;
			procedure WriteCtrlsToSettings();
			procedure UpdateCheckBoxesByGuiSearchParams;
			procedure UpdateCheckBoxes;
			function CheckAndCorrectMultiLine(const _str : TMultiLineString) : string;
			procedure CopyCtrlsToProxy(var _ctrlProxy : TSearchFormCtrlValueProxy);
			procedure CopyItemsToProxy(var _arr : TArrayEx<string>; _setting : IArraySetting);
			function HasHistItemObjWithResult : Boolean;
			function GetInIDESelectedText : string;
			function GetValuesFromHistObjRipGrepArguments(const _argName : string; const _separator : string = ' ') : string;

			procedure LoadNewSearchSettings;
			procedure LoadExtensionSearchSettings(var _ctrlProxy : TSearchFormCtrlValueProxy);
			procedure LoadOldHistorySearchSettings;
			procedure LoadInitialSearchSettings();
			procedure SetCmbSearchPathText(const _sPath : string);
			procedure SetOrigHeights;
			class procedure SetReplaceText(_settings : TRipGrepperSettings; const _replaceText : string);
			procedure SetReplaceTextSetting(const _replaceText : string);
			procedure UpdateExtensionOptionsHint(const _paths : string);
			procedure ShowReplaceCtrls(const _bShow : Boolean);
			procedure UpdateSearchOptionsBtns;
			procedure UpdateCmbsOnIDEContextChange(_icv : IIDEContextValues);
			procedure OnRgFilterOptionsPanelItemSelect(Sender : TObject; Item : TCustomCheckItem);
			procedure OnRgOutputOptionsPanelItemSelect(Sender : TObject; Item : TCustomCheckItem);
			procedure OnEncodingComboBoxChange(Sender : TObject);
			procedure UpdateFileMasksInHistObjRgOptions; overload;
			procedure AdjustHeight();
			procedure UpdateRbExtensionItemIndex(const _dic : EDelphiIDESearchContext);
			function ValidateRegex : Boolean;
			procedure WriteOptionCtrlToProxy;
			procedure CopyProxyToCtrls();
			procedure CopySettingsToHistObj;
			procedure SetCmbSearchTextText(const _sText : string);
			procedure SetCmbSearchTextAutoComplete(const _Value : Boolean);
			procedure UpdateMemoTextFormat();
			function getOptionsAndFiltersHeight(const _bWithLabel : Boolean) : integer;
			procedure SetRgFilterOptionsPanel(const _settings : TRipGrepperSettings);
			procedure SetRgOutputOptionsPanel(const _settings : TRipGrepperSettings);
			procedure SetMinimumWidthFromOptionsPanels();
			procedure CopyProxyToSearchFormSettings(const _ctrlProxy : TSearchFormCtrlValueProxy; const _settings : TSearchFormSettings);
			procedure OnAppSettingsPanelItemSelect(Sender : TObject; Item : TCustomCheckItem);
			procedure SetAppSettingsPanel(const _settings : TRipGrepperSettings);

	private
		FUpdateLayoutRuns : Boolean;
		function GetFormExpertHeightDiff() : Integer;
		function GetFormNormalHeight() : integer;
		procedure SetExtensionContextPanel(const _settings : TRipGrepperSettings);
		procedure SetPrettyCheckboxHint();
		procedure UpdateExpertModeInOptionPanels(const _bIsExpert : boolean);
		property FormExpertHeightDiff : Integer read GetFormExpertHeightDiff;
		property FormNormalHeight : integer read GetFormNormalHeight;

	protected
		procedure ChangeScale(M, D : Integer; isDpiChange : Boolean); override;		public
			constructor Create(AOwner : TComponent; const _settings : TRipGrepperSettings; const _histObj : IHistoryItemObject);
				reintroduce; virtual;
			destructor Destroy; override;
			procedure CopySettingsToCtrlProxy(var _ctrlProxy : TSearchFormCtrlValueProxy; _histObj : IHistoryItemObject;
				_settings : TRipGrepperSettings);
			procedure CopyProxyToSettings(const _ctrlProxy : TSearchFormCtrlValueProxy; _histObj : IHistoryItemObject;
				_settings : TRipGrepperSettings);
			function GetMaxCountHistoryItems(const _arr : TArrayEx<string>) : TArrayEx<string>;
			function IsReplaceMode : Boolean;
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
	RipGrepper.CommandLine.Builder,
	Vcl.Clipbrd,
	RipGrepper.CommandLine.OptionHelper,
	RipGrepper.Tools.DebugUtils,
	Winapi.ShellAPI,
	System.StrUtils,
	{$IFNDEF STANDALONE}
	RipGrepper.Common.IOTAUtils,
	ToolsAPI,
	{$ENDIF}
	RipGrepper.Tools.FileUtils,
	System.IOUtils,
	Winapi.Windows,
	RipGrepper.Settings.AppSettings,
	RipGrepper.Settings.ExtensionSettings,
	RipGrepper.CommandLine.OptionStrings,
	RipGrepper.Helper.MemIniFile,
	RipGrepper.Settings.SettingsDictionary,
	RipGrepper.Common.SearchTextWithOptions,
	Spring.DesignPatterns;

{$R *.dfm}

constructor TRipGrepperSearchDialogForm.Create(AOwner : TComponent; const _settings : TRipGrepperSettings;
	const _histObj : IHistoryItemObject);
begin
	FSettings := _settings;
	inherited Create(AOwner);
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.Create');

	FHistItemObj := _histObj;

	// Create extension context panel first so it appears at the top
	SetExtensionContextPanel(_settings);
	SetRgFilterOptionsPanel(_settings);
	SetRgOutputOptionsPanel(_settings);
	SetAppSettingsPanel(_settings);

	UpdateExpertModeInOptionPanels(_settings.AppSettings.ExpertMode.Value);

	// align buttons a bit lower
	btnSearch.Top := btnSearch.Top + RG_OPTIONS_PADDING_TOP;
	btnCancel.Top := btnCancel.Top + RG_OPTIONS_PADDING_TOP;

	// Set minimum width based on widest options panel
	SetMinimumWidthFromOptionsPanels();

	LoadInitialSearchSettings;

	dbgMsg.Msg(FSettings.SearchFormSettings.ToLogString);
	dbgMsg.Msg('gui params=' + FSettingsProxy.ToLogString);

	// FDpiScaler := TRipGrepperDpiScaler.Create(self);
	var
	mainSettingInstance := Spring.DesignPatterns.TSingleton.GetInstance<TRipGrepperSettings>();
	FThemeHandler := TThemeHandler.Create(self, mainSettingInstance.AppSettings.ColorTheme);

	FOrigHeight := 0;

	FIsKeyboardInput := False;
	toolbarSearchTextOptions.AutoSize := False; // else shrinked as extension
	cmbOptions.AutoComplete := False; // so we know the old value after change

	// Reduce margins for tighter layout
	pnlTop.Margins.Top := 2;
	pnlTop.Margins.Bottom := 2;
	// pnlTop.Margins.Left := 2;
	// pnlTop.Margins.Right := 2;
	pnlMiddle.Margins.Top := 2;
	pnlMiddle.Margins.Bottom := 2;
	// pnlMiddle.Margins.Left := 2;
	// pnlMiddle.Margins.Right := 2;
	pnlBottom.Margins.Top := 2;
	pnlBottom.Margins.Bottom := 2;
	// pnlBottom.Margins.Left := 2;
	// pnlBottom.Margins.Right := 2;
	gbOptionsFilters.Margins.Top := 2;
	gbOptionsFilters.Margins.Bottom := 2;
	// gbOptionsFilters.Margins.Left := 2;
	// gbOptionsFilters.Margins.Right := 2;
end;

destructor TRipGrepperSearchDialogForm.Destroy;
begin
	FOrigSearchFormSettings.Free;

	// FDpiScaler.Free;
	inherited Destroy;
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamMatchCaseExecute(Sender : TObject);
begin
	FSettingsProxy.SwitchOption(EGuiOption.soMatchCase);
	UpdateCmbOptionsAndMemoCommandLine();
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamMatchCaseUpdate(Sender : TObject);
begin
	ButtonDown(EGuiOption.soMatchCase, tbMatchCase);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamRegexExecute(Sender : TObject);
begin
	FSettingsProxy.SwitchOption(EGuiOption.soUseRegex);
	UpdateCmbOptionsAndMemoCommandLine();
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamRegexUpdate(Sender : TObject);
begin
	ButtonDown(EGuiOption.soUseRegex, tbUseRegex);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamWordExecute(Sender : TObject);
begin
	FSettingsProxy.SwitchOption(EGuiOption.soMatchWord);
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
	selectedDirs := GetSelectedPaths(cmbSearchDir.Text, [fdoAllowMultiSelect, fdoPickfolders]);

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
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.ActionSearchExecute');
	WriteCtrlsToSettings();

	if ValidateRegex() then begin
		dbgMsg.Msg(memoCommandLine.Text);
		ModalResult := mrOk;
	end;
end;

procedure TRipGrepperSearchDialogForm.ActionSearchFileExecute(Sender : TObject);
var
	selectedFiles : string;
begin
	selectedFiles := GetSelectedPaths(cmbSearchDir.Text, [fdoAllowMultiSelect]);

	if selectedFiles <> '' then begin
		SetCmbSearchPathText(selectedFiles);
		UpdateCtrls(cmbSearchDir);
	end;
end;

procedure TRipGrepperSearchDialogForm.ActionShowFileMaskHelpExecute(Sender : TObject);
begin
	ShellExecute(0, 'OPEN', PChar(WWW_LINK_GLOBBING_HELP), '', '', SW_SHOWNORMAL);
end;

procedure TRipGrepperSearchDialogForm.ActionShowInLinesExecute(Sender : TObject);
var
	nextHint : string;
begin
	if FMemoTextFormat = mtfSeparateLines then begin
		FMemoTextFormat := mtfOneLine;
		nextHint := SHOW_CMD_IN_ONE_LINE;
	end else begin
		FMemoTextFormat := mtfSeparateLines;
		nextHint := SHOW_CMD_IN_SEPARATE_LINES;
	end;
	UpdateMemoTextFormat();
	ActionShowInLines.Hint := nextHint;
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
			FSettings.RipGrepParameters.RgExeOptions.RemoveOptions(
				{ } RG_NECESSARY_PARAMS + RG_GUI_SET_PARAMS);
			cmbOptions.Text := FSettings.RipGrepParameters.RgExeOptions.AsString;
			UpdateCtrls(cmbOptions);
		end;
	finally
		frm.Free;
	end;
end;

procedure TRipGrepperSearchDialogForm.FormClose(Sender : TObject; var Action : TCloseAction);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.FormClose');

	if ModalResult = mrCancel then begin
		Exit;
	end;

	CopyCtrlsToProxy(FCtrlProxy); // FormClose
	CopyProxyToSettings(FCtrlProxy, FHistItemObj, FSettings);

	if HasHistItemObjWithResult or FHistItemObj.IsLoadedFromStream then begin
		dbgMsg.Msg('HasHistItemObjWithResult');
		// Reload orig settings...
		FSettings.SearchFormSettings.Copy(FOrigSearchFormSettings);
		FSettings.SearchFormSettings.LoadFromDict();
	end;

	FSettings.StoreToPersister();

	var
	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());

	FSettings.UpdateFile();
end;

procedure TRipGrepperSearchDialogForm.FormShow(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.FormShow');
	FShowing := True;
	try
		WriteCtrlProxyToCtrls;
		// LoadExtensionSearchSettings;

		dbgMsg.Msg('RipGrepPath=' + FSettings.RipGrepParameters.RipGrepPath);
		// vscode rg doesn't support --pretty
		if TFileUtils.IsVsCodeRipGrep(FSettings.RipGrepParameters.RipGrepPath) then begin
			SetPrettyCheckboxHint;
		end;
		WriteCtrlsToRipGrepParametersSettings; // FormShow

		ActionShowInLines.Hint := SHOW_CMD_IN_SEPARATE_LINES;
		UpdateCmbOptionsAndMemoCommandLine;

		// Scale by active Monitor
		ScaleBy(TRipGrepperDpiScaler.GetActualDPI, self.PixelsPerInch);

		// Calculate and store base heights for both modes (only once)
		if (FExtensionContextPanelNormalHeight = 0) then begin
			CalculateAndStoreBaseHeights();
		end;

		// Apply the layout for the current mode
		AdjustHeight();

		ActiveControl := cmbSearchText;
	finally
		FShowing := False;
	end;
end;

function TRipGrepperSearchDialogForm.GetSelectedPaths(const _initialDir : string; const _fdo : TFileDialogOptions) : string;
var
	dlg : TFileOpenDialog;
begin
	dlg := TFileOpenDialog.Create(nil);
	try
		if not _initialDir.IsEmpty then begin
			if DirectoryExists(_initialDir) or FileExists(_initialDir) then begin
				dlg.DefaultFolder := _initialDir;
			end else begin
				var
				initialDir := _initialDir.Split([';'])[0];
				// get the valid part of the path...
				repeat
					initialDir := ExtractFileDir(initialDir); // cuts the last part of the path
				until DirectoryExists(initialDir);
				dlg.DefaultFolder := initialDir;
			end;
		end;

		if dlg.DefaultFolder.IsEmpty then begin
			dlg.DefaultFolder := 'C:\';
		end;

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

procedure TRipGrepperSearchDialogForm.WriteCtrlProxyToCtrls();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.WriteCtrlProxyToCtrls');

	SetComboItemsAndText(cmbSearchText, FCtrlProxy.SearchText, FCtrlProxy.SearchTextHist);
	SetComboItemsAndText(cmbSearchDir, FCtrlProxy.SearchPath, FCtrlProxy.SearchPathHist);
	SetComboItemsAndText(cmbReplaceText, FCtrlProxy.ReplaceText, FCtrlProxy.ReplaceTextHist);
	SetComboItemsFromOptions(cmbFileMasks, FCtrlProxy.FileMasks, FCtrlProxy.FileMasksHist);
	SetComboItemsAndText(cmbRgParamEncoding, FCtrlProxy.Encoding, FCtrlProxy.EncodingItems);
	SetComboItemsAndText(cmbOutputFormat, FCtrlProxy.OutputFormat, FCtrlProxy.OutputFormatItems);

	UpdateSearchOptionsBtns;
	UpdateCheckBoxes();
	SetComboItemsAndText(cmbOptions, FCtrlProxy.AdditionalExpertOptions, FCtrlProxy.AdditionalExpertOptionsHist);
end;

procedure TRipGrepperSearchDialogForm.ButtonDown(const _searchOption : EGuiOption; _tb : TToolButton; const _bNotMatch : Boolean = False);
var
	options : TSearchOptionSet;
begin
	options := FSettingsProxy.GetSearchOptions;
	if (_bNotMatch) then begin
		_tb.Down := not(_searchOption in options);
	end else begin
		_tb.Down := _searchOption in options;
	end;
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
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.cmbSearchTextChange');

	SetCmbSearchTextText(cmbSearchText.Text);
	UpdateCtrls(cmbSearchText);
	dbgMsg.MsgFmt('cmbSearchText.Text = %s', [cmbSearchText.Text]);
end;

function TRipGrepperSearchDialogForm.GetFullHeight(_ctrl : TControl) : integer;
begin
	Result := _ctrl.Margins.Top + _ctrl.Height + _ctrl.Margins.Bottom;
end;

function TRipGrepperSearchDialogForm.IsOptionSet(const _sParamRegex : string; const _sParamValue : string = '') : Boolean;
begin
	Result := FSettingsProxy.RgOptions.IsOptionSet(_sParamRegex, _sParamValue);
end;

procedure TRipGrepperSearchDialogForm.RemoveNecessaryOptionsFromCmbOptionsText;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.RemoveNecessaryOptionsFromCmbOptionsText');
	dbgMsg.Msg('ExpertOptions=' + FSettingsProxy.ExpertOptions.AsString);

	var
	os := TOptionStrings.New(FCtrlProxy.AdditionalExpertOptions);
	// Remove necessary options
	os.RemoveOptions(RG_NECESSARY_PARAMS + RG_GUI_SET_PARAMS);
	cmbOptions.Text := os.AsString;
	dbgMsg.Msg('cmbOptions.Text=' + cmbOptions.Text);

	WriteOptionCtrlToProxy;

	dbgMsg.Msg('ExpertOptions=' + FSettingsProxy.ExpertOptions.AsString);
end;

procedure TRipGrepperSearchDialogForm.SetComboItemsAndText(_cmb : TComboBox; _txt : string; const _lst : TArrayEx<string>);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.SetComboItemsAndText');
	_cmb.Items.Clear;
	_cmb.Items.AddStrings(_lst.Items);
	_cmb.Text := _txt;
	dbgMsg.MsgFmt('idx:%d, %s.Text = %s', [_cmb.ItemIndex, _cmb.Name, _cmb.Text]);
end;

procedure TRipGrepperSearchDialogForm.SetComboItemsFromOptions(_cmb : TComboBox; const _argMaskRegex : string;
	const _items : TArrayEx<string>);
var
	params : TArray<string>;
begin
	_cmb.Items.Clear;
	_cmb.Items.AddStrings(_items.Items);
	if HasHistItemObjWithResult then begin
		params := FHistItemObj.RipGrepArguments.GetOptions();
		_cmb.Text := TCommandLineBuilder.GetUniqueFileMasksDelimited(string.Join(' ', params));
	end else begin
		_cmb.Text := _cmb.Items[0];
	end;
	TDebugUtils.DebugMessageFormat('TRipGrepperSearchDialogForm.SetComboItemsFromOptions: %s - %s', [_cmb.Name, _cmb.Text]);
end;

procedure TRipGrepperSearchDialogForm.UpdateCmbOptionsAndMemoCommandLine;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.UpdateCmbOptionsAndMemoCommandLine');

	if not Assigned(FSettingsProxy) then begin
		Exit;
	end;

	dbgMsg.Msg('FSettingsProxy=' + FSettingsProxy.ToString);
	RemoveNecessaryOptionsFromCmbOptionsText;
	dbgMsg.Msg('RgOptions=' + string.Join(' ', FSettingsProxy.RgOptions.AsString));
	UpdateMemoCommandLine(True); // RgExeOptions may changed
	dbgMsg.Msg('FSettingsProxy=' + FSettingsProxy.ToString);
end;

procedure TRipGrepperSearchDialogForm.StoreCmbHistorieItems();
begin
	ChangeHistoryItems(cmbSearchText, FCtrlProxy.SearchTextHist);
	ChangeHistoryItems(cmbSearchDir, FCtrlProxy.SearchPathHist);
	ChangeHistoryItems(cmbReplaceText, FCtrlProxy.ReplaceTextHist);
	ChangeHistoryItems(cmbFileMasks, FCtrlProxy.FileMasksHist);
	ChangeHistoryItems(cmbOptions, FCtrlProxy.AdditionalExpertOptionsHist);
end;

procedure TRipGrepperSearchDialogForm.WriteCtrlsToRipGrepParametersSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.WriteCtrlsToRipGrepParametersSettings');

	if not Assigned(FSettingsProxy) then begin
		Exit;
	end;

	dbgMsg.Msg('FSettingsProxy=' + FSettingsProxy.ToString);
	FSettingsProxy.SetSearchText(cmbSearchText.Text);
	FSettingsProxy.IsReplaceMode := IsReplaceMode;
	if IsReplaceMode then begin
		if not FShowing then begin
			SetReplaceTextSetting(cmbReplaceText.Text);
		end;
		FSettingsProxy.SetRgOptionWithValue(RG_PARAM_REGEX_REPLACE, FSettingsProxy.ReplaceText, True);
	end else begin
		FSettingsProxy.SetRgOption(RG_PARAM_REGEX_REPLACE, True);
		FSettingsProxy.ReplaceText := '';
	end;
	dbgMsg.Msg('ReplaceText=' + FSettingsProxy.ReplaceText);

	ShowReplaceCtrls(IsReplaceMode());

	FSettings.RipGrepParameters.SearchPath := cmbSearchDir.Text;
	dbgMsg.Msg('SearchPath=' + cmbSearchDir.Text);

	FSettings.RipGrepParameters.FileMasks := cmbFileMasks.Text;
	dbgMsg.Msg('FileMasks=' + cmbFileMasks.Text);

	FSettingsProxy.SetRgOption(RG_PARAM_REGEX_HIDDEN, not cbRgParamHidden.Checked);
	FSettingsProxy.SetRgOption(RG_PARAM_REGEX_NO_IGNORE, not cbRgParamNoIgnore.Checked);

	FSettingsProxy.SetRgOption(RG_PARAM_REGEX_PRETTY, not(cbRgParamPretty.Enabled and cbRgParamPretty.Checked));
	if cbRgParamContext.Checked then begin
		if seContextLineNum.Text = '' then begin
			seContextLineNum.Text := '0';
		end;
		FSettingsProxy.SetRgOptionWithValue(RG_PARAM_REGEX_CONTEXT, seContextLineNum.Text, True);
	end else begin
		FSettingsProxy.SetRgOption(RG_PARAM_REGEX_CONTEXT, True);
	end;

	if cbRgParamEncoding.Checked then begin
		// if cmbRgParamEncoding.Text = '' then begin
		// cmbRgParamEncoding.Text := cmbRgParamEncoding.Items[0];
		// end;
		// FSettings.SearchFormSettings.Encoding.Value := cmbRgParamEncoding.Text;
		FSettingsProxy.SetRgOptionWithValue(RG_PARAM_REGEX_ENCODING, cmbRgParamEncoding.Text, { bUnique } True);
	end else begin
		FSettingsProxy.SetRgOption(RG_PARAM_REGEX_ENCODING, { bReset } True);
		// FSettings.SearchFormSettings.Encoding.Value := '';
	end;

	FSettings.SearchFormSettings.OutputFormat.Value := cmbOutputFormat.Text;
	if OUTPUT_FORMAT_JSON = cmbOutputFormat.Text then begin
		FSettingsProxy.SetRgOption(RG_PARAM_REGEX_JSON_OUTPUT);
		FSettingsProxy.SetRgOption(RG_PARAM_REGEX_VIMGREP_OUTPUT, True { Reset } );
	end else if OUTPUT_FORMAT_VIMGREP = cmbOutputFormat.Text then begin
		FSettingsProxy.SetRgOption(RG_PARAM_REGEX_JSON_OUTPUT, True { Reset } );
		FSettingsProxy.SetRgOption(RG_PARAM_REGEX_VIMGREP_OUTPUT);
	end;

	if Fsettings.AppSettings.IsExpertMode then begin
		WriteOptionCtrlToProxy;
	end;

	dbgMsg.Msg('FSettingsProxy = ' + FSettingsProxy.ToString);
end;

procedure TRipGrepperSearchDialogForm.WriteCtrlsToSettings();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.WriteCtrlsToSettings');

	StoreCmbHistorieItems();

	WriteCtrlsToRipGrepParametersSettings(); // WriteCtrlsToSettings
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.WriteCtrlsToSettings: set GuiSearchTextParams=' + FSettingsProxy.ToString);
	FSettings.RipGrepParameters.GuiSearchTextParams.Copy(FSettingsProxy);

	FSettings.RebuildArguments();
end;

procedure TRipGrepperSearchDialogForm.UpdateCheckBoxesByGuiSearchParams;
var
	sVal : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.UpdateCheckBoxesByGuiSearchParams');

	var
	ar := TAutoSetReset.New(FRgFilterOptionsPanel.FEventsEnabled, False);
	var
	ar2 := TAutoSetReset.New(FCbClickEventEnabled, False);

	cbRgParamHidden.Checked := IsOptionSet(RG_PARAM_REGEX_HIDDEN);
	cbRgParamNoIgnore.Checked := IsOptionSet(RG_PARAM_REGEX_NO_IGNORE);
	// cbRgParamEncoding.Checked := FSettingsProxy.RgOptions.GetOptionValue(RG_PARAM_REGEX_ENCODING, sVal);
	// cmbRgParamEncoding.Enabled := cbRgParamEncoding.Checked;
	// cmbRgParamEncoding.Text := IfThen(cmbRgParamEncoding.Enabled, sVal, '');

	cbRgParamPretty.Checked := IsOptionSet(RG_PARAM_REGEX_PRETTY);
	cbRgParamContext.Checked := FSettingsProxy.RgOptions.GetOptionValue(RG_PARAM_REGEX_CONTEXT, sVal);
	seContextLineNum.Enabled := cbRgParamContext.Checked;
	seContextLineNum.Text := IfThen(seContextLineNum.Enabled, sVal, '0');

	if (FSettingsProxy.RgOptions.GetOptionValue(RG_PARAM_REGEX_JSON_OUTPUT, sVal)) then begin
		cmbOutputFormat.Text := sVal;
	end else if (FSettingsProxy.RgOptions.GetOptionValue(RG_PARAM_REGEX_VIMGREP_OUTPUT, sVal)) then begin
		cmbOutputFormat.Text := sVal;
	end;

	sVal := '';
	var
	bReplaceMode := FSettingsProxy.RgOptions.GetOptionValue(RG_PARAM_REGEX_REPLACE, sVal);
	TabControl1.TabIndex := IfThen(bReplaceMode, 1, 0);
	cmbReplaceText.Text := TOptionStrings.MaybeDeQuoteIfQuoted(sVal);

	dbgMsg.MsgFmt('Hidden %s NoIgnore %s Pretty %s',
		{ } [BoolToStr(cbRgParamHidden.Checked),
		{ } BoolToStr(cbRgParamNoIgnore.Checked),
		{ } BoolToStr(cbRgParamPretty.Checked)]);
end;

procedure TRipGrepperSearchDialogForm.UpdateCheckBoxes;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.UpdateCheckBoxes');

	var
	ar := TAutoSetReset.New(FRgFilterOptionsPanel.FEventsEnabled, False);
	var
	ar2 := TAutoSetReset.New(FCbClickEventEnabled, False);

	CopyProxyToCtrls();

	dbgMsg.MsgFmt('cbHidden %s cbNoIgnore %s cbPretty %s',
		{ } [BoolToStr(cbRgParamHidden.Checked),
		{ } BoolToStr(cbRgParamNoIgnore.Checked),
		{ } BoolToStr(cbRgParamPretty.Checked)]);
end;

procedure TRipGrepperSearchDialogForm.UpdateMemoCommandLine(const _bSkipReadCtrls : Boolean = False);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.UpdateMemoCommandLine');
	dbgMsg.Msg('FSettingsProxy= ' + FSettingsProxy.ToLogString);

	if not _bSkipReadCtrls then begin
		dbgMsg.Msg('not SkipReadCtrls');
		WriteCtrlsToRipGrepParametersSettings(); // UpdateMemoCommandLine
	end;

	// it can be changed by btns tbMatchCase, tbMatchWord etc.
	FSettings.RipGrepParameters.GuiSearchTextParams.Copy(FSettingsProxy);

	FSettings.RebuildArguments();
	// memoCommandLine.Text := FSettings.RipGrepParameters.GetCommandLine(FSettings.AppSettings.CopyToClipBoardShell);
	UpdateMemoTextFormat();
	FSettingsProxy.Copy(FSettings.RipGrepParameters.GuiSearchTextParams());
	dbgMsg.Msg('FSettingsProxy= ' + FSettingsProxy.ToLogString);
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
		UpdateFileMasksInHistObjRgOptions();
		UpdateMemoCommandLine(); // UpdateCtrls
	end else if
	{ } (FRgFilterOptionsPanel = _ctrlChanged)
	{ } or (FRgOutputOptionsPanel = _ctrlChanged)
	{ } or (cbRgParamHidden = _ctrlChanged)
	{ } or (cbRgParamNoIgnore = _ctrlChanged)
	{ } or (cbRgParamPretty = _ctrlChanged)
	{ } or (cbRgParamContext = _ctrlChanged)
	{ } or (seContextLineNum = _ctrlChanged)
	{ } or (cbRgParamEncoding = _ctrlChanged)
	{ } or (cmbRgParamEncoding = _ctrlChanged)
	{ } or (cmbOutputFormat = _ctrlChanged)
	{ } then begin
		UpdateMemoCommandLine(); // this should be done first! UpdateCtrls
	end else if cmbOptions = _ctrlChanged then begin
		UpdateMemoCommandLine(); // this should be done first! UpdateCtrls
		UpdateCheckBoxesByGuiSearchParams(); // UpdateCtrs(cmbControls)
	end;
end;

procedure TRipGrepperSearchDialogForm.ChangeHistoryItems(_cmb : TComboBox; var _items : TArrayEx<string>);
begin
	_items.InsertUnique(0, _cmb.Text);
	TComboBoxHelper.ChangeItems(_cmb, _items.Items);
end;

procedure TRipGrepperSearchDialogForm.ChangeScale(M, D : Integer; isDpiChange : Boolean);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.ChangeScale');

	inherited ChangeScale(M, D, isDpiChange);

	dbgMsg.MsgFmt('M(%d) / D(%d) = %d%%', [M, D, MulDiv(100, M, D)]);
	if isDpiChange or (FOrigHeight = 0) then begin
		SetOrigHeights;
		toolbarSearchTextOptions.AutoSize := false;
		toolbarSearchTextOptions.Width := MulDiv(toolbarSearchTextOptions.Width, M, D);
	end;
end;

function TRipGrepperSearchDialogForm.CheckAndCorrectMultiLine(const _str : TMultiLineString) : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.CheckAndCorrectMultiLine');

	Result := '';
	if (_str.IsMultiLine) then begin
		TMsgBox.ShowWarning('Multiline string not supported.' + CRLF + 'Only first line will be searched.');
		// Save in ini not implemented for multiline strings
	end;
	Result := _str.GetLine(0);
	dbgMsg.MsgFmt('Result = %s', [Result]);
end;

procedure TRipGrepperSearchDialogForm.cmbOptionsChange(Sender : TObject);
begin
	UpdateCtrls(cmbOptions);
end;

procedure TRipGrepperSearchDialogForm.cmbReplaceTextChange(Sender : TObject);
begin
	if FShowing then
		Exit;

	cmbReplaceText.Text := CheckAndCorrectMultiLine(cmbReplaceText.Text);
	UpdateCtrls(cmbReplaceText);
end;

procedure TRipGrepperSearchDialogForm.cmbSearchTextKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
	FIsKeyboardInput := True;
end;

procedure TRipGrepperSearchDialogForm.CopyCtrlsToProxy(var _ctrlProxy : TSearchFormCtrlValueProxy);
begin
	_ctrlProxy.IsReplaceMode := (TabControl1.TabIndex = 1);
	_ctrlProxy.ReplaceText := cmbReplaceText.Text;
	_ctrlProxy.IsHiddenChecked := cbRgParamHidden.Checked;
	_ctrlProxy.IsNoIgnoreChecked := cbRgParamNoIgnore.Checked;
	_ctrlProxy.IsPrettyChecked := cbRgParamPretty.Checked;
	_ctrlProxy.LineContext := seContextLineNum.Value;
	_ctrlProxy.Encoding := cmbRgParamEncoding.Text;
	_ctrlProxy.OutputFormat := cmbOutputFormat.Text;
	_ctrlProxy.SearchOptions := TSearchTextWithOptions.GetAsSearchOptionSet(
		{ } tbMatchCase.Down,
		{ } tbMatchWord.Down,
		{ } tbUseRegex.Down);
end;

procedure TRipGrepperSearchDialogForm.CopyItemsToProxy(var _arr : TArrayEx<string>; _setting : IArraySetting);
begin
	_arr.Clear;
	for var i := 0 to _setting.Value.MaxIndex do begin
		_arr.Add(_setting.Value[i]);
	end;
end;

procedure TRipGrepperSearchDialogForm.CopySettingsToCtrlProxy(var _ctrlProxy : TSearchFormCtrlValueProxy; _histObj : IHistoryItemObject;
	_settings : TRipGrepperSettings);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.CopySettingsToCtrlProxy');

	CopyItemsToProxy(_ctrlProxy.SearchTextHist, FSettings.SearchTextsHistory);
	CopyItemsToProxy(_ctrlProxy.ReplaceTextHist, FSettings.ReplaceTextsHistory);
	CopyItemsToProxy(_ctrlProxy.SearchPathHist, FSettings.SearchPathsHistory);
	CopyItemsToProxy(_ctrlProxy.FileMasksHist, FSettings.FileMasksHistory);
	CopyItemsToProxy(_ctrlProxy.AdditionalExpertOptionsHist, FSettings.ExpertOptionHistory);

	_ctrlProxy.EncodingItems := FSettings.AppSettings.EncodingItems;
	_ctrlProxy.OutputFormatItems.Items := OUTPUT_FORMATS;
	var
	iIDEContext := FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext.IDESearchContext;
	_ctrlProxy.ExtensionContext := EDelphiIDESearchContext(iIDECOntext);
	dbgMsg.MsgFmt('IDESearchContext = %d', [Integer(_ctrlProxy.ExtensionContext)]);

	if HasHistItemObjWithResult or FHistItemObj.IsLoadedFromStream then begin
		_ctrlProxy.SearchText := FHistItemObj.GuiSearchTextParams.SearchTextWithOptions.SearchTextOfUser;
		_ctrlProxy.ReplaceText := FHistItemObj.ReplaceText;
		_ctrlProxy.SearchOptions := FHistItemObj.GuiSearchTextParams.GetSearchOptions;
		_ctrlProxy.IsReplaceMode := FHistItemObj.IsReplaceMode;

		_ctrlProxy.SearchPath := GetValuesFromHistObjRipGrepArguments(RG_ARG_SEARCH_PATH, SEARCH_PATH_SEPARATOR);
		_ctrlProxy.FileMasks := GetValuesFromHistObjRipGrepArguments(RG_PARAM_REGEX_GLOB);

		_ctrlProxy.IsHiddenChecked := FHistItemObj.SearchFormSettings.Hidden.Value;
		_ctrlProxy.IsNoIgnoreChecked := FHistItemObj.SearchFormSettings.NoIgnore.Value;
		_ctrlProxy.Encoding := FHistItemObj.SearchFormSettings.Encoding.Value;

		_ctrlProxy.OutputFormat := FHistItemObj.SearchFormSettings.OutputFormat.Value;
		_ctrlProxy.IsPrettyChecked := FHistItemObj.SearchFormSettings.Pretty.Value;
		_ctrlProxy.LineContext := FHistItemObj.SearchFormSettings.Context.Value;
		_ctrlProxy.AdditionalExpertOptions := FHistItemObj.GuiSearchTextParams.ExpertOptions.AsString;
	end else begin
		_ctrlProxy.SearchText := _ctrlProxy.SearchTextHist.SafeItem[0];
		_ctrlProxy.ReplaceText := _ctrlProxy.ReplaceTextHist.SafeItem[0];
		_ctrlProxy.SearchOptions := FSettings.RipGrepParameters.GuiSearchTextParams.GetSearchOptions;
		_ctrlProxy.IsReplaceMode := FSettings.IsReplaceMode;

		_ctrlProxy.SearchPath := _ctrlProxy.SearchPathHist.SafeItem[0];
		_ctrlProxy.FileMasks := _ctrlProxy.FileMasksHist.SafeItem[0];

		_ctrlProxy.IsHiddenChecked := FSettings.SearchFormSettings.Hidden.Value;
		_ctrlProxy.IsNoIgnoreChecked := FSettings.SearchFormSettings.NoIgnore.Value;
		_ctrlProxy.Encoding := FSettings.SearchFormSettings.Encoding.Value;

		_ctrlProxy.OutputFormat := FSettings.SearchFormSettings.OutputFormat.Value;
		_ctrlProxy.IsPrettyChecked := FSettings.SearchFormSettings.Pretty.Value;
		_ctrlProxy.LineContext := FSettings.SearchFormSettings.Context.Value;
		_ctrlProxy.AdditionalExpertOptions := _ctrlProxy.AdditionalExpertOptionsHist.SafeItem[0];
	end;
	dbgMsg.MsgFmt('Proxy filled from Settings: %s', [_ctrlProxy.ToString]);
end;

procedure TRipGrepperSearchDialogForm.CopyProxyToSettings(const _ctrlProxy : TSearchFormCtrlValueProxy; _histObj : IHistoryItemObject;
	_settings : TRipGrepperSettings);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.CopyProxyToSettings');

	FSettings.SearchTextsHistory.Value := GetMaxCountHistoryItems(_ctrlProxy.SearchTextHist);
	FSettings.ReplaceTextsHistory.Value := GetMaxCountHistoryItems(_ctrlProxy.ReplaceTextHist);
	FSettings.SearchPathsHistory.Value := GetMaxCountHistoryItems(_ctrlProxy.SearchPathHist);
	FSettings.FileMasksHistory.Value := GetMaxCountHistoryItems(_ctrlProxy.FileMasksHist);
	FSettings.ExpertOptionHistory.Value := GetMaxCountHistoryItems(_ctrlProxy.AdditionalExpertOptionsHist);
	var
	rgec := FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext;
	rgec.IDESearchContext := _ctrlProxy.ExtensionContext;
	dbgMsg.MsgFmt('IDESearchContext = %d', [Integer(rgec.IDESearchContext)]);

	FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext := rgec;

	if HasHistItemObjWithResult or _histObj.IsLoadedFromStream then begin
		_histObj.GuiSearchTextParams.SetSearchOptions(_ctrlProxy.SearchOptions);
		CopyProxyToSearchFormSettings(_ctrlProxy, _histObj.SearchFormSettings);
		// copy every other setting...
		CopySettingsToHistObj;
	end else begin
		FSettings.RipGrepParameters.GuiSearchTextParams.SetSearchOptions(_ctrlProxy.SearchOptions);
		CopyProxyToSearchFormSettings(_ctrlProxy, FSettings.SearchFormSettings);
	end;
end;

procedure TRipGrepperSearchDialogForm.FormResize(Sender : TObject);
begin
	inherited;
	UpdateLayoutForMode();
end;

function TRipGrepperSearchDialogForm.HasHistItemObjWithResult : Boolean;
begin
	Result := Assigned(FHistItemObj) and (FHistItemObj.HasResult);
end;

procedure TRipGrepperSearchDialogForm.OnContextChange(Sender : TObject; _icv : IIDEContextValues);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.OnContextChange');
	var
	bSkipp :=
	{$IF IS_GUITEST OR IS_EXTENSION} False; {$ELSE} True; {$ENDIF}
	dbgMsg.MsgFmt('FbExtensionOptionsSkipClick %s', [BoolToStr(FbExtensionOptionsSkipClick, True)]);
	if bSkipp or FbExtensionOptionsSkipClick then begin
		Exit;
	end;
	UpdateCmbsOnIDEContextChange(_icv);
	WriteCtrlsToRipGrepParametersSettings(); // OnContextChange
	UpdateCmbOptionsAndMemoCommandLine();
end;

function TRipGrepperSearchDialogForm.GetInIDESelectedText : string;
var
	selectedText : TMultiLineString;
begin
	{$IF IS_EXTENSION}
	IOTAUtils.GxOtaGetActiveEditorTextAsMultilineString(selectedText, True);
	{$ELSE}
	{$IF IS_GUITEST}
	selectedText := 'guitest selected test'; // this should appear in search combo;
	{$ENDIF}
	{$ENDIF}
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.GetInIDESelectedText: ' + selectedText);

	selectedText := string(selectedText).Trim();
	Result := selectedText;
end;

function TRipGrepperSearchDialogForm.GetValuesFromHistObjRipGrepArguments(const _argName : string; const _separator : string = ' ')
	: string;
begin
	Result := string.Join(_separator, FHistItemObj.RipGrepArguments.GetValues(_argName));
end;

function TRipGrepperSearchDialogForm.IsReplaceMode : Boolean;
begin
	Result := TabControl1.TabIndex = 1;
end;

procedure TRipGrepperSearchDialogForm.LoadNewSearchSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.LoadNewSearchSettings');
	FSettings.ReadFile;
	// TODO option to load always defaults:
	// FSettings.CopyDefaultsToValues;
	// OR last used  ...
	FSettings.LoadFromDict;

	// TODO set only if it was saved before!
	SetCmbSearchPathText(IfThen(FSettings.RipGrepParameters.SearchPath.IsEmpty, cmbSearchDir.Text, FSettings.RipGrepParameters.SearchPath));
	dbgMsg.Msg('cmbSearchDir=' + cmbSearchDir.Text);
	// {$IFNDEF STANDALONE}
	var
	cic := FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext;
	dbgMsg.Msg('IDESearchContext=' + cic.ToLogString);
	UpdateRbExtensionItemIndex(cic.IDESearchContext);
	dbgMsg.Msg('cmbSearchDir=' + cmbSearchDir.Text);
	// {$ENDIF}
	cmbFileMasks.Text := IfThen(FSettings.RipGrepParameters.FileMasks.IsEmpty, cmbFileMasks.Text, FSettings.RipGrepParameters.FileMasks);
	dbgMsg.Msg('cmbFileMasks.Text=' + cmbFileMasks.Text);

	FSettingsProxy := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create(TRipGrepParameterSettings.INI_SECTION));
	// FSettingsProxy.ReadIni;
	// FSettingsProxy.LoadDefaultsFromDict;

	FSettingsProxy.Copy(FSettings.RipGrepParameters.GuiSearchTextParams());
	FSettingsProxy.UpdateRgParamsByGuiOptions();
	UpdateCheckBoxes();
end;

procedure TRipGrepperSearchDialogForm.LoadExtensionSearchSettings(var _ctrlProxy : TSearchFormCtrlValueProxy);
var
	dic : TDelphiIDEContext;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.LoadExtensionSearchSettings');
	dbgMsg.MsgFmt('ExtensionSettings.IsAlreadyRead=%s', [
		{ } BoolToStr(FSettings.SearchFormSettings.ExtensionSettings.IsAlreadyRead)]);
	var
	selectedText := GetInIDESelectedText;
	if not HasHistItemObjWithResult then begin
		if not selectedText.IsEmpty then begin
			// SetCmbSearchTextAutoComplete(False);
			_ctrlProxy.SearchTextHist.Insert(0, selectedText);
			_ctrlProxy.SearchText := selectedText;
			dbgMsg.Msg('SelectedText=' + selectedText);
		end;
	end;

	dic := FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext;
	{$IF IS_GUITEST}
	dic.ActiveFile := 'guitest\active file';
	dic.ProjectFiles := ['guitest\project\files'];
	dic.OpenFiles := ['guitest\open\files'];
	dic.ActiveProject := 'guitest active project';
	{$ENDIF}
	dbgMsg.Msg('ActiveFile=' + dic.ActiveFile);
	dbgMsg.MsgFmt('ProjectFiles.Count=', [Length(dic.ProjectFiles)]);
	dbgMsg.MsgFmt('OpenFiles.Count=', [Length(dic.OpenFiles)]);
	dbgMsg.Msg('ActiveProject=' + dic.ActiveProject);
	dbgMsg.Msg('CurrentIDEContext:' + dic.ToLogString);

	FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext := dic;
	UpdateRbExtensionItemIndex(dic.IDESearchContext);
end;

procedure TRipGrepperSearchDialogForm.LoadInitialSearchSettings();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.LoadInitialSearchSettings');
	if HasHistItemObjWithResult or FHistItemObj.IsLoadedFromStream then begin
		LoadOldHistorySearchSettings;
	end else begin
		LoadNewSearchSettings;
		SetCmbSearchTextAutoComplete(True);
	end;
	CopySettingsToCtrlProxy(FCtrlProxy, FHistItemObj, FSettings);
	LoadExtensionSearchSettings(FCtrlProxy);
end;

procedure TRipGrepperSearchDialogForm.SetCmbSearchTextAutoComplete(const _Value : Boolean);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.SetCmbSearchTextAutoComplete');
	dbgMsg.MsgFmt('cmbSearchText.AutoComplete=%s', [BoolToStr(_Value, True)]);
	cmbSearchText.AutoComplete := _Value;
end;

procedure TRipGrepperSearchDialogForm.SetCmbSearchPathText(const _sPath : string);
begin
	cmbSearchDir.Text := _sPath;
	TDebugUtils.Msg('cmbSearchDir.Text=' + cmbSearchDir.Text);
end;

procedure TRipGrepperSearchDialogForm.UpdateExtensionOptionsHint(const _paths : string);
begin
	if _paths.IsEmpty then begin
		Exit;
	end;
	FExtensionContextPanel.SelectedItem.RadioButton.Hint := TExtensionContexPanel.GetAsHint(_paths);
end;

procedure TRipGrepperSearchDialogForm.SetOrigHeights;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.SetOrigHeights');

	// Capture common heights that don't change between modes
	FRgFilterOptionsPanelOrigHeight := FRgFilterOptionsPanel.Height;
	FRgOutputOptionsPanelOrigHeight := FRgOutputOptionsPanel.Height;
	FpnlPathOrigHeight := pnlPath.Height;
	FOptionsOutputOrigTop := gbOptionsOutput.Top;
	FpnlMiddleOrigHeight := pnlMiddle.Height;
	FTopPanelOrigHeight := pnlTop.Height;
	FOrigHeight := Height;

	dbgMsg.MsgFmt('Captured heights - pnlTop: %d, FilterPanel: %d, PathPanel: %d, Form: %d',
		[FTopPanelOrigHeight, FRgFilterOptionsPanelOrigHeight, FpnlPathOrigHeight, FOrigHeight]);
end;

procedure TRipGrepperSearchDialogForm.CalculateNormalModeHeights();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.CalculateNormalModeHeights');

	// Temporarily switch to normal mode to calculate heights
	FExtensionContextPanel.ContextRadioGroup.ShowExpertItems(False);
	FExtensionContextPanel.ContextRadioGroup.AlignControlItems();
	FExtensionContextPanelNormalHeight := FExtensionContextPanel.ContextRadioGroup.Height;

	// Calculate form height for normal mode (without expert group box)
	var
	normalOptionsFiltersHeight := getOptionsAndFiltersHeight(True) + FExtensionContextPanelNormalHeight + FExtensionContextPanel.Margins.Top
		+ FExtensionContextPanel.Margins.Bottom;

	FFormNormalHeight :=
	{ } GetFullHeight(pnlTop) +
	{ } pnlMiddle.Margins.Top +
	{ } GetFullHeight(lblPaths) +
	{ } normalOptionsFiltersHeight +
	{ } GetFullHeight(gbOptionsOutput) +
	{ } pnlMiddle.Margins.Bottom +
	{ } GetFullHeight(pnlBottom);

	dbgMsg.MsgFmt('Normal mode - ExtensionPanel: %d, Form: %d', [FExtensionContextPanelNormalHeight, FFormNormalHeight]);
end;

procedure TRipGrepperSearchDialogForm.CalculateExpertModeHeights();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.CalculateExpertModeHeights');

	// Temporarily switch to expert mode to calculate heights
	FExtensionContextPanel.ContextRadioGroup.ShowExpertItems(True);
	FExtensionContextPanel.ContextRadioGroup.AlignControlItems();
	var
	expertPanelHeight := FExtensionContextPanel.ContextRadioGroup.Height;

	// Calculate the difference from normal mode
	FExtensionContextPanelExpertHeightDiff := expertPanelHeight - FExtensionContextPanelNormalHeight;

	// Calculate form height difference (expert mode includes expert group box)
	// The form difference is the panel difference plus the expert group box height
	// Ensure gbExpert is visible to get correct height
	gbExpert.Visible := True;

	var
	expertOptionsFiltersHeight := getOptionsAndFiltersHeight(False) + expertPanelHeight + FExtensionContextPanel.Margins.Top +
		FExtensionContextPanel.Margins.Bottom;

	var
	normalOptionsFiltersHeight := getOptionsAndFiltersHeight(True) + FExtensionContextPanelNormalHeight + FExtensionContextPanel.Margins.Top
		+ FExtensionContextPanel.Margins.Bottom;

	FFormExpertHeightDiff := (expertOptionsFiltersHeight - normalOptionsFiltersHeight) + gbExpert.Height;

	// Don't restore visibility - let the layout method handle it
	// The layout method will set the correct visibility based on the mode

	dbgMsg.MsgFmt('Expert mode differences - ExtensionPanel: +%d, Form: +%d (gbExpert.Height=%d)',
		[FExtensionContextPanelExpertHeightDiff, FFormExpertHeightDiff, gbExpert.Height]);
end;

procedure TRipGrepperSearchDialogForm.CalculateAndStoreBaseHeights();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.CalculateAndStoreBaseHeights');

	// Calculate heights for both modes
	CalculateNormalModeHeights();
	CalculateExpertModeHeights();

	// Restore to current mode
	var
	isExpert := FSettings.AppSettings.IsExpertMode;
	FExtensionContextPanel.ContextRadioGroup.ShowExpertItems(isExpert);
	FExtensionContextPanel.ContextRadioGroup.AlignControlItems();

	dbgMsg.Msg('Base heights calculated and stored');
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

	FSettingsProxy.ReplaceText := _replaceText;
	if FSettingsProxy.IsReplaceMode and _replaceText.IsEmpty then begin
		FSettingsProxy.ReplaceText := QuotedStr('');
	end;
	dbgMsg.Msg('LastReplaceText=' + FSettingsProxy.ReplaceText);
end;

procedure TRipGrepperSearchDialogForm.ShowReplaceCtrls(const _bShow : Boolean);
begin
	cmbReplaceText.Visible := _bShow;
	ActionShowRGReplaceOptionHelp.Visible := _bShow;
end;

class function TRipGrepperSearchDialogForm.ShowSearchForm(_owner : TComponent;
	{ } _settings : TRipGrepperSettings;
	{ } _histObj : IHistoryItemObject) : integer;
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
			TRipGrepperSearchDialogForm.SetReplaceText(_settings, frm.cmbReplaceText.Text);
		end else begin
			_settings.LastSearchText := _histObj.SearchText;
			TRipGrepperSearchDialogForm.SetReplaceText(_settings, _histObj.ReplaceText);
			dbgMsg.MsgFmtIf(_histObj.SearchText <> _histObj.GuiSearchTextParams.GetSearchText,
				{ } 'ERROR? _histObj.SearchText=%s <> GuiSearchTextParams=%s',
				[_histObj.SearchText, _histObj.GuiSearchTextParams.GetSearchText]);
		end;
		dbgMsg.Msg('LastSearchText=' + _settings.LastSearchText);
	finally
		frm.Free;
	end;
end;

procedure TRipGrepperSearchDialogForm.TabControl1Change(Sender : TObject);
begin
	ShowReplaceCtrls(IsReplaceMode());
	AdjustHeight();
	UpdateCtrls(TabControl1);
end;

procedure TRipGrepperSearchDialogForm.UpdateSearchOptionsBtns;
var
	s : string;
begin
	ButtonDown(EGuiOption.soMatchCase, tbMatchCase);
	ButtonDown(EGuiOption.soMatchWord, tbMatchWord);
	if tbMatchWord.Down then begin
		s := cmbSearchText.Text;
		TCommandLineBuilder.RemoveWordBoundaries(s);
		SetCmbSearchTextText(s);
	end;
	ButtonDown(EGuiOption.soUseRegex, tbUseRegex);
end;

procedure TRipGrepperSearchDialogForm.UpdateCmbsOnIDEContextChange(_icv : IIDEContextValues);
var
	contextValue : string;
begin
	var
	bSkipp := {$IF IS_GUITEST OR IS_EXTENSION} False; {$ELSE} True; {$ENDIF}
	if not bSkipp then begin
		var
		dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.UpdateCmbsOnIDEContextChange');
		cmbSearchDir.Enabled := False;
		if _icv.GetContextType() = dicNotSet then begin
			dbgMsg.WarningMsgFmt('Extension IDE Context not supported :%d. fallback to custom locations:',
				[Ord(FCtrlProxy.ExtensionContext)]);
			FCtrlProxy.ExtensionContext := EDelphiIDESearchContext.dicCustomLocation;
		end else begin
			FCtrlProxy.ExtensionContext := _icv.GetContextType();
		end;
		contextValue := _icv.GetValue();
		dbgMsg.MsgFmt('ExtensionContext=%d, Value=%s', [Ord(FCtrlProxy.ExtensionContext), contextValue]);
		case FCtrlProxy.ExtensionContext of
			EDelphiIDESearchContext.dicActiveFile,
			{ } EDelphiIDESearchContext.dicProjectFiles,
			{ } EDelphiIDESearchContext.dicProjectRootDirectory,
			{ } EDelphiIDESearchContext.dicOpenFiles,
			{ } EDelphiIDESearchContext.dicProjectLibraryPath,
			{ } EDelphiIDESearchContext.dicProjectFilesDirs : begin
				SetCmbSearchPathText(contextValue);
			end;
			EDelphiIDESearchContext.dicCustomLocation : begin
				cmbSearchDir.Enabled := True;
				dbgMsg.MsgFmt('SearchPath=%s', [FCtrlProxy.SearchPath]);
				SetComboItemsAndText(cmbSearchDir, FCtrlProxy.SearchPath, FSettings.SearchPathsHistory.Value);
				UpdateExtensionOptionsHint(FCtrlProxy.SearchPath);
			end
		end;

		FSettings.RipGrepParameters.SearchPath := cmbSearchDir.Text;
		var
			dic : TDelphiIDEContext;
		dic.IDESearchContext := FCtrlProxy.ExtensionContext;
		FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext := dic;
		dbgMsg.Msg(dic.ToLogString);
	end;
end;

procedure TRipGrepperSearchDialogForm.UpdateFileMasksInHistObjRgOptions;
begin
	FSettingsProxy.RgOptions.UpdateFileMasks(cmbFileMasks.Text);
	FSettings.RipGrepParameters.FileMasks := cmbFileMasks.Text;
end;

procedure TRipGrepperSearchDialogForm.AdjustHeight();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.AdjustHeight');
	dbgMsg.Msg('Height=' + Height.ToString);

	// Capture other original heights after layout is applied
	if (FOrigHeight = 0) then begin
		SetOrigHeights();
	end;
	// Adjust top panel for replace text visibility
	if cmbReplaceText.Visible then begin
		pnlTop.Height := FTopPanelOrigHeight;
	end else begin
		pnlTop.Height := FTopPanelOrigHeight - GetFullHeight(cmbReplaceText);
	end;
	dbgMsg.Msg('pnlTop.Height=' + pnlTop.Height.ToString);

	// Handle extension context frame visibility
	var
	bStandalone := {$IF IS_STANDALONE} True; {$ELSE} False; {$ENDIF}
	lblPaths.Visible := {$IF IS_GUITEST} FALSE; {$ELSE} bStandalone; {$ENDIF};;
	if bStandalone then begin
		var
		bVisible :=
		{$IF IS_GUITEST} True; {$ELSE} False;
		{$ENDIF};
		FExtensionContextPanel.Enabled := bVisible;
		FExtensionContextPanel.Visible := bVisible;
		dbgMsg.MsgFmt('FExtensionContextPanel.Visible=%s', [BoolToStr(bVisible, True)]);
	end;

	// Apply the appropriate layout mode (expert or normal)
	// The layout methods handle extension panel and form sizing
	UpdateLayoutForMode();
end;

procedure TRipGrepperSearchDialogForm.UpdateRbExtensionItemIndex(const _dic : EDelphiIDESearchContext);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.UpdateRbExtensionItemIndex');

	FbExtensionOptionsSkipClick := True;
	try
		FExtensionContextPanel.SetSelectedIDEContext(_dic);
		dbgMsg.MsgFmt('ContextRadioGroup.ItemIndex = %d', [Integer(_dic)]);

		// Create the appropriate context values directly to ensure proper initialization
		var
			icv : IIDEContextValues;
		case _dic of
			{ } EDelphiIDESearchContext.dicCustomLocation : begin
				icv := TIDEContextValues.Create(_dic, FCtrlProxy.SearchPath, False { isExpert } );
			end;
			else
			icv := FExtensionContextPanel.ContextValues;
		end;

		UpdateCmbsOnIDEContextChange(icv);
	finally
		FbExtensionOptionsSkipClick := False;
	end;
end;

function TRipGrepperSearchDialogForm.ValidateRegex : Boolean;
var
	bError : Boolean;
begin
	bError := False;
	if EGuiOption.soUseRegex in FSettingsProxy.GetSearchOptions then begin
		try
			var
			tryRegex := TRegEx.Create(FSettingsProxy.GetSearchText, [roCompiled]);
		except
			on E : Exception do begin
				TMsgBox.ShowError(E.Message + CRLF + 'Regex validation failed.');
				ActiveControl := cmbSearchText;
				bError := True;
			end;
		end;
	end;
	Result := not bError;
end;

procedure TRipGrepperSearchDialogForm.WriteOptionCtrlToProxy;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.WriteOptionCtrlToProxy');
	FSettingsProxy.ExpertOptions := TOptionStrings.New(cmbOptions.Text);
	FCtrlProxy.AdditionalExpertOptions := cmbOptions.Text;
	dbgMsg.Msg('FSettingsProxy.ExpertOptions=' + FSettingsProxy.ExpertOptions.AsString);
end;

procedure TRipGrepperSearchDialogForm.CopyProxyToCtrls();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.CopyProxyToCtrls');

	TabControl1.TabIndex := IfThen(FCtrlProxy.IsReplaceMode, 1, 0);
	cmbReplaceText.Text := FCtrlProxy.ReplaceText;
	dbgMsg.MsgFmt('cmbReplaceText.Text %s', [cmbReplaceText.Text]);

	// these assignings doesn't needed anymore, XxxxxOptionsPanel do it
	// cbRgParamPretty.Checked := FCtrlProxy.IsPrettyChecked;
	dbgMsg.MsgFmt('cbRgParamPretty.Checked %s', [BoolToStr(cbRgParamPretty.Checked)]);

	// cbRgParamContext.Checked := FCtrlProxy.LineContext <> 0;
	// seContextLineNum.Enabled := cbRgParamContext.Checked;
	// seContextLineNum.Value := FCtrlProxy.LineContext;
	dbgMsg.MsgFmt('seContextLineNum.Value=%d', [seContextLineNum.Value]);

	// FRgFilterOptionsPanel.EventsEnabled := False;
	try
		dbgMsg.MsgFmt('FCtrlProxy.IsHiddenChecked=%s', [BoolToStr(FCtrlProxy.IsHiddenChecked, True)]);
		// cbRgParamHidden.Checked := FCtrlProxy.IsHiddenChecked;
		dbgMsg.MsgFmt('cbRgParamHidden.Checked=%s', [BoolToStr(cbRgParamHidden.Checked, True)]);

		dbgMsg.MsgFmt('FCtrlProxy.IsNoIgnoreChecked=%s', [BoolToStr(FCtrlProxy.IsNoIgnoreChecked, True)]);
		// cbRgParamNoIgnore.Checked := FCtrlProxy.IsNoIgnoreChecked;
		dbgMsg.MsgFmt('cbRgParamNoIgnore.Checked=%s', [BoolToStr(cbRgParamNoIgnore.Checked, True)]);

		// cbRgParamEncoding.Checked := FCtrlProxy.Encoding <> '';
		// cmbRgParamEncoding.Enabled := cbRgParamEncoding.Checked;
		// cmbRgParamEncoding.Text := FCtrlProxy.Encoding;
		dbgMsg.Msg('cmbRgParamEncoding.Text=' + cmbRgParamEncoding.Text);

		// cmbOutputFormat.Text := FCtrlProxy.OutputFormat;
		dbgMsg.Msg('cmbOutputFormat.Text=' + cmbOutputFormat.Text);
	finally
		// FRgFilterOptionsPanel.EventsEnabled := True;
	end;
end;

procedure TRipGrepperSearchDialogForm.CopyProxyToSearchFormSettings(const _ctrlProxy : TSearchFormCtrlValueProxy;
	const _settings : TSearchFormSettings);
begin
	_settings.Hidden.Value := _ctrlProxy.IsHiddenChecked;
	_settings.NoIgnore.Value := _ctrlProxy.IsNoIgnoreChecked;
	_settings.Encoding.Value := _ctrlProxy.Encoding;
	_settings.OutputFormat.Value := _ctrlProxy.OutputFormat;
	_settings.Pretty.Value := _ctrlProxy.IsPrettyChecked;
	_settings.Context.Value := _ctrlProxy.LineContext;
end;

procedure TRipGrepperSearchDialogForm.CopySettingsToHistObj;
begin
	// FSettings.SearchFormSettings.StoreSearchSettings(False);
	FHistItemObj.SearchFormSettings.Copy(FSettings.SearchFormSettings);
	FHistItemObj.SearchFormSettings.LoadFromDict();
	FHistItemObj.RipGrepArguments.Clear;
	var
	args := FSettings.GetRipGrepArguments();
	FHistItemObj.RipGrepArguments.Assign(args());
	FHistItemObj.GuiSearchTextParams.Copy(FSettingsProxy);
end;

function TRipGrepperSearchDialogForm.GetMaxCountHistoryItems(const _arr : TArrayEx<string>) : TArrayEx<string>;
begin
	Result := _arr.GetRange(0, MAX_HISTORY_COUNT); // .GetReversedRange();
end;

function TRipGrepperSearchDialogForm.getOptionsAndFiltersHeight(const _bWithLabel : Boolean) : integer;
begin
	// pnlPath and pnlRgFilterOptions heights are now properly set to match their children
	Result := IfThen(_bWithLabel, GetFullHeight(lblPaths)) +
	{ } GetFullHeight(pnlPath) +
	{ } GetFullHeight(pnlRgFilterOptions) +
	{ } gbOptionsFilters.Padding.Top +
	{ } gbOptionsFilters.Padding.Bottom;
end;

procedure TRipGrepperSearchDialogForm.LoadOldHistorySearchSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.LoadOldHistorySearchSettings');

	FSettingsProxy := FHistItemObj.GuiSearchTextParams;
	if Assigned(FHistItemObj.SearchFormSettings) then begin
		FOrigSearchFormSettings := TSearchFormSettings.Create;
		dbgMsg.Msg('Hist: ' + FHistItemObj.SearchFormSettings.ToLogString);

		FOrigSearchFormSettings.Copy(FHistItemObj.SearchFormSettings);
		FSettings.SearchFormSettings.Copy(FHistItemObj.SearchFormSettings);
		FSettings.LoadFromDict();
	end;
end;

procedure TRipGrepperSearchDialogForm.SetCmbSearchTextText(const _sText : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.SetCmbSearchTextText');
	cmbSearchText.Text := CheckAndCorrectMultiLine(_sText);
	dbgMsg.MsgFmt('AutoComplete = %s SearchText = %s', [BoolToStr(cmbSearchText.AutoComplete, True), cmbSearchText.Text]);
end;

procedure TRipGrepperSearchDialogForm.UpdateMemoTextFormat();
begin
	var
	params := FSettings.RipGrepParameters;
	var
	shellType := FSettings.AppSettings.CopyToClipBoardShell;
	if FMemoTextFormat = mtfSeparateLines then begin
		memoCommandLine.Text := string.Join(CRLF, params.GetCommandLineAsArray(shellType));
	end else begin
		memoCommandLine.Text := params.GetCommandLine(shellType);
	end;
end;

procedure TRipGrepperSearchDialogForm.OnRgFilterOptionsPanelItemSelect(Sender : TObject; Item : TCustomCheckItem);
begin
	if not FCbClickEventEnabled then
		Exit;

	UpdateCtrls(FRgFilterOptionsPanel);
end;

procedure TRipGrepperSearchDialogForm.OnRgOutputOptionsPanelItemSelect(Sender : TObject; Item : TCustomCheckItem);
begin
	if not FCbClickEventEnabled then
		Exit;

	UpdateCtrls(FRgOutputOptionsPanel);
end;

procedure TRipGrepperSearchDialogForm.OnEncodingComboBoxChange(Sender : TObject);
begin
	// if FShowing then
	// Exit;

	UpdateCtrls(cmbRgParamEncoding);
end;

procedure TRipGrepperSearchDialogForm.OnAppSettingsPanelItemSelect(Sender : TObject; Item : TCustomCheckItem);
begin
	if not FCbClickEventEnabled then begin
		Exit;
	end;

	var
	isExpert := FSettings.AppSettings.ExpertMode.Value;
	if not isExpert and (mrYes <>
		{ } TMsgBox.ShowQuestion('Switch to normal mode? ' + CRLF2 +
		{ } 'All expert settings will be reset to defaults.')) then begin
		Exit;
	end;

	UpdateExpertModeInOptionPanels(isExpert);
end;

procedure TRipGrepperSearchDialogForm.SetPrettyCheckboxHint();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.SetPrettyCheckboxHint');

	cbRgParamPretty.Enabled := False;
end;

procedure TRipGrepperSearchDialogForm.SetRgFilterOptionsPanel(const _settings : TRipGrepperSettings);
begin
	FRgFilterOptionsPanel := TRgFilterOptionsPanel.Create(self);
	FRgFilterOptionsPanel.Settings := _settings;
	pnlRgFilterOptions.Caption := '';
	pnlRgFilterOptions.Padding.Left := RG_OPTIONS_PADDING_LEFT;
	pnlRgFilterOptions.Padding.Right := RG_OPTIONS_PADDING_LEFT;
	FRgFilterOptionsPanel.Parent := pnlRgFilterOptions;
	FRgFilterOptionsPanel.Align := alClient;
	FRgFilterOptionsPanel.AddItems();
	FRgFilterOptionsPanel.OnOptionChange := OnRgFilterOptionsPanelItemSelect;

	var
	optionsGroup := FRgFilterOptionsPanel.CheckOptionsGroup;
	optionsGroup.GetItemByCaption(RG_FILTER_OPTION_ENCODING_CAPTION).ComboBox.OnChange := OnEncodingComboBoxChange;

	optionsGroup.AlignControlItems;
	FRgFilterOptionsPanel.AdjustHeight();
	// Ensure parent panel height matches the filter panel plus padding
	pnlRgFilterOptions.Height := FRgFilterOptionsPanel.Height + pnlRgFilterOptions.Padding.Top + pnlRgFilterOptions.Padding.Bottom;

	cbRgParamHidden := optionsGroup.GetItemByCaption(RG_FILTER_OPTION_HIDDEN_CAPTION).CheckBox;
	cbRgParamNoIgnore := optionsGroup.GetItemByCaption(RG_FILTER_OPTION_NO_IGNORE_CAPTION).CheckBox;
	cbRgParamEncoding := optionsGroup.GetItemByCaption(RG_FILTER_OPTION_ENCODING_CAPTION).CheckBox;
	cmbRgParamEncoding := optionsGroup.GetItemByCaption(RG_FILTER_OPTION_ENCODING_CAPTION).ComboBox;
end;

procedure TRipGrepperSearchDialogForm.SetRgOutputOptionsPanel(const _settings : TRipGrepperSettings);
begin
	FRgOutputOptionsPanel := TRgOutputOptionsPanel.Create(self);
	FRgOutputOptionsPanel.Settings := _settings;
	pnlRgOutputOptions.Caption := '';
	pnlRgOutputOptions.Padding.Left := RG_OPTIONS_PADDING_LEFT;
	pnlRgOutputOptions.Padding.Right := RG_OPTIONS_PADDING_LEFT;
	FRgOutputOptionsPanel.Parent := pnlRgOutputOptions;
	FRgOutputOptionsPanel.Align := alClient;
	FRgOutputOptionsPanel.AddItems();
	FRgOutputOptionsPanel.OnOptionChange := OnRgOutputOptionsPanelItemSelect;

	var
	optionsGroup := FRgOutputOptionsPanel.CheckOptionsGroup;

	optionsGroup.AlignControlItems();
	FRgOutputOptionsPanel.AdjustHeight();
	// Ensure parent panel height matches the output panel plus padding
	pnlRgOutputOptions.Height := FRgOutputOptionsPanel.Height + pnlRgOutputOptions.Padding.Top + pnlRgOutputOptions.Padding.Bottom;

	// Map checkbox controls for output options
	cbRgParamPretty := optionsGroup.GetItemByCaption(RG_OUTPUT_OPTION_PRETTY_CAPTION).CheckBox;
	cbRgParamContext := optionsGroup.GetItemByCaption(RG_OUTPUT_OPTION_CONTEXT_CAPTION).CheckBox;
	seContextLineNum := optionsGroup.GetItemByCaption(RG_OUTPUT_OPTION_CONTEXT_CAPTION).SpinEdit;
	cmbOutputFormat := optionsGroup.GetItemByCaption(RG_OUTPUT_OPTION_OUTPUT_FORMAT_CAPTION).ComboBox;
end;

procedure TRipGrepperSearchDialogForm.SetMinimumWidthFromOptionsPanels();
var minWidth : Integer;
	filterPanelWidth, outputPanelWidth : Integer;
begin
	minWidth := 0;

	// Get minimum width from filter options panel
	if Assigned(FRgFilterOptionsPanel) and Assigned(FRgFilterOptionsPanel.CheckOptionsGroup) then begin
		filterPanelWidth := FRgFilterOptionsPanel.CheckOptionsGroup.GetMinimumWidth();
		minWidth := Max(minWidth, filterPanelWidth);
	end;

	// Get minimum width from output options panel
	if Assigned(FRgOutputOptionsPanel) and Assigned(FRgOutputOptionsPanel.CheckOptionsGroup) then begin
		outputPanelWidth := FRgOutputOptionsPanel.CheckOptionsGroup.GetMinimumWidth();
		minWidth := Max(minWidth, outputPanelWidth);
	end;

	// Add extra space for form borders, margins, and other controls
	if minWidth > 0 then begin
		minWidth := minWidth + 50; // Add padding for form chrome
		Constraints.MinWidth := minWidth;
	end;
end;

procedure TRipGrepperSearchDialogForm.SetAppSettingsPanel(const _settings : TRipGrepperSettings);
begin
	FAppSettingsPanel := TAppOptionsPanel.Create(self);
	FAppSettingsPanel.Settings := _settings;
	pnlBottom.Padding.Left := RG_OPTIONS_PADDING_LEFT;
	pnlBottom.Padding.Top := RG_OPTIONS_PADDING_TOP;
	FAppSettingsPanel.Parent := pnlBottom;

	FAppSettingsPanel.Align := alClient;
	FAppSettingsPanel.AddItems();
	// OnOptionChange should be set after AddItems
	FAppSettingsPanel.OnOptionChange := OnAppSettingsPanelItemSelect;
	FAppSettingsPanel.SendToBack();
end;

procedure TRipGrepperSearchDialogForm.ApplyExpertModeLayout();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.ApplyExpertModeLayout');

	// Show expert items in extension panel
	if Assigned(FExtensionContextPanel) then begin
		FExtensionContextPanel.ContextRadioGroup.ShowExpertItems();
		FExtensionContextPanel.ContextRadioGroup.AlignControlItems();
		// Set height to base normal height + expert difference
		FExtensionContextPanel.Height := FExtensionContextPanelNormalHeight + FExtensionContextPanelExpertHeightDiff;
		dbgMsg.MsgFmt('ExtensionPanel Height set to=%d (base %d + diff %d)',
			[FExtensionContextPanel.Height, FExtensionContextPanelNormalHeight, FExtensionContextPanelExpertHeightDiff]);
	end;

	// Update gbOptionsFilters height (base + difference)
	var
	expertPanelHeight := FExtensionContextPanelNormalHeight + FExtensionContextPanelExpertHeightDiff;
	gbOptionsFilters.Height := getOptionsAndFiltersHeight(False) + 
	{ } expertPanelHeight + FExtensionContextPanel.Margins.Top +
	{ }	FExtensionContextPanel.Margins.Bottom;

	// Show expert controls
	gbExpert.Visible := True;
	cmbOptions.Visible := True;

	// Allow vertical resizing in expert mode
	Constraints.MinHeight := FormNormalHeight; // Allow shrinking to normal mode height
	Constraints.MaxHeight := 0; // Allow unlimited vertical growth

	// Set form height only during initial layout (FormShow), not during user resize
	if FShowing then begin
		Height := FormNormalHeight + FormExpertHeightDiff;
	end;

	dbgMsg.MsgFmt('ExpertMode layout applied, gbOptionsFilters.Height=%d, Form.Height=%d (base %d + diff %d)',
		[gbOptionsFilters.Height, Height, FormNormalHeight, FormExpertHeightDiff]);
end;

procedure TRipGrepperSearchDialogForm.ApplyNormalModeLayout();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.ApplyNormalModeLayout');

	// Show normal items in extension panel
	if Assigned(FExtensionContextPanel) then begin
		FExtensionContextPanel.ContextRadioGroup.ShowExpertItems(False);
		FExtensionContextPanel.ContextRadioGroup.AlignControlItems();
		// Set height to pre-calculated normal mode height
		FExtensionContextPanel.Height := FExtensionContextPanelNormalHeight;
		dbgMsg.MsgFmt('ExtensionPanel Height set to=%d', [FExtensionContextPanel.Height]);
	end;

	// Update gbOptionsFilters height
	gbOptionsFilters.Height := getOptionsAndFiltersHeight(True) + FExtensionContextPanelNormalHeight + FExtensionContextPanel.Margins.Top +
		FExtensionContextPanel.Margins.Bottom;

	// Hide expert controls
	gbExpert.Visible := False;
	cmbOptions.Visible := False;

	// Fix height in normal mode (no vertical resizing)
	// Set form height to pre-calculated normal mode height
	Constraints.MinHeight := FormNormalHeight;
	Constraints.MaxHeight := FormNormalHeight;
	Height := FormNormalHeight;

	dbgMsg.MsgFmt('NormalMode layout applied, gbOptionsFilters.Height=%d, Form.Height=%d', [gbOptionsFilters.Height, Height]);
end;

function TRipGrepperSearchDialogForm.GetFormExpertHeightDiff() : Integer;
begin
	if FFormExpertHeightDiff = 0 then begin
		CalculateExpertModeHeights;
	end;
	Result := FFormExpertHeightDiff;
end;

function TRipGrepperSearchDialogForm.GetFormNormalHeight() : integer;
begin
	if FFormNormalHeight = 0 then begin
		CalculateNormalModeHeights();
	end;
	Result := FFormNormalHeight;
end;

procedure TRipGrepperSearchDialogForm.SetExtensionContextPanel(const _settings : TRipGrepperSettings);
begin
	FExtensionContextPanel := TExtensionContexPanel.Create(self);
	FExtensionContextPanel.Settings := _settings;
	FExtensionContextPanel.Parent := gbOptionsFilters;
	FExtensionContextPanel.Align := alTop;
	FExtensionContextPanel.Margins.Top := 2;
	FExtensionContextPanel.AlignWithMargins := True;
	FExtensionContextPanel.AddItems();
	FExtensionContextPanel.OnContextChange := OnContextChange;
	FExtensionContextPanel.AdjustHeight();
end;

procedure TRipGrepperSearchDialogForm.UpdateExpertModeInOptionPanels(const _bIsExpert : boolean);
begin
	FExtensionContextPanel.UpdateExpertMode(_bIsExpert);
	FRgFilterOptionsPanel.UpdateExpertMode(_bIsExpert);
	FRgOutputOptionsPanel.UpdateExpertMode(_bIsExpert);
	FAppSettingsPanel.UpdateExpertMode(_bIsExpert);

	// AdjustHeight will apply the appropriate layout mode
	// The layout methods will handle extension panel sizing
	AdjustHeight();
end;

procedure TRipGrepperSearchDialogForm.UpdateLayoutForMode();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchDialogForm.UpdateLayoutForMode');

	if FUpdateLayoutRuns then begin
		Exit;
	end;

	FUpdateLayoutRuns := True;
	try
		var
		isExpert := FSettings.AppSettings.IsExpertMode;

		dbgMsg.MsgFmt('IsExpertMode=%s', [BoolToStr(isExpert, True)]);

		if isExpert then begin
			ApplyExpertModeLayout();
		end else begin
			ApplyNormalModeLayout();
		end;
	finally
		FUpdateLayoutRuns := False;
	end;
end;

end.
