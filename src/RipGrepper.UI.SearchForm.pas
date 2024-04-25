unit RipGrepper.UI.SearchForm;

interface

uses
	Winapi.Windows,
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
	RipGrepper.Common.Settings,
	Vcl.StdActns,
	Vcl.Dialogs,
	RipGrepper.Common.Constants,
	RipGrepper.UI.DpiScaler,
	GX_BaseForm,
	Vcl.ButtonGroup,
	Vcl.Buttons,
	Vcl.ToolWin,
	Vcl.ComCtrls,
	RipGrepper.Common.Settings.RipGrepParameterSettings,
	RipGrepper.Data.HistoryItemObject;

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
		ActionAddParamIgnoreCase : TAction;
		ActionAddParamWord : TAction;
		ActionAddParamRegex : TAction;
		ToolBar1 : TToolBar;
		tbIgnoreCase : TToolButton;
		tbMatchWord : TToolButton;
		tbUseRegex : TToolButton;
		gbExpert : TGroupBox;
		gbOptions : TGroupBox;
		lblFileMasks : TLabel;
		cmbFileMasks : TComboBox;
		Label1 : TLabel;
		memoCommandLine : TMemo;
		cbHidden : TCheckBox;
		btnCopyToClipBoard : TButton;
		ActionCopyToClipboard : TAction;
		procedure ActionAddParamIgnoreCaseExecute(Sender : TObject);
		procedure ActionAddParamIgnoreCaseUpdate(Sender : TObject);
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
		procedure cmbFileMasksChange(Sender : TObject);
		procedure cmbFileMasksExit(Sender : TObject);
		procedure cmbFileMasksSelect(Sender : TObject);
		procedure cmbOptionsExit(Sender : TObject);
		procedure cmbOptionsSelect(Sender : TObject);
		procedure cmbSearchDirChange(Sender : TObject);
		procedure cmbSearchTextChange(Sender : TObject);
		procedure ShowOptionsForm;
		procedure FormClose(Sender : TObject; var Action : TCloseAction);
		procedure FormShow(Sender : TObject);
		procedure gbExpertDblClick(Sender : TObject);

		private
			FHistItemObj : THistoryItemObject;
			FDpiScaler : TRipGrepperDpiScaler;
			FGuiSetSearchParams : TGuiSetSearchParams;
			FSettings : TRipGrepperSettings;
			FRipGrepParameters : TRipGrepParameterSettings;
			function GetSelectedPaths(const _fdo : TFileDialogOptions) : string;
			procedure LoadSettings;
			procedure AddRemoveRgExeOptions(const _sParamRegex : string; const _bRemove : Boolean = False);
			procedure ButtonDown(const _searchOption : EGuiOption; _tb : TToolButton; const _bNotMatch : Boolean = False); overload;
			function GetFullHeight(_ctrl : TControl) : integer;
			function IsOptionSet(const _sParamRegex : string; const _sParamValue : string = '') : Boolean;
			procedure RemoveRgExeOptionsText(const _paramRegex : string);
			procedure ProcessControl(_ctrl : TControl; _imgList : TImageList);
			procedure SetCmbOptionText;
			procedure SetComboItemsAndText(_cmb : TComboBox; const _argName : string; const _items : TStrings);
			procedure SetComboItemsFromOptions(_cmb : TComboBox; const _argMaskRegex : string; const _items : TStrings);
			procedure SetOption(const _searchOption : EGuiOption; const _paramRegex : string = '');
			procedure ResetOption(const _searchOption : EGuiOption; const _paramRegex : string = '');
			procedure SetOptionInGuiAndRgExeOptions(const _searchOption : EGuiOption; const _paramRegex : string = '';
				const _bReset : Boolean = False);
			procedure StoreHistoriesAsCmbEntries;
			procedure WriteCtrlsToRipGrepParametersSettings;
			procedure StoreSearchSettings;
			procedure UpdateExpertGroupBox;
			procedure UpdateFileMasksInFileMasks;
			function UpdateFileMasksInOptions(const sOptions, sMasks : string) : string; overload;
			procedure UpdateFileMasksInOptions; overload;
			procedure UpdateHeight;
			procedure WriteOptionCtrlToRipGrepParametersSetting;

		protected
		public
			constructor Create(AOwner : TComponent; const _settings : TRipGrepperSettings; const _histObj : THistoryItemObject);
				reintroduce; virtual;
			destructor Destroy; override;
			procedure UpdateCommandLine(const _bSkipReadCtrls : Boolean = False);
			procedure UpdateCtrls(_ctrlChanged : TControl);
			procedure UpdateRipGrepOptionsAndCommanLine;
	end;

var
	RipGrepperSearchDialogForm : TRipGrepperSearchDialogForm;

implementation

uses
	RipGrepper.Helper.UI,
	RipGrepper.Tools.ProcessUtils,
	System.UITypes,
	RipGrepper.UI.RipGrepOptionsForm,
	RipGrepper.Helper.Types,
	GX_OtaUtils,
	System.SysUtils,
	System.RegularExpressions,
	System.Math,
	ArrayEx,
	RipGrepper.CommandLine.Builder,
	Vcl.Clipbrd, RipGrepper.CommandLine.OptionHelper;

const
	RIPGREPPER_SEARCH_FORM = 'RipGrepperSearchDialogForm';

	{$R *.dfm}

constructor TRipGrepperSearchDialogForm.Create(AOwner : TComponent; const _settings : TRipGrepperSettings;
	const _histObj : THistoryItemObject);
begin
	inherited Create(AOwner);

	FSettings := _settings;
	FRipGrepParameters := _settings.RipGrepParameters;
	FHistItemObj := _histObj;
	if Assigned(_histObj) then
		FGuiSetSearchParams := _histObj.GuiSetSearchParams;
	FDpiScaler := TRipGrepperDpiScaler.Create(self);
end;

destructor TRipGrepperSearchDialogForm.Destroy;
begin
	FDpiScaler.Free;
	inherited;
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamIgnoreCaseExecute(Sender : TObject);
begin
	SetOption(EGuiOption.soMatchCase, RG_PARAM_REGEX_CASE_SENSITIVE);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamIgnoreCaseUpdate(Sender : TObject);
begin
	ButtonDown(EGuiOption.soMatchCase, tbIgnoreCase);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamRegexExecute(Sender : TObject);
begin
	ResetOption(EGuiOption.soUseRegex, RG_PARAM_REGEX_FIXED_STRINGS);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamRegexUpdate(Sender : TObject);
begin
	ButtonDown(EGuiOption.soUseRegex, tbUseRegex);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamWordExecute(Sender : TObject);
begin
	SetOption(EGuiOption.soMatchWord);
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
	end;
end;

procedure TRipGrepperSearchDialogForm.ActionShowRipGrepOptionsFormExecute(Sender : TObject);
begin
	ShowOptionsForm();
end;

procedure TRipGrepperSearchDialogForm.ActionSearchExecute(Sender : TObject);
begin
	StoreHistoriesAsCmbEntries();
	StoreSearchSettings();
	ModalResult := mrOk;
end;

procedure TRipGrepperSearchDialogForm.ActionSearchFileExecute(Sender : TObject);
var
	selectedFiles : string;
begin
	selectedFiles := GetSelectedPaths([fdoAllowMultiSelect]);

	if selectedFiles <> '' then begin
		cmbSearchDir.Text := selectedFiles;
	end;
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
	frm := TRipGrepOptionsForm.Create(self, FRipGrepParameters);
	try
		if (mrOk = frm.ShowModal) then begin
			cmbOptions.Text := TOptionsHelper.AddRemoveRgExeOptions(
				{ } FRipGrepParameters.RgExeOptions, string.Join('|', RG_NECESSARY_PARAMS + [RG_PARAM_REGEX_CASE_SENSITIVE,
				RG_PARAM_REGEX_FIXED_STRINGS, RG_PARAM_REGEX_GLOB, RG_PARAM_END]), True); // from options form
		end;
	finally
		frm.Free;
	end;
end;

procedure TRipGrepperSearchDialogForm.FormClose(Sender : TObject; var Action : TCloseAction);
begin
	//
end;

procedure TRipGrepperSearchDialogForm.FormShow(Sender : TObject);
begin
	LoadSettings;
	UpdateRipGrepOptionsAndCommanLine();
	UpdateExpertGroupBox();
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
			Result := dlg.Files.DelimitedText;
		end;
	finally
		dlg.Free;
	end;
end;

procedure TRipGrepperSearchDialogForm.LoadSettings;
var
	s : string;
begin
	SetComboItemsAndText(cmbSearchDir, RG_ARG_SEARCH_PATH, FSettings.SearchPathsHistory);
	SetComboItemsAndText(cmbSearchText, RG_ARG_SEARCH_TEXT, FSettings.SearchTextsHistory);
	SetComboItemsAndText(cmbOptions, RG_ARG_OPTIONS, FSettings.RipGrepOptionsHistory);
	SetComboItemsFromOptions(cmbFileMasks, RG_PARAM_REGEX_GLOB, FSettings.FileMasksHistory);

	ButtonDown(EGuiOption.soMatchCase, tbIgnoreCase);
	ButtonDown(EGuiOption.soMatchWord, tbMatchWord);
	if tbMatchWord.Down then begin
		s := cmbSearchText.Text;
		TCommandLineBuilder.RemoveWordBoundaries(s);
		cmbSearchText.Text := s;
	end;
	ButtonDown(EGuiOption.soUseRegex, tbUseRegex);
end;

procedure TRipGrepperSearchDialogForm.AddRemoveRgExeOptions(const _sParamRegex : string; const _bRemove : Boolean = False);
begin
	FRipGrepParameters.RgExeOptions := TOptionsHelper.AddRemoveRgExeOptions(FRipGrepParameters.RgExeOptions, _sParamRegex, _bRemove);
end;

procedure TRipGrepperSearchDialogForm.ButtonDown(const _searchOption : EGuiOption; _tb : TToolButton;
	const _bNotMatch : Boolean = False);
begin
	if (_bNotMatch) then begin
		_tb.Down := not(_searchOption in FGuiSetSearchParams.SearchOptions);
	end else begin
		_tb.Down := _searchOption in FGuiSetSearchParams.SearchOptions;
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

procedure TRipGrepperSearchDialogForm.cmbOptionsExit(Sender : TObject);
begin
	UpdateCtrls(cmbOptions);
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
	UpdateCtrls(cmbSearchText);
end;

procedure TRipGrepperSearchDialogForm.gbExpertDblClick(Sender : TObject);
begin
	FSettings.RipGrepperSettings.ExpertMode := not FSettings.RipGrepperSettings.ExpertMode;
	UpdateExpertGroupBox();
	UpdateHeight();
end;

function TRipGrepperSearchDialogForm.GetFullHeight(_ctrl : TControl) : integer;
begin
	Result := _ctrl.Margins.Top + _ctrl.Height + _ctrl.Margins.Bottom;
end;

function TRipGrepperSearchDialogForm.IsOptionSet(const _sParamRegex : string; const _sParamValue : string = '') : Boolean;
begin
	Result := TOptionsHelper.IsOptionSet(FRipGrepParameters.RgExeOptions, _sParamRegex, _sParamValue);
end;

procedure TRipGrepperSearchDialogForm.RemoveRgExeOptionsText(const _paramRegex : string);
begin
	AddRemoveRgExeOptions(_paramRegex, True);
end;

procedure TRipGrepperSearchDialogForm.SetCmbOptionText;
begin
	// Remove necessary options
	cmbOptions.Text := TOptionsHelper.AddRemoveRgExeOptions(
		{ } FRipGrepParameters.RgExeOptions, string.Join('|', RG_NECESSARY_PARAMS + [RG_PARAM_REGEX_CASE_SENSITIVE,
		RG_PARAM_REGEX_FIXED_STRINGS, RG_PARAM_REGEX_GLOB, RG_PARAM_END]), True);
end;

procedure TRipGrepperSearchDialogForm.SetComboItemsAndText(_cmb : TComboBox; const _argName : string; const _items : TStrings);
begin
	_cmb.Items.Assign(_items);
	if Assigned(FHistItemObj) then begin
		_cmb.Text := string.Join(' ', FHistItemObj.RipGrepArguments.GetValues(_argName));
	end else begin
		_cmb.ItemIndex := 0;
	end;
end;

procedure TRipGrepperSearchDialogForm.SetComboItemsFromOptions(_cmb : TComboBox; const _argMaskRegex : string; const _items : TStrings);
var
	params : TArray<string>;
begin
	_cmb.Items.Assign(_items);
	if Assigned(FHistItemObj) then begin
		params := FHistItemObj.RipGrepArguments.GetValues(RG_ARG_OPTIONS);
		_cmb.Text := TCommandLineBuilder.GetFileMasksDelimited(string.Join(' ', params));
	end else begin
		_cmb.ItemIndex := 0;
	end;
end;

procedure TRipGrepperSearchDialogForm.SetOption(const _searchOption : EGuiOption; const _paramRegex : string = '');
var
	bIsOpSet : Boolean;
begin
	bIsOpSet := not _paramRegex.IsEmpty and IsOptionSet(_paramRegex);
	if not bIsOpset then begin
		SetOptionInGuiAndRgExeOptions(_searchOption, _paramRegex);
	end;

end;

procedure TRipGrepperSearchDialogForm.ResetOption(const _searchOption : EGuiOption; const _paramRegex : string = '');
var
	bIsOpNotSet : Boolean;
begin
	bIsOpNotSet := not _paramRegex.IsEmpty and (not IsOptionSet(_paramRegex));
	if not bIsOpNotSet then begin
		SetOptionInGuiAndRgExeOptions(_searchOption, _paramRegex, True);
	end;
end;

procedure TRipGrepperSearchDialogForm.SetOptionInGuiAndRgExeOptions(const _searchOption : EGuiOption;
	const _paramRegex : string = ''; const _bReset : Boolean = False);
begin

	FRipGrepParameters.RgExeOptions := TOptionsHelper.SetFlagAndOption(_searchOption, _paramRegex, FRipGrepParameters.RgExeOptions,
		FGuiSetSearchParams, _bReset);

	UpdateCommandLine(True); // RgExeOptions may changed
	SetCmbOptionText;
end;

procedure TRipGrepperSearchDialogForm.StoreHistoriesAsCmbEntries;
begin
	TItemInserter.AddTextToItemsIfNotContains(cmbOptions);
	TItemInserter.AddTextToItemsIfNotContains(cmbSearchDir);
	TItemInserter.AddTextToItemsIfNotContains(cmbSearchText);
	TItemInserter.AddTextToItemsIfNotContains(cmbFileMasks);
end;

procedure TRipGrepperSearchDialogForm.WriteCtrlsToRipGrepParametersSettings;
begin
	FRipGrepParameters.SearchPath := cmbSearchDir.Text;

	FGuiSetSearchParams.SearchText := cmbSearchText.Text;

	if Fsettings.RipGrepperSettings.ExpertMode then begin
		WriteOptionCtrlToRipGrepParametersSetting;
	end else begin
		FRipGrepParameters.RgExeOptions := '';
	end;
end;

procedure TRipGrepperSearchDialogForm.StoreSearchSettings;
begin
	FSettings.SearchPathsHistory := cmbSearchDir.Items;
	FSettings.SearchTextsHistory := cmbSearchText.Items;
	FSettings.RipGrepOptionsHistory := cmbOptions.Items;
	FSettings.FileMasksHistory := cmbFileMasks.Items;

	WriteCtrlsToRipGrepParametersSettings;
	FRipGrepParameters.GuiSetSearchParams := FGuiSetSearchParams;

	FSettings.RebuildArguments;
	FSettings.Store
end;

procedure TRipGrepperSearchDialogForm.UpdateCommandLine(const _bSkipReadCtrls : Boolean = False);
begin
	if not _bSkipReadCtrls then
		WriteCtrlsToRipGrepParametersSettings;

	FRipGrepParameters.GuiSetSearchParams := FGuiSetSearchParams;
	FSettings.RebuildArguments();
	memoCommandLine.Text := FRipGrepParameters.GetCommandLine();
end;

procedure TRipGrepperSearchDialogForm.UpdateCtrls(_ctrlChanged : TControl);
begin
	if cmbSearchText = _ctrlChanged then begin
		UpdateCommandLine(); // UpdateCtrls
	end else if cmbSearchDir = _ctrlChanged then begin
		UpdateCommandLine(); // UpdateCtrls
	end else if cmbFileMasks = _ctrlChanged then begin
		UpdateFileMasksInOptions();
		UpdateCommandLine(); // UpdateCtrls
	end else if cmbOptions = _ctrlChanged then begin
		UpdateCommandLine(); // UpdateCtrls
		UpdateFileMasksInFileMasks();
	end;
end;

procedure TRipGrepperSearchDialogForm.UpdateExpertGroupBox;
begin
	if FSettings.RipGrepperSettings.ExpertMode then begin
		gbExpert.Height := GROUPBOX_EXPERT_HEIGHT;
		gbExpert.Caption := EXPERT_GRPBX_CAPTIONS;
		FSettings.RipGrepperSettings.ExpertMode := True;
	end else begin
		gbExpert.Height := 20;
		gbExpert.Caption := 'Show ' + EXPERT_GRPBX_CAPTIONS;
		FSettings.RipGrepperSettings.ExpertMode := False;
	end;
end;

procedure TRipGrepperSearchDialogForm.UpdateFileMasksInFileMasks;
begin
	cmbFileMasks.Text := TCommandLineBuilder.GetFileMasksDelimited(FRipGrepParameters.RgExeOptions);
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
	FRipGrepParameters.RgExeOptions := UpdateFileMasksInOptions(FRipGrepParameters.RgExeOptions, cmbFileMasks.Text);
end;

procedure TRipGrepperSearchDialogForm.UpdateHeight;
begin
	var
	iHeight :=
	{ } GetFullHeight(gbSearch) +
	{ } GetFullHeight(gbOptions) +
	{ } GetFullHeight(pnlSearch) - pnlSearch.Height +
	{ } GetFullHeight(pnlBottom) +
	{ } GetFullHeight(gbExpert);

	// Constraints.MaxHeight := iHeight;
	Constraints.MinHeight := iHeight;
	Height := iHeight;
end;

procedure TRipGrepperSearchDialogForm.UpdateRipGrepOptionsAndCommanLine;
begin
	UpdateCommandLine();
	SetCmbOptionText;
end;

procedure TRipGrepperSearchDialogForm.WriteOptionCtrlToRipGrepParametersSetting;
var
	optionsText, sOp, sOpName, sVal : string;
	options : TArrayEx<string>;
begin
	FRipGrepParameters.RgExeOptions := '';
	optionsText := cmbOptions.Text;
	options := optionsText.Split([' '], TStringSplitOptions.ExcludeEmpty);
	for var i : integer := 0 to options.MaxIndex do begin
		sOp := options[i];
		if TOptionsHelper.IsOptionWithValue(sOp) then begin
			sVal := TOptionsHelper.GetOptionsValue(sOp, sOpName);

			if not IsOptionSet(sOp, sVal) then begin
				FRipGrepParameters.RgExeOptions.Insert(0, sOpName + '=' + sVal + ' ').Trim;
			end;
			if i = options.MaxIndex then
				break;
		end else if not IsOptionSet(sOp) then begin
			FRipGrepParameters.RgExeOptions.Insert(0, sOp + ' ').Trim;
		end;
	end;
end;

end.
