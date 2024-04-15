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
	Vcl.ComCtrls;

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
		procedure ActionAddParamIgnoreCaseExecute(Sender : TObject);
		procedure ActionAddParamIgnoreCaseUpdate(Sender : TObject);
		procedure ActionAddParamRegexExecute(Sender : TObject);
		procedure ActionAddParamRegexUpdate(Sender : TObject);
		procedure ActionAddParamWordExecute(Sender : TObject);
		procedure ActionAddParamWordUpdate(Sender : TObject);
		procedure ActionCancelExecute(Sender : TObject);
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
			FActualRipGrepParams : TRipGrepArguments;
			FDpiScaler : TRipGrepperDpiScaler;
			FSettings : TRipGrepperSettings;
			function GetSelectedPaths(const _fdo : TFileDialogOptions) : string;
			procedure LoadSettings;
			function UpdateRgExeOptions(const _sParamRegex : string = ''; const _bRemove : Boolean = False) : string;
			procedure ButtonDown(const _paramRegex : string; _tb : TToolButton; const _bNotMatch : Boolean = False);
			function GetFullHeight(_ctrl : TControl) : integer;
			function OptionIsSet(const _paramRegex : string) : Boolean;
			procedure RemoveRgExeOptions(const _paramRegex : string);
			procedure ProcessControl(_ctrl : TControl; _imgList : TImageList);
			procedure SetComboItemsAndText(_cmb : TComboBox; const _argName : string; const _items : TStrings);
			procedure SetComboItemsFromOptions(_cmb : TComboBox; const _argMaskRegex : string; const _items : TStrings);
			procedure SetOption(const _paramRegex : string);
			procedure StoreHistoriesAsCmbEntries;
			procedure WriteCtrlsToRipGrepParametersSettings;
			procedure StoreSearchSettings;
			procedure UpdateExpertGroupBox;
			procedure UpdateFileMasksInFileMasks;
			function UpdateFileMasksInOptions(const sOptions, sMasks : string) : string; overload;
			procedure UpdateFileMasksInOptions; overload;
			procedure UpdateHeight;

		protected
		public
			constructor Create(AOwner : TComponent; const _settings : TRipGrepperSettings; const _actualArgs : TRipGrepArguments);
				reintroduce; virtual;
			destructor Destroy; override;
			procedure UpdateCommandLine;
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
	RipGrepper.Common.Settings.RipGrepParameterSettings,
	RipGrepper.Common.CommandLineBuilder;

const
	RIPGREPPER_SEARCH_FORM = 'RipGrepperSearchDialogForm';

	{$R *.dfm}

constructor TRipGrepperSearchDialogForm.Create(AOwner : TComponent; const _settings : TRipGrepperSettings;
	const _actualArgs : TRipGrepArguments);
begin
	inherited Create(AOwner);

	FSettings := _settings;
	FActualRipGrepParams := _actualArgs;
	FDpiScaler := TRipGrepperDpiScaler.Create(self);
end;

destructor TRipGrepperSearchDialogForm.Destroy;
begin
	FDpiScaler.Free;
	inherited;
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamIgnoreCaseExecute(Sender : TObject);
begin
	SetOption(RG_PARAM_REGEX_IGNORE_CASE);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamIgnoreCaseUpdate(Sender : TObject);
begin
	ButtonDown(RG_PARAM_REGEX_IGNORE_CASE, tbIgnoreCase);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamRegexExecute(Sender : TObject);
begin
	SetOption(RG_PARAM_REGEX_FIXED_STRINGS);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamRegexUpdate(Sender : TObject);
begin
	ButtonDown(RG_PARAM_REGEX_FIXED_STRINGS, tbUseRegex, true);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamWordExecute(Sender : TObject);
var
	sSearchText : string;
begin
	FSettings.RipGrepParameters.MatchWholeWord := not FSettings.RipGrepParameters.MatchWholeWord;

	sSearchText := FSettings.RipGrepParameters.RipGrepArguments.Values[RG_ARG_SEARCH_TEXT];

	if FSettings.RipGrepParameters.MatchWholeWord then begin
		if TCommandLineBuilder.IsWordBoundOnBothSide(sSearchText) then begin
			exit;
		end;
	end else if TCommandLineBuilder.IsWordBoundOnBothSide(sSearchText) then begin

	end;
	var
	params := FSettings.RipGrepParameters;
	TCommandLineBuilder.RebuildArguments(params);

	UpdateRgExeOptions(RG_PARAM_REGEX_FIXED_STRINGS, FSettings.RipGrepParameters.MatchWholeWord);
	UpdateCommandLine();
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamWordUpdate(Sender : TObject);
begin
	tbMatchWord.Enabled := not TCommandLineBuilder.IsWordBoundOnOneSide(cmbSearchText.Text);
	tbMatchWord.Down := FSettings.RipGrepParameters.MatchWholeWord;
end;

procedure TRipGrepperSearchDialogForm.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
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
	frm := TRipGrepOptionsForm.Create(self, FSettings.RipGrepParameters);
	try
		if (mrOk = frm.ShowModal) then begin
			cmbOptions.Text := FSettings.RipGrepParameters.RgExeOptions;
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
begin
	SetComboItemsAndText(cmbSearchDir, RG_ARG_SEARCH_PATH, FSettings.SearchPathsHistory);
	SetComboItemsAndText(cmbSearchText, RG_ARG_SEARCH_TEXT, FSettings.SearchTextsHistory);
	SetComboItemsAndText(cmbOptions, RG_ARG_OPTIONS, FSettings.RipGrepParamsHistory);
	// SetComboItemsAndText(cmbFileMasks, RG_ARG_OPTIONS, FSettings.FileMasksHistory);

	SetComboItemsFromOptions(cmbFileMasks, RG_PARAM_REGEX_GLOB, FSettings.FileMasksHistory);
end;

function TRipGrepperSearchDialogForm.UpdateRgExeOptions(const _sParamRegex : string = ''; const _bRemove : Boolean = False) : string;
begin
	cmbOptions.Text := TCommandLineBuilder.UpdateRgExeOptions(cmbOptions.Text, _sParamRegex, _bRemove);
end;

procedure TRipGrepperSearchDialogForm.ButtonDown(const _paramRegex : string; _tb : TToolButton; const _bNotMatch : Boolean = False);
begin
	if (_bNotMatch) then begin
		_tb.Down := not OptionIsSet(_paramRegex)
	end else begin
		_tb.Down := OptionIsSet(_paramRegex);
	end;
end;

procedure TRipGrepperSearchDialogForm.cmbFileMasksChange(Sender : TObject);
begin
	UpdateCommandLine();
end;

procedure TRipGrepperSearchDialogForm.cmbFileMasksExit(Sender : TObject);
begin
	UpdateFileMasksInOptions;
end;

procedure TRipGrepperSearchDialogForm.cmbFileMasksSelect(Sender : TObject);
begin
	UpdateFileMasksInOptions;
end;

procedure TRipGrepperSearchDialogForm.cmbOptionsExit(Sender : TObject);
begin
	UpdateFileMasksInFileMasks;
end;

procedure TRipGrepperSearchDialogForm.cmbOptionsSelect(Sender : TObject);
begin
	UpdateFileMasksInFileMasks;
end;

procedure TRipGrepperSearchDialogForm.cmbSearchDirChange(Sender : TObject);
begin
	UpdateCommandLine();
end;

procedure TRipGrepperSearchDialogForm.cmbSearchTextChange(Sender : TObject);
begin
	UpdateCommandLine();
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

function TRipGrepperSearchDialogForm.OptionIsSet(const _paramRegex : string) : Boolean;
begin
	Result := TRegEx.IsMatch(cmbOptions.Text, _paramRegex);
end;

procedure TRipGrepperSearchDialogForm.RemoveRgExeOptions(const _paramRegex : string);
begin
	UpdateRgExeOptions(_paramRegex, True);
	UpdateCommandLine();
end;

procedure TRipGrepperSearchDialogForm.SetComboItemsAndText(_cmb : TComboBox; const _argName : string; const _items : TStrings);
begin
	_cmb.Items.Assign(_items);
	if Assigned(FActualRipGrepParams) then begin
		_cmb.Text := string.Join(' ', FActualRipGrepParams.GetValues(_argName));
	end else begin
		_cmb.ItemIndex := 0;
	end;
end;

procedure TRipGrepperSearchDialogForm.SetComboItemsFromOptions(_cmb : TComboBox; const _argMaskRegex : string; const _items : TStrings);
var
	params : TArray<string>;
begin
	_cmb.Items.Assign(_items);
	if Assigned(FActualRipGrepParams) then begin
		params := FActualRipGrepParams.GetValues(RG_ARG_OPTIONS);
		_cmb.Text := TCommandLineBuilder.GetFileMasksDelimited(string.Join(' ', params), _argMaskRegex);
	end else begin
		_cmb.ItemIndex := 0;
	end;
end;

procedure TRipGrepperSearchDialogForm.SetOption(const _paramRegex : string);
begin
	if OptionIsSet(_paramRegex) then begin
		RemoveRgExeOptions(_paramRegex)
	end else begin
		UpdateRgExeOptions(_paramRegex);
	end;
	UpdateCommandLine();
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
	FSettings.RipGrepParameters.SearchPath := cmbSearchDir.Text;
	FSettings.RipGrepParameters.SearchText := cmbSearchText.Text;
	if Fsettings.RipGrepperSettings.ExpertMode then begin
		FSettings.RipGrepParameters.RgExeOptions := cmbOptions.Text;
	end else begin
		FSettings.RipGrepParameters.RgExeOptions := '';
	end;
end;

procedure TRipGrepperSearchDialogForm.StoreSearchSettings;
begin
	FSettings.SearchPathsHistory := cmbSearchDir.Items;
	FSettings.SearchTextsHistory := cmbSearchText.Items;
	FSettings.RipGrepParamsHistory := cmbOptions.Items;
	FSettings.FileMasksHistory := cmbFileMasks.Items;

	WriteCtrlsToRipGrepParametersSettings;

	FSettings.RebuildArguments;
	FSettings.Store
end;

procedure TRipGrepperSearchDialogForm.UpdateCommandLine;
begin
	WriteCtrlsToRipGrepParametersSettings;
	FSettings.RebuildArguments();
	memoCommandLine.Text := FSettings.RipGrepParameters.GetCommandLine();
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
	cmbFileMasks.Text := TCommandLineBuilder.GetFileMasksDelimited(cmbOptions.Text, RG_PARAM_REGEX_GLOB);
end;

function TRipGrepperSearchDialogForm.UpdateFileMasksInOptions(const sOptions, sMasks : string) : string;
var
	oldMaskOptions : TArrayEx<string>;
	newMaskOptions : string;
begin
	Result := sOptions;
	oldMaskOptions := TCommandLineBuilder.GetFileMaskParamsFromOptions(sOptions);
	newMaskOptions := TCommandLineBuilder.GetFileMaskParamsFromDelimitedText(sMasks, ';');

	Result := TCommandLineBuilder.RemoveAllParams(sOptions, RG_PARAM_REGEX_GLOB);
	Result := Result + ' ' + newMaskOptions.Trim([' ']);

end;

procedure TRipGrepperSearchDialogForm.UpdateFileMasksInOptions;
begin
	cmbOptions.Text := UpdateFileMasksInOptions(cmbOptions.Text, cmbFileMasks.Text);
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

	Constraints.MaxHeight := iHeight;
	Constraints.MinHeight := iHeight;
	Height := iHeight;
end;

procedure TRipGrepperSearchDialogForm.UpdateRipGrepOptionsAndCommanLine;
begin
	UpdateRgExeOptions();
	UpdateCommandLine();
end;

end.
