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
		procedure cmbFileMasksExit(Sender : TObject);
		procedure cmbOptionsExit(Sender : TObject);
		procedure ShowOptionsForm;
		procedure FormClose(Sender : TObject; var Action : TCloseAction);
		procedure FormShow(Sender : TObject);

		private
			FActualRipGrepParams : TRipGrepArguments;
			FDpiScaler : TRipGrepperDpiScaler;
			FSettings : TRipGrepperSettings;
			function GetSelectedPaths(const _fdo : TFileDialogOptions) : string;
			procedure LoadSettings;
			procedure AddRgOption(const _paramRegex : string; const _bRemove : Boolean = False);
			procedure ButtonDown(const _paramRegex : string; _tb : TToolButton; const _bNotMatch : Boolean = False);
			function GetFullHeight(_ctrl : TControl) : integer;
			function OptionIsSet(const _paramRegex : string) : Boolean;
			procedure RemoveRgOption(const _paramRegex : string);
			procedure ProcessControl(_ctrl : TControl; _imgList : TImageList);
			procedure SetComboItemsAndText(_cmb : TComboBox; const _argName : string; const _items : TStrings);
			procedure SetComboItemsFromOptions(_cmb : TComboBox; const _argMaskRegex : string; const _items : TStrings);
			procedure SetOption(const _paramRegex : string);
			procedure StoreHistoriesAsCmbEntries;
			procedure StoreSearchSettings;
			function UpdateFileMasksInOptions(const sOptions, sMasks: string): string;

		protected
		public
			constructor Create(AOwner : TComponent; const _settings : TRipGrepperSettings; const _actualArgs : TRipGrepArguments);
				reintroduce; virtual;
			destructor Destroy; override;
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
	ArrayEx;

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
begin
	SetOption(RG_PARAM_REGEX_WORD_REGEX);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamWordUpdate(Sender : TObject);
begin
	ButtonDown(RG_PARAM_REGEX_WORD_REGEX, tbMatchWord);
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
			cmbOptions.Text := FSettings.RipGrepParameters.Options;
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
	gbExpert.Visible := FSettings.RipGrepperSettings.ExpertMode;

	Height :=
	{ } GetFullHeight(gbSearch) +
	{ } GetFullHeight(gbOptions) +
	{ } GetFullHeight(pnlSearch) - pnlSearch.Height +
	{ } GetFullHeight(pnlBottom) +
	{ } IfThen(gbExpert.Visible, GetFullHeight(gbExpert));

	Constraints.MinHeight := Height;
	Constraints.MaxHeight := Height;
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

	SetComboItemsFromOptions(cmbFileMasks, RG_PARAM_REGEX_GLOB, FSettings.FileMasksHistory);
end;

procedure TRipGrepperSearchDialogForm.AddRgOption(const _paramRegex : string; const _bRemove : Boolean = False);
var
	list : TStringList;
	options : string;
	params : TArray<string>;
	bFoundIdx : integer;
begin
	list := TStringList.Create(dupIgnore, False, True);
	list.Delimiter := ' ';
	try
		bFoundIdx := -1;
		options := cmbOptions.Text;

		list.AddStrings(options.Split([' ']));

		params := _paramRegex.Split(['|']);
		for var p in params do begin
			bFoundIdx := list.IndexOf(p);
			if (bFoundIdx >= 0) then begin
				break; // Already has
			end;
		end;

		if (bFoundIdx < 0) then begin
			list.Add(params[1]); // long params
		end else if (_bRemove) then begin
			list.Delete(bFoundIdx);
		end;

		cmbOptions.Text := list.DelimitedText;
	finally
		list.Free;
	end;
end;

procedure TRipGrepperSearchDialogForm.ButtonDown(const _paramRegex : string; _tb : TToolButton; const _bNotMatch : Boolean = False);
begin
	if (_bNotMatch) then begin
		_tb.Down := not OptionIsSet(_paramRegex)
	end else begin
		_tb.Down := OptionIsSet(_paramRegex);
	end;
end;

procedure TRipGrepperSearchDialogForm.cmbFileMasksExit(Sender : TObject);
begin
	cmbOptions.Text := UpdateFileMasksInOptions(cmbOptions.Text, cmbFileMasks.Text);
end;

procedure TRipGrepperSearchDialogForm.cmbOptionsExit(Sender : TObject);
begin
	cmbFileMasks.Text := TRipGrepParameterSettings.GetFileMasksDelimited(cmbOptions.Text, RG_PARAM_REGEX_GLOB);
end;

function TRipGrepperSearchDialogForm.GetFullHeight(_ctrl : TControl) : integer;
begin
	Result := _ctrl.Margins.Top + _ctrl.Height + _ctrl.Margins.Bottom;
end;

function TRipGrepperSearchDialogForm.OptionIsSet(const _paramRegex : string) : Boolean;
var
	regex : TRegEx;
begin
	regex := TRegEx.Create(_paramRegex);
	Result := regex.IsMatch(cmbOptions.Text);
end;

procedure TRipGrepperSearchDialogForm.RemoveRgOption(const _paramRegex : string);
begin
	AddRgOption(_paramRegex, True);
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
	if Assigned(FActualRipGrepParams) then begin
		params := FActualRipGrepParams.GetValues(RG_ARG_OPTIONS);
		_cmb.Text := TRipGrepParameterSettings.GetFileMasksDelimited(string.Join(' ', params), _argMaskRegex);
	end else begin
		_cmb.ItemIndex := 0;
	end;
end;

procedure TRipGrepperSearchDialogForm.SetOption(const _paramRegex : string);
begin
	if OptionIsSet(_paramRegex) then begin
		RemoveRgOption(_paramRegex)
	end else begin
		AddRgOption(_paramRegex);
	end;
end;

procedure TRipGrepperSearchDialogForm.StoreHistoriesAsCmbEntries;
begin
	TItemInserter.AddTextToItemsIfNotContains(cmbOptions);
	TItemInserter.AddTextToItemsIfNotContains(cmbSearchDir);
	TItemInserter.AddTextToItemsIfNotContains(cmbSearchText);
	TItemInserter.AddTextToItemsIfNotContains(cmbFileMasks);
end;

procedure TRipGrepperSearchDialogForm.StoreSearchSettings;
begin
	FSettings.SearchPathsHistory := cmbSearchDir.Items;
	FSettings.RipGrepParameters.SearchPath := cmbSearchDir.Text;

	FSettings.SearchTextsHistory := cmbSearchText.Items;
	FSettings.RipGrepParameters.SearchText := cmbSearchText.Text;

	FSettings.RipGrepParamsHistory := cmbOptions.Items;
	FSettings.RipGrepParameters.Options := cmbOptions.Text;

	FSettings.FileMasksHistory := cmbFileMasks.Items;

	FSettings.ReBuildArguments;
	FSettings.Store
end;

function TRipGrepperSearchDialogForm.UpdateFileMasksInOptions(const sOptions, sMasks: string): string;
var
	oldMaskOptions : TArrayEx<string>;
	newOptions : string;
begin
	Result := sOptions;
	oldMaskOptions := TRipGrepParameterSettings.GetFileMaskParamsFromOptions(sOptions);
	newOptions := TRipGrepParameterSettings.GetMissingFileMaskOptions(sOptions, sMasks);

	if not newOptions.IsEmpty then begin
		Result := TRipGrepParameterSettings.RemoveAllParams(sOptions, RG_PARAM_REGEX_GLOB) + ' ' + newOptions.Trim([' ']);
	end;

end;

end.
