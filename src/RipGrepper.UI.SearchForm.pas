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
  RipGrepper.Common.GuiSearchParams,
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
    ActionAddParamMatchCase : TAction;
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
    btnCopyToClipBoard : TButton;
    ActionCopyToClipboard : TAction;
    btnHelpFileMask : TButton;
    ActionShowFileMaskHelp : TAction;
    btnRGOptionsHelp : TButton;
    ActionShowRGOptionsHelp : TAction;
    gbExtensionOnlyFiles : TGroupBox;
    gbView : TGroupBox;
    cbRgParamHidden : TCheckBox;
    cbRgParamNoIgnore : TCheckBox;
    cbRgParamPretty : TCheckBox;
    Action1 : TAction;
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
    procedure ActionShowRGOptionsHelpExecute(Sender : TObject);
    procedure cbRgParamHiddenClick(Sender : TObject);
    procedure cbRgParamNoIgnoreClick(Sender : TObject);
    procedure cbRgParamPrettyClick(Sender : TObject);
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
      FExpertGroupHeight : Integer;
      FGuiSetSearchParams : TGuiSearchTextParams;
      FCbClickEventEnabled : Boolean;
      FSettings : TRipGrepperSettings;

      function GetSelectedPaths(const _fdo : TFileDialogOptions) : string;
      procedure LoadSettings;
      procedure ButtonDown(const _searchOption : EGuiOption; _tb : TToolButton; const _bNotMatch : Boolean = False); overload;
      function GetFullHeight(_ctrl : TControl) : integer;
      procedure InsertOption(const _sOp : string);
      function IsOptionSet(const _sParamRegex : string; const _sParamValue : string = '') : Boolean;
      procedure ProcessControl(_ctrl : TControl; _imgList : TImageList);
      procedure SetCmbOptionText;
      procedure SetComboItemsAndText(_cmb : TComboBox; const _argName : string; const _items : TStrings);
      procedure SetComboItemsFromOptions(_cmb : TComboBox; const _argMaskRegex : string; const _items : TStrings);
      procedure UpdateGuiCtrls;
      procedure StoreHistoriesAsCmbEntries;
      procedure WriteCtrlsToRipGrepParametersSettings;
      procedure StoreSearchSettings;
      procedure UpdateCheckBoxesByRgOptions;
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
      procedure UpdateMemoCommandLine(const _bSkipReadCtrls : Boolean = False);
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
  System.SysUtils,
  System.RegularExpressions,
  System.Math,
  ArrayEx,
  RipGrepper.CommandLine.Builder,
  Vcl.Clipbrd,
  RipGrepper.CommandLine.OptionHelper,
  RipGrepper.Tools.DebugUtils,
  Winapi.ShellAPI;

const
  RIPGREPPER_SEARCH_FORM = 'RipGrepperSearchDialogForm';

  {$R *.dfm}

constructor TRipGrepperSearchDialogForm.Create(AOwner : TComponent; const _settings : TRipGrepperSettings;
    const _histObj : THistoryItemObject);
begin
  inherited Create(AOwner);
  FSettings := _settings;
  FHistItemObj := _histObj;
  if Assigned(FHistItemObj) then begin
    TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.Create: set hist obj');
    FGuiSetSearchParams := FHistItemObj.GuiSetSearchParams;
  end;
  var
    searchText : string := _settings.RipGrepParameters.SearchText;
  if not searchText.IsEmpty then begin
    _settings.SearchTextsHistory.DeleteAll([searchText]);
    _settings.SearchTextsHistory.Insert(0, searchText);
    TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.Create: new SearchText=' + searchText);
    FGuiSetSearchParams.SearchText := searchText;
    if Assigned(FHistItemObj) then begin
      FHistItemObj.RipGrepArguments.Values[RG_ARG_SEARCH_TEXT] := searchText;
      TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.Create: new rg arg=' + string.Join('',
          FHistItemObj.RipGrepArguments.GetValues(RG_ARG_SEARCH_TEXT)));
    end;

  end;

  TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.Create: gui params=' + FGuiSetSearchParams.ToString);
  FDpiScaler := TRipGrepperDpiScaler.Create(self);
  FExpertGroupHeight := gbExpert.Height;
end;

destructor TRipGrepperSearchDialogForm.Destroy;
begin
  FDpiScaler.Free;
  inherited;
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamMatchCaseExecute(Sender : TObject);
begin
  FGuiSetSearchParams.SetOrReset(EGuiOption.soMatchCase);
  UpdateGuiCtrls();
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamMatchCaseUpdate(Sender : TObject);
begin
  ButtonDown(EGuiOption.soMatchCase, tbIgnoreCase);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamRegexExecute(Sender : TObject);
begin
  FGuiSetSearchParams.SetOrReset(EGuiOption.soUseRegex);
  UpdateGuiCtrls();
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamRegexUpdate(Sender : TObject);
begin
  ButtonDown(EGuiOption.soUseRegex, tbUseRegex);
end;

procedure TRipGrepperSearchDialogForm.ActionAddParamWordExecute(Sender : TObject);
begin
  FGuiSetSearchParams.SetOrReset(EGuiOption.soMatchWord);
  UpdateGuiCtrls();
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

procedure TRipGrepperSearchDialogForm.ActionShowFileMaskHelpExecute(Sender : TObject);
begin
  ShellExecute(0, 'OPEN', PChar(WWW_LINK_GLOBBING_HELP), '', '', SW_SHOWNORMAL);
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
      cmbOptions.Text := TGuiSearchTextParams.AddRemoveRgExeOptions(
          { } FSettings.RipGrepParameters.RgExeOptions, string.Join('|', RG_NECESSARY_PARAMS + RG_GUI_SET_PARAMS), True);
      UpdateCtrls(cmbOptions);
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
  TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.FormShow');

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
  TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.LoadSettings');

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

procedure TRipGrepperSearchDialogForm.ButtonDown(const _searchOption : EGuiOption; _tb : TToolButton; const _bNotMatch : Boolean = False);
begin
  if (_bNotMatch) then begin
    _tb.Down := not(_searchOption in FGuiSetSearchParams.SearchOptions);
  end else begin
    _tb.Down := _searchOption in FGuiSetSearchParams.SearchOptions;
  end;
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

  FSettings.SearchFormSettings.Pretty := cbRgParamPretty.Checked;
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

procedure TRipGrepperSearchDialogForm.InsertOption(const _sOp : string);
begin
  FGuiSetSearchParams.RgOptions.Insert(0, _sOp + ' ').Trim;
end;

function TRipGrepperSearchDialogForm.IsOptionSet(const _sParamRegex : string; const _sParamValue : string = '') : Boolean;
begin
  Result := TOptionsHelper.IsOptionSet(FGuiSetSearchParams.RgOptions, _sParamRegex, _sParamValue);
end;

procedure TRipGrepperSearchDialogForm.SetCmbOptionText;
begin
  // Remove necessary options
  cmbOptions.Text := TGuiSearchTextParams.AddRemoveRgExeOptions(
      { } FGuiSetSearchParams.RgOptions, string.Join('|', RG_NECESSARY_PARAMS + RG_GUI_SET_PARAMS), True);
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

procedure TRipGrepperSearchDialogForm.UpdateGuiCtrls;
begin
  TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.UpdateGuiCtrls: start ' + FGuiSetSearchParams.ToString);
  SetCmbOptionText;

  TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.UpdateGuiCtrls:  ' + string.Join(' ', FGuiSetSearchParams.RgOptions));

  UpdateMemoCommandLine(True); // RgExeOptions may changed
  TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.UpdateGuiCtrls: end ' + FGuiSetSearchParams.ToString);

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
  TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.WriteCtrlsToRipGrepParametersSettings: start ' + FGuiSetSearchParams.ToString);
  FSettings.RipGrepParameters.SearchPath := cmbSearchDir.Text;
  FGuiSetSearchParams.SearchText := cmbSearchText.Text;
  // FGuiSetSearchParams.RgOptions := '';

  FGuiSetSearchParams.SetRgOptions(RG_PARAM_REGEX_HIDDEN, not cbRgParamHidden.Checked);
  FGuiSetSearchParams.SetRgOptions(RG_PARAM_REGEX_NO_IGNORE, not cbRgParamNoIgnore.Checked);
  FGuiSetSearchParams.SetRgOptions(RG_PARAM_REGEX_PRETTY, not cbRgParamPretty.Checked);

  if Fsettings.RipGrepperSettings.ExpertMode then begin
    WriteOptionCtrlToRipGrepParametersSetting;
  end;
  TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.WriteCtrlsToRipGrepParametersSettings: end ' + FGuiSetSearchParams.ToString);
end;

procedure TRipGrepperSearchDialogForm.StoreSearchSettings;
begin
  FSettings.SearchPathsHistory := cmbSearchDir.Items;
  FSettings.SearchTextsHistory := cmbSearchText.Items;
  FSettings.RipGrepOptionsHistory := cmbOptions.Items;
  FSettings.FileMasksHistory := cmbFileMasks.Items;

  WriteCtrlsToRipGrepParametersSettings;
  FSettings.RipGrepParameters.GuiSetSearchParams := FGuiSetSearchParams;
  TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.StoreSearchSettings: GuiSetSearchParams start ' + FGuiSetSearchParams.ToString);

  FSettings.RebuildArguments;
  FSettings.Store
end;

procedure TRipGrepperSearchDialogForm.UpdateCheckBoxesByRgOptions;
begin
  FCbClickEventEnabled := False;
  try
    cbRgParamHidden.Checked := IsOptionSet(RG_PARAM_REGEX_HIDDEN);
    cbRgParamNoIgnore.Checked := IsOptionSet(RG_PARAM_REGEX_NO_IGNORE);
    cbRgParamPretty.Checked := IsOptionSet(RG_PARAM_REGEX_PRETTY);
  finally
    FCbClickEventEnabled := True;
  end;
  TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.UpdateCheckBoxesByRgOptions: ' + Format('Hidden %s NoIgnore %s Pretty %s',
      [BoolToStr(cbRgParamHidden.Checked), BoolToStr(cbRgParamNoIgnore.Checked), BoolToStr(cbRgParamPretty.Checked)]));
end;

procedure TRipGrepperSearchDialogForm.UpdateMemoCommandLine(const _bSkipReadCtrls : Boolean = False);
begin
  TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.UpdateMemoCommandLine: start ' + FGuiSetSearchParams.ToString);

  if not _bSkipReadCtrls then
    WriteCtrlsToRipGrepParametersSettings;

  FSettings.RipGrepParameters.GuiSetSearchParams := FGuiSetSearchParams;
  FSettings.RebuildArguments();
  memoCommandLine.Text := FSettings.RipGrepParameters.GetCommandLine();
  FGuiSetSearchParams := FSettings.RipGrepParameters.GuiSetSearchParams;
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
  end else if (cbRgParamHidden = _ctrlChanged) or (cbRgParamNoIgnore = _ctrlChanged) or (cbRgParamPretty = _ctrlChanged) then begin
    UpdateMemoCommandLine(); // this should be done first! UpdateCtrls
  end else if cmbOptions = _ctrlChanged then begin
    UpdateMemoCommandLine(); // this should be done first! UpdateCtrls
    UpdateFileMasksInFileMasks();
    UpdateCheckBoxesByRgOptions();
  end;
end;

procedure TRipGrepperSearchDialogForm.UpdateExpertGroupBox;
begin
  if FSettings.RipGrepperSettings.ExpertMode then begin
    gbExpert.Height := FExpertGroupHeight;
    gbExpert.Caption := CAPTION_GRPBX_EXPERT_MODE;
    gbExpert.Font.Style := gbExpert.Font.Style - [fsBold, fsUnderline];
    FSettings.RipGrepperSettings.ExpertMode := True;
  end else begin
    gbExpert.Height := 0;
    gbExpert.Caption := 'Show ' + CAPTION_GRPBX_EXPERT_MODE;
    gbExpert.Font.Style := gbExpert.Font.Style + [fsBold, fsUnderline];
    FSettings.RipGrepperSettings.ExpertMode := False;
  end;
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
end;

procedure TRipGrepperSearchDialogForm.UpdateHeight;
begin
  var
  iHeight :=
  { } GetFullHeight(gbSearch) +
  { } GetFullHeight(gbOptions) +
  { } GetFullHeight(pnlSearch) - pnlSearch.Height +
  { } GetFullHeight(pnlBottom);

  var
  gbExpertHeight := GetFullHeight(gbExpert);
  iHeight := iHeight + gbExpertHeight;

  // Constraints.MaxHeight := iHeight;
  Constraints.MinHeight := iHeight;
  Height := iHeight;
end;

procedure TRipGrepperSearchDialogForm.UpdateRipGrepOptionsAndCommanLine;
begin
  UpdateMemoCommandLine(True);
  SetCmbOptionText;
  UpdateCheckBoxesByRgOptions();
end;

procedure TRipGrepperSearchDialogForm.WriteOptionCtrlToRipGrepParametersSetting;
var
  optionsText, sOp, sOpName, sVal : string;
  options : TArrayEx<string>;
begin
  optionsText := cmbOptions.Text;
  options := optionsText.Split([' '], TStringSplitOptions.ExcludeEmpty);
  for var i : integer := 0 to options.MaxIndex do begin
    sOp := options[i];
    if TOptionsHelper.IsOptionWithValue(sOp) then begin
      sVal := TOptionsHelper.GetOptionsValue(sOp, sOpName);

      if not IsOptionSet(sOpName, sVal) then begin
        InsertOption(sOpName + '=' + sVal);
      end;
      if i = options.MaxIndex then
        break;
    end else if not IsOptionSet(sOp) then begin
      InsertOption(sOp);
    end;
  end;
end;

end.
