unit RipGrepper.UI.TopFrame;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	Vcl.ComCtrls,
	Vcl.ToolWin,
	System.ImageList,
	Vcl.ImgList,
	System.Actions,
	Vcl.ActnList,
	RipGrepper.UI.MiddleFrame,
	RipGrepper.UI.DpiScaler,
	Vcl.StdCtrls,
	Vcl.WinXCtrls,
	VirtualTrees,
	Vcl.ExtCtrls,
	Vcl.Menus,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Tools.Replacer,
	RipGrepper.UI.IFrameEvents,
	SVGIconImageListBase,
	SVGIconImageList,
	RipGrepper.UI.Components.HistoryButtonedEdit,
	Spring;

type

	TRipGrepperTopFrame = class(TFrame, IFrameEvents)
		ActionList : TActionList;
		ActionSearch : TAction;
		ActionConfig : TAction;
		ActionShowRelativePath : TAction;
		ActionCmdLineCopy : TAction;
		ActionShowSearchForm : TAction;
		ActionShowFileIcons : TAction;
		ActionAlternateRowColors : TAction;
		ActionAbortSearch : TAction;
		ActionRefreshSearch : TAction;
		ActionIndentLine : TAction;
		tbarSearch : TToolBar;
		tbShowSearchForm : TToolButton;
		tbRefreshSearch : TToolButton;
		ToolButton3 : TToolButton;
		tbAbortSearch : TToolButton;
		ToolButton1 : TToolButton;
		tbCopyCmdLine : TToolButton;
		tbAlternateRowColors : TToolButton;
		tbShowFileIcon : TToolButton;
		tbShowRelativePath : TToolButton;
		tbIndentLines : TToolButton;
		ToolButton4 : TToolButton;
		tbOpenWith : TToolButton;
		ToolButton6 : TToolButton;
		tbConfigure : TToolButton;
		tbExpandCollapse : TToolButton;
		ActionExpandCollapse : TAction;
		ToolButton7 : TToolButton;
		tbarResult : TToolBar;
		tbarConfig : TToolBar;
		PopupMenuToolBar : TPopupMenu;
		AlignToolbar1 : TMenuItem;
		ToolButton2 : TToolButton;
		ToolButton5 : TToolButton;
		ToolButton8 : TToolButton;
		ActionSearchInResult : TAction;
		edtFilter : THistoryButtonedEdit;
		edtReplace : THistoryButtonedEdit;
		ToolButton9 : TToolButton;
		tbSaveReplacement : TToolButton;
		ActionSaveReplacement : TAction;
		ActionSaveAllReplacement : TAction;
		ActionSetFileFilterMode : TAction;
		ActionSetTextFilterMode : TAction;
		ActionSetFilterModeCaseSensitive : TAction;
		ActionSetFilterModeRegex : TAction;
		miSetFileFilterMode : TMenuItem;
		miSetTextFilterMode : TMenuItem;
		N1 : TMenuItem;
		miFilterModeCaseSensitive : TMenuItem;
		miFilterModeUseRegex : TMenuItem;
		PopupMenuReplace : TPopupMenu;
		mniCaseSensitive : TMenuItem;
		mniUseRegex : TMenuItem;
		ActionReplaceCaseSensitive : TAction;
		ActionReplaceUseRegex : TAction;
		SvgImgLstTopFrame : TSVGIconImageList;
		pnlTop : TPanel;
		procedure ActionAbortSearchExecute(Sender : TObject);
		procedure ActionAlignToolbarsExecute(Sender : TObject);
		procedure ActionAlternateRowColorsExecute(Sender : TObject);
		procedure ActionAlternateRowColorsUpdate;
		procedure ActionCmdLineCopyExecute(Sender : TObject);
		procedure ActionConfigExecute(Sender : TObject);
		procedure ActionCopyFileNameExecute(Sender : TObject);
		procedure ActionCopyPathToClipboardExecute(Sender : TObject);
		procedure ActionExpandCollapseExecute(Sender : TObject);
		procedure ActionExpandCollapseUpdate;
		procedure ActionIndentLineExecute(Sender : TObject);
		procedure ActionIndentLineUpdate;
		procedure ActionOpenWithExecute(Sender : TObject);
		procedure ActionRefreshSearchExecute(Sender : TObject);
		procedure ActionRefreshSearchUpdate(Sender : TObject);
		procedure ActionReplaceCaseSensitiveExecute(Sender : TObject);
		procedure ActionReplaceUseRegexExecute(Sender : TObject);
		procedure ActionSaveReplacementExecute(Sender : TObject);
		procedure ActionSaveReplacementUpdate(Sender : TObject);
		procedure ActionSearchExecute(Sender : TObject);
		procedure ActionSearchInResultExecute(Sender : TObject);
		procedure ActionSetFileFilterModeExecute(Sender : TObject);
		procedure ActionSetFilterModeCaseSensitiveExecute(Sender : TObject);
		procedure ActionSetFilterModeRegexExecute(Sender : TObject);
		procedure ActionSetTextFilterModeExecute(Sender : TObject);
		procedure ActionShowFileIconsExecute(Sender : TObject);
		procedure ActionShowFileIconsUpdate;
		procedure ActionShowRelativePathExecute(Sender : TObject);
		procedure ActionShowRelativePathUpdate;
		procedure ActionShowSearchFormExecute(Sender : TObject);
		procedure edtFilterChange(Sender : TObject);
		procedure edtFilterKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
		procedure edtFilterRightButtonClick(Sender : TObject);
		procedure edtReplaceChange(Sender : TObject);
		procedure edtReplaceKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
		procedure edtReplaceRightButtonClick(Sender : TObject);
		procedure SearchBox1Change(Sender : TObject);

		private
			FDpiScaler : TRipGrepperDpiScaler;
			FFilterMode : TFilterModes;
			FGuiReplaceModes : TGuiReplaceModes;
			FHistItemObj : IHistoryItemObject;
			FIsInitialized : Boolean;
			FPrevFoundNode : PVirtualNode;
			FSettings : TRipGrepperSettings;
			FSkipButtonEditChange : Boolean;
			FViewStyleIndex : integer;
			FReplaceList : IShared<TReplaceList>;
			procedure ChangeButtonedEditTextButSkipChangeEvent(_edt : TButtonedEdit; const _txt : string);
			procedure GetCheckedReplaceList();
			function GetIsGuiReplaceMode : Boolean;
			function GetIsInitialized() : Boolean;
			function GetSettings : TRipGrepperSettings;
			function GetToolBarWidth(_tb : TToolBar) : Integer;
			procedure HandleReplaceErrors(const failReplaceData : TFailedReplaceData);
			function IsFilterOn : Boolean;
			function SaveSelectedReplacements() : ESaveReplacementResult;
			procedure SelectNextFoundNode(const _prevFoundNode : PVirtualNode; const _searchPattern : string);
			procedure SetFilterMode(const _fm : EFilterMode; const _bReset : Boolean = False);
			procedure SetGuiReplaceModes(const _bOn : Boolean = True);
			procedure SetReplaceModeOnGui;
			procedure SetReplaceModeOnToolBar;
			procedure SetReplaceTextInSettings(const _sReplText : string);
			class function ShowWarningBeforeSave(_list : TReplaceList) : Boolean;
			procedure StartNewSearch;
			procedure ToggleFilterMode(const _fm : EFilterMode);
			procedure ToggleGuiReplaceMode(const _grm : EGuiReplaceMode);
			procedure UpdateFilterEditMenuAndHint;
			procedure UpdateReplaceMenu;
			procedure WMSettingChange(var Message : TWMSettingChange); message WM_SETTINGCHANGE;
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

		protected
			procedure ChangeScale(M, D : Integer; isDpiChange : Boolean); override;

		public
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;
			procedure AfterHistObjChange;
			procedure AfterSearch;
			procedure AlignToolBars(iTbResultLeft, iSearchMaxWidth, iResultMinWidth : integer);
			procedure BeforeSearch(var _bAbort : Boolean);
			function GetNextViewStyleIdx : integer;
			function GetReplaceMode : TReplaceModes;
			procedure Initialize();
			function IsRgReplaceMode : Boolean;
			procedure SearchForText(Sender : TBaseVirtualTree; Node : PVirtualNode; Data : Pointer; var Abort : Boolean);
			procedure SetFilterBtnImage(const _bOn : Boolean = True);
			procedure SetGuiReplaceMode(const _modes : TGuiReplaceModes; const _sReplaceText : string = '');
			procedure UpdateUIStyle(_sNewStyle : string = '');
			property IsGuiReplaceMode : Boolean read GetIsGuiReplaceMode;
			property IsInitialized : Boolean read GetIsInitialized;

	end;

var
	TopFrame : TRipGrepperTopFrame;

implementation

{$R *.dfm}

uses
	RipGrepper.UI.MainForm,
	RipGrepper.UI.ParentFrame,
	Vcl.Clipbrd,
	RipGrepper.Common.Constants,
	RipGrepper.OpenWith.ConfigForm,
	RipGrepper.OpenWith.Params,
	RipGrepper.OpenWith,
	RipGrepper.Helper.UI,
	RipGrepper.UI.SearchForm,
	System.Math,
	System.StrUtils,
	RipGrepper.Settings.RipGrepParameterSettings,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.UI.RipGrepOptionsForm,
	RipGrepper.Common.ParsedObject,
	RipGrepper.Common.NodeData,
	System.IOUtils,
	RipGrepper.Helper.Types,
	System.SysUtils,
	RipGrepper.UI.Settings.ConfigForm,
	System.RegularExpressions,
	{$IFNDEF STANDALONE}
	RipGrepper.Common.IOTAUtils,
	RipGrepper.Common.IOTAFileUtils,
	{$ENDIF}
	RipGrepper.Settings.NodeLook.FilterSettings,
	RipGrepper.Helper.UI.DarkMode,
	Winapi.UxTheme,
	System.TypInfo,
	Vcl.Themes,
	ArrayEx;

constructor TRipGrepperTopFrame.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	FDpiScaler := TRipGrepperDpiScaler.Create(self, False);
	TopFrame := self;
	FGuiReplaceModes := []; // [EGuiReplaceMode.grmEditEnabled];
	FFilterMode := [EFilterMode.fmFilterFile];
	FIsInitialized := False;
	FReplaceList := Shared.Make<TReplaceList>();
end;

destructor TRipGrepperTopFrame.Destroy;
begin
	FDpiScaler.Free;
	inherited Destroy;
end;

procedure TRipGrepperTopFrame.ActionAbortSearchExecute(Sender : TObject);
begin
	if MainFrame.IsSearchRunning then begin
		MainFrame.RipGrepTask.Cancel;
	end;
	MainFrame.AbortSearch := True;
end;

procedure TRipGrepperTopFrame.ActionAlignToolbarsExecute(Sender : TObject);
begin
	MainFrame.AlignToolBars;
end;

procedure TRipGrepperTopFrame.ActionAlternateRowColorsExecute(Sender : TObject);
begin
	Settings.NodeLookSettings.AlternateRowColors := (not Settings.NodeLookSettings.AlternateRowColors);
	Settings.StoreViewSettings('AlternateRowColors');
	MainFrame.VstResult.Repaint();
	ActionAlternateRowColorsUpdate;
end;

procedure TRipGrepperTopFrame.ActionAlternateRowColorsUpdate();
begin
	tbAlternateRowColors.Down := Settings.NodeLookSettings.AlternateRowColors;
end;

procedure TRipGrepperTopFrame.ActionCmdLineCopyExecute(Sender : TObject);
begin
	ClipBoard.AsText := Settings.RipGrepParameters.GetCommandLine(Settings.AppSettings.CopyToClipBoardShell);
end;

procedure TRipGrepperTopFrame.ActionConfigExecute(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperTopFrame.ActionConfigExecute');
	var
	owSettings := Settings.OpenWithSettings;
	owSettings.TestFile := MainFrame.GetOpenWithParamsFromSelected();
	dbgMsg.Msg('TestFile: ' + owSettings.TestFile.GetRelativePath());

	// write ini file content
	Settings.UpdateFile(True);
	var
	owForm := TConfigForm.Create(Settings);
	try
		owForm.ShowModal;
	finally
		owForm.Free;
	end;

	// write ini file content after close config form
	Settings.UpdateFile(True);

	owSettings.TestFile := default (TOpenWithParams);
	MainFrame.ReloadColorSettings;

	{$IFDEF STANDALONE}
	UpdateUIStyle;
	// TDarkModeHelper.BroadcastThemeChanged(Parent.Handle);
	{$ENDIF}
end;

procedure TRipGrepperTopFrame.ActionCopyFileNameExecute(Sender : TObject);
begin
	MainFrame.CopyToClipboardFileOfSelected();
end;

procedure TRipGrepperTopFrame.ActionCopyPathToClipboardExecute(Sender : TObject);
begin
	MainFrame.CopyToClipboardPathOfSelected();
end;

procedure TRipGrepperTopFrame.ActionExpandCollapseExecute(Sender : TObject);
begin
	Settings.NodeLookSettings.ExpandNodes := not Settings.NodeLookSettings.ExpandNodes;
	Settings.StoreViewSettings('ExpandNodes');

	if Settings.NodeLookSettings.ExpandNodes then begin
		MainFrame.VstResult.FullExpand();
	end else begin
		MainFrame.VstResult.FullCollapse();
	end;
	ActionExpandCollapseUpdate();
end;

procedure TRipGrepperTopFrame.ActionExpandCollapseUpdate;
begin
	ActionExpandCollapse.ImageIndex := IfThen(Settings.NodeLookSettings.ExpandNodes, IMG_IDX_COLLAPSE, IMG_IDX_EXPAND);
	ActionExpandCollapse.Hint := IfThen(Settings.NodeLookSettings.ExpandNodes, 'Collapse Nodes', 'Expand Nodes');
end;

procedure TRipGrepperTopFrame.ActionIndentLineExecute(Sender : TObject);
begin
	Settings.NodeLookSettings.IndentLines := not Settings.NodeLookSettings.IndentLines;
	Settings.StoreViewSettings('IndentLines');
	MainFrame.VstResult.Repaint();
	ActionIndentLineUpdate;
end;

procedure TRipGrepperTopFrame.ActionIndentLineUpdate();
begin
	tbIndentLines.ImageIndex :=
	{ } IfThen(Settings.NodeLookSettings.IndentLines, IMG_IDX_INDENT_OFF, IMG_IDX_INDENT_ON);
end;

procedure TRipGrepperTopFrame.ActionOpenWithExecute(Sender : TObject);
begin
	MainFrame.ActionOpenWithExecute(Sender);
end;

procedure TRipGrepperTopFrame.ActionRefreshSearchExecute(Sender : TObject);
var
	cursor : TCursorSaver;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperTopFrame.ActionRefreshSearchExecute');
	cursor.SetHourGlassCursor;
	MainFrame.RefreshSearch();
end;

procedure TRipGrepperTopFrame.ActionRefreshSearchUpdate(Sender : TObject);
begin
	ActionRefreshSearch.Enabled := // Settings.IsAlreadyRead and
	{ } (not MainFrame.IsSearchRunning)
	{ } and Assigned(MainFrame.HistItemObject);
end;

procedure TRipGrepperTopFrame.ActionReplaceCaseSensitiveExecute(Sender : TObject);
begin
	ToggleGuiReplaceMode(EGuiReplaceMode.grmCaseSensitive);
	UpdateReplaceMenu;
end;

procedure TRipGrepperTopFrame.ActionReplaceUseRegexExecute(Sender : TObject);
begin
	ToggleGuiReplaceMode(EGuiReplaceMode.grmUseRegex);
	UpdateReplaceMenu;
end;

procedure TRipGrepperTopFrame.ActionSaveReplacementExecute(Sender : TObject);
var
	saveResult : ESaveReplacementResult;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperTopFrame.ActionSaveReplacementExecute');

	GetCheckedReplaceList();

	{$IF IS_EXTENSION}
	repeat
		var
		arrModifiedFiles := IOTAUTils.GetOpenedEditorFiles(True);
		var
			replKeyArrEx : TArrayEx<string> := FReplaceList.Items.Keys.ToArray;
		var
			idx : integer := replKeyArrEx.GetFirstMatchIndex(arrModifiedFiles);
		if (idx <> -1) then begin
			case IOTAFileUtils.AskSaveModifiedFiles(arrModifiedFiles[idx]) of
				smfrActSaved : begin
					dbgMsg.Msg('smfrActSaved - file saved: ' + arrModifiedFiles[idx]);
					continue;
				end;
				smfrAllSaved : begin
					dbgMsg.Msg('smfrAllSaved - all files saved');
					break;
				end;
				smfrActNotSaved : begin
					dbgMsg.Msg('smfrActNotSaved - user chose not to save: ' + arrModifiedFiles[idx]);
					Exit;
				end;
				smfrCancel : begin
					dbgMsg.Msg('smfrCancel - user cancel');
					Exit;
				end;
				smfrError : begin
					dbgMsg.ErrorMsg('smfrError - error occured');
					Exit;
				end;
				else begin
					dbgMsg.Msg('Unknown result, exiting');
					Exit;
				end;
			end;
		end else begin
			break;
		end;
	until True;
	{$ENDIF}
	saveResult := SaveSelectedReplacements();
	case saveResult of
		srrDone : begin
			dbgMsg.Msg('srrDone');
			ActionRefreshSearchExecute(self);
		end;
		srrError : begin
			dbgMsg.ErrorMsg('srrError - error occured');
			Exit;
		end;
		srrCancel : begin
			dbgMsg.Msg('srrCancel - user cancel');
			Exit;
		end;
	end;
	if IsRGReplaceMode then begin
		// nothing to do?
	end else if IsGuiReplaceMode then begin
		SetGuiReplaceMode([EGuiReplaceMode.grmEditEnabled], '');
	end;
end;

procedure TRipGrepperTopFrame.ActionSaveReplacementUpdate(Sender : TObject);
begin
	ActionSaveReplacement.Enabled := (EGuiReplaceMode.grmSaveEnabled in FGuiReplaceModes)
	{ } and (MainFrame.VstResult.CheckedCount > 0);
end;

procedure TRipGrepperTopFrame.ActionSearchExecute(Sender : TObject);
var
	cursor : TCursorSaver;
begin
	cursor.SetHourGlassCursor;
	MainFrame.PrepareAndDoSearch();
end;

procedure TRipGrepperTopFrame.ActionSearchInResultExecute(Sender : TObject);
begin
	SelectNextFoundNode(FPrevFoundNode, edtFilter.Text);
end;

procedure TRipGrepperTopFrame.ActionSetFileFilterModeExecute(Sender : TObject);
begin
	SetFilterMode(EFilterMode.fmFilterText, True);
	SetFilterMode(EFilterMode.fmFilterFile);
	UpdateFilterEditMenuAndHint;
	Settings.StoreViewSettings(TFilterSettings.SETTING_FILTERMODE);
end;

procedure TRipGrepperTopFrame.ActionSetFilterModeCaseSensitiveExecute(Sender : TObject);
begin
	ToggleFilterMode(EFilterMode.fmCaseSensitive);
	ActionSetFilterModeCaseSensitive.Checked := EFilterMode.fmCaseSensitive in FFilterMode;
	Settings.StoreViewSettings(TFilterSettings.SETTING_CASE_SENSITIVE);
end;

procedure TRipGrepperTopFrame.ActionSetFilterModeRegexExecute(Sender : TObject);
begin
	ToggleFilterMode(EFilterMode.fmUseRegex);
	ActionSetFilterModeRegex.Checked := EFilterMode.fmUseRegex in FFilterMode;
	Settings.StoreViewSettings(TFilterSettings.SETTING_USE_REGEX);
end;

procedure TRipGrepperTopFrame.ActionSetTextFilterModeExecute(Sender : TObject);
begin
	SetFilterMode(EFilterMode.fmFilterFile, True);
	SetFilterMode(EFilterMode.fmFilterText);
	UpdateFilterEditMenuAndHint;
	Settings.StoreViewSettings(TFilterSettings.SETTING_FILTERMODE);
end;

procedure TRipGrepperTopFrame.ActionShowFileIconsExecute(Sender : TObject);
begin
	Settings.NodeLookSettings.ShowFileIcon := not Settings.NodeLookSettings.ShowFileIcon;
	Settings.StoreViewSettings('ShowFileIcon');
	MainFrame.VstResult.Repaint();
	ActionShowFileIconsUpdate;
end;

procedure TRipGrepperTopFrame.ActionShowFileIconsUpdate();
begin
	tbShowFileIcon.Down := Settings.NodeLookSettings.ShowFileIcon;
end;

procedure TRipGrepperTopFrame.ActionShowRelativePathExecute(Sender : TObject);
const
	PARSER_TYPES : TArray<TFileNameType> = [ftAbsolute, ftRelative];
begin
	Settings.NodeLookSettings.ShowRelativePath := not Settings.NodeLookSettings.ShowRelativePath;
	var
	idx := integer(Settings.NodeLookSettings.ShowRelativePath);
	MainFrame.FileNameType := PARSER_TYPES[idx mod Length(PARSER_TYPES)];

	Settings.StoreViewSettings('ShowRelativePath');
	MainFrame.VstResult.Repaint();

	ActionShowRelativePathUpdate;
end;

procedure TRipGrepperTopFrame.ActionShowRelativePathUpdate;
begin
	// tbShowRelativePath.Down := Settings.NodeLookSettings.ShowRelativePath;
	ActionShowRelativePath.ImageIndex := Ifthen(Settings.NodeLookSettings.ShowRelativePath, IMG_IDX_SHOW_ABS_PATH,
		IMG_IDX_SHOW_RELATIVE_PATH);
end;

procedure TRipGrepperTopFrame.ActionShowSearchFormExecute(Sender : TObject);
begin
	// try rg.exe
	if not Settings.RipGrepParameters.IsRgPathInitOk then begin
		TAsyncMsgBox.ShowError(Format(FORMAT_RIPGREP_EXE_NOT_FOUND, [Settings.PersisterFactory.FilePath]));
		ActionConfigExecute(self);
		Exit;
	end;

	TDebugUtils.DebugMessage('TRipGrepperTopFrame.ActionShowSearchFormExecute');
	SetFilterBtnImage(False);
	StartNewSearch;
end;

procedure TRipGrepperTopFrame.AfterHistObjChange;
begin
	UpdateFilterEditMenuAndHint;
end;

procedure TRipGrepperTopFrame.AfterSearch;
begin
	ActionAbortSearch.Enabled := False;
	ActionShowSearchForm.Enabled := True;
end;

procedure TRipGrepperTopFrame.AlignToolBars(iTbResultLeft, iSearchMaxWidth, iResultMinWidth : integer);
begin
	tbarSearch.Top := tbarSearch.Margins.Top;
	tbarResult.Top := tbarResult.Margins.Top;
	tbarConfig.Top := tbarConfig.Margins.Top;
	tbarSearch.Width := GetToolBarWidth(tbarSearch);
	tbarResult.Width := GetToolBarWidth(tbarResult);
	tbarConfig.Width := GetToolBarWidth(tbarConfig);

	tbarSearch.Left := tbarSearch.Margins.Left;
	if iSearchMaxWidth >= tbarSearch.Width then begin
		tbarResult.Left := iTbResultLeft;
	end else begin
		tbarResult.Left := tbarSearch.Width + 2 * tbarSearch.Margins.Right;
	end;

	if iResultMinWidth <= (tbarResult.Width + tbarConfig.Width + 2 * tbarConfig.Margins.Left) then begin
		tbarResult.Left := tbarConfig.Left - tbarResult.Width - 2 * tbarConfig.Margins.Left;
	end;
end;

procedure TRipGrepperTopFrame.BeforeSearch(var _bAbort : Boolean);
begin
	ActionAbortSearch.Enabled := True;
	ActionShowSearchForm.Enabled := False;
end;

procedure TRipGrepperTopFrame.ChangeButtonedEditTextButSkipChangeEvent(_edt : TButtonedEdit; const _txt : string);
begin
	FSkipButtonEditChange := True;
	try
		_edt.Text := _txt;
	finally
		FSkipButtonEditChange := False;
	end;
end;

procedure TRipGrepperTopFrame.ChangeScale(M, D : Integer; isDpiChange : Boolean);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperTopFrame.ChangeScale');

	dbgMsg.MsgFmt('M(%d) / D(%d) = %d%%', [M, D, MulDiv(100, M, D)]);
	dbgMsg.MsgFmt('Orig edtFilter.Font %d edtReplace.Font %d', [edtFilter.Font.Height, edtReplace.Font.Height]);
	dbgMsg.MsgFmt('Orig tbarSearch.Width %d and tbarResult.Width %d', [tbarSearch.Width, tbarResult.Width]);
	inherited ChangeScale(M, D, isDpiChange);
	if isDpiChange then begin
		{$IFDEF STANDALONE}   // scaling is done by ide ?
		edtFilter.Font.Height := MulDiv(edtFilter.Font.Height, M, D);
		edtReplace.Font.Height := MulDiv(edtReplace.Font.Height, M, D);

		tbarSearch.Width := MulDiv(tbarSearch.Width, M, D);
		tbarResult.Width := MulDiv(tbarResult.Width, M, D);
		tbarConfig.Width := MulDiv(tbarConfig.Width, M, D);
		{$ENDIF}
	end;
	dbgMsg.MsgFmt('New edtFilter.Font %d and edtReplace.Font %d', [edtFilter.Font.Height, edtReplace.Font.Height]);
	dbgMsg.MsgFmt('New tbarSearch.Width %d and tbarResult.Width %d', [tbarSearch.Width, tbarResult.Width]);
end;

procedure TRipGrepperTopFrame.edtFilterChange(Sender : TObject);
begin
	if FSkipButtonEditChange then begin
		Exit;
	end;

	if IsFilterOn then begin
		MainFrame.FilterNodes(edtFilter.Text, FFilterMode);
	end;
end;

procedure TRipGrepperTopFrame.edtFilterKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
	if Key = VK_RETURN then begin
		// Enter key was pressed
		SetFilterBtnImage(True);
		MainFrame.FilterNodes(edtFilter.Text, FFilterMode);
	end;
end;

procedure TRipGrepperTopFrame.edtFilterRightButtonClick(Sender : TObject);
begin
	if IsFilterOn then begin
		SetFilterBtnImage(False);
		MainFrame.ClearFilter();
	end else begin
		SetFilterBtnImage(True);
		MainFrame.FilterNodes(edtFilter.Text, FFilterMode);
	end;
end;

procedure TRipGrepperTopFrame.edtReplaceChange(Sender : TObject);
begin
	if FSkipButtonEditChange then begin
		Exit;
	end;

	if IsGuiReplaceMode then begin
		SetReplaceTextInSettings(edtReplace.Text);
	end else begin
		if not string(edtReplace.Text).IsEmpty then begin
			edtReplaceRightButtonClick(self);
		end;
	end;
end;

procedure TRipGrepperTopFrame.edtReplaceKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
	if Key = VK_RETURN then begin
		if IsGuiReplaceMode then begin
			SetGuiReplaceModes(True);
		end;
		SetReplaceTextInSettings(edtReplace.Text);
		SetReplaceModeOnGui();
	end;
end;

procedure TRipGrepperTopFrame.edtReplaceRightButtonClick(Sender : TObject);
begin
	if IsGuiReplaceMode then begin
		SetReplaceTextInSettings('');
		SetGuiReplaceModes(False);
	end else begin
		SetGuiReplaceModes(True);
		SetReplaceTextInSettings(edtReplace.Text);
	end;
	SetReplaceModeOnGui();
end;

procedure TRipGrepperTopFrame.GetCheckedReplaceList();
var
	node : PVirtualNode;
	nodeData : PVSFileNodeData;
	parentData : PVSFileNodeData;
	replaceLine : string;
	fileName : string;
	lineNum : integer;
	rowNum : integer;
	rm : TReplaceModes;
begin
	node := MainFrame.VstResult.GetFirstChecked();
	rm := GetReplaceMode();
	var
	replaceText := Settings.RipGrepParameters.ReplaceText;
	var
	isJson := Settings.RipGrepParameters.RipGrepArguments.IsOptionSet(RG_PARAM_REGEX_JSON_OUTPUT);
	FReplaceList.Items.Clear;
	while Assigned(node) do begin
		if node.Parent <> MainFrame.VstResult.RootNode then begin
			nodeData := MainFrame.VstResult.GetNodeData(node);
			parentData := MainFrame.VstResult.GetNodeData(node.Parent);
			fileName := parentData.FilePath;
			if fileName = RG_STATS_LINE then begin
				continue;
			end;
			lineNum := nodeData.MatchData.Row;
			rowNum := nodeData.MatchData.ColBegin;
			var
			lineText := nodeData.MatchData.LineText;

			if IsRgReplaceMode then begin
				if not isJson then begin
					replaceLine := lineText; // ok every replacement is done by rg.exe if not --json
				end else begin
					replaceLine := TReplaceHelper.ReplaceString(lineText, Settings.LastSearchText, replaceText, rowNum, rm);
				end;
			end else if IsGuiReplaceMode then begin
				if FReplaceList.TryGet(fileName, lineNum, rowNum, lineText) then begin
					FReplaceList.Remove(fileName, lineNum, rowNum, lineText);
				end;
				// we should replace only from nodeData.MatchData.Col?
				replaceLine := TReplaceHelper.ReplaceString(lineText, Settings.LastSearchText, replaceText, rowNum, rm);
			end;

			FReplaceList.AddUnique(fileName, lineNum, rowNum, lineText, replaceLine);
		end;
		node := MainFrame.VstResult.GetNextChecked(Node);
	end;
end;

function TRipGrepperTopFrame.GetIsGuiReplaceMode : Boolean;
begin
	Result := EGuiReplaceMode.grmActive in FGuiReplaceModes;
end;

function TRipGrepperTopFrame.GetIsInitialized() : Boolean;
begin
	Result := FIsInitialized;
end;

function TRipGrepperTopFrame.GetNextViewStyleIdx : integer;
begin
	Result := IfThen(FViewStyleIndex < Length(LISTVIEW_TYPES) - 1, FViewStyleIndex + 1, 0);
	Result := (Result mod Length(LISTVIEW_TYPES));
end;

function TRipGrepperTopFrame.GetReplaceMode : TReplaceModes;
begin
	Result := [];
	if EGuiReplaceMode.grmUseRegex in FGuiReplaceModes then begin
		Include(Result, EReplaceMode.rmUseRegex);
	end;

	if not(EGuiReplaceMode.grmCaseSensitive in FGuiReplaceModes) then begin
		Include(Result, EReplaceMode.rmIgnoreCase);
	end;
end;

function TRipGrepperTopFrame.GetSettings : TRipGrepperSettings;
begin
	if not Assigned(FSettings) then begin
		FSettings := ParentFrame.Settings;
	end;
	Result := FSettings;
end;

function TRipGrepperTopFrame.GetToolBarWidth(_tb : TToolBar) : Integer;
begin
	Result := 0;
	// TDebugUtils.DebugMessage(Format('TRipGrepperTopFrame.GetToolBarWidth %s Width: %d BtnCnt:%d ', [_tb.Name, _tb.Width, _tb.ButtonCount]));

	for var i : integer := 0 to _tb.ButtonCount - 1 do begin
		Result := Result + _tb.Buttons[i].Width;
	end;
	// TDebugUtils.DebugMessage(Format('TRipGrepperTopFrame.GetToolBarWidth %s Width: %d', [_tb.Name, Result, _tb.ButtonCount]));
end;

procedure TRipGrepperTopFrame.HandleReplaceErrors(const failReplaceData : TFailedReplaceData);
var
	errMsg : TArrayEx<string>;
begin
	if failReplaceData.Count > 0 then begin
		errMsg.Add('Current line(s) in file(s) doesn''t match the line which was replaced:');
		for var p in failReplaceData do begin
			errMsg.Add(Format('%s(%d)', [p.Key, p.Value.Row]));
		end;

		TMsgBox.ShowWarning(string.Join(CRLF, errMsg.Items) + CRLF2 +
			{ } 'Please save the file, refresh search results, then try replacing again.');
	end;
end;

procedure TRipGrepperTopFrame.Initialize();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperTopFrame.Initialize');

	if IsInitialized then begin
		dbgMsg.Msg('Already initialized');
		Exit;
	end;
	FFilterMode := Settings.NodeLookSettings.FilterSettings.FilterModes;
	UpdateFilterEditMenuAndHint();
	ActionExpandCollapseUpdate();
	ActionShowRelativePathUpdate();
	ActionAlternateRowColorsUpdate();
	ActionShowFileIconsUpdate();
	ActionIndentLineUpdate();

	ActionAbortSearch.Enabled := False;

	FIsInitialized := True;
end;

function TRipGrepperTopFrame.IsFilterOn : Boolean;
begin
	Result := edtFilter.RightButton.ImageIndex = IMG_IDX_FILTER_ON;
end;

function TRipGrepperTopFrame.IsRgReplaceMode : Boolean;
begin
	Result := EGuiReplaceMode.grmRGReplace in FGuiReplaceModes;
end;

function TRipGrepperTopFrame.SaveSelectedReplacements() : ESaveReplacementResult;
var
	failReplaceData : TFailedReplaceData;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperTopFrame.SaveSelectedReplacements');
	Result := srrDone;

	if ShowWarningBeforeSave(FReplaceList) then begin
		TReplaceHelper.ReplaceLineInFiles(FReplaceList, failReplaceData, { _bCreateBackup } True, IsRgReplaceMode);
		HandleReplaceErrors(failReplaceData);
		if failReplaceData.Count > 0 then begin
			Result := srrError;
		end;
	end else begin
		Result := srrCancel;
	end;
end;

procedure TRipGrepperTopFrame.SearchBox1Change(Sender : TObject);
begin
	inherited; // OnChange(Sender);
	FPrevFoundNode := nil;
	// SelectNextFoundNode(FPrevFoundNode);
	MainFrame.FilterNodes(edtFilter.Text, FFilterMode);
end;

procedure TRipGrepperTopFrame.SearchForText(Sender : TBaseVirtualTree; Node : PVirtualNode; Data : Pointer; var Abort : Boolean);
var
	dataStr : string;
	NodeData : PVSFileNodeData;
begin
	NodeData := Sender.GetNodeData(Node);
	dataStr := NodeData.FilePath + ' ' + NodeData.MatchData.LineText;
	Abort := ContainsText(dataStr, string(Data));
	// abort the search if a node with the text is found.
	TDebugUtils.DebugMessage(Format('Search ''%s'' in %s', [string(Data), dataStr]));
end;

procedure TRipGrepperTopFrame.SelectNextFoundNode(const _prevFoundNode : PVirtualNode; const _searchPattern : string);
var
	bLast : Boolean;
	nextNode, lastNode, foundNode : PVirtualNode;
begin
	if _searchPattern.IsEmpty then begin
		Exit;
	end;
	lastNode := MainFrame.VstResult.GetLast(nil, true);
	nextNode := _prevFoundNode;
	repeat
		if not Assigned(nextNode) then begin
			nextNode := MainFrame.VstResult.GetFirst;
		end else begin
			nextNode := MainFrame.VstResult.GetNext(nextNode, true);
		end;
		bLast := (nextNode = lastNode);
		Assert(nextNode <> MainFrame.VstResult.RootNode);
		// first param is your starting point. nil starts at top of tree. if you want to implement findnext
		// functionality you will need to supply the previous found node to continue from that point.
		// be sure to set the IncrementalSearchTimeout to allow users to type a few characters before starting a search.
		foundNode := MainFrame.VstResult.IterateSubtree(nextNode, SearchForText, Pointer(_searchPattern));
	until Assigned(foundNode) or bLast;

	if Assigned(foundNode) then begin
		MainFrame.VstResult.FocusedNode := foundNode;
		MainFrame.VstResult.Selected[foundNode] := True;
		FPrevFoundNode := foundNode;
	end;
	if bLast then begin
		FPrevFoundNode := nil;
	end;
end;

procedure TRipGrepperTopFrame.SetFilterBtnImage(const _bOn : Boolean = True);
begin
	edtFilter.RightButton.ImageIndex :=
	{ } IfThen(_bOn and (edtFilter.Text <> ''), IMG_IDX_FILTER_ON, IMG_IDX_FILTER_OFF);
end;

procedure TRipGrepperTopFrame.SetFilterMode(const _fm : EFilterMode; const _bReset : Boolean = False);
begin
	if _bReset then begin
		Exclude(FFilterMode, _fm);
	end else begin
		Include(FFilterMode, _fm);
	end;
	Settings.NodeLookSettings.FilterSettings.FilterModes := FFilterMode;
end;

procedure TRipGrepperTopFrame.SetGuiReplaceMode(const _modes : TGuiReplaceModes; const _sReplaceText : string = '');
begin
	FGuiReplaceModes := _modes;
	edtReplace.Text := _sReplaceText;
	SetReplaceModeOnGui();
end;

procedure TRipGrepperTopFrame.SetGuiReplaceModes(const _bOn : Boolean = True);
begin
	if _bOn then begin
		Include(FGuiReplaceModes, EGuiReplaceMode.grmActive);
		Include(FGuiReplaceModes, EGuiReplaceMode.grmSaveEnabled);
	end else begin
		Exclude(FGuiReplaceModes, EGuiReplaceMode.grmActive);
		Exclude(FGuiReplaceModes, EGuiReplaceMode.grmSaveEnabled);
	end;
end;

procedure TRipGrepperTopFrame.SetReplaceModeOnGui();
begin
	SetReplaceModeOnToolBar();
	ParentFrame.MainFrame.SetReplaceModeOnGrid(EGuiReplaceMode.grmActive in FGuiReplaceModes);
end;

procedure TRipGrepperTopFrame.SetReplaceModeOnToolBar;

begin
	ActionSaveReplacement.Enabled := EGuiReplaceMode.grmSaveEnabled in FGuiReplaceModes;
	// ActionSaveAllReplacement.Enabled := EGuiReplaceMode.grmSaveEnabled in FGuiReplaceModes;
	edtReplace.Enabled := EGuiReplaceMode.grmEditEnabled in FGuiReplaceModes;
	edtReplace.RightButton.ImageIndex := IfThen(
		{ } (EGuiReplaceMode.grmActive in FGuiReplaceModes), IMG_IDX_REPLACE_ON, IMG_IDX_REPLACE_OFF);

	if (not edtReplace.Enabled) and (edtReplace.Text = '') then begin
		ChangeButtonedEditTextButSkipChangeEvent(edtReplace, edtReplace.TextHint);
	end;

end;

procedure TRipGrepperTopFrame.SetReplaceTextInSettings(const _sReplText : string);
begin
	if Assigned(FSettings) then begin
		FSettings.RipGrepParameters.ReplaceText := _sReplText;
		MainFrame.VstResult.Repaint();
	end;
end;

class function TRipGrepperTopFrame.ShowWarningBeforeSave(_list : TReplaceList) : Boolean;
begin
	var
	cnt := _list.GetCounters();
	Result := mrYes =
	{ } TMsgBox.ShowQuestion(Format('Are you sure to change %d line(s) in %d file(s)?',
		{ } [cnt.LineCount, cnt.FileCount]),
		{ } 'Replace',
		{ } [mbYes, mbNo]
		{$IFNDEF STANDALONE}
		{ } ,
		{ } 'Warning',
		{ } 'The files already opened in the editor should be saved after replace.'
		{$ENDIF}
		);
end;

procedure TRipGrepperTopFrame.StartNewSearch;
var
	formResult : Integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperTopFrame.StartNewSearch');
	FHistItemObj := MainFrame.CreateNewHistObject;
	formResult := TRipGrepperSearchDialogForm.ShowSearchForm(self, Settings, FHistItemObj);
	if (mrOk = formResult) then begin
		dbgMsg.Msg('after showmodal gui params: ' + Settings.RipGrepParameters.GuiSearchTextParams.ToLogString);
		ActionSearchExecute(self);
	end else begin
		ParentFrame.MainFrame.MiddleLeftFrame1.DeleteCurrentHistoryItemFromList;
		FHistItemObj := nil;
	end;
	ParentFrame.MainFrame.MiddleLeftFrame1.SetReplaceMode();
end;

procedure TRipGrepperTopFrame.ToggleFilterMode(const _fm : EFilterMode);
begin
	if _fm in FFilterMode then begin
		Exclude(FFilterMode, _fm);
	end else begin
		Include(FFilterMode, _fm);
	end;
	Settings.NodeLookSettings.FilterSettings.FilterModes := FFilterMode;
end;

procedure TRipGrepperTopFrame.ToggleGuiReplaceMode(const _grm : EGuiReplaceMode);
begin
	if _grm in FGuiReplaceModes then begin
		Exclude(FGuiReplaceModes, _grm);
	end else begin
		Include(FGuiReplaceModes, _grm);
	end;
	// Settings.NodeLookSettings. := FFilterMode;
end;

procedure TRipGrepperTopFrame.UpdateFilterEditMenuAndHint;
begin
	edtFilter.Enabled := Assigned(MainFrame.HistItemObject) and MainFrame.HistItemObject.HasResult;
	if (not edtFilter.Enabled) and (edtFilter.Text = '') then begin
		ChangeButtonedEditTextButSkipChangeEvent(edtFilter, edtFilter.TextHint);
	end else if edtFilter.Enabled and (edtFilter.Text = edtFilter.TextHint) then begin
		ChangeButtonedEditTextButSkipChangeEvent(edtFilter, '');
	end;

	ActionSetFileFilterMode.Checked := EFilterMode.fmFilterFile in FFilterMode;
	ActionSetTextFilterMode.Checked := EFilterMode.fmFilterText in FFilterMode;
	ActionSetFilterModeCaseSensitive.Checked := EFilterMode.fmCaseSensitive in FFilterMode;
	ActionSetFilterModeRegex.Checked := EFilterMode.fmUseRegex in FFilterMode;

	if (ActionSetFileFilterMode.Checked) then begin
		edtFilter.Hint := 'File Filter';
	end else if ActionSetTextFilterMode.Checked then begin
		edtFilter.Hint := 'Text Filter';
	end;

	edtFilter.Hint := edtFilter.Hint + ' (right-click to change)';
end;

procedure TRipGrepperTopFrame.UpdateReplaceMenu;
begin
	ActionReplaceCaseSensitive.Checked := EGuiReplaceMode.grmCaseSensitive in FGuiReplaceModes;
	ActionReplaceUseRegex.Checked := EGuiReplaceMode.grmUseRegex in FGuiReplaceModes;
end;

procedure TRipGrepperTopFrame.UpdateUIStyle(_sNewStyle : string = '');
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperTopFrame.UpdateUIStyle');
	if _sNewStyle.IsEmpty then begin
		StyleName := TDarkModeHelper.GetActualThemeName();
	end else begin
		StyleName := _sNewStyle;
	end;

	MainFrame.UpdateUIStyle(styleName);

	// var
	// propi : PPropInfo := GetPropInfo(Self, 'StyleElements');

	// if Assigned(propi) then begin
	// var
	// v := GetPropValue(Self, propi, true);
	// dbgMsg.MsgFmt('TopFrame.StyleElements = %s', [VarToStrDef(v, 'n/a')]);
	// end;
	// dbgMsg.MsgFmt('StyleName of TopFrame = %s, pnlTop = %s, MainFrame = %s', [TopFrame.StyleName, pnlTop.StyleName, MainFrame.StyleName])
end;

procedure TRipGrepperTopFrame.WMSettingChange(var message : TWMSettingChange);
begin
	if SameText('ImmersiveColorSet', string(message.Section)) then begin
		{$IFDEF STANDALONE}
		UpdateUIStyle;
		// TDarkModeHelper.BroadcastThemeChanged(Parent.Handle);
		{$ENDIF}
	end;
end;

end.
