unit RipGrepper.UI.TopFrame;

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
	RipGrepper.Common.Settings.RipGrepperSettings;

type

	TRipGrepperTopFrame = class(TFrame)
		ImageListButtons : TImageList;
		ActionList : TActionList;
		ActionSearch : TAction;
		ActionConfig : TAction;
		ActionSwitchView : TAction;
		ActionShowRelativePath : TAction;
		ActionCmdLineCopy : TAction;
		ActionShowSearchForm : TAction;
		ActionShowFileIcons : TAction;
		ActionAlternateRowColors : TAction;
		ActionAbortSearch : TAction;
		ActionRefreshSearch : TAction;
		ActionIndentLine : TAction;
		tbarSearch : TToolBar;
		tbView : TToolButton;
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
		SearchBox1 : TSearchBox;
		tbarResult : TToolBar;
		tbarConfig : TToolBar;
		PopupMenu1 : TPopupMenu;
		AlignToolbar1 : TMenuItem;
		ToolButton2 : TToolButton;
		ToolButton5 : TToolButton;
		ToolButton8 : TToolButton;
		ActionSearchInResult : TAction;
		procedure ActionAbortSearchExecute(Sender : TObject);
		procedure ActionAbortSearchUpdate(Sender : TObject);
		procedure ActionAlignToolbarsExecute(Sender : TObject);
		procedure ActionAlternateRowColorsExecute(Sender : TObject);
		procedure ActionAlternateRowColorsUpdate(Sender : TObject);
		procedure ActionCancelExecute(Sender : TObject);
		procedure ActionCmdLineCopyExecute(Sender : TObject);
		procedure ActionCmdLineCopyUpdate(Sender : TObject);
		procedure ActionConfigExecute(Sender : TObject);
		procedure ActionCopyFileNameExecute(Sender : TObject);
		procedure ActionCopyPathToClipboardExecute(Sender : TObject);
		procedure ActionExpandCollapseExecute(Sender : TObject);
		procedure ActionExpandCollapseUpdate(Sender : TObject);
		procedure ActionIndentLineExecute(Sender : TObject);
		procedure ActionIndentLineUpdate(Sender : TObject);
		procedure ActionOpenWithExecute(Sender : TObject);
		procedure ActionRefreshSearchExecute(Sender : TObject);
		procedure ActionRefreshSearchUpdate(Sender : TObject);
		procedure ActionSearchExecute(Sender : TObject);
		procedure ActionSearchInResultExecute(Sender : TObject);
		procedure ActionShowFileIconsExecute(Sender : TObject);
		procedure ActionShowFileIconsUpdate(Sender : TObject);
		procedure ActionShowRelativePathExecute(Sender : TObject);
		procedure ActionShowRelativePathUpdate(Sender : TObject);
		procedure ActionShowSearchFormExecute(Sender : TObject);
		procedure ActionShowSearchFormUpdate(Sender : TObject);
		procedure ActionSwitchViewExecute(Sender : TObject);
		procedure ActionSwitchViewUpdate(Sender : TObject);
		procedure SearchBox1Change(Sender : TObject);

		private
			FDpiScaler : TRipGrepperDpiScaler;
			FPrevFoundNode : PVirtualNode;
			FSettings : TRipGrepperSettings;
			FViewStyleIndex : integer;
			function GetSettings : TRipGrepperSettings;
			function GetToolBarWidth(_tb : TToolBar) : Integer;
			procedure SelectNextFoundNode(const _prevFoundNode : PVirtualNode);
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

		public
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;
			procedure AlignToolBars(iTbResultLeft, iSearchMaxWidth, iResultMinWidth : integer);
			function GetNextViewStyleIdx : integer;
			procedure Init;
			procedure SearchForText(Sender : TBaseVirtualTree; Node : PVirtualNode; Data : Pointer; var Abort : Boolean);
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
	RipGrepper.OpenWith.SimpleTypes,
	RipGrepper.OpenWith,
	RipGrepper.Helper.UI,
	RipGrepper.UI.SearchForm,
	System.Math,
	System.StrUtils,
	RipGrepper.Common.Settings.RipGrepParameterSettings,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.UI.RipGrepOptionsForm,
	RipGrepper.Common.ParsedObject,
	RipGrepper.Common.IOTAUtils;

constructor TRipGrepperTopFrame.Create(AOwner : TComponent);
begin
	inherited;
	FDpiScaler := TRipGrepperDpiScaler.Create(self);
	TopFrame := self;
end;

destructor TRipGrepperTopFrame.Destroy;
begin
	FDpiScaler.Free;
	inherited;
end;

procedure TRipGrepperTopFrame.ActionAbortSearchExecute(Sender : TObject);
begin
	if MainFrame.IsSearchRunning then begin
		MainFrame.RipGrepTask.Cancel;
	end;
	MainFrame.AbortSearch := True;
end;

procedure TRipGrepperTopFrame.ActionAbortSearchUpdate(Sender : TObject);
begin
	ActionAbortSearch.Enabled := MainFrame.IsSearchRunning;
end;

procedure TRipGrepperTopFrame.ActionAlignToolbarsExecute(Sender : TObject);
begin
	MainFrame.AlignToolBars;
end;

procedure TRipGrepperTopFrame.ActionAlternateRowColorsExecute(Sender : TObject);
begin
	Settings.RipGrepperViewSettings.AlternateRowColors := (not Settings.RipGrepperViewSettings.AlternateRowColors);
	Settings.StoreViewSettings('AlternateRowColors');
	MainFrame.VstResult.Repaint();
end;

procedure TRipGrepperTopFrame.ActionAlternateRowColorsUpdate(Sender : TObject);
begin
	tbAlternateRowColors.Down := Settings.RipGrepperViewSettings.AlternateRowColors;
end;

procedure TRipGrepperTopFrame.ActionCancelExecute(Sender : TObject);
begin
	// ModalResult := mrCancel;
	// Close();
end;

procedure TRipGrepperTopFrame.ActionCmdLineCopyExecute(Sender : TObject);
begin
	ClipBoard.AsText := Settings.RipGrepParameters.GetCommandLine;
end;

procedure TRipGrepperTopFrame.ActionCmdLineCopyUpdate(Sender : TObject);
begin
	ActionCmdLineCopy.Hint := 'Copy Command Line:' + CRLF + Settings.RipGrepParameters.GetCommandLine;
end;

procedure TRipGrepperTopFrame.ActionConfigExecute(Sender : TObject);
begin
	var
	Settings := Settings.RipGrepperOpenWithSettings;
	Settings.TestFile := MainFrame.GetOpenWithParamsFromSelected();
	TOpenWithConfigForm.CreateAndShow(Settings);
	Settings.TestFile := default (TOpenWithParams);
	Settings.Reload();
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
	Settings.RipGrepperViewSettings.ExpandNodes := not Settings.RipGrepperViewSettings.ExpandNodes;
	Settings.StoreViewSettings('ExpandNodes');

	if Settings.RipGrepperViewSettings.ExpandNodes then begin
		MainFrame.VstResult.FullExpand();
	end else begin
		MainFrame.VstResult.FullCollapse();
	end;
end;

procedure TRipGrepperTopFrame.ActionExpandCollapseUpdate(Sender : TObject);
begin
	ActionExpandCollapse.ImageIndex := IfThen(Settings.RipGrepperViewSettings.ExpandNodes, 23, 22);
	ActionExpandCollapse.Hint := IfThen(Settings.RipGrepperViewSettings.ExpandNodes, 'Collapse Nodes', 'Expand Nodes');
end;

procedure TRipGrepperTopFrame.ActionIndentLineExecute(Sender : TObject);
begin
	Settings.RipGrepperViewSettings.IndentLines := not Settings.RipGrepperViewSettings.IndentLines;
	Settings.StoreViewSettings('IndentLines');
	MainFrame.VstResult.Repaint();
end;

procedure TRipGrepperTopFrame.ActionIndentLineUpdate(Sender : TObject);
begin
	tbIndentLines.Down := Settings.RipGrepperViewSettings.IndentLines;
end;

procedure TRipGrepperTopFrame.ActionOpenWithExecute(Sender : TObject);
begin
	MainFrame.ActionOpenWithExecute(Sender);
end;

procedure TRipGrepperTopFrame.ActionRefreshSearchExecute(Sender : TObject);
var
	cursor : TCursorSaver;
begin
	cursor.SetHourGlassCursor;
	MainFrame.UpdateHistObject();
	MainFrame.ClearHistoryObject();
	MainFrame.InitSearch();
	MainFrame.DoSearch();
end;

procedure TRipGrepperTopFrame.ActionRefreshSearchUpdate(Sender : TObject);
begin
	ActionRefreshSearch.Enabled := Settings.IsLoaded and (not MainFrame.IsSearchRunning) and Assigned(MainFrame.HistObject);
end;

procedure TRipGrepperTopFrame.ActionSearchExecute(Sender : TObject);
var
	cursor : TCursorSaver;
begin
	cursor.SetHourGlassCursor;

	MainFrame.AddOrUpdateHistoryItem;
	MainFrame.SetSelectedHistoryItem(MainFrame.CurrentHistoryItemIndex);

	MainFrame.Data.ClearMatchFiles;
	MainFrame.InitSearch();
	MainFrame.DoSearch();
end;

procedure TRipGrepperTopFrame.ActionSearchInResultExecute(Sender : TObject);
begin
	SelectNextFoundNode(FPrevFoundNode);
end;

procedure TRipGrepperTopFrame.ActionShowFileIconsExecute(Sender : TObject);
begin
	Settings.RipGrepperViewSettings.ShowFileIcon := not Settings.RipGrepperViewSettings.ShowFileIcon;
	Settings.StoreViewSettings('ShowFileIcon');
	MainFrame.VstResult.Repaint();
end;

procedure TRipGrepperTopFrame.ActionShowFileIconsUpdate(Sender : TObject);
begin
	tbShowFileIcon.Down := Settings.RipGrepperViewSettings.ShowFileIcon;
	// ActionShowFileIcons.ImageIndex := Ifthen(Settings.ShowFileIcon, IMG_IDX_SHOW_FILE_ICON_TRUE, IMG_IDX_SHOW_FILE_ICON_FALSE);
end;

procedure TRipGrepperTopFrame.ActionShowRelativePathExecute(Sender : TObject);
const
	PARSER_TYPES : TArray<TFileNameType> = [ftAbsolute, ftRelative];
begin
	Settings.RipGrepperViewSettings.ShowRelativePath := not Settings.RipGrepperViewSettings.ShowRelativePath;
	var
	idx := integer(Settings.RipGrepperViewSettings.ShowRelativePath);
	MainFrame.FileNameType := PARSER_TYPES[idx mod Length(PARSER_TYPES)];
	var
	arr := MainFrame.MaxWidths;
	// MainFrame.VstResult.InitMaxWidths(arr);
	MainFrame.MaxWidths := arr;
	Settings.StoreViewSettings('ShowRelativePath');
	MainFrame.VstResult.Repaint;
end;

procedure TRipGrepperTopFrame.ActionShowRelativePathUpdate(Sender : TObject);
begin
	tbShowRelativePath.Down := Settings.RipGrepperViewSettings.ShowRelativePath;
	// ActionShowRelativePath.ImageIndex := Ifthen(Settings.ShowRelativePath, IMG_IDX_SHOW_RELATIVE_PATH, IMG_IDX_SHOW_ABS_PATH);
end;

procedure TRipGrepperTopFrame.ActionShowSearchFormExecute(Sender : TObject);
var
	frm : TRipGrepperSearchDialogForm;
begin
	frm := TRipGrepperSearchDialogForm.Create(self, Settings, MainFrame.HistObject);
	try
		TDebugUtils.DebugMessage('TRipGrepperTopFrame.ActionShowSearchFormExecute');

		if (mrOk = frm.ShowModal) then begin
			TDebugUtils.DebugMessage('TRipGrepperTopFrame.ActionShowSearchFormExecute: after showmodal gui params: ' +
				Settings.RipGrepParameters.GuiSetSearchParams.ToString);
			TDebugUtils.DebugMessage('TRipGrepperTopFrame.ActionShowSearchFormExecute: after showmodal cmdline: ' + Settings.RipGrepParameters.GetCommandLine);

			ActionSearchExecute(self);
		end;

	finally
		frm.Free;
	end;
end;

procedure TRipGrepperTopFrame.ActionShowSearchFormUpdate(Sender : TObject);
begin
	ActionShowSearchForm.Enabled := Settings.IsEmpty or (not MainFrame.IsSearchRunning);
end;

procedure TRipGrepperTopFrame.ActionSwitchViewExecute(Sender : TObject);
var
	idx : integer;
begin
	idx := GetNextViewStyleIdx;
	// MainFrame.VstResult.ViewStyle := LISTVIEW_TYPES[idx];
	FViewStyleIndex := idx;
end;

procedure TRipGrepperTopFrame.ActionSwitchViewUpdate(Sender : TObject);
begin
	var
	next := GetNextViewStyleIdx();
	var
	idx := IfThen(next <= (Length(LISTVIEW_TYPES) - 1), next, 0);
	// ActionSwitchView.ImageIndex := idx + 2;
	ActionSwitchView.Hint := 'Change View ' + LISTVIEW_TYPE_TEXTS[idx];
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

function TRipGrepperTopFrame.GetNextViewStyleIdx : integer;
begin
	Result := IfThen(FViewStyleIndex < Length(LISTVIEW_TYPES) - 1, FViewStyleIndex + 1, 0);
	Result := (Result mod Length(LISTVIEW_TYPES));
end;

function TRipGrepperTopFrame.GetSettings : TRipGrepperSettings;
begin
	if not Assigned(FSettings) then begin
		FSettings := ParentFrame.Settings;;
	end;
	Result := FSettings;
end;

function TRipGrepperTopFrame.GetToolBarWidth(_tb : TToolBar) : Integer;
begin
	Result := 0;
	TDebugUtils.DebugMessage(Format('TRipGrepperTopFrame.GetToolBarWidth %s Width: %d BtnCnt:%d ', [_tb.Name, _tb.Width, _tb.ButtonCount]));

	for var i : integer := 0 to _tb.ButtonCount - 1 do begin
		Result := Result + _tb.Buttons[i].Width;
	end;
	TDebugUtils.DebugMessage(Format('TRipGrepperTopFrame.GetToolBarWidth %s Width: %d', [_tb.Name, Result, _tb.ButtonCount]));
end;

procedure TRipGrepperTopFrame.Init;
begin
	if not IOTAUTils.IsStandAlone then begin
		SearchBox1.BorderStyle := bsNone;
		Height := Height - 2;
	end;
end;

procedure TRipGrepperTopFrame.SearchBox1Change(Sender : TObject);
begin
	inherited;
	FPrevFoundNode := nil;
	SelectNextFoundNode(FPrevFoundNode);
end;

procedure TRipGrepperTopFrame.SearchForText(Sender : TBaseVirtualTree; Node : PVirtualNode; Data : Pointer; var Abort : Boolean);
var
	dataStr : string;
	NodeData : PVSFileNodeData; // replace by your record structure
begin

	NodeData := Sender.GetNodeData(Node);
	dataStr := NodeData.FilePath + ' ' + NodeData.MatchData.LineText;
	Abort := ContainsText(dataStr, string(Data));
	// abort the search if a node with the text is found.
	TDebugUtils.DebugMessage(Format('Search ''%s'' in %s', [string(Data), dataStr]));

end;

procedure TRipGrepperTopFrame.SelectNextFoundNode(const _prevFoundNode : PVirtualNode);
var
	bLast : Boolean;
	nextNode, lastNode, foundNode : PVirtualNode;
begin
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
		foundNode := MainFrame.VstResult.IterateSubtree(nextNode, SearchForText, Pointer(SearchBox1.text));
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

end.
