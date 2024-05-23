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
  RipGrepper.Common.Settings,
  RipGrepper.UI.MiddleFrame,
  RipGrepper.UI.DpiScaler,
  Vcl.StdCtrls,
  Vcl.WinXCtrls,
  VirtualTrees,
  Vcl.ExtCtrls,
  Vcl.Menus;

type

  TRipGrepperTopFrame = class(TFrame)
    ImageListButtons : TImageList;
    ActionList : TActionList;
    ActionSearch : TAction;
    ActionConfig : TAction;
    ActionSwitchView : TAction;
    ActionShowRelativePath : TAction;
    ActionCmdLineCopy : TAction;
    ActionCopyFileName : TAction;
    ActionCopyPathToClipboard : TAction;
    ActionShowSearchForm : TAction;
    ActionShowFileIcons : TAction;
    ActionAlternateRowColors : TAction;
    ActionAbortSearch : TAction;
    ActionRefreshSearch : TAction;
    ActionIndentLine : TAction;
    ActionOpenWith : TAction;
    tbarSearch : TToolBar;
    tbView : TToolButton;
    tbDoSearchCancel : TToolButton;
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
    ControlBar1 : TControlBar;
    tbarConfig : TToolBar;
    PopupMenu1 : TPopupMenu;
    AlignToolbar1 : TMenuItem;
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
      FSettings : TRipGrepperSettings;
      FViewStyleIndex : integer;
      function GetSettings : TRipGrepperSettings;
      property Settings : TRipGrepperSettings read GetSettings write FSettings;

    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
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
  RipGrepper.Common.ParsedObject;

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
  MainFrame.ListBoxSearchHistory.ItemIndex := MainFrame.CurrentHistoryItemIndex;

  MainFrame.Data.ClearMatchFiles;
  MainFrame.InitSearch();
  MainFrame.DoSearch();
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
    if (mrOk = frm.ShowModal) then begin
      TDebugUtils.DebugMessage('TRipGrepperTopFrame.ActionShowSearchFormExecute: after showmodal gui params: ' +
          Settings.RipGrepParameters.GuiSetSearchParams.ToString);
      TDebugUtils.DebugMessage('TRipGrepperTopFrame.ActionShowSearchFormExecute: after showmodal cmdline: ' +
          Settings.RipGrepParameters.GetCommandLine);

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

procedure TRipGrepperTopFrame.ActionRealignToolbarsExecute(Sender : TObject);
begin
	FrameResize(self);
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

procedure TRipGrepperTopFrame.FrameResize(Sender : TObject);
begin
    tbarSearch.Top := 0;
	tbarResult.Top := 0;
	tbarConfig.Top := 0;

	tbarSearch.Left := 0;
	if MainFrame.PanelResult.Left > tbarSearch.Width then begin
		tbarResult.Left := MainFrame.PanelResult.Left;
	end else begin
		tbarResult.Left := tbarSearch.Width;
	end;
	tbarConfig.Left := Width - tbarConfig.Width;
end;

function TRipGrepperTopFrame.GetNextViewStyleIdx : integer;
begin
  Result := IfThen(FViewStyleIndex < Length(LISTVIEW_TYPES) - 1, FViewStyleIndex + 1, 0);
  Result := (Result mod Length(LISTVIEW_TYPES));
end;

function TRipGrepperTopFrame.GetSettings : TRipGrepperSettings;
begin
  if not Assigned(FSettings) then begin
    FSettings := GSettings;
  end;
  Result := FSettings;
end;

procedure TRipGrepperTopFrame.Init;
begin
 ControlBar1.Height := 31;
  tbarSearch.Height := ControlBar1.Height;
  tbarResult.Height := ControlBar1.Height;
  tbarConfig.Height := ControlBar1.Height;
end;

procedure TRipGrepperTopFrame.SearchBox1Change(Sender : TObject);
var
  foundNode : PVirtualNode;
begin
  inherited;
  // first param is your starting point. nil starts at top of tree. if you want to implement findnext
  // functionality you will need to supply the previous found node to continue from that point.
  // be sure to set the IncrementalSearchTimeout to allow users to type a few characters before starting a search.
  foundNode := MainFrame.VstResult.IterateSubtree(nil, SearchForText, Pointer(SearchBox1.text));

  if Assigned(foundNode) then begin
    MainFrame.VstResult.FocusedNode := foundNode;
    MainFrame.VstResult.Selected[foundNode] := True;
  end;
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
  TDebugUtils.DebugMessage(Format('%s in %s', [string(Data), dataStr]));
end;

end.
