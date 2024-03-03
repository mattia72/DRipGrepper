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
	RipGrepper.Common.Settings;

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
		ToolBar1 : TToolBar;
		tbView : TToolButton;
		tbDoSearchCancel : TToolButton;
		tbRefreshSearch : TToolButton;
		ToolButton3 : TToolButton;
		tbAbortSearch : TToolButton;
		ToolButton1 : TToolButton;
		tbCopyCmdLine : TToolButton;
		ToolButton2 : TToolButton;
		tbAlternateRowColors : TToolButton;
		tbShowFileIcon : TToolButton;
		tbShowRelativePath : TToolButton;
		tbIndentLines : TToolButton;
		ToolButton4 : TToolButton;
		ToolButton5 : TToolButton;
		ToolButton6 : TToolButton;
		tbConfigure : TToolButton;
		procedure ActionAbortSearchExecute(Sender : TObject);
		procedure ActionAbortSearchUpdate(Sender : TObject);
		procedure ActionAlternateRowColorsExecute(Sender : TObject);
		procedure ActionAlternateRowColorsUpdate(Sender : TObject);
		procedure ActionCancelExecute(Sender : TObject);
		procedure ActionCmdLineCopyExecute(Sender : TObject);
		procedure ActionCmdLineCopyUpdate(Sender : TObject);
		procedure ActionConfigExecute(Sender : TObject);
		procedure ActionCopyFileNameExecute(Sender : TObject);
		procedure ActionCopyPathToClipboardExecute(Sender : TObject);
		procedure ActionIndentLineExecute(Sender : TObject);
		procedure ActionIndentLineUpdate(Sender : TObject);
		procedure ActionOpenWithExecute(Sender: TObject);
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

		private
			FSettings : TRipGrepperSettings;
			function GetSettings : TRipGrepperSettings;
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

			{ Private-Deklarationen }
		public
			{ Public-Deklarationen }
	end;

implementation

{$R *.dfm}

uses
	RipGrepper.UI.MainForm,
	Vcl.Clipbrd,
	RipGrepper.Common.Types,
	RipGrepper.OpenWith.ConfigForm,
	RipGrepper.OpenWith.SimpleTypes,
	RipGrepper.OpenWith,
	RipGrepper.Helper.UI,
	RipGrepper.UI.SearchForm,
	System.Math;

procedure TRipGrepperTopFrame.ActionAbortSearchExecute(Sender : TObject);
begin
	if RipGrepperForm.IsSearchRunning then begin
		RipGrepperForm.RipGrepTask.Cancel;
	end;
	RipGrepperForm.AbortSearch := True;
end;

procedure TRipGrepperTopFrame.ActionAbortSearchUpdate(Sender : TObject);
begin
	ActionAbortSearch.Enabled := RipGrepperForm.IsSearchRunning;
end;

procedure TRipGrepperTopFrame.ActionAlternateRowColorsExecute(Sender : TObject);
begin
	Settings.RipGrepperViewSettings.AlternateRowColors := (not Settings.RipGrepperViewSettings.AlternateRowColors);
	Settings.StoreViewSettings('AlternateRowColors');
	RipGrepperForm.ListViewResult.Repaint();
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
	ClipBoard.AsText := Settings.RipGrepParameters.BuildCmdLine;
end;

procedure TRipGrepperTopFrame.ActionCmdLineCopyUpdate(Sender : TObject);
begin
	ActionCmdLineCopy.Hint := 'Copy command line:' + CRLF + Settings.RipGrepParameters.BuildCmdLine;
end;

procedure TRipGrepperTopFrame.ActionConfigExecute(Sender : TObject);
begin
	var
	settings := Settings.RipGrepperOpenWithSettings;
	settings.TestFile := RipGrepperForm.GetOpenWithParamsFromSelected();
	TOpenWithConfigForm.CreateAndShow(settings);
	settings.TestFile := default (TOpenWithParams);
end;

procedure TRipGrepperTopFrame.ActionCopyFileNameExecute(Sender : TObject);
begin
	RipGrepperForm.CopyToClipboardFileOfSelected();
end;

procedure TRipGrepperTopFrame.ActionCopyPathToClipboardExecute(Sender : TObject);
begin
	RipGrepperForm.CopyToClipboardPathOfSelected();
end;

procedure TRipGrepperTopFrame.ActionIndentLineExecute(Sender : TObject);
begin
	Settings.RipGrepperViewSettings.IndentLines := not Settings.RipGrepperViewSettings.IndentLines;
	Settings.StoreViewSettings('IndentLines');
	RipGrepperForm.ListViewResult.Repaint();
end;

procedure TRipGrepperTopFrame.ActionIndentLineUpdate(Sender : TObject);
begin
	tbIndentLines.Down := Settings.RipGrepperViewSettings.IndentLines;
end;

procedure TRipGrepperTopFrame.ActionOpenWithExecute(Sender: TObject);
begin
      RipGrepperForm.ActionOpenWithExecute(Sender);
end;

procedure TRipGrepperTopFrame.ActionRefreshSearchExecute(Sender : TObject);
var
	cursor : TCursorSaver;
begin
	cursor.SetHourGlassCursor;
	RipGrepperForm.UpdateHistObject();
	RipGrepperForm.ClearHistoryObject();
	RipGrepperForm.InitSearch();
	RipGrepperForm.DoSearch();
end;

procedure TRipGrepperTopFrame.ActionRefreshSearchUpdate(Sender : TObject);
begin
	ActionRefreshSearch.Enabled := Settings.IsLoaded and (not RipGrepperForm.IsSearchRunning);
end;

procedure TRipGrepperTopFrame.ActionSearchExecute(Sender : TObject);
var
	cursor : TCursorSaver;
begin
	cursor.SetHourGlassCursor;

	RipGrepperForm.AddOrUpdateHistoryItem;
	RipGrepperForm.ListBoxSearchHistory.ItemIndex := RipGrepperForm.CurrentHistoryItemIndex;

	RipGrepperForm.Data.ClearMatchFiles;
	RipGrepperForm.InitSearch();
	RipGrepperForm.DoSearch();
end;

procedure TRipGrepperTopFrame.ActionShowFileIconsExecute(Sender : TObject);
begin
	Settings.RipGrepperViewSettings.ShowFileIcon := not Settings.RipGrepperViewSettings.ShowFileIcon;
	Settings.StoreViewSettings('ShowFileIcon');
	RipGrepperForm.ListViewResult.Repaint();
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
	idx := Integer(Settings.RipGrepperViewSettings.ShowRelativePath);
	RipGrepperForm.FileNameType := PARSER_TYPES[idx mod Length(PARSER_TYPES)];
	var arr :=  RipGrepperForm.MaxWidths;
	RipGrepperForm.ListViewResult.InitMaxWidths(arr);
	RipGrepperForm.MaxWidths := arr;
	Settings.StoreViewSettings('ShowRelativePath');
	RipGrepperForm.ListViewResult.Repaint;
end;

procedure TRipGrepperTopFrame.ActionShowRelativePathUpdate(Sender : TObject);
begin
	tbShowRelativePath.Down := Settings.RipGrepperViewSettings.ShowRelativePath;
	// ActionShowRelativePath.ImageIndex := Ifthen(Settings.ShowRelativePath, IMG_IDX_SHOW_RELATIVE_PATH, IMG_IDX_SHOW_ABS_PATH);
end;

procedure TRipGrepperTopFrame.ActionShowSearchFormExecute(Sender : TObject);
var
	args : TRipGrepArguments;
begin
	args := nil;
	if Assigned(RipGrepperForm.HistObject) then begin
		args := RipGrepperForm.HistObject.RipGrepArguments;
	end;
	var
	frm := TRipGrepperSearchDialogForm.Create(self, Settings, args);
	try
		if (mrOk = frm.ShowModal) then begin
			ActionSearchExecute(self);
		end;

	finally
		frm.Free;
	end;
end;

procedure TRipGrepperTopFrame.ActionShowSearchFormUpdate(Sender : TObject);
begin
	ActionShowSearchForm.Enabled := Settings.IsEmpty or (not RipGrepperForm.IsSearchRunning);
end;

procedure TRipGrepperTopFrame.ActionSwitchViewExecute(Sender : TObject);
begin
	RipGrepperForm.ListViewResult.ViewStyle := LISTVIEW_TYPES[RipGrepperForm.ViewStyleIndex];
end;

procedure TRipGrepperTopFrame.ActionSwitchViewUpdate(Sender : TObject);
begin
	var
	next := RipGrepperForm.GetNextViewStyleIdx();
	var
	idx := IfThen(next <= (Length(LISTVIEW_TYPES) - 1), next, 0);
	// ActionSwitchView.ImageIndex := idx + 2;
	ActionSwitchView.Hint := 'Change View ' + LISTVIEW_TYPE_TEXTS[idx];
end;

function TRipGrepperTopFrame.GetSettings : TRipGrepperSettings;
begin
	if not Assigned(FSettings) then begin
		FSettings := GSettings;
	end;
	Result := FSettings;
end;

end.
