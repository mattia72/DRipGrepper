unit RipGrepper.UI.BottomFrame;

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
	Vcl.WinXCtrls,
	Vcl.ComCtrls,
	Vcl.ExtCtrls,
	System.Actions,
	Vcl.ActnList,
	RipGrepper.UI.IFrameEvents;

type
	TRipGrepperBottomFrame = class(TFrame, IFrameEvents)
		pnlBottom : TPanel;
		StatusBar1 : TStatusBar;
		ActivityIndicator1 : TActivityIndicator;
		ActionList : TActionList;
		ActionStatusBar : TAction;
		procedure ActionStatusBarUpdate(Sender : TObject);
		procedure FrameResize(Sender : TObject);
		procedure StatusBar1Click(Sender : TObject);

		private
			FIsInitialized : Boolean;
			FStatusBarMessage : string;
			FStatusBarStatistic : string;
			FStatusBarStatus : string;
			function GetIsInitialized() : Boolean;
			function IsPanelInfoClicked : Boolean;
			procedure ShowAboutDialog;

		public
			constructor Create(AOwner : TComponent); override;
			procedure AfterHistObjChange;
			procedure Initialize();
			procedure AfterSearch;
			procedure BeforeSearch(var _bAbort : Boolean);
			procedure SetRunningStatus;
			procedure SetReadyStatus;
			procedure SetStatusBarMessage;
			procedure UpdateUIStyle(_sNewStyle : string = '');
			property IsInitialized : Boolean read GetIsInitialized;
			property StatusBarMessage : string read FStatusBarMessage write FStatusBarMessage;
			property StatusBarStatistic : string read FStatusBarStatistic write FStatusBarStatistic;
			property StatusBarStatus : string read FStatusBarStatus write FStatusBarStatus;
	end;

var
	BottomFrame : TRipGrepperBottomFrame;

implementation

uses
	RipGrepper.Common.Constants,
	RipGrepper.UI.MiddleFrame,
	System.StrUtils,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Helper.UI.DarkMode;

{$R *.dfm}

constructor TRipGrepperBottomFrame.Create(AOwner : TComponent);
begin
	inherited;
	BottomFrame := self;
	FIsInitialized := False;
end;

procedure TRipGrepperBottomFrame.ActionStatusBarUpdate(Sender : TObject);
begin
	StatusBar1.Panels[PNL_MESSAGE_IDX].Text := FStatusBarMessage;
	StatusBar1.Panels[PNL_STATUS_IDX].Text := FStatusBarStatus;
	StatusBar1.Panels[PNL_STATISTIC_IDX].Text := FStatusBarStatistic;
end;

procedure TRipGrepperBottomFrame.AfterHistObjChange;
begin
	SetStatusBarMessage();
end;

procedure TRipGrepperBottomFrame.AfterSearch;
begin
	// SetStatusBarMessage();
end;

procedure TRipGrepperBottomFrame.BeforeSearch(var _bAbort : Boolean);
begin
	if _bAbort then begin
		// StatusBarStatistic := 'ERROR';
	end else begin;
		StatusBarStatistic := 'Searching...';
	end;
end;

procedure TRipGrepperBottomFrame.FrameResize(Sender : TObject);
begin
	var
	hisWidth := MainFrame.PanelHistory.Width;
	StatusBar1.Panels[PNL_STATISTIC_IDX].Width := hisWidth;
	StatusBar1.Panels[PNL_MESSAGE_IDX].Width := StatusBar1.Width - hisWidth - StatusBar1.Panels[PNL_STATUS_IDX].Width -
		StatusBar1.Panels[PNL_INFO_IDX].Width;
	ActivityIndicator1.Left := hisWidth + 5;
end;

function TRipGrepperBottomFrame.GetIsInitialized() : Boolean;
begin
	Result := FIsInitialized;
end;

procedure TRipGrepperBottomFrame.Initialize();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperBottomFrame.Initialize');

	if IsInitialized then begin
		dbgMsg.Msg('Already initialized');
		Exit;
	end;
	if IsInitialized then begin
		Exit;
	end;

	{$IFDEF STANDALONE}
	var
	bStandalone := True;
	{$ELSE}
	var
	bStandalone := False;
	{$ENDIF}
	if not bStandalone then begin
		Height := Height - 5;
	end;
	StatusBarMessage := Format(FORMAT_VERSION_INFO_IN_STATUSBAR, [MainFrame.ModuleNameAndVersion]);
	SetReadyStatus;
	FIsInitialized := True;
end;

function TRipGrepperBottomFrame.IsPanelInfoClicked : Boolean;
var
	pt : TPoint;
	i : Integer;
	xSum : Integer;
	panelIdx : Integer;
begin
	Result := True;
	pt := StatusBar1.ScreenToClient(Mouse.CursorPos);
	xSum := 0;
	panelIdx := -1;
	for i := 0 to StatusBar1.Panels.Count - 1 do begin
		xSum := xSum + StatusBar1.Panels[i].Width;
		if pt.X < xSum then begin
			panelIdx := i;
			Break;
		end;
	end;
	if panelIdx <> PNL_INFO_IDX then begin
		Result := False;
	end;
end;

procedure TRipGrepperBottomFrame.SetRunningStatus;
begin
	ActivityIndicator1.Animate := True;
	FStatusBarStatus := 'RUNNING';
end;

procedure TRipGrepperBottomFrame.SetReadyStatus;
begin
	ActivityIndicator1.Animate := False;
	FStatusBarStatus := 'READY';
end;

procedure TRipGrepperBottomFrame.SetStatusBarMessage;
var
	msg : string;
begin
	msg := Format('Search took %s seconds', // with ' + FORMAT_VERSION_INFO_IN_STATUSBAR,
		[MainFrame.HistItemObject.ElapsedTimeText]); // , MainFrame.ExeVersion]);
	StatusBarStatus := IfThen(MainFrame.HistItemObject.RipGrepResult = RG_ERROR, 'ERROR', 'SUCCESS');
	StatusBarMessage := msg;
end;

procedure TRipGrepperBottomFrame.ShowAboutDialog;
var
	dlg : TTaskDialog;
begin
	dlg := TTaskDialog.Create(self);
	try
		dlg.Caption := 'About DRipGrepper';
		dlg.Title := MainFrame.ModuleNameAndVersion;
		dlg.Text := 'A ripgrep GUI for Windows.' + CRLF2 +
		{ } '<A HREF="https://github.com/mattia72/DripGrepper">https://github.com/mattia72/DripGrepper</A>';
		dlg.MainIcon := tdiInformation;
		dlg.Flags := [tfEnableHyperlinks]; // Enable hyperlink support
		dlg.CommonButtons := [tcbOk];
		dlg.Execute();
	finally
		dlg.Free;
	end;
end;

procedure TRipGrepperBottomFrame.StatusBar1Click(Sender : TObject);
begin
	if IsPanelInfoClicked then
		ShowAboutDialog;
end;

procedure TRipGrepperBottomFrame.UpdateUIStyle(_sNewStyle : string = '');
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperBottomFrame.UpdateUIStyle');

	if _sNewStyle.IsEmpty then begin
		StyleName := TDarkModeHelper.GetActualThemeName();
	end else begin
		StyleName := _sNewStyle;
	end;
	dbgMsg.Msg('Theme: ' + StyleName);
end;

end.
