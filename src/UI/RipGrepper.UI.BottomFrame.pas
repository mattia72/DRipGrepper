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
	RipGrepper.UI.FrameBase;

type
	TRipGrepperBottomFrame = class(TFrameBase)
		pnlBottom : TPanel;
		StatusBar1 : TStatusBar;
		ActivityIndicator1 : TActivityIndicator;
		ActionList : TActionList;
		ActionStatusBar : TAction;
		procedure ActionStatusBarUpdate(Sender : TObject);
		procedure FrameResize(Sender : TObject);

		private
			FStatusBarMessage : string;
			FStatusBarStatistic : string;
			FStatusBarStatus : string;
			procedure SetStatusBarMessage;

		public
			constructor Create(AOwner : TComponent); override;
			procedure AfterHistObjChange; override;
			procedure Init; override;
			procedure AfterSearch; override;
			procedure BeforeSearch; override;
			procedure SetRunningStatus;
			procedure SetReadyStatus;
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
	System.StrUtils;

{$R *.dfm}

constructor TRipGrepperBottomFrame.Create(AOwner : TComponent);
begin
	inherited;
	BottomFrame := self;
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
	SetStatusBarMessage();
end;

procedure TRipGrepperBottomFrame.BeforeSearch;
begin
	StatusBarStatistic := 'Searching...';
end;

procedure TRipGrepperBottomFrame.FrameResize(Sender : TObject);
begin
	var
	width := MainFrame.PanelHistory.Width;
	StatusBar1.Panels[0].Width := width;
	ActivityIndicator1.Left := width + 5
end;

procedure TRipGrepperBottomFrame.Init;
begin
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
	StatusBarMessage := Format(FORMAT_VERSION_INFO_IN_STATUSBAR, [MainFrame.ExeVersion]);
	SetReadyStatus;
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

end.
