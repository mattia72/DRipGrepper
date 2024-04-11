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
	Vcl.ActnList;

type
	TRipGrepperBottomFrame = class(TFrame)
		pnlBottom : TPanel;
		StatusBar1 : TStatusBar;
		ActivityIndicator1 : TActivityIndicator;
		ActionList : TActionList;
		ActionStatusBar : TAction;
		procedure ActionStatusBarUpdate(Sender : TObject);
		procedure FrameResize(Sender: TObject);

		public
			{ Public-Deklarationen }
			FStatusBarMessage : string;
			FStatusBarStatistic : string;
			FStatusBarStatus : string;
		constructor Create(AOwner: TComponent); override;
		procedure SetRunningStatus;
		procedure SetReadyStatus;
	end;

var
	BottomFrame : TRipGrepperBottomFrame;

implementation

uses
	RipGrepper.Common.Constants, RipGrepper.UI.MiddleFrame;

{$R *.dfm}

constructor TRipGrepperBottomFrame.Create(AOwner: TComponent);
begin
	inherited;
	BottomFrame := self;
end;

procedure TRipGrepperBottomFrame.ActionStatusBarUpdate(Sender : TObject);
begin
	StatusBar1.Panels[PNL_MESSAGE_IDX].Text := FStatusBarMessage;
	StatusBar1.Panels[PNL_STATUS_IDX].Text := FStatusBarStatus;
	StatusBar1.Panels[PNL_STATTS_IDX].Text := FStatusBarStatistic;
end;

procedure TRipGrepperBottomFrame.FrameResize(Sender: TObject);
begin
	var width := MainFrame.PanelHistory.Width;
	StatusBar1.Panels[0].Width := width;
	ActivityIndicator1.Left := width + 5
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

end.
