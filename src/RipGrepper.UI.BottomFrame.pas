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

		public
			{ Public-Deklarationen }
			FStatusBarMessage : string;
			FStatusBarStatistic : string;
			FStatusBarStatus : string;
	end;

implementation

uses
	RipGrepper.Common.Types;

{$R *.dfm}

procedure TRipGrepperBottomFrame.ActionStatusBarUpdate(Sender : TObject);
begin
	StatusBar1.Panels[PNL_MESSAGE_IDX].Text := FStatusBarMessage;
	StatusBar1.Panels[PNL_STATUS_IDX].Text := FStatusBarStatus;
	StatusBar1.Panels[PNL_STATTS_IDX].Text := FStatusBarStatistic;
end;

end.
