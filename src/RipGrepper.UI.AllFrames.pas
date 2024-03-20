unit RipGrepper.UI.AllFrames;

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
	RipGrepper.UI.TopFrame,
	RipGrepper.UI.MainFrame,
	RipGrepper.UI.BottomFrame,
	RipGrepper.Common.Settings;

type
	TAllFrames = class(TFrame)
		BottomFrame : TRipGrepperBottomFrame;
		MainFrame : TRipGrepperMainFrame;
		TopFrame : TRipGrepperTopFrame;

		private
			FSettings : TRipGrepperSettings;
			function GetSettings : TRipGrepperSettings;
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

			{ Private-Deklarationen }
		public
			constructor Create(AOwner : TComponent); override;
			procedure Init;
			procedure InitStatusBar;
			procedure OnClose(Sender : TObject; var Action : TCloseAction);
			procedure OnShow(Sender : TObject);
			procedure SetStatusBarMessage(const _bWithElapsedTime : Boolean = False);
			procedure SetStatusBarStatistic(const _s : string);
			{ Public-Deklarationen }
	end;

implementation

uses
	RipGrepper.Tools.DebugTools,
	RipGrepper.Common.Types,
	System.StrUtils;

{$R *.dfm}

constructor TAllFrames.Create(AOwner : TComponent);
begin
	inherited;
	Settings.Load;
end;

procedure TAllFrames.OnClose(Sender : TObject; var Action : TCloseAction);
begin
	TDebugUtils.DebugMessage('TFrames.FormClose - begin action:' + Integer(Action).ToString);
	Settings.Store;
end;

procedure TAllFrames.OnShow(Sender : TObject);
begin
	TDebugUtils.DebugMessage('TFrames.FormShow - begin');
	inherited;
	SetStatusBarMessage();
	TDebugUtils.DebugMessage('TFrames.FormShow - end');
end;

function TAllFrames.GetSettings : TRipGrepperSettings;
begin
	if not Assigned(FSettings) then begin
		FSettings := GSettings;
	end;
	Result := FSettings;
end;

procedure TAllFrames.Init;
begin
	TDebugUtils.DebugMessage('TAllFrames.InitForm Begin');

	MainFrame.Init();

	// if not IsStandAlone then begin
	// TDebugUtils.DebugMessage('RegisterDockableForm - ' + RIPGREPPER_FORM);
	// IdeDockManager.RegisterDockableForm(TAllFrames, self, RIPGREPPER_FORM);
	// end;

	TDebugUtils.DebugMessage('TAllFrames.InitForm End');
end;

procedure TAllFrames.SetStatusBarMessage(const _bWithElapsedTime : Boolean = False);
var
	msg : string;
begin
	if _bWithElapsedTime then begin
		msg := Format('Search took %s seconds with ' + EXE_AND_VERSION_FORMAT,
			[MainFrame.HistObject.ElapsedTimeText, MainFrame.ExeVersion]);
		BottomFrame.FStatusBarStatus := IfThen(MainFrame.HistObject.RipGrepResult = RIPGREP_ERROR, 'ERROR', 'SUCCES');
	end else begin
		msg := Format(EXE_AND_VERSION_FORMAT, [MainFrame.ExeVersion]);
		BottomFrame.FStatusBarStatus := 'READY';
	end;
	BottomFrame.FStatusBarMessage := msg;
end;

procedure TAllFrames.InitStatusBar;
begin
	SetStatusBarMessage();
	SetStatusBarStatistic('Ready.');
end;

procedure TAllFrames.SetStatusBarStatistic(const _s : string);
begin
	BottomFrame.FStatusBarStatistic := _s;
end;

end.
