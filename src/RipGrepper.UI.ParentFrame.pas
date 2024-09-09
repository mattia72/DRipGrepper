unit RipGrepper.UI.ParentFrame;

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
	RipGrepper.UI.BottomFrame,
	RipGrepper.Common.Settings.RipGrepperSettings,
	u_dzDpiScaleUtils,
	RipGrepper.UI.MiddleFrame,
	VirtualTrees;

type
	TParentFrame = class(TFrame)
		BottomFrame : TRipGrepperBottomFrame;
		MainFrame : TRipGrepperMiddleFrame;
		TopFrame : TRipGrepperTopFrame;

		private
			FSettings : TRipGrepperSettings;
			function GetSettings : TRipGrepperSettings;
			procedure FrameOnShowHide(var M : TMessage); message CM_SHOWINGCHANGED;

		public
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;
			procedure Init;
			procedure InitStatusBar;
			procedure OnClose(Sender : TObject; var Action : TCloseAction);
			procedure FrameOnShow(Sender : TObject);
			procedure SetStatusBarMessage(const _bWithElapsedTime : Boolean = False);
			procedure SetStatusBarStatistic(const _s : string);
			property Settings : TRipGrepperSettings read GetSettings write FSettings;
			{ Public-Deklarationen }
	end;

var
	ParentFrame : TParentFrame;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	System.StrUtils,
	Vcl.StdCtrls,
	u_dzVclUtils;

{$R *.dfm}

constructor TParentFrame.Create(AOwner : TComponent);
begin
	inherited;
	// OnShow := FrameOnShow;
	ParentFrame := self;
	Init();
end;

destructor TParentFrame.Destroy;
begin
	//
	inherited;
end;

procedure TParentFrame.OnClose(Sender : TObject; var Action : TCloseAction);
begin
	TDebugUtils.DebugMessage('TFrames.FormClose - begin action:' + Integer(Action).ToString);
	Settings.Store;
end;

procedure TParentFrame.FrameOnShow(Sender : TObject);
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TFrames.FormShow');
	SetStatusBarMessage();
end;

procedure TParentFrame.FrameOnShowHide(var M : TMessage);
begin
	inherited;
	if Showing then begin // onShow
		FrameOnShow(self);
	end else begin // onHide

	end;
end;

function TParentFrame.GetSettings : TRipGrepperSettings;
begin
	if not Assigned(FSettings) then begin
		FSettings := GSettings;
		if not FSettings.IsAlreadyRead then begin
			FSettings.ReadIni;
		end;
	end;
	Result := FSettings;
end;

procedure TParentFrame.Init;
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TParentFrame.InitForm');
	MainFrame.Init();
	TopFrame.Init();
	BottomFrame.Init();
end;

procedure TParentFrame.SetStatusBarMessage(const _bWithElapsedTime : Boolean = False);
var
	msg : string;
begin
	if _bWithElapsedTime then begin
		msg := Format('Search took %s seconds', // with ' + FORMAT_VERSION_INFO_IN_STATUSBAR,
			[MainFrame.HistItemObject.ElapsedTimeText]); // , MainFrame.ExeVersion]);
		BottomFrame.FStatusBarStatus := IfThen(MainFrame.HistItemObject.RipGrepResult = RG_ERROR, 'ERROR', 'SUCCES');
	end else begin
		msg := Format(FORMAT_VERSION_INFO_IN_STATUSBAR, [MainFrame.ExeVersion]);
		BottomFrame.FStatusBarStatus := 'READY';
	end;
	BottomFrame.FStatusBarMessage := msg;
end;

procedure TParentFrame.InitStatusBar;
begin
	SetStatusBarMessage();
	SetStatusBarStatistic('Ready.');
end;

procedure TParentFrame.SetStatusBarStatistic(const _s : string);
begin
	BottomFrame.FStatusBarStatistic := _s;
end;

end.
