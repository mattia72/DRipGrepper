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
	RipGrepper.Common.Settings,
	u_dzDpiScaleUtils,
	RipGrepper.UI.MiddleFrame,
	RipGrepper.UI.FrameEx,
	VirtualTrees;

type
	TParentFrame = class(TFrameEx)
		BottomFrame : TRipGrepperBottomFrame;
		MainFrame : TRipGrepperMiddleFrame;
		TopFrame : TRipGrepperTopFrame;

		private
			FSettings : TRipGrepperSettings;
			function GetSettings : TRipGrepperSettings;
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

			{ Private-Deklarationen }
		protected
		public
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;
			procedure Init;
			procedure InitStatusBar;
			procedure OnClose(Sender : TObject; var Action : TCloseAction);
			procedure FrameOnShow(Sender : TObject);
			procedure SetStatusBarMessage(const _bWithElapsedTime : Boolean = False);
			procedure SetStatusBarStatistic(const _s : string);
			{ Public-Deklarationen }
	end;

var
	ParentFrame : TParentFrame;

implementation

uses
	RipGrepper.Tools.DebugTools,
	RipGrepper.Common.Types,
	System.StrUtils,
	Vcl.StdCtrls,
	u_dzVclUtils;

{$R *.dfm}

constructor TParentFrame.Create(AOwner : TComponent);
begin
	inherited;
	OnShow := FrameOnShow;
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
	TDebugUtils.DebugMessage('TFrames.FormShow - begin');
	SetStatusBarMessage();
	TDebugUtils.DebugMessage('TFrames.FormShow - end');
end;

function TParentFrame.GetSettings : TRipGrepperSettings;
begin
	if not Assigned(FSettings) then begin
		FSettings := GSettings;
	end;
	Result := FSettings;
end;

procedure TParentFrame.Init;
begin
	TDebugUtils.DebugMessage('TParentFrame.InitForm Begin');
	MainFrame.Init();
	TDebugUtils.DebugMessage('TParentFrame.InitForm End');
end;

procedure TParentFrame.SetStatusBarMessage(const _bWithElapsedTime : Boolean = False);
var
	msg : string;
begin
	if _bWithElapsedTime then begin
		msg := Format('Search took %s seconds', //with ' + EXE_AND_VERSION_FORMAT,
			[MainFrame.HistObject.ElapsedTimeText]); //, MainFrame.ExeVersion]);
		BottomFrame.FStatusBarStatus := IfThen(MainFrame.HistObject.RipGrepResult = RIPGREP_ERROR, 'ERROR', 'SUCCES');
	end else begin
		msg := Format(EXE_AND_VERSION_FORMAT, [MainFrame.ExeVersion]);
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
