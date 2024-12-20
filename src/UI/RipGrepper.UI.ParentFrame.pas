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
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.UI.MiddleFrame,
	VirtualTrees,
	RipGrepper.UI.IFrameEvents;

type
	TParentFrame = class(TFrame, IFrameEvents)
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
			procedure AfterHistObjChange;
			procedure AfterSearch;
			procedure BeforeSearch;
			procedure Init;
			procedure OnClose(Sender : TObject; var Action : TCloseAction);
			procedure FrameOnShow(Sender : TObject);
			property Settings : TRipGrepperSettings read GetSettings write FSettings;
	end;

var
	ParentFrame : TParentFrame;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	System.StrUtils,
	Vcl.StdCtrls;

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
	TDebugUtils.Msg('TParentFrame.Destroy - Settings.StoreToDict');
	inherited;
end;

procedure TParentFrame.AfterHistObjChange;
begin
 	TopFrame.AfterHistObjChange();
	MainFrame.AfterHistObjChange();
	BottomFrame.AfterHistObjChange();
end;

procedure TParentFrame.AfterSearch;
begin
	TopFrame.AfterSearch();
	MainFrame.AfterSearch();
	BottomFrame.AfterSearch();
end;

procedure TParentFrame.BeforeSearch;
begin
  	TopFrame.BeforeSearch();
	MainFrame.BeforeSearch();
	BottomFrame.BeforeSearch();
end;

procedure TParentFrame.OnClose(Sender : TObject; var Action : TCloseAction);
begin
	TDebugUtils.Msg('TParentFrame.OnClose - begin action:' + Integer(Action).ToString);
	Settings.StoreToDict; // combo box histories can be StoreToDictd here
end;

procedure TParentFrame.FrameOnShow(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TParentFrame.FrameOnShow');
	Settings.LoadInitialSettings;
	TopFrame.Init();
	BottomFrame.Init();
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
		FSettings.ReadIni;
	end;
	Result := FSettings;
end;

procedure TParentFrame.Init;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TParentFrame.InitForm');
	MainFrame.Init();
	TopFrame.Init();
	BottomFrame.Init();
end;

end.
