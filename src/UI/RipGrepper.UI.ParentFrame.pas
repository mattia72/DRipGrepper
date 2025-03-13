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
			procedure WMSettingChange(var Message : TWMSettingChange); message WM_SETTINGCHANGE;

		public
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;
			procedure AfterHistObjChange;
			procedure AfterSearch;
			procedure BeforeSearch(var _bAbort : Boolean);
			procedure Init;
			procedure OnClose(Sender : TObject; var Action : TCloseAction);
			procedure FrameOnShow(Sender : TObject);
			procedure UpdateUIStyle(_sNewStyle : string = '');
			property Settings : TRipGrepperSettings read GetSettings write FSettings;
	end;

var
	ParentFrame : TParentFrame;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	System.StrUtils,
	Vcl.StdCtrls,
	RipGrepper.Helper.UI.DarkMode;

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
	var
	dbgMsg := TDebugMsgBeginEnd.New('TParentFrame.Destroy');
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
	var
	dbgMsg := TDebugMsgBeginEnd.New('TParentFrame.AfterSearch');

	TopFrame.AfterSearch();
	BottomFrame.AfterSearch();
end;

procedure TParentFrame.BeforeSearch(var _bAbort : Boolean);
begin
	if _bAbort then begin
		Exit;
	end;
	TopFrame.BeforeSearch(_bAbort);
	MainFrame.BeforeSearch(_bAbort);
	BottomFrame.BeforeSearch(_bAbort);
end;

procedure TParentFrame.OnClose(Sender : TObject; var Action : TCloseAction);
begin
	TDebugUtils.Msg('TParentFrame.OnClose - begin action:' + Integer(Action).ToString);
	Settings.StoreToPersister; // combo box histories can be StoreToPersisterd here
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
	TDarkModeHelper.AllowThemes();

	MainFrame.Init();
	TopFrame.Init();
	BottomFrame.Init();
	{$IFDEF STANDALONE} // UpdateUIStyle doesn't work in dark Delphi.
	UpdateUIStyle;
	TDarkModeHelper.BroadcastThemeChanged(Handle);
	{$ENDIF}
end;

procedure TParentFrame.UpdateUIStyle(_sNewStyle : string = '');
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TParentFrame.UpdateUIStyle');

	if _sNewStyle.IsEmpty then begin
		StyleName := TDarkModeHelper.GetActualThemeName();
	end else begin
		StyleName := _sNewStyle;
	end;

	TopFrame.UpdateUIStyle(StyleName);

	dbgMsg.Msg('Theme: ' + StyleName);
end;

procedure TParentFrame.WMSettingChange(var Message : TWMSettingChange);
begin
	if SameText('ImmersiveColorSet', string(message.Section)) then begin
		UpdateUIStyle;
		// TDarkModeHelper.BroadcastThemeChanged(Handle);
	end;
end;

end.
