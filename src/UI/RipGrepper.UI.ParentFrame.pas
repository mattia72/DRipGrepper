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
			procedure LoadLastSearchHistory;
			procedure SaveLastSearchHistory;
			procedure WMSettingChange(var Message : TWMSettingChange); message WM_SETTINGCHANGE;
		public
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;
			procedure AfterConstruction; override;
			procedure BeforeDestruction; override;
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
	RipGrepper.Helper.UI.DarkMode,
	System.IOUtils,
	RipGrepper.UI.MiddleLeftFrame;

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

procedure TParentFrame.AfterConstruction;
begin
	inherited;
	LoadLastSearchHistory;
end;

procedure TParentFrame.AfterHistObjChange;
var
	frame : IFrameEvents;
begin
	for var i := 0 to ComponentCount - 1 do begin
		var
		comp := Components[i];
		if Supports(comp, IFrameEvents, frame) then
			frame.AfterHistObjChange();
	end;
end;

procedure TParentFrame.AfterSearch;
var
	frame : IFrameEvents;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TParentFrame.AfterSearch');

	for var i := 0 to ComponentCount - 1 do begin
		var
		comp := Components[i];
		if Supports(comp, IFrameEvents, frame) then begin
			frame.AfterSearch();
		end;
	end;
end;

procedure TParentFrame.BeforeDestruction;
begin
	inherited;
	SaveLastSearchHistory();
end;

procedure TParentFrame.BeforeSearch(var _bAbort : Boolean);
var
	frame : IFrameEvents;
begin
	if _bAbort then begin
		Exit;
	end;

	for var i := 0 to ComponentCount - 1 do begin
		var
		comp := Components[i];
		if Supports(comp, IFrameEvents, frame) then begin
			frame.BeforeSearch(_bAbort);
		end;
		if _bAbort then
			break;
	end;
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
	//SaveLastSearchHistory();
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
		FSettings.ReadFile;
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

procedure TParentFrame.LoadLastSearchHistory;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TParentFrame.LoadLastSearchHistory');
	dbgMsg.Msg('from: ' + TPath.GetFullPath(SEARCH_HISTORY_DRH));
	if Settings.AppSettings.LoadLastSearchHistory and TFile.Exists(SEARCH_HISTORY_DRH) then begin
		dbgMsg.Msg('LoadLastSearchHistory = True, FileExists');
		MiddleLeftFrame.VstHistory.LoadFromFile(SEARCH_HISTORY_DRH);
	end;
end;

procedure TParentFrame.SaveLastSearchHistory;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TParentFrame.SaveLastSearchHistory');

	dbgMsg.Msg('SaveToFile to: ' + TPath.GetFullPath(SEARCH_HISTORY_DRH));
	if Settings.AppSettings.LoadLastSearchHistory then begin
		dbgMsg.Msg('LoadLastSearchHistory = TRUE');
		MiddleLeftFrame.VstHistory.SaveToFile(SEARCH_HISTORY_DRH);
	end;
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
