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
			FbNewVersioMsgBoxAlreadyShown : Boolean;
			FIsInitialized : Boolean;
			FSettings : TRipGrepperSettings;
			function GetSettings : TRipGrepperSettings;
			procedure FrameOnShowHide(var M : TMessage); message CM_SHOWINGCHANGED;
			function GetIsInitialized() : Boolean;
			procedure LoadHistory;
			function GetSearchHistoryPath : string;
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
			procedure Initialize();
			procedure OnClose(Sender : TObject; var Action : TCloseAction);
			procedure FrameOnShow(Sender : TObject);
			procedure UpdateUIStyle(_sNewStyle : string = '');
			property IsInitialized : Boolean read GetIsInitialized;
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
	{$IFNDEF STANDALONE}
	RipGrepper.Common.IOTAUtils,
	{$ENDIF}
	RipGrepper.UI.MiddleLeftFrame,
	Spring.DesignPatterns,
	RipGrepper.Helper.UI,
	RipGrepper.Tools.ReleaseUtils;

{$R *.dfm}

constructor TParentFrame.Create(AOwner : TComponent);
begin
	inherited;
	ParentFrame := self;
	FIsInitialized := False;
	FbNewVersioMsgBoxAlreadyShown := False;
	Initialize();
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
	LoadHistory;
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

{ TParentFrame }

procedure TParentFrame.BeforeDestruction();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TParentFrame.BeforeDestruction');

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
	TopFrame.Initialize();
	BottomFrame.Initialize();
	if (not FbNewVersioMsgBoxAlreadyShown) and Settings.AppSettings.CheckNewVersionOnStartup then begin
		var
			ru : TReleaseUtils;
		ru.ShowNewVersionMsgBox(True);
		FbNewVersioMsgBoxAlreadyShown := True;
	end;
end;

procedure TParentFrame.FrameOnShowHide(var M : TMessage);
begin
	inherited;
	if Showing then begin // onShow
		FrameOnShow(self);
	end else begin // onHide

	end;
end;

function TParentFrame.GetIsInitialized() : Boolean;
begin
	Result := FIsInitialized;
end;

function TParentFrame.GetSettings : TRipGrepperSettings;
begin
	if not Assigned(FSettings) then begin
		FSettings := TSingleton.GetInstance<TRipGrepperSettings>();
		FSettings.ReadFile;
	end;
	Result := FSettings;
end;

procedure TParentFrame.Initialize();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TParentFrame.Initialize');

	if IsInitialized then begin
		dbgMsg.Msg('Already initialized');
		Exit;
	end;

	TDarkModeHelper.AllowThemes();

	MainFrame.Initialize();
	TopFrame.Initialize();
	BottomFrame.Initialize();

	{$IFDEF STANDALONE} // UpdateUIStyle doesn't work in dark Delphi.
	UpdateUIStyle;
	TDarkModeHelper.BroadcastThemeChanged(Handle);
	{$ENDIF}
	FIsInitialized := True;
end;

procedure TParentFrame.LoadHistory;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TParentFrame.LoadHistory');
	dbgMsg.Msg('from: ' + TPath.GetFullPath(GetSearchHistoryPath()));
	if Settings.AppSettings.LoadHistoryMode.IsSaveHistoryActive and TFile.Exists(GetSearchHistoryPath()) then begin
		dbgMsg.Msg('LoadHistory = True, FileExists');
		try
			MiddleLeftFrame.VstHistory.LoadFromFile(GetSearchHistoryPath());
		except
			on E : Exception do
				TMsgBox.ShowError('Error occurred while loading saved searches.');
		end;
	end;
end;

procedure TParentFrame.SaveLastSearchHistory;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TParentFrame.SaveLastSearchHistory');
	try
		dbgMsg.Msg('SaveToFile to: ' + TPath.GetFullPath(GetSearchHistoryPath()));
		if Settings.AppSettings.LoadHistoryMode.IsSaveHistoryActive then begin
			dbgMsg.Msg('LoadHistory = TRUE');
			MiddleLeftFrame.VstHistory.SaveToFile(GetSearchHistoryPath());
		end;
	except
		on E : Exception do
			dbgMsg.ErrorMsg(E.Message);
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

function TParentFrame.GetSearchHistoryPath : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TParentFrame.GetSearchHistoryPath');
	{$IFDEF STANDALONE}
	Result := TPath.Combine(TPath.GetDirectoryName(Application.ExeName), SEARCH_HISTORY_DRH);
	{$ELSE}
	Result := TPath.Combine(IOTAUTils.GetSettingFilePath, SEARCH_HISTORY_DRH);
	{$ENDIF}
	dbgMsg.Msg('Result: ' + Result);
end;

end.
