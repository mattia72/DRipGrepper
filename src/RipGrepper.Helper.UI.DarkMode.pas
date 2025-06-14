unit RipGrepper.Helper.UI.DarkMode;

interface

uses
	{$IFNDEF STANDALONE}
	ToolsAPI,
	{$ENDIF}
	Winapi.Windows,
	System.UITypes,
	Vcl.Themes,
	Vcl.Controls,
	System.Classes,
	Vcl.Forms,
	Spring;

type
	EThemeMode = (tmLight, tmDark, tmSystem);

	TThemeHandler = class(TComponent) // base is TComponent it will be freed by the Owner
		private
			FThemeName : string;
			FForm : TForm;
			FOnFormCreate : TNotifyEvent;

		protected
			procedure FormCreate(Sender : TObject);

		public
			constructor Create(_form : TForm; _theme : string = ''); reintroduce;
			/// Put it in TForm.OnCreate, NOT in OnShow
			procedure HandleThemes(const _theme : string);
			procedure Init(_themeName : string);
			property OnFormCreate : TNotifyEvent read FOnFormCreate write FOnFormCreate;
	end;

	TDarkModeHelper = class
		const
			DARK_THEME_NAME = 'Carbon'; // too dark 'Windows10 Dark';
			LIGHT_THEME_NAME = 'Windows';
			SYSTEM_THEME = 'System';
			DARK_THEME_NAMES : array of string = [DARK_THEME_NAME, 'Dark' { in Delphi IDE!!! } ];
			LIGHT_THEME_NAMES : array of string = [LIGHT_THEME_NAME, 'Light' { in Delphi IDE!!! } ];

		private
			class procedure SetFixedColorInSVGImgLists(_ctrl : TWinControl; const _color : TColor);
			// Sets either a Dark Mode or non Dark mode theme based in the "AsDarkMode" boolean
			// For example:
			// SetSpecificThemeMode(False, 'TheDarkModeThemeName', 'TheLightModeThemeName');
			// Would change the application theme to the theme with the name 'TheLightModeThemeName'
			// if it exists.
			//
			class procedure SetSpecificThemeMode(const AsDarkMode : Boolean; const DarkModeThemeName : string; LightModeThemeName : string;
				_component : TComponent = nil);

		public
			class procedure AllowThemes;
			class procedure ApplyTheme(const _style : string; _component : TComponent = nil);
			class function GetActualThemeName : string;
			class procedure BroadcastThemeChanged(_handle : HWND);

			// Automatically sets a Dark Mode theme is Windows is running in Dark Mode
			// To use:
			// 1. Got to project properties
			// 2. Select appearance and choose two or more themes.  Note down the names!
			// 3. In your FormCreate (or wherever) put the following line:
			// SetAppropriateThemeMode(...);
			//
			class procedure SetAppropriateThemeMode(_component : TComponent = nil);

			class function GetActualThemeMode : EThemeMode;
			class function GetIdeSystemColor(const _color : TColor) : TColor;
			class function GetIdeStyleColor(const _styleColor : TStyleColor) : TColor;
			class function GetThemeNameByMode(const _tm : EThemeMode) : string;
			// Checks the Windows registry to see if Windows Dark Mode is enabled
			class function IsSystemDark : Boolean;
			class procedure SetIconTheme(_ctrl : TWinControl);
			class procedure SetThemeMode(const _mode : EThemeMode); overload;
			class procedure SetThemeMode(const _themeName : string; _component : TComponent = nil); overload;
	end;

type
	TThemeChangeEventSubscriber = class(TComponent)

		private
			FOnThemeChanged : Event<TNotifyEvent>; // Event record
			function GetOnThemeChanged() : IInvokableEvent<TNotifyEvent>;

		public
			constructor Create(AOwner : TComponent); override;
			procedure HandleThemeChangedEvent(Sender : TObject);
			property OnThemeChanged : IInvokableEvent<TNotifyEvent> read GetOnThemeChanged;
	end;

	{$IFNDEF STANDALONE}

	TIDEThemeHelper = class // TODO: separate IDE functions in another unit...

		public
			class procedure AllowThemes(_formClass : TCustomFormClass = nil);
			class procedure ApplyTheme(_component : TComponent = nil);
			class procedure RegisterNotifier(const _notifier : INTAIDEThemingServicesNotifier);
	end;

	{$ENDIF}

implementation

uses
	{$IFDEF MSWINDOWS}
	// for the pre-defined registry key constants
	System.Win.Registry, // for the registry read access
	{$ENDIF}

	// Used for access to TStyleManager

	Winapi.UxTheme,
	Winapi.Messages,
	System.StrUtils,
	RipGrepper.Tools.DebugUtils,
	System.SysUtils,
	Vcl.Graphics,
	SVGIconImageList;

class procedure TDarkModeHelper.AllowThemes;
begin
	SetThemeAppProperties(STAP_ALLOW_NONCLIENT or STAP_ALLOW_CONTROLS or STAP_ALLOW_WEBCONTENT);
end;

class procedure TDarkModeHelper.ApplyTheme(const _style : string; _component : TComponent = nil);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDarkModeHelper.ApplyTheme');
	dbgMsg.Msg('Applying theme: ' + _style);
	if (not _style.IsEmpty) and TStyleManager.TrySetStyle(_style) then begin
		TStyleManager.FormBorderStyle := fbsCurrentStyle;
	end;
end;

class procedure TDarkModeHelper.SetAppropriateThemeMode(_component : TComponent = nil);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDarkModeHelper.SetAppropriateThemeMode');

	if Assigned(_component) then begin
		dbgMsg.MsgFmt('Component name: %s', [_component.Name]);
	end;
	SetSpecificThemeMode(IsSystemDark, DARK_THEME_NAME, LIGHT_THEME_NAME, _component);
end;

class procedure TDarkModeHelper.SetSpecificThemeMode(const AsDarkMode : Boolean; const DarkModeThemeName : string;
	LightModeThemeName : string; _component : TComponent = nil);
var
	chosenTheme : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDarkModeHelper.SetSpecificThemeMode');

	chosenTheme := IfThen(AsDarkMode, DarkModeThemeName, LightModeThemeName);
	dbgMsg.MsgFmt('Theme name: %s', [chosenTheme]);
	if Assigned(_component) then begin
		dbgMsg.MsgFmt('Component name: %s', [_component.Name]);
	end;
	{$IFDEF STANDALONE}
	TDarkModeHelper.ApplyTheme(chosenTheme);
	{$ELSE}
	TIDEThemeHelper.ApplyTheme(_component);
	{$ENDIF}
end;

class function TDarkModeHelper.IsSystemDark : Boolean;
{$IFDEF MSWINDOWS}
const
	TheKey = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize\';
	TheValue = 'AppsUseLightTheme';
var
	Reg : TRegistry;
	{$ENDIF}
begin

	Result := False; // There is no dark side - the Jedi are victorious!

	Reg := TRegistry.Create(KEY_READ);
	try
		Reg.RootKey := HKEY_CURRENT_USER;
		if Reg.KeyExists(TheKey) then
			if Reg.OpenKey(TheKey, False) then
				try
					if Reg.ValueExists(TheValue) then
						Result := Reg.ReadInteger(TheValue) = 0;
				finally
					Reg.CloseKey;
				end;
	finally
		Reg.Free;
	end;
end;

class function TDarkModeHelper.GetActualThemeName : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDarkModeHelper.GetActualThemeName');
	{$IFDEF STANDALONE}
	Result := '';
	if Assigned(TStyleManager.ActiveStyle) then begin
		Result := TStyleManager.ActiveStyle.Name;
	end;
	{$ELSE}
	var
		themingServices : IOTAIDEThemingServices;

	if Supports(BorlandIDEServices, IOTAIDEThemingServices, ThemingServices) then begin
		Result := themingServices.ActiveTheme;
	end;
	{$ENDIF}
	dbgMsg.Msg('Thema: ' + Result);
end;

class procedure TDarkModeHelper.BroadcastThemeChanged(_handle : HWND);
begin
	SendMessage(Application.Handle, WM_THEMECHANGED, 0, 0);
	// SendMessage(Application.MainForm.Handle, CM_RECREATEWND, 0, 0);
	// InvalidateRect(_handle, nil, True); ??
end;

class function TDarkModeHelper.GetActualThemeMode : EThemeMode;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDarkModeHelper.GetActualThemeMode');

	Result := tmSystem;
	var
	themeName := GetActualThemeName;
	if (MatchStr(themeName, DARK_THEME_NAMES)) then begin
		Result := tmDark;
		dbgMsg.Msg('Mode: Dark');
	end else if (MatchStr(themeName, LIGHT_THEME_NAMES)) then begin
		Result := tmLight;
		dbgMsg.Msg('Mode: Light');
	end else begin
		dbgMsg.Msg('Mode: System');
	end;;
end;

class function TDarkModeHelper.GetIdeSystemColor(const _color : TColor) : TColor;
begin
	Result := clNone;
	{$IFNDEF STANDALONE}
	var
		themingServices : IOTAIDEThemingServices;
	if Supports(BorlandIDEServices, IOTAIDEThemingServices, ThemingServices) then begin
		Result := ThemingServices.StyleServices.GetSystemColor(_color);
	end;
	{$ENDIF}
end;

class function TDarkModeHelper.GetIdeStyleColor(const _styleColor : TStyleColor) : TColor;
begin
	Result := clNone;
	{$IFNDEF STANDALONE}
	var
		themingServices : IOTAIDEThemingServices;
	if Supports(BorlandIDEServices, IOTAIDEThemingServices, ThemingServices) then begin
		Result := ThemingServices.StyleServices.GetStyleColor(_styleColor);
	end;
	{$ENDIF}
end;

class function TDarkModeHelper.GetThemeNameByMode(const _tm : EThemeMode) : string;
begin
	{$IFDEF STANDALONE}
	var
	dark := DARK_THEME_NAME;
	var
	light := LIGHT_THEME_NAME;
	{$ELSE}
	var
	dark := 'Dark';
	var
	light := 'Light';
	{$ENDIF};

	case _tm of
		tmLight :
		Result := light;
		tmDark :
		Result := dark;
		else
		Result := SYSTEM_THEME;
	end;
end;

class procedure TDarkModeHelper.SetFixedColorInSVGImgLists(_ctrl : TWinControl; const _color : TColor);
var
	subImgList : TSVGIconImageList;
begin
	for var i := 0 to _ctrl.ComponentCount - 1 do begin
		var
		subCmp := _ctrl.Components[i];
		if subCmp is TSVGIconImageList then begin
			subImgList := subCmp as TSVGIconImageList;
			subImgList.FixedColor := _color;
			for var j := 0 to subImgList.Count - 1 do begin
				subImgList.SVGIconItems[j].FixedColor := _color;
				subImgList.SVGIconItems[j].GrayScale := False;
			end;
		end else if subCmp is TWinControl then begin
			SetFixedColorInSVGImgLists(TWinControl(subCmp), _color);
		end;
	end;
end;

class procedure TDarkModeHelper.SetIconTheme(_ctrl : TWinControl);
begin
	var
	color := clBlack;
	if tmDark = TDarkModeHelper.GetActualThemeMode then begin
		color := clGray;
	end;
	SetFixedColorInSVGImgLists(_ctrl, color);
end;

class procedure TDarkModeHelper.SetThemeMode(const _mode : EThemeMode);
begin
	SetSpecificThemeMode(_mode = tmDark, DARK_THEME_NAME, LIGHT_THEME_NAME);
end;

class procedure TDarkModeHelper.SetThemeMode(const _themeName : string; _component : TComponent = nil);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDarkModeHelper.SetThemeMode');

	{$IFDEF STANDALONE}
	var
	dark := DARK_THEME_NAME;
	{$ELSE}
	var
	dark := 'Dark';
	{$ENDIF}
	SetSpecificThemeMode(_themeName = dark, DARK_THEME_NAME, LIGHT_THEME_NAME, _component);
end;

{$IFNDEF STANDALONE}

class procedure TIDEThemeHelper.AllowThemes(_formClass : TCustomFormClass = nil);
begin
	var
		themingServices : IOTAIDEThemingServices;

	if Supports(BorlandIDEServices, IOTAIDEThemingServices, themingServices) and Assigned(_formClass) then begin
		themingServices.RegisterFormClass(_formClass);
	end;

end;

class procedure TIDEThemeHelper.ApplyTheme(_component : TComponent = nil);
var
	themingServices : IOTAIDEThemingServices;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIDEThemeHelper.ApplyTheme');

	if Supports(BorlandIDEServices, IOTAIDEThemingServices, ThemingServices) then begin
		if Assigned(_component) then begin
			dbgMsg.MsgFmt('Component name: %s', [_component.Name]);
		end;
		themingServices.ApplyTheme(_component);
	end;
end;

class procedure TIDEThemeHelper.RegisterNotifier(const _notifier : INTAIDEThemingServicesNotifier);
var
	themingServices : IOTAIDEThemingServices;
begin
	if Supports(BorlandIDEServices, IOTAIDEThemingServices, ThemingServices) then begin
		themingServices.AddNotifier(_notifier);
	end;
end;

{$ENDIF}

constructor TThemeHandler.Create(_form : TForm; _theme : string = '');
begin
	inherited Create(_form);
	FForm := _form;
	FOnFormCreate := FForm.OnCreate;
	FForm.OnCreate := self.FormCreate;
	FThemeName := _theme;
end;

procedure TThemeHandler.FormCreate(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TThemeHandler.FormCreate');
	if Assigned(FOnFormCreate) then begin
		FOnFormCreate(Self);
	end;
	{$IFNDEF STANDALONE}
	var formType : TCustomFormClass := PPointer(FForm)^; // see TObject.ClassType implementation
	dbgMsg.Msg('AllowThemes: ' + formType.ClassName);
	TIDEThemeHelper.AllowThemes(formType);
	{$ELSE}
	TDarkModeHelper.AllowThemes();
	{$ENDIF}
	HandleThemes(FThemeName);
end;

procedure TThemeHandler.HandleThemes(const _theme : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TThemeHandler.HandleThemes');
	dbgMsg.Msg('Theme: ' + _theme);
	if _theme.IsEmpty then begin
		TDarkModeHelper.SetAppropriateThemeMode(FForm);
	end else begin
		TDarkModeHelper.SetThemeMode(_theme, FForm);
	end;

	TDarkModeHelper.SetIconTheme(FForm);
end;

procedure TThemeHandler.Init(_themeName : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TThemeHandler.Init');
	dbgMsg.MsgFmt('themeName: %s', [_themeName]);
	FThemeName := _themeName;
end;

constructor TThemeChangeEventSubscriber.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
end;

procedure TThemeChangeEventSubscriber.HandleThemeChangedEvent(Sender : TObject);
begin
	FOnThemeChanged.Invoke(self);
end;

function TThemeChangeEventSubscriber.GetOnThemeChanged() : IInvokableEvent<TNotifyEvent>;
begin
	Result := FOnThemeChanged;
end;

end.
