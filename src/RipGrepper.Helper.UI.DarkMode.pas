unit RipGrepper.Helper.UI.DarkMode;

//
// Originally written by Ian Barker
// https://github.com/checkdigits
// https://about.me/IanBarker
// ian.barker@gmail.com
//
// Free software - use for any purpose including commercial use.
//

interface

type
	EThemeMode = (tmLight, tmDark);

	TDarkModeHelper = class
		const
			DARK_THEME_NAME = 'Carbon';

		const
			LIGHT_THEME_NAME = 'Windows10';

		private
		public
			// Checks the Windows registry to see if Windows Dark Mode is enabled
			class function DarkModeIsEnabled : boolean;
			class function GetActualThemaName : string;
			class procedure RefreshThemes;

			// Automatically sets a Dark Mode theme is Windows is running in Dark Mode
			// To use:
			// 1. Got to project properties
			// 2. Select appearance and choose two or more themes.  Note down the names!
			// 3. In your FormCreate (or wherever) put the following line:
			// SetAppropriateThemeMode(**name_of_the_dark_theme**, **namme_of_the_non_dark_theme**);
			//
			// For example:
			// SetAppropriateThemeMode('Carbon', 'Windows10');
			//
			class procedure SetAppropriateThemeMode(const DarkModeThemeName, LightModeThemeName : string);

			// Sets either a Dark Mode or non Dark mode theme based in the "AsDarkMode" boolean
			// For example:
			// SetSpecificThemeMode(False, 'TheDarkModeThemeName', 'TheLightModeThemeName');
			// Would change the application theme to the theme with the name 'TheLightModeThemeName'
			// if it exists.
			//
			class procedure SetSpecificThemeMode(const AsDarkMode : Boolean; const DarkModeThemeName, LightModeThemeName : string);

			class function GetActualThemeMode : EThemeMode;
			class procedure SetThemeMode(const _mode : EThemeMode);
	end;

implementation

uses
	{$IFDEF MSWINDOWS}
	Winapi.Windows, // for the pre-defined registry key constants
	System.Win.Registry, // for the registry read access
	{$ENDIF}
	Vcl.Forms,
	Vcl.Controls,
	VCL.themes, // Used for access to TStyleManager
	System.Classes,
	Winapi.UxTheme,
	Winapi.Messages,
	System.StrUtils;

class procedure TDarkModeHelper.SetAppropriateThemeMode(const DarkModeThemeName, LightModeThemeName : string);
begin
	SetSpecificThemeMode(DarkModeIsEnabled, DarkModeThemeName, LightModeThemeName);
end;

class procedure TDarkModeHelper.SetSpecificThemeMode(const AsDarkMode : Boolean; const DarkModeThemeName, LightModeThemeName : string);
var
	ChosenTheme : string;
begin
	ChosenTheme := IfThen(AsDarkMode, DarkModeThemeName, LightModeThemeName);
	TStyleManager.TrySetStyle(ChosenTheme, False);
end;

class function TDarkModeHelper.DarkModeIsEnabled : boolean;
{$IFDEF MSWINDOWS}
const
	TheKey = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize\';
	TheValue = 'AppsUseLightTheme';
var
	Reg : TRegistry;
	{$ENDIF}
begin

	Result := False; // There is no dark side - the Jedi are victorious!

	// This relies on a registry setting only available on MS Windows
	// If the developer has somehow managed to get to this point then tell
	// them not to do this!
	{$IFNDEF MSWINDOWS}
	{$MESSAGE WARN '"DarkModeIsEnabled" will only work on MS Windows targets'}
	{$ELSE}
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
	{$ENDIF}
end;

class function TDarkModeHelper.GetActualThemaName : string;
begin
	Result := '';
	if Assigned(TStyleManager.ActiveStyle) then begin
		Result := TStyleManager.ActiveStyle.Name;
	end;
end;

class procedure TDarkModeHelper.RefreshThemes;
begin
	SetThemeAppProperties(STAP_ALLOW_NONCLIENT or STAP_ALLOW_CONTROLS or STAP_ALLOW_WEBCONTENT);
	SendMessage(Application.Handle, WM_THEMECHANGED, 0, 0);
	SendMessage(Application.MainForm.Handle, CM_RECREATEWND, 0, 0);
end;

class function TDarkModeHelper.GetActualThemeMode : EThemeMode;
begin
	Result := tmLight;
	if (GetActualThemaName = DARK_THEME_NAME) then begin
		Result := tmDark;
	end;
end;

class procedure TDarkModeHelper.SetThemeMode(const _mode : EThemeMode);
begin
	SetSpecificThemeMode(_mode = tmDark, DARK_THEME_NAME, LIGHT_THEME_NAME);
end;

end.
