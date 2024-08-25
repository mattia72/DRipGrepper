unit RipGrepper.Common.Settings.RipGrepperSearchFormSettings;

interface

uses
	System.Classes,
	System.IniFiles,
	System.Generics.Collections,
	System.Generics.Defaults,
	RipGrepper.OpenWith.Constants,
	RipGrepper.Common.Constants,
	RipGrepper.Common.Settings.Persistable,
	ArrayEx,
	RipGrepper.Common.Settings.RipGrepParameterSettings;

type

	ERipGrepperExtensionContext = (
		{ } rgecActiveFile = EXT_SEARCH_ACTIVE_FILE,
		{ } rgecProjectFiles = EXT_SEARCH_PROJECT_FILES,
		{ } rgecOpeneFiles = EXT_SEARCH_OPEN_FILES,
		{ } rgecPath = EXT_SEARCH_GIVEN_PATH
		{ } );

	TRipGrepperSearchFormSettings = class(TPersistableSettings)
		const
			SEARCH_SETTINGS : array [0 .. 4] of string = ('Pretty', 'Hidden', 'NoIgnore', 'Context', 'Encoding');

		const
			INI_SECTION = 'RipGrepperSearchSettings';

		private

			function GetContext: Integer;
			function GetEncoding: string;
			function GetHidden: Boolean;
			function GetNoIgnore: Boolean;
			function GetPretty: Boolean;
			procedure SetContext(const Value: Integer);
			procedure SetEncoding(const Value: string);
			procedure SetHidden(const Value: Boolean);
			procedure SetNoIgnore(const Value: Boolean);
			procedure SetPretty(const Value: Boolean);

		public
			constructor Create(const _ini : TMemIniFile); overload;
			constructor Create; overload;

			procedure StoreSearchSettings(const _s : string = '');
			procedure Init; override;
			procedure Load; override;
			procedure Store; override;
			procedure Copy(const _other : TRipGrepperSearchFormSettings); reintroduce;
			procedure StoreAsDefault; override;
			procedure LoadDefault; override;

			property Context: Integer read GetContext write SetContext;
			property Encoding: string read GetEncoding write SetEncoding;
			property Hidden: Boolean read GetHidden write SetHidden;
			property NoIgnore: Boolean read GetNoIgnore write SetNoIgnore;
			property Pretty: Boolean read GetPretty write SetPretty;
	end;

implementation

uses
	System.SysUtils,
	Vcl.Forms,
	System.StrUtils,
	RipGrepper.Helper.Types,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Tools.FileUtils,
	Vcl.Dialogs,
	System.IOUtils,
	Winapi.Windows,
	System.UITypes,
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.Helper.UI,
	Vcl.Menus,
	System.RegularExpressions,
	RipGrepper.CommandLine.Builder,
	RipGrepper.Common.IOTAUtils;

constructor TRipGrepperSearchFormSettings.Create(const _ini : TMemIniFile);
begin
	inherited;
end;

constructor TRipGrepperSearchFormSettings.Create;
begin
	inherited;
	IniSectionName := INI_SECTION;
end;

procedure TRipGrepperSearchFormSettings.Copy(const _other : TRipGrepperSearchFormSettings);
begin
	Context := _other.Context;
	Encoding := _other.Encoding;
	Hidden := _other.Hidden;
	NoIgnore := _other.NoIgnore;
	Pretty := _other.Pretty;
end;

function TRipGrepperSearchFormSettings.GetContext: Integer;
begin
	Result := GetSetting('Context');
end;

function TRipGrepperSearchFormSettings.GetEncoding: string;
begin
	Result := GetSetting('Encoding');
end;

function TRipGrepperSearchFormSettings.GetHidden: Boolean;
begin
	Result := GetSetting('Hidden');
end;

function TRipGrepperSearchFormSettings.GetNoIgnore: Boolean;
begin
	Result := GetSetting('NoIgnore');
end;

function TRipGrepperSearchFormSettings.GetPretty: Boolean;
begin
	Result := GetSetting('Pretty');
end;

procedure TRipGrepperSearchFormSettings.Init;
begin
	inherited;
	CreateDefaultSetting('Pretty',   varBoolean, True);
	CreateDefaultSetting('Hidden',   varBoolean, False);
	CreateDefaultSetting('NoIgnore', varBoolean, False);
	CreateDefaultSetting('Context',  varInteger, 0);
	CreateDefaultSetting('Encoding', varString, '');
end;

procedure TRipGrepperSearchFormSettings.Load;
begin
	inherited Load();
end;

procedure TRipGrepperSearchFormSettings.SetContext(const Value: Integer);
begin
	SetSettingValue('Context', Value);
end;

procedure TRipGrepperSearchFormSettings.SetEncoding(const Value: string);
begin
	SetSettingValue('Encoding', Value);
end;

procedure TRipGrepperSearchFormSettings.SetHidden(const Value: Boolean);
begin
	SetSettingValue('Hidden', Value);
end;

procedure TRipGrepperSearchFormSettings.SetNoIgnore(const Value: Boolean);
begin
	SetSettingValue('NoIgnore', Value);
end;

procedure TRipGrepperSearchFormSettings.SetPretty(const Value: Boolean);
begin
	SetSettingValue('Pretty', Value);
end;

procedure TRipGrepperSearchFormSettings.Store;
begin
	inherited Store();
end;

procedure TRipGrepperSearchFormSettings.StoreAsDefault;
begin
	StoreSearchSettings();
	inherited StoreAsDefault();
end;

procedure TRipGrepperSearchFormSettings.LoadDefault;
begin
	inherited LoadDefault();
end;

procedure TRipGrepperSearchFormSettings.StoreSearchSettings(const _s : string = '');
var
	i : integer;
begin
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.StoreSearchSettings: ' + _s);

	i := 0;
	if _s.IsEmpty then begin
		// store all
		for i := 0 to high(SEARCH_SETTINGS) do begin
			StoreSearchSettings(SEARCH_SETTINGS[i]);
		end;
	end else if MatchStr(_s, SEARCH_SETTINGS[i]) then begin
		StoreSetting(SEARCH_SETTINGS[i], Pretty, True);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], Hidden, True);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], NoIgnore, True);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], Context, True);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], Encoding, True);
	end else begin
		raise Exception.Create('Settings: ' + _s + ' not stored!');
	end;
	IsModified := True;
end;

end.
