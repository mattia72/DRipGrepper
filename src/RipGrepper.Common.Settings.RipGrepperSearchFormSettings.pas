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

			FContext : Integer;
			FEncoding : string;
			FHidden : Boolean;
			FNoIgnore : Boolean;
			FPretty : Boolean;
			procedure SetContext(const Value : Integer);
			procedure SetEncoding(const Value : string);
			procedure SetHidden(const Value : Boolean);
			procedure SetNoIgnore(const Value : Boolean);
			procedure SetPretty(const Value : Boolean);

		public
			constructor Create(const _ini : TMemIniFile); overload;
			constructor Create; overload;

			procedure StoreSearchSettings(const _s : string = '');
			procedure Init; override;
			procedure Load; override;
			procedure Store; override;
			procedure Copy(const _other : TRipGrepperSearchFormSettings); reintroduce;
			procedure StoreAsDefault; override;

			property Context : Integer read FContext write SetContext;
			property Encoding : string read FEncoding write SetEncoding;
			property Hidden : Boolean read FHidden write SetHidden;
			property NoIgnore : Boolean read FNoIgnore write SetNoIgnore;
			property Pretty : Boolean read FPretty write SetPretty;
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

procedure TRipGrepperSearchFormSettings.Init;
begin
	inherited;
	CreateSetting('Pretty', TSettingVariant.New(varBoolean, True), True);
	CreateSetting('Hidden', TSettingVariant.New(varBoolean, False), True);
	CreateSetting('NoIgnore', TSettingVariant.New(varBoolean, False), True);
	CreateSetting('Context', TSettingVariant.New(varInteger, 0), True);
	CreateSetting('Encoding', TSettingVariant.New(varString, ''), True);
end;

procedure TRipGrepperSearchFormSettings.Load;
begin
	inherited Load();
	Pretty := LoadSetting('Pretty', True);
	Hidden := LoadSetting('Hidden', True);
	NoIgnore := LoadSetting('NoIgnore', True);
	Context := LoadSetting('Context', True);
	Encoding := LoadSetting('Encoding', True);
	FIsLoaded := True;
end;

procedure TRipGrepperSearchFormSettings.SetContext(const Value : Integer);
begin
	if FContext <> Value then begin
		FContext := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepperSearchFormSettings.SetEncoding(const Value : string);
begin
	if FEncoding <> Value then begin
		FEncoding := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepperSearchFormSettings.SetHidden(const Value : Boolean);
begin
	if FHidden <> Value then begin
		FHidden := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepperSearchFormSettings.SetNoIgnore(const Value : Boolean);
begin
	if FNoIgnore <> Value then begin
		FNoIgnore := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepperSearchFormSettings.SetPretty(const Value : Boolean);
begin
	if FPretty <> Value then begin
		FPretty := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepperSearchFormSettings.Store;
begin
	if IsLoaded and IsModified then begin
		StoreSearchSettings('');
		inherited Store();
		FIsModified := False;
	end;
end;

procedure TRipGrepperSearchFormSettings.StoreAsDefault;
begin
	StoreSearchSettings('');
	inherited StoreAsDefault();
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
