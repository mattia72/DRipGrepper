unit RipGrepper.Common.Settings.RipGrepperSearchFormSettings;

interface

uses
	System.Classes,
	System.IniFiles,
	System.Generics.Collections,
	System.Generics.Defaults,
	RipGrepper.OpenWith.SimpleTypes,
	RipGrepper.Common.Constants,
	RipGrepper.Common.Settings.Base,
	ArrayEx,
	RipGrepper.Common.Settings.RipGrepParameterSettings;

type

	ERipGrepperExtensionContext = (
		{ } rgecActiveFile = EXT_SEARCH_ACTIVE_FILE,
		{ } rgecProjectFiles = EXT_SEARCH_PROJECT_FILES,
		{ } rgecOpeneFiles = EXT_SEARCH_OPEN_FILES,
		{ } rgecPath = EXT_SEARCH_GIVEN_PATH
		{ } );

	TRipGrepperSearchFormSettings = class(TRipGrepperSettingsBase)
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
			function GetIniSectionName : string; override;

			procedure Init; override;
			procedure Load; override;
			procedure Store; override;
			procedure Copy(const _other : TRipGrepperSearchFormSettings);

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
	TDebugUtils.DebugMessage('TRipGrepperSearchFormSettings.Create: ' + FIniFile.FileName + '[' + GetIniSectionName + ']');
end;

constructor TRipGrepperSearchFormSettings.Create;
begin
	inherited;
end;

procedure TRipGrepperSearchFormSettings.Copy(const _other : TRipGrepperSearchFormSettings);
begin
	Context := _other.Context;
	Encoding := _other.Encoding;
	Hidden := _other.Hidden;
	NoIgnore := _other.NoIgnore;
	Pretty := _other.Pretty;
end;

function TRipGrepperSearchFormSettings.GetIniSectionName : string;
begin
	Result := INI_SECTION;
end;

procedure TRipGrepperSearchFormSettings.Init;
begin
	inherited;
	CreateSetting('Pretty', TRipGrepperSetting.New(vtBoolean, True));
	CreateSetting('Hidden', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('NoIgnore', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('Context', TRipGrepperSetting.New(vtInteger, 0));
	CreateSetting('Encoding', TRipGrepperSetting.New(vtString, ''));
end;

procedure TRipGrepperSearchFormSettings.Load;
begin
	inherited Load();
	Pretty := LoadSetting('Pretty');
	Hidden := LoadSetting('Hidden');
	NoIgnore := LoadSetting('NoIgnore');
	Context := LoadSetting('Context');
	Encoding := LoadSetting('Encoding');
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
		StoreSetting(SEARCH_SETTINGS[i], Pretty);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], Hidden);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], NoIgnore);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], Context);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], Encoding);
	end else begin
		raise Exception.Create('Settings: ' + _s + ' not stored!');
	end;
	IsModified := True;
end;

end.
