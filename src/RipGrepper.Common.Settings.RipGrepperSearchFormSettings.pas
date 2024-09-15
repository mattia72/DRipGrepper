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
	RipGrepper.Common.Settings.RipGrepParameterSettings,
	RipGrepper.Common.Settings.Misc;

type

	ERipGrepperExtensionContext = (
		{ } rgecActiveFile = EXT_SEARCH_ACTIVE_FILE,
		{ } rgecProjectFiles = EXT_SEARCH_PROJECT_FILES,
		{ } rgecOpeneFiles = EXT_SEARCH_OPEN_FILES,
		{ } rgecPath = EXT_SEARCH_GIVEN_PATH
		{ } );

	TRipGrepperSearchFormSettings = class(TPersistableSettings)
		const
			SEARCH_SETTINGS : array [0 .. 4] of string =
			{ } ('Pretty', 'Hidden', 'NoIgnore', 'Context', 'Encoding');

		const
			INI_SECTION = 'RipGrepperSearchSettings';

		private

			FContext : Integer;
			FEncoding : string;
			FExtensionSettings : TRipGrepperExtensionSettings;
			FHidden : Boolean;
			FNoIgnore : Boolean;
			FPretty : Boolean;
			function GetContext : Integer;
			function GetEncoding : string;
			function GetExtensionSettings : TRipGrepperExtensionSettings;
			function GetHidden : Boolean;
			function GetNoIgnore : Boolean;
			function GetPretty : Boolean;
			procedure SetContext(const Value : Integer);
			procedure SetEncoding(const Value : string);
			procedure SetHidden(const Value : Boolean);
			procedure SetNoIgnore(const Value : Boolean);
			procedure SetPretty(const Value : Boolean);

		public
			constructor Create(const _ini : TMemIniFile); overload;
			constructor Create; overload;
			destructor Destroy; override;

			procedure StoreSearchSettings(_bAsDefault : Boolean; const _s : string = '');
			procedure Init; override;
			procedure ReadIni; override;
			procedure Store; override;
			procedure Copy(const _other : TRipGrepperSearchFormSettings); reintroduce;
			procedure StoreAsDefault; override;
			procedure LoadDefault; override;
			procedure RefreshMembers(const _bWithDefault : Boolean); override;
			function ToLogString : string; override;

			property Context : Integer read GetContext write SetContext;
			property Encoding : string read GetEncoding write SetEncoding;
			property ExtensionSettings : TRipGrepperExtensionSettings read GetExtensionSettings write FExtensionSettings;
			property Hidden : Boolean read GetHidden write SetHidden;
			property NoIgnore : Boolean read GetNoIgnore write SetNoIgnore;
			property Pretty : Boolean read GetPretty write SetPretty;
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
	IniSectionName := INI_SECTION;
	inherited Create(_ini);
	FExtensionSettings := TRipGrepperExtensionSettings.Create(_ini);
end;

constructor TRipGrepperSearchFormSettings.Create;
begin
	IniSectionName := INI_SECTION;
	inherited Create;
	FExtensionSettings := TRipGrepperExtensionSettings.Create();
end;

destructor TRipGrepperSearchFormSettings.Destroy;
begin
	FExtensionSettings.Free;
	inherited;
end;

procedure TRipGrepperSearchFormSettings.Copy(const _other : TRipGrepperSearchFormSettings);
begin
	Context := _other.Context;
	Encoding := _other.Encoding;
	Hidden := _other.Hidden;
	NoIgnore := _other.NoIgnore;
	Pretty := _other.Pretty;
	FExtensionSettings.Copy(_other.ExtensionSettings);
end;

function TRipGrepperSearchFormSettings.GetContext : Integer;
begin
	Result := FContext;
end;

function TRipGrepperSearchFormSettings.GetEncoding : string;
begin
	Result := FEncoding;
end;

function TRipGrepperSearchFormSettings.GetExtensionSettings : TRipGrepperExtensionSettings;
begin
	if not FExtensionSettings.IsAlreadyRead then begin
		var
		dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchFormSettings.GetExtensionSettings');

		FExtensionSettings.ReadIni;
		FExtensionSettings.LoadDefault;
	end;
	Result := FExtensionSettings;
end;

function TRipGrepperSearchFormSettings.GetHidden : Boolean;
begin
	Result := FHidden;
end;

function TRipGrepperSearchFormSettings.GetNoIgnore : Boolean;
begin
	Result := FNoIgnore;
end;

function TRipGrepperSearchFormSettings.GetPretty : Boolean;
begin
	Result := FPretty;
end;

procedure TRipGrepperSearchFormSettings.Init;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchFormSettings.Init');
	CreateDefaultSetting('Pretty', varBoolean, True);
	CreateDefaultSetting('Hidden', varBoolean, False);
	CreateDefaultSetting('NoIgnore', varBoolean, False);
	CreateDefaultSetting('Context', varInteger, 0);
	CreateDefaultSetting('Encoding', varString, '');
end;

procedure TRipGrepperSearchFormSettings.ReadIni;
begin
	FExtensionSettings.ReadIni;
	inherited ReadIni();
end;

procedure TRipGrepperSearchFormSettings.SetContext(const Value : Integer);
begin
	FContext := Value;
end;

procedure TRipGrepperSearchFormSettings.SetEncoding(const Value : string);
begin
	FEncoding := Value;
end;

procedure TRipGrepperSearchFormSettings.SetHidden(const Value : Boolean);
begin
	FHidden := Value;
end;

procedure TRipGrepperSearchFormSettings.SetNoIgnore(const Value : Boolean);
begin
	FNoIgnore := Value;
end;

procedure TRipGrepperSearchFormSettings.SetPretty(const Value : Boolean);
begin
	FPretty := Value;
end;

procedure TRipGrepperSearchFormSettings.Store;
begin
	StoreSearchSettings(False);
	FExtensionSettings.Store;
	inherited Store();
end;

procedure TRipGrepperSearchFormSettings.StoreAsDefault;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchFormSettings.StoreAsDefault');

	StoreSearchSettings(True);
	FExtensionSettings.StoreAsDefault;
	inherited StoreAsDefault();
end;

procedure TRipGrepperSearchFormSettings.LoadDefault;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSearchFormSettings.LoadDefault');
	FExtensionSettings.LoadDefault;
	inherited LoadDefault();
end;

procedure TRipGrepperSearchFormSettings.RefreshMembers(const _bWithDefault : Boolean);
begin
	Pretty := GetSetting('Pretty', _bWithDefault);
	Hidden := GetSetting('Hidden', _bWithDefault);
	NoIgnore := GetSetting('NoIgnore', _bWithDefault);
	Context := GetSetting('Context', _bWithDefault);
	Encoding := GetSetting('Encoding', _bWithDefault);
	FExtensionSettings.RefreshMembers(_bWithDefault);
end;

procedure TRipGrepperSearchFormSettings.StoreSearchSettings(_bAsDefault : Boolean; const _s : string = '');
var
	i : integer;
begin
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.StoreSearchSettings: ' + _s);

	i := 0;
	if _s.IsEmpty then begin
		// store all
		for i := 0 to high(SEARCH_SETTINGS) do begin
			StoreSearchSettings(_bAsDefault, SEARCH_SETTINGS[i]);
		end;
	end else if MatchStr(_s, SEARCH_SETTINGS[i]) then begin
		StoreSetting(SEARCH_SETTINGS[i], Pretty, _bAsDefault);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], Hidden, _bAsDefault);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], NoIgnore, _bAsDefault);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], Context, _bAsDefault);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], Encoding, _bAsDefault);
	end else begin
		raise Exception.Create('Settings: ' + _s + ' not stored!');
	end;
end;

function TRipGrepperSearchFormSettings.ToLogString : string;
begin
	Result := Format('Hidden=%s NoIgnore=%s Pretty=%s Context=%d Encoding=%s Extension:[%s]',
		{ } [BoolToStr(Hidden, True),
		{ } BoolToStr(NoIgnore, True),
		{ } BoolToStr(Pretty, True),
		{ } Context,
		{ } Encoding, FExtensionSettings.ToLogString]);
end;

end.
