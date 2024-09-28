unit RipGrepper.Common.Settings.SearchFormSettings;

interface

uses
	System.Classes,
	System.IniFiles,
	System.Generics.Collections,
	System.Generics.Defaults,
	ArrayEx,
	RipGrepper.OpenWith.Constants,
	RipGrepper.Common.Constants,
	RipGrepper.Common.Settings.Persistable,
	RipGrepper.Common.Settings.RipGrepParameterSettings,
	RipGrepper.Common.Settings.AppSettings,
	RipGrepper.Common.Settings.ExtensionSettings;

type

	ERipGrepperExtensionContext = (
		{ } rgecActiveFile = EXT_SEARCH_ACTIVE_FILE,
		{ } rgecProjectFiles = EXT_SEARCH_PROJECT_FILES,
		{ } rgecOpeneFiles = EXT_SEARCH_OPEN_FILES,
		{ } rgecPath = EXT_SEARCH_GIVEN_PATH
		{ } );

	TSearchFormSettings = class(TPersistableSettings)
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
			procedure LoadMembers(const _bDefault : Boolean);
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
			procedure StoreToDict; override;
			procedure Copy(const _other : TSearchFormSettings); reintroduce;
			procedure CopyDefaultsToValues; override;
			procedure StoreAsDefaultsToDict; override;
			procedure LoadDefaultsFromDict; override;
			procedure LoadFromDict(); override;
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

constructor TSearchFormSettings.Create(const _ini : TMemIniFile);
begin
	IniSectionName := INI_SECTION;
	inherited Create(_ini);
	FExtensionSettings := TRipGrepperExtensionSettings.Create(_ini);
end;

constructor TSearchFormSettings.Create;
begin
	IniSectionName := INI_SECTION;
	inherited Create;
	FExtensionSettings := TRipGrepperExtensionSettings.Create();
end;

destructor TSearchFormSettings.Destroy;
begin
	FExtensionSettings.Free;
	inherited Destroy(); // ok;
end;

procedure TSearchFormSettings.Copy(const _other : TSearchFormSettings);
begin
	if Assigned(_other) then begin
		FExtensionSettings.Copy(_other.ExtensionSettings);
		inherited Copy(_other as TPersistableSettings);
	end;
end;

procedure TSearchFormSettings.CopyDefaultsToValues;
begin
	FExtensionSettings.CopyDefaultsToValues;
	inherited CopyDefaultsToValues;
end;

function TSearchFormSettings.GetContext : Integer;
begin
	Result := FContext;
end;

function TSearchFormSettings.GetEncoding : string;
begin
	Result := FEncoding;
end;

function TSearchFormSettings.GetExtensionSettings : TRipGrepperExtensionSettings;
begin
	if not FExtensionSettings.IsAlreadyRead then begin
		var
		dbgMsg := TDebugMsgBeginEnd.New('TSearchFormSettings.GetExtensionSettings');

		FExtensionSettings.ReadIni;
		FExtensionSettings.LoadDefaultsFromDict;
	end;
	Result := FExtensionSettings;
end;

function TSearchFormSettings.GetHidden : Boolean;
begin
	Result := FHidden;
end;

function TSearchFormSettings.GetNoIgnore : Boolean;
begin
	Result := FNoIgnore;
end;

function TSearchFormSettings.GetPretty : Boolean;
begin
	Result := FPretty;
end;

procedure TSearchFormSettings.Init;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSearchFormSettings.Init');
	SettingsDict.CreateDefaultRelevantSetting('Pretty', varBoolean, True);
	SettingsDict.CreateDefaultRelevantSetting('Hidden', varBoolean, False);
	SettingsDict.CreateDefaultRelevantSetting('NoIgnore', varBoolean, False);
	SettingsDict.CreateDefaultRelevantSetting('Context', varInteger, 0);
	SettingsDict.CreateDefaultRelevantSetting('Encoding', varString, '');
end;

procedure TSearchFormSettings.ReadIni;
begin
	FExtensionSettings.ReadIni;
	inherited ReadIni();
end;

procedure TSearchFormSettings.SetContext(const Value : Integer);
begin
	FContext := Value;
end;

procedure TSearchFormSettings.SetEncoding(const Value : string);
begin
	FEncoding := Value;
end;

procedure TSearchFormSettings.SetHidden(const Value : Boolean);
begin
	FHidden := Value;
end;

procedure TSearchFormSettings.SetNoIgnore(const Value : Boolean);
begin
	FNoIgnore := Value;
end;

procedure TSearchFormSettings.SetPretty(const Value : Boolean);
begin
	FPretty := Value;
end;

procedure TSearchFormSettings.StoreToDict;
begin
	StoreSearchSettings(False);
	FExtensionSettings.StoreToDict;
	inherited StoreToDict();
end;

procedure TSearchFormSettings.StoreAsDefaultsToDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSearchFormSettings.StoreAsDefaultsToDict');

	StoreSearchSettings(True);
	FExtensionSettings.StoreAsDefaultsToDict;
	inherited StoreAsDefaultsToDict();
end;

procedure TSearchFormSettings.LoadDefaultsFromDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSearchFormSettings.LoadDefaultsFromDict');
	LoadMembers(True);
	FExtensionSettings.LoadDefaultsFromDict;
end;

procedure TSearchFormSettings.LoadFromDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSearchFormSettings.LoadFromDict');

	LoadMembers(False);
	FExtensionSettings.LoadFromDict();
end;

procedure TSearchFormSettings.LoadMembers(const _bDefault : Boolean);
begin
	Pretty := SettingsDict.GetSetting('Pretty', _bDefault);
	Hidden := SettingsDict.GetSetting('Hidden', _bDefault);
	NoIgnore := SettingsDict.GetSetting('NoIgnore', _bDefault);
	Context := SettingsDict.GetSetting('Context', _bDefault);
	Encoding := SettingsDict.GetSetting('Encoding', _bDefault);
end;

procedure TSearchFormSettings.StoreSearchSettings(_bAsDefault : Boolean; const _s : string = '');
var
	i : integer;
begin
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.StoreSearchSettings: ' + _s);

	i := 0;
	if _s.IsEmpty then begin
		// StoreToDict all
		for i := 0 to high(SEARCH_SETTINGS) do begin
			StoreSearchSettings(_bAsDefault, SEARCH_SETTINGS[i]);
		end;
	end else if MatchStr(_s, SEARCH_SETTINGS[i]) then begin
		SettingsDict.StoreSetting(SEARCH_SETTINGS[i], Pretty, _bAsDefault);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		SettingsDict.StoreSetting(SEARCH_SETTINGS[i], Hidden, _bAsDefault);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		SettingsDict.StoreSetting(SEARCH_SETTINGS[i], NoIgnore, _bAsDefault);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		SettingsDict.StoreSetting(SEARCH_SETTINGS[i], Context, _bAsDefault);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		SettingsDict.StoreSetting(SEARCH_SETTINGS[i], Encoding, _bAsDefault);
	end else begin
		raise Exception.Create('Settings: ' + _s + ' not stored!');
	end;
end;

function TSearchFormSettings.ToLogString : string;
begin
	Result := Format('Hidden=%s NoIgnore=%s Pretty=%s Context=%d Encoding=%s Extension:[%s]',
		{ } [BoolToStr(Hidden, True),
		{ } BoolToStr(NoIgnore, True),
		{ } BoolToStr(Pretty, True),
		{ } Context,
		{ } Encoding, FExtensionSettings.ToLogString]);
end;

end.
