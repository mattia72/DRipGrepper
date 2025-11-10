unit RipGrepper.Settings.AppSettings;

interface

uses
	System.Classes,
	System.IniFiles,
	System.Generics.Collections,
	System.Generics.Defaults,
	RipGrepper.OpenWith.Constants,
	RipGrepper.Common.Constants,
	RipGrepper.Settings.Persistable,
	ArrayEx,
	RipGrepper.Settings.FontColors,
	RipGrepper.Settings.RipGrepParameterSettings,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Settings.SettingVariant,
	RipGrepper.Common.LoadHistoryMode,
	Spring;

type

	TAppSettings = class(TPersistableSettings, IIniPersistable)
		const
			INI_SECTION = 'RipGrepperSettings';
			KEY_ENCODING_ITEMS = 'EncodingItems';
			KEY_COLORTHEME = 'ColorTheme';
			KEY_EXPERTMODE = 'ExpertMode';
			KEY_DEBUGTRACEREGEXFILTER = 'DebugTraceRegexFilter';
			KEY_DEBUGTRACE = 'DebugTrace';
			KEY_COPYTOCLIPBOARDSHELL = 'CopyToClipBoardShell';
			KEY_CMBHISTORYCOUNT = 'ComboHistoryCount';
			KEY_SEARCH_HISTORYCOUNT = 'SearchHistoryCount';
			KEY_CHECK_NEW_VERSION_ON_STARTUP = 'CheckNewVersionOnStartup';
			KEY_LOAD_LAST_HISTORIES_ON_STARTUP = 'LoadLastSearchesOnStartup';
			KEY_LOAD_LAST_HISTORY_MODE = 'LoadLastSearchesOnStartupMode';

		private
			FColorTheme : IStringSetting;
			FCopyToClipBoardShell : IIntegerSetting;
			FComboHistoryCount : IIntegerSetting;
			FSearchHistoryCount : IIntegerSetting;
			FLoadHistory : IBoolSetting;
			FCheckNewVersionOnStartup : IBoolSetting;
			FDebugTrace : IStringSetting;
			FDebugTraceRegexFilter : IStringSetting;
			FExpertMode: IBoolSetting;
			FEncodingItems : IStringSetting;
			FInternalLoadHistoryMode : IShared<TLoadHistoryModes>;
			FLoadHistoryMode : IStringSetting;
			function GetColorTheme() : string;
			function GetComboHistoryCount() : Integer;
			function GetSearchHistoryCount() : Integer;
			function GetCopyToClipBoardShell() : TShellType;
			function GetDebugTrace() : string;
			function GetDebugTraceRegexFilter() : string;
			function GetEncodingItems() : TArray<string>;
			function GetExpertMode(): IBoolSetting;
			function GetLoadHistoryMode(): IShared<TLoadHistoryModes>;
			function GetCheckNewVersionOnStartup : Boolean;
			procedure SetColorTheme(const Value : string);
			procedure SetComboHistoryCount(const Value : Integer);
			procedure SetSearchHistoryCount(const Value : Integer);
			procedure SetCopyToClipBoardShell(const Value : TShellType);
			procedure SetDebugTrace(const Value : string);
			procedure SetDebugTraceRegexFilter(const Value : string);
			procedure SetEncodingItems(const Value : TArray<string>);
			procedure SetLoadHistoryMode(const Value: IShared<TLoadHistoryModes>);
			procedure SetCheckNewVersionOnStartup(const Value : Boolean);

		protected
			procedure Init; override;

		public
			constructor Create(const _Owner : TPersistableSettings);
			destructor Destroy; override;
			function IsExpertMode(): Boolean;
			procedure UpdateSettingsFromInternals(); override;
			procedure UpdateInternalsFromSettings(); override;
			property ColorTheme : string read GetColorTheme write SetColorTheme;
			property ComboHistoryCount : Integer read GetComboHistoryCount write SetComboHistoryCount;
			property SearchHistoryCount : Integer read GetSearchHistoryCount write SetSearchHistoryCount;
			property CopyToClipBoardShell : TShellType read GetCopyToClipBoardShell write SetCopyToClipBoardShell;
			property DebugTrace : string read GetDebugTrace write SetDebugTrace;
			property DebugTraceRegexFilter : string read GetDebugTraceRegexFilter write SetDebugTraceRegexFilter;
			property ExpertMode: IBoolSetting read GetExpertMode;
			property EncodingItems : TArray<string> read GetEncodingItems write SetEncodingItems;
			property LoadHistoryMode: IShared<TLoadHistoryModes> read GetLoadHistoryMode
				write SetLoadHistoryMode;
			property CheckNewVersionOnStartup : Boolean read GetCheckNewVersionOnStartup write SetCheckNewVersionOnStartup;
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
	System.Variants,
	Vcl.Themes,
	RipGrepper.Helper.UI.DarkMode;

constructor TAppSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := INI_SECTION;
	inherited;
	TDebugUtils.DebugMessage('TAppSettings.Create: ' + '[' + IniSectionName + ']');
end;

destructor TAppSettings.Destroy;
begin
	inherited Destroy() // ok;
end;

function TAppSettings.GetColorTheme() : string;
begin
	Result := FColorTheme.Value;
end;

function TAppSettings.GetComboHistoryCount() : Integer;
begin
	Result := FComboHistoryCount.Value;
end;

function TAppSettings.GetSearchHistoryCount() : Integer;
begin
	Result := FSearchHistoryCount.Value;
end;

function TAppSettings.GetCopyToClipBoardShell() : TShellType;
begin
	Result := TShellType(FCopyToClipBoardShell.Value);
end;

function TAppSettings.GetDebugTrace() : string;
begin
	Result := FDebugTrace.Value;
end;

function TAppSettings.GetDebugTraceRegexFilter() : string;
begin
	Result := FDebugTraceRegexFilter.Value;
end;

function TAppSettings.GetEncodingItems() : TArray<string>;
begin
	Result := FEncodingItems.Value.Split([ARRAY_SEPARATOR]);
end;

function TAppSettings.GetExpertMode(): IBoolSetting;
begin
	Result := FExpertMode;
end;

function TAppSettings.GetLoadHistoryMode(): IShared<TLoadHistoryModes>;
begin
	Result := FInternalLoadHistoryMode;
end;

function TAppSettings.GetCheckNewVersionOnStartup : Boolean;
begin
	Result := FCheckNewVersionOnStartup.Value;
end;

procedure TAppSettings.Init;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAppSettings.Init');

	FColorTheme := TStringSetting.Create(KEY_COLORTHEME, ''); // TDarkModeHelper.GetActualThemeName);
	FCopyToClipBoardShell := TIntegerSetting.Create(KEY_COPYTOCLIPBOARDSHELL, Integer(TShellType.stPowershell));
	FComboHistoryCount := TIntegerSetting.Create(KEY_CMBHISTORYCOUNT, MAX_HISTORY_COUNT);
	FSearchHistoryCount := TIntegerSetting.Create(KEY_SEARCH_HISTORYCOUNT, MAX_HISTORY_COUNT);
	FCheckNewVersionOnStartup := TBoolSetting.Create(KEY_CHECK_NEW_VERSION_ON_STARTUP, False);
	FLoadHistory := TBoolSetting.Create(KEY_LOAD_LAST_HISTORIES_ON_STARTUP, False);
	FLoadHistoryMode := TStringSetting.Create(KEY_LOAD_LAST_HISTORY_MODE, '');
    FInternalLoadHistoryMode := Shared.Make<TLoadHistoryModes>();
	FDebugTrace := TStringSetting.Create(KEY_DEBUGTRACE, '');
	FDebugTraceRegexFilter := TStringSetting.Create(KEY_DEBUGTRACEREGEXFILTER, '');
	FEncodingItems := TStringSetting.Create(KEY_ENCODING_ITEMS, string.join(ARRAY_SEPARATOR, TDefaults.RG_PARAM_ENCODING_VALUES));
	FExpertMode := TBoolSetting.Create(KEY_EXPERTMODE, False);

	CreateSetting(FColorTheme);
	CreateSetting(FCopyToClipBoardShell);
	CreateSetting(FComboHistoryCount);
	CreateSetting(FSearchHistoryCount);
	CreateSetting(FCheckNewVersionOnStartup);
	CreateSetting(FLoadHistory);
	CreateSetting(FLoadHistoryMode);
	CreateSetting(FDebugTrace);
	CreateSetting(FDebugTraceRegexFilter);
	CreateSetting(FEncodingItems);
	CreateSetting(FExpertMode);
end;

function TAppSettings.IsExpertMode(): Boolean;
begin
	Result := FExpertMode.Value;
end;

procedure TAppSettings.SetColorTheme(const Value : string);
begin
	FColorTheme.Value := Value;
end;

procedure TAppSettings.SetComboHistoryCount(const Value : Integer);
begin
	FComboHistoryCount.Value := Value;
end;

procedure TAppSettings.SetSearchHistoryCount(const Value : Integer);
begin
	FSearchHistoryCount.Value := Value;
end;

procedure TAppSettings.SetCopyToClipBoardShell(const Value : TShellType);
begin
	FCopyToClipBoardShell.Value := Integer(Value);
end;

procedure TAppSettings.SetDebugTrace(const Value : string);
begin
	FDebugTrace.Value := Value;
end;

procedure TAppSettings.SetDebugTraceRegexFilter(const Value : string);
begin
	FDebugTraceRegexFilter.Value := Value;
end;

procedure TAppSettings.SetEncodingItems(const Value : TArray<string>);
begin
	FEncodingItems.Value := string.join(ARRAY_SEPARATOR, Value);
end;

procedure TAppSettings.SetLoadHistoryMode(const Value:
	IShared<TLoadHistoryModes>);
begin
	FInternalLoadHistoryMode := Value;
	UpdateSettingsFromInternals;
end;

procedure TAppSettings.SetCheckNewVersionOnStartup(const Value : Boolean);
begin
	FCheckNewVersionOnStartup.Value := Value;
end;

procedure TAppSettings.UpdateSettingsFromInternals();
begin
	FLoadHistoryMode.Value := FInternalLoadHistoryMode.ToString();
end;

procedure TAppSettings.UpdateInternalsFromSettings();
begin
	FInternalLoadHistoryMode.FromString(FLoadHistoryMode.Value);
end;

end.
