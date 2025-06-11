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
	RipGrepper.Settings.SettingVariant;

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
			KEY_LOADLASTHISTORIESONSTARTUP = 'LoadLastSearchesOnStartup';
			KEY_LOADLASTHISTORIESONSTARTUP_REUSEDONLY = 'LoadLastSearchesOnStartup_ReusedOnly';

		private
			FColorTheme : IStringSetting;
			FCopyToClipBoardShell : IIntegerSetting;
			FComboHistoryCount : IIntegerSetting;
			FSearchHistoryCount : IIntegerSetting;
			FLoadLastSearchHistory : IBoolSetting;
			FDebugTrace : IStringSetting;
			FDebugTraceRegexFilter : IStringSetting;
			FExpertMode : IBoolSetting;
			FEncodingItems : IStringSetting;
			FLoadLastSearchHistoryMode : IIntegerSetting;
			function GetColorTheme() : string;
			function GetComboHistoryCount() : Integer;
			function GetSearchHistoryCount() : Integer;
			function GetCopyToClipBoardShell() : TShellType;
			function GetDebugTrace() : string;
			function GetDebugTraceRegexFilter() : string;
			function GetEncodingItems() : TArray<string>;
			function GetExpertMode() : Boolean;
			function GetLoadLastSearchHistory() : Boolean;
			function GetLoadLastSearchHistoryMode() : ELoadLastHistoryMode;
			procedure SetColorTheme(const Value : string);
			procedure SetComboHistoryCount(const Value : Integer);
			procedure SetSearchHistoryCount(const Value : Integer);
			procedure SetCopyToClipBoardShell(const Value : TShellType);
			procedure SetDebugTrace(const Value : string);
			procedure SetDebugTraceRegexFilter(const Value : string);
			procedure SetEncodingItems(const Value : TArray<string>);
			procedure SetExpertMode(const Value : Boolean);
			procedure SetLoadLastSearchHistory(const Value : Boolean);
			procedure SetLoadLastSearchHistoryMode(const Value : ELoadLastHistoryMode);

		protected
			procedure Init; override;

		public
			constructor Create(const _Owner : TPersistableSettings);
			destructor Destroy; override;
			property ColorTheme : string read GetColorTheme write SetColorTheme;
			property ComboHistoryCount : Integer read GetComboHistoryCount write SetComboHistoryCount;
			property SearchHistoryCount : Integer read GetSearchHistoryCount write SetSearchHistoryCount;
			property CopyToClipBoardShell : TShellType read GetCopyToClipBoardShell write SetCopyToClipBoardShell;
			property DebugTrace : string read GetDebugTrace write SetDebugTrace;
			property DebugTraceRegexFilter : string read GetDebugTraceRegexFilter write SetDebugTraceRegexFilter;
			property ExpertMode : Boolean read GetExpertMode write SetExpertMode;
			property EncodingItems : TArray<string> read GetEncodingItems write SetEncodingItems;
			property LoadLastSearchHistory : Boolean read GetLoadLastSearchHistory write SetLoadLastSearchHistory;
			property LoadLastSearchHistoryMode : ELoadLastHistoryMode read GetLoadLastSearchHistoryMode write SetLoadLastSearchHistoryMode;
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

function TAppSettings.GetExpertMode() : Boolean;
begin
	Result := FExpertMode.Value;
end;

function TAppSettings.GetLoadLastSearchHistory() : Boolean;
begin
	Result := FLoadLastSearchHistory.Value;
end;

function TAppSettings.GetLoadLastSearchHistoryMode() : ELoadLastHistoryMode;
begin
	Result := ELoadLastHistoryMode(FLoadLastSearchHistoryMode.Value);
end;

procedure TAppSettings.Init;
begin
	FColorTheme := TStringSetting.Create(TDarkModeHelper.GetActualThemeName);
	FCopyToClipBoardShell := TIntegerSetting.Create(Integer(TShellType.stPowershell));
	FComboHistoryCount := TIntegerSetting.Create(MAX_HISTORY_COUNT);
	FSearchHistoryCount := TIntegerSetting.Create(MAX_HISTORY_COUNT);
	FLoadLastSearchHistory := TBoolSetting.Create(False);
	FLoadLastSearchHistoryMode := TIntegerSetting.Create();
	FDebugTrace := TStringSetting.Create('');
	FDebugTraceRegexFilter := TStringSetting.Create('');
	FExpertMode := TBoolSetting.Create(False);
	FEncodingItems := TStringSetting.Create(string.join(ARRAY_SEPARATOR, TDefaults.RG_PARAM_ENCODING_VALUES));

	CreateSetting(KEY_COLORTHEME, FColorTheme);
	CreateSetting(KEY_COPYTOCLIPBOARDSHELL, FCopyToClipBoardShell);
	CreateSetting(KEY_CMBHISTORYCOUNT, FComboHistoryCount);
	CreateSetting(KEY_LOADLASTHISTORIESONSTARTUP, FLoadLastSearchHistory);
	CreateSetting(KEY_LOADLASTHISTORIESONSTARTUP_REUSEDONLY, FLoadLastSearchHistoryMode);
	CreateSetting(KEY_DEBUGTRACE, FDebugTrace);
	CreateSetting(KEY_DEBUGTRACEREGEXFILTER, FDebugTraceRegexFilter);
	CreateSetting(KEY_ENCODING_ITEMS, FEncodingItems);
	CreateSetting(KEY_EXPERTMODE, FExpertMode);
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

procedure TAppSettings.SetExpertMode(const Value : Boolean);
begin
	FExpertMode.Value := Value;
end;

procedure TAppSettings.SetLoadLastSearchHistory(const Value : Boolean);
begin
	FLoadLastSearchHistory.Value := Value;
end;

procedure TAppSettings.SetLoadLastSearchHistoryMode(const Value : ELoadLastHistoryMode);
begin
	FLoadLastSearchHistoryMode.Value := Integer(Value);
end;

end.
