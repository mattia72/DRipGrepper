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

		private
			FColorTheme : TStringSetting;
			FCopyToClipBoardShell : TIntegerSetting;
			FDebugTrace : TStringSetting;
			FDebugTraceRegexFilter : TStringSetting;
			FExpertMode : TBoolSetting;
			FEncodingItems: TStringSetting;
			function GetColorTheme() : string;
			function GetCopyToClipBoardShell() : TShellType;
			function GetDebugTrace() : string;
			function GetDebugTraceRegexFilter(): string;
			function GetEncodingItems(): TArray<string>;
			function GetExpertMode(): Boolean;
			procedure SetColorTheme(const Value : string);
			procedure SetCopyToClipBoardShell(const Value : TShellType);
			procedure SetDebugTrace(const Value : string);
			procedure SetDebugTraceRegexFilter(const Value: string);
			procedure SetEncodingItems(const Value: TArray<string>);
			procedure SetExpertMode(const Value: Boolean);

		protected
			procedure Init; override;

		public
			constructor Create(const _Owner : TPersistableSettings);
			destructor Destroy; override;
			procedure LoadFromDict(); override;
			property ColorTheme : string read GetColorTheme write SetColorTheme;
			property CopyToClipBoardShell : TShellType read GetCopyToClipBoardShell write SetCopyToClipBoardShell;
			property DebugTrace : string read GetDebugTrace write SetDebugTrace;
			property DebugTraceRegexFilter: string read GetDebugTraceRegexFilter write
				SetDebugTraceRegexFilter;
			property ExpertMode: Boolean read GetExpertMode write SetExpertMode;
			property EncodingItems: TArray<string> read GetEncodingItems write
				SetEncodingItems;
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
	System.Variants;

constructor TAppSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := INI_SECTION;
	inherited;
	TDebugUtils.DebugMessage('TAppSettings.Create: ' +   '[' + IniSectionName + ']');
end;

destructor TAppSettings.Destroy;
begin
	FEncodingItems.Free;
	inherited Destroy() // ok;
end;

function TAppSettings.GetColorTheme() : string;
begin
	Result := FColorTheme.Value;
end;

function TAppSettings.GetCopyToClipBoardShell() : TShellType;
begin
	Result := TShellType(FCopyToClipBoardShell.Value);
end;

function TAppSettings.GetDebugTrace() : string;
begin
	Result := FDebugTrace.Value;
end;

function TAppSettings.GetDebugTraceRegexFilter(): string;
begin
	Result := FDebugTraceRegexFilter.Value;
end;

function TAppSettings.GetEncodingItems(): TArray<string>;
begin
	Result := FEncodingItems.Value.Split([ARRAY_SEPARATOR]);
end;

function TAppSettings.GetExpertMode(): Boolean;
begin
	Result := FExpertMode.Value;
end;

procedure TAppSettings.Init;
begin
	FColorTheme := TStringSetting.Create('');
	FCopyToClipBoardShell := TIntegerSetting.Create(Integer(TShellType.stPowershell));
	FDebugTrace := TStringSetting.Create('');
	FDebugTraceRegexFilter := TStringSetting.Create('');
	FExpertMode := TBoolSetting.Create(False);
    FEncodingItems := TStringSetting.Create(string.join(ARRAY_SEPARATOR , TDefaults.RG_PARAM_ENCODING_VALUES));

	CreateSetting(KEY_COLORTHEME, FColorTheme);
	CreateSetting(KEY_COPYTOCLIPBOARDSHELL, FCopyToClipBoardShell);
	CreateSetting(KEY_DEBUGTRACE, FDebugTrace);
	CreateSetting(KEY_DEBUGTRACEREGEXFILTER, FDebugTraceRegexFilter);
	CreateSetting(KEY_ENCODING_ITEMS, FEncodingItems);
	CreateSetting(KEY_EXPERTMODE, FExpertMode);
end;

procedure TAppSettings.LoadFromDict();
begin
	// TODO -cMM: TAppSettings.LoadFromDict default body inserted
end;

procedure TAppSettings.SetColorTheme(const Value : string);
begin
	FColorTheme.Value := Value;
end;

procedure TAppSettings.SetCopyToClipBoardShell(const Value : TShellType);
begin
	FCopyToClipBoardShell.Value := Integer(Value);
end;

procedure TAppSettings.SetDebugTrace(const Value : string);
begin
	FDebugTrace.Value := Value;
end;

procedure TAppSettings.SetDebugTraceRegexFilter(const Value: string);
begin
	FDebugTraceRegexFilter.Value := Value;
end;

procedure TAppSettings.SetEncodingItems(const Value: TArray<string>);
begin
	FEncodingItems.Value := string.join(ARRAY_SEPARATOR, Value);
end;

procedure TAppSettings.SetExpertMode(const Value: Boolean);
begin
	FExpertMode.Value := Value;
end;

end.
