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
    RipGrepper.Common.SimpleTypes;

type

	TAppSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'RipGrepperSettings';
			KEY_ENCODING_ITEMS = 'EncodingItems';
			KEY_COLORTHEME = 'ColorTheme';
			KEY_EXPERTMODE = 'ExpertMode';
			KEY_DEBUGTRACEREGEXFILTER = 'DebugTraceRegexFilter';
			KEY_DEBUGTRACE = 'DebugTrace';
			KEY_COPYTOCLIPBOARDSHELL = 'CopyToClipBoardShell';

		private
			FColorTheme : string;
			FDebugTrace : string;
			FCopyToClipBoardShell : TShellType;
			FDebugTraceRegexFilter : string;
			FExpertMode : Boolean;
			FEncodingItems : TStringList;

		protected
			procedure Init; override;

		public
			constructor Create(const _Owner : TPersistableSettings);
			destructor Destroy; override;
			procedure LoadFromDict(); override;
			procedure StoreToDict; override;
			property ColorTheme : string read FColorTheme write FColorTheme;
			property CopyToClipBoardShell : TShellType read FCopyToClipBoardShell write FCopyToClipBoardShell;
			property DebugTrace : string read FDebugTrace write FDebugTrace;
			property DebugTraceRegexFilter : string read FDebugTraceRegexFilter write FDebugTraceRegexFilter;
			property ExpertMode : Boolean read FExpertMode write FExpertMode;
			property EncodingItems : TStringList read FEncodingItems write FEncodingItems;
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
	TDebugUtils.DebugMessage('TAppSettings.Create: ' + IniFile.FileName + '[' + IniSectionName + ']');
	FEncodingItems := TStringList.Create();
end;

destructor TAppSettings.Destroy;
begin
	FEncodingItems.Free;
	inherited Destroy() // ok;
end;

procedure TAppSettings.Init;
begin
	SettingsDict.CreateSetting(KEY_DEBUGTRACE, varString, 'tftError');
	SettingsDict.CreateSetting(KEY_DEBUGTRACEREGEXFILTER, varString, '');
	SettingsDict.CreateSetting(KEY_EXPERTMODE, varBoolean, False);
	SettingsDict.CreateSetting(KEY_COLORTHEME, varString, '');
	SettingsDict.CreateSetting(KEY_ENCODING_ITEMS, varString, string.join(ARRAY_SEPARATOR, TDefaults.RG_PARAM_ENCODING_VALUES));
	SettingsDict.CreateSetting(KEY_COPYTOCLIPBOARDSHELL, varInteger, Integer(TShellType.stPowershell));
end;

procedure TAppSettings.LoadFromDict;
begin
	FDebugTrace := SettingsDict.GetSetting(KEY_DEBUGTRACE);
	FDebugTraceRegexFilter := SettingsDict.GetSetting(KEY_DEBUGTRACEREGEXFILTER);
	FExpertMode := SettingsDict.GetSetting(KEY_EXPERTMODE);
	FColorTheme := SettingsDict.GetSetting(KEY_COLORTHEME);
	FEncodingItems.Clear;
	FEncodingItems.AddStrings(string(SettingsDict.GetSetting(KEY_ENCODING_ITEMS)).Split([ARRAY_SEPARATOR]));

	FCopyToClipBoardShell := TShellType(SettingsDict.GetSetting(KEY_COPYTOCLIPBOARDSHELL));
end;

procedure TAppSettings.StoreToDict;
begin
	SettingsDict.StoreSetting(KEY_DEBUGTRACE, FDebugTrace);
	SettingsDict.StoreSetting(KEY_DEBUGTRACEREGEXFILTER, FDebugTraceRegexFilter);
	SettingsDict.StoreSetting(KEY_EXPERTMODE, FExpertMode);
	SettingsDict.StoreSetting(KEY_COLORTHEME, FColorTheme);
	SettingsDict.StoreSetting(KEY_COPYTOCLIPBOARDSHELL, Integer(FCopyToClipBoardShell));
	inherited StoreToDict();
end;

end.
