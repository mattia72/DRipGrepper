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
	RipGrepper.Settings.RipGrepParameterSettings;

type

	TAppSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'RipGrepperSettings';

		private
			FDebugTrace : string;
			FDebugTraceRegexFilter: string;
			FExpertMode : Boolean;
			FEncodingItems : TStringList;

		protected
			procedure Init; override;

		public
			constructor Create(const _Owner : TPersistableSettings);
			destructor Destroy; override;
			procedure LoadFromDict(); override;
			procedure LoadDefaultsFromDict; override;
			procedure StoreToDict; override;
			property DebugTrace : string read FDebugTrace write FDebugTrace;
			property DebugTraceRegexFilter: string read FDebugTraceRegexFilter write
				FDebugTraceRegexFilter;
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
	SettingsDict.CreateSetting('DebugTrace', varString, 'tftError');
	SettingsDict.CreateSetting('DebugTraceRegexFilter', varString, '');
	SettingsDict.CreateSetting('ExpertMode', varBoolean, False);
	SettingsDict.CreateSetting('EncodingItems', varString, string.join(ARRAY_SEPARATOR, TDefaults.RG_PARAM_ENCODING_VALUES));
end;

procedure TAppSettings.LoadFromDict;
begin
	FDebugTrace := SettingsDict.GetSetting('DebugTrace');
	FDebugTraceRegexFilter := SettingsDict.GetSetting('DebugTraceRegexFilter');
	FExpertMode := SettingsDict.GetSetting('ExpertMode');
	FEncodingItems.Clear;
	FEncodingItems.AddStrings(string(SettingsDict.GetSetting('EncodingItems')).Split([ARRAY_SEPARATOR]));
end;

procedure TAppSettings.LoadDefaultsFromDict;
begin
	// nothing to do
end;

procedure TAppSettings.StoreToDict;
begin
	SettingsDict.StoreSetting('DebugTrace', FDebugTrace);
	SettingsDict.StoreSetting('DebugTraceRegexFilter', FDebugTraceRegexFilter);
	SettingsDict.StoreSetting('ExpertMode', FExpertMode);
	inherited StoreToDict();
end;

end.
