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
	RipGrepper.Settings.RipGrepParameterSettings;

type

	TAppSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'RipGrepperSettings';

		private
			FDebugTrace : Boolean;
			FExpertMode : Boolean;
			FEncodingItems : TStringList;

		protected
			procedure Init; override;

		public
			constructor Create(const _ini : TMemIniFile);
			destructor Destroy; override;
			procedure LoadFromDict(); override;
			procedure LoadDefaultsFromDict; override;
			procedure StoreToDict; override;
			property DebugTrace : Boolean read FDebugTrace write FDebugTrace;
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
	RipGrepper.Common.IOTAUtils,
	System.Variants;

constructor TAppSettings.Create(const _ini : TMemIniFile);
begin
	IniSectionName := INI_SECTION;
	inherited;
	TDebugUtils.DebugMessage('TAppSettings.Create: ' + FIniFile.FileName + '[' + IniSectionName + ']');
	FEncodingItems := TStringList.Create();
end;

destructor TAppSettings.Destroy;
begin
	FEncodingItems.Free;
	inherited Destroy() //ok;
end;

procedure TAppSettings.Init;
begin
	SettingsDict.CreateSetting('DebugTrace', varBoolean, False);
	SettingsDict.CreateSetting('ExpertMode', varBoolean, False);
	SettingsDict.CreateSetting('EncodingItems', varString, string.join(ARRAY_SEPARATOR, TDefaults.RG_PARAM_ENCODING_VALUES));
end;

procedure TAppSettings.LoadFromDict;
begin
	FExpertMode := SettingsDict.GetSetting('ExpertMode');
	FDebugTrace := SettingsDict.GetSetting('DebugTrace');
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
	SettingsDict.StoreSetting('ExpertMode', FExpertMode);
	inherited StoreToDict();
end;

end.
