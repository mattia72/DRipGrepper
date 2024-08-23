unit RipGrepper.Common.Settings.Misc;

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
	RipGrepper.Common.Settings.RipGrepperSearchFormSettings;

type

	ERipGrepperExtensionContext = (
		{ } rgecActiveFile = EXT_SEARCH_ACTIVE_FILE,
		{ } rgecProjectFiles = EXT_SEARCH_PROJECT_FILES,
		{ } rgecOpeneFiles = EXT_SEARCH_OPEN_FILES,
		{ } rgecPath = EXT_SEARCH_GIVEN_PATH
		{ } );

	TRipGrepperExtensionContext = record
		Context : ERipGrepperExtensionContext;
		ActiveFile : string;
		OpenFiles : TArray<string>;
		ProjectFiles : TArray<string>;
		ActiveProject : string;

		public
			function ToString : string;
	end;

	TRipGrepperExtensionSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'DelphiExtensionSettings';

		private
			FDripGrepperShortCut : string;
			FCurrentSearchSettings : TRipGrepperExtensionContext;

		public
			constructor Create(const _ini : TMemIniFile);
			procedure Load; override;
			procedure Store; override;
			procedure StoreAsDefault; override;
			function ToString : string; override;
			property DripGrepperShortCut : string read FDripGrepperShortCut write FDripGrepperShortCut;
			property CurrentSearchSettings : TRipGrepperExtensionContext read FCurrentSearchSettings write FCurrentSearchSettings;
	end;

	TRipGrepperAppSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'RipGrepperSettings';

		private
			FDebugTrace : Boolean;
			FExpertMode : Boolean;
			FEncodingItems: TStringList;

		protected
			procedure Init; override;

		public
			constructor Create(const _ini : TMemIniFile);
			destructor Destroy; override;
			procedure Load; override;
			procedure Store; override;
			property DebugTrace : Boolean read FDebugTrace write FDebugTrace;
			property ExpertMode : Boolean read FExpertMode write FExpertMode;
			property EncodingItems: TStringList read FEncodingItems write FEncodingItems;
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

constructor TRipGrepperExtensionSettings.Create(const _ini : TMemIniFile);
begin
	inherited;
	IniSectionName :=  INI_SECTION;
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Create: ' + FIniFile.FileName + '[' + IniSectionName + ']');
end;

procedure TRipGrepperExtensionSettings.Load;
begin
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	if Assigned(FIniFile) then begin
		var
		css := CurrentSearchSettings;
		css.Context := ERipGrepperExtensionContext(FIniFile.ReadInteger(GetIniSectionName, 'DripGrepperContext', EXT_SEARCH_GIVEN_PATH));
		CurrentSearchSettings := css;

		DripGrepperShortCut := FIniFile.ReadString(GetIniSectionName, 'DripGrepperShortCut', '');
		if DripGrepperShortCut = '' then begin
			DripGrepperShortCut := TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH;
		end;
		FIsModified := True;
		FIsLoaded := True;
		TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Load ' + ToString());

	end else begin
		raise ESettingsException.Create('Settings ini file is nil!')
	end;
end;

procedure TRipGrepperExtensionSettings.Store;
begin
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	var
	bStore := IsLoaded and IsModified;
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Store ' + BoolToStr(bStore) + ' ' + ToString());
	if bStore then begin
		FIniFile.WriteString(GetIniSectionName, 'DripGrepperShortCut', DripGrepperShortCut);
		FIniFile.WriteInteger(GetIniSectionName, 'DripGrepperContext', Integer(CurrentSearchSettings.Context));
		FIsModified := False;
	end;

end;

procedure TRipGrepperExtensionSettings.StoreAsDefault;
begin
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	var
	bStore := IsLoaded and IsModified;
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.StoreAsDefault ' + BoolToStr(bStore) + ' ' + ToString());
	if bStore then begin
		FIniFile.WriteInteger(DEFAULTS_INI_SECTION, 'DripGrepperContext', Integer(CurrentSearchSettings.Context));
		FIsModified := False;
	end;

end;

function TRipGrepperExtensionSettings.ToString : string;
begin
	Result := Format('ShortCut: %s, CurrentSearchSettings: %s', [DripGrepperShortCut, CurrentSearchSettings.ToString]);
end;

constructor TRipGrepperAppSettings.Create(const _ini : TMemIniFile);
begin
	inherited;
    IniSectionName := INI_SECTION;
	TDebugUtils.DebugMessage('TRipGrepperAppSettings.Create: ' + FIniFile.FileName + '[' + IniSectionName + ']');
	FEncodingItems := TStringList.Create();
end;

destructor TRipGrepperAppSettings.Destroy;
begin
	FEncodingItems.Free;
	inherited;
end;

procedure TRipGrepperAppSettings.Init;
begin
	inherited;
	CreateSetting('DebugTrace', TSettingVariant.New(varBoolean, False));
	CreateSetting('ExpertMode', TSettingVariant.New(varBoolean, False));
	CreateSetting('EncodingItems', TSettingVariant.New(varString, string.join(ARRAY_SEPARATOR,
		TDefaults.RG_PARAM_ENCODING_VALUES)));
end;

procedure TRipGrepperAppSettings.Load;
begin
	inherited Load();
	TDebugUtils.DebugMessage('TRipGrepperAppSettings.Load start');

	FExpertMode := GetSetting('ExpertMode');
	FDebugTrace := GetSetting('DebugTrace');
	FEncodingItems.AddStrings(string(GetSetting('EncodingItems')).Split([ARRAY_SEPARATOR]));
end;

procedure TRipGrepperAppSettings.Store;
begin
	inherited Store();
end;

function TRipGrepperExtensionContext.ToString : string;
begin
	Result := Format('Context: %d, ActiveProject: %s, ActiveFile: %s', [Integer(Context), ActiveProject, ActiveFile]);
end;

end.
