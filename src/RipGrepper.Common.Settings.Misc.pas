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
	RipGrepper.Common.Settings.RipGrepParameterSettings;

type

	ERipGrepperExtensionContext = (
		{ } rgecActiveFile = EXT_SEARCH_ACTIVE_FILE,
		{ } rgecProjectFiles = EXT_SEARCH_PROJECT_FILES,
		{ } rgecOpeneFiles = EXT_SEARCH_OPEN_FILES,
		{ } rgecPath = EXT_SEARCH_GIVEN_PATH
		{ } );

	TRipGrepperExtensionContext = record
		IDEContext : ERipGrepperExtensionContext;
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
			KEY_CONTEXT = 'IDEContext';
			KEY_SHORTCUT = 'DripGrepperShortCut';

		private
			FDripGrepperShortCut : string;
			FCurrentSearchSettings : TRipGrepperExtensionContext;

		public
			constructor Create(const _ini : TMemIniFile); overload;
			constructor Create; overload;
			procedure Init; override;
			procedure ReadIni; override;
			procedure LoadDefault; override;
			procedure RefreshMembers(const _bWithDefault : Boolean); override;
			procedure Store; override;
			procedure StoreAsDefault; override;
			function ToString : string; override;
			property DripGrepperShortCut : string read FDripGrepperShortCut write FDripGrepperShortCut;
			property CurrentContext : TRipGrepperExtensionContext read FCurrentSearchSettings write FCurrentSearchSettings;
	end;

	TRipGrepperAppSettings = class(TPersistableSettings)
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
			procedure RefreshMembers(const _bWithDefault : Boolean); override;
			procedure Store; override;
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
	RipGrepper.Common.IOTAUtils;

constructor TRipGrepperExtensionSettings.Create(const _ini : TMemIniFile);
begin
	inherited;
	IniSectionName := INI_SECTION;
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Create: ' + FIniFile.FileName + '[' + IniSectionName + ']');
end;

constructor TRipGrepperExtensionSettings.Create;
begin
	inherited;
	IniSectionName := INI_SECTION;
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Create: ' + FIniFile.FileName + '[' + IniSectionName + ']');
end;

procedure TRipGrepperExtensionSettings.Init;
begin
	CreateSetting('DripGrepperShortCut', vtString, TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH);
	CreateDefaultSetting(KEY_CONTEXT, vtInteger, EXT_SEARCH_GIVEN_PATH);
end;

procedure TRipGrepperExtensionSettings.ReadIni;
begin
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	inherited ReadIni();
end;

procedure TRipGrepperExtensionSettings.LoadDefault;
begin
	inherited LoadDefault;
end;

procedure TRipGrepperExtensionSettings.RefreshMembers(const _bWithDefault : Boolean);
begin
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;

	var
	css := CurrentContext;
	css.IDEContext := ERipGrepperExtensionContext(GetSetting(KEY_CONTEXT, _bWithDefault));
	CurrentContext := css;

	if not _bWithDefault then begin
		DripGrepperShortCut := GetSetting(KEY_SHORTCUT);
		if DripGrepperShortCut = '' then begin
			DripGrepperShortCut := TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH;
		end;
	end;

	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.RefreshMembers ' + ToString());
end;

procedure TRipGrepperExtensionSettings.Store;
begin
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;

	StoreSetting(KEY_SHORTCUT, DripGrepperShortCut);
	StoreSetting(KEY_CONTEXT, Integer(CurrentContext.IDEContext));
	inherited Store; // Write to mem ini, after UpdateIniFile will be saved
end;

procedure TRipGrepperExtensionSettings.StoreAsDefault;
begin
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	TDebugUtils.DebugMessageFormat('TRipGrepperAppSettings.StoreAsDefault: Context=%d',
		{ } [Integer(CurrentContext.IDEContext)]);

	StoreDefaultSetting(KEY_CONTEXT, Integer(CurrentContext.IDEContext));
	inherited StoreAsDefault;
end;

function TRipGrepperExtensionSettings.ToString : string;
begin
	Result := Format('ShortCut: %s, CurrentContext: %s', [DripGrepperShortCut, CurrentContext.ToString]);
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
	CreateSetting('DebugTrace', varBoolean, False);
	CreateSetting('ExpertMode', varBoolean, False);
	CreateSetting('EncodingItems', varString, string.join(ARRAY_SEPARATOR, TDefaults.RG_PARAM_ENCODING_VALUES));
end;

procedure TRipGrepperAppSettings.RefreshMembers(const _bWithDefault : Boolean);
begin
	if _bWithDefault then
		Exit;
	FExpertMode := GetSetting('ExpertMode');
	FDebugTrace := GetSetting('DebugTrace');
	FEncodingItems.Clear;
	FEncodingItems.AddStrings(string(GetSetting('EncodingItems')).Split([ARRAY_SEPARATOR]));
end;

procedure TRipGrepperAppSettings.Store;
begin
	StoreSetting('DebugTrace', FDebugTrace);
	StoreSetting('ExpertMode', FExpertMode);
	inherited Store();
end;

function TRipGrepperExtensionContext.ToString : string;
begin
	Result := Format('IDEContext: %d, ActiveProject: %s, ActiveFile: %s', [Integer(IDEContext), ActiveProject, ActiveFile]);
end;

end.
