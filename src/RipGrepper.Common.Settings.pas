unit RipGrepper.Common.Settings;

interface

uses
	System.Classes,
	System.IniFiles,
	System.Generics.Collections,
	System.Generics.Defaults,
	RipGrepper.OpenWith.SimpleTypes,
	RipGrepper.Common.Constants,
	RipGrepper.Common.Settings.Base,
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

	TRipGrepperExtensionSettings = class(TRipGrepperSettingsBase)
		const
			INI_SECTION = 'DelphiExtensionSettings';

		private
			FDripGrepperShortCut : string;
			FCurrentSearchSettings : TRipGrepperExtensionContext;

		public
			constructor Create(const _ini : TMemIniFile);
			function GetIniSectionName : string; override;
			procedure Load; override;
			procedure Store; override;
			function ToString : string; override;
			property DripGrepperShortCut : string read FDripGrepperShortCut write FDripGrepperShortCut;
			property CurrentSearchSettings : TRipGrepperExtensionContext read FCurrentSearchSettings write FCurrentSearchSettings;
	end;

	TRipGrepperAppSettings = class(TRipGrepperSettingsBase)
		const
			INI_SECTION = 'RipGrepperSettings';

		private
			FDebugTrace : Boolean;
			FExpertMode : Boolean;
			FEncodings : TStringList;

		protected
			procedure Init; override;

		public
			constructor Create(const _ini : TMemIniFile);
			destructor Destroy; override;
			function GetIniSectionName : string; override;
			procedure Load; override;
			procedure Store; override;
			property DebugTrace : Boolean read FDebugTrace write FDebugTrace;
			property ExpertMode : Boolean read FExpertMode write FExpertMode;
			property Encodings : TStringList read FEncodings write FEncodings;
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
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Create: ' + FIniFile.FileName + '[' + GetIniSectionName + ']');
end;

function TRipGrepperExtensionSettings.GetIniSectionName : string;
begin
	Result := INI_SECTION;
end;

procedure TRipGrepperExtensionSettings.Load;
begin
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	if Assigned(FIniFile) then begin
		var
		css := CurrentSearchSettings;
		css.Context := ERipGrepperExtensionContext(FIniFile.ReadInteger(INI_SECTION, 'DripGrepperContext', EXT_SEARCH_GIVEN_PATH));
		CurrentSearchSettings := css;

		DripGrepperShortCut := FIniFile.ReadString(INI_SECTION, 'DripGrepperShortCut', '');
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
		FIniFile.WriteString(INI_SECTION, 'DripGrepperShortCut', DripGrepperShortCut);
		FIniFile.WriteInteger(INI_SECTION, 'DripGrepperContext', Integer(CurrentSearchSettings.Context));
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
	TDebugUtils.DebugMessage('TRipGrepperAppSettings.Create: ' + FIniFile.FileName + '[' + GetIniSectionName + ']');
	FEncodings := TStringList.Create();
end;

destructor TRipGrepperAppSettings.Destroy;
begin
	FEncodings.Free;
	inherited;
end;

function TRipGrepperAppSettings.GetIniSectionName : string;
begin
	Result := INI_SECTION;
end;

procedure TRipGrepperAppSettings.Init;
begin
	inherited;
	CreateSetting('DebugTrace', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('ExpertMode', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('Encodings', TRipGrepperSetting.New(vtstring, string.join(ARRAY_SEPARATOR, RG_PARAM_ENCODING_VALUES)));
end;

procedure TRipGrepperAppSettings.Load;
begin
	inherited Load();
	TDebugUtils.DebugMessage('TRipGrepperAppSettings.Load start');

	FExpertMode := LoadSetting('ExpertMode');
	FDebugTrace := LoadSetting('DebugTrace');
	FEncodings.AddStrings(string(LoadSetting('Encodings')).Split([ARRAY_SEPARATOR]));
end;

procedure TRipGrepperAppSettings.Store;
begin
	StoreSetting('ExpertMode', FExpertMode);
	StoreSetting('DebugTrace', FDebugTrace);
	StoreSetting('Encodings', string.join(ARRAY_SEPARATOR, FEncodings.ToStringArray()));
	inherited Store();
end;

function TRipGrepperExtensionContext.ToString : string;
begin
	Result := Format('Context: %d, ActiveProject: %s, ActiveFile: %s', [Integer(Context), ActiveProject, ActiveFile]);
end;

end.
