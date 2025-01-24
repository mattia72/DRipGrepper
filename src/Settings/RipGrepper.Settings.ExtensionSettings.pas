unit RipGrepper.Settings.ExtensionSettings;

interface

uses
	RipGrepper.Settings.Persistable,
	System.IniFiles,
	RipGrepper.Common.Constants;

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
			function ToLogString : string;
			class function FromString(const _context, _proj, _file : string) : TRipGrepperExtensionContext; static;
	end;

	TRipGrepperExtensionSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'DelphiExtensionSettings';
			KEY_IDE_CONTEXT = 'IDEContext';
			KEY_SHORTCUT_SEARCH_SELECTED = 'SearchSelectedShortCut';
			KEY_SHORTCUT_OPENWITH = 'OpenWithShortCut';

		private
			FSearchSelectedShortcut : string;
			FCurrentSearchSettings : TRipGrepperExtensionContext;
			FOpenWithShortCut : string;
			procedure LoadIdeContextFromDict(const _bDefault : Boolean);

		public
			constructor Create(const _Owner : TPersistableSettings); overload;
			constructor Create; overload;
			destructor Destroy; override;
			procedure Init; override;
			procedure ReadIni; override;
			procedure LoadDefaultsFromDict; override;
			procedure LoadFromDict(); override;
			procedure StoreToDict; override;
			procedure StoreAsDefaultsToDict; override;
			function ToLogString : string; override;
			property SearchSelectedShortcut : string read FSearchSelectedShortcut write FSearchSelectedShortcut;
			property OpenWithShortCut : string read FOpenWithShortCut write FOpenWithShortCut;
			property CurrentIDEContext : TRipGrepperExtensionContext read FCurrentSearchSettings write FCurrentSearchSettings;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	{$IFNDEF STANDALONE} RipGrepper.Common.IOTAUtils, {$ENDIF}
	System.SysUtils,
	System.Variants;

constructor TRipGrepperExtensionSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := INI_SECTION;
	inherited Create(_Owner);
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Create: ' + IniFile.FileName + '[' + IniSectionName + ']');
end;

constructor TRipGrepperExtensionSettings.Create;
begin
	IniSectionName := INI_SECTION;
	inherited;
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Create: ' + IniFile.FileName + '[' + IniSectionName + ']');
end;

destructor TRipGrepperExtensionSettings.Destroy;
begin
	inherited;
end;

procedure TRipGrepperExtensionSettings.Init;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.Init');
	SettingsDict.CreateSetting('SearchSelectedShortcut', varString, TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH);
	SettingsDict.CreateSetting('OpenWithShortCut', varString, TDefaults.EXT_DEFAULT_SHORTCUT_OPEN_WITH);
	SettingsDict.CreateDefaultRelevantSetting(KEY_IDE_CONTEXT, varInteger, EXT_SEARCH_GIVEN_PATH);
end;

procedure TRipGrepperExtensionSettings.ReadIni;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.ReadIni');
	{$IFNDEF TESTINSIGHT} //or ($APPTYPE = CONSOLE))} // skip if unittest
 	{$IFDEF STANDALONE}
	Exit;
	{$ELSE}
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	{$ENDIF}
	{$ENDIF}
	inherited ReadIni();
end;

procedure TRipGrepperExtensionSettings.LoadDefaultsFromDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.LoadDefaultsFromDict');
	LoadIdeContextFromDict(True);
end;

procedure TRipGrepperExtensionSettings.LoadFromDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.LoadFromDict');

	{$IFNDEF TESTINSIGHT} //or ($APPTYPE = CONSOLE))} // skip if unittest
	{$IFDEF STANDALONE}
	Exit;
	{$ELSE}
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	{$ENDIF}
	{$ENDIF}

	LoadIdeContextFromDict(False);

	SearchSelectedShortcut := SettingsDict.GetSetting(KEY_SHORTCUT_SEARCH_SELECTED);
	if SearchSelectedShortcut = '' then begin
		SearchSelectedShortcut := TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH;
		FIsModified := True;
	end;
	OpenWithShortCut := SettingsDict.GetSetting(KEY_SHORTCUT_OPENWITH);
	if OpenWithShortCut = '' then begin
		OpenWithShortCut := TDefaults.EXT_DEFAULT_SHORTCUT_OPEN_WITH;
		FIsModified := True;
	end;

	if IsModified then begin
		dbgMsg.Msg('Update ini file with shortcuts');
		StoreToDict();
		UpdateIniFile(IniSectionName); // create temp ini
	end;

	dbgMsg.Msg(ToLogString());
end;

procedure TRipGrepperExtensionSettings.LoadIdeContextFromDict(const _bDefault : Boolean);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.LoadIdeContextFromDict Default=' + BoolToStr(_bDefault));

	var
	val := SettingsDict.GetSetting(KEY_IDE_CONTEXT, _bDefault);
	dbgMsg.MsgFmt('IDEContext %s', [VarToStrDef(val, '')]);

	var
	css := CurrentIDEContext;
	css.IDEContext := ERipGrepperExtensionContext(val);
	CurrentIDEContext := css;
	dbgMsg.MsgFmt('after copy IDEContext %d', [Integer(CurrentIDEContext.IDEContext)]);
end;

procedure TRipGrepperExtensionSettings.StoreToDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.StoreToDict');

	{$IFNDEF TESTINSIGHT} //or ($APPTYPE = CONSOLE))} // skip if unittest
	{$IFDEF STANDALONE}
	Exit;
	{$ELSE}
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	{$ENDIF}
	{$ENDIF}
	SettingsDict.StoreSetting(KEY_SHORTCUT_SEARCH_SELECTED, SearchSelectedShortcut);
	SettingsDict.StoreSetting(KEY_SHORTCUT_OPENWITH, OpenWithShortCut);
	SettingsDict.StoreSetting(KEY_IDE_CONTEXT, Integer(CurrentIDEContext.IDEContext));
	inherited StoreToDict; // Write to mem ini, after UpdateIniFile will be saved
end;

procedure TRipGrepperExtensionSettings.StoreAsDefaultsToDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.StoreAsDefaultsToDict');

	{$IFNDEF TESTINSIGHT} //or ($APPTYPE = CONSOLE))} // skip if unittest
	{$IFDEF STANDALONE}
	Exit;
	{$ELSE}
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	{$ENDIF}
	{$ENDIF}
	dbgMsg.MsgFmt('TAppSettings.StoreAsDefaultsToDict: IDEContext=%d',
		{ } [Integer(CurrentIDEContext.IDEContext)]);

	SettingsDict.StoreDefaultSetting(KEY_IDE_CONTEXT, Integer(CurrentIDEContext.IDEContext));
	inherited StoreAsDefaultsToDict;
end;

function TRipGrepperExtensionSettings.ToLogString : string;
begin
	Result := Format('OpenWithShortCut=%s, SearchSelectedShortCut=%s, IDEContext=%s', [OpenWithShortCut, SearchSelectedShortcut,
		CurrentIDEContext.ToLogString]);
end;

function TRipGrepperExtensionContext.ToLogString : string;
begin
	Result := Format('IDEContext: %d, ActiveProject: %s, ActiveFile: %s', [Integer(IDEContext), ActiveProject, ActiveFile]);
end;

class function TRipGrepperExtensionContext.FromString(const _context, _proj, _file : string) : TRipGrepperExtensionContext;
begin
	Result.IDEContext := ERipGrepperExtensionContext(StrToInt(_context));
	Result.ActiveProject := _proj;
	Result.ActiveFile := _file;
end;

end.
