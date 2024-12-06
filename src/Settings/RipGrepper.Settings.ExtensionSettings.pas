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
	end;

	TRipGrepperExtensionSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'DelphiExtensionSettings';
			KEY_IDE_CONTEXT = 'IDEContext';
			KEY_SHORTCUT_DRIPGREPPER = 'DripGrepperShortCut';
			KEY_SHORTCUT_OPENWITH = 'OpenWithShortCut';

		private
			FDripGrepperShortCut : string;
			FCurrentSearchSettings : TRipGrepperExtensionContext;
			FOpenWithShortCut : string;
			procedure LoadIdeContextFromDict(const _bDefault : Boolean);

		public
			constructor Create(const _ini : TMemIniFile); overload;
			constructor Create; overload;
			procedure Init; override;
			procedure ReadIni; override;
			procedure LoadDefaultsFromDict; override;
			procedure LoadFromDict(); override;
			procedure StoreToDict; override;
			procedure StoreAsDefaultsToDict; override;
			function ToLogString : string; override;
			property DripGrepperShortCut : string read FDripGrepperShortCut write FDripGrepperShortCut;
			property OpenWithShortCut : string read FOpenWithShortCut write FOpenWithShortCut;
			property CurrentIDEContext : TRipGrepperExtensionContext read FCurrentSearchSettings write FCurrentSearchSettings;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	{$IFNDEF STANDALONE} RipGrepper.Common.IOTAUtils, {$ENDIF}
	System.SysUtils,
	System.Variants;

constructor TRipGrepperExtensionSettings.Create(const _ini : TMemIniFile);
begin
	IniSectionName := INI_SECTION;
	inherited;
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Create: ' + IniFile.FileName + '[' + IniSectionName + ']');
end;

constructor TRipGrepperExtensionSettings.Create;
begin
	IniSectionName := INI_SECTION;
	inherited;
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Create: ' + IniFile.FileName + '[' + IniSectionName + ']');
end;

procedure TRipGrepperExtensionSettings.Init;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.Init');
	SettingsDict.CreateSetting('DripGrepperShortCut', vtString, TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH);
	SettingsDict.CreateSetting('OpenWithShortCut', vtString, TDefaults.EXT_DEFAULT_SHORTCUT_OPEN_WITH);
	SettingsDict.CreateDefaultRelevantSetting(KEY_IDE_CONTEXT, vtInteger, EXT_SEARCH_GIVEN_PATH);
end;

procedure TRipGrepperExtensionSettings.ReadIni;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.ReadIni');
	{$IFDEF STANDALONE}
	Exit;
	{$ELSE}
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
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
	{$IFDEF STANDALONE}
	Exit;
	{$ELSE}
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	{$ENDIF}
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.LoadFromDict');

	LoadIdeContextFromDict(False);

	DripGrepperShortCut := SettingsDict.GetSetting(KEY_SHORTCUT_DRIPGREPPER);
	if DripGrepperShortCut = '' then begin
		DripGrepperShortCut := TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH;
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
		UpdateIniFile;
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

	{$IFDEF STANDALONE}
	Exit;
	{$ELSE}
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	{$ENDIF}
	SettingsDict.StoreSetting(KEY_SHORTCUT_DRIPGREPPER, DripGrepperShortCut);
	SettingsDict.StoreSetting(KEY_SHORTCUT_OPENWITH, OpenWithShortCut);
	SettingsDict.StoreSetting(KEY_IDE_CONTEXT, Integer(CurrentIDEContext.IDEContext));
	inherited StoreToDict; // Write to mem ini, after UpdateIniFile will be saved
end;

procedure TRipGrepperExtensionSettings.StoreAsDefaultsToDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.StoreAsDefaultsToDict');

	{$IFDEF STANDALONE}
	Exit;
	{$ELSE}
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	{$ENDIF}
	dbgMsg.MsgFmt('TAppSettings.StoreAsDefaultsToDict: IDEContext=%d',
		{ } [Integer(CurrentIDEContext.IDEContext)]);

	SettingsDict.StoreDefaultSetting(KEY_IDE_CONTEXT, Integer(CurrentIDEContext.IDEContext));
	inherited StoreAsDefaultsToDict;
end;

function TRipGrepperExtensionSettings.ToLogString : string;
begin
	Result := Format('OpenWithShortCut=%s, ShortCut=%s, CurrentIDEContext=[%s]',
		[OpenWithShortCut, DripGrepperShortCut, CurrentIDEContext.ToLogString]);
end;

function TRipGrepperExtensionContext.ToLogString : string;
begin
	Result := Format('IDEContext: %d, ActiveProject: %s, ActiveFile: %s', [Integer(IDEContext), ActiveProject, ActiveFile]);
end;

end.
