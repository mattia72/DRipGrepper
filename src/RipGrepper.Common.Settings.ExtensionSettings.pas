unit RipGrepper.Common.Settings.ExtensionSettings;

interface

uses
	RipGrepper.Common.Settings.Persistable,
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

		public
			constructor Create(const _ini : TMemIniFile); overload;
			constructor Create; overload;
			procedure Init; override;
			procedure ReadIni; override;
			procedure LoadDefault; override;
			procedure RefreshMembers(const _bWithDefault : Boolean); override;
			procedure Store; override;
			procedure StoreAsDefault; override;
			function ToLogString : string; override;
			property DripGrepperShortCut : string read FDripGrepperShortCut write FDripGrepperShortCut;
			property OpenWithShortCut : string read FOpenWithShortCut write FOpenWithShortCut;
			property CurrentIDEContext : TRipGrepperExtensionContext read FCurrentSearchSettings write FCurrentSearchSettings;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,

	RipGrepper.Common.IOTAUtils,
	System.SysUtils,
	System.Variants;

constructor TRipGrepperExtensionSettings.Create(const _ini : TMemIniFile);
begin
	IniSectionName := INI_SECTION;
	inherited;
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Create: ' + FIniFile.FileName + '[' + IniSectionName + ']');
end;

constructor TRipGrepperExtensionSettings.Create;
begin
	IniSectionName := INI_SECTION;
	inherited;
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Create: ' + FIniFile.FileName + '[' + IniSectionName + ']');
end;

procedure TRipGrepperExtensionSettings.Init;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.Init');
	CreateSetting('DripGrepperShortCut', vtString, TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH);
	CreateSetting('OpenWithShortCut', vtString, TDefaults.EXT_DEFAULT_SHORTCUT_OPEN_WITH);
	CreateDefaultRelevantSetting(KEY_IDE_CONTEXT, vtInteger, EXT_SEARCH_GIVEN_PATH);
end;

procedure TRipGrepperExtensionSettings.ReadIni;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.ReadIni');
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	inherited ReadIni();
end;

procedure TRipGrepperExtensionSettings.LoadDefault;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.LoadDefault');
	inherited LoadDefault;
	dbgMsg.Msg('inherited LoadDefault ended')
end;

procedure TRipGrepperExtensionSettings.RefreshMembers(const _bWithDefault : Boolean);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.RefreshMembers');
	dbgMsg.MsgFmt('WithDefault %s', [BoolToStr(_bWithDefault, True)]);
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;

	var
	css := CurrentIDEContext;
	var
	val := GetSetting(KEY_IDE_CONTEXT, _bWithDefault);
	dbgMsg.MsgFmt('IDEContext %s', [VarToStrDef(val, '')]);
	css.IDEContext := ERipGrepperExtensionContext(val);
	CurrentIDEContext := css;
	dbgMsg.MsgFmt('after copy IDEContext %d', [Integer(CurrentIDEContext.IDEContext)]);

	if not _bWithDefault then begin
		DripGrepperShortCut := GetSetting(KEY_SHORTCUT_DRIPGREPPER);
		if DripGrepperShortCut = '' then begin
			DripGrepperShortCut := TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH;
		end;
	end;

	if not _bWithDefault then begin
		OpenWithShortCut := GetSetting(KEY_SHORTCUT_OPENWITH);
		if OpenWithShortCut = '' then begin
			OpenWithShortCut := TDefaults.EXT_DEFAULT_SHORTCUT_OPEN_WITH;
		end;
	end;

	dbgMsg.Msg(ToLogString());
end;

procedure TRipGrepperExtensionSettings.Store;
begin
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;

	StoreSetting(KEY_SHORTCUT_DRIPGREPPER, DripGrepperShortCut);
	StoreSetting(KEY_SHORTCUT_OPENWITH, OpenWithShortCut);
	StoreSetting(KEY_IDE_CONTEXT, Integer(CurrentIDEContext.IDEContext));
	inherited Store; // Write to mem ini, after UpdateIniFile will be saved
end;

procedure TRipGrepperExtensionSettings.StoreAsDefault;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.StoreAsDefault');

	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	dbgMsg.MsgFmt('TAppSettings.StoreAsDefault: IDEContext=%d',
		{ } [Integer(CurrentIDEContext.IDEContext)]);

	StoreDefaultSetting(KEY_IDE_CONTEXT, Integer(CurrentIDEContext.IDEContext));
	inherited StoreAsDefault;
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
