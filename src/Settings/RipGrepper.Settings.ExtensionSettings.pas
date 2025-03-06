unit RipGrepper.Settings.ExtensionSettings;

interface

uses
	RipGrepper.Settings.Persistable,
	System.IniFiles,
	RipGrepper.Common.Constants,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Settings.SettingVariant;

type
	TRipGrepperExtensionContext = record
		IDEContext : TIntegerSetting;
		ActiveFile : string;
		OpenFiles : TArray<string>;
		ProjectFiles : TArray<string>;
		ActiveProject : string;

		public
			function ToLogString : string;
			class function FromString(const _context, _proj, _file : string) : TRipGrepperExtensionContext; static;
		class operator Initialize(out Dest: TRipGrepperExtensionContext);
	end;

	TRipGrepperExtensionSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'DelphiExtensionSettings';
			KEY_IDE_CONTEXT = 'IDEContext';
			KEY_SHORTCUT_SEARCH_SELECTED = 'SearchSelectedShortcut';
			KEY_SHORTCUT_OPENWITH = 'OpenWithShortcut';

		private
			FSearchSelectedShortcut : TStringSetting;
			FCurrentIDEContext: TRipGrepperExtensionContext;
			FOpenWithShortCut : TStringSetting;
			function GetOpenWithShortcut(): string;
			function GetSearchSelectedShortcut() : string;
			procedure LoadIdeContextFromDict();
			procedure SetOpenWithShortcut(const Value: string);
			procedure SetSearchSelectedShortcut(const Value : string);

		public
			constructor Create(const _Owner : TPersistableSettings); overload;
			constructor Create; overload;
			destructor Destroy; override;
			procedure Init; override;
			procedure ReadIni; override;
			procedure LoadFromDict(); override;
			procedure StoreToDict; override;
			function ToLogString : string; override;
			property SearchSelectedShortcut : string read GetSearchSelectedShortcut write SetSearchSelectedShortcut;
			property OpenWithShortcut: string read GetOpenWithShortcut write
				SetOpenWithShortcut;
			property CurrentIDEContext: TRipGrepperExtensionContext read FCurrentIDEContext
				write FCurrentIDEContext;
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
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Create: ' + '[' + IniSectionName + ']');
end;

constructor TRipGrepperExtensionSettings.Create;
begin
	IniSectionName := INI_SECTION;
	inherited;
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Create: ' + '[' + IniSectionName + ']');
	FSearchSelectedShortcut := TStringSetting.Create(TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH);
	FOpenWithShortCut := TStringSetting.Create(TDefaults.EXT_DEFAULT_SHORTCUT_OPEN_WITH);
 end;

destructor TRipGrepperExtensionSettings.Destroy;
begin
	inherited;
end;

function TRipGrepperExtensionSettings.GetOpenWithShortcut(): string;
begin
	Result := FOpenWithShortcut.Value;
end;

function TRipGrepperExtensionSettings.GetSearchSelectedShortcut() : string;
begin
	Result := FSearchSelectedShortcut.Value;
end;

procedure TRipGrepperExtensionSettings.Init;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.Init');
	CreateSetting(KEY_SHORTCUT_SEARCH_SELECTED, FSearchSelectedShortcut);
	CreateSetting(KEY_SHORTCUT_OPENWITH, FOpenWithShortCut);
	CreateSetting(KEY_IDE_CONTEXT, FCurrentIDEContext.IDEContext);
end;

procedure TRipGrepperExtensionSettings.ReadIni;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.ReadIni');
	{$IFNDEF TESTINSIGHT} // or ($APPTYPE = CONSOLE))} // skip if unittest
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

procedure TRipGrepperExtensionSettings.LoadFromDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.LoadFromDict');

	{$IFNDEF TESTINSIGHT} // or ($APPTYPE = CONSOLE))} // skip if unittest
	{$IFDEF STANDALONE}
	Exit;
	{$ELSE}
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	{$ENDIF}
	{$ENDIF}
	LoadIdeContextFromDict();

	SearchSelectedShortcut := TStringSetting(SettingsDict.GetSetting(KEY_SHORTCUT_SEARCH_SELECTED)).Value;
	if SearchSelectedShortcut = '' then begin
		SearchSelectedShortcut := TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH;
		FIsModified := True;
	end;
	OpenWithShortcut := TStringSetting(SettingsDict.GetSetting(KEY_SHORTCUT_OPENWITH)).Value;
	if OpenWithShortcut = '' then begin
		OpenWithShortcut := TDefaults.EXT_DEFAULT_SHORTCUT_OPEN_WITH;
		FIsModified := True;
	end;

	if IsModified then begin
		dbgMsg.Msg('Update ini file with shortcuts');
		StoreToDict();
		UpdateIniFile(IniSectionName); // create temp ini
	end;

	dbgMsg.Msg(ToLogString());
end;

procedure TRipGrepperExtensionSettings.LoadIdeContextFromDict();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.LoadIdeContextFromDict');

	var
	val := TIntegerSetting(SettingsDict.GetSetting(KEY_IDE_CONTEXT)).Value;;
	dbgMsg.MsgFmt('IDEContext %s', [VarToStrDef(val, '')]);

	var
	css := CurrentIDEContext;
	css.IDEContext.Value := val;
 	CurrentIDEContext := css;
	dbgMsg.MsgFmt('after copy IDEContext %d', [Integer(CurrentIDEContext.IDEContext)]);
end;

procedure TRipGrepperExtensionSettings.SetOpenWithShortcut(const Value: string);
begin
	FOpenWithShortcut.Value := Value;
end;

procedure TRipGrepperExtensionSettings.SetSearchSelectedShortcut(const Value : string);
begin
	FSearchSelectedShortcut.Value := Value;
end;

procedure TRipGrepperExtensionSettings.StoreToDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.StoreToDict');

	{$IFNDEF TESTINSIGHT} // or ($APPTYPE = CONSOLE))} // skip if unittest
	{$IFDEF STANDALONE}
	Exit;
	{$ELSE}
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	{$ENDIF}
	{$ENDIF}
	inherited StoreToDict; // Write to mem ini, after UpdateIniFile will be saved
end;

function TRipGrepperExtensionSettings.ToLogString : string;
begin
	Result := Format('OpenWithShortcut=%s, SearchSelectedShortcut=%s, IDEContext=%s',
		[OpenWithShortcut, SearchSelectedShortcut, CurrentIDEContext.ToLogString]);
end;

function TRipGrepperExtensionContext.ToLogString : string;
begin
	Result := Format('IDEContext: %d, ActiveProject: %s, ActiveFile: %s', [Integer(IDEContext), ActiveProject, ActiveFile]);
end;

class function TRipGrepperExtensionContext.FromString(const _context, _proj, _file : string) : TRipGrepperExtensionContext;
begin
	Result.IDEContext.Value := StrToInt(_context);
	Result.ActiveProject := _proj;
	Result.ActiveFile := _file;
end;

class operator TRipGrepperExtensionContext.Initialize(out Dest:
	TRipGrepperExtensionContext);
begin
    Dest := default(TRipGrepperExtensionContext);
	Dest.IDEContext := TIntegerSetting.Create(EXT_SEARCH_GIVEN_PATH);
end;

end.
