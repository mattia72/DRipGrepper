unit RipGrepper.Settings.ExtensionSettings;

interface

uses
	RipGrepper.Settings.Persistable,
	System.IniFiles,
	RipGrepper.Common.Constants,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Settings.SettingVariant;


const GUITEST = {$IFDEF DEBUG} TRUE; {$ELSE} FALSE; {$ENDIF}

type
	TRipGrepperExtensionContext = record
		IDEContext : integer;
		ActiveFile : string;
		OpenFiles : TArray<string>;
		ProjectFiles : TArray<string>;
		ActiveProject : string;

		public
			function ToLogString : string;
			class function FromString(const _context, _proj, _file : string) : TRipGrepperExtensionContext; static;
			class operator Initialize(out Dest : TRipGrepperExtensionContext);
	end;

	TRipGrepperExtensionSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'DelphiExtensionSettings';
			KEY_IDE_CONTEXT = 'IDEContext';
			KEY_SHORTCUT_SEARCH_SELECTED = 'SearchSelectedShortcut';
			KEY_SHORTCUT_OPENWITH = 'OpenWithShortcut';

		private
			FSearchSelectedShortcut : IStringSetting;
			FCurrentIDEContext : TRipGrepperExtensionContext;
			FIDEContext : IIntegerSetting;
			FOpenWithShortCut : IStringSetting;
			function GetCurrentIDEContext() : TRipGrepperExtensionContext;
			function GetOpenWithShortcut() : string;
			function GetSearchSelectedShortcut() : string;
			procedure SetCurrentIDEContext(const Value : TRipGrepperExtensionContext);
			procedure SetOpenWithShortcut(const Value : string);
			procedure SetSearchSelectedShortcut(const Value : string);

		public
			constructor Create(const _Owner : TPersistableSettings); overload;
			constructor Create; overload;
			procedure Init; override;
			procedure ReadFile(); override;
			procedure StoreToPersister; override;
			function ToLogString : string; override;
			property SearchSelectedShortcut : string read GetSearchSelectedShortcut write SetSearchSelectedShortcut;
			property OpenWithShortcut : string read GetOpenWithShortcut write SetOpenWithShortcut;
			property CurrentIDEContext : TRipGrepperExtensionContext read GetCurrentIDEContext write SetCurrentIDEContext;
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
end;

function TRipGrepperExtensionSettings.GetCurrentIDEContext() : TRipGrepperExtensionContext;
begin
	FCurrentIDEContext.IDEContext := FIDEContext.Value;
	Result := FCurrentIDEContext;
end;

function TRipGrepperExtensionSettings.GetOpenWithShortcut() : string;
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

	FSearchSelectedShortcut := TStringSetting.Create(TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH);
	FOpenWithShortCut := TStringSetting.Create(TDefaults.EXT_DEFAULT_SHORTCUT_OPEN_WITH);

	FIDEContext := TIntegerSetting.Create();
    FCurrentIDEContext.IDEContext := EXT_SEARCH_GIVEN_PATH;

	CreateSetting(KEY_SHORTCUT_SEARCH_SELECTED, FSearchSelectedShortcut);
	CreateSetting(KEY_SHORTCUT_OPENWITH, FOpenWithShortCut);
	CreateSetting(KEY_IDE_CONTEXT, FIDEContext);
end;

procedure TRipGrepperExtensionSettings.ReadFile();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.ReadFile');
	{$IFNDEF TESTINSIGHT} // or ($APPTYPE = CONSOLE))} // skip if unittest
	{$IF NOT GUITEST AND DEFINED(STANDALONE)}
	Exit;
	{$ENDIF}
	{$ENDIF}
	inherited ReadFile();
end;

procedure TRipGrepperExtensionSettings.SetCurrentIDEContext(const Value : TRipGrepperExtensionContext);
begin
	FCurrentIDEContext := Value;
    FIDEContext.Value := FCurrentIDEContext.IDEContext;
end;

procedure TRipGrepperExtensionSettings.SetOpenWithShortcut(const Value : string);
begin
	FOpenWithShortcut.Value := Value;
end;

procedure TRipGrepperExtensionSettings.SetSearchSelectedShortcut(const Value : string);
begin
	FSearchSelectedShortcut.Value := Value;
end;

procedure TRipGrepperExtensionSettings.StoreToPersister;  // switch of if TESTINSIGHT
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.StoreToPersister');

	{$IFNDEF TESTINSIGHT} // or ($APPTYPE = CONSOLE))} // skip if unittest
	{$IFDEF STANDALONE}
	//Exit;
	{$ELSE}
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	{$ENDIF}
	{$ENDIF}
	inherited StoreToPersister; // Write to mem ini, after UpdateIniFile will be saved
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
	Result.IDEContext := StrToInt(_context);
	Result.ActiveProject := _proj;
	Result.ActiveFile := _file;
end;

class operator TRipGrepperExtensionContext.Initialize(out Dest : TRipGrepperExtensionContext);
begin
	Dest.ActiveFile := '';
	Dest.ActiveProject := '';
	Dest.OpenFiles := [];
	Dest.ProjectFiles := [];
	Dest.IDEContext := Integer(ERipGrepperExtensionContext.rgecNotSet);
end;

end.
