unit RipGrepper.Settings.ExtensionSettings;

interface

uses
	RipGrepper.Settings.Persistable,
	System.IniFiles,
	RipGrepper.Common.Constants,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Settings.SettingVariant, ArrayEx;

const
//	IS_GUITEST = FALSE;
	 IS_GUITEST = {$IFDEF DEBUG} TRUE; {$ELSE} FALSE; {$ENDIF}

type
	TDelphiIDEContext = record
		IDESearchContext : EDelphiIDESearchContext;
		ActiveFile : string;
		OpenFiles : TArray<string>;
		ProjectFiles : TArray<string>;
		ProjectSourcePath : TArray<string>;
		NotExistsPath : TArrayEx<string>;
		ActiveProject : string;
		function IsEmpty() : Boolean;

		public
			function ToLogString : string;
			class function FromString(const _context, _proj, _file : string) : TDelphiIDEContext; static;
			procedure LoadFromIOTA();
			class operator Initialize(out Dest : TDelphiIDEContext);
	end;

	TRipGrepperExtensionSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'DelphiExtensionSettings';
			KEY_IDE_CONTEXT = 'IDESearchContext';
			KEY_SHORTCUT_SEARCH_SELECTED = 'SearchSelectedShortcut';
			KEY_SHORTCUT_OPENWITH = 'OpenWithShortcut';
			KEY_SHORTCUT_SETTINGS = 'SettingsShortcut';
			KEY_HANDLE_OPEN_WITH_DELPHI_COMMANDS = 'HandleOpenWithDelphiCommands';

		private
			FSearchSelectedShortcut : IStringSetting;
			FCurrentIDEContext : TDelphiIDEContext;
			FIDEContext : IIntegerSetting;
			FOpenWithShortCut : IStringSetting;
			FSettingsShortCut : IStringSetting;
			FHandleOpenWithDelphiCommands : IBoolSetting;
			function GetCurrentIDEContext() : TDelphiIDEContext;
			function GetHandleOpenWithDelphiCommands() : Boolean;
			function GetOpenWithShortcut() : string;
			function GetSearchSelectedShortcut() : string;
			function GetSettingsShortcut() : string;
			procedure SetCurrentIDEContext(const Value : TDelphiIDEContext);
			procedure SetHandleOpenWithDelphiCommands(const Value : Boolean);
			procedure SetOpenWithShortcut(const Value : string);
			procedure SetSearchSelectedShortcut(const Value : string);
			procedure SetSettingsShortcut(const Value : string);

		public
			constructor Create(const _Owner : TPersistableSettings); overload;
			constructor Create; overload;
			procedure Init; override;
			function ToLogString : string; override;
			property SearchSelectedShortcut : string read GetSearchSelectedShortcut write SetSearchSelectedShortcut;
			property OpenWithShortcut : string read GetOpenWithShortcut write SetOpenWithShortcut;
			property SettingsShortcut : string read GetSettingsShortcut write SetSettingsShortcut;
			property HandleOpenWithDelphiCommands : Boolean read GetHandleOpenWithDelphiCommands write SetHandleOpenWithDelphiCommands;
			property CurrentIDEContext : TDelphiIDEContext read GetCurrentIDEContext write SetCurrentIDEContext;
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

function TRipGrepperExtensionSettings.GetCurrentIDEContext() : TDelphiIDEContext;
begin
	{$IF IS_EXTENSION}
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.GetCurrentIDEContext');

	if FCurrentIDEContext.IsEmpty then begin
		dbgMsg.Msg('CurrentIDEContext is empty. Load from IOTA...');
		FCurrentIDEContext.LoadFromIOTA();
	end;
	{$ENDIF}
	FCurrentIDEContext.IDESearchContext := EDelphiIDESearchContext(FIDEContext.Value);
	Result := FCurrentIDEContext;
end;

function TRipGrepperExtensionSettings.GetHandleOpenWithDelphiCommands() : Boolean;
begin
	Result := FHandleOpenWithDelphiCommands.Value;
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
	FSettingsShortCut := TStringSetting.Create(TDefaults.EXT_DEFAULT_SHORTCUT_SETTINGS);
	FHandleOpenWithDelphiCommands := TBoolSetting.Create(False);

	FIDEContext := TIntegerSetting.Create();
	FCurrentIDEContext.IDESearchContext := EDelphiIDESearchContext.dicCustomLocation;

	CreateSetting(KEY_SHORTCUT_SEARCH_SELECTED, FSearchSelectedShortcut);
	CreateSetting(KEY_SHORTCUT_OPENWITH, FOpenWithShortCut);
	CreateSetting(KEY_SHORTCUT_SETTINGS, FSettingsShortCut);
	CreateSetting(KEY_HANDLE_OPEN_WITH_DELPHI_COMMANDS, FHandleOpenWithDelphiCommands);
	CreateSetting(KEY_IDE_CONTEXT, FIDEContext);
end;

procedure TRipGrepperExtensionSettings.SetCurrentIDEContext(const Value : TDelphiIDEContext);
begin
	FCurrentIDEContext := Value;
	FIDEContext.Value := Integer(FCurrentIDEContext.IDESearchContext);
end;

procedure TRipGrepperExtensionSettings.SetHandleOpenWithDelphiCommands(const Value : Boolean);
begin
	FHandleOpenWithDelphiCommands.Value := Value;
end;

procedure TRipGrepperExtensionSettings.SetOpenWithShortcut(const Value : string);
begin
	FOpenWithShortcut.Value := Value;
end;

procedure TRipGrepperExtensionSettings.SetSearchSelectedShortcut(const Value : string);
begin
	FSearchSelectedShortcut.Value := Value;
end;

function TRipGrepperExtensionSettings.GetSettingsShortcut : string;
begin
	Result := FSettingsShortCut.Value;
end;

procedure TRipGrepperExtensionSettings.SetSettingsShortcut(const Value : string);
begin
	FSettingsShortCut.Value := Value;
end;

function TRipGrepperExtensionSettings.ToLogString : string;
begin
	Result := Format
		('OpenWithShortcut=%s, SearchSelectedShortcut=%s, SettingsShortcut=%s, HandleOpenWithDelphiCommands=%s, IDESearchContext=%s',
		[OpenWithShortcut, SearchSelectedShortcut, SettingsShortcut, BoolToStr(HandleOpenWithDelphiCommands, True),
		CurrentIDEContext.ToLogString]);
end;

function TDelphiIDEContext.ToLogString : string;
begin
	Result := Format('IDESearchContext: %d, ActiveProject: %s, ActiveFile: %s', [Integer(IDESearchContext), ActiveProject, ActiveFile]);
end;

class function TDelphiIDEContext.FromString(const _context, _proj, _file : string) : TDelphiIDEContext;
begin
	Result.IDESearchContext := EDelphiIDESearchContext(StrToInt(_context));
	Result.ActiveProject := _proj;
	Result.ActiveFile := _file;
end;

function TDelphiIDEContext.IsEmpty() : Boolean;
begin
	Result := ActiveProject.IsEmpty;
end;

procedure TDelphiIDEContext.LoadFromIOTA();
begin
	{$IF IS_EXTENSION}
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDelphiIDEContext.LoadFromIOTA');

	ActiveFile := IOTAUTils.GxOtaGetCurrentSourceFile();
	ProjectFiles := IOTAUTils.GetProjectFiles();
	OpenFiles := IOTAUTils.GetOpenedEditBuffers();
	var
	ap := IOTAUTils.GxOtaGetCurrentProject;
	if Assigned(ap) then begin
		ActiveProject := ap.FileName;
//      SourcePath := IOTAUtils.GxOtaGetProjectSourcePathStrings(ap, NotExistsPath);
		ProjectSourcePath := IOTAUtils.GxOtaGetEffectiveLibraryPath(ap, NotExistsPath);
	end;
	dbgMsg.Msg('ActiveFile: ' + ActiveFile);
	dbgMsg.Msg('ActiveProject: ' + ActiveProject);
	dbgMsg.Msg('OpenFiles: ' + string.Join(';', OpenFiles));
	dbgMsg.Msg('ProjectFiles: ' + string.Join(';', ProjectFiles));
	dbgMsg.Msg('SourcePath: ' + string.Join(';', ProjectSourcePath));
	dbgMsg.Msg('NotExistsPath: ' + string.Join(';', NotExistsPath.Items));

	{$ENDIF}
end;

class operator TDelphiIDEContext.Initialize(out Dest : TDelphiIDEContext);
begin
	Dest.ActiveFile := '';
	Dest.ActiveProject := '';
	Dest.OpenFiles := [];
	Dest.ProjectFiles := [];
	Dest.ProjectSourcePath := [];
	Dest.IDESearchContext := EDelphiIDESearchContext.dicNotSet;
end;

end.
