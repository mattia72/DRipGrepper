unit RipGrepper.Settings.OpenWithSettings;

interface

uses
	RipGrepper.Settings.Persistable,
	System.Classes,
	System.IniFiles,
	RipGrepper.OpenWith.Constants,
	RipGrepper.OpenWith.Params,
	RipGrepper.Settings.SettingVariant,
	RipGrepper.Settings.Persister.Interfaces,
	RipGrepper.Tools.FileUtils,
	ArrayEx;

type
	TOpenWithSettings = class(TPersistableSettings)
		private
			FCommandListSetting : IArraySetting;
			FTestFile : TOpenWithParams;
			function GetCommand(Index : Integer) : string;
			function GetCommandListSetting(): IArraySetting;
			function GetDefaultEditorsWithVSCodeDetection() : TArray<string>;
			procedure SetDefaultsIfEmpty();
			procedure SetCommand(Index : Integer; const Value : string);

		public
			constructor Create(const _Owner : TPersistableSettings);
			procedure ClearCommandList;
			function GetCommands() : TArray<string>;
			function GetCommandItems() : TArray<TCommandItem>;
			procedure Init; override;
			procedure ReadFile(); override;
			procedure ForceUpdateFile();
			procedure LoadFromDict(); override;
			procedure RecreateCommandList(_cmdListItems : TArrayEx<string>); overload;
			procedure RecreateCommandList(_cmdItems : TArray<TCommandItem>); overload;
			procedure LoadCommandsFromJSON();
			procedure SaveCommandsToJSON();
			function ToString : string; override;
			property Command[index : Integer] : string read GetCommand write SetCommand;
			property CommandListSetting: IArraySetting read GetCommandListSetting;
			property TestFile : TOpenWithParams read FTestFile write FTestFile;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.SysUtils,
	RipGrepper.Settings.SettingsDictionary,
	RipGrepper.Common.Constants,
	System.JSON;

constructor TOpenWithSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := OPEN_WITH_SETTINGS;
	inherited Create(_Owner);
	TDebugUtils.DebugMessage('TOpenWithSettings.Create: ' + '[' + IniSectionName + ']');
end;

procedure TOpenWithSettings.ClearCommandList;
begin
	FCommandListSetting.Value.Clear;
	FSettingsDict.InnerDictionary.Clear;
	FIsModified := True;
end;

function TOpenWithSettings.GetCommands() : TArray<string>;
begin
	Result := CommandListSetting.Value;
end;

function TOpenWithSettings.GetCommand(Index : Integer) : string;
begin
	Result := '';
	if TArraySetting(CommandListSetting).Count > index then begin
		Result := TArraySetting(FCommandListSetting)[index];
	end;
end;

procedure TOpenWithSettings.Init;
begin
	FCommandListSetting := TArraySetting.Create(OPEN_WITH_SETTINGS);
//  FCommandListSetting.SaveBehaviour := [ssbStoreIfModified, ssbStoreOnceEvenIfNotModified];
	CreateSetting(OPEN_WITH_SETTINGS, ITEM_KEY_PREFIX, FCommandListSetting);
end;

procedure TOpenWithSettings.ReadFile();
var
	iarr : IArraySetting;
	arr : TArray<string>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithSettings.ReadFile');

	iarr := TArraySetting.Create('OpenWithCommands', arr);
	iarr.Copy(FCommandListSetting);
	iarr.LoadFromPersister();
	if not iarr.Value.IsEmpty then begin
		FCommandListSetting.Copy(iarr);
	end;

end;

procedure TOpenWithSettings.SetCommand(Index : Integer; const Value : string);
var
	arrCmds : TArrayEx<string>;
begin
	if Value.IsEmpty then
		Exit;

	if FCommandListSetting.Value.Count > index then begin
		if (FCommandListSetting.Value[index] <> Value) then begin
			FCommandListSetting.Value[index] := Value;
			FIsModified := True;
		end;
	end else begin
		arrCmds := FCommandListSetting.Value;
		arrCmds.Add(Value);
		FCommandListSetting.Value := arrCmds;
		FIsModified := True;
	end;
end;

procedure TOpenWithSettings.ForceUpdateFile();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithSettings.ForceUpdateFile');

	GetRootOwner().UpdateFile(True, True);
end;

function TOpenWithSettings.GetCommandListSetting(): IArraySetting;
begin
	SetDefaultsIfEmpty();
	Result := FCommandListSetting;
end;

procedure TOpenWithSettings.LoadFromDict();
begin
     inherited;
end;

function TOpenWithSettings.GetDefaultEditorsWithVSCodeDetection() : TArray<string>;
var
	vscodeItem : TCommandItem;
	vscodeString : string;
	resultList : TArrayEx<string>;
begin
	resultList := DEFAULT_EDITORS;
	
	vscodeItem := TFileUtils.GetVsCodeCommandItem();
	if vscodeItem.IsActive then begin
		vscodeString := 'TRUE' + SEPARATOR + vscodeItem.Caption + SEPARATOR + 
			vscodeItem.CommandLine.AsString() + SEPARATOR + vscodeItem.Description;
	end else begin
		vscodeString := 'FALSE' + SEPARATOR + vscodeItem.Caption + SEPARATOR + 
			vscodeItem.CommandLine.AsString() + SEPARATOR + vscodeItem.Description;
	end;

	resultList[resultList.IndexOf(VSCODE_EDITOR_SETTING)] := vscodeString;

	Result := resultList;
end;

procedure TOpenWithSettings.SetDefaultsIfEmpty();
begin
	if FCommandListSetting.Value.IsEmpty then begin
        RecreateCommandList(GetDefaultEditorsWithVSCodeDetection());
        StoreToPersister;
	end;
end;

procedure TOpenWithSettings.RecreateCommandList(_cmdListItems : TArrayEx<string>);
var
	settings : string;
	sCmd : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithSettings.RecreateCommandList');

	settings := '';
	self.ClearCommandList; // so deleted entries will be recognized
	for var i := 0 to _cmdListItems.Count - 1 do begin
		sCmd := _cmdListItems[i];
		Command[i] := sCmd;
		dbgMsg.Msg(Format('%s', [Command[i]]));
	end;
	// after ClearCommandList recreate settings...
	SettingsDict.CreateSetting(OPEN_WITH_SETTINGS, ITEM_KEY_PREFIX,
		{ } FCommandListSetting,
		{ } PersisterFactory);
	GetRootOwner().SettingsDict().CopySection(IniSectionName, SettingsDict);
end;

function TOpenWithSettings.GetCommandItems() : TArray<TCommandItem>;
var
	commands : TArray<string>;
	i : Integer;
begin
	commands := GetCommands();
	SetLength(Result, Length(commands));
	
	for i := 0 to High(commands) do begin
		Result[i] := RipGrepper.Tools.FileUtils.TCommandItem.New(commands[i].Split([SEPARATOR]));
	end;
end;

procedure TOpenWithSettings.RecreateCommandList(_cmdItems : TArray<TCommandItem>);
var
	stringItems : TArrayEx<string>;
	item : TCommandItem;
	tabSeparatedString : string;
begin
	stringItems.Clear;
	
	for item in _cmdItems do begin
		tabSeparatedString := Format('%s%s%s%s%s%s%s',
			[BoolToStr(item.IsActive, True), SEPARATOR,
			 item.Caption, SEPARATOR,
			 item.CommandLine.AsString(), SEPARATOR,
			 item.Description]);
		stringItems.Add(tabSeparatedString);
	end;
	
	RecreateCommandList(stringItems);
end;

procedure TOpenWithSettings.LoadCommandsFromJSON();
const
	JSON_COMMANDS_KEY = 'CommandsJSON';
var
	jsonString : string;
	cmdItems : TArray<TCommandItem>;
	persister : IFilePersister<string>;
begin
	// Try to load from JSON first
	persister := PersisterFactory.GetStringPersister(IniSectionName, JSON_COMMANDS_KEY);
	
	if persister.TryLoadValue(jsonString) and not jsonString.Trim.IsEmpty then begin
		cmdItems := RipGrepper.Tools.FileUtils.TCommandItem.ArrayFromJSON(jsonString);
		if Length(cmdItems) > 0 then begin
			RecreateCommandList(cmdItems);
			Exit;
		end;
	end;
	
	// Fallback to tab-separated format if JSON not found
	// (existing logic in ReadFile)
end;

procedure TOpenWithSettings.SaveCommandsToJSON();
const
	JSON_COMMANDS_KEY = 'CommandsJSON';
var
	cmdItems : TArray<TCommandItem>;
	jsonString : string;
	persister : IFilePersister<string>;
begin
	cmdItems := GetCommandItems();
	jsonString := RipGrepper.Tools.FileUtils.TCommandItem.ArrayToJSON(cmdItems);
	
	persister := PersisterFactory.GetStringPersister(IniSectionName, JSON_COMMANDS_KEY);
	persister.StoreValue(jsonString);
end;

function TOpenWithSettings.ToString : string;
begin
	Result := FTestFile.ToString;
end;

end.
