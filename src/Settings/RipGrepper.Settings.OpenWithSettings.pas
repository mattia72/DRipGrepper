unit RipGrepper.Settings.OpenWithSettings;

interface

uses
	RipGrepper.Settings.Persistable,
	System.Classes,
	System.IniFiles,
	RipGrepper.OpenWith.Constants,
	RipGrepper.OpenWith.Params;

type
	TOpenWithSettings = class(TPersistableSettings)
		private
			FCommandList : TStringList;
			FTestFile : TOpenWithParams;
			function GetCommand(Index : Integer) : string;
			procedure SetCommand(Index : Integer; const Value : string);

		public
			constructor Create(const _Owner : TPersistableSettings);
			destructor Destroy; override;
			procedure ClearCommandList;
			procedure Init; override;
			procedure ReadIni; override; // TODO: use persistable base
			procedure LoadFromDict(); override;
			procedure LoadDefaultsFromDict; override;
			procedure ForceWriteToIni;
			function ToString : string; override;
			property Command[index : Integer] : string read GetCommand write SetCommand;
			property TestFile : TOpenWithParams read FTestFile write FTestFile;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.SysUtils;

constructor TOpenWithSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := OPEN_WITH_SETTINGS;
	FCommandList := TStringList.Create;
	inherited Create(_Owner);
	TDebugUtils.DebugMessage('TOpenWithSettings.Create: ' + IniFile.FileName + '[' + IniSectionName + ']');
end;

destructor TOpenWithSettings.Destroy;
begin
	FCommandList.Free;
	inherited Destroy(); // ok
end;

procedure TOpenWithSettings.ClearCommandList;
begin
	FCommandList.Clear;
	FSettingsDict.Clear;
	FIsModified := True;
end;

function TOpenWithSettings.GetCommand(Index : Integer) : string;
begin
	Result := '';
	if FCommandList.Count > index then begin
		Result := FCommandList[index];
	end;
end;

procedure TOpenWithSettings.Init;
begin
	inherited;
	for var i : integer := 0 to Length(DEFAULT_EDITORS) - 1 do begin
		Command[i] := DEFAULT_EDITORS[i];
	end;
end;

procedure TOpenWithSettings.ReadIni;
var
	s : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithSettings.ReadIni');

	for var i : integer := 0 to MAX_COMMAND_NUM do begin
		s := IniFile.ReadString(OPEN_WITH_SETTINGS, OPENWITH_COMMAND_KEY + i.ToString, '');
		if (not s.IsEmpty) then begin
			Command[i] := s;
		end else begin
			break
		end;
	end;
end;

procedure TOpenWithSettings.LoadFromDict;
begin
	//
end;

procedure TOpenWithSettings.LoadDefaultsFromDict;
begin
	// it has no defaults yet
end;

procedure TOpenWithSettings.SetCommand(Index : Integer; const Value : string);
begin
	if Value.IsEmpty then
		Exit;

	if FCommandList.Count > index then begin
		if (FCommandList[index] <> Value) then begin
			FCommandList[index] := Value;
			FIsModified := True;
		end;
	end else begin
		FCommandList.Add(Value);
		FIsModified := True;
	end;
end;

procedure TOpenWithSettings.ForceWriteToIni;
var
	cmdListItem : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithSettings.ForceWriteToIni');
	if { IsAlreadyRead never set :( } IsModified then begin
		if FCommandList.Count > 0 then begin
			for var i : integer := 0 to MAX_COMMAND_NUM do begin
				cmdListItem := Command[i];
				if cmdListItem.IsEmpty then
					break;
				SettingsDict.SetSettingValue(
					{ } Format('%s|%s', [OPEN_WITH_SETTINGS, OPENWITH_COMMAND_KEY + i.ToString]), cmdListItem);
				dbgMsg.MsgFmt('[%s] %s%d: %s', [OPEN_WITH_SETTINGS, OPENWITH_COMMAND_KEY, i, cmdListItem]);
			end;
			UpdateIniFile(OPEN_WITH_SETTINGS, True, True);
		end;
		FIsModified := False;
	end;
end;

function TOpenWithSettings.ToString : string;
begin
	Result := FTestFile.ToString;
end;

end.
