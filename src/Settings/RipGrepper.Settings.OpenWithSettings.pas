unit RipGrepper.Settings.OpenWithSettings;

interface

uses
	RipGrepper.Settings.Persistable,
	System.Classes,
	System.IniFiles,
	RipGrepper.OpenWith.Constants,
	RipGrepper.OpenWith.Params,
	RipGrepper.Settings.SettingVariant;

type
	TOpenWithSettings = class(TPersistableSettings)
		private
			FCommandList : IArraySetting;
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
			procedure ForceWriteToIni;
			function ToString : string; override;
			property Command[index : Integer] : string read GetCommand write SetCommand;
			property TestFile : TOpenWithParams read FTestFile write FTestFile;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.SysUtils,
	ArrayEx;

constructor TOpenWithSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := OPEN_WITH_SETTINGS;
	FCommandList := TArraySetting.Create();
	inherited Create(_Owner);
	TDebugUtils.DebugMessage('TOpenWithSettings.Create: ' + '[' + IniSectionName + ']');
end;

destructor TOpenWithSettings.Destroy;
begin
	inherited Destroy(); // ok
end;

procedure TOpenWithSettings.ClearCommandList;
begin
	FCommandList.Value.Clear;
	FSettingsDict.InnerDictionary.Clear;
	FIsModified := True;
end;

function TOpenWithSettings.GetCommand(Index : Integer) : string;
begin
	Result := '';
	if TArraySetting(FCommandList).Count > index then begin
		Result := TArraySetting(FCommandList)[index];
	end;
end;

procedure TOpenWithSettings.Init;
begin
	inherited;
	for var i : integer := 0 to Length(DEFAULT_EDITORS) - 1 do begin
		Command[i] := DEFAULT_EDITORS[i];
	end;
	CreateSetting(OPENWITH_COMMAND_KEY, FCommandList);
end;

procedure TOpenWithSettings.ReadIni;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithSettings.ReadIni');
	FCommandList.LoadFromFile();
end;

procedure TOpenWithSettings.LoadFromDict;
begin
	//
end;

procedure TOpenWithSettings.SetCommand(Index : Integer; const Value : string);
begin
	if Value.IsEmpty then
		Exit;

	if FCommandList.Value.Count > index then begin
		if (FCommandList.Value[index] <> Value) then begin
			FCommandList.Value[index] := Value;
			FIsModified := True;
		end;
	end else begin
		FCommandList.Value.Add(Value);
		FIsModified := True;
	end;
end;

procedure TOpenWithSettings.ForceWriteToIni;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithSettings.ForceWriteToIni');
	SettingsDict.SaveToFile(OPEN_WITH_SETTINGS);
	UpdateIniFile(OPEN_WITH_SETTINGS, True, True);
end;

function TOpenWithSettings.ToString : string;
begin
	Result := FTestFile.ToString;
end;

end.
