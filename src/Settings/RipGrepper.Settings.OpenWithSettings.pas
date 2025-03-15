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
			procedure ClearCommandList;
			function GetCommands() : TArray<string>;
			procedure Init; override;
			procedure ReadIni; override; // TODO: use persistable base
			procedure ForceWriteToIni;
			function ToString : string; override;
			property Command[index : Integer] : string read GetCommand write SetCommand;
			property TestFile : TOpenWithParams read FTestFile write FTestFile;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.SysUtils,
	ArrayEx,
	RipGrepper.Settings.SettingsDictionary;

constructor TOpenWithSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := OPEN_WITH_SETTINGS;
	inherited Create(_Owner);
	TDebugUtils.DebugMessage('TOpenWithSettings.Create: ' + '[' + IniSectionName + ']');
end;

procedure TOpenWithSettings.ClearCommandList;
begin
	FCommandList.Value.Clear;
	FSettingsDict.InnerDictionary.Clear;
	FIsModified := True;
end;

function TOpenWithSettings.GetCommands() : TArray<string>;
begin
	Result := FCommandList.Value;
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
	FCommandList := TArraySetting.Create();

	for var i : integer := 0 to Length(DEFAULT_EDITORS) - 1 do begin
		Command[i] := DEFAULT_EDITORS[i];
	end;
	CreateSetting(OPENWITH_COMMAND_KEY, FCommandList);
end;

procedure TOpenWithSettings.ReadIni;
var
	iarr : IArraySetting;
    arr : TArray<string>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithSettings.ReadIni');

	iarr := TArraySetting.Create(arr);
    iarr.Copy(FCommandList);
	iarr.LoadFromFile();
    if not iarr.Value.IsEmpty then begin
        FCommandList.Copy(iarr);
    end;
end;

procedure TOpenWithSettings.SetCommand(Index : Integer; const Value : string);
var
	arrCmds : TArrayEx<string>;
begin
	if Value.IsEmpty then
		Exit;

	if FCommandList.Value.Count > index then begin
		if (FCommandList.Value[index] <> Value) then begin
			FCommandList.Value[index] := Value;
			FIsModified := True;
		end;
	end else begin
		arrCmds := FCommandList.Value;
		arrCmds.Add(Value);

		FCommandList.Value := arrCmds;

		FIsModified := True;
	end;
end;

procedure TOpenWithSettings.ForceWriteToIni;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithSettings.ForceWriteToIni');
	var
	dbgArr := TSettingsDictionary.DictToStringArray(SettingsDict());

	SettingsDict.SaveToFile(OPEN_WITH_SETTINGS);
	UpdateIniFile(OPEN_WITH_SETTINGS, True, True);
end;

function TOpenWithSettings.ToString : string;
begin
	Result := FTestFile.ToString;
end;

end.
