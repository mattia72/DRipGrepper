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
			FOwnIniFile : TMemIniFile;
			FTestFile : TOpenWithParams;
			function GetCommand(Index : Integer) : string;
			procedure SetCommand(Index : Integer; const Value : string);

		public
			constructor Create(const _ini : TMemIniFile);
			destructor Destroy; override;
			procedure ClearCommandList;
			procedure Init; override;
			procedure ReadIni; override; // TODO: use persistable base
			procedure LoadFromDict(); override;
			procedure LoadDefaultsFromDict; override;
			procedure WriteToIni;
			function ToString : string; override;
			property Command[index : Integer] : string read GetCommand write SetCommand;
			property TestFile : TOpenWithParams read FTestFile write FTestFile;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.SysUtils;

constructor TOpenWithSettings.Create(const _ini : TMemIniFile);
begin
	IniSectionName := OPEN_WITH_SETTINGS;
	inherited;
	FCommandList := TStringList.Create;
	FOwnIniFile := TMemIniFile.Create(_ini.FileName, TEncoding.UTF8);
	TDebugUtils.DebugMessage('TOpenWithSettings.Create: ' + FIniFile.FileName + '[' + IniSectionName + ']');
end;

destructor TOpenWithSettings.Destroy;
begin
	FCommandList.Free;
	FOwnIniFile.Free;
	inherited Destroy(); // ok
end;

procedure TOpenWithSettings.ClearCommandList;
begin
	FCommandList.Clear;
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
	// TODO -cMM: TOpenWithSettings.Init default body inserted
end;

procedure TOpenWithSettings.ReadIni;
var
	s : string;
begin
	ReCreateMemIni(FOwnIniFile);
	for var i : integer := 0 to MAX_COMMAND_NUM do begin
		s := FOwnIniFile.ReadString(OPEN_WITH_SETTINGS, OPENWITH_COMMAND_KEY + i.ToString, '');
		if (not s.IsEmpty) then begin
			Command[i] := s;
		end else begin
			break
		end;
	end;
end;

procedure TOpenWithSettings.LoadFromDict;
begin
	// TODO -cMM: TOpenWithSettings.LoadFromDict default body inserted
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

procedure TOpenWithSettings.WriteToIni;
var
	s : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithSettings.WriteToIni');
	if { IsAlreadyRead never set :( } IsModified then begin
		ReCreateMemIni(FOwnIniFile);
		FOwnIniFile.EraseSection(OPEN_WITH_SETTINGS);
		if FCommandList.Count > 0 then begin
			for var i : integer := 0 to MAX_COMMAND_NUM do begin
				s := Command[i];
				if s.IsEmpty then
					break;
				FOwnIniFile.WriteString(OPEN_WITH_SETTINGS, OPENWITH_COMMAND_KEY + i.ToString, s);
				dbgMsg.MsgFmt('[%s] %s%d: %s', [OPEN_WITH_SETTINGS, OPENWITH_COMMAND_KEY, i, s]);
			end;
		end;
		FOwnIniFile.UpdateFile;
		FIsModified := False;
	end;
end;

function TOpenWithSettings.ToString : string;
begin
	Result := FTestFile.ToString;
end;

end.
