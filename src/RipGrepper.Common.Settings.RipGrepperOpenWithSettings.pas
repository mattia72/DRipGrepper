unit RipGrepper.Common.Settings.RipGrepperOpenWithSettings;

interface

uses
	RipGrepper.Common.Settings.Persistable,
	System.Classes,
	System.IniFiles,
	RipGrepper.OpenWith.Constants,
	RipGrepper.OpenWith.Params;

type
	TRipGrepperOpenWithSettings = class(TPersistableSettings)
		private
			FCommandList : TStringList;
			FTestFile : TOpenWithParams;
			function GetCommand(Index : Integer) : string;
			procedure SetCommand(Index : Integer; const Value : string);

		public
			constructor Create(const _ini : TMemIniFile);
			destructor Destroy; override;
			procedure Read; override;
			procedure Store; override;
			property Command[index : Integer] : string read GetCommand write SetCommand;
			property TestFile : TOpenWithParams read FTestFile write FTestFile;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.SysUtils;

constructor TRipGrepperOpenWithSettings.Create(const _ini : TMemIniFile);
begin
	inherited;
	IniSectionName := OPEN_WITH_SETTINGS;
	FCommandList := TStringList.Create;
	TDebugUtils.DebugMessage('TRipGrepperOpenWithSettings.Create: ' + FIniFile.FileName + '[' + IniSectionName + ']');
end;

destructor TRipGrepperOpenWithSettings.Destroy;
begin
	FCommandList.Free;
	inherited;
end;

function TRipGrepperOpenWithSettings.GetCommand(Index : Integer) : string;
begin
	Result := '';
	if FCommandList.Count > index then begin
		Result := FCommandList[index];
	end;
end;

procedure TRipGrepperOpenWithSettings.Read;
var
	s : string;
begin
	for var i : integer := 0 to MAX_COMMAND_NUM do begin
		s := FIniFile.ReadString(OPEN_WITH_SETTINGS, OPENWITH_COMMAND_KEY + i.ToString, '');
		if (not s.IsEmpty) then begin
			Command[i] := s;
		end else begin
			break
		end;
	end;
	//FIsAlreadyRead := True;
end;

procedure TRipGrepperOpenWithSettings.SetCommand(Index : Integer; const Value : string);
begin
	if Value.IsEmpty then
		Exit;

	if FCommandList.Count > index then begin
		if (FCommandList[index] <> Value) then begin
			FCommandList[index] := Value;
			IsModified := True;
		end;
	end else begin
		FCommandList.Add(Value);
		IsModified := True;
	end;
end;

procedure TRipGrepperOpenWithSettings.Store;
var
	s : string;
begin
	if IsAlreadyRead and IsModified then begin
		if FCommandList.Count > 0 then begin
			for var i : integer := 0 to MAX_COMMAND_NUM do begin
				s := Command[i];
				if s.IsEmpty then
					break;
				FIniFile.WriteString(OPEN_WITH_SETTINGS, OPENWITH_COMMAND_KEY + i.ToString, s);
			end;
		end;
		FIsModified := False;
	end;
end;

end.
