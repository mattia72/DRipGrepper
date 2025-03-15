unit RipGrepper.Settings.SettingsDictionary;

interface

uses
	RipGrepper.Settings.SettingVariant,
	Spring.Collections,
	System.IniFiles,
	RipGrepper.Settings.FilePersister;

type
	TSettingSection = string;
	TSettingKey = string;
	ISettingKeys = IDictionary<TSettingKey, ISetting>;
	ISettingSections = IDictionary<TSettingSection, ISettingKeys>;

	TSettingsDictionary = class

		private
			FInnerDictionary : ISettingSections;
			FSectionName : string;
			procedure AddNewSectionAndKey(const _key : string; _setting : ISetting);
			procedure AddOrChangeStrArrSettings(const _key : string; _setting : ISetting; _factory : IPersisterFactory);
			function GetCount() : Integer;
			function GetSections(index : string) : ISettingKeys; overload;
			procedure SaveSectionToFile(const _section : string);
			procedure SetSections(index : string; const Value : ISettingKeys);
			property SectionName : string read FSectionName;

		public
			constructor Create(const _section : string); overload;
			constructor Create; overload;
			procedure AddOrChange(const _key : string; _setting : ISetting);
			function ContainsSection(const _section : string) : Boolean;
			procedure CopySection(const _section : string; _from : TSettingsDictionary);
			procedure CreateSetting(const _key : string; _setting : ISetting; _factory : IPersisterFactory);
			function GetEnumerator() : IEnumerator<Spring.Collections.TPair<TSettingSection, ISettingKeys>>;
			class function DictToStringArray(_dict : TSettingsDictionary) : TArray<TArray<string>>;
			function GetSetting(const _key : string) : ISetting; overload;
			function GetSections() : IReadOnlyCollection<string>; overload;
			procedure LoadFromFile();
			procedure SaveToFile(const _section : string = '');

			property Count : Integer read GetCount;
			property InnerDictionary : ISettingSections read FInnerDictionary;
			property Sections[index : string] : ISettingKeys read GetSections write SetSections; default;
	end;

implementation

uses
	RipGrepper.Common.Constants,
	RipGrepper.Tools.DebugUtils,
	System.StrUtils,
	System.SysUtils,
	RipGrepper.Helper.Types,
	Spring,
	System.RegularExpressions;

constructor TSettingsDictionary.Create(const _section : string);
begin
	Create;
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.Create(_section)', True);
	FSectionName := _section;
	dbgMsg.MsgFmt('Create %p for section: %s', [Pointer(self), FSectionName]);
end;

constructor TSettingsDictionary.Create;
begin
	inherited;
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.Create', True);
	dbgMsg.MsgFmt('Create %p for section: ???', [Pointer(self)]);
	FInnerDictionary := TCollections.CreateSortedDictionary<TSettingSection, ISettingKeys>();
end;

procedure TSettingsDictionary.AddNewSectionAndKey(const _key : string; _setting : ISetting);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.AddNewSectionAndKey', True);

	FInnerDictionary.Add(SectionName,
		{ } TCollections.CreateSortedDictionary<TSettingKey, ISetting>());
	FInnerDictionary[SectionName].Add(_key, _setting);
	dbgMsg.MsgFmt('Add %s', [_key]);
end;

procedure TSettingsDictionary.AddOrChange(const _key : string; _setting : ISetting);
var
	keys : ISettingKeys;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.AddOrChange');

	if FInnerDictionary.TryGetValue(SectionName, keys) then begin
		keys[_key] := _setting;
		dbgMsg.MsgFmt('Update %s', [_key]);
	end else begin
		AddNewSectionAndKey(_key, _setting);
	end;
end;

procedure TSettingsDictionary.AddOrChangeStrArrSettings(const _key : string; _setting : ISetting; _factory : IPersisterFactory);
var
	i: Integer;
begin
	i := 0;
	for var cmd in TArraySetting(_setting).AsArray do begin
		var s : ISetting := TStringSetting.Create(cmd);
		var
		key := Format('%s%d', [_key, i]);
		TStringSetting(s).Persister := _factory.GetStringPersister(SectionName, key);
		AddOrChange(key, s);
		Inc(i);
	end;
end;

function TSettingsDictionary.ContainsSection(const _section : string) : Boolean;
begin
	Result := FInnerDictionary.ContainsKey(_section);
end;

procedure TSettingsDictionary.CopySection(const _section : string; _from : TSettingsDictionary);
var
	sdSelf : ISettingKeys;
	sdFrom : ISettingKeys;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.CopySection');

	if FInnerDictionary.TryGetValue(_section, sdSelf) then begin
		if _from.InnerDictionary.TryGetValue(_section, sdFrom) then begin
			for var key in sdFrom.Keys do begin
				if sdSelf.ContainsKey(key) then begin
					sdSelf[key].Copy(sdFrom[key]);
				end else begin
					sdSelf[key] := sdFrom[key];
				end;
			end;
		end else begin
			dbgMsg.ErrorMsgFmt('Copy from non existent section [%s]', [_section]);
			raise ESettingsException.CreateFmt('Copy from non existent section [%s]', [_section]);
		end;
	end else begin
		dbgMsg.ErrorMsgFmt('Section not exists, add [%s]', [_section]);
		FInnerDictionary.Add(_section, _from[_section]);
	end;
end;

procedure TSettingsDictionary.CreateSetting(const _key : string; _setting : ISetting; _factory : IPersisterFactory);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.CreateSetting');

	case _setting.SettingType of
		stString : begin
			TStringSetting(_setting).Persister := _factory.GetStringPersister(SectionName, _key);
		end;
		stInteger : begin
			TIntegerSetting(_setting).Persister := _factory.GetIntegerPersister(SectionName, _key);
		end;
		stBool : begin
			TBoolSetting(_setting).Persister := _factory.GetBoolPersister(SectionName, _key);
		end;
		stStrArray : begin
			TArraySetting(_setting).Persister := _factory.GetStrArrayPersister(SectionName, _key);
		end;
		else
		raise ESettingsException.Create('Setting Type not supported.');
	end;

	if _setting.SettingType = stStrArray then begin
		AddOrChangeStrArrSettings(_key, _setting, _factory);
	end else begin
		AddOrChange(_key, _setting);
	end;

	dbgMsg.MsgFmt('TSettingsDictionary.CreateSetting [%s] %s', [SectionName, _key]);
end;

class function TSettingsDictionary.DictToStringArray(_dict : TSettingsDictionary) : TArray<TArray<string>>;
begin
	{$IFDEF DEBUG}
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.DictToStringArray');
	for var section in _dict.InnerDictionary.Keys do begin
		Result := Result + [['Section', section]];
		dbgMsg.MsgFmt('[%s]', [section], tftVerbose);

		for var pair in _dict.InnerDictionary[section] do begin
			var
			setting := pair.Value;
			var
			sVal := setting.AsString;
			Result := Result + [[pair.Key, sVal]];
			dbgMsg.MsgFmt('%s=%s', [pair.Key, sVal], tftVerbose);
		end;
	end;
	{$ENDIF}
end;

function TSettingsDictionary.GetCount() : Integer;
begin
	Result := InnerDictionary.Count;
end;

function TSettingsDictionary.GetEnumerator() : IEnumerator<Spring.Collections.TPair<TSettingSection, ISettingKeys>>;
begin
	Result := FInnerDictionary.GetEnumerator();
end;

function TSettingsDictionary.GetSections(index : string) : ISettingKeys;
begin
	Result := FInnerDictionary[index];
end;

function TSettingsDictionary.GetSetting(const _key : string) : ISetting;
var
	keys : ISettingKeys;
begin
	Result := nil;
	if FInnerDictionary.TryGetValue(SectionName, keys) then begin
		Result := keys[_key];
	end;
end;

function TSettingsDictionary.GetSections() : IReadOnlyCollection<string>;
begin
	Result := FInnerDictionary.Keys;
end;

procedure TSettingsDictionary.LoadFromFile();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.LoadFromFile');

	for var key in InnerDictionary[SectionName].Keys do begin
		InnerDictionary[SectionName][key].LoadFromFile();
		var
		value := InnerDictionary[SectionName][key].AsString;
		dbgMsg.MsgFmt('LoadFromFile [%s] %s = %s', [SectionName, key, value]);
	end;
end;

procedure TSettingsDictionary.SaveSectionToFile(const _section : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.SaveSectionToFile');

	for var keys in InnerDictionary[_section] do begin
		keys.Value.SaveToFile();
		{$IFDEF DEBUG}
		var
		value := InnerDictionary[_section][keys.Key].AsString;
		dbgMsg.MsgFmt('SaveToFile [%s] %s = %s', [_section, keys.Key, value]);
		{$ENDIF}
	end;
end;

procedure TSettingsDictionary.SaveToFile(const _section : string = '');
var
	section : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.SaveToFile');

	section := IfThen(_section = '', SectionName, _section);

	if (ROOT_DUMMY_INI_SECTION = section) then begin
		for section in InnerDictionary.Keys do begin
			SaveSectionToFile(section);
		end;
	end else begin
		if not section.IsEmpty and InnerDictionary.ContainsKey(section) then begin
			SaveSectionToFile(section);
		end else begin
			dbgMsg.MsgFmt('invalid section: ''%s''', [section]);
			raise ESettingsException.CreateFmt('invalid section: ''%s''', [section]);
		end;
	end;
end;

procedure TSettingsDictionary.SetSections(index : string; const Value : ISettingKeys);
begin
	FInnerDictionary[index] := Value;
end;

end.
