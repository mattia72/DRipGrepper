unit RipGrepper.Settings.SettingsDictionary;

interface

uses
	System.Classes,
	System.IniFiles,
	System.SyncObjs,
	Spring.Collections,
	RipGrepper.Common.Interfaces.StreamPersistable,
	RipGrepper.Settings.SettingVariant,
	RipGrepper.Settings.FilePersister,
	RipGrepper.Settings.Persister.Interfaces;

type
	TSettingSection = string;
	TSettingKey = string;
	ISettingKeys = IDictionary<TSettingKey, ISetting>;
	ISettingSections = IDictionary<TSettingSection, ISettingKeys>;

	TSettingsDictionary = class(TNoRefCountObject, IStreamReaderWriterPersistable)
		private
			FInnerDictionary : ISettingSections;
			FSectionName : string;
			FOwnerPersister : IPersisterFactory;
			FDictionaryLock : TCriticalSection;
			procedure AddNewSectionAndKey(const _key : string; _setting : ISetting);
			procedure AddOrChangeStrArrSettings(const _key : string; _setting : ISetting; _factory : IPersisterFactory);
			function GetCount() : Integer;
			function GetSections(index : string) : ISettingKeys; overload;
			procedure StoreSectionToPersister(const _section : string);
			procedure SetSections(index : string; const Value : ISettingKeys);
			property SectionName : string read FSectionName;

		public
			constructor Create(const _section : string; _ownerPersister : IPersisterFactory); overload;
			constructor Create; overload;
			destructor Destroy; override;
			procedure AddOrChange(const _key : string; _setting : ISetting);
			procedure ClearSection(const _section : string);
			function ContainsSection(const _section : string) : Boolean;
			procedure CopySection(const _section : string; _from : TSettingsDictionary);
			procedure CreateSetting(const _key : string; _setting : ISetting; _factory : IPersisterFactory); overload;
			procedure CreateSetting(const _section, _key : string; _setting : ISetting; _factory : IPersisterFactory); overload;
			function GetEnumerator() : IEnumerator<Spring.Collections.TPair<TSettingSection, ISettingKeys>>;
			class function DictToStringArray(_dict : TSettingsDictionary) : TArray<TArray<string>>;
			function GetSetting(const _key : string) : ISetting; overload;
			function GetSections() : IReadOnlyCollection<string>; overload;
			procedure LoadFromPersister();
			procedure SetState(const _from, _to : TSettingState; const _section : string = '');
			function HasState(const _state : TSettingState; const _section : string = '') : Boolean;
			procedure LoadFromStreamReader(_sr : TStreamReader);
			procedure SaveToStreamWriter(_sw : TStreamWriter);
			procedure StoreToPersister(const _section : string);

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
	System.RegularExpressions,
	RipGrepper.Helper.StreamReaderWriter;

constructor TSettingsDictionary.Create(const _section : string; _ownerPersister : IPersisterFactory);
begin
	Create;
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.Create(_section)', True);
	FSectionName := _section;
	dbgMsg.MsgFmt('Create %p for section: %s', [Pointer(self), FSectionName]);
	FOwnerPersister := _ownerPersister;
end;

destructor TSettingsDictionary.Destroy;
begin
	FDictionaryLock.Free;
	inherited;
end;

constructor TSettingsDictionary.Create;
begin
	inherited;
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.Create', True);
	dbgMsg.MsgFmt('Create %p for section: ???', [Pointer(self)]);
	FInnerDictionary := TCollections.CreateSortedDictionary<TSettingSection, ISettingKeys>();
	FDictionaryLock := TCriticalSection.Create;
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
	autoLock := TAutoLock.Create(FDictionaryLock);
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
	i : Integer;
begin
	i := 0;
	for var cmd in TArraySetting(_setting).AsArray do begin
		var
			s : ISetting := TStringSetting.Create(_key, cmd);
		s.SaveBehaviour := _setting.SaveBehaviour;
		s.State := _setting.State;
		var
		key := Format('%s%d', [_key, i]);
		TStringSetting(s).Persister := _factory.GetStringPersister(SectionName, key);
		AddOrChange(key, s);
		Inc(i);
	end;
end;

procedure TSettingsDictionary.ClearSection(const _section : string);
var
	sd : IDictionary<TSettingKey, ISetting>;
begin
	var
	autoLock := TAutoLock.Create(FDictionaryLock);
	if InnerDictionary.TryGetValue(_section, sd) then begin
		for var key in sd.Keys do begin
			sd[key].Clear();
			sd[key].State := ssModified;
		end;
	end;
end;

function TSettingsDictionary.ContainsSection(const _section : string) : Boolean;
begin
	var
	autoLock := TAutoLock.Create(FDictionaryLock);
	Result := FInnerDictionary.ContainsKey(_section);
end;

procedure TSettingsDictionary.CopySection(const _section : string; _from : TSettingsDictionary);
var
	sdSelf : ISettingKeys;
	sdFrom : ISettingKeys;
begin
	var
	autoLock := TAutoLock.Create(FDictionaryLock);
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.CopySection');

	if FInnerDictionary.TryGetValue(_section, sdSelf) then begin
		if _from.InnerDictionary.TryGetValue(_section, sdFrom) then begin
			var
			dbgFrom := TSettingsDictionary.DictToStringArray(_from);
			FInnerDictionary[_section] := sdFrom;
			var
			dbgTo := TSettingsDictionary.DictToStringArray(self);
			dbgMsg.MsgFmt('Copy from existent section [%s]', [_section]);
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
	CreateSetting(SectionName, _key, _setting, _factory);
end;

procedure TSettingsDictionary.CreateSetting(const _section, _key : string; _setting : ISetting; _factory : IPersisterFactory);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.CreateSetting');

	case _setting.SettingType of
		stString : begin
			TStringSetting(_setting).Persister := _factory.GetStringPersister(_section, _key);
		end;
		stInteger : begin
			TIntegerSetting(_setting).Persister := _factory.GetIntegerPersister(_section, _key);
		end;
		stBool : begin
			TBoolSetting(_setting).Persister := _factory.GetBoolPersister(_section, _key);
		end;
		stStrArray : begin
			TArraySetting(_setting).Persister := _factory.GetStrArrayPersister(_section, _key);
		end;

		else
		raise ESettingsException.Create('Setting Type not supported.');
	end;

	if _setting.SettingType = stStrArray then begin
		AddOrChangeStrArrSettings(_key, _setting, _factory);
	end else begin
		AddOrChange(_key, _setting);
	end;

	dbgMsg.MsgFmt('TSettingsDictionary.CreateSetting [%s] %s', [_section, _key]);
end;

class function TSettingsDictionary.DictToStringArray(_dict : TSettingsDictionary) : TArray<TArray<string>>;
const
	{ } IS_CONSOLE = {$IFDEF CONSOLE} TRUE; {$ELSE} FALSE; {$ENDIF}
{ } IS_UNITTEST = {$IFDEF TESTINSIGHT} TRUE; {$ELSE} FALSE; {$ENDIF}
{ } IS_DEBUG = {$IFDEF DEBUG} TRUE; {$ELSE} FALSE; {$ENDIF}
begin
	{$IF IS_DEBUG OR IS_UNITTEST OR IS_CONSOLE}
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
	var
	autoLock := TAutoLock.Create(FDictionaryLock);
	Result := FInnerDictionary[index];
end;

function TSettingsDictionary.GetSetting(const _key : string) : ISetting;
var
	keys : ISettingKeys;
begin
	var
	autoLock := TAutoLock.Create(FDictionaryLock);
	Result := nil;
	if FInnerDictionary.TryGetValue(SectionName, keys) then begin
		Result := keys[_key];
	end;
end;

function TSettingsDictionary.GetSections() : IReadOnlyCollection<string>;
begin
	var
	autoLock := TAutoLock.Create(FDictionaryLock);
	Result := FInnerDictionary.Keys;
end;

procedure TSettingsDictionary.LoadFromPersister();
begin
	var
	autoLock := TAutoLock.Create(FDictionaryLock);
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.LoadFromPersister');

	try
		for var key in InnerDictionary[SectionName].Keys do begin
			dbgMsg.MsgFmt('Get InnerDictionary[%s][%s]', [SectionName, key]);
			var
			section := InnerDictionary[SectionName];
			if InnerDictionary.TryGetValue(SectionName, section) then begin
				var
					setting : ISetting;
				if section.TryGetValue(key, setting) then begin
					dbgMsg.MsgFmt('InnerDictionary[%s][%s] found', [SectionName, key]);
					setting.LoadFromPersister();
					var
					value := setting.AsString;
					dbgMsg.MsgFmt('LoadFromPersister [%s] %s = %s', [SectionName, key, value]);
				end else begin
					dbgMsg.MsgFmt('InnerDictionary[%s][%s] not found', [SectionName, key]);
					raise Exception.CreateFmt('InnerDictionary[%s][%s] not found', [SectionName, key]);
				end;
			end;
		end;
	except
		on E : Exception do begin
			dbgMsg.ErrorMsgFmt('Error loading from persister: %s', [E.Message]);
		end;
	end;
end;

procedure TSettingsDictionary.StoreSectionToPersister(const _section : string);
var
	setting : ISetting;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.StoreSectionToPersister');

	for var keys in InnerDictionary[_section] do begin
		setting := keys.Value;
		setting.StoreToPersister(_section);

		{$IFDEF DEBUG}
		var
		value := InnerDictionary[_section][keys.Key].AsString;
		dbgMsg.MsgFmt('StoreToPersister [%s] %s = dic:''%s'' ? set:''%s''', [_section, keys.Key, value,  setting.AsString]);

	    Assert(value = setting.AsString, Format('StoreToPersister [%s] %s %s <> %s', [_section, keys.Key, value, setting.AsString]));
		{$ENDIF}
	end;
end;

procedure TSettingsDictionary.StoreToPersister(const _section : string);
var
	section : string;
begin
	var
	autoLock := TAutoLock.Create(FDictionaryLock);
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.StoreToPersister');

	section := IfThen(_section = '', SectionName, _section);

	if (ROOT_DUMMY_INI_SECTION = section) then begin
		for section in InnerDictionary.Keys do begin
			StoreSectionToPersister(section);
		end;
	end else begin
		if not section.IsEmpty and InnerDictionary.ContainsKey(section) then begin
			StoreSectionToPersister(section);
		end else begin
			// var
			// dbgArr := TSettingsDictionary.DictToStringArray(self);
			dbgMsg.MsgFmt('invalid section: ''%s''', [section]);
			// raise ESettingsException.CreateFmt('invalid section: ''%s''', [section]);
		end;
	end;
end;

procedure TSettingsDictionary.SetSections(index : string; const Value : ISettingKeys);
begin
	var
	autoLock := TAutoLock.Create(FDictionaryLock);
	FInnerDictionary[index] := Value;
end;

procedure TSettingsDictionary.SetState(const _from, _to : TSettingState; const _section : string = '');
begin
	var
	autoLock := TAutoLock.Create(FDictionaryLock);
	if _section.IsEmpty then begin
		InnerDictionary.Where(
			function(const p : TPair<string, ISettingKeys>) : Boolean
			begin
				Result := p.Value.Values.Any(
					function(const s : ISetting) : Boolean
					begin
						Result := s.State = _from;
					end);
			end).ForEach(
			procedure(const p : TPair<string, ISettingKeys>)
			begin
				p.Value.Values.ForEach(
					procedure(const s : ISetting)
					begin
						if s.State = _from then begin
							s.State := _to;
						end;
					end);
			end);
	end else begin
		InnerDictionary[_section].ForEach(
			procedure(const p : TPair<string, ISetting>)
			begin
				var
					s : ISetting := p.Value;
				if s.State = _from then begin
					s.State := _to;
				end;
			end);

	end;
end;

function TSettingsDictionary.HasState(const _state : TSettingState; const _section : string = '') : Boolean;
begin
	var
	autoLock := TAutoLock.Create(FDictionaryLock);
	if _section.IsEmpty then begin
		Result := InnerDictionary.Any(
			function(const p : TPair<string, ISettingKeys>) : Boolean
			begin
				Result := p.Value.Values.Any(
					function(const s : ISetting) : Boolean
					begin
						Result := s.State = _state;
					end);
			end);
	end else begin
		Result := InnerDictionary[_section].Any(
			function(const p : TPair<string, ISetting>) : Boolean
			begin
				Result := p.Value.State = _state;
			end);
	end;
end;

procedure TSettingsDictionary.LoadFromStreamReader(_sr : TStreamReader);
var
	keyCount : Integer;
	section : string;
	sectionCount : integer;
	setting : ISetting;
	settingType : TSettingType;
begin
	var
	autoLock := TAutoLock.Create(FDictionaryLock);
	InnerDictionary.Clear;
	sectionCount := _sr.ReadLineAsInteger('SectionCount');
	for var i := 0 to sectionCount - 1 do begin
		section := _sr.ReadLineAsString(false, 'SettingsDictionary.SectionName'); // Section names should not be empty

		if not FInnerDictionary.ContainsKey(section) then begin
			FInnerDictionary[section] :=
			{ } TCollections.CreateSortedDictionary<TSettingKey, ISetting>();
		end;

		keyCount := _sr.ReadLineAsInteger('KeyCount');
		for var j := 0 to keyCount - 1 do begin
			var
			key := _sr.ReadLineAsString();
			settingType := TSettingType(_sr.ReadLineAsInteger(key + '.SettingType'));
			setting := TSettingFactory.CreateSetting(settingType, key);
			(setting as IStreamReaderWriterPersistable).LoadFromStreamReader(_sr);
			CreateSetting(section, key, setting, FOwnerPersister);
		end;
	end;
end;

procedure TSettingsDictionary.SaveToStreamWriter(_sw : TStreamWriter);
begin
	var
	autoLock := TAutoLock.Create(FDictionaryLock);
	_sw.WriteLineAsInteger(FInnerDictionary.Count, 'FInnerDictionary.Count');
	var
	arr := DictToStringArray(self);
	for var section in FInnerDictionary.Keys do begin
		_sw.WriteLineAsString(section, false, 'SettingsDictionary.SectionName');
		_sw.WriteLineAsInteger(FInnerDictionary[section].Count, section + '.Count');
		for var key in FInnerDictionary[section].Keys do begin
			_sw.WriteLineAsString(key, false, Format('[%s]%s', [section, key]));
			_sw.WriteLineAsInteger(Integer(FInnerDictionary[section][key].SettingType), Format('[%s]%s.SettingType',[section, key]));
			(FInnerDictionary[section][key] as IStreamReaderWriterPersistable).SaveToStreamWriter(_sw);
		end;
	end;
end;

end.
