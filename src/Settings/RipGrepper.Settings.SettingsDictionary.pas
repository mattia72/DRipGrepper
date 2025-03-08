unit RipGrepper.Settings.SettingsDictionary;

interface

uses
	RipGrepper.Settings.SettingVariant,
	Spring.Collections,
	System.Variants,
	System.Classes,
	System.IniFiles,
	RipGrepper.Settings.FilePersister;

type
	TSettingSection = string;
	TSettingKey = string;
	ISettingsKeyCollection = IDictionary<TSettingKey, ISetting>;
	ISettingsCollection = IDictionary<TSettingSection, ISettingsKeyCollection>;

	TSettingsDictionary = class
		private
			FInnerDictionary : ISettingsCollection;
			FSectionName : string;
			procedure AddNewSectionAndKey(const _key : string; _setting : ISetting);
			function GetCount() : Integer;
			function GetSections(Index : string) : ISettingsKeyCollection; overload;
			property SectionName : string read FSectionName;

		public
			constructor Create(const _section : string); overload;
			constructor Create; overload;
			procedure AddOrChange(const _key : string; _setting : ISetting);
			procedure CopySection(const _section : string; _from : TSettingsDictionary);
			procedure CreateSetting(const _key : string; _setting : ISetting; _factory : IPersisterFactory);
			function GetSetting(const _key : string) : ISetting; overload;
			function GetSections() : IReadOnlyCollection<string>; overload;
			procedure LoadFromFile();
			procedure SaveToFile();
			property Count : Integer read GetCount;
			property InnerDictionary : ISettingsCollection read FInnerDictionary;
			property Sections[index : string] : ISettingsKeyCollection read GetSections; default;
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
	FInnerDictionary := TCollections.CreateSortedDictionary<TSettingSection, ISettingsKeyCollection>();
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
	keys : ISettingsKeyCollection;
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

procedure TSettingsDictionary.CopySection(const _section : string; _from : TSettingsDictionary);
begin
	FInnerDictionary[_section].Clear;
    FInnerDictionary.Remove(_section);
	FInnerDictionary.Add(_section, _from[_section]);
end;

procedure TSettingsDictionary.CreateSetting(const _key : string; _setting : ISetting; _factory : IPersisterFactory);
begin

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
	AddOrChange(_key, _setting);
	TDebugUtils.MsgFmt('TSettingsDictionary.CreateSetting - [%s] %s', [SectionName, _key]);
end;

function TSettingsDictionary.GetCount() : Integer;
begin
	Result := InnerDictionary.Count;
end;

function TSettingsDictionary.GetSections(Index : string) : ISettingsKeyCollection;
begin
	Result := FInnerDictionary[index];
end;

function TSettingsDictionary.GetSetting(const _key : string) : ISetting;
var
	keys : ISettingsKeyCollection;
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
var
	key : string;
	value : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.LoadFromFile');

	for key in InnerDictionary[SectionName].Keys do begin
		InnerDictionary[SectionName][key].LoadFromFile();
		dbgMsg.MsgFmt('LoadFromFile [%s] %s = %s', [SectionName, key, value]);
	end;
end;

procedure TSettingsDictionary.SaveToFile();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.SaveToFile');
	for var keys in InnerDictionary[SectionName] do begin
		keys.Value.SaveToFile();
		dbgMsg.MsgFmt('SaveToFile [%s] %s', [SectionName, keys.Key]);
	end;
end;

end.
