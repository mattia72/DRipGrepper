unit RipGrepper.Settings.StreamPersister;

interface

uses
	System.Classes,
	System.SysUtils,
	RipGrepper.Settings.Persister.Interfaces,
	Spring,
	Spring.Collections,
	RipGrepper.Common.Interfaces.StreamPersistable,
	RipGrepper.Settings.SettingsDictionary;

type
	TDictionaryStreamPersister = class(TInterfacedObject, IStreamPersistable)
		private
			FDictionary : ISettingSections;
			FStreamReader : IShared<TStreamReader>;
			FStreamWriter : IShared<TStreamWriter>;

		public
			constructor Create(_sr : IShared<TStreamReader>; _sw : IShared<TStreamWriter>; _dict : ISettingSections); overload;

			procedure LoadFromStream(_stream : TStream);
			procedure SaveToStream(_stream : TStream);
			procedure LoadFromStreamReader(_sr : TStreamReader);
			procedure SaveToStreamWriter(_sw : TStreamWriter);
	end;

implementation

uses
	RipGrepper.Settings.SettingVariant, RipGrepper.Tools.DebugUtils,
  RipGrepper.Helper.StreamReaderWriter;

{ TDictionaryStreamPersister }

procedure TDictionaryStreamPersister.LoadFromStream(_stream : TStream);
begin

end;

procedure TDictionaryStreamPersister.LoadFromStreamReader(_sr : TStreamReader);
var
	key : string;
	keyCount : Integer;
	section : string;
	sectionCount : Integer;
	keyDict : ISettingKeys;
	setting : ISetting;
	settingType : TSettingType;
	settingValue : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDictionaryStreamPersister.LoadFromStreamReader');
	FDictionary.Clear();
	sectionCount := _sr.ReadLineAsInteger;
	dbgMsg.MsgFmt('SectionCount: %d', [sectionCount]);
	for var i : integer := 0 to sectionCount - 1 do begin
		section := _sr.ReadLineAsString(false, 'SettingsDictionary.SectionName'); // Section names should not be empty
		dbgMsg.MsgFmt('Section: %s', [section]);
		keyDict := TCollections.CreateSortedDictionary<TSettingKey, ISetting>();
		FDictionary.Add(section, keyDict);
		keyCount := _sr.ReadLineAsInteger;
		dbgMsg.MsgFmt('KeyCount: %d', [keyCount]);
		for var j : integer := 0 to keyCount - 1 do begin
			key := _sr.ReadLineAsString(false); // Setting keys should not be empty
			dbgMsg.MsgFmt('Key: %s', [key]);
			settingType := TSettingType(_sr.ReadLineAsInteger);
			dbgMsg.MsgFmt('KeyType: %d', [Integer(settingType)]);
			settingValue := _sr.ReadLineAsString(true); // Setting values can be empty
			dbgMsg.MsgFmt('KeyValue: %s', [settingValue]);
			case settingType of
				stString :
				setting := TStringSetting.Create(key, settingValue);
				stInteger :
				setting := TIntegerSetting.Create(key, StrToIntDef(settingValue, 0));
				stBool :
				setting := TBoolSetting.Create(key, settingValue <> '0');
				stStrArray :
				begin
					var arr := TArraySetting.Create(key);
					if not settingValue.IsEmpty then begin
						var parts := settingValue.Split([',']);
						for var part in parts do begin
							arr.Value.Add(part);
						end;
					end;
					setting := arr;
				end;
			else
				raise ESettingsException.CreateFmt('Unsupported setting type: %d', [Integer(settingType)]);
			end;
			FDictionary[section].Add(key, setting);
		end;
	end;
end;

procedure TDictionaryStreamPersister.SaveToStream(_stream : TStream);
begin

end;

procedure TDictionaryStreamPersister.SaveToStreamWriter(_sw : TStreamWriter);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDictionaryStreamPersister.SaveToStreamWriter');

	dbgMsg.MsgFmt('Count: %d', [FDictionary.Count]);
	_sw.WriteLineAsInteger(FDictionary.Count);
	for var section in FDictionary.Keys do begin
		dbgMsg.MsgFmt('Section: %s', [section]);
		_sw.WriteLineAsString(section, false, 'SettingsDictionary.SectionName');
		dbgMsg.MsgFmt('Count: %d', [FDictionary[section].Count]);
		_sw.WriteLineAsInteger(FDictionary[section].Count);
		for var key in FDictionary[section].Keys do begin
			var
			setting := FDictionary[section][key];
			dbgMsg.MsgFmt('Key: %s', [key]);
			_sw.WriteLineAsString(key, false, 'SettingsDictionary.KeyName');
			dbgMsg.MsgFmt('Type: %d', [Integer(setting.SettingType)]);
			_sw.WriteLineAsInteger(Integer(setting.SettingType));
			dbgMsg.MsgFmt('Value: %s', [setting.AsString]);
			_sw.WriteLineAsString(setting.AsString, false, 'SettingsDictionary.KeyValue');
		end;
	end;
end;

constructor TDictionaryStreamPersister.Create( { } _sr : IShared<TStreamReader>;
	{ } _sw : IShared<TStreamWriter>; { } _dict : ISettingSections);
begin
	inherited Create();
	FStreamReader := _sr;
	FStreamWriter := _sw;
	FDictionary := _dict;
end;

end.
