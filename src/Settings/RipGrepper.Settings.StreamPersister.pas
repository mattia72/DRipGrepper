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
	RipGrepper.Settings.SettingVariant;

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
	FDictionary.Clear();
	sectionCount := _sr.ReadLine().ToInteger;
	for var i : integer := 0 to sectionCount - 1 do begin
		section := _sr.ReadLine();
		keyDict := TCollections.CreateSortedDictionary<TSettingKey, ISetting>();
		FDictionary.Add(section, keyDict);
		keyCount := _sr.ReadLine().ToInteger;
		for var j : integer := 0 to keyCount - 1 do begin
			key := _sr.ReadLine();
			settingType := TSettingType(_sr.ReadLine().ToInteger());
			settingValue := _sr.ReadLine();
			case settingType of
				stString :
				setting := TStringSetting.Create(settingValue);
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
	_sw.WriteLine(FDictionary.Count);
	for var section in FDictionary.Keys do begin
		_sw.WriteLine(section);
		_sw.WriteLine(FDictionary[section].Count);
		for var key in FDictionary[section].Keys do begin
			var
			setting := FDictionary[section][key];
			_sw.WriteLine(key);
			_sw.WriteLine(Integer(setting.SettingType));
			_sw.WriteLine(setting.AsString);
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
