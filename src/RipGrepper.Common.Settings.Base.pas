unit RipGrepper.Common.Settings.Base;

interface

uses
	System.Generics.Defaults,
	System.IniFiles,
	System.Generics.Collections;

type

	ISettingsPersister = interface
		['{A841C46D-56AF-4391-AB88-4C9496589FF4}']
		procedure Load;
		procedure Store;
	end;

	TRipGrepperSetting = record
		ValueType : integer;
		DefaultValue : Variant;
		Value : Variant;
		IsModified : Boolean;

		class function New(const _type : Integer; const _v : Variant) : TRipGrepperSetting; static;
	end;

	TSettingsDictionary = TDictionary<string, TRipGrepperSetting>;

	TRipGrepperSettingsBase = class(TSingletonImplementation, ISettingsPersister)
		private
			FSettings : TSettingsDictionary;
			function GetIniFile : TIniFile;
			procedure SetIniFile(const Value : TIniFile);
			procedure SetIsModified(const Value : Boolean);

		protected
			FIniFile : TIniFile;
			FIsLoaded : Boolean;
			FIsModified : Boolean;

			procedure CreateSetting(const _sName : string; const _setting : TRipGrepperSetting);
			function GetIsLoaded : Boolean; virtual;
			function GetIsModified : Boolean; virtual;
			procedure Init; virtual;
			procedure Load; virtual;
			procedure StoreSetting(const _name : string; const _v : Variant);
			function LoadSetting(const _name : string) : Variant;
			function LoadDefaultSetting(const _name : string) : Variant;
			procedure Store; virtual;

		public
			constructor Create(const _ini : TIniFile);
			destructor Destroy; override;
			function GetIniSectionName : string; virtual; abstract;
			property IniFile : TIniFile read GetIniFile write SetIniFile;
			property IsLoaded : Boolean read GetIsLoaded;
			property IsModified : Boolean read GetIsModified write SetIsModified;
	end;

implementation

uses
	System.Classes,
	RipGrepper.Tools.DebugUtils;

constructor TRipGrepperSettingsBase.Create(const _ini : TIniFile);
begin
	inherited Create();
	FIniFile := _ini;
	FIsModified := False;
	FIsLoaded := False;
	FSettings := TSettingsDictionary.Create();
end;

destructor TRipGrepperSettingsBase.Destroy;
begin
	FSettings.Free;
	inherited;
end;

procedure TRipGrepperSettingsBase.CreateSetting(const _sName : string; const _setting : TRipGrepperSetting);
begin
	FSettings.Add(_sName, _setting);
end;

function TRipGrepperSettingsBase.GetIniFile : TIniFile;
begin
	Result := FIniFile;
end;

function TRipGrepperSettingsBase.GetIsLoaded : Boolean;
begin
	Result := FIsLoaded;
end;

function TRipGrepperSettingsBase.GetIsModified : Boolean;
begin
	Result := FIsModified;
end;

procedure TRipGrepperSettingsBase.Init;
begin
	//
end;

procedure TRipGrepperSettingsBase.Load;
var
	strs : TStringList;
	name : string;
	value : string;
	setting : TRipGrepperSetting;
begin
	Init();
	strs := TStringList.Create();
	try
		FIniFile.ReadSectionValues(GetIniSectionName, strs);
		for var i : integer := 0 to strs.Count - 1 do begin
			name := strs.Names[i];
			value := strs.Values[name];
			setting := FSettings[name];
			setting.Value := value;
			FSettings.AddOrSetValue(name, setting);
		end;
	finally
		strs.Free;
	end;
end;

procedure TRipGrepperSettingsBase.SetIniFile(const Value : TIniFile);
begin
	if Assigned(FIniFile) then
		FIniFile.Free;
	FIniFile := Value;
end;

procedure TRipGrepperSettingsBase.SetIsModified(const Value : Boolean);
begin
	FIsModified := Value;
end;

procedure TRipGrepperSettingsBase.Store;
var
	setting : TRipGrepperSetting;
begin
	for var key in FSettings.Keys do begin
		setting := FSettings[key];
		if setting.IsModified then begin
			case setting.ValueType of
				vtString :
				FIniFile.WriteString(GetIniSectionName, key, setting.Value);
				vtBoolean :
				FIniFile.WriteBool(GetIniSectionName, key, setting.Value);
				vtInteger :
				FIniFile.WriteInteger(GetIniSectionName, key, setting.Value);
			end;
			TDebugUtils.DebugMessage('TRipGrepperSettingsBase.Store: ' + key + ' stored');
		end;
	end;
end;

procedure TRipGrepperSettingsBase.StoreSetting(const _name : string; const _v : Variant);
var
	setting : TRipGrepperSetting;
begin
	setting := FSettings[_name];
	if setting.Value <> _v then begin
		setting.IsModified := True;
		FSettings.AddOrSetValue(_name, setting);
	end;
end;

function TRipGrepperSettingsBase.LoadSetting(const _name : string) : Variant;
var
	setting : TRipGrepperSetting;
begin
	setting := FSettings[_name];
	Result := setting.Value;
end;

function TRipGrepperSettingsBase.LoadDefaultSetting(const _name : string) : Variant;
var
	setting : TRipGrepperSetting;
begin
	setting := FSettings[_name];
	Result := setting.DefaultValue;
end;

class function TRipGrepperSetting.New(const _type : Integer; const _v : Variant) : TRipGrepperSetting;
begin
	Result.ValueType := _type;
	Result.Value := _v;
	Result.DefaultValue := _v;
	Result.IsModified := False;
end;

end.
