unit RipGrepper.Settings.TestOwnerSettings;

interface

uses
	RipGrepper.Settings.Persistable,
	RipGrepper.Settings.SettingsDictionary;

type
	TTestOwnerSettings = class(TPersistableSettings)
		private
			FStrSetting : string;

		public
			constructor Create(const _Owner : TPersistableSettings); overload;
			constructor Create(const _iniSection : string); overload;
			function GetDict : TSettingsDictionary;
			procedure Init; override;
			procedure ReadIni; override;
			procedure LoadFromDict(); override;
			procedure LoadDefaultsFromDict; override;
			procedure StoreToDict; override;
			procedure StoreAsDefaultsToDict; override;

			property StrSetting : string read FStrSetting write FStrSetting;
	end;

implementation

constructor TTestOwnerSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := 'dummy owner'; // should be set before create
	inherited Create(_Owner);
end;

constructor TTestOwnerSettings.Create(const _iniSection : string);
begin
	IniSectionName := _iniSection; // as in  GUISearchTextParams
	inherited Create();
end;

function TTestOwnerSettings.GetDict : TSettingsDictionary;
begin
	Result := FSettingsDict;
end;

procedure TTestOwnerSettings.Init;
begin
//
end;

procedure TTestOwnerSettings.ReadIni;
begin
	inherited ReadIni;
end;

procedure TTestOwnerSettings.LoadFromDict;
begin
//
end;

procedure TTestOwnerSettings.LoadDefaultsFromDict;
begin
//
end;

procedure TTestOwnerSettings.StoreToDict;
begin
//
end;

procedure TTestOwnerSettings.StoreAsDefaultsToDict;
begin
//
end;

end.
