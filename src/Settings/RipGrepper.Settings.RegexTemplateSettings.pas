unit RipGrepper.Settings.RegexTemplateSettings;

interface

uses
	RipGrepper.Common.Constants,
	RipGrepper.Settings.Persistable,
	RipGrepper.Settings.PersistableArray,
	RipGrepper.Settings.SettingVariant;

type
	TRegexTemplateSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'RegexTemplates';

		private
			FPersistableArray : TPersistableArray;
			function GetPersistableArray() : IPersistableArray;

		public
			constructor Create(const _owner : TPersistableSettings); overload;
			constructor Create; overload;
			procedure Init; override;
			procedure Copy(const _other : TRegexTemplateSettings); reintroduce;
			property PersistableArray : IPersistableArray read GetPersistableArray;
	end;

implementation

uses
	RipGrepper.Helper.SettingStoreBehaviours,
	RipGrepper.Tools.DebugUtils;

constructor TRegexTemplateSettings.Create(const _owner : TPersistableSettings);
begin
	IniSectionName := INI_SECTION;
	inherited Create(_owner);
end;

constructor TRegexTemplateSettings.Create;
begin
	IniSectionName := INI_SECTION;
	inherited Create;
end;

procedure TRegexTemplateSettings.Init;
var
	arrSetting : IArraySetting;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRegexTemplateSettings.Init');

	arrSetting :=
	{ } TArraySetting.Create(INI_SECTION, ssInitialized, [ssbStoreOnceEvenIfNotModified]);

	// Set default regex templates
	if arrSetting.Count = 0 then begin
		arrSetting.Add(SEARCH_AS_TYPE);
		arrSetting.Add(SEARCH_AS_DECLARATION);
		arrSetting.Add(SEARCH_AS_FUNCTION);
	end;

	CreateSetting(arrSetting.Name, ITEM_KEY_PREFIX, arrSetting);

	FPersistableArray := TPersistableArray.Create(INI_SECTION, arrSetting);
	AddChildSettings(FPersistableArray);
end;

procedure TRegexTemplateSettings.Copy(const _other : TRegexTemplateSettings);
begin
	if Assigned(_other) then begin
		inherited Copy(_other as TPersistableSettings);
		FPersistableArray.Copy(_other.PersistableArray);
	end;
end;

function TRegexTemplateSettings.GetPersistableArray() : IPersistableArray;
begin
	Result := FPersistableArray;
end;

end.
