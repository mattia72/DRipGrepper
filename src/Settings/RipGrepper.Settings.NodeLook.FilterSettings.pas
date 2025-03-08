unit RipGrepper.Settings.NodeLook.FilterSettings;

interface

uses
	RipGrepper.Settings.Persistable,
	System.IniFiles,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Settings.SettingVariant;

type
	TFilterSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'NodeLookSettings';
			FILE_FILTER_MODE = 'File';
			TEXT_FILTER_MODE = 'Text';
			FILTER_MODES : array of string = [FILE_FILTER_MODE, TEXT_FILTER_MODE];
			SETTING_FILTERMODE = 'FilterMode';
			SETTING_CASE_SENSITIVE = 'FilterMode.CaseSensitive';
			SETTING_USE_REGEX = 'FilterMode.UseRegex';

		const
			VIEW_SETTINGS : array [0 .. 2] of string = (
				{ } TFilterSettings.SETTING_FILTERMODE,
				{ } TFilterSettings.SETTING_CASE_SENSITIVE,
				{ } TFilterSettings.SETTING_USE_REGEX);

		private
			FChosenFilterMode : TStringSetting;
			FFilterModes : TFilterModes;
			FIsCaseSensitive : TBoolSetting;
			FIsUseRegex : TBoolSetting;
			function GetChosenFilterMode() : string;
			function GetFilterModes : TFilterModes;
			function GetIsCaseSensitive() : Boolean;
			function GetIsUseRegex() : Boolean;
			procedure SetChosenFilterMode(const Value : string);
			procedure SetFilterModes(const Value : TFilterModes);
			procedure SetIsCaseSensitive(const Value : Boolean);
			procedure SetIsUseRegex(const Value : Boolean);

		protected

		public
			constructor Create(const _Owner : TPersistableSettings); overload;
			constructor Create; overload;
			destructor Destroy; override;
			procedure Init; override;
			procedure LoadFromDict(); override;
			procedure StoreToDict; override;
			property ChosenFilterMode : string read GetChosenFilterMode write SetChosenFilterMode;
			property FilterModes : TFilterModes read GetFilterModes write SetFilterModes;
			property IsCaseSensitive : Boolean read GetIsCaseSensitive write SetIsCaseSensitive;
			property IsUseRegex : Boolean read GetIsUseRegex write SetIsUseRegex;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.StrUtils,
	RipGrepper.Helper.Types,
	System.SysUtils,
	ArrayEx,
	RipGrepper.Settings.SettingsDictionary;

constructor TFilterSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := INI_SECTION;
	inherited Create(_Owner);
	TDebugUtils.DebugMessage('TFilterSettings.Create: ' + '[' + GetIniSectionName + ']');
end;

constructor TFilterSettings.Create;
begin
	IniSectionName := INI_SECTION;
	inherited Create;
end;

destructor TFilterSettings.Destroy;
begin
	inherited Destroy(); // ok;
end;

function TFilterSettings.GetChosenFilterMode() : string;
begin
	Result := FChosenFilterMode.Value;
end;

function TFilterSettings.GetFilterModes : TFilterModes;
begin
	var modes : TArrayEx<string> := FILTER_MODES;
	var
	idx := modes.IndexOf(ChosenFilterMode);
	case idx of
		0 : begin
			Exclude(FFilterModes, EFilterMode.fmFilterText);
			Include(FFilterModes, EFilterMode.fmFilterFile)
		end;
		1 : begin
			Exclude(FFilterModes, EFilterMode.fmFilterFile);
			Include(FFilterModes, EFilterMode.fmFilterText);
		end;
	end;

	if IsCaseSensitive then begin
		Include(FFilterModes, EFilterMode.fmCaseSensitive);
	end else begin
		Exclude(FFilterModes, EFilterMode.fmCaseSensitive);
	end;

	if IsUseRegex then begin
		Include(FFilterModes, EFilterMode.fmUseRegex);
	end else begin
		Exclude(FFilterModes, EFilterMode.fmUseRegex);
	end;

	Result := FFilterModes;
end;

function TFilterSettings.GetIsCaseSensitive() : Boolean;
begin
	Result := FIsCaseSensitive.Value;
end;

function TFilterSettings.GetIsUseRegex() : Boolean;
begin
	Result := FIsUseRegex.Value;
end;

procedure TFilterSettings.Init;
begin
	FChosenFilterMode := TStringSetting.Create(FILE_FILTER_MODE);
	FIsCaseSensitive := TBoolSetting.Create(False);
	FIsUseRegex := TBoolSetting.Create(False);

	CreateSetting('FilterMode', FChosenFilterMode);
	CreateSetting('FilterMode.CaseSensitive', FIsCaseSensitive);
	CreateSetting('FilterMode.UseRegex', FIsUseRegex);
end;

procedure TFilterSettings.LoadFromDict;
begin
	GetFilterModes();
end;

procedure TFilterSettings.SetChosenFilterMode(const Value : string);
begin
	FChosenFilterMode.Value := Value;
end;

procedure TFilterSettings.SetFilterModes(const Value : TFilterModes);
begin
	FFilterModes := Value;
	if EFilterMode.fmFilterFile in Value then begin
		ChosenFilterMode := FILE_FILTER_MODE;
	end else if EFilterMode.fmFilterText in Value then begin
		ChosenFilterMode := TEXT_FILTER_MODE;
	end;
	IsCaseSensitive := EFilterMode.fmCaseSensitive in Value;
	IsUseRegex := EFilterMode.fmUseRegex in Value;
	FIsModified := True;
end;

procedure TFilterSettings.SetIsCaseSensitive(const Value : Boolean);
begin
	FIsCaseSensitive.Value := Value;
end;

procedure TFilterSettings.SetIsUseRegex(const Value : Boolean);
begin
	FIsUseRegex.Value := Value;
end;

procedure TFilterSettings.StoreToDict;
begin
	inherited StoreToDict();
end;

end.
