unit RipGrepper.Settings.NodeLook.FilterSettings;

interface

uses
	RipGrepper.Settings.Persistable,
	System.IniFiles,
	RipGrepper.Common.SimpleTypes;

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
			FChosenFilterMode : string;
			FFilterModes : TFilterModes;
			FIsCaseSensitive : Boolean;
			FIsUseRegex : Boolean;
			function GetFilterModes : TFilterModes;
			procedure SetChosenFilterMode(const Value : string);
			procedure SetFilterModes(const Value : TFilterModes);
			procedure SetIsCaseSensitive(const Value : Boolean);
			procedure SetIsUseRegex(const Value : Boolean);

		protected

		public
			constructor Create(const _ini : TMemIniFile); overload;
			constructor Create; overload;
			destructor Destroy; override;
			procedure Init; override;
			procedure LoadFromDict(); override;
			procedure LoadDefaultsFromDict; override;
			procedure SetViewSettingValues(const _s : string = '');
			procedure StoreToDict; override;
			property ChosenFilterMode : string read FChosenFilterMode write SetChosenFilterMode;
			property FilterModes : TFilterModes read GetFilterModes write SetFilterModes;
			property IsCaseSensitive : Boolean read FIsCaseSensitive write SetIsCaseSensitive;
			property IsUseRegex : Boolean read FIsUseRegex write SetIsUseRegex;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.StrUtils,
	RipGrepper.Helper.Types,
	System.SysUtils,
	ArrayEx;

constructor TFilterSettings.Create(const _ini : TMemIniFile);
begin
	IniSectionName := INI_SECTION;
	inherited;
	TDebugUtils.DebugMessage('TFilterSettings.Create: ' + FIniFile.FileName + '[' + GetIniSectionName + ']');
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

procedure TFilterSettings.Init;
begin
	SettingsDict.CreateSetting('FilterMode', varString, FILE_FILTER_MODE);
	SettingsDict.CreateSetting('FilterMode.CaseSensitive', varBoolean, False);
	SettingsDict.CreateSetting('FilterMode.UseRegex', varBoolean, False);
end;

procedure TFilterSettings.LoadFromDict;
begin
	ChosenFilterMode := SettingsDict.GetSetting('FilterMode');
	IsCaseSensitive := SettingsDict.GetSetting('FilterMode.CaseSensitive');
	IsUseRegex := SettingsDict.GetSetting('FilterMode.UseRegex');
	GetFilterModes();
end;

procedure TFilterSettings.LoadDefaultsFromDict;
begin
	//
end;

procedure TFilterSettings.SetChosenFilterMode(const Value : string);
begin
	FChosenFilterMode := Value;
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
	FIsCaseSensitive := Value;
end;

procedure TFilterSettings.SetIsUseRegex(const Value : Boolean);
begin
	FIsUseRegex := Value;
end;

procedure TFilterSettings.SetViewSettingValues(const _s : string = '');
var
	i : integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TFilterSettings.SetViewSettingValues');
	i := 0;
	if _s.IsEmpty then begin
		// StoreToDict all
		for i := 0 to high(VIEW_SETTINGS) do begin
			SetViewSettingValues(VIEW_SETTINGS[i]);
		end;
	end else if MatchStr(_s, VIEW_SETTINGS[i]) then begin
		SettingsDict.SetSettingValue(VIEW_SETTINGS[i], ChosenFilterMode);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		SettingsDict.SetSettingValue(VIEW_SETTINGS[i], IsCaseSensitive);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		SettingsDict.SetSettingValue(VIEW_SETTINGS[i], IsUseRegex);
	end else begin
		raise Exception.Create('Settings: ' + _s + ' not stored!');
	end;
end;

procedure TFilterSettings.StoreToDict;
begin
	SettingsDict.StoreSetting('FilterMode', FChosenFilterMode);
	SettingsDict.StoreSetting('FilterMode.CaseSensitive', FIsCaseSensitive);
	SettingsDict.StoreSetting('FilterMode.UseRegex', FIsUseRegex);

	inherited StoreToDict();
end;

end.
