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
			DATE_FILTER_MODE = 'Date';
			FILTER_MODES : array of string = [FILE_FILTER_MODE, TEXT_FILTER_MODE, DATE_FILTER_MODE];
			SETTING_FILTERMODE = 'FilterMode';
			SETTING_CASE_SENSITIVE = 'FilterMode.CaseSensitive';
			SETTING_USE_REGEX = 'FilterMode.UseRegex';
			SETTING_DATE_FROM = 'FilterMode.DateFrom';
			SETTING_DATE_TO = 'FilterMode.DateTo';
			SETTING_DATE_TIME_TYPE = 'FilterMode.DateTimeType';

		const
			VIEW_SETTINGS : array [0 .. 5] of string = (
				{ } TFilterSettings.SETTING_FILTERMODE,
				{ } TFilterSettings.SETTING_CASE_SENSITIVE,
				{ } TFilterSettings.SETTING_USE_REGEX,
				{ } TFilterSettings.SETTING_DATE_FROM,
				{ } TFilterSettings.SETTING_DATE_TO,
				{ } TFilterSettings.SETTING_DATE_TIME_TYPE);

		private
			FChosenFilterMode : IStringSetting;
			FFilterModes : TFilterModes;
			FIsCaseSensitive : IBoolSetting;
			FIsUseRegex : IBoolSetting;
			FDateFrom : IStringSetting;
			FDateTo : IStringSetting;
			FDateTimeType : IStringSetting;
			function GetChosenFilterMode() : string;
			function GetFilterModes : TFilterModes;
			function GetIsCaseSensitive() : Boolean;
			function GetIsUseRegex() : Boolean;
			function GetDateFrom() : TDateTime;
			function GetDateTo() : TDateTime;
			function GetDateTimeType() : EDateTimeType;
			procedure SetChosenFilterMode(const Value : string);
			procedure SetFilterModes(const Value : TFilterModes);
			procedure SetIsCaseSensitive(const Value : Boolean);
			procedure SetIsUseRegex(const Value : Boolean);
			procedure SetDateFrom(const Value : TDateTime);
			procedure SetDateTo(const Value : TDateTime);
			procedure SetDateTimeType(const Value : EDateTimeType);

		protected

		public
			constructor Create(const _Owner : TPersistableSettings); overload;
			constructor Create; overload;
			destructor Destroy; override;
			procedure Init; override;
			procedure LoadFromDict(); override;
			property ChosenFilterMode : string read GetChosenFilterMode write SetChosenFilterMode;
			property FilterModes : TFilterModes read GetFilterModes write SetFilterModes;
			property IsCaseSensitive : Boolean read GetIsCaseSensitive write SetIsCaseSensitive;
			property IsUseRegex : Boolean read GetIsUseRegex write SetIsUseRegex;
			property DateFrom : TDateTime read GetDateFrom write SetDateFrom;
			property DateTo : TDateTime read GetDateTo write SetDateTo;
			property DateTimeType : EDateTimeType read GetDateTimeType write SetDateTimeType;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.StrUtils,
	RipGrepper.Helper.Types,
	System.SysUtils,
	System.DateUtils,
	ArrayEx,
	RipGrepper.Settings.SettingsDictionary;

const
	DATE_FORMAT_ISO = 'yyyy-mm-dd hh:nn:ss';

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
	var
		modes : TArrayEx<string> := FILTER_MODES;
	var
	idx := modes.IndexOf(ChosenFilterMode);
	case idx of
		0 : begin
			Exclude(FFilterModes, EFilterMode.fmFilterText);
			Exclude(FFilterModes, EFilterMode.fmFilterDate);
			Include(FFilterModes, EFilterMode.fmFilterFile)
		end;
		1 : begin
			Exclude(FFilterModes, EFilterMode.fmFilterFile);
			Exclude(FFilterModes, EFilterMode.fmFilterDate);
			Include(FFilterModes, EFilterMode.fmFilterText);
		end;
		2 : begin
			Exclude(FFilterModes, EFilterMode.fmFilterFile);
			Exclude(FFilterModes, EFilterMode.fmFilterText);
			Include(FFilterModes, EFilterMode.fmFilterDate);
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

function TFilterSettings.GetDateFrom() : TDateTime;
var
	s : string;
	fs : TFormatSettings;
begin
	s := FDateFrom.Value;
	if s <> '' then begin
		fs := TFormatSettings.Create;
		fs.DateSeparator := '-';
		fs.TimeSeparator := ':';
		fs.ShortDateFormat := 'yyyy-mm-dd';
		fs.LongTimeFormat := 'hh:nn:ss';
		Result := StrToDateTimeDef(s, 0, fs);
	end else begin
		Result := 0;
	end;
end;

function TFilterSettings.GetDateTo() : TDateTime;
var
	s : string;
	fs : TFormatSettings;
begin
	s := FDateTo.Value;
	if s <> '' then begin
		fs := TFormatSettings.Create;
		fs.DateSeparator := '-';
		fs.TimeSeparator := ':';
		fs.ShortDateFormat := 'yyyy-mm-dd';
		fs.LongTimeFormat := 'hh:nn:ss';
		Result := StrToDateTimeDef(s, 0, fs);
	end else begin
		Result := 0;
	end;
end;

function TFilterSettings.GetDateTimeType() : EDateTimeType;
var
	s : string;
begin
	s := FDateTimeType.Value;
	for var dtt := Low(EDateTimeType) to High(EDateTimeType) do begin
		if SameText(DATE_TIME_TYPE_KEYS[dtt], s) then begin
			Exit(dtt);
		end;
	end;
	Result := EDateTimeType.dttLastWrite;
end;

procedure TFilterSettings.SetDateFrom(const Value : TDateTime);
begin
	if Value > 0 then begin
		FDateFrom.Value := FormatDateTime(DATE_FORMAT_ISO, Value);
	end else begin
		FDateFrom.Value := '';
	end;
end;

procedure TFilterSettings.SetDateTo(const Value : TDateTime);
begin
	if Value > 0 then begin
		FDateTo.Value := FormatDateTime(DATE_FORMAT_ISO, Value);
	end else begin
		FDateTo.Value := '';
	end;
end;

procedure TFilterSettings.SetDateTimeType(const Value : EDateTimeType);
begin
	FDateTimeType.Value := DATE_TIME_TYPE_KEYS[Value];
end;

procedure TFilterSettings.Init;
begin
	FChosenFilterMode := TStringSetting.Create('FilterMode', FILE_FILTER_MODE);
	FIsCaseSensitive := TBoolSetting.Create('FilterMode.CaseSensitive', False);
	FIsUseRegex := TBoolSetting.Create('FilterMode.UseRegex', False);
	FDateFrom := TStringSetting.Create('FilterMode.DateFrom', '');
	FDateTo := TStringSetting.Create('FilterMode.DateTo', '');
	FDateTimeType := TStringSetting.Create('FilterMode.DateTimeType', 'LastWrite');

	CreateSetting(FChosenFilterMode);
	CreateSetting(FIsCaseSensitive);
	CreateSetting(FIsUseRegex);
	CreateSetting(FDateFrom);
	CreateSetting(FDateTo);
	CreateSetting(FDateTimeType);
end;

procedure TFilterSettings.LoadFromDict;
begin
	inherited;
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
	end else if EFilterMode.fmFilterDate in Value then begin
		ChosenFilterMode := DATE_FILTER_MODE;
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

end.
