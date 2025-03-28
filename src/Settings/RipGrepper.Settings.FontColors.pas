unit RipGrepper.Settings.FontColors;

interface

uses
	System.Classes,
	System.UITypes,
	Vcl.Graphics,
	RipGrepper.Settings.Persistable,
	System.IniFiles,
	RipGrepper.Helper.UI.DarkMode,
	Spring.Collections,
	RipGrepper.Settings.SettingsDictionary,
	RipGrepper.Settings.SettingVariant,
	ArrayEx;

type
	TFontStyleSet = set of TFontStyle;

	TFontAttributes = record
		Name : string;
		Size : integer;
		Color : TColor;
		BgColor : TColor;
		Style : TFontStyleSet;

		public
			procedure FromString(const _s : string);
			function FromFont(const _font : TFont) : TFontAttributes;
			function ToFont(var _font : TFont) : TFontAttributes;
			function ToString : string;
	end;

	TNameColorDef = record
		Name : string;
		ColorDef : string;
	end;

	TDefaultFontColors = class

		const
			RG_OPTIONS_TREEVIEW_SECTION_TITLE_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clPurple; BgColor : clNone;
				Style : [fsBold];);

			DEFAULT_COLORS : array [0 .. 13] of TNameColorDef = (
				{ } (name : 'MatchText'; ColorDef : 'Segoe UI|9|clMaroon|clNone|fsBold|fsUnderline'),
				{ } (name : 'SearchTextInHistory'; ColorDef : 'Segoe UI|9|clMaroon|clNone|fsBold'),
				{ } (name : 'ReplacedTextInHistory'; ColorDef : 'Segoe UI|9|clMaroon|clNone|fsBold|fsStrikeOut'),
				{ } (name : 'NormalText'; ColorDef : 'Segoe UI|9|clBlack|clNone'),
				{ } (name : 'CounterText'; ColorDef : 'Segoe UI|9|clPurple|clNone'),
				{ } (name : 'ReplacedText'; ColorDef : 'Segoe UI|9|clWhite|clMaroon|fsBold|fsStrikeOut'),
				{ } (name : 'ErrorText'; ColorDef : 'Segoe UI|9|clRed|clNone'),
				{ } (name : 'StatisticsText'; ColorDef : 'Segoe UI|9|clHotLight|clNone|fsBold'),
				{ } (name : 'ReplaceText'; ColorDef : 'Segoe UI|9|clWhite|clGreen|fsBold'),
				{ } (name : 'FileText'; ColorDef : 'Segoe UI|9|clWindowFrame|clNone|fsBold'),
				{ } (name : 'ReplaceTextInHistory'; ColorDef : 'Segoe UI|9|clGreen|clNone|fsBold'),
				{ } (name : 'LineNumText'; ColorDef : 'Segoe UI|9|clGrayText|clNone'),
				{ } (name : 'ColNumText'; ColorDef : 'Segoe UI|9|clGrayText|clNone'),
				{ } (name : 'AlternateRow'; ColorDef : 'Segoe UI|9|clNone|cl3DLight'));

			DEFAULT_DARK_COLORS : array [0 .. 13] of TNameColorDef = (
				{ } (name : 'MatchText'; ColorDef : 'Segoe UI|9|$00558CFF|clNone|fsBold|fsUnderline'),
				{ } (name : 'SearchTextInHistory'; ColorDef : 'Segoe UI|9|$004080FF|clNone|fsBold'),
				{ } (name : 'ReplacedTextInHistory'; ColorDef : 'Segoe UI|9|clMaroon|clNone|fsBold|fsStrikeOut'),
				{ } (name : 'NormalText'; ColorDef : 'Segoe UI|9|clBtnHighlight|clNone'),
				{ } (name : 'CounterText'; ColorDef : 'Segoe UI|9|clFuchsia|clNone'),
				{ } (name : 'ReplacedText'; ColorDef : 'Segoe UI|9|clWhite|clMaroon|fsBold|fsStrikeOut'),
				{ } (name : 'ErrorText'; ColorDef : 'Segoe UI|9|clRed|clNone'),
				{ } (name : 'StatisticsText'; ColorDef : 'Segoe UI|9|clHotLight|clNone|fsBold'),
				{ } (name : 'ReplaceText'; ColorDef : 'Segoe UI|9|clWhite|clGreen|fsBold'),
				{ } (name : 'FileText'; ColorDef : 'Segoe UI|9|clSkyBlue|clNone|fsBold'),
				{ } (name : 'ReplaceTextInHistory'; ColorDef : 'Segoe UI|9|clGreen|clNone|fsBold'),
				{ } (name : 'LineNumText'; ColorDef : 'Segoe UI|9|clGray|clNone'),
				{ } (name : 'ColNumText'; ColorDef : 'Segoe UI|9|clGray|clNone'),
				{$IFDEF STANDALONE}
				{ } (name : 'AlternateRow'; ColorDef : 'Segoe UI|9|clNone|$002B2B2B'));
			{$ELSE}
				{ } (name : 'AlternateRow'; ColorDef : 'Segoe UI|9|clNone|$00404040'));
		{$ENDIF}

		private
			FTheme : EThemeMode;

		public

		var
			HIST_TREEVIEW_SEARCH_TEXT : TFontAttributes;
			HIST_TREEVIEW_REPLACE_TEXT : TFontAttributes;
			HIST_TREEVIEW_REPLACED_TEXT : TFontAttributes;

			TREEVIEW_MATCH_TEXT : TFontAttributes;
			TREEVIEW_REPLACE_TEXT : TFontAttributes;
			TREEVIEW_REPLACED_TEXT : TFontAttributes;

			TREEVIEW_FILE_TEXT : TFontAttributes;
			TREEVIEW_NORMAL_TEXT : TFontAttributes;

			TREEVIEW_LINE_NUM_TEXT : TFontAttributes;
			TREEVIEW_COL_NUM_TEXT : TFontAttributes;
			TREEVIEW_ERROR_TEXT : TFontAttributes;
			TREEVIEW_STATS_TEXT : TFontAttributes;
			TREEVIEW_STAT_TEXT : TFontAttributes; //
			TREEVIEW_ALTERNATE_ROW : TFontAttributes;

			constructor Create(

				const _theme : EThemeMode = EThemeMode.tmLight);
			procedure SetDefault(

				const _name : string;

				var _color : TFontAttributes);
	end;

	TFontColors = record
		private
		public
			FileText : TFontAttributes; // <-- First in config form
			LineNumText : TFontAttributes;
			ColNumText : TFontAttributes;
			NormalText : TFontAttributes;
			MatchText : TFontAttributes;
			CounterText : TFontAttributes;
			ErrorText : TFontAttributes;
			StatisticsText : TFontAttributes;
			ReplaceText : TFontAttributes;
			ReplacedText : TFontAttributes;
			SearchTextInHistory : TFontAttributes;
			ReplaceTextInHistory : TFontAttributes;
			ReplacedTextInHistory : TFontAttributes;
			AlternateRow : TFontAttributes;
			procedure SetByName(const _name : string;

				const _fa : TFontAttributes);
			function GetByName(

				const _name : string) : TFontAttributes;
			function IsEmpty : Boolean;
			procedure SetDefaultColors(

				const _df : TDefaultFontColors);
	end;

	TColorSettings = class(TPersistableSettings)
		const INI_SECTION = 'ColorSettings';

		private
			FFontColors : TFontColors;
			FFontColorsSettings : ISettingKeys;
			procedure CopyFontColorsToSettings();
			procedure CopySettingsToFontColors();

		protected
			function GetIsAlreadyRead : Boolean; override;
			procedure Init; override;

		public
			constructor Create(

				const _Owner : TPersistableSettings);
			destructor Destroy; override;
			procedure LoadFromDict(); override;
			procedure LoadDefaultColors(

				const _theme : EThemeMode);
			procedure ReadFile(); override;
			procedure ReloadColors;
			procedure StoreToPersister(); override;
			property FontColors : TFontColors read FFontColors write FFontColors;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.SysUtils,
	RipGrepper.Common.Constants,
	RipGrepper.Helper.Types,

	System.Rtti,
	System.Generics.Defaults,
	Spring;

{ TColorSettings }

constructor TColorSettings.Create(

	const _Owner : TPersistableSettings);
begin
	IniSectionName := INI_SECTION;
	inherited;
	TDebugUtils.DebugMessage('TColorSettings.Create: ' + '[' + IniSectionName + ']');
end;

destructor TColorSettings.Destroy;
begin
	inherited Destroy() // ok;
end;

procedure TColorSettings.CopyFontColorsToSettings();
begin
	for var s in TDefaultFontColors.DEFAULT_COLORS do begin
		var
		setting := TStringSetting.Create(FontColors.GetByName(s.Name).ToString);
		TStringSetting.CopySettingValue(setting, FFontColorsSettings[s.Name]);
		FFontColorsSettings[s.Name].StoreToPersister();
	end;
end;

procedure TColorSettings.CopySettingsToFontColors();
begin
	for var s in TDefaultFontColors.DEFAULT_COLORS do begin
		var fc : TFontAttributes;
		fc.FromString(FFontColorsSettings[s.Name].AsString);
		FontColors.SetByName(s.Name, fc);
	end;
end;

function TColorSettings.GetIsAlreadyRead : Boolean;
begin
	Result := inherited;
end;

procedure TColorSettings.Init;
begin
	FFontColorsSettings := TCollections.CreateSortedDictionary<string, ISetting>();

	for var s in TDefaultFontColors.DEFAULT_COLORS do begin
		FFontColorsSettings.Add(s.Name, TStringSetting.Create(''));
	end;

	for var pair in FFontColorsSettings do begin
		CreateSetting(pair.key, pair.Value);
	end;

	LoadDefaultColors(TDarkModeHelper.GetActualThemeMode);
	CopyFontColorsToSettings;
end;

procedure TColorSettings.LoadDefaultColors(

	const _theme : EThemeMode);
begin
	var
	df := TDefaultFontColors.Create(_theme);
	try
		FFontColors.SetDefaultColors(df);

		CopyFontColorsToSettings;
	finally
		df.Free;
	end;
end;

procedure TColorSettings.LoadFromDict;
begin
	CopySettingsToFontColors;
end;

procedure TColorSettings.ReadFile();
var bLoaded : Boolean;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TColorSettings.ReadFile');
	bLoaded := True;

	for var pair in FFontColorsSettings do begin
		pair.Value.LoadFromPersister;
		if pair.Value.AsString.IsEmpty then begin
			bLoaded := False;
			break;
		end;
	end;

	if not bLoaded then begin
		if FFontColors.IsEmpty then begin
			LoadDefaultColors(TDarkModeHelper.GetActualThemeMode);
		end;

		CopyFontColorsToSettings();
	end;
end;

procedure TColorSettings.ReloadColors;
begin
	ReLoad;
	// LoadFromDict;

	if FFontColors.IsEmpty then begin
		LoadDefaultColors(TDarkModeHelper.GetActualThemeMode);
	end;
end;

procedure TColorSettings.StoreToPersister();
begin
	CopyFontColorsToSettings;

end;

procedure TFontAttributes.FromString(

	const _s : string);
var i : integer; arr : TArrayEx<string>; sEnumName : string; st : TFontStyle;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TFontAttributes.FromString');

	i := 0;
	arr := _s.Split([ARRAY_SEPARATOR]);
	if arr.Count >= 4 then begin
		name := arr[PostInc(i)];
		Size := StrToIntDef(arr[PostInc(i)], 0);
		Color := StringToColor(arr[PostInc(i)]);
		BgColor := StringToColor(arr[PostInc(i)]);
		Style := [];

		while i < arr.Count do begin
			sEnumName := arr[PostInc(i)].Trim();
			st := TRttiEnumerationType.GetValue<TFontStyle>(sEnumName);
			Style := Style + [st];
		end;
	end else begin
		raise Exception.CreateFmt('Can''t convert string to TFontAttributes: %s', [_s]);
	end;
	dbgMsg.Msg(self.ToString);
end;

function TFontAttributes.FromFont(

	const _font : TFont) : TFontAttributes;
begin
	name := _font.Name;
	Style := _font.Style;
	Size := _font.Size;
	Color := _font.Color;
end;

function TFontAttributes.ToFont(

	var _font : TFont) : TFontAttributes;
begin
	_font.Name := name;
	_font.Style := Style;
	_font.Size := Size;
	_font.Color := Color;
end;

function TFontAttributes.ToString : string;
var arrFontStyles : TArrayEx<string>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TFontAttributes.ToString');

	for var st : TFontStyle in Style do begin
		arrFontStyles.Add(TRttiEnumerationType.GetName(st));
	end;

	var sStyles : string := string.Join(ARRAY_SEPARATOR, arrFontStyles.Items);

	Result := string.Join(ARRAY_SEPARATOR, [
		{ } name,
		{ } IntToStr(Size),
		{ } ColorToString(Color),
		{ } ColorToString(BgColor),
		{ } sStyles]).TrimRight([ARRAY_SEPARATOR]);
	dbgMsg.Msg(Result);
end;

function TFontColors.IsEmpty : Boolean;
begin
	Result := NormalText.Color = NormalText.BgColor;
end;

procedure TFontColors.SetByName(const _name : string;

	const _fa : TFontAttributes);
begin
	if _name = 'MatchText' then
		MatchText := _fa
	else if _name = 'ReplaceText' then
		ReplaceText := _fa
	else if _name = 'ReplacedText' then
		ReplacedText := _fa
	else if _name = 'SearchTextInHistory' then
		SearchTextInHistory := _fa
	else if _name = 'ReplaceTextInHistory' then
		ReplaceTextInHistory := _fa
	else if _name = 'ReplacedTextInHistory' then
		ReplacedTextInHistory := _fa
	else if _name = 'NormalText' then
		NormalText := _fa
	else if _name = 'CounterText' then
		CounterText := _fa
	else if _name = 'ErrorText' then
		ErrorText := _fa
	else if _name = 'StatisticsText' then
		StatisticsText := _fa
	else if _name = 'FileText' then
		FileText := _fa
	else if _name = 'LineNumText' then
		LineNumText := _fa
	else if _name = 'ColNumText' then
		ColNumText := _fa
	else if _name = 'AlternateRow' then
		AlternateRow := _fa
	else
		raise Exception.Create('Unknown font attribute name: ' + _name);
end;

function TFontColors.GetByName(

	const _name : string) : TFontAttributes;
begin
	if _name = 'MatchText' then
		Result := MatchText
	else if _name = 'ReplaceText' then
		Result := ReplaceText
	else if _name = 'ReplacedText' then
		Result := ReplacedText
	else if _name = 'SearchTextInHistory' then
		Result := SearchTextInHistory
	else if _name = 'ReplaceTextInHistory' then
		Result := ReplaceTextInHistory
	else if _name = 'ReplacedTextInHistory' then
		Result := ReplacedTextInHistory
	else if _name = 'NormalText' then
		Result := NormalText
	else if _name = 'CounterText' then
		Result := CounterText
	else if _name = 'ErrorText' then
		Result := ErrorText
	else if _name = 'StatisticsText' then
		Result := StatisticsText
	else if _name = 'FileText' then
		Result := FileText
	else if _name = 'LineNumText' then
		Result := LineNumText
	else if _name = 'ColNumText' then
		Result := ColNumText
	else if _name = 'AlternateRow' then
		Result := AlternateRow
	else
		raise Exception.Create('Unknown font attribute name: ' + _name);
end;

procedure TFontColors.SetDefaultColors(

	const _df : TDefaultFontColors);
begin
	self.AlternateRow.FromString(_df.TREEVIEW_ALTERNATE_ROW.ToString());
	self.ColNumText.FromString(_df.TREEVIEW_COL_NUM_TEXT.ToString());
	self.CounterText.FromString(_df.TREEVIEW_STAT_TEXT.ToString());
	self.ErrorText.FromString(_df.TREEVIEW_ERROR_TEXT.ToString());
	self.FileText.FromString(_df.TREEVIEW_FILE_TEXT.ToString());
	self.LineNumText.FromString(_df.TREEVIEW_LINE_NUM_TEXT.ToString());
	self.MatchText.FromString(_df.TREEVIEW_MATCH_TEXT.ToString());
	self.NormalText.FromString(_df.TREEVIEW_NORMAL_TEXT.ToString());
	self.ReplaceText.FromString(_df.TREEVIEW_REPLACE_TEXT.ToString());
	self.ReplaceTextInHistory.FromString(_df.HIST_TREEVIEW_REPLACE_TEXT.ToString());
	self.ReplacedText.FromString(_df.TREEVIEW_REPLACED_TEXT.ToString());
	self.ReplacedTextInHistory.FromString(_df.HIST_TREEVIEW_REPLACED_TEXT.ToString());
	self.SearchTextInHistory.FromString(_df.HIST_TREEVIEW_SEARCH_TEXT.ToString());
	self.StatisticsText.FromString(_df.TREEVIEW_STATS_TEXT.ToString());
end;

constructor TDefaultFontColors.Create(

	const _theme : EThemeMode = EThemeMode.tmLight);
begin
	inherited Create();
	FTheme := _theme;
	SetDefault('MatchText', TREEVIEW_MATCH_TEXT);
	SetDefault('ReplaceText', TREEVIEW_REPLACE_TEXT);
	SetDefault('ReplacedText', TREEVIEW_REPLACED_TEXT);
	SetDefault('SearchTextInHistory', HIST_TREEVIEW_SEARCH_TEXT);
	SetDefault('ReplaceTextInHistory', HIST_TREEVIEW_REPLACE_TEXT);
	SetDefault('ReplacedTextInHistory', HIST_TREEVIEW_REPLACED_TEXT);
	SetDefault('NormalText', TREEVIEW_NORMAL_TEXT);
	SetDefault('CounterText', TREEVIEW_STAT_TEXT);
	SetDefault('ErrorText', TREEVIEW_ERROR_TEXT);
	SetDefault('StatisticsText', TREEVIEW_STATS_TEXT);
	SetDefault('ColNumText', TREEVIEW_COL_NUM_TEXT);
	SetDefault('LineNumText', TREEVIEW_LINE_NUM_TEXT);
	SetDefault('FileText', TREEVIEW_FILE_TEXT);
	SetDefault('AlternateRow', TREEVIEW_ALTERNATE_ROW);
end;

procedure TDefaultFontColors.SetDefault(const _name : string; var _color : TFontAttributes);
var
	dc : TArrayEx<TNameColorDef>;
	idx : Integer;
begin
	if FTheme = EThemeMode.tmLight then begin
		dc := DEFAULT_COLORS;
	end else begin
		dc := DEFAULT_DARK_COLORS;
	end;

	for idx := 0 to dc.MaxIndex do begin
		if dc[idx].Name = _name then begin
			break;
		end;
	end;
	var
	ac := dc[idx].ColorDef;
	_color.FromString(ac);
end;

end.
