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
	RipGrepper.Settings.SettingVariant;

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

	TDefaultFontColors = class
		const
			RG_OPTIONS_TREEVIEW_SECTION_TITLE_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clPurple; BgColor : clNone;
				Style : [fsBold];);

			DEFAULT_COLORS : array of string = [
			{ } 'MatchText=Segoe UI|9|clMaroon|clNone|fsBold|fsUnderline',
			{ } 'SearchTextInHistory=Segoe UI|9|clMaroon|clNone|fsBold',
			{ } 'ReplacedTextInHistory=Segoe UI|9|clMaroon|clNone|fsBold|fsStrikeOut',
			{ } 'NormalText=Segoe UI|9|clBlack|clNone',
			{ } 'CounterText=Segoe UI|9|clPurple|clNone',
			{ } 'ReplacedText=Segoe UI|9|clWhite|clMaroon|fsBold|fsStrikeOut',
			{ } 'ErrorText=Segoe UI|9|clRed|clNone',
			{ } 'StatisticsText=Segoe UI|9|clHotLight|clNone|fsBold',
			{ } 'ReplaceText=Segoe UI|9|clWhite|clGreen|fsBold',
			{ } 'FileText=Segoe UI|9|clWindowFrame|clNone|fsBold',
			{ } 'ReplaceTextInHistory=Segoe UI|9|clGreen|clNone|fsBold',
			{ } 'LineNumText=Segoe UI|9|clGrayText|clNone',
			{ } 'ColNumText=Segoe UI|9|clGrayText|clNone',
			{ } 'AlternateRow=Segoe UI|9|clNone|cl3DLight'];

			DEFAULT_DARK_COLORS : array of string = [
			{ } 'MatchText=Segoe UI|9|$00558CFF|clNone|fsBold|fsUnderline',
			{ } 'SearchTextInHistory=Segoe UI|9|$004080FF|clNone|fsBold',
			{ } 'ReplacedTextInHistory=Segoe UI|9|clMaroon|clNone|fsBold|fsStrikeOut',
			{ } 'NormalText=Segoe UI|9|clBtnHighlight|clNone',
			{ } 'CounterText=Segoe UI|9|clFuchsia|clNone',
			{ } 'ReplacedText=Segoe UI|9|clWhite|clMaroon|fsBold|fsStrikeOut',
			{ } 'ErrorText=Segoe UI|9|clRed|clNone',
			{ } 'StatisticsText=Segoe UI|9|clHotLight|clNone|fsBold',
			{ } 'ReplaceText=Segoe UI|9|clWhite|clGreen|fsBold',
			{ } 'FileText=Segoe UI|9|clSkyBlue|clNone|fsBold',
			{ } 'ReplaceTextInHistory=Segoe UI|9|clGreen|clNone|fsBold',
			{ } 'LineNumText=Segoe UI|9|clGray|clNone',
			{ } 'ColNumText=Segoe UI|9|clGray|clNone',
			{$IFDEF STANDALONE}
			{ } 'AlternateRow=Segoe UI|9|clNone|$002B2B2B'];
			{$ELSE}
			{ } 'AlternateRow=Segoe UI|9|clNone|$00404040'];
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

			constructor Create(const _theme : EThemeMode = EThemeMode.tmLight);
			procedure SetDefault(const _name : string; var _color : TFontAttributes);
	end;

	TFontColors = record
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
		procedure SetByName(const _name : string; const _fa : TFontAttributes);

		public
			function IsEmpty : Boolean;
	end;

	TColorSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'ColorSettings';

		private
			FFontColors : TFontColors;
			FFontColorsSettings : ISettingKeys;

		protected
			function GetIsAlreadyRead : Boolean; override;
			procedure Init; override;

		public
			constructor Create(const _Owner : TPersistableSettings);
			destructor Destroy; override;
			procedure LoadFromDict(); override;
			procedure LoadDefaultColors(const _theme : EThemeMode);
			procedure ReloadColors;
			property FontColors : TFontColors read FFontColors write FFontColors;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.SysUtils,
	RipGrepper.Common.Constants,
	RipGrepper.Helper.Types,
	ArrayEx,
	System.Rtti,
	System.Generics.Defaults,
	Spring;

{ TColorSettings }

constructor TColorSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := INI_SECTION;
	inherited;
	TDebugUtils.DebugMessage('TColorSettings.Create: ' + '[' + IniSectionName + ']');
end;

destructor TColorSettings.Destroy;
begin
	inherited Destroy() // ok;
end;

function TColorSettings.GetIsAlreadyRead : Boolean;
begin
	Result := inherited;
end;

procedure TColorSettings.Init;
begin
	LoadDefaultColors(TDarkModeHelper.GetActualThemeMode);
	FFontColorsSettings := TCollections.CreateSortedDictionary<string, ISetting>();

    var ss : TSettingState := ssModified;
	FFontColorsSettings.Add('AlternateRow', TStringSetting.Create(FFontColors.AlternateRow.ToString, ss));
	FFontColorsSettings.Add('ColNumText', TStringSetting.Create(FFontColors.ColNumText.ToString, ss));
	FFontColorsSettings.Add('CounterText', TStringSetting.Create(FFontColors.CounterText.ToString, ss));
	FFontColorsSettings.Add('ErrorText', TStringSetting.Create(FFontColors.ErrorText.ToString, ss));
	FFontColorsSettings.Add('FileText', TStringSetting.Create(FFontColors.FileText.ToString, ss));
	FFontColorsSettings.Add('LineNumText', TStringSetting.Create(FFontColors.LineNumText.ToString, ss));
	FFontColorsSettings.Add('MatchText', TStringSetting.Create(FFontColors.MatchText.ToString, ss));
	FFontColorsSettings.Add('NormalText', TStringSetting.Create(FFontColors.NormalText.ToString, ss));
	FFontColorsSettings.Add('ReplaceText', TStringSetting.Create(FFontColors.ReplaceText.ToString, ss));
	FFontColorsSettings.Add('ReplaceTextInHistory', TStringSetting.Create(FFontColors.ReplaceTextInHistory.ToString, ss));
	FFontColorsSettings.Add('ReplacedText', TStringSetting.Create(FFontColors.ReplacedText.ToString, ss));
	FFontColorsSettings.Add('ReplacedTextInHistory', TStringSetting.Create(FFontColors.ReplacedTextInHistory.ToString, ss));
	FFontColorsSettings.Add('SearchTextInHistory', TStringSetting.Create(FFontColors.SearchTextInHistory.ToString, ss));
	FFontColorsSettings.Add('StatisticsText', TStringSetting.Create(FFontColors.StatisticsText.ToString, ss));

	for var pair in FFontColorsSettings do begin
		CreateSetting(pair.key, pair.Value);
	end;
end;

procedure TColorSettings.LoadDefaultColors(const _theme : EThemeMode);
begin
	var
	df := TDefaultFontColors.Create(_theme);
	try
		FFontColors.AlternateRow.FromString(df.TREEVIEW_ALTERNATE_ROW.ToString());
		FFontColors.ColNumText.FromString(df.TREEVIEW_COL_NUM_TEXT.ToString());
		FFontColors.CounterText.FromString(df.TREEVIEW_STAT_TEXT.ToString());
		FFontColors.ErrorText.FromString(df.TREEVIEW_ERROR_TEXT.ToString());
		FFontColors.FileText.FromString(df.TREEVIEW_FILE_TEXT.ToString());
		FFontColors.LineNumText.FromString(df.TREEVIEW_LINE_NUM_TEXT.ToString());
		FFontColors.MatchText.FromString(df.TREEVIEW_MATCH_TEXT.ToString());
		FFontColors.NormalText.FromString(df.TREEVIEW_NORMAL_TEXT.ToString());
		FFontColors.ReplaceText.FromString(df.TREEVIEW_REPLACE_TEXT.ToString());
		FFontColors.ReplaceTextInHistory.FromString(df.HIST_TREEVIEW_REPLACE_TEXT.ToString());
		FFontColors.ReplacedText.FromString(df.TREEVIEW_REPLACED_TEXT.ToString());
		FFontColors.ReplacedTextInHistory.FromString(df.HIST_TREEVIEW_REPLACED_TEXT.ToString());
		FFontColors.SearchTextInHistory.FromString(df.HIST_TREEVIEW_SEARCH_TEXT.ToString());
		FFontColors.StatisticsText.FromString(df.TREEVIEW_STATS_TEXT.ToString());
	finally
		df.Free;
	end;
end;

procedure TColorSettings.LoadFromDict;
begin
	// FFontColors.MatchText.FromString(TStringSetting(SettingsDict.GetSetting('MatchText')).Value);
	// FFontColors.ReplaceText.FromString(TStringSetting(SettingsDict.GetSetting('ReplaceText')).Value);
	// FFontColors.ReplacedText.FromString(TStringSetting(SettingsDict.GetSetting('ReplacedText')).Value);
	// FFontColors.SearchTextInHistory.FromString(TStringSetting(SettingsDict.GetSetting('SearchTextInHistory')).Value);
	// FFontColors.ReplaceTextInHistory.FromString(TStringSetting(SettingsDict.GetSetting('ReplaceTextInHistory')).Value);
	// FFontColors.ReplacedTextInHistory.FromString(TStringSetting(SettingsDict.GetSetting('ReplacedTextInHistory')).Value);
	// FFontColors.NormalText.FromString(TStringSetting(SettingsDict.GetSetting('NormalText')).Value);
	// FFontColors.CounterText.FromString(TStringSetting(SettingsDict.GetSetting('CounterText')).Value);
	// FFontColors.ErrorText.FromString(TStringSetting(SettingsDict.GetSetting('ErrorText')).Value);
	// FFontColors.StatisticsText.FromString(TStringSetting(SettingsDict.GetSetting('StatisticsText')).Value);
	// FFontColors.FileText.FromString(TStringSetting(SettingsDict.GetSetting('FileText')).Value);
	// FFontColors.LineNumText.FromString(TStringSetting(SettingsDict.GetSetting('LineNumText')).Value);
	// FFontColors.ColNumText.FromString(TStringSetting(SettingsDict.GetSetting('ColNumText')).Value);
	// FFontColors.AlternateRow.FromString(TStringSetting(SettingsDict.GetSetting('AlternateRow')).Value);
end;

procedure TColorSettings.ReloadColors;
begin
	ReLoad;
	// LoadFromDict;

	if FFontColors.IsEmpty then begin
		LoadDefaultColors(TDarkModeHelper.GetActualThemeMode);
	end;
end;

procedure TFontAttributes.FromString(const _s : string);
var
	i : integer;
	arr : TArrayEx<string>;
	sEnumName : string;
	st : TFontStyle;
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

function TFontAttributes.FromFont(const _font : TFont) : TFontAttributes;
begin
	name := _font.Name;
	Style := _font.Style;
	Size := _font.Size;
	Color := _font.Color;
end;

function TFontAttributes.ToFont(var _font : TFont) : TFontAttributes;
begin
	_font.Name := name;
	_font.Style := Style;
	_font.Size := Size;
	_font.Color := Color;
end;

function TFontAttributes.ToString : string;
var
	arrFontStyles : TArrayEx<string>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TFontAttributes.ToString');

	for var st : TFontStyle in Style do begin
		arrFontStyles.Add(TRttiEnumerationType.GetName(st));
	end;

	var
		sStyles : string := string.Join(ARRAY_SEPARATOR, arrFontStyles.Items);

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

procedure TFontColors.SetByName(const _name : string; const _fa : TFontAttributes);
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

constructor TDefaultFontColors.Create(const _theme : EThemeMode = EThemeMode.tmLight);
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
	dc : TArrayEx<string>;
begin
	if FTheme = EThemeMode.tmLight then begin
		dc := DEFAULT_COLORS;
	end else begin
		dc := DEFAULT_DARK_COLORS;
	end;

	var
	idx := dc.IndexOf(_name, TComparer<string>.Construct(
		function(const Left, Right : string) : Integer
		begin
			var
			al := Left.Split(['=']);
			var
			ar := Right.Split(['=']);
			Result := TComparer<string>.Default.Compare(al[0], ar[0]);
		end));
	var
	ac := dc[idx].Split(['=']);
	_color.FromString(ac[1]);
end;

end.
