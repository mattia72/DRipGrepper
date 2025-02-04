unit RipGrepper.Settings.FontColors;

interface

uses
	System.Classes,
	System.UITypes,
	Vcl.Graphics,
	RipGrepper.Settings.Persistable,
	System.IniFiles,
	RipGrepper.Helper.UI.DarkMode;

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
			{ } 'AlternateRow=Segoe UI|9|clNone|cl3DLight'
			];

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
			{ } 'AlternateRow=Segoe UI|9|clNone|$002B2B2B'
			];

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
			FFontColors: TFontColors;
		protected
			function GetIsAlreadyRead : Boolean; override;
			procedure Init; override;

		public
			constructor Create(const _Owner : TPersistableSettings);
			destructor Destroy; override;
			procedure LoadFromDict(); override;
			procedure LoadDefaultsFromDict(); override;
			procedure LoadDefaultColors;
			procedure ReloadColors;
			procedure StoreToDict; override;
			property FontColors: TFontColors read FFontColors write FFontColors;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.SysUtils,
	RipGrepper.Common.Constants,
	RipGrepper.Helper.Types,
	ArrayEx,
	System.Rtti,
	System.Generics.Defaults;

{ TColorSettings }

constructor TColorSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := INI_SECTION;
	inherited;
	TDebugUtils.DebugMessage('TColorSettings.Create: ' + IniFile.FileName + '[' + IniSectionName + ']');
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
	var
	df := TDefaultFontColors.Create();
	try
		SettingsDict.CreateSetting('MatchText', varString, df.TREEVIEW_MATCH_TEXT.ToString());
		SettingsDict.CreateSetting('ReplaceText', varString, df.TREEVIEW_REPLACE_TEXT.ToString());
		SettingsDict.CreateSetting('ReplacedText', varString, df.TREEVIEW_REPLACED_TEXT.ToString());
		SettingsDict.CreateSetting('SearchTextInHistory', varString, df.HIST_TREEVIEW_SEARCH_TEXT.ToString());
		SettingsDict.CreateSetting('ReplaceTextInHistory', varString, df.HIST_TREEVIEW_REPLACE_TEXT.ToString());
		SettingsDict.CreateSetting('ReplacedTextInHistory', varString, df.HIST_TREEVIEW_REPLACED_TEXT.ToString());
		SettingsDict.CreateSetting('NormalText', varString, df.TREEVIEW_NORMAL_TEXT.ToString());
		SettingsDict.CreateSetting('CounterText', varString, df.TREEVIEW_STAT_TEXT.ToString());
		SettingsDict.CreateSetting('ErrorText', varString, df.TREEVIEW_ERROR_TEXT.ToString());
		SettingsDict.CreateSetting('StatisticsText', varString, df.TREEVIEW_STATS_TEXT.ToString());
		SettingsDict.CreateSetting('ColNumText', varString, df.TREEVIEW_COL_NUM_TEXT.ToString());
		SettingsDict.CreateSetting('LineNumText', varString, df.TREEVIEW_LINE_NUM_TEXT.ToString());
		SettingsDict.CreateSetting('FileText', varString, df.TREEVIEW_FILE_TEXT.ToString());
		SettingsDict.CreateSetting('AlternateRow', varString, df.TREEVIEW_ALTERNATE_ROW.ToString());
	finally
		df.Free;
	end;
end;

procedure TColorSettings.LoadDefaultColors;
begin
	var
	df := TDefaultFontColors.Create(TDarkModeHelper.GetActualThemeMode);
	try
		FFontColors.MatchText.FromString(df.TREEVIEW_MATCH_TEXT.ToString());
		FFontColors.ReplaceText.FromString(df.TREEVIEW_REPLACE_TEXT.ToString());
		FFontColors.ReplacedText.FromString(df.TREEVIEW_REPLACED_TEXT.ToString());
		FFontColors.SearchTextInHistory.FromString(df.HIST_TREEVIEW_SEARCH_TEXT.ToString());
		FFontColors.ReplaceTextInHistory.FromString(df.HIST_TREEVIEW_REPLACE_TEXT.ToString());
		FFontColors.ReplacedTextInHistory.FromString(df.HIST_TREEVIEW_REPLACED_TEXT.ToString());
		FFontColors.NormalText.FromString(df.TREEVIEW_NORMAL_TEXT.ToString());
		FFontColors.CounterText.FromString(df.TREEVIEW_STAT_TEXT.ToString());
		FFontColors.ErrorText.FromString(df.TREEVIEW_ERROR_TEXT.ToString());
		FFontColors.StatisticsText.FromString(df.TREEVIEW_STATS_TEXT.ToString());
		FFontColors.FileText.FromString(df.TREEVIEW_FILE_TEXT.ToString());
		FFontColors.LineNumText.FromString(df.TREEVIEW_LINE_NUM_TEXT.ToString());
		FFontColors.ColNumText.FromString(df.TREEVIEW_COL_NUM_TEXT.ToString());
		FFontColors.AlternateRow.FromString(df.TREEVIEW_ALTERNATE_ROW.ToString());
	finally
		df.Free;
	end;
end;

procedure TColorSettings.LoadDefaultsFromDict;
begin
	// abstract func should be defined, settings are not `default relevant` so we can ignore this
end;

procedure TColorSettings.LoadFromDict;
begin
	FFontColors.MatchText.FromString(SettingsDict.GetSetting('MatchText'));
	FFontColors.ReplaceText.FromString(SettingsDict.GetSetting('ReplaceText'));
	FFontColors.ReplacedText.FromString(SettingsDict.GetSetting('ReplacedText'));
	FFontColors.SearchTextInHistory.FromString(SettingsDict.GetSetting('SearchTextInHistory'));
	FFontColors.ReplaceTextInHistory.FromString(SettingsDict.GetSetting('ReplaceTextInHistory'));
	FFontColors.ReplacedTextInHistory.FromString(SettingsDict.GetSetting('ReplacedTextInHistory'));
	FFontColors.NormalText.FromString(SettingsDict.GetSetting('NormalText'));
	FFontColors.CounterText.FromString(SettingsDict.GetSetting('CounterText'));
	FFontColors.ErrorText.FromString(SettingsDict.GetSetting('ErrorText'));
	FFontColors.StatisticsText.FromString(SettingsDict.GetSetting('StatisticsText'));
	FFontColors.FileText.FromString(SettingsDict.GetSetting('FileText'));
	FFontColors.LineNumText.FromString(SettingsDict.GetSetting('LineNumText'));
	FFontColors.ColNumText.FromString(SettingsDict.GetSetting('ColNumText'));
	FFontColors.AlternateRow.FromString(SettingsDict.GetSetting('AlternateRow'));
end;

procedure TColorSettings.ReloadColors;
begin
	ReLoad;
	LoadFromDict;

	if FFontColors.IsEmpty then begin
		LoadDefaultColors;
	end;
end;

procedure TColorSettings.StoreToDict;
begin
	SettingsDict.StoreSetting('MatchText', FFontColors.MatchText.ToString());
	SettingsDict.StoreSetting('ReplaceText', FFontColors.ReplaceText.ToString());
	SettingsDict.StoreSetting('ReplacedText', FFontColors.ReplacedText.ToString());
	SettingsDict.StoreSetting('SearchTextInHistory', FFontColors.SearchTextInHistory.ToString());
	SettingsDict.StoreSetting('ReplaceTextInHistory', FFontColors.ReplaceTextInHistory.ToString());
	SettingsDict.StoreSetting('ReplacedTextInHistory', FFontColors.ReplacedTextInHistory.ToString());
	SettingsDict.StoreSetting('NormalText', FFontColors.NormalText.ToString());
	SettingsDict.StoreSetting('CounterText', FFontColors.CounterText.ToString());
	SettingsDict.StoreSetting('ErrorText', FFontColors.ErrorText.ToString());
	SettingsDict.StoreSetting('StatisticsText', FFontColors.StatisticsText.ToString());
	SettingsDict.StoreSetting('LineNumText', FFontColors.LineNumText.ToString());
	SettingsDict.StoreSetting('ColNumText', FFontColors.ColNumText.ToString());
	SettingsDict.StoreSetting('FileText', FFontColors.FileText.ToString());
	SettingsDict.StoreSetting('AlternateRow', FFontColors.AlternateRow.ToString());
	inherited StoreToDict();
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
