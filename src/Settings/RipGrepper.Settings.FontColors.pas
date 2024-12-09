unit RipGrepper.Settings.FontColors;

interface

uses
	System.UITypes,
	Vcl.Graphics,
	RipGrepper.Settings.Persistable,
	System.IniFiles;

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
			HIST_TREEVIEW_SEARCH_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clOlive; BgColor : clWhite; Style : []);
			HIST_TREEVIEW_REPLACE_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clGreen; BgColor : clWhite;
				Style : [fsBold]);
			HIST_TREEVIEW_REPLACED_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clMaroon; BgColor : clWhite; Style : [];);

			TREEVIEW_MATCH_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clWhite; BgColor : clOlive; Style : [];);
			TREEVIEW_REPLACE_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clWhite; BgColor : clGreen; Style : [fsBold];);
			TREEVIEW_REPLACED_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clWhite; BgColor : clMaroon;
				Style : [fsBold, fsStrikeOut];);

			TREEVIEW_FILE_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clDkGray; BgColor : clNone; Style : [fsBold];);
			TREEVIEW_NORMAL_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clDkGray; BgColor : clNone; Style : [];);
			TREEVIEW_STAT_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clPurple; BgColor : clNone; Style : [];);
			TREEVIEW_LINE_NUM_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clPurple; BgColor : clNone; Style : [];);
			TREEVIEW_COL_NUM_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clPurple; BgColor : clNone; Style : [];);
			TREEVIEW_ERROR_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clRed; BgColor : clNone; Style : [];);
			TREEVIEW_STATS_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clTeal; BgColor : clNone; Style : [];);
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
		procedure SetByName(const _name : string; const _fa : TFontAttributes);
	end;

	TColorSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'ColorSettings';

		private
			FFontColors : TFontColors;

		protected
			function GetIsAlreadyRead : Boolean; override;
			procedure Init; override;

		public
			constructor Create(const _ini : TMemIniFile);
			destructor Destroy; override;
			procedure LoadFromDict(); override;
			procedure LoadDefaultsFromDict; override;
			procedure ReloadColors;
			procedure StoreToDict; override;
			property FontColors : TFontColors read FFontColors write FFontColors;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.SysUtils,
	RipGrepper.Common.Constants,
	RipGrepper.Helper.Types,
	ArrayEx,
	System.Rtti;

{ TColorSettings }

constructor TColorSettings.Create(const _ini : TMemIniFile);
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
	SettingsDict.CreateSetting('MatchText', varString, TDefaultFontColors.TREEVIEW_MATCH_TEXT.ToString());
	SettingsDict.CreateSetting('ReplaceText', varString, TDefaultFontColors.TREEVIEW_REPLACE_TEXT.ToString());
	SettingsDict.CreateSetting('ReplacedText', varString, TDefaultFontColors.TREEVIEW_REPLACED_TEXT.ToString());
	SettingsDict.CreateSetting('SearchTextInHistory', varString, TDefaultFontColors.HIST_TREEVIEW_SEARCH_TEXT.ToString());
	SettingsDict.CreateSetting('ReplaceTextInHistory', varString, TDefaultFontColors.HIST_TREEVIEW_REPLACE_TEXT.ToString());
	SettingsDict.CreateSetting('ReplacedTextInHistory', varString, TDefaultFontColors.HIST_TREEVIEW_REPLACED_TEXT.ToString());
	SettingsDict.CreateSetting('NormalText', varString, TDefaultFontColors.TREEVIEW_NORMAL_TEXT.ToString());
	SettingsDict.CreateSetting('CounterText', varString, TDefaultFontColors.TREEVIEW_STAT_TEXT.ToString());
	SettingsDict.CreateSetting('ErrorText', varString, TDefaultFontColors.TREEVIEW_ERROR_TEXT.ToString());
	SettingsDict.CreateSetting('StatisticsText', varString, TDefaultFontColors.TREEVIEW_STATS_TEXT.ToString());
	SettingsDict.CreateSetting('ColNumText', varString, TDefaultFontColors.TREEVIEW_STAT_TEXT.ToString());
	SettingsDict.CreateSetting('LineNumText', varString, TDefaultFontColors.TREEVIEW_STAT_TEXT.ToString());
	SettingsDict.CreateSetting('FileText', varString, TDefaultFontColors.TREEVIEW_FILE_TEXT.ToString());
end;

procedure TColorSettings.LoadDefaultsFromDict;
begin
	inherited;
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
end;

procedure TColorSettings.ReloadColors;
begin
	ReLoad;
	LoadFromDict;
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
			sEnumName := arr[PostInc(i)];
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
	else
		raise Exception.Create('Unknown font attribute name: ' + _name);
end;

end.
