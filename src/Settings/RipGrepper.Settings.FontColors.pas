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
			TREEVIEW_ERROR_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clRed; BgColor : clNone; Style : [];);
			TREEVIEW_STATS_TEXT : TFontAttributes = (name : 'Segoe UI'; Size : 9; Color : clTeal; BgColor : clNone; Style : [];);
	end;

	TFontColors = record
		TreeViewMatchText : TFontAttributes;
		TreeViewReplaceText : TFontAttributes;
		TreeViewReplacedText : TFontAttributes;
		HistTreeViewSearchText : TFontAttributes;
		HistTreeViewReplaceText : TFontAttributes;
		HistTreeViewReplacedText : TFontAttributes;
		TreeViewStatText : TFontAttributes;
		TreeViewErrorText : TFontAttributes;
		TreeViewStatisicsText : TFontAttributes;
		TreeViewNormalText : TFontAttributes;
		ResultFileText : TFontAttributes;
		procedure SetByName(const _name : string; const _fa : TFontAttributes);
	end;

	TColorSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'ColorSettings';

		private
			FFontColors : TFontColors;

		protected
			procedure Init; override;

		public
			constructor Create(const _ini : TMemIniFile);
			destructor Destroy; override;
			procedure LoadFromDict(); override;
			procedure LoadDefaultsFromDict; override;
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
	TDebugUtils.DebugMessage('TColorSettings.Create: ' + FIniFile.FileName + '[' + IniSectionName + ']');
end;

destructor TColorSettings.Destroy;
begin
	inherited Destroy() // ok;
end;

procedure TColorSettings.Init;
begin
	SettingsDict.CreateSetting('TreeViewMatchText', varString, TDefaultFontColors.TREEVIEW_MATCH_TEXT.ToString());
	SettingsDict.CreateSetting('TreeViewReplaceText', varString, TDefaultFontColors.TREEVIEW_REPLACE_TEXT.ToString());
	SettingsDict.CreateSetting('TreeViewReplacedText', varString, TDefaultFontColors.TREEVIEW_REPLACED_TEXT.ToString());
	SettingsDict.CreateSetting('HistTreeViewSearchText', varString, TDefaultFontColors.HIST_TREEVIEW_SEARCH_TEXT.ToString());
	SettingsDict.CreateSetting('HistTreeViewReplaceText', varString, TDefaultFontColors.HIST_TREEVIEW_REPLACE_TEXT.ToString());
	SettingsDict.CreateSetting('HistTreeViewReplacedText', varString, TDefaultFontColors.HIST_TREEVIEW_REPLACED_TEXT.ToString());
	SettingsDict.CreateSetting('TreeViewNormalText', varString, TDefaultFontColors.TREEVIEW_NORMAL_TEXT.ToString());
	SettingsDict.CreateSetting('TreeViewStatText', varString, TDefaultFontColors.TREEVIEW_STAT_TEXT.ToString());
	SettingsDict.CreateSetting('TreeViewErrorText', varString, TDefaultFontColors.TREEVIEW_ERROR_TEXT.ToString());
	SettingsDict.CreateSetting('TreeViewStatisicsText', varString, TDefaultFontColors.TREEVIEW_STATS_TEXT.ToString());
	SettingsDict.CreateSetting('ResultFileText', varString, TDefaultFontColors.TREEVIEW_FILE_TEXT.ToString());
end;

procedure TColorSettings.LoadDefaultsFromDict;
begin
	inherited;
end;

procedure TColorSettings.LoadFromDict;
begin
	FFontColors.TreeViewMatchText.FromString(SettingsDict.GetSetting('TreeViewMatchText'));
	FFontColors.TreeViewReplaceText.FromString(SettingsDict.GetSetting('TreeViewReplaceText'));
	FFontColors.TreeViewReplacedText.FromString(SettingsDict.GetSetting('TreeViewReplacedText'));
	FFontColors.HistTreeViewSearchText.FromString(SettingsDict.GetSetting('HistTreeViewSearchText'));
	FFontColors.HistTreeViewReplaceText.FromString(SettingsDict.GetSetting('HistTreeViewReplaceText'));
	FFontColors.HistTreeViewReplacedText.FromString(SettingsDict.GetSetting('HistTreeViewReplacedText'));
	FFontColors.TreeViewNormalText.FromString(SettingsDict.GetSetting('TreeViewNormalText'));
	FFontColors.TreeViewStatText.FromString(SettingsDict.GetSetting('TreeViewStatText'));
	FFontColors.TreeViewErrorText.FromString(SettingsDict.GetSetting('TreeViewErrorText'));
	FFontColors.TreeViewStatisicsText.FromString(SettingsDict.GetSetting('TreeViewStatisicsText'));
	FFontColors.ResultFileText.FromString(SettingsDict.GetSetting('ResultFileText'));
end;

procedure TColorSettings.StoreToDict;
begin
	SettingsDict.StoreSetting('TreeViewMatchText', FFontColors.TreeViewMatchText.ToString());
	SettingsDict.StoreSetting('TreeViewReplaceText', FFontColors.TreeViewReplaceText.ToString());
	SettingsDict.StoreSetting('TreeViewReplacedText', FFontColors.TreeViewReplacedText.ToString());
	SettingsDict.StoreSetting('HistTreeViewSearchText', FFontColors.HistTreeViewSearchText.ToString());
	SettingsDict.StoreSetting('HistTreeViewReplaceText', FFontColors.HistTreeViewReplaceText.ToString());
	SettingsDict.StoreSetting('HistTreeViewReplacedText', FFontColors.HistTreeViewReplacedText.ToString());
	SettingsDict.StoreSetting('TreeViewNormalText', FFontColors.TreeViewNormalText.ToString());
	SettingsDict.StoreSetting('TreeViewStatText', FFontColors.TreeViewStatText.ToString());
	SettingsDict.StoreSetting('TreeViewErrorText', FFontColors.TreeViewErrorText.ToString());
	SettingsDict.StoreSetting('TreeViewStatisicsText', FFontColors.TreeViewStatisicsText.ToString());
	SettingsDict.StoreSetting('ResultFileText', FFontColors.ResultFileText.ToString());
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
	Name := _font.Name;
	Style := _font.Style;
	Size := _font.Size;
	Color := _font.Color;
end;

function TFontAttributes.ToFont(var _font : TFont) : TFontAttributes;
begin
	_font.Name := Name;
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
	if _name = 'TreeViewMatchText' then
		TreeViewMatchText := _fa
	else if _name = 'TreeViewReplaceText' then
		TreeViewReplaceText := _fa
	else if _name = 'TreeViewReplacedText' then
		TreeViewReplacedText := _fa
	else if _name = 'HistTreeViewSearchText' then
		HistTreeViewSearchText := _fa
	else if _name = 'HistTreeViewReplaceText' then
		HistTreeViewReplaceText := _fa
	else if _name = 'HistTreeViewReplacedText' then
		HistTreeViewReplacedText := _fa
	else if _name = 'TreeViewNormalText' then
		TreeViewNormalText := _fa
	else if _name = 'TreeViewStatText' then
		TreeViewStatText := _fa
	else if _name = 'TreeViewErrorText' then
		TreeViewErrorText := _fa
	else if _name = 'TreeViewStatisicsText' then
		TreeViewStatisicsText := _fa
	else if _name = 'ResultFileText' then
		ResultFileText := _fa
	else
		raise Exception.Create('Unknown font attribute name: ' + _name);
end;

end.
