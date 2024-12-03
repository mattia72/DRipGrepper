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
			function ToFont(var _font: TFont): TFontAttributes;
			function ToString : string;
	end;

const
	HIST_TREEVIEW_SEARCH_TEXT : TFontAttributes = (Color : clOlive; BgColor : clWhite; Style : []);
	HIST_TREEVIEW_REPLACE_TEXT : TFontAttributes = (Color : clGreen; BgColor : clWhite; Style : [fsBold]);
	HIST_TREEVIEW_REPLACED_TEXT : TFontAttributes = (Color : clMaroon; BgColor : clWhite; Style : [];);

	TREEVIEW_MATCH_TEXT : TFontAttributes = (Color : clWhite; BgColor : clOlive; Style : [];);
	TREEVIEW_REPLACE_TEXT : TFontAttributes = (Color : clWhite; BgColor : clGreen; Style : [fsBold];);
	TREEVIEW_REPLACED_TEXT : TFontAttributes = (Color : clWhite; BgColor : clMaroon; Style : [fsBold, fsStrikeOut];);

	TREEVIEW_NORMAL_TEXT : TFontAttributes = (Color : clDkGray; BgColor : clNone; Style : [];);
	TREEVIEW_STAT_TEXT : TFontAttributes = (Color : clPurple; BgColor : clNone; Style : [];);
	TREEVIEW_ERROR_TEXT : TFontAttributes = (Color : clRed; BgColor : clNone; Style : [];);
	TREEVIEW_STATS_TEXT : TFontAttributes = (Color : clTeal; BgColor : clNone; Style : [];);

type
	TFontColors = record
		TreeViewMatchText : TFontAttributes;
		TreeViewReplaceText : TFontAttributes;
		TreeViewReplacedText : TFontAttributes;

		HistTreeViewSearchText : TFontAttributes;
		HistTreeViewReplaceText : TFontAttributes;
		HistTreeViewReplacedText : TFontAttributes;

		TreeViewNormalText : TFontAttributes;
		TreeViewStatText : TFontAttributes;
		TreeViewErrorText : TFontAttributes;

		TreeViewStatisicsText : TFontAttributes;
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
	ArrayEx;

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
	SettingsDict.CreateSetting('TreeViewMatchText', varString, TREEVIEW_MATCH_TEXT.ToString());
end;

procedure TColorSettings.LoadDefaultsFromDict;
begin
	inherited;
end;

procedure TColorSettings.LoadFromDict;
begin
	FFontColors.TreeViewMatchText.FromString(SettingsDict.GetSetting('TreeViewMatchText'));
end;

procedure TColorSettings.StoreToDict;
begin
	SettingsDict.StoreSetting('TreeViewMatchText', FFontColors.TreeViewMatchText.ToString());
	inherited StoreToDict();
end;

procedure TFontAttributes.FromString(const _s : string);
var
	i : integer;
	arr : TArrayEx<string>;
begin
	i := 0;
	arr := _s.Split([ARRAY_SEPARATOR]);
	name := arr[PostInc(i)];
	Size := StrToIntDef(arr[PostInc(i)], 0);
	Color := StringToColor(arr[PostInc(i)]);
	BgColor := StringToColor(arr[PostInc(i)]);
	Style := [];

	while i < arr.Count do begin
		var
		s := TConversions<TFontStyle>.StringToEnumeration(arr[PostInc(i)]);
		Style := Style + [s];
	end;
end;

function TFontAttributes.FromFont(const _font : TFont) : TFontAttributes;
begin
	Name := _font.Name;
	Style := _font.Style;
	Size := _font.Size;
	Color := _font.Color;
end;

function TFontAttributes.ToFont(var _font: TFont): TFontAttributes;
begin
	_font.Name := Name;
	_font.Style := Style;
	_font.Size := Size;
	_font.Color := Color;
end;

function TFontAttributes.ToString : string;
var
	arr : TArrayEx<string>;
begin
	for var st : TFontStyle in Style do begin
		arr.Add(TConversions<TFontStyle>.EnumerationToString(st));
	end;

	Result := string.Join(ARRAY_SEPARATOR, [
		{ } name,
		{ } IntToStr(Size),
		{ } ColorToString(Color),
		{ } ColorToString(BgColor),
		{ } string.Join(ARRAY_SEPARATOR, arr.Items)]);
end;

end.
