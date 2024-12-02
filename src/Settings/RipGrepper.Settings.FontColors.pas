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
		Color : TColor;
		BgColor : TColor;
		Style : TFontStyleSet;

		public
			class function FromString(const _s : string): TFontAttributes; static;
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
		HistTreeViewSearchText : TFontAttributes;
		HistTreeViewReplaceText : TFontAttributes;
		HistTreeViewReplacedText : TFontAttributes;

		TreeViewMatchText : TFontAttributes;
		TreeViewReplaceText : TFontAttributes;
		TreeViewReplacedText : TFontAttributes;

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
	RipGrepper.Common.Constants;

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
	FFontColors.TreeViewMatchText := TFontAttributes.FromString(SettingsDict.GetSetting('TreeViewMatchText'));
end;

procedure TColorSettings.StoreToDict;
begin
	SettingsDict.StoreSetting('TreeViewMatchText', FFontColors.TreeViewMatchText.ToString());
	inherited StoreToDict();
end;

class function TFontAttributes.FromString(const _s : string): TFontAttributes;
begin
	var
	arr := _s.Split([ARRAY_SEPARATOR]);
	Result.Color := StringToColor(arr[0]);
	Result.BgColor := StringToColor(arr[1]);
end;

function TFontAttributes.ToString : string;
begin
	Result := string.Join(ARRAY_SEPARATOR, [ColorToString(Color), ColorToString(BgColor)]);
end;

end.
