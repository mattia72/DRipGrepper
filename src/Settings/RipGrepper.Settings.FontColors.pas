unit RipGrepper.Settings.FontColors;

interface

uses
	System.UITypes,
	Vcl.Graphics;

type
	TFontStyleSet = set of TFontStyle;

	TFontAttributes = record
		Color : TColor;
		BgColor : TColor;
		Style : TFontStyleSet;
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

	// TColorSettings = class(TPersistableSettings)
	// 	const
	// 		INI_SECTION = 'RipGrepperColorSettings';

	// 	private
    // 		FFontAttributes: TFontAttributesRecord;
	// 	protected
	// 		procedure Init; override;

	// 	public
	// 		constructor Create(const _ini : TMemIniFile);
	// 		destructor Destroy; override;
	// 		procedure LoadFromDict(); override;
	// 		procedure LoadDefaultsFromDict; override;
	// 		procedure StoreToDict; override;

	// end;
implementation

end.
