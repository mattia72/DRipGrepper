unit RipGrepper.UI.RegexTemplateMenu;

interface

uses
	System.Classes,
	System.SysUtils,
	Vcl.Menus,
	Vcl.Controls,
	Vcl.Graphics,
	RipGrepper.Helper.RegexTemplates,
	System.Types,
	Winapi.Windows,
	RipGrepper.Settings.SettingVariant,
	RipGrepper.Settings.Persistable;

type
	TRegexTemplateSelectedEvent = procedure(const _pattern : string) of object;

	TRegexTemplateMenuItem = class(TMenuItem)
		private
			FPatternPreview : string;
			FLastPreviewedIndex : PInteger;
			FOnTemplateSelected : TRegexTemplateSelectedEvent;

	protected
		procedure AdvancedDrawItem(ACanvas : TCanvas; ARect : TRect; State : TOwnerDrawState; TopLevel : Boolean); override;

		public
			constructor Create(_owner : TComponent; const _caption, _patternPreview : string; { }
					_lastPreviewedIndex : PInteger; { }
					_onTemplateSelected : TRegexTemplateSelectedEvent); reintroduce;
			procedure Click(); override;
			property PatternPreview : string read FPatternPreview;
	end;

	TRegexTemplateMenu = class
		private
			FPopupMenu : TPopupMenu;
			FTemplateManager : TRegexTemplateManager;
			FOnTemplateSelected : TRegexTemplateSelectedEvent;
			FOriginalText : string;
			FLastPreviewedIndex : Integer;
			FSettings : IPersistableArray;
			FColorTheme : string;
			procedure OnOriginalTextMenuItemClick(Sender : TObject);
			procedure OnSettingsMenuItemClick(Sender : TObject);
			procedure PopulateMenu;
			procedure validatePattern(const _columnIndex : Integer; const _newText : string; { }
					var _isValid : Boolean; var _errorMsg : string);

		public
			constructor Create(_popupMenu : TPopupMenu; { } _templateManager : TRegexTemplateManager; { } _settings : IPersistableArray;
					{ } const _colorTheme : string);
			procedure ShowAtControl(_control : TControl; const _originalText : string);
			property OnTemplateSelected : TRegexTemplateSelectedEvent read FOnTemplateSelected write FOnTemplateSelected;
	end;

implementation

uses
	RipGrepper.UI.TabSeparatedConfigForm;

{ TRegexTemplateMenuItem }

constructor TRegexTemplateMenuItem.Create(_owner : TComponent; const _caption, _patternPreview : string; { }
		_lastPreviewedIndex : PInteger; { }
		_onTemplateSelected : TRegexTemplateSelectedEvent);
begin
	inherited Create(_owner);
	Caption := _caption;
	FPatternPreview := _patternPreview;
	FLastPreviewedIndex := _lastPreviewedIndex;
	FOnTemplateSelected := _onTemplateSelected;
end;

procedure TRegexTemplateMenuItem.Click();
begin
	if Assigned(FOnTemplateSelected) then begin
		FOnTemplateSelected(FPatternPreview);
	end;
end;

procedure TRegexTemplateMenuItem.AdvancedDrawItem(ACanvas : TCanvas; ARect : TRect; State : TOwnerDrawState; TopLevel : Boolean);
begin
	// Trigger preview when this item becomes highlighted
	if (odSelected in State) and (Tag <> FLastPreviewedIndex^) then begin
		if Assigned(FOnTemplateSelected) then begin
			FOnTemplateSelected(FPatternPreview);
		end;
		FLastPreviewedIndex^ := Tag;
	end;
	inherited;
end;

{ TRegexTemplateMenu }

constructor TRegexTemplateMenu.Create(_popupMenu : TPopupMenu;
		{ } _templateManager : TRegexTemplateManager;
		{ } _settings : IPersistableArray;
		{ } const _colorTheme : string);
begin
	inherited Create;
	FPopupMenu := _popupMenu;
	FTemplateManager := _templateManager;
	FSettings := _settings;
	FColorTheme := _colorTheme;
	FOriginalText := '';
	FLastPreviewedIndex := -1;
	FPopupMenu.OwnerDraw := True;
end;

procedure TRegexTemplateMenu.PopulateMenu;
var
	menuItem : TMenuItem;
	template : TRegexTemplate;
begin
	FPopupMenu.Items.Clear;

	// Load templates from settings in case they were updated eg. in a HistObj
	FSettings.ReLoadFromFile();
	FTemplateManager.LoadTemplates(FSettings);

	for var i : Integer := 0 to FTemplateManager.GetTemplateCount - 1 do begin
		template := FTemplateManager.GetTemplate(i);
		var
		templateItem := TRegexTemplateMenuItem.Create(FPopupMenu, template.Description, { }
				template.ApplyToText(FOriginalText), @FLastPreviewedIndex, FOnTemplateSelected);
		templateItem.Tag := i;
		FPopupMenu.Items.Add(templateItem);
	end;

	// Add separator
	if FTemplateManager.GetTemplateCount > 0 then begin
		menuItem := TMenuItem.Create(FPopupMenu);
		menuItem.Caption := '-';
		FPopupMenu.Items.Add(menuItem);
	end;

	// Add 'as is' menu item
	menuItem := TMenuItem.Create(FPopupMenu);
	menuItem.Caption := 'Original Text';
	menuItem.Tag := -1; // Special tag for 'as is'
	menuItem.OnClick := OnOriginalTextMenuItemClick;
	FPopupMenu.Items.Add(menuItem);

	// Add separator before Settings
	menuItem := TMenuItem.Create(FPopupMenu);
	menuItem.Caption := '-';
	FPopupMenu.Items.Add(menuItem);

	// Add 'Settings...' menu item
	menuItem := TMenuItem.Create(FPopupMenu);
	menuItem.Caption := 'Customize...';
	menuItem.Tag := -2; // Special tag for 'Settings'
	menuItem.OnClick := OnSettingsMenuItemClick;
	FPopupMenu.Items.Add(menuItem);
end;

procedure TRegexTemplateMenu.OnOriginalTextMenuItemClick(Sender : TObject);
begin
	if Assigned(FOnTemplateSelected) then begin
		FOnTemplateSelected(FOriginalText);
	end;
end;

procedure TRegexTemplateMenu.OnSettingsMenuItemClick(Sender : TObject);
var
	form : TTabSeparatedConfigForm;
begin
	form := TTabSeparatedConfigForm.Create(nil, FSettings, FColorTheme);
	try
		form.LoadColumnHeaders(['Description', 'Pattern']);
		form.OnValidate := validatePattern;
		if form.ShowModal = mrOk then begin
			// Reload templates from the updated settings
			FTemplateManager.LoadTemplates(FSettings);
		end;
	finally
		form.Free;
	end;
end;

procedure TRegexTemplateMenu.validatePattern(const _columnIndex : Integer; const _newText : string; { }
		var _isValid : Boolean; var _errorMsg : string);
begin
	// Column index 1 = Pattern (0 = Description)
	if _columnIndex = 1 then begin
		var placeholderCount := (_newText.Length - _newText.Replace(TRegexTemplate.TEXT_PLACEHOLDER, '').Length) 
		{ }	div TRegexTemplate.TEXT_PLACEHOLDER.Length;
		_isValid := placeholderCount = 1;
		if not _isValid then begin
			_errorMsg := Format('Pattern must contain the placeholder "%s" exactly once.', [TRegexTemplate.TEXT_PLACEHOLDER]);
		end;
	end;
end;

procedure TRegexTemplateMenu.ShowAtControl(_control : TControl; const _originalText : string);
var
	pt : TPoint;
begin
	FOriginalText := _originalText;
	FLastPreviewedIndex := -1;
	PopulateMenu;
	pt := _control.ClientToScreen(Point(0, _control.Height));
	FPopupMenu.Popup(pt.X, pt.Y);
end;

end.
