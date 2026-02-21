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

			procedure AdvancedDrawItemHandler(Sender : TObject; ACanvas : TCanvas; ARect : TRect; State : TOwnerDrawState);

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
			procedure OnAsIsMenuItemClick(Sender : TObject);
			procedure OnSettingsMenuItemClick(Sender : TObject);
			procedure PopulateMenu;

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
	OnAdvancedDrawItem := AdvancedDrawItemHandler;
end;

procedure TRegexTemplateMenuItem.Click();
begin
	if Assigned(FOnTemplateSelected) then begin
		FOnTemplateSelected(FPatternPreview);
	end;
end;

procedure TRegexTemplateMenuItem.AdvancedDrawItem(ACanvas : TCanvas; ARect : TRect; State : TOwnerDrawState; TopLevel : Boolean);
begin
	inherited;
end;

procedure TRegexTemplateMenuItem.AdvancedDrawItemHandler(Sender : TObject; ACanvas : TCanvas; ARect : TRect; State : TOwnerDrawState);
var
	isSelected : Boolean;
begin
	AdvancedDrawItem(ACanvas, ARect, State, False);

	isSelected := odSelected in State;
	// Handle selected state only: trigger preview when a different item is highlighted
	if not isSelected then begin
		Exit;
	end;

	if Tag <> FLastPreviewedIndex^ then begin
		if Assigned(FOnTemplateSelected) then begin
			FOnTemplateSelected(FPatternPreview);
		end;
		FLastPreviewedIndex^ := Tag;
	end;
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
	menuItem.OnClick := OnAsIsMenuItemClick;
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

procedure TRegexTemplateMenu.OnAsIsMenuItemClick(Sender : TObject);
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
		if form.ShowModal = mrOk then begin
			// Reload templates from the updated settings
			FTemplateManager.LoadTemplates(FSettings);
		end;
	finally
		form.Free;
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
