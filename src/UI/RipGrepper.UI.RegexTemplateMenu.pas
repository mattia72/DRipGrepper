unit RipGrepper.UI.RegexTemplateMenu;

interface

uses
	ArrayEx,
	System.Classes,
	System.SysUtils,
	Vcl.Menus,
	Vcl.Controls,
	RipGrepper.Helper.RegexTemplates;

type
	TRegexTemplateSelectedEvent = procedure(const _pattern : string) of object;

	TRegexTemplateMenu = class
		private
			FPopupMenu : TPopupMenu;
			FTemplateManager : TRegexTemplateManager;
			FOnTemplateSelected : TRegexTemplateSelectedEvent;
			FOriginalText : string;
			FBuiltPatterns : TArrayEx<string>;
			procedure OnMenuItemClick(Sender : TObject);
			procedure OnAsIsMenuItemClick(Sender : TObject);
			procedure PopulateMenu;

		public
			constructor Create(_popupMenu : TPopupMenu; _templateManager : TRegexTemplateManager);
			procedure ShowAtControl(_control : TControl; const _originalText : string);
			property OnTemplateSelected : TRegexTemplateSelectedEvent read FOnTemplateSelected write FOnTemplateSelected;
	end;

implementation

uses
	System.Types;

{ TRegexTemplateMenu }

constructor TRegexTemplateMenu.Create(_popupMenu : TPopupMenu; _templateManager : TRegexTemplateManager);
begin
	inherited Create;
	FPopupMenu := _popupMenu;
	FTemplateManager := _templateManager;
	FOriginalText := '';
end;

procedure TRegexTemplateMenu.PopulateMenu;
var
	menuItem : TMenuItem;
	template : TRegexTemplate;
begin
	FPopupMenu.Items.Clear;

	// Build all patterns from original text
	FBuiltPatterns.Clear;
	for var i : Integer := 0 to FTemplateManager.GetTemplateCount - 1 do begin
		template := FTemplateManager.GetTemplate(i);
		FBuiltPatterns.Add(template.ApplyToText(FOriginalText));

		menuItem := TMenuItem.Create(FPopupMenu);
		menuItem.Caption := template.Description;
		menuItem.Tag := i;
		menuItem.OnClick := OnMenuItemClick;
		FPopupMenu.Items.Add(menuItem);
	end;

	// Add separator
	if FTemplateManager.GetTemplateCount > 0 then begin
		menuItem := TMenuItem.Create(FPopupMenu);
		menuItem.Caption := '-';
		FPopupMenu.Items.Add(menuItem);
	end;

	// Add 'as is' menu item
	menuItem := TMenuItem.Create(FPopupMenu);
	menuItem.Caption := 'as is';
	menuItem.OnClick := OnAsIsMenuItemClick;
	FPopupMenu.Items.Add(menuItem);
end;

procedure TRegexTemplateMenu.OnMenuItemClick(Sender : TObject);
var
	menuItem : TMenuItem;
	pattern : string;
begin
	if not(Sender is TMenuItem) then begin
		Exit;
	end;

	menuItem := TMenuItem(Sender);
	pattern := FBuiltPatterns[menuItem.Tag];

	if Assigned(FOnTemplateSelected) then begin
		FOnTemplateSelected(pattern);
	end;
end;

procedure TRegexTemplateMenu.OnAsIsMenuItemClick(Sender : TObject);
begin
	if Assigned(FOnTemplateSelected) then begin
		FOnTemplateSelected(FOriginalText);
	end;
end;

procedure TRegexTemplateMenu.ShowAtControl(_control : TControl; const _originalText : string);
var
	pt : TPoint;
begin
	FOriginalText := _originalText;
	PopulateMenu;
	pt := _control.ClientToScreen(Point(0, _control.Height));
	FPopupMenu.Popup(pt.X, pt.Y);
end;

end.
