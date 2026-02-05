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
	Winapi.Windows;

type
	TRegexTemplateSelectedEvent = procedure(const _pattern : string) of object;

	TRegexTemplateMenu = class
		private
			FPopupMenu : TPopupMenu;
			FTemplateManager : TRegexTemplateManager;
			FOnTemplateSelected : TRegexTemplateSelectedEvent;
			FOriginalText : string;
			FBuiltPatterns : TArray<string>;
			FLastPreviewedIndex : Integer;
			procedure OnMenuItemClick(Sender : TObject);
			procedure OnAsIsMenuItemClick(Sender : TObject);
			procedure OnMenuItemDrawItem(Sender : TObject; ACanvas : TCanvas; ARect : TRect; State : TOwnerDrawState);
			procedure PopulateMenu;

		public
			constructor Create(_popupMenu : TPopupMenu; _templateManager : TRegexTemplateManager);
			procedure ShowAtControl(_control : TControl; const _originalText : string);
			property OnTemplateSelected : TRegexTemplateSelectedEvent read FOnTemplateSelected write FOnTemplateSelected;
	end;

implementation

{ TRegexTemplateMenu }

constructor TRegexTemplateMenu.Create(_popupMenu : TPopupMenu; _templateManager : TRegexTemplateManager);
begin
	inherited Create;
	FPopupMenu := _popupMenu;
	FTemplateManager := _templateManager;
	FOriginalText := '';
	FLastPreviewedIndex := -1;
end;

procedure TRegexTemplateMenu.PopulateMenu;
var
	menuItem : TMenuItem;
	template : TRegexTemplate;
begin
	FPopupMenu.Items.Clear;

	// Build all patterns from original text
	SetLength(FBuiltPatterns, FTemplateManager.GetTemplateCount);
	for var i : Integer := 0 to FTemplateManager.GetTemplateCount - 1 do begin
		template := FTemplateManager.GetTemplate(i);
		FBuiltPatterns[i] := template.ApplyToText(FOriginalText);

		menuItem := TMenuItem.Create(FPopupMenu);
		menuItem.Caption := template.Description;
		menuItem.Tag := i;
		menuItem.OnClick := OnMenuItemClick;
		menuItem.OnAdvancedDrawItem := OnMenuItemDrawItem;
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
	menuItem.Tag := -1; // Special tag for 'as is'
	menuItem.OnClick := OnAsIsMenuItemClick;
	menuItem.OnAdvancedDrawItem := OnMenuItemDrawItem;
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

procedure TRegexTemplateMenu.OnMenuItemDrawItem(Sender : TObject; ACanvas : TCanvas; ARect : TRect; State : TOwnerDrawState);
var
	menuItem : TMenuItem;
	currentIndex : Integer;
	isSelected : Boolean;
begin
	if not(Sender is TMenuItem) then begin
		Exit;
	end;

	menuItem := TMenuItem(Sender);
	isSelected := odSelected in State;

	// Draw the menu item
	if isSelected then begin
		ACanvas.Brush.Color := clHighlight;
		ACanvas.Font.Color := clHighlightText;
	end else begin
		ACanvas.Brush.Color := clMenu;
		ACanvas.Font.Color := clMenuText;
	end;
	ACanvas.FillRect(ARect);
	DrawText(ACanvas.Handle, PChar(menuItem.Caption), -1, ARect, DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP);

	// Show preview when highlighting different item
	currentIndex := menuItem.Tag;
	if isSelected and (currentIndex <> FLastPreviewedIndex) then begin
		if Assigned(FOnTemplateSelected) then begin
			if currentIndex = -1 then begin
				// 'as is' menu item - show original text
				FOnTemplateSelected(FOriginalText);
			end else begin
				// Template menu item - show built pattern
				FOnTemplateSelected(FBuiltPatterns[currentIndex]);
			end;
		end;
		FLastPreviewedIndex := currentIndex;
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
	FLastPreviewedIndex := -1;
	PopulateMenu;
	pt := _control.ClientToScreen(Point(0, _control.Height));
	FPopupMenu.Popup(pt.X, pt.Y);
end;

end.
