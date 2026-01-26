unit RipGrepper.UI.RegexTemplateMenu;

interface

uses
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
			procedure OnMenuItemClick(Sender : TObject);
			procedure PopulateMenu;

		public
			constructor Create(_popupMenu : TPopupMenu; _templateManager : TRegexTemplateManager);
			procedure ShowAtControl(_control : TControl);
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
end;

procedure TRegexTemplateMenu.PopulateMenu;
var
	menuItem : TMenuItem;
	template : TRegexTemplate;
begin
	FPopupMenu.Items.Clear;

	for var i : Integer := 0 to FTemplateManager.GetTemplateCount - 1 do begin
		template := FTemplateManager.GetTemplate(i);

		menuItem := TMenuItem.Create(FPopupMenu);
		menuItem.Caption := template.Description;
		menuItem.Tag := i;
		menuItem.OnClick := OnMenuItemClick;
		FPopupMenu.Items.Add(menuItem);
	end;
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
	pattern := FTemplateManager.GetTemplate(menuItem.Tag).Pattern;

	if Assigned(FOnTemplateSelected) then begin
		FOnTemplateSelected(pattern);
	end;
end;

procedure TRegexTemplateMenu.ShowAtControl(_control : TControl);
var
	pt : TPoint;
begin
	PopulateMenu;
	pt := _control.ClientToScreen(Point(0, _control.Height));
	FPopupMenu.Popup(pt.X, pt.Y);
end;

end.
