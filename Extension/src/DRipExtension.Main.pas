// https://www.gexperts.org/open-tools-api-faq/#sample
unit DRipExtension.Main;

interface

uses
	ToolsAPI,
	RipGrepper.UI.MainForm;

type
	// TNotifierObject has stub implementations for the necessary but
	// unused IOTANotifer methods
	TDRipExtension = class(TNotifierObject, IOTAMenuWizard, IOTAWizard)
		private
			FRipGrepperForm : TRipGrepperForm;
			procedure CreateMenu;
			procedure DoMainMenuClick(Sender : TObject);

		public
			constructor Create;
			destructor Destroy; override;
			// IOTAWizard interafce methods(required for all wizards/experts)
			function GetIDString : string;
			function GetName : string;
			function GetState : TWizardState;
			procedure Execute;
			// IOTAMenuWizard (creates a simple menu item on the help menu)
			function GetMenuText : string;
	end;

procedure Register;

implementation

uses
	Dialogs,
	System.SysUtils,
	RipGrepper.Common.Types,
	RipGrepper.Common.Settings,
	System.IniFiles,
	DripExtension.IOTA.Utils,
	System.IOUtils,
	Vcl.Menus,
	Vcl.Graphics,
	RipGrepper.Tools.DebugTools;

var
	G_MainMenu : TMenuItem;

procedure Register;
begin
	RegisterPackageWizard(TDRipExtension.Create);
end;

procedure TDRipExtension.CreateMenu;
const
	MENUITEM_NAME = 'DRipExpertMenuItem';
var
	mainMenu : TMainMenu;
	iPos : integer;
begin
	if Assigned(G_MainMenu) then
		exit;

	mainMenu := (BorlandIDEServices as INTAServices).MainMenu;
	iPos := mainMenu.Items.Count - 1;
	for var i := 0 to mainMenu.Items.Count - 1 do begin
		if CompareText(mainMenu.Items[i].Name, MENUITEM_NAME) = 0 then begin
			mainMenu.Items.Remove(mainMenu.Items[i]);
			break
		end;
	end;
	for var i := 0 to mainMenu.Items.Count - 1 do begin
		if CompareText(mainMenu.Items[i].Name, 'ToolsMenu') = 0 then begin
			iPos := i;
			break;
		end;
	end;

	G_MainMenu := Vcl.Menus.NewItem(GetMenuText, 0, False, True, DoMainMenuClick, 0, MENUITEM_NAME);
	mainMenu.Items.Insert(iPos, G_MainMenu);
end;

constructor TDRipExtension.Create;
begin
	inherited;
	CreateMenu
end;

destructor TDRipExtension.Destroy;
begin
	TDebugUtils.DebugMessage('TDRipExtension.Destroy');
	FRipGrepperForm.Free;
	inherited;
end;

procedure TDRipExtension.DoMainMenuClick(Sender : TObject);
begin
	//
end;

procedure TDRipExtension.Execute;
begin
	TDebugUtils.DebugMessage('TDRipExtension.Execute');

	if not Assigned(FRipGrepperForm) then begin
		TDebugUtils.DebugMessage('CreateAndShow');
		FRipGrepperForm := TRipGrepperForm.CreateAndShow(GSettings);
	end else begin
		TDebugUtils.DebugMessage('Show');
		FRipGrepperForm.Show();
	end;

end;

function TDRipExtension.GetIDString : string;
begin
	Result := 'ID.' + EXTENSION_NAME;
end;

function TDRipExtension.GetMenuText : string;
begin
	Result := '&' + GetName();
end;

function TDRipExtension.GetName : string;
begin
	Result := EXTENSION_MENU_CAPTION;
end;

function TDRipExtension.GetState : TWizardState;
begin
	Result := [wsEnabled];
end;

end.
