// https://www.gexperts.org/open-tools-api-faq/#sample
unit DRipExtension.Main;

interface

uses
	ToolsAPI,
	RipGrepper.UI.MainForm,
	DripExtension.UI.DockableForm;

type
	// TNotifierObject has stub implementations for the necessary but
	// unused IOTANotifer methods
	TDRipExtension = class(TNotifierObject, IOTAMenuWizard, IOTAWizard)
		const
			DRIP_MENUITEM_NAME = 'DRipExpertMenuItem';

		private
			FKeyNotifier : IOTAKeyboardBinding;
			FKeyBinding : integer;

			FDockableForm : TRipGrepperDockableForm;
			procedure CreateMenu;
			procedure DoDripGrepperMenuClick(Sender : TObject);
			procedure InitKeyboardNotifier;
			procedure RegisterKeyboardBinding;
			procedure RemoveExtensionMenu;
			procedure ShowDripGrepperForm;
			procedure UnregisterKeyboardBinding;

		public
			constructor Create; virtual;
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
	RipGrepper.Tools.DebugTools,
	System.Classes;

var
	DripMenu : TMenuItem;
	G_DRipExtension : TDRipExtension;

procedure Register;
begin
	RegisterPackageWizard(TDRipExtension.Create);
end;

procedure TDRipExtension.CreateMenu;
var
	mainMenu : TMainMenu;
	Item : TMenuItem;
	iPos : integer;
	sc : TShortCut;
begin
	if Assigned(DripMenu) then
		exit;
	TDebugUtils.DebugMessage('TDRipExtension.CreateMenu');

	RemoveExtensionMenu();
	sc := TextToShortCut(GSettings.ExtensionSettings.DripGrepperShortCut);
	DripMenu := Vcl.Menus.NewItem(GetMenuText + '...', sc, False, True, DoDripGrepperMenuClick, 0,
		DRIP_MENUITEM_NAME);
	TDebugUtils.DebugMessage('TDRipExtension.CreateMenu - NewItem ' + DRIP_MENUITEM_NAME);

	Item := IOTAUTils.FindMenuItem('ToolsMenu');
	if Item <> nil then begin
		iPos := Item.IndexOf(IOTAUTils.FindMenuItem('ToolsDebuggerOptionsItem'));
		TDebugUtils.DebugMessage('TDRipExtension.CreateMenu - iPos ' + iPos.ToString);
		if iPos = -1 then
			iPos := Item.IndexOf(IOTAUTils.FindMenuItem('ToolsToolsItem')) - 1;
		if iPos >= 0 then begin
			TDebugUtils.DebugMessage('TDRipExtension.CreateMenu - ToolsToolsItem iPos ' + iPos.ToString);
			Item.Insert(iPos + 1, DripMenu)
		end else begin
			TDebugUtils.DebugMessage('TDRipExtension.CreateMenu - ToolsToolsItem not found');
			Item.Insert(1, DripMenu);
		end;
	end;

end;

constructor TDRipExtension.Create;
begin
	inherited;
	G_DRipExtension := self;
	CreateMenu;
end;

destructor TDRipExtension.Destroy;
begin
	TDebugUtils.DebugMessage('TDRipExtension.Destroy');
	DripMenu.Free;

	TDebugUtils.DebugMessage('TDRipExtension.Destroy FDockableForm.FreeInstance');
	FDockableForm.FreeInstance;
	TDebugUtils.DebugMessage('TDRipExtension.Destroy FDockableForm.Free');
	FDockableForm.Free;

	G_DRipExtension := nil;
	inherited;
end;

procedure TDRipExtension.DoDripGrepperMenuClick(Sender : TObject);
begin
	ShowDripGrepperForm;
end;

// IOTAWizard
procedure TDRipExtension.Execute;
begin
	ShowMessage('https://github.com/mattia72/DRipGrepper');
end;

// IOTAWizard
function TDRipExtension.GetIDString : string;
begin
	Result := 'ID.' + EXTENSION_NAME;
end;

// IOTAWizard
function TDRipExtension.GetMenuText : string;
begin
	Result := '&' + EXTENSION_NAME;
end;

// IOTAWizard
function TDRipExtension.GetName : string;
begin
	Result := EXTENSION_MENU_CAPTION;
end;

// IOTAWizard
function TDRipExtension.GetState : TWizardState;
begin
	Result := [wsEnabled];
end;

// IOTAWizard
procedure TDRipExtension.InitKeyboardNotifier;
begin
	if not Assigned(FKeyNotifier) then begin
		// FKeyNotifier := TAGExpertKeyboardNotifier.Create;
		// FKeyNotifier.OnShortCut := DoShortCut;
	end;
end;

procedure TDRipExtension.RegisterKeyboardBinding;
var
	kbServices : IOTAKeyboardServices;
begin
	if not Assigned(FKeyNotifier) then
		exit;

	kbServices := BorlandIDEServices as IOTAKeyboardServices;
	try
		if Assigned(kbServices) then
			FKeyBinding := kbServices.AddKeyboardBinding(FKeyNotifier);
	except
		FKeyBinding := -1;
	end;
end;

procedure TDRipExtension.RemoveExtensionMenu();
var
	toolsMenu : TMenu;
	dripMenuItem : TMenuItem;
begin
	TDebugUtils.DebugMessage('TDRipExtension.RemoveExtensionMenu');
	toolsMenu := IOTAUTils.FindMenu('ToolsMenu');
	if toolsMenu <> nil then begin
		dripMenuItem := IOTAUTils.FindMenuItem(DRIP_MENUITEM_NAME);
		if dripMenuItem <> nil then begin
			TDebugUtils.DebugMessage('TDRipExtension.RemoveExtensionMenu - ' + dripMenuItem.Caption);
			toolsMenu.Items.Remove(dripMenuItem);
		end;
	end;

end;

procedure TDRipExtension.ShowDripGrepperForm;
begin
	TDebugUtils.DebugMessage('TDRipExtension.ShowDripGrepperForm');

	if not Assigned(FDockableForm) then begin
		TDebugUtils.DebugMessage('TDRipExtension.ShowDripGrepperForm - Create FDockableForm');
		FDockableForm := TRipGrepperDockableForm.Instance;
		TRipGrepperDockableForm.CreateOrShowDockableForm();
	end else begin
		TDebugUtils.DebugMessage('TDRipExtension.ShowDripGrepperForm - Show FDockableForm');
		TRipGrepperDockableForm.CreateOrShowDockableForm();
	end;
end;

procedure TDRipExtension.UnregisterKeyboardBinding;
begin
	TDebugUtils.DebugMessage('UnregisterKeyboardBinding');
	if FKeyBinding > -1 then begin
		(BorlandIDEServices as IOTAKeyboardServices).RemoveKeyboardBinding(FKeyBinding);
		FKeyBinding := -1;
	end
	else
		// FKeyNotifier.Free;
		FKeyNotifier := nil;
end;

end.
