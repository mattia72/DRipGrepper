// https://www.gexperts.org/open-tools-api-faq/#sample
unit DRipExtension.Main;

interface

uses
	ToolsAPI,
	RipGrepper.UI.MainForm,
	DripExtension.UI.DockableForm,
	Vcl.Graphics;

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
			FIconBmp : TBitmap;
			FiPluginIndexAbout : Integer;
			procedure CreateMenu;
			procedure DoDripGrepperMenuClick(Sender : TObject);
			function GetIconBmp : Vcl.Graphics.TBitmap;

			procedure InitPluginInfo;
			procedure RemoveExtensionMenu;
			// **********************************************************************************************************************
			// Plugin-Infos entfernen
			// **********************************************************************************************************************
			procedure RemovePluginInfo;
			procedure ShowDripGrepperForm;
			property IconBmp : TBitmap read GetIconBmp;

		public
			constructor Create; virtual;
			destructor Destroy; override;
			function AddToImageList : Integer;
			// IOTAWizard interafce methods(required for all wizards/experts)
			function GetIDString : string;
			function GetName : string;
			function GetState : TWizardState;
			procedure Execute;
			// IOTAMenuWizard (creates a simple menu item on the help menu)
			function GetMenuText : string;
			// IOTAWizard
			procedure InitKeyboardNotifier;
			procedure RegisterKeyboardBinding;
			procedure UnregisterKeyboardBinding;

	end;

procedure Register;

implementation

uses
	Dialogs,
	System.SysUtils,
	RipGrepper.Common.Constants,
	RipGrepper.Common.Settings.RipGrepperSettings,
	RipGrepper.Tools.FileUtils,
	System.IniFiles,
	RipGrepper.Common.IOTAUtils,
	System.IOUtils,
	Vcl.Menus,
	RipGrepper.Tools.DebugUtils,
	System.Classes,
	Winapi.Windows,
	Vcl.ImgList;

var
	G_DripMenu : TMenuItem;
	G_DRipExtension : TDRipExtension;

procedure Register;
begin
	RegisterPackageWizard(TDRipExtension.Create);
end;

procedure TDRipExtension.CreateMenu;
var
	Item : TMenuItem;
	iPos : integer;
	sc : TShortCut;
begin
	TDebugUtils.DebugMessage('TDRipExtension.CreateMenu start');

	// if Assigned(G_DripMenu) then
	// exit;

	RemoveExtensionMenu();
	if not GSettings.ExtensionSettings.IsAlreadyRead then begin
		GSettings.ExtensionSettings.ReadIni;
	end;
	TDebugUtils.DebugMessage('TDRipExtension.CreateMenu shortcut ' + GSettings.ExtensionSettings.DripGrepperShortCut);
	sc := TextToShortCut(GSettings.ExtensionSettings.DripGrepperShortCut);
	if sc = 0 then begin
		sc := TextToShortCut(TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH);
	end;

	G_DripMenu := Vcl.Menus.NewItem(GetMenuText + '...', sc, False, True, DoDripGrepperMenuClick, 0, DRIP_MENUITEM_NAME);
	TDebugUtils.DebugMessage(Format('TDRipExtension.CreateMenu - NewItem ''%s 0x%x''', [DRIP_MENUITEM_NAME, sc]));
	G_DripMenu.ImageIndex := AddToImageList;
	//G_DripMenu.ImageIndex := IOTAUTils.AddToImageList(IconBmp, 'DripExtension icon');
	TDebugUtils.DebugMessage(Format('TDRipExtension.CreateMenu - G_DripMenu.ImageIndex %d', [G_DripMenu.ImageIndex]));

	Item := IOTAUTils.FindMenuItem('ToolsMenu');
	if Item <> nil then begin
		iPos := Item.IndexOf(IOTAUTils.FindMenuItem('ToolsDebuggerOptionsItem'));
		TDebugUtils.DebugMessage('TDRipExtension.CreateMenu - iPos ' + iPos.ToString);
		if iPos = -1 then
			iPos := Item.IndexOf(IOTAUTils.FindMenuItem('ToolsToolsItem')) - 1;
		if iPos >= 0 then begin
			TDebugUtils.DebugMessage('TDRipExtension.CreateMenu - ToolsToolsItem iPos ' + iPos.ToString);
			Item.Insert(iPos + 1, G_DripMenu)
		end else begin
			TDebugUtils.DebugMessage('TDRipExtension.CreateMenu - ToolsToolsItem not found');
			Item.Insert(1, G_DripMenu);
		end;
	end;

end;

constructor TDRipExtension.Create;
begin
	inherited;
	TDebugUtils.DebugMessage('TDRipExtension.Create begin');
	InitPluginInfo;
	TRipGrepperDockableForm.CreateInstance; // saved layout loading ...
	G_DRipExtension := self;
	CreateMenu;
	TDebugUtils.DebugMessage('TDRipExtension.Create end');
end;

destructor TDRipExtension.Destroy;
begin
	TDebugUtils.DebugMessage('TDRipExtension.Destroy');
	RemovePluginInfo;
	TDebugUtils.DebugMessage('TDRipExtension.Destroy FDockableForm.Free');
	FDockableForm.Free;
	TDebugUtils.DebugMessage('TDRipExtension.Destroy G_DripMenu.Free');
	G_DripMenu.Free;
	G_DRipExtension := nil;
	FIconBmp.Free;
	inherited;
end;

function TDRipExtension.AddToImageList : Integer;
begin
	IconBmp.LoadFromResourceName(hInstance, 'about_icon');
	Result := IOTAUTils.AddToImageList(IconBmp, 'DRipExtension icon');
end;

procedure TDRipExtension.DoDripGrepperMenuClick(Sender : TObject);
begin
	ShowDripGrepperForm;
end;

// IOTAWizard
procedure TDRipExtension.Execute;
begin
	ShowMessage(EXTENSION_NAME + CRLF + HOME_PAGE);
end;

function TDRipExtension.GetIconBmp : Vcl.Graphics.TBitmap;
begin
	if not Assigned(FIconBmp) then begin
		FIconBmp := Vcl.Graphics.TBitmap.Create();
	end;
	result := FIconBmp;
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
	Result := CAPTION_EXTENSION_MENU;
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

procedure TDRipExtension.InitPluginInfo;
var
	bmpHandle : HBITMAP;
	aFileName : array [0 .. MAX_PATH] of char;
	dFileAge : TDateTime;
	aLicenseStatus : string;
	sExeVersion : string;
begin
	TDebugUtils.DebugMessage('TDRipExtension.InitPluginInfo');
	GetModuleFileName(hInstance, aFileName, MAX_PATH);
	System.SysUtils.FileAge(aFileName, dFileAge);
	aLicenseStatus := FormatDateTime('dd.mm.yy - h:nn', dFileAge);
	sExeVersion := TFileUtils.GetAppVersion(aFileName);
	bmpHandle := LoadBitmap(hInstance, 'splash_icon');
	(SplashScreenServices as IOTASplashScreenServices).AddPluginBitmap(EXTENSION_NAME, bmpHandle, False, '', sExeVersion);

	bmpHandle := LoadBitmap(hInstance, 'about_icon');
	FiPluginIndexAbout := (BorlandIDEServices as IOTAAboutBoxServices).AddPluginInfo(EXTENSION_NAME, EXTENSION_NAME + CRLF + HOME_PAGE,
		bmpHandle, False, aLicenseStatus, sExeVersion);
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

procedure TDRipExtension.RemovePluginInfo;
begin
	if FiPluginIndexAbout > 0 then
		(BorlandIDEServices as IOTAAboutBoxServices).RemovePluginInfo(FiPluginIndexAbout);
end;

procedure TDRipExtension.ShowDripGrepperForm;
begin
	TDebugUtils.DebugMessage('TDRipExtension.ShowDripGrepperForm');
	TRipGrepperDockableForm.ShowDockableFormAndSearch();
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
