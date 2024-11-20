// https://www.gexperts.org/open-tools-api-faq/#sample
unit DRipExtension.Main;

interface

uses
	ToolsAPI,
	RipGrepper.UI.MainForm,
	DripExtension.UI.DockableForm,
	Vcl.Graphics,
	Vcl.Menus,
	ArrayEx,
	RipGrepper.Settings.ExtensionSettings,
	System.Classes;

type
	// TNotifierObject has stub implementations for the necessary but
	// unused IOTANotifer methods
	TDRipExtension = class(TNotifierObject, IOTAMenuWizard, IOTAWizard)
		const
			DRIP_MENUITEM_NAME = 'DRipExpertMenuItem';
			DRIP_MENUITEM_DRIPGREPPER_NAME = 'DRipExpert_DripGrepper_MenuItem';
			DRIP_MENUITEM_OPENWITH_NAME = 'DRipExpert_OpenWith_MenuItem';

		private
			FKeyNotifier : IOTAKeyboardBinding;
			FKeyBinding : integer;

			FDockableForm : TRipGrepperDockableForm;
			FIconBmp : TBitmap;
			FiPluginIndexAbout : Integer;
			procedure CreateMenu;
			procedure DripMenuClick(Sender : TObject);
			procedure DoDripGrepperMenuClick(Sender : TObject);
			procedure DoOpenWithMenuClick(Sender : TObject);
			function GetIconBmp : Vcl.Graphics.TBitmap;

			procedure InitPluginInfo;
			function CreateSubMenuItem(const _MenuName, _Caption, _icoResource, _scText,
				_defScText : string; _onClick : TNotifyEvent): TMenuItem;
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
			function AddToImageList(const _resourceName : string) : Integer;
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
	RipGrepper.Common.Constants,
	RipGrepper.Common.IOTAUtils,
	RipGrepper.Settings.AppSettings,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.OpenWith,
	RipGrepper.OpenWith.Params,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Tools.FileUtils,
	System.IniFiles,
	System.IOUtils,
	System.SysUtils,
	Vcl.Dialogs,
	Vcl.ImgList,
	Winapi.Windows;

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
	DripMenuItems : TArrayEx<TMenuItem>;
	iPos : integer;
	extSettings : TRipGrepperExtensionSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDRipExtension.CreateMenu');

	// if Assigned(G_DripMenu) then
	// exit;

	RemoveExtensionMenu();
	extSettings := GSettings.SearchFormSettings.ExtensionSettings;
	dbgMsg.Msg('ReadIni');
	extSettings.ReadIni();
	dbgMsg.Msg('LoadFromDict');
	extSettings.LoadFromDict();

	DripMenuItems.Add(CreateSubMenuItem(DRIP_MENUITEM_DRIPGREPPER_NAME,
		{ } 'DRipGrepper...',
		{ } 'dripgrepper_icon',
		{ } extSettings.DripGrepperShortCut,
		{ } TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH,
		{ } DoDripGrepperMenuClick));

	DripMenuItems.Add(
		{ } CreateSubMenuItem(DRIP_MENUITEM_DRIPGREPPER_NAME,
		{ } 'Open With...',
		{ } 'openwith_icon',
		{ } extSettings.OpenWithShortCut,
		{ } TDefaults.EXT_DEFAULT_SHORTCUT_OPEN_WITH,
		{ } DoOpenWithMenuClick));

	G_DripMenu := Vcl.Menus.NewSubMenu(GetMenuText + '...', 0, DRIP_MENUITEM_NAME, DripMenuItems.Items);

	G_DripMenu.ImageIndex := AddToImageList('splash_icon');
	dbgMsg.MsgFmt('G_DripMenu.ImageIndex %d', [G_DripMenu.ImageIndex]);

	G_DripMenu.OnClick := DripMenuClick;

	Item := IOTAUTils.FindMenuItem('ToolsMenu');
	if Item <> nil then begin
		iPos := Item.IndexOf(IOTAUTils.FindMenuItem('ToolsDebuggerOptionsItem'));
		dbgMsg.Msg('iPos ' + iPos.ToString);
		if iPos = -1 then
			iPos := Item.IndexOf(IOTAUTils.FindMenuItem('ToolsToolsItem')) - 1;
		if iPos >= 0 then begin
			dbgMsg.Msg('ToolsToolsItem iPos ' + iPos.ToString);
			Item.Insert(iPos + 1, G_DripMenu)
		end else begin
			dbgMsg.Msg('ToolsToolsItem not found');
			Item.Insert(1, G_DripMenu);
		end;
	end;

end;

constructor TDRipExtension.Create;
begin
	inherited;
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDRipExtension.Create');
	InitPluginInfo;
	TRipGrepperDockableForm.CreateInstance; // saved layout loading ...
	G_DRipExtension := self;
	CreateMenu;
end;

destructor TDRipExtension.Destroy;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDRipExtension.Destroy');
	RemovePluginInfo;
	dbgMsg.Msg('TDRipExtension.Destroy FDockableForm.Free');
	FDockableForm.Free;
	dbgMsg.Msg('TDRipExtension.Destroy G_DripMenu.Free');
	G_DripMenu.Free;
	G_DRipExtension := nil;
	FIconBmp.Free;
	inherited;
end;

function TDRipExtension.AddToImageList(const _resourceName : string) : Integer;
begin
	IconBmp.LoadFromResourceName(hInstance, _resourceName);
	// icon resource can be only bmp
	Result := IOTAUTils.AddToImageList(IconBmp, _resourceName);
end;

procedure TDRipExtension.DoDripGrepperMenuClick(Sender : TObject);
begin
	ShowDripGrepperForm;
end;

procedure TDRipExtension.DoOpenWithMenuClick(Sender : TObject);
var
	owp : TOpenWithParams;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDRipExtension.DoOpenWithMenuClick');
	owp := TOpenWithParams.GetParamsOfActiveFileInDelphiIde();
	dbgMsg.MsgFmt('TDRipExtension.DoOpenWithMenuClick %s', [owp.ToString]);
	if not owp.IsEmpty then begin
		TOpenWith.Execute(owp);
	end;
end;

procedure TDRipExtension.DripMenuClick(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDRipExtension.DripMenuClick');
	var
	bEnabled := Assigned(IOTAUTils.GxOtaGetCurrentProject());
	G_DripMenu.Items[0].Enabled := bEnabled;
	dbgMsg.MsgFmt('%s img=%d enabled=%s', [G_DripMenu.Items[0].Caption, G_DripMenu.Items[0].ImageIndex, BoolToStr(bEnabled, True)]);

	bEnabled := not IOTAUTils.GxOtaGetCurrentSourceFile.IsEmpty;
	G_DripMenu.Items[1].Enabled := bEnabled;
	dbgMsg.MsgFmt('%s enabled = %s', [G_DripMenu.Items[1].Caption, BoolToStr(bEnabled, True)]);
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

function TDRipExtension.CreateSubMenuItem(const _MenuName, _Caption,
	_icoResource, _scText, _defScText : string; _onClick : TNotifyEvent):
	TMenuItem;
var
	sc : TShortCut;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDRipExtension.CreateSubMenuItem');
	dbgMsg.Msg('shortcut ' + _scText);

	sc := TextToShortCut(_scText);
	if sc = 0 then begin
		sc := TextToShortCut(_defScText);
	end;

	Result := Vcl.Menus.NewItem(_Caption, sc, False, True, _onClick, 0, _MenuName);
	Result.ImageIndex := AddToImageList(_icoResource);
	//Result.ImageIndex
    dbgMsg.MsgFmt('NewItem ''%s 0x%x''', [_MenuName, sc]);
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
