unit DRipExtension.Menu;

interface

uses
	Vcl.Menus,
	Vcl.Graphics,
	System.Classes,
	Vcl.ActnList,
	RipGrepper.Settings.ExtensionSettings,
	RipGrepper.Settings.RipGrepperSettings,
	ArrayEx,
	Vcl.Controls,
	Spring;

type
	// {$DEFINE IMAGELIST_WORKAROUND_DELPHI12} // doesn't work as expected
	TDripExtensionMenu = class(TObject)
		const
			DRIP_EXTENSIONS = 'DRipExtensions';
			MENUITEM_POSTFIX = '_MenuItem';
			DRIP_MENUITEM_NAME = DRIP_EXTENSIONS + MENUITEM_POSTFIX;
			DRIP_MENUITEM_DRIPGREPPER_NAME = DRIP_EXTENSIONS + '_DripGrepper' + MENUITEM_POSTFIX;
			DRIP_MENUITEM_OPENWITH_NAME = DRIP_EXTENSIONS + '_OpenWith' + MENUITEM_POSTFIX;
			DRIP_MENUITEM_SETTINGS_NAME = DRIP_EXTENSIONS + '_Settings' + MENUITEM_POSTFIX;

			ACTION_PREFIX = DRIP_EXTENSIONS + '_';
			IDE_TOOLSMENU = 'ToolsMenu';
			IDE_TOOLS_TOOLS_ITEM = 'ToolsToolsItem';
			IDE_TOOLS_DEBUGGER_OPTIONS_ITEM = 'ToolsDebuggerOptionsItem';

			ACTION_POSTFIX = '_Action';
			ICON_POSTFIX = '_Icon';

		strict private
			class var FSettings : TRipGrepperSettings;
			class var FActions : TArrayEx<TAction>;
			class var FDripExtensionsMenuItems : TArrayEx<TMenuItem>;
			class constructor Create;
			class destructor Destroy;
			{$IFDEF IMAGELIST_WORKAROUND_DELPHI12}
			class var FTempImageList : TImageList;
			class function addToTempImageList(const _resourceName : string) : Integer;
			class function addToIdeImageList() : Integer; overload;
			{$ENDIF}
			class procedure doDripGrepperMenuClick(Sender : TObject);
			class procedure doOpenWithMenuClick(Sender : TObject);
			class procedure doSettingsMenuClick(Sender : TObject);
			class procedure dripMenuClick(Sender : TObject);
			class procedure removeExtensionMenu();
			class procedure showDripGrepperForm();
			class procedure showSettingsForm();
			class function createActions(_extSettings : TRipGrepperExtensionSettings) : TArrayEx<TAction>;
			class function createMenuFromActions(_actions : TArrayEx<TAction>) : TArrayEx<TMenuItem>;
			class procedure addActionsToIdeToolbar(_actions : TArrayEx<TAction>);
			class procedure insertIntoToolsMenu(_submenu : TMenuItem);
			class function addToImageList(const _resourceName : string) : Integer;
			class procedure enableMenuItem(const _sName : string; const _bEnabled : Boolean);
			class function getMenuItemIdxByName(const _sName : string) : integer;

		public
			class procedure CreateMenu(const _sMenuText : string; settings : TRipGrepperSettings);
	end;

implementation

uses
	System.SysUtils,
	Vcl.Forms,
	RipGrepper.Common.IOTAUtils,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	RipGrepper.OpenWith.Params,
	RipGrepper.OpenWith,
	DripExtension.UI.DockableForm,
	RipGrepper.UI.Settings.ConfigForm,
	ToolsAPI,
	System.Generics.Defaults;

var
	G_DripMenu : TMenuItem;

class constructor TDripExtensionMenu.Create;
begin
	inherited;
	{$IFDEF IMAGELIST_WORKAROUND_DELPHI12}
	FTempImageList := TImageList.Create(nil);
	{$ENDIF}
end;

class destructor TDripExtensionMenu.Destroy;
var
	dbgMsg : TDebugMsgBeginEnd;
begin
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.Destroy');
	FreeAndNil(G_DripMenu);
	{$IFDEF IMAGELIST_WORKAROUND_DELPHI12}
	FTempImageList.Free;
	{$ENDIF}
	inherited;
end;

class function TDripExtensionMenu.addToImageList(const _resourceName : string) : Integer;
var
	IconBmp : TBitmap;
	Services : INTAServices;
	UniqueIdent : string;
	dbgMsg : TDebugMsgBeginEnd;
	TranspColor : TColor;
begin
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.addToImageList');
	IconBmp := TBitmap.Create;
	try
		IconBmp.LoadFromResourceName(hInstance, _resourceName);

		// Force transparent color to be the bottom-left pixel (standard approach)
		TranspColor := IconBmp.Canvas.Pixels[0, IconBmp.Height - 1];
		IconBmp.TransparentColor := TranspColor;
		IconBmp.Transparent := True;

		dbgMsg.MsgFmt('Loaded bitmap %s: %dx%d, TransparentColor: %d', [_resourceName, IconBmp.Width, IconBmp.Height, TranspColor]);

		// Create a unique identifier to avoid conflicts - use simple counter approach
		UniqueIdent := 'DRipExtension_' + _resourceName;

		// Add directly to IDE image list with unique identifier
		if Supports(BorlandIDEServices, INTAServices, Services) then begin
			Result := Services.AddMasked(IconBmp, TranspColor, UniqueIdent);
			dbgMsg.MsgFmt('Added %s to IDE ImageList with identifier %s, got index %d', [_resourceName, UniqueIdent, Result]);
		end else begin
			Result := -1;
			dbgMsg.ErrorMsg('Could not get INTAServices');
		end;
	finally
		IconBmp.Free;
	end;
end;

class procedure TDripExtensionMenu.CreateMenu(const _sMenuText : string; settings : TRipGrepperSettings);
var
	extSettings : TRipGrepperExtensionSettings;
	dbgMsg : TDebugMsgBeginEnd;
begin
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.CreateMenu');

	// Always remove existing menu first
	removeExtensionMenu();

	FSettings := settings;
	extSettings := settings.SearchFormSettings.ExtensionSettings;
	extSettings.ReadFile();
	extSettings.LoadFromDict();

	// Create new actions (arrays are now empty after removeExtensionMenu)
	FActions := createActions(extSettings);

	// Create new menu items from actions
	FDripExtensionsMenuItems := createMenuFromActions(FActions);

	dbgMsg.MsgFmt('Vcl.Menus.NewSubMenu %s', [_sMenuText]);
	G_DripMenu := Vcl.Menus.NewSubMenu(_sMenuText, 0, DRIP_MENUITEM_NAME, FDripExtensionsMenuItems.Items);
	G_DripMenu.ImageIndex := addToImageList('splash_icon');
	dbgMsg.MsgFmt('G_DripMenu Name %s ImageIndex %d', [G_DripMenu.Name, G_DripMenu.ImageIndex]);
	G_DripMenu.OnClick := dripMenuClick;
	insertIntoToolsMenu(G_DripMenu);

	// Add actions to IDE toolbar
	addActionsToIdeToolbar(FActions);
end;

class procedure TDripExtensionMenu.doDripGrepperMenuClick(Sender : TObject);
begin
	showDripGrepperForm;
end;

class procedure TDripExtensionMenu.doOpenWithMenuClick(Sender : TObject);
var
	owp : TOpenWithParams;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.doOpenWithMenuClick');
	owp := TOpenWithParams.GetParamsOfActiveFileInDelphiIde();

	if owp.IsEmpty then begin
		Exit;
	end;

	dbgMsg.MsgFmt('TDripExtensionMenu.doOpenWithMenuClick %s', [owp.ToString]);

	TOpenWith.Execute(owp);

end;

class procedure TDripExtensionMenu.doSettingsMenuClick(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.doSettingsMenuClick');
	showSettingsForm;
end;

class procedure TDripExtensionMenu.dripMenuClick(Sender : TObject);
var
	projPathGetter : IIdeProjectPathHelper;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.dripMenuClick');

	enableMenuItem(DRIP_MENUITEM_DRIPGREPPER_NAME, Assigned(IOTAUtils.GxOtaGetCurrentProject()));

	projPathGetter := TIdeProjectPathHelper.Create();
	enableMenuItem(DRIP_MENUITEM_OPENWITH_NAME, not projPathGetter.GetCurrentSourceFile.IsEmpty);

	// Settings menu item is always enabled
	enableMenuItem(DRIP_MENUITEM_SETTINGS_NAME, True);
end;

class procedure TDripExtensionMenu.removeExtensionMenu();
var
	toolsMenu : TMenuItem;
	dripMenuItem : TMenuItem;
	action : TAction;
	menuItem : TMenuItem;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.removeExtensionMenu');

	// Remove existing actions first
	for action in FActions do begin
		if Assigned(action) then begin
			dbgMsg.MsgFmt('Removing action %s', [action.Name]);
			action.Free;
		end;
	end;
	FActions.Clear;

	// Remove existing menu items
	for menuItem in FDripExtensionsMenuItems do begin
		if Assigned(menuItem) then begin
			dbgMsg.MsgFmt('Removing menu item %s', [menuItem.Name]);
			menuItem.Free;
		end;
	end;
	FDripExtensionsMenuItems.Clear;

	// Remove from Tools menu
	toolsMenu := IOTAUtils.FindInMainMenu(IDE_TOOLSMENU);
	if toolsMenu <> nil then begin
		dripMenuItem := IOTAUtils.FindInMenu(toolsMenu, DRIP_MENUITEM_NAME);
		if dripMenuItem <> nil then begin
			dbgMsg.Msg('remove - ' + dripMenuItem.Caption);
			toolsMenu.Remove(dripMenuItem);
			// Don't free here - it will be freed when G_DripMenu is freed
		end else begin
			dbgMsg.ErrorMsg(DRIP_MENUITEM_NAME + ' not found.');
		end;
	end else begin
		dbgMsg.ErrorMsg(IDE_TOOLSMENU + ' not found.');
	end;
end;

class procedure TDripExtensionMenu.showDripGrepperForm();
begin
	TDebugUtils.DebugMessage('TDripExtensionMenu.showDripGrepperForm');
	TRipGrepperDockableForm.ShowDockableFormAndSearch();
end;

class procedure TDripExtensionMenu.showSettingsForm();
begin
	TDebugUtils.DebugMessage('TDripExtensionMenu.showSettingsForm');
	var
	settingsForm := TConfigForm.Create(FSettings);
	try
		settingsForm.ShowModal;
	finally
		settingsForm.Free;
	end;
end;

class function TDripExtensionMenu.createActions(_extSettings : TRipGrepperExtensionSettings) : TArrayEx<TAction>;
var
	action : TAction;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.createActions');

	// Create Open With action
	action := TAction.Create(Application.MainForm);
	action.Name := ACTION_PREFIX + 'OpenWith' + ACTION_POSTFIX;
	action.Caption := 'Open With';
	action.Hint := 'Open With';
	action.Category := 'DRipExtensions';
	action.ShortCut := TextToShortcut(_extSettings.OpenWithShortcut);
	action.OnExecute := doOpenWithMenuClick;
	action.ImageIndex := addToImageList('openwith_icon');
	Result.Add(action);
	dbgMsg.MsgFmt('Created Open With action with ImageIndex %d', [action.ImageIndex]);

	// Create DripGrepper Search action
	action := TAction.Create(Application.MainForm);
	action.Name := ACTION_PREFIX + 'DRipGrepper' + ACTION_POSTFIX;
	action.Caption := 'Search with DRipGrepper';
	action.Hint := 'Search with DRipGrepper';
	action.Category := 'DRipExtensions';
	action.ShortCut := TextToShortcut(_extSettings.SearchSelectedShortcut);
	action.OnExecute := doDripGrepperMenuClick;
	action.ImageIndex := addToImageList('dripgrepper_icon');
	Result.Add(action);
	dbgMsg.MsgFmt('Created Search action with ImageIndex %d', [action.ImageIndex]);

	// Create Settings action
	action := TAction.Create(Application.MainForm);
	action.Name := ACTION_PREFIX + 'Settings' + ACTION_POSTFIX;
	action.Caption := 'Settings';
	action.Hint := 'DRipExtensions Settings...';
	action.Category := 'DRipExtensions';
	action.ShortCut := TextToShortcut(_extSettings.SettingsShortcut);
	action.OnExecute := doSettingsMenuClick;
	action.ImageIndex := addToImageList('settings_icon');
	Result.Add(action);
	dbgMsg.MsgFmt('Created Settings action with ImageIndex %d', [action.ImageIndex]);
end;

class function TDripExtensionMenu.createMenuFromActions(_actions : TArrayEx<TAction>) : TArrayEx<TMenuItem>;
var
	action : TAction;
	menuItem : TMenuItem;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.createMenuFromActions');

	for var i := 0 to _actions.Count - 1 do begin
		action := _actions[i];

		// Create menu item from action
		menuItem := TMenuItem.Create(Application.MainForm);
		menuItem.Name := string(action.Name).Replace(ACTION_POSTFIX, MENUITEM_POSTFIX);
		menuItem.Caption := action.Caption;
		menuItem.Hint := action.Hint;
		menuItem.ShortCut := action.ShortCut;
		menuItem.ImageIndex := action.ImageIndex;
		menuItem.OnClick := action.OnExecute;

		Result.Add(menuItem);
		dbgMsg.MsgFmt('Created menu item %s with ImageIndex %d', [menuItem.Caption, menuItem.ImageIndex]);

		// Add separator before last settings menu
		if i = _actions.Count - 2 then begin
			Result.Add(Vcl.Menus.NewLine);
			dbgMsg.Msg('Added separator after Open With');
		end;
	end;
end;

class procedure TDripExtensionMenu.addActionsToIdeToolbar(_actions : TArrayEx<TAction>);
var
	NTAServices : INTAServices;
	actionList : TCustomActionList;
	action : TAction;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.addActionsToIdeToolbar');

	// Get IDE services for toolbar integration
	if Supports(BorlandIDEServices, INTAServices, NTAServices) then begin
		actionList := NTAServices.actionList;
		dbgMsg.Msg('Got IDE actionList');
		try
			for var i := 0 to _actions.Count - 1 do begin
				action := _actions[i];
				action.actionList := actionList;
				dbgMsg.MsgFmt('Added %s to IDE actionList with ImageIndex %d', [action.Caption, action.ImageIndex]);
			end;

			dbgMsg.Msg('All actions added to IDE - they should now appear in Tools > Customize');
		except
			on E : Exception do
				dbgMsg.ErrorMsg('Exception adding actions: ' + E.Message);
		end;
	end else begin
		dbgMsg.ErrorMsg('Could not get INTAServices - toolbar integration not available');
	end;
end;

class procedure TDripExtensionMenu.enableMenuItem(const _sName : string; const _bEnabled : Boolean);
var
	idx : integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.enableMenuItem');

	idx := getMenuItemIdxByName(_sName);
	if idx < FDripExtensionsMenuItems.Count then begin
		FDripExtensionsMenuItems[idx].Enabled := _bEnabled;
		dbgMsg.MsgFmt('%s idx=%d img=%d enabled=%s',
			{ } [FDripExtensionsMenuItems[idx].Caption, idx, FDripExtensionsMenuItems[idx].ImageIndex, BoolToStr(_bEnabled, True)]);
	end else begin
		dbgMsg.MsgFmt('%s not found idx=%d ', [_sName, idx]);
	end;
end;

class function TDripExtensionMenu.getMenuItemIdxByName(const _sName : string) : integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.getMenuItemIdxByName');
	Result := -1;

	for var i : Integer := 0 to FDripExtensionsMenuItems.Count - 1 do begin
			dbgMsg.MsgFmt('Check %s at index %d', [FDripExtensionsMenuItems[i].Name, i]);
		if SameText(FDripExtensionsMenuItems[i].Name, _sName) then begin
			Result := i;
			dbgMsg.MsgFmt('Found %s at index %d', [_sName, Result]);
			break;
		end;
	end;
	dbgMsg.MsgFmtIf(Result = -1, 'Item %s not found, Result := %d', [_sName, Result]);
end;

{$IFDEF IMAGELIST_WORKAROUND_DELPHI12}

class function TDripExtensionMenu.addToIdeImageList() : Integer;
var
	Services : INTAServices;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.addToIdeImageList');
	// Add directly to IDE image list with unique identifier
	if Supports(BorlandIDEServices, INTAServices, Services) then begin
		// This is a workaround for a bug in the Delphi 12 INTAServices.AddMasked OTAPI
		// Icons must be added to IDE image list separately from action creation

		Result := Services.AddImages(FTempImageList, DRIP_EXTENSIONS);
		dbgMsg.MsgFmt('FTempImageList added to IDE ImageList, got index %d', [Result]);
	end else begin
		Result := -1;
		dbgMsg.ErrorMsg('Could not get INTAServices');
	end;
end;

class function TDripExtensionMenu.addToTempImageList(const _resourceName : string) : Integer;
var
	iconBmp : IShared<TBitmap>;
	dbgMsg : TDebugMsgBeginEnd;
	transpColor : TColor;
begin
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.addToImageList');
	iconBmp := Shared.Make<TBitmap>();

	iconBmp.LoadFromResourceName(hInstance, _resourceName);

	// Force transparent color to be the bottom-left pixel (standard approach)
	transpColor := iconBmp.Canvas.Pixels[0, iconBmp.Height - 1];
	iconBmp.TransparentColor := transpColor;
	iconBmp.Transparent := True;

	dbgMsg.MsgFmt('Loaded bitmap %s: %dx%d, TransparentColor: %d', [_resourceName, iconBmp.Width, iconBmp.Height, transpColor]);

	Result := FTempImageList.AddMasked(iconBmp, transpColor);
	dbgMsg.MsgFmt('Added %s to IDE ImageList, got index %d', [_resourceName, Result]);
end;
{$ENDIF}

class procedure TDripExtensionMenu.insertIntoToolsMenu(_submenu : TMenuItem);
var
	menuItem : TMenuItem;
	iPos : integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.insertIntoToolsMenu');

	menuItem := IOTAUtils.FindMenuItem(IDE_TOOLSMENU);
	if menuItem <> nil then begin
		iPos := menuItem.IndexOf(IOTAUtils.FindMenuItem(IDE_TOOLS_DEBUGGER_OPTIONS_ITEM));
		dbgMsg.Msg('iPos ' + iPos.ToString);
		if iPos = -1 then
			iPos := menuItem.IndexOf(IOTAUtils.FindMenuItem(IDE_TOOLS_TOOLS_ITEM)) - 1;
		if iPos >= 0 then begin
			dbgMsg.Msg(IDE_TOOLS_TOOLS_ITEM + ' iPos ' + iPos.ToString);
			menuItem.Insert(iPos + 1, G_DripMenu);
		end else begin
			dbgMsg.Msg(IDE_TOOLS_TOOLS_ITEM + ' not found');
			menuItem.Insert(1, G_DripMenu);
		end;
	end;
end;

end.
