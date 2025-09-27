unit DRipExtension.Menu;

interface

uses
	Vcl.Menus,
	Vcl.Graphics,
	System.Classes,
	Vcl.ActnList,
	RipGrepper.Settings.ExtensionSettings,
	RipGrepper.Settings.RipGrepperSettings,
	ArrayEx;

type
	TDripExtensionMenu = class(TObject)
		const
			DRIP_MENUITEM_NAME = 'DRipExpert_MenuItem';
			DRIP_MENUITEM_DRIPGREPPER_NAME = 'DRipExpert_DripGrepper_MenuItem';
			DRIP_MENUITEM_OPENWITH_NAME = 'DRipExpert_OpenWith_MenuItem';
			DRIP_MENUITEM_SETTINGS_NAME = 'DRipExpert_Settings_MenuItem';

			IDE_TOOLSMENU = 'ToolsMenu';
			IDE_TOOLS_TOOLS_ITEM = 'ToolsToolsItem';
			IDE_TOOLS_DEBUGGER_OPTIONS_ITEM = 'ToolsDebuggerOptionsItem';

		strict private
			class var FSettings : TRipGrepperSettings;
			class constructor Create;
			class destructor Destroy;
			class function addToImageList(const _resourceName : string): Integer;
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
	ToolsAPI;

var
	G_DripMenu : TMenuItem;

class constructor TDripExtensionMenu.Create;
begin
	inherited;
end;

class destructor TDripExtensionMenu.Destroy;
var
	dbgMsg : TDebugMsgBeginEnd;
begin
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.Destroy');
	FreeAndNil(G_DripMenu);
end;

class function TDripExtensionMenu.addToImageList(const _resourceName : string):
	Integer;
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
	dripMenuArr : TArrayEx<TMenuItem>;
	extSettings : TRipGrepperExtensionSettings;
	dbgMsg : TDebugMsgBeginEnd;
	actionArr : TArrayEx<TAction>;
begin
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.CreateMenu');

	removeExtensionMenu();

	FSettings := settings;
	extSettings := settings.SearchFormSettings.ExtensionSettings;
	dbgMsg.Msg('ReadFile');
	extSettings.ReadFile();
	dbgMsg.Msg('LoadFromDict');
	extSettings.LoadFromDict();

	// First create actions
	actionArr := createActions(extSettings);

	// Then create menu items from actions
	dripMenuArr := createMenuFromActions(actionArr);
	dbgMsg.MsgFmt('Vcl.Menus.NewSubMenu %s', [_sMenuText]);
	G_DripMenu := Vcl.Menus.NewSubMenu(_sMenuText, 0, DRIP_MENUITEM_NAME, dripMenuArr.Items);
	G_DripMenu.ImageIndex := addToImageList('splash_icon');
	dbgMsg.MsgFmt('G_DripMenu Name %s ImageIndex %d', [G_DripMenu.Name, G_DripMenu.ImageIndex]);
	G_DripMenu.OnClick := dripMenuClick;
	insertIntoToolsMenu(G_DripMenu);

	// Add actions to IDE toolbar
	addActionsToIdeToolbar(actionArr);

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
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.dripMenuClick');
	var
	bEnabled := Assigned(IOTAUtils.GxOtaGetCurrentProject());
	G_DripMenu.Items[0].Enabled := bEnabled;
	dbgMsg.MsgFmt('%s img=%d enabled=%s', [G_DripMenu.Items[0].Caption, G_DripMenu.Items[0].ImageIndex, BoolToStr(bEnabled, True)]);
	var
		projPathGetter : IIdeProjectPathHelper := TIdeProjectPathHelper.Create();
	bEnabled := not projPathGetter.GetCurrentSourceFile.IsEmpty;
	G_DripMenu.Items[1].Enabled := bEnabled;
	dbgMsg.MsgFmt('%s enabled = %s', [G_DripMenu.Items[1].Caption, BoolToStr(bEnabled, True)]);

	// Settings menu item is always enabled (index 3 because of separator at index 2)
	G_DripMenu.Items[3].Enabled := True;
	dbgMsg.MsgFmt('%s enabled = %s', [G_DripMenu.Items[3].Caption, BoolToStr(True, True)]);
end;

class procedure TDripExtensionMenu.removeExtensionMenu();
var
	toolsMenu : TMenuItem;
	dripMenuItem : TMenuItem;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.removeExtensionMenu');

	toolsMenu := IOTAUtils.FindInMainMenu(IDE_TOOLSMENU);
	if toolsMenu <> nil then begin
		dripMenuItem := IOTAUtils.FindInMenu(toolsMenu, DRIP_MENUITEM_NAME);
		if dripMenuItem <> nil then begin
			dbgMsg.Msg('remove - ' + dripMenuItem.Caption);
			toolsMenu.Remove(dripMenuItem);
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
	action.Name := 'DRipGrepperOpenWithAction';
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
	action.Name := 'DRipGrepperSearchAction';
	action.Caption := 'Search with DripGrepper';
	action.Hint := 'Search with DripGrepper';
	action.Category := 'DRipExtensions';
	action.ShortCut := TextToShortcut(_extSettings.SearchSelectedShortcut);
	action.OnExecute := doDripGrepperMenuClick;
	action.ImageIndex := addToImageList('dripgrepper_icon');
	Result.Add(action);
	dbgMsg.MsgFmt('Created Search action with ImageIndex %d', [action.ImageIndex]);

	// Create Settings action
	action := TAction.Create(Application.MainForm);
	action.Name := 'DRipGrepperSettingsAction';
	action.Caption := 'Configure DRipExtensions';
	action.Hint := 'Configure DRipExtensions';
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
		menuItem.Name := action.Name + '_MenuItem';
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
