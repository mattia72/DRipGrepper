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
			class function AddToImageList(const _resourceName : string) : Integer;
			class procedure DoDripGrepperMenuClick(Sender : TObject);
			class procedure DoOpenWithMenuClick(Sender : TObject);
			class procedure DoSettingsMenuClick(Sender : TObject);
			class procedure DripMenuClick(Sender : TObject);
			class procedure RemoveExtensionMenu;
			class procedure ShowDripGrepperForm;
			class procedure ShowSettingsForm;

		private
			class function CreateActions(_extSettings : TRipGrepperExtensionSettings) : TArrayEx<TAction>;
			class function CreateMenuFromActions(_actions : TArrayEx<TAction>) : TArrayEx<TMenuItem>;
			class procedure AddActionsToIdeToolbar(_actions : TArrayEx<TAction>);
			class procedure InsertIntoToolsMenu(_submenu : TMenuItem);

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

class function TDripExtensionMenu.AddToImageList(const _resourceName : string) : Integer;
var
	IconBmp : TBitmap;
	Services : INTAServices;
	UniqueIdent : string;
	dbgMsg : TDebugMsgBeginEnd;
	TranspColor : TColor;
begin
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.AddToImageList');
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

	RemoveExtensionMenu();

	FSettings := settings;
	extSettings := settings.SearchFormSettings.ExtensionSettings;
	dbgMsg.Msg('ReadFile');
	extSettings.ReadFile();
	dbgMsg.Msg('LoadFromDict');
	extSettings.LoadFromDict();

    var i := AddToImageList('splash_icon');
	// First create actions
	actionArr := CreateActions(extSettings);

	// Then create menu items from actions
	dripMenuArr := CreateMenuFromActions(actionArr);
	dbgMsg.MsgFmt('Vcl.Menus.NewSubMenu %s', [_sMenuText]);
	G_DripMenu := Vcl.Menus.NewSubMenu(_sMenuText, 0, DRIP_MENUITEM_NAME, dripMenuArr.Items);
	G_DripMenu.ImageIndex := i; //AddToImageList('splash_icon');
	dbgMsg.MsgFmt('G_DripMenu Name %s ImageIndex %d', [G_DripMenu.Name, G_DripMenu.ImageIndex]);
	G_DripMenu.OnClick := DripMenuClick;
	InsertIntoToolsMenu(G_DripMenu);

	// Add actions to IDE toolbar
	AddActionsToIdeToolbar(actionArr);

end;

class procedure TDripExtensionMenu.DoDripGrepperMenuClick(Sender : TObject);
begin
	ShowDripGrepperForm;
end;

class procedure TDripExtensionMenu.DoOpenWithMenuClick(Sender : TObject);
var
	owp : TOpenWithParams;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.DoOpenWithMenuClick');
	owp := TOpenWithParams.GetParamsOfActiveFileInDelphiIde();

	if owp.IsEmpty then begin
		Exit;
	end;

	dbgMsg.MsgFmt('TDripExtensionMenu.DoOpenWithMenuClick %s', [owp.ToString]);

	TOpenWith.Execute(owp);

end;

class procedure TDripExtensionMenu.DoSettingsMenuClick(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.DoSettingsMenuClick');
	ShowSettingsForm;
end;

class procedure TDripExtensionMenu.DripMenuClick(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.DripMenuClick');
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

class procedure TDripExtensionMenu.RemoveExtensionMenu;
var
	toolsMenu : TMenuItem;
	dripMenuItem : TMenuItem;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.RemoveExtensionMenu');

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

class procedure TDripExtensionMenu.ShowDripGrepperForm;
begin
	TDebugUtils.DebugMessage('TDripExtensionMenu.ShowDripGrepperForm');
	TRipGrepperDockableForm.ShowDockableFormAndSearch();
end;

class procedure TDripExtensionMenu.ShowSettingsForm;
begin
	TDebugUtils.DebugMessage('TDripExtensionMenu.ShowSettingsForm');
	var
	settingsForm := TConfigForm.Create(FSettings);
	try
		settingsForm.ShowModal;
	finally
		settingsForm.Free;
	end;
end;

class function TDripExtensionMenu.CreateActions(_extSettings : TRipGrepperExtensionSettings) : TArrayEx<TAction>;
var
	Action : TAction;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.CreateActions');

	// Create Open With Action
	Action := TAction.Create(Application.MainForm);
	Action.Name := 'DRipGrepperOpenWithAction';
	Action.Caption := 'Open With';
	Action.Hint := 'Open current file with external application';
	Action.Category := 'DRipGrepper';
	Action.ShortCut := TextToShortcut(_extSettings.OpenWithShortcut);
	Action.OnExecute := DoOpenWithMenuClick;
	Action.ImageIndex := AddToImageList('openwith_icon');
	Result.Add(Action);
	dbgMsg.MsgFmt('Created Open With action with ImageIndex %d', [Action.ImageIndex]);

	// Create DripGrepper Search Action
	Action := TAction.Create(Application.MainForm);
	Action.Name := 'DRipGrepperSearchAction';
	Action.Caption := 'Search with DripGrepper';
	Action.Hint := 'Search in project files using DripGrepper';
	Action.Category := 'DRipGrepper';
	Action.ShortCut := TextToShortcut(_extSettings.SearchSelectedShortcut);
	Action.OnExecute := DoDripGrepperMenuClick;
	Action.ImageIndex := AddToImageList('dripgrepper_icon');
	Result.Add(Action);
	dbgMsg.MsgFmt('Created Search action with ImageIndex %d', [Action.ImageIndex]);

	// Create Settings Action
	Action := TAction.Create(Application.MainForm);
	Action.Name := 'DRipGrepperSettingsAction';
	Action.Caption := 'DripGrepper Settings';
	Action.Hint := 'Configure DripGrepper settings';
	Action.Category := 'DRipGrepper';
	Action.ShortCut := TextToShortcut(_extSettings.SettingsShortcut);
	Action.OnExecute := DoSettingsMenuClick;
	Action.ImageIndex := AddToImageList('settings_icon');
	Result.Add(Action);
	dbgMsg.MsgFmt('Created Settings action with ImageIndex %d', [Action.ImageIndex]);
end;

class function TDripExtensionMenu.CreateMenuFromActions(_actions : TArrayEx<TAction>) : TArrayEx<TMenuItem>;
var
	Action : TAction;
	MenuItem : TMenuItem;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.CreateMenuFromActions');

	for var i := 0 to _actions.Count - 1 do begin
		Action := _actions[i];

		// Create menu item from action
		MenuItem := TMenuItem.Create(Application.MainForm);
		MenuItem.Name := Action.Name + '_MenuItem';
		MenuItem.Caption := Action.Caption;
		MenuItem.Hint := Action.Hint;
		MenuItem.ImageIndex := Action.ImageIndex;
		MenuItem.OnClick := Action.OnExecute;

		Result.Add(MenuItem);
		dbgMsg.MsgFmt('Created menu item %s with ImageIndex %d', [MenuItem.Caption, MenuItem.ImageIndex]);

		// Add separator after Open With (index 1)
		if i = 1 then begin
			Result.Add(Vcl.Menus.NewLine);
			dbgMsg.Msg('Added separator after Open With');
		end;
	end;
end;

class procedure TDripExtensionMenu.AddActionsToIdeToolbar(_actions : TArrayEx<TAction>);
var
	NTAServices : INTAServices;
	ActionList : TCustomActionList;
	Action : TAction;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.AddActionsToIdeToolbar');

	// Get IDE services for toolbar integration
	if Supports(BorlandIDEServices, INTAServices, NTAServices) then begin
		ActionList := NTAServices.ActionList;
		dbgMsg.Msg('Got IDE ActionList');

		try
			for var i := 0 to _actions.Count - 1 do begin
				Action := _actions[i];
				Action.ActionList := ActionList;
				dbgMsg.MsgFmt('Added %s to IDE ActionList with ImageIndex %d', [Action.Caption, Action.ImageIndex]);
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

class procedure TDripExtensionMenu.InsertIntoToolsMenu(_submenu : TMenuItem);
var
	menuItem : TMenuItem;
	iPos : integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.InsertIntoToolsMenu');

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
