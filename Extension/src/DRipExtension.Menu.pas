unit DRipExtension.Menu;

interface

uses
	Vcl.Menus,
	Vcl.Graphics,
	System.Classes,
	RipGrepper.Settings.ExtensionSettings,
	RipGrepper.Settings.RipGrepperSettings;

type
	TDripExtensionMenu = class(TObject)
		const
			DRIP_MENUITEM_NAME = 'DRipExpert_MenuItem';
			DRIP_MENUITEM_DRIPGREPPER_NAME = 'DRipExpert_DripGrepper_MenuItem';
			DRIP_MENUITEM_OPENWITH_NAME = 'DRipExpert_OpenWith_MenuItem';
			DRIP_MENUITEM_SETTINGS_NAME = 'DRipExpert_Settings_MenuItem';

		strict private
			class var FIconBmp : TBitmap;
			class var FSettings : TRipGrepperSettings;
			class constructor Create;
			class destructor Destroy;
			class function AddToImageList(const _resourceName : string) : Integer;
			class function CreateSubMenuItem(const _MenuName, _Caption, _icoResource, _scText, _defScText : string; _onClick : TNotifyEvent)
				: TMenuItem;
			class procedure DoDripGrepperMenuClick(Sender : TObject);
			class procedure DoOpenWithMenuClick(Sender : TObject);
			class procedure DoSettingsMenuClick(Sender : TObject);
			class procedure DripMenuClick(Sender : TObject);
			class procedure RemoveExtensionMenu;
			class procedure ShowDripGrepperForm;
			class procedure ShowSettingsForm;

		public
			class procedure CreateMenu(const _sMenuText : string; settings : TRipGrepperSettings);
	end;

implementation

uses
	System.SysUtils,
	RipGrepper.Common.IOTAUtils,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	ArrayEx,
	RipGrepper.OpenWith.Params,
	RipGrepper.OpenWith,
	DripExtension.UI.DockableForm,
	RipGrepper.UI.Settings.ConfigForm;

var
	G_DripMenu : TMenuItem;

class constructor TDripExtensionMenu.Create;
begin
	inherited;
	FIconBmp := Vcl.Graphics.TBitmap.Create();
end;

class destructor TDripExtensionMenu.Destroy;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.Destroy');
	FreeAndNil(G_DripMenu);
end;

class function TDripExtensionMenu.AddToImageList(const _resourceName : string) : Integer;
begin
	FIconBmp.LoadFromResourceName(hInstance, _resourceName);
	// icon resource can be only bmp
	Result := IOTAUTils.AddToImageList(FIconBmp, _resourceName);
end;

class procedure TDripExtensionMenu.CreateMenu(const _sMenuText : string; settings : TRipGrepperSettings);
var
	Item : TMenuItem;
	DripMenuItems : TArrayEx<TMenuItem>;
	iPos : integer;
	extSettings : TRipGrepperExtensionSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.CreateMenu');

	// if Assigned(G_DripMenu) then
	// exit;

	RemoveExtensionMenu();

	FSettings := settings;
	extSettings := settings.SearchFormSettings.ExtensionSettings;
	dbgMsg.Msg('ReadFile');
	extSettings.ReadFile();
	dbgMsg.Msg('LoadFromDict');
	extSettings.LoadFromDict();

	dbgMsg.MsgFmt('DripMenuItems.Add %s', [MENU_ITEM_SEARCH_WITH_DRIPGREPPER]);
	DripMenuItems.Add(CreateSubMenuItem(DRIP_MENUITEM_DRIPGREPPER_NAME,
		{ } MENU_ITEM_SEARCH_WITH_DRIPGREPPER,
		{ } 'dripgrepper_icon',
		{ } extSettings.SearchSelectedShortcut,
		{ } TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH,
		{ } DoDripGrepperMenuClick));

	dbgMsg.MsgFmt('DripMenuItems.Add %s', [MENU_ITEM_OPEN_WITH]);
	DripMenuItems.Add(
		{ } CreateSubMenuItem(DRIP_MENUITEM_OPENWITH_NAME,
		{ } MENU_ITEM_OPEN_WITH,
		{ } 'openwith_icon',
		{ } extSettings.OpenWithShortcut,
		{ } TDefaults.EXT_DEFAULT_SHORTCUT_OPEN_WITH,
		{ } DoOpenWithMenuClick));

	// Add separator before settings
	dbgMsg.Msg('DripMenuItems.Add separator');
	DripMenuItems.Add(Vcl.Menus.NewLine);

	dbgMsg.MsgFmt('DripMenuItems.Add %s', [MENU_ITEM_SETTINGS]);
	DripMenuItems.Add(
		{ } CreateSubMenuItem(DRIP_MENUITEM_SETTINGS_NAME,
		{ } MENU_ITEM_SETTINGS,
		{ } 'settings_icon',
		{ } extSettings.SettingsShortcut,
		{ } TDefaults.EXT_DEFAULT_SHORTCUT_SETTINGS,
		{ } DoSettingsMenuClick));

	dbgMsg.MsgFmt('Vcl.Menus.NewSubMenu %s', [_sMenuText]);
	G_DripMenu := Vcl.Menus.NewSubMenu(_sMenuText, 0, DRIP_MENUITEM_NAME, DripMenuItems.Items);

	G_DripMenu.ImageIndex := AddToImageList('splash_icon');
	dbgMsg.MsgFmt('G_DripMenu Name %s ImageIndex %d', [G_DripMenu.Name, G_DripMenu.ImageIndex]);
	// dbgMsg.MsgFmt('G_DripMenu.ImageIndex %d', [G_DripMenu.ImageIndex]);

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

class function TDripExtensionMenu.CreateSubMenuItem(const _MenuName, _Caption, _icoResource, _scText, _defScText : string;
	_onClick : TNotifyEvent) : TMenuItem;
var
	sc : TShortCut;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.CreateSubMenuItem');
	dbgMsg.Msg('shortcut ' + _scText);

	sc := TextToShortCut(_scText);
	if sc = 0 then begin
		sc := TextToShortCut(_defScText);
	end;

	Result := Vcl.Menus.NewItem(_Caption, sc, False, True, _onClick, 0, _MenuName);
	if (not _icoResource.IsEmpty) then begin
		Result.ImageIndex := AddToImageList(_icoResource);
	end;
	// Result.ImageIndex
	dbgMsg.MsgFmt('NewItem ''%s - %s 0x%x''', [_MenuName, _Caption, sc]);
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
	dbgMsg.MsgFmt('TDripExtensionMenu.DoOpenWithMenuClick %s', [owp.ToString]);
	if not owp.IsEmpty then begin
		TOpenWith.Execute(owp);
	end;
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
	bEnabled := Assigned(IOTAUTils.GxOtaGetCurrentProject());
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
const
	IDE_TOOLSMENU = 'ToolsMenu';
var
	toolsMenu : TMenuItem;
	dripMenuItem : TMenuItem;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.RemoveExtensionMenu');

	toolsMenu := IOTAUTils.FindInMainMenu(IDE_TOOLSMENU);
	if toolsMenu <> nil then begin
		dripMenuItem := IOTAUTils.FindInMenu(toolsMenu, DRIP_MENUITEM_NAME);
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

end.
