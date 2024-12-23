unit DRipExtension.Menu;

interface

uses
	Vcl.Menus,
	Vcl.Graphics,
	System.Classes,
	RipGrepper.Settings.ExtensionSettings;

type
	TDripExtensionMenu = class(TObject)
		const
			DRIP_MENUITEM_NAME = 'DRipExpert_MenuItem';
			DRIP_MENUITEM_DRIPGREPPER_NAME = 'DRipExpert_DripGrepper_MenuItem';
			DRIP_MENUITEM_OPENWITH_NAME = 'DRipExpert_OpenWith_MenuItem';

		strict private
			class var FIconBmp : TBitmap;
			class constructor Create;
			class destructor Destroy;
			class function AddToImageList(const _resourceName : string) : Integer;
			class function CreateSubMenuItem(const _MenuName, _Caption, _icoResource, _scText, _defScText : string; _onClick : TNotifyEvent)
				: TMenuItem;
			class procedure DoDripGrepperMenuClick(Sender : TObject);
			class procedure DoOpenWithMenuClick(Sender : TObject);
			class procedure DripMenuClick(Sender : TObject);
			class procedure RemoveExtensionMenu;
			class procedure ShowDripGrepperForm;

		public
			class procedure CreateMenu(const _sMenuText : string; extSettings : TRipGrepperExtensionSettings);
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
	DripExtension.UI.DockableForm;

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
	dbgMsg := TDebugMsgBeginEnd.New('class TDripExtensionMenu.Destroy G_DripMenu.Free');
	G_DripMenu.Free;
end;

class function TDripExtensionMenu.AddToImageList(const _resourceName : string) : Integer;
begin
	FIconBmp.LoadFromResourceName(hInstance, _resourceName);
	// icon resource can be only bmp
	Result := IOTAUTils.AddToImageList(FIconBmp, _resourceName);
end;

class procedure TDripExtensionMenu.CreateMenu(const _sMenuText : string; extSettings : TRipGrepperExtensionSettings);
var
	Item : TMenuItem;
	DripMenuItems : TArrayEx<TMenuItem>;
	iPos : integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.CreateMenu');

	// if Assigned(G_DripMenu) then
	// exit;

	RemoveExtensionMenu();

	dbgMsg.Msg('ReadIni');
	extSettings.ReadIni();
	dbgMsg.Msg('LoadFromDict');
	extSettings.LoadFromDict();

	DripMenuItems.Add(CreateSubMenuItem(DRIP_MENUITEM_DRIPGREPPER_NAME,
		{ } 'Search with DripGrepper...',
		{ } 'dripgrepper_icon',
		{ } extSettings.SearchSelectedShortcut,
		{ } TDefaults.EXT_DEFAULT_SHORTCUT_SEARCH,
		{ } DoDripGrepperMenuClick));

	DripMenuItems.Add(
		{ } CreateSubMenuItem(DRIP_MENUITEM_DRIPGREPPER_NAME,
		{ } 'Open With...',
		{ } 'openwith_icon',
		{ } extSettings.OpenWithShortCut,
		{ } TDefaults.EXT_DEFAULT_SHORTCUT_OPEN_WITH,
		{ } DoOpenWithMenuClick));

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
	Result.ImageIndex := AddToImageList(_icoResource);
	// Result.ImageIndex
	dbgMsg.MsgFmt('NewItem ''%s 0x%x''', [_MenuName, sc]);
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

class procedure TDripExtensionMenu.DripMenuClick(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDripExtensionMenu.DripMenuClick');
	var
	bEnabled := Assigned(IOTAUTils.GxOtaGetCurrentProject());
	G_DripMenu.Items[0].Enabled := bEnabled;
	dbgMsg.MsgFmt('%s img=%d enabled=%s', [G_DripMenu.Items[0].Caption, G_DripMenu.Items[0].ImageIndex, BoolToStr(bEnabled, True)]);

	bEnabled := not IOTAUTils.GxOtaGetCurrentSourceFile.IsEmpty;
	G_DripMenu.Items[1].Enabled := bEnabled;
	dbgMsg.MsgFmt('%s enabled = %s', [G_DripMenu.Items[1].Caption, BoolToStr(bEnabled, True)]);
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

end.
