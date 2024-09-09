unit DripExtension.UI.DockableForm;

interface

uses
	Vcl.Forms,
	Vcl.ActnList,
	Vcl.ImgList,
	Vcl.Menus,
	Vcl.ComCtrls,
	System.IniFiles,
	DesignIntf,
	System.Classes,
	ToolsAPI,
	RipGrepper.UI.MainForm;

type
	TRipGrepperDockableForm = class(TInterfacedPersistent, INTACustomDockableForm)
		private
			FCaption : string;
			FIdentifier : string;

			class var FInstance : TRipGrepperDockableForm;
			class function GetInstance : TRipGrepperDockableForm; static;

		public
			class function ShowDockableFormAndSearch : TCustomForm;
			/// <summary>
			/// Creates an OleDockForm container for our Frame
			/// </summary>
			class procedure CreateInstance;
			/// <summary>
			/// Returns the Caption for the Dockable Form
			/// </summary>
			function GetCaption : string;
			/// <summary>
			/// Returns a unique identifier for this form.  This should not be translated.
			/// This identifier is used as the section name when saving information for
			/// this form in the desktop state file
			/// </summary>
			function GetIdentifier : string;
			/// <summary>
			/// Returns the class of the frame that you want embedded in the dockable form
			/// </summary>
			function GetFrameClass : TCustomFrameClass;
			/// <summary>
			/// Called when an instance of the specified frame class is created
			/// </summary>
			procedure FrameCreated(AFrame : TCustomFrame);
			/// <summary>
			/// Returns an action list that is used to populate the form's context menu.
			/// By default the context menu will have 2 items that are common to all
			/// dockable forms in the IDE: &quot;Stay on top&quot; and &quot;Dockable&quot;.  If the form
			/// has a toolbar, there will also be a &quot;Toolbar&quot; menu item.  If this
			/// function returns a non-nil action list, the items in the action list will
			/// be added to the menu (above the default items).  To specify sub-menus, use
			/// categories for the actions contained in the Action List.  Any action that
			/// has a Category set, will appear on a sub-menu in the context menu.  The
			/// Caption of the Parent menu will be the Category name.
			/// </summary>
			function GetMenuActionList : TCustomActionList;
			/// <summary>
			/// Returns an image list that contains the images associated with the action
			/// list returned by GetMenuActionList
			/// </summary>
			function GetMenuImageList : TCustomImageList;
			/// <summary>
			/// Called when the popup menu is about to be shown.  This allows further
			/// customization beyond just adding items from an Action List
			/// </summary>
			procedure CustomizePopupMenu(PopupMenu : TPopupMenu);
			/// <summary>
			/// Returns an action list that is used to populate a toolbar on the form.  If
			/// nil is returned, then the dockable form will not have a toolbar.  Items in
			/// the Action List that have '-' as the caption will be added to the toolbar
			/// as a separator
			/// </summary>
			function GetToolBarActionList : TCustomActionList;
			/// <summary>
			/// Returns an image list that contains the images associated with the action
			/// list returned by GetToolbarActionList
			/// </summary>
			function GetToolBarImageList : TCustomImageList;
			/// <summary>
			/// Called after the toolbar has been populated with the Action List returned
			/// from GetToolbarActionList.  This allows further customization beyond just
			/// adding items from an Action List
			/// </summary>
			procedure CustomizeToolBar(ToolBar : TToolBar);
			class procedure DestroyInstance;
			/// <summary>
			/// Called when state for this form is saved to a desktop file.  The Section
			/// paramter is passed in for convenience, but it should match the string
			/// returned by GetIdentifier.  This is only called for INTACustomDockableForm
			/// instances that have been registered using INTAServices.RegisterDockableForm.
			/// IsProject indicates whether the desktop being saved is a project desktop
			/// (as opposed to a dekstop state)
			/// </summary>
			procedure SaveWindowState(Desktop : TCustomIniFile; const Section : string; IsProject : Boolean);
			/// <summary>
			/// Called when state for this form is loaded from a desktop file.  The
			/// Section paramter is passed in for convenience, but it should match the
			/// string returned by GetIdentifier.  This is only called for
			/// INTACustomDockableForm instances that have been registered using
			/// INTAServices.RegisterDockableForm
			/// </summary>
			procedure LoadWindowState(Desktop : TCustomIniFile; const Section : string);
			/// <summary>
			/// Allows the form to control the enabled state of the clipboard commands on
			/// the IDE's &quot;Edit&quot; menu when this view is active
			/// </summary>
			function GetEditState : TEditState;
			/// <summary>
			/// Called when the user uses one of the clipboard commands on the IDE's &quot;Edit&quot;
			/// menu
			/// </summary>
			function EditAction(Action : TEditAction) : Boolean;
			property Caption : string read GetCaption write FCaption;
			property Identifier : string read GetIdentifier;
			property FrameClass : TCustomFrameClass read GetFrameClass;
			class property Instance : TRipGrepperDockableForm read GetInstance;
			property MenuActionList : TCustomActionList read GetMenuActionList;
			property MenuImageList : TCustomImageList read GetMenuImageList;
			property ToolbarActionList : TCustomActionList read GetToolbarActionList;
			property ToolbarImageList : TCustomImageList read GetToolbarImageList;

	end;

implementation

uses
	RipGrepper.Common.Constants,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.UI.ParentFrame,
	System.SysUtils,
	Winapi.Windows,
	System.TypInfo,
	RipGrepper.Common.IOTAUtils,
	Vcl.Controls,
	RipGrepper.Common.Settings.Misc;

{ TRipGrepperDockableForm }

procedure TRipGrepperDockableForm.CustomizePopupMenu(PopupMenu : TPopupMenu);
begin
	PopupMenu := ParentFrame.MainFrame.PopupMenuResult;
end;

procedure TRipGrepperDockableForm.CustomizeToolBar(ToolBar : TToolBar);
begin

end;

function TRipGrepperDockableForm.EditAction(Action : TEditAction) : Boolean;
begin
	Result := False;
end;

procedure TRipGrepperDockableForm.FrameCreated(AFrame : TCustomFrame);
begin
	TDebugUtils.DebugMessage('TRipGrepperDockableForm.FrameCreated');
	ParentFrame.Init;
	if (ParentFrame.Settings.RipGrepParameters.SearchPath.IsEmpty) then begin
		ParentFrame.Settings.RipGrepParameters.SearchPath := IOTAUtils.GxOtaGetCurrentProjectFileName();
		TDebugUtils.DebugMessage('TRipGrepperDockableForm.FrameCreated SearchPath:' + ParentFrame.Settings.RipGrepParameters.SearchPath);
	end;

end;

function TRipGrepperDockableForm.GetCaption : string;
begin
	Result := APPNAME;
end;

function TRipGrepperDockableForm.GetEditState : TEditState;
begin
	Result := [];
end;

function TRipGrepperDockableForm.GetIdentifier : string;
begin
	if FIdentifier.IsEmpty then
		FIdentifier := APPNAME; // + '.' + GetGuid;
	Result := FIdentifier;
end;

function TRipGrepperDockableForm.GetMenuActionList : TCustomActionList;
begin
	Result := nil;
end;

function TRipGrepperDockableForm.GetMenuImageList : TCustomImageList;
begin
	Result := ParentFrame.MainFrame.ImageList1;
end;

function TRipGrepperDockableForm.GetToolBarActionList : TCustomActionList;
begin
	Result := nil; // ParentFrame.TopFrame.ActionList;  //so we will have an other toolbar, over
end;

function TRipGrepperDockableForm.GetToolBarImageList : TCustomImageList;
begin
	Result := nil; // ParentFrame.TopFrame.ImageListButtons; //so we will have an other toolbar
end;

procedure TRipGrepperDockableForm.LoadWindowState(Desktop : TCustomIniFile; const Section : string);
begin
	TDebugUtils.DebugMessage('TRipGrepperDockableForm.LoadWindowState: ' + Desktop.FileName + '[' + Section + ']');
end;

procedure TRipGrepperDockableForm.SaveWindowState(Desktop : TCustomIniFile; const Section : string; IsProject : Boolean);
begin
	TDebugUtils.DebugMessage(Format('TRipGrepperDockableForm.SaveWindowState: %s [%s] IsProject:%s',
		[Desktop.FileName, Section, BoolToStr(IsProject, True)]));
end;

class procedure TRipGrepperDockableForm.CreateInstance;
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperDockableForm.CreateInstance');

	FInstance := TRipGrepperDockableForm.Create();
	var
	services := (BorlandIDEServices as INTAServices);
	try
		services.RegisterDockableForm(FInstance);
	except
		on e : Exception do
			dbgMsg.Msg(e.Message);
	end;
end;

class procedure TRipGrepperDockableForm.DestroyInstance;
begin
	(BorlandIDEServices as INTAServices).UnregisterDockableForm(FInstance);
	FInstance.Free;
end;

class function TRipGrepperDockableForm.ShowDockableFormAndSearch : TCustomForm;
begin
	Result := (BorlandIDEServices as INTAServices).CreateDockableForm(FInstance);
	TDebugUtils.DebugMessage('TRipGrepperDockableForm.ShowDockableFormAndSearch Result.Visible=' + BoolToStr(Result.Visible, True));

	// Do search...
	ParentFrame.TopFrame.ActionShowSearchForm.Execute;
	if not Assigned(Result.HostDockSite) then begin
		var
		parent := IOTAUTils.GxOtaGetCurrentIDEEditControl();
		if not(PtInRect(parent.BoundsRect, Result.BoundsRect.TopLeft)
			{ } or PtInRect(parent.BoundsRect, Result.BoundsRect.BottomRight)) then begin
			TDebugUtils.DebugMessage('TRipGrepperDockableForm.ShowDockableFormAndSearch MakeFullyVisible');
			Result.MakeFullyVisible();
		end;
	end;
end;

function TRipGrepperDockableForm.GetFrameClass : TCustomFrameClass;
begin
	Result := TParentFrame;
end;

class function TRipGrepperDockableForm.GetInstance : TRipGrepperDockableForm;
begin
	TDebugUtils.DebugMessage('TRipGrepperDockableForm.GetInstance');
	if not Assigned(FInstance) then begin
		CreateInstance()
	end;
	Result := FInstance;
end;

end.
