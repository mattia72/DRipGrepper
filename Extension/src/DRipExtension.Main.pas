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
		private
			FKeyNotifier : IOTAKeyboardBinding;
			FKeyBinding : integer;
			FiPluginIndexAbout : Integer;
			procedure InitPluginInfo;
			// **********************************************************************************************************************
			// Plugin-Infos entfernen
			// **********************************************************************************************************************
			procedure RemovePluginInfo;

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
			// IOTAWizard
			procedure InitKeyboardNotifier;
			procedure RegisterKeyboardBinding;
			procedure UnregisterKeyboardBinding;

	end;

function InitWizard(const BorlandIDEServices : IBorlandIDEServices; RegisterProc : TWizardRegisterProc;
	var Terminate : TWizardTerminateProc) : Boolean stdcall;
procedure FinalizeWizard;

implementation

uses
	RipGrepper.Common.Constants,
	RipGrepper.Common.IOTAUtils,
	RipGrepper.Settings.AppSettings,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Tools.FileUtils,
	System.IniFiles,
	System.IOUtils,
	System.SysUtils,
	Vcl.Dialogs,
	Vcl.ImgList,
	Winapi.Windows,
	DRipExtension.Menu;

var
	MMOTAExpertIndex : integer;

function InitWizard(const BorlandIDEServices : IBorlandIDEServices; RegisterProc : TWizardRegisterProc;
	var Terminate : TWizardTerminateProc) : Boolean stdcall;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('InitWizard');

	Result := True;
	Terminate := FinalizeWizard;
	MMOTAExpertIndex := (BorlandIDEServices as IOTAWizardServices).AddWizard(TDRipExtension.Create);
end;

procedure FinalizeWizard;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('FinalizeWizard');
	if MMOTAExpertIndex <> -1 then
		(BorlandIDEServices as IOTAWizardServices).RemoveWizard(MMOTAExpertIndex);
	MMOTAExpertIndex := -1;
end;

constructor TDRipExtension.Create;
begin
	inherited Create();
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDRipExtension.Create');
	InitPluginInfo;
	TRipGrepperDockableForm.CreateInstance; // saved layout loading ...
	TDripExtensionMenu.CreateMenu(GetMenuText, GSettings.SearchFormSettings.ExtensionSettings);
end;

destructor TDRipExtension.Destroy;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDRipExtension.Destroy');
	RemovePluginInfo;
	TRipGrepperDockableForm.DestroyInstance;
	inherited;
end;

// IOTAWizard
procedure TDRipExtension.Execute;
begin
	ShowMessage(EXTENSION_NAME + CRLF + HOME_PAGE);
end;

// IOTAWizard
function TDRipExtension.GetIDString : string;
begin
	Result := 'ID.' + EXTENSION_NAME;
end;

// IOTAWizard
function TDRipExtension.GetMenuText : string;
begin
	Result := EXTENSION_MENU_ROOT_TEXT;
end;

// IOTAWizard
function TDRipExtension.GetName : string;
begin
	Result := DRIPGREPPER_WIZARD_NAME;
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
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDRipExtension.InitPluginInfo');
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

	var
	dbgMsg := TDebugMsgBeginEnd.New('TDRipExtension.RegisterKeyboardBinding');

	kbServices := BorlandIDEServices as IOTAKeyboardServices;
	try
		if Assigned(kbServices) then
			FKeyBinding := kbServices.AddKeyboardBinding(FKeyNotifier);
	except
		FKeyBinding := -1;
	end;
end;

procedure TDRipExtension.RemovePluginInfo;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDRipExtension.RemovePluginInfo');

	if FiPluginIndexAbout > 0 then
		(BorlandIDEServices as IOTAAboutBoxServices).RemovePluginInfo(FiPluginIndexAbout);
end;

procedure TDRipExtension.UnregisterKeyboardBinding;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TDRipExtension.UnregisterKeyboardBinding');
	if FKeyBinding > -1 then begin
		(BorlandIDEServices as IOTAKeyboardServices).RemoveKeyboardBinding(FKeyBinding);
		FKeyBinding := -1;
	end
	else
		// FKeyNotifier.Free;
		FKeyNotifier := nil;
end;

initialization

GSettings := TRipGrepperSettings.Create;
GSettings.AppSettings.ReadIni;
GSettings.AppSettings.LoadFromDict();
TDebugUtils.UpdateTraceActive;

OutputDebugString(PChar('DRipExtension initialized.'));

finalization

TFileUtils.DeleteTempDirectory(
	{ } TPath.GetFileNameWithoutExtension(GSettings.IniFile.FileName) + '*', True);
FreeAndNil(GSettings);
OutputDebugString(PChar('DRipExtension finalized.'));

end.
