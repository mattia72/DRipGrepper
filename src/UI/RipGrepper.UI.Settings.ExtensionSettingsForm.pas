unit RipGrepper.UI.Settings.ExtensionSettingsForm;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	RipGrepper.UI.SettingsFormBase,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Settings.ExtensionSettings,
	RipGrepper.Common.Constants,
	Vcl.StdCtrls,
	Vcl.ComCtrls,
	Vcl.ExtCtrls,
	System.Actions,
	RipGrepper.Tools.DelphiVersions,
	Vcl.ActnList,
	System.ImageList,
	Vcl.ImgList,
	SVGIconImageListBase,
	SVGIconImageList;

type
	TExtensionSettingsForm = class(TSettingsBaseForm)
		pnlMiddle : TPanel;
		hkedtOpenWidth : THotKey;
		lblOpenWith : TLabel;
		grpShortcuts : TGroupBox;
		hkedtSearchSelected : THotKey;
		lblSearch : TLabel;
		btnInstallPackage : TButton;
		ActionList1 : TActionList;
		ActionExtensionInstall : TAction;
		cmbDelphiVersions : TComboBox;
		grpInstallation : TGroupBox;
		OpenDialog1 : TOpenDialog;
		btnedtDllPath : TButtonedEdit;
		SVGIconImageList1 : TSVGIconImageList;
		procedure ActionExtensionInstallExecute(Sender : TObject);
		procedure btnedtDllPathRightButtonClick(Sender : TObject);
		procedure cmbDelphiVersionsChange(Sender : TObject);
		procedure FormShow(Sender : TObject);

		private const
			EXTENSION_NAME_DLL_NOT_FOUND = EXTENSION_NAME_DLL + ' not found!';
			REGISTERED_DLL_NOT_FOUND = 'No registered ' + EXTENSION_NAME_DLL + ' found!';
			UNINSTALL_CAPTION = 'Uninstall...';
			INSTALL_CAPTTION = 'Install...';

		var
			FDelphiVersions : TDelphiVersions;
			FExtensionSettings : TRipGrepperExtensionSettings;
			FEditedDllPath : string;
			FRegisteredDllPath : string;
			function OpenDlgToGetExpertDllPath(const _initPath : string) : string;
			function GetRegisteredExpertPath(dv : IDelphiVersion) : string;
			function GetSelectedDelphiVersion : IDelphiVersion;
			procedure SetEditedDllPath(const Value : string);
			function TryGetDllPath(var _dllPath : string; _dv : IDelphiVersion) : Boolean;
			procedure UpdateInstallBtnCaption;
			procedure UpdateRegisteredDllPath;
			property EditedDllPath : string read FEditedDllPath write SetEditedDllPath;

		protected
			procedure OnSettingsUpdated(); override;
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperSettings);
			destructor Destroy; override;
	end;

var
	ExtensionSettingsForm : TExtensionSettingsForm;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	{$IFNDEF STANDALONE}
	DRipExtension.Menu,
	{$ENDIF}
	Vcl.Menus,
	RipGrepper.Tools.PackageInstall,
	System.IOUtils,
	RipGrepper.Helper.UI,
	RipGrepper.Tools.FileUtils,
	System.StrUtils;

{$R *.dfm}

constructor TExtensionSettingsForm.Create(_Owner : TComponent; _settings : TRipGrepperSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := 'Extension';
	FExtensionSettings := (FSettings as TRipGrepperSettings).SearchFormSettings.ExtensionSettings;
	FDelphiVersions := TDelphiVersions.Create;

	// UpdateRegisteredDllPath;
	{$IF IS_GUITEST OR IS_EXTENSION}
	ReadSettings;
	{$ENDIF}
end;

destructor TExtensionSettingsForm.Destroy;
begin
	FDelphiVersions.Free;
	inherited;
end;

procedure TExtensionSettingsForm.ActionExtensionInstallExecute(Sender : TObject);
var
	installer : TPackageInstallMain;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperExtensionSettings.ActionExtensionInstallExecute');

	installer := TPackageInstallMain.Create;
	try

		dbgMsg.MsgFmt('Dll path: %s', [FEditedDllPath]);
		if FileExists(FEditedDllPath) and
		{ } string.EndsText(EXTENSION_NAME_DLL, FEditedDllPath) then begin
			var
				dv : IDelphiVersion := GetSelectedDelphiVersion;
			var
			sDescr := EXTENSION_NAME;
			installer.Execute(FEditedDllPath, dv as TDelphiVersion, sDescr, btnInstallPackage.Caption = UNINSTALL_CAPTION);
		end else begin;
			TMsgBox.ShowError(EXTENSION_NAME_DLL_NOT_FOUND);
		end;
		UpdateRegisteredDllPath;
		UpdateInstallBtnCaption;
	finally
		installer.Free;
	end;
end;

procedure TExtensionSettingsForm.btnedtDllPathRightButtonClick(Sender : TObject);
begin
	EditedDllPath := OpenDlgToGetExpertDllPath(EditedDllPath);
end;

procedure TExtensionSettingsForm.cmbDelphiVersionsChange(Sender : TObject);
begin
	UpdateRegisteredDllPath;
	UpdateInstallBtnCaption;
end;

procedure TExtensionSettingsForm.FormShow(Sender : TObject);
var
	dllPath : string;
	installer : TPackageInstallMain;
	versions : TStrings;
	dv : IDelphiVersion;
begin
	{$IF NOT IS_GUITEST AND IS_STANDALONE}
	grpShortcuts.Visible := False;
	grpInstallation.Top := grpShortcuts.Top;
	{$ENDIF}
	installer := TPackageInstallMain.Create;
	versions := TStringList.Create;
	try
		cmbDelphiVersions.Items.Clear;
		if installer.GetInstalledDelphiVersions(versions) then begin
			for var v in versions do begin
				dv := TEmptyDelphiVersion.Create;
				FDelphiVersions.Find(v, dv);
				cmbDelphiVersions.Items.AddObject('Delphi ' + (dv as TDelphiVersion).Name, TObject(dv));
			end;
		end else begin
			cmbDelphiVersions.Items.Add('No Delphi installation found');
		end;
		cmbDelphiVersions.ItemIndex := 0;
		dv := GetSelectedDelphiVersion;
		if not TryGetDllPath(dllPath, dv) then begin
			dllPath := TPath.Combine(TFileUtils.GetAppDirectory(), EXTENSION_NAME_DLL);
		end;
		EditedDllPath := dllPath;

		UpdateInstallBtnCaption;
	finally
		versions.Free;
		installer.Free;
	end;
end;

function TExtensionSettingsForm.OpenDlgToGetExpertDllPath(const _initPath : string) : string;
begin
	Result := '';
	OpenDialog1.InitialDir := _initPath;
	OpenDialog1.Filter := 'Package files (*.dll)|*.dll';
	if OpenDialog1.Execute(self.Handle) then begin
		Result := OpenDialog1.FileName;
	end;
end;

function TExtensionSettingsForm.GetRegisteredExpertPath(dv : IDelphiVersion) : string;
var
	sDesc : string;
begin
	sDesc := EXTENSION_NAME;
	Result := (dv as TDelphiVersion).GetKnownExpertPath(EXTENSION_NAME_DLL, sDesc);
end;

function TExtensionSettingsForm.GetSelectedDelphiVersion : IDelphiVersion;
var
	idv : IDelphiVersion;
	dvo : TObject;
begin
	var
	idx := cmbDelphiVersions.ItemIndex;
	dvo := cmbDelphiVersions.Items.Objects[idx];
	if Supports(dvo, IDelphiVersion, idv) then
		Result := idv;
end;

procedure TExtensionSettingsForm.OnSettingsUpdated();
begin
	// here you can update things depending on changed settings
	{$IFNDEF STANDALONE}
	TDripExtensionMenu.CreateMenu(EXTENSION_MENU_ROOT_TEXT, FExtensionSettings);
	{$ENDIF}
end;

procedure TExtensionSettingsForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TExtensionSettingsForm.ReadSettings');

	FExtensionSettings.LoadFromDict;
	{$IF IS_GUITEST OR IS_EXTENSION}
	hkedtOpenWidth.HotKey := TextToShortCut(FExtensionSettings.OpenWithShortcut);
	hkedtSearchSelected.HotKey := TextToShortCut(FExtensionSettings.SearchSelectedShortcut);
	{$ENDIF}
end;

procedure TExtensionSettingsForm.SetEditedDllPath(const Value : string);
begin
	FEditedDllPath := IfThen(
		{ } Value.EndsWith(EXTENSION_NAME_DLL, True) and TFile.Exists(Value),
		{ } Value, REGISTERED_DLL_NOT_FOUND);
	btnedtDllPath.Text := FEditedDllPath;
	btnInstallPackage.Enabled := (FEditedDllPath <> EXTENSION_NAME_DLL_NOT_FOUND);
end;

function TExtensionSettingsForm.TryGetDllPath(var _dllPath : string; _dv : IDelphiVersion) : Boolean;
var
	aFileName : array [0 .. MAX_PATH] of char;
	modulPath : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TExtensionSettingsForm.TryGetDllPath');

	FRegisteredDllPath := GetRegisteredExpertPath(_dv);
	dbgMsg.MsgFmt('RegisteredDllPath=%s', [FRegisteredDllPath]);

	if FileExists(FRegisteredDllPath) then begin
		_dllPath := FRegisteredDllPath;
	end else begin
		GetModuleFileName(hInstance, aFileName, MAX_PATH);
		modulPath := ExtractFilePath(aFileName);
		dbgMsg.MsgFmt('modulPath = %s', [modulPath]);
		_dllPath := TPath.Combine(modulPath, EXTENSION_NAME + '.dll');
	end;
	Result := FileExists(_dllPath);
	dbgMsg.MsgFmt('_dllPath = %s %s', [_dllPath, BoolToStr(Result, True)]);
end;

procedure TExtensionSettingsForm.UpdateInstallBtnCaption;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TExtensionSettingsForm.ReadSettings');

	btnInstallPackage.Enabled := TFile.Exists(EditedDllPath);
	if ((UpperCase(FRegisteredDllPath) = UpperCase(FEditedDllPath)) or (not FRegisteredDllPath.IsEmpty)) then begin
		btnInstallPackage.Caption := UNINSTALL_CAPTION;
	end else begin
		btnInstallPackage.Caption := INSTALL_CAPTTION;
	end;
	dbgMsg.Msg('Set=' + btnInstallPackage.Caption);
end;

procedure TExtensionSettingsForm.UpdateRegisteredDllPath;
begin
	var
		dv : IDelphiVersion := GetSelectedDelphiVersion;
	FRegisteredDllPath := GetRegisteredExpertPath(dv);
	EditedDllPath := FRegisteredDllPath;
end;

procedure TExtensionSettingsForm.WriteSettings;
begin
	{$IF IS_GUITEST OR IS_EXTENSION}
	var
	dbgMsg := TDebugMsgBeginEnd.New('TExtensionSettingsForm.WriteSettings');
	FExtensionSettings.OpenWithShortcut := ShortCutToText(hkedtOpenWidth.HotKey);
	FExtensionSettings.SearchSelectedShortcut := ShortCutToText(hkedtSearchSelected.HotKey);
	{$ENDIF}
end;

end.
