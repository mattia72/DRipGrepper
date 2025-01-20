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
	Vcl.StdCtrls,
	Vcl.ComCtrls,
	Vcl.ExtCtrls,
	System.Actions,
	RipGrepper.Tools.DelphiVersions,
	Vcl.ActnList;

type
	TEmptyDelphiVersion = class(TDelphiVersion)
		public
			constructor Create();
			function GetBplDir : string; override;
	end;

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
		procedure ActionExtensionInstallExecute(Sender : TObject);
		procedure cmbDelphiVersionsChange(Sender : TObject);
		procedure FormShow(Sender : TObject);

		private
			FDelphiVersions : TDelphiVersions;
			FExtensionSettings : TRipGrepperExtensionSettings;
			FDllPath : string;
			function GetExpertDllPath : string;
			function GetSelectedDelphiVersion : IDelphiVersion;
			procedure UpdateBtnCaption;

		protected
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperExtensionSettings);
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
	RipGrepper.Common.Constants,
	RipGrepper.Tools.PackageInstall,
	System.IOUtils,
	RipGrepper.Helper.UI,
	RipGrepper.Tools.FileUtils;

{$R *.dfm}

const
	UNINSTALL_CAPTION = 'Uninstall...';
	INSTALL_CAPTTION = 'Install...';

constructor TExtensionSettingsForm.Create(_Owner : TComponent; _settings : TRipGrepperExtensionSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := 'Extension';
	FExtensionSettings := _settings;
	FDelphiVersions := TDelphiVersions.Create;
	FDllPath := TPath.Combine(TFileUtils.GetAppDirectory(), EXTENSION_NAME_DLL);
	{$IFNDEF STANDALONE}
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
		if not TFile.Exists(FDllPath) then begin
			FDllPath := GetExpertDllPath;
		end;
		dbgMsg.MsgFmt('Dll path: %s', [FDllPath]);
		if FileExists(FDllPath) and
		{ } string.EndsText(EXTENSION_NAME_DLL, FDllPath) then begin
			var
				dv : IDelphiVersion := GetSelectedDelphiVersion;
			var
			sDescr := EXTENSION_NAME;
			installer.Execute(FDllPath, dv as TDelphiVersion, sDescr, btnInstallPackage.Caption = UNINSTALL_CAPTION);
		end else begin;
			TMsgBox.ShowError(EXTENSION_NAME_DLL + ' not found!');
		end;
		UpdateBtnCaption;
	finally
		installer.Free;
	end;
end;

procedure TExtensionSettingsForm.cmbDelphiVersionsChange(Sender : TObject);
begin
	UpdateBtnCaption;
end;

procedure TExtensionSettingsForm.FormShow(Sender : TObject);
var
	installer : TPackageInstallMain;
	versions : TStrings;
	dv : IDelphiVersion;
begin
	{$IFDEF STANDALONE}
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
        UpdateBtnCaption;
	finally
		versions.Free;
		installer.Free;
	end;
end;

function TExtensionSettingsForm.GetExpertDllPath : string;
var
	aFileName : array [0 .. MAX_PATH] of char;
	modulPath : string;
begin
	GetModuleFileName(hInstance, aFileName, MAX_PATH);
	modulPath := ExtractFilePath(aFileName);
	Result := TPath.Combine(modulPath, EXTENSION_NAME + '.dll');
	if not FileExists(Result) then begin
		OpenDialog1.InitialDir := modulPath;
		OpenDialog1.Filter := 'Package files (*.dll)|*.dll';
		if OpenDialog1.Execute(self.Handle) then begin
			Result := OpenDialog1.FileName;
		end;
	end;
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

procedure TExtensionSettingsForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TExtensionSettingsForm.ReadSettings');
	FExtensionSettings.ReadIni;
	FExtensionSettings.LoadFromDict;
	{$IFNDEF STANDALONE}
	hkedtOpenWidth.HotKey := TextToShortCut(FExtensionSettings.OpenWithShortCut);
	hkedtSearchSelected.HotKey := TextToShortCut(FExtensionSettings.SearchSelectedShortcut);
	{$ENDIF}
end;

procedure TExtensionSettingsForm.UpdateBtnCaption;
var
	sDesc : string;
	dv : IDelphiVersion;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TExtensionSettingsForm.ReadSettings');

	dv := GetSelectedDelphiVersion;
	sDesc := EXTENSION_NAME;
	var
	realExpertPath := (dv as TDelphiVersion).GetKnownExpertPath(TPath.GetFileName(FDllPath), sDesc);

	if ((UpperCase(realExpertPath) = UpperCase(FDllPath)) or (not realExpertPath.IsEmpty))
    then begin
        FDllPath := realExpertPath;
		btnInstallPackage.Caption := UNINSTALL_CAPTION;
	end else begin
		btnInstallPackage.Caption := INSTALL_CAPTTION;
	end;
	dbgMsg.Msg('Set=' + btnInstallPackage.Caption);
end;

procedure TExtensionSettingsForm.WriteSettings;
begin
	{$IFNDEF STANDALONE}
	var
	dbgMsg := TDebugMsgBeginEnd.New('TExtensionSettingsForm.WriteSettings');

	FExtensionSettings.OpenWithShortCut := ShortCutToText(hkedtOpenWidth.HotKey);
	FExtensionSettings.SearchSelectedShortcut := ShortCutToText(hkedtSearchSelected.HotKey);

	// FExtensionSettings.
	inherited WriteSettings;
	TDripExtensionMenu.CreateMenu(EXTENSION_MENU_ROOT_TEXT, FExtensionSettings);
	{$ENDIF}
end;

constructor TEmptyDelphiVersion.Create;
begin
	inherited Create;
end;

function TEmptyDelphiVersion.GetBplDir : string;
begin
	Result := inherited GetBplDir;
end;

end.
