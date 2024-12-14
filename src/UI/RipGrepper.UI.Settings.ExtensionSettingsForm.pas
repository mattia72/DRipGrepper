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
	u_DelphiVersions,
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
			FExtensionSettings : TRipGrepperExtensionSettings;
			FPackagePath : string;
			function GetPackagePath : string;
			function GetSelectedDelphiVersion : TDelphiVersion;
			procedure UpdateBtnCaption;

		protected
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperExtensionSettings);
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
	RipGrepper.Helper.UI;

{$R *.dfm}

const
	UNINSTALL_CAPTION = 'Uninstall...';
	INSTALL_CAPTTION = 'Install...';

constructor TExtensionSettingsForm.Create(_Owner : TComponent; _settings : TRipGrepperExtensionSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := 'Extension';
	FExtensionSettings := _settings;
	{$IFNDEF STANDALONE}
	ReadSettings;
	{$ENDIF}
end;

procedure TExtensionSettingsForm.ActionExtensionInstallExecute(Sender : TObject);
var
	installer : TPackageInstallMain;
begin
	installer := TPackageInstallMain.Create;
	try
		if FPackagePath.IsEmpty then begin
            FPackagePath := GetPackagePath;
		end;
		if FileExists(FPackagePath) and string.EndsText(EXTENSION_NAME + '.bpl', FPackagePath) then begin
			var dv : TDelphiVersion := GetSelectedDelphiVersion;
			installer.Execute(FPackagePath, dv, btnInstallPackage.Caption = UNINSTALL_CAPTION);
		end else begin;
			TMsgBox.ShowError(EXTENSION_NAME + '.bpl not found!');
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
	dvs : TDelphiVersions;
	dv : TDelphiVersion;
begin
	{$IFDEF STANDALONE}
	grpShortcuts.Visible := False;
	grpInstallation.Top := grpShortcuts.Top;
	{$ENDIF}
	installer := TPackageInstallMain.Create;
	versions := TStringList.Create;
	dvs := TDelphiVersions.Create;
	try
		cmbDelphiVersions.Items.Clear;
		if installer.GetInstalledDelphiVersions(versions) then begin
			for var v in versions do begin
				dv := TEmptyDelphiVersion.Create;
				dvs.Find(v, dv);
				cmbDelphiVersions.Items.AddObject('Delphi ' + dv.name, dv);
			end;
		end else begin
			cmbDelphiVersions.Items.Add('No Delphi installation found');
		end;
		cmbDelphiVersions.ItemIndex := 0;
	finally
		dvs.Free;
		versions.Free;
		installer.Free;
	end;
end;

function TExtensionSettingsForm.GetPackagePath() : string;
var
	aFileName : array [0 .. MAX_PATH] of char;
	modulPath : string;
begin
	GetModuleFileName(hInstance, aFileName, MAX_PATH);
	modulPath := ExtractFilePath(aFileName);
	Result := TPath.Combine(modulPath, EXTENSION_NAME + '.bpl');
	if not FileExists(Result) then begin
		OpenDialog1.InitialDir := modulPath;
		OpenDialog1.Filter := 'Package files (*.bpl)|*.bpl';
		if OpenDialog1.Execute(self.Handle) then begin
			Result := OpenDialog1.FileName;
		end;
	end;
end;

function TExtensionSettingsForm.GetSelectedDelphiVersion : TDelphiVersion;
begin
	var
	idx := cmbDelphiVersions.ItemIndex;
	Result := cmbDelphiVersions.Items.Objects[idx] as TDelphiVersion;
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
	desc : string;
	dv : TDelphiVersion;
begin
	dv := GetSelectedDelphiVersion;
	if FPackagePath.IsEmpty or (not dv.IsKnownPackage(FPackagePath, desc)) then begin
		btnInstallPackage.Caption := INSTALL_CAPTTION;
	end else begin
		btnInstallPackage.Caption := UNINSTALL_CAPTION;
	end;
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
