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
		InstallPackage : TButton;
		ActionList1 : TActionList;
		ActionExtensionInstall : TAction;
		cmbDelphiVersions : TComboBox;
		grpInstallation : TGroupBox;
		OpenDialog1 : TOpenDialog;
		procedure ActionExtensionInstallExecute(Sender : TObject);
		procedure FormShow(Sender : TObject);

		private
			FExtensionSettings : TRipGrepperExtensionSettings;

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
	aFileName : array [0 .. MAX_PATH] of char;
	modulPath : string;
	sPackPath : string;
begin
	installer := TPackageInstallMain.Create;
	try
		GetModuleFileName(hInstance, aFileName, MAX_PATH);
		var
		idx := cmbDelphiVersions.ItemIndex;
		var dv : TDelphiVersion := cmbDelphiVersions.Items.Objects[idx] as TDelphiVersion;
		modulPath := ExtractFilePath(aFileName);
		sPackPath := TPath.Combine(modulPath, EXTENSION_NAME + '.bpl');
		if not FileExists(sPackPath) then begin
			OpenDialog1.InitialDir := modulPath;
			OpenDialog1.Filter := 'Package files (*.bpl)|*.bpl';
			if OpenDialog1.Execute(self.Handle) then begin
				sPackPath := OpenDialog1.FileName;
			end;
		end;
		if FileExists(sPackPath) and string.EndsText(EXTENSION_NAME + '.bpl', sPackPath) then begin
			installer.Execute(sPackPath, dv);
		end else begin;
			TMsgBox.ShowError(EXTENSION_NAME + '.bpl not found!');
		end;
	finally
		installer.Free;
	end;
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
