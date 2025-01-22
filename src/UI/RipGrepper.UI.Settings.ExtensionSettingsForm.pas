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
			UNINSTALL_CAPTION = 'Uninstall...';
			INSTALL_CAPTTION = 'Install...';

		var
			FDelphiVersions : TDelphiVersions;
			FExtensionSettings : TRipGrepperExtensionSettings;
			FEditedDllPath : string;
			FRegisteredDllPath : string;
			function OpenDlgToGetExpertDllPath : string;
			function GetRegisteredExpertPath : string;
			function GetSelectedDelphiVersion : IDelphiVersion;
			procedure SetEditedDllPath(const Value : string);
			procedure UpdateInstallBtnCaption;
			procedure UpdateRegisteredDllPath;
			property EditedDllPath : string read FEditedDllPath write SetEditedDllPath;

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
	RipGrepper.Tools.PackageInstall,
	System.IOUtils,
	RipGrepper.Helper.UI,
	RipGrepper.Tools.FileUtils,
	System.StrUtils;

{$R *.dfm}

constructor TExtensionSettingsForm.Create(_Owner : TComponent; _settings : TRipGrepperExtensionSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := 'Extension';
	FExtensionSettings := _settings;
	FDelphiVersions := TDelphiVersions.Create;
	EditedDllPath := TPath.Combine(TFileUtils.GetAppDirectory(), EXTENSION_NAME_DLL);
	// UpdateRegisteredDllPath;
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
	EditedDllPath := OpenDlgToGetExpertDllPath;
end;

procedure TExtensionSettingsForm.cmbDelphiVersionsChange(Sender : TObject);
begin
	UpdateRegisteredDllPath;
	UpdateInstallBtnCaption;
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
		UpdateRegisteredDllPath;
		UpdateInstallBtnCaption;
	finally
		versions.Free;
		installer.Free;
	end;
end;

function TExtensionSettingsForm.OpenDlgToGetExpertDllPath : string;
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

function TExtensionSettingsForm.GetRegisteredExpertPath : string;
var
	sDesc : string;
	dv : IDelphiVersion;
begin
	dv := GetSelectedDelphiVersion;
	sDesc := EXTENSION_NAME;
	Result := (dv as TDelphiVersion).GetKnownExpertPath(TPath.GetFileName(FEditedDllPath), sDesc);
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

procedure TExtensionSettingsForm.SetEditedDllPath(const Value : string);
begin
	FEditedDllPath := IfThen(
		{ } Value.EndsWith(EXTENSION_NAME_DLL, True) and TFile.Exists(Value),
		{ } Value, EXTENSION_NAME_DLL_NOT_FOUND);
	btnedtDllPath.Text := FEditedDllPath;
	btnInstallPackage.Enabled := (FEditedDllPath <> EXTENSION_NAME_DLL_NOT_FOUND);
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
	FRegisteredDllPath := GetRegisteredExpertPath;
	EditedDllPath := FRegisteredDllPath;
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

end.
