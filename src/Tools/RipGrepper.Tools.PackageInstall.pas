unit RipGrepper.Tools.PackageInstall;

interface

uses
	u_DelphiVersions,
	Winapi.Windows,
	ArrayEx,
	System.Classes;

type
	EInstallResult = (irInstallSuccess, irAlreadyInstalled, irInstallError, irIUnnstallSuccess, irNotInstalled, irUnInstallError);

	TPackageInstallMain = class // (TDefaultMain)
		private
			FDelphiVersions : TDelphiVersions;
			function handlePackage(_DelphiVer : TDelphiVersion; const _Package : string; const _Uninstall : Boolean) : EInstallResult;
			function installPackage(const _DelphiVer : TDelphiVersion; var _pkg : string) : EInstallResult;
			function uninstallPackage(const _DelphiVer : TDelphiVersion; var _pkg : string) : EInstallResult;

		public
			function Execute(const _sPackagePath : string; const _dVer : TDelphiVersion; const _bUninstall : Boolean = False)
				: EInstallResult;
			constructor Create; // override;
			destructor Destroy; override;
			function GetInstalledDelphiVersions(var _list : TStrings) : Boolean;
	end;

implementation

uses
	System.SysUtils,
	u_dzMiscUtils,
	u_dzConvertUtils,
	System.Win.Registry,
	RipGrepper.Helper.UI,
	System.IOUtils;

procedure PackageInfoProc(const _Name : string; _NameType : TNameType; _Flags : Byte; _Param : Pointer);
begin
	// do nothing, but we need this empty procedure beause we cannot pass NIL to GetPackageInfo
end;

{ TPackageInstallMain }

constructor TPackageInstallMain.Create;
begin
	inherited;
	FDelphiVersions := TDelphiVersions.Create;
end;

destructor TPackageInstallMain.Destroy;
begin
	FreeAndNil(FDelphiVersions);
	inherited;
end;

function TPackageInstallMain.handlePackage(_DelphiVer : TDelphiVersion; const _Package : string; const _Uninstall : Boolean)
	: EInstallResult;
var
	pkg : string;
begin
	if _Uninstall then begin
		Result := EInstallResult.irUnInstallError;
	end else begin
		Result := EInstallResult.irInstallError;
	end;
	pkg := _Package;
	if ExtractFileDir(pkg) = '' then begin
		var
		fname := TPath.GetFileName(pkg);
		pkg := TPath.Combine(_DelphiVer.GetBplDir, fname);
	end;

	try
		if _Uninstall then begin
			Result := uninstallPackage(_DelphiVer, pkg);
		end else begin
			Result := installPackage(_DelphiVer, pkg);
		end;
	except
		on E : Exception do
			TMsgBox.ShowError(E.Message);
	end;
end;

function TPackageInstallMain.Execute(const _sPackagePath : string; const _dVer : TDelphiVersion; const _bUninstall : Boolean = False)
	: EInstallResult;
begin
	_dVer.CheckInstalled;
	Result := handlePackage(_dVer, _sPackagePath, _bUninstall);
	case Result of
		irInstallSuccess :
		TMsgBox.ShowInfo('The package has been installed successfully.');
		irAlreadyInstalled :
		TMsgBox.ShowInfo('Package is already installed.');
		irInstallError :
		TMsgBox.ShowInfo('Package install failed.');
		irIUnnstallSuccess :
		TMsgBox.ShowInfo('The package has been uninstalled successfully.');
		irNotInstalled :
		TMsgBox.ShowInfo('The package is not installed.');
		irUnInstallError :
		TMsgBox.ShowInfo('Package uninstall failed.');
	end;
end;

function TPackageInstallMain.installPackage(const _DelphiVer : TDelphiVersion; var _pkg : string) : EInstallResult;
var
	Description : string;
	Handle : THandle;
	LastError : Cardinal;
	Flags : Integer;
	OrigName : string;
begin
	if not FileExists(_pkg) then begin
		OrigName := _pkg;
		if not _DelphiVer.TryApplyBplSuffix(_pkg) then
			raise Exception.Create('Could not find package file.' + #13#10 + _pkg);
		if not FileExists(_pkg) then
			raise Exception.Create('Could not find package file.' + #13#10 + _pkg + #13#10 + OrigName);
	end;

	Description := GetPackageDescription(PChar(_pkg));
	// StdOut.WriteLn(_('Installing "%s"'), [_pkg.Filename]);
	// StdOut.WriteLn(_('Description: "%s"'), [Description]);

	Handle := LoadLibraryEx(PChar(_pkg), 0, LOAD_LIBRARY_AS_DATAFILE);
	if Handle = 0 then begin
		LastError := GetLastError;
		RaiseLastOSErrorEx(LastError, Format('LoadLibrary failed for package "%s". (Error %%1:s (%%0:d))', [_pkg]));
	end;
	try
		GetPackageInfo(Handle, nil, Flags, PackageInfoProc);
	finally
		FreeLibrary(Handle);
	end;

	if (Flags and pfRunOnly) <> 0 then begin
		raise Exception.Create('This is a runtime only package that cannot be installed into the Delphi IDE.');
	end;
	if _DelphiVer.IsKnownPackage(_pkg, Description) then begin
		Result := EInstallResult.irAlreadyInstalled;
	end else begin
		_DelphiVer.AddKnownPackage(_pkg, Description);
		Result := EInstallResult.irInstallSuccess;
	end;
end;

function TPackageInstallMain.uninstallPackage(const _DelphiVer : TDelphiVersion; var _pkg : string) : EInstallResult;
var
	Description : string;
begin
	if _DelphiVer.IsKnownPackage(_pkg, Description) then begin
		_DelphiVer.RemoveKnownPackage(_pkg);
		Result := EInstallResult.irIUnnstallSuccess;
		// StdOut.Success.WriteLn(_('The package has been uninstalled successfully.'));
	end else if not _DelphiVer.TryApplyBplSuffix(_pkg) then begin
		Result := EInstallResult.irNotInstalled;
		// StdOut.Hint.WriteLn(_('The package is not installed.'));
	end else if _DelphiVer.IsKnownPackage(_pkg, Description) then begin
		_DelphiVer.RemoveKnownPackage(_pkg);
		Result := EInstallResult.irIUnnstallSuccess;
		// StdOut.Success.WriteLn(_('The package has been uninstalled successfully.'));
	end else begin
		Result := EInstallResult.irUnInstallError;
		// StdOut.Hint.WriteLn(_('The package is not installed.'));
	end;
end;

function TPackageInstallMain.GetInstalledDelphiVersions(var _list : TStrings) : Boolean;
var
	reg : TRegistry;
	arrDelphiKeys : TArrayEx<string>;
	tmp : TStringList;
	sDelphiKey : string;
begin
	reg := TRegistry.Create(KEY_READ);
	tmp := TStringList.Create;
	try
		_list.Clear;
		reg.RootKey := HKEY_LOCAL_MACHINE;
		arrDelphiKeys := ['SOFTWARE\Embarcadero\BDS', 'SOFTWARE\Wow6432Node\Embarcadero\BDS'];
		for sDelphiKey in arrDelphiKeys do begin
			if reg.OpenKeyReadOnly(sDelphiKey) then begin
				reg.GetKeyNames(tmp);
				_list.AddStrings(tmp);
			end;
		end;
		Result := _list.Count > 0;
	finally
		tmp.free;
		reg.Free;
	end;
end;

end.
