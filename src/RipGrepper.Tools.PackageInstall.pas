unit RipGrepper.Tools.PackageInstall;

interface

uses
	u_DelphiVersions,
	Winapi.Windows,
	u_dzFileUtils;

type
	TPackageInstallMain = class // (TDefaultMain)
		private
			FDelphiVersions : TDelphiVersions;
			procedure handlePackage(_DelphiVer : TDelphiVersion; const _Package : string; _Uninstall : Boolean);
			procedure installPackage(const _DelphiVer : TDelphiVersion; var _pkg : u_dzFileUtils.TFilename);
			procedure uninstallPackage(const _DelphiVer : TDelphiVersion; var _pkg : u_dzFileUtils.TFilename);

		public
			function Execute(const _sPackagePath : string; const _sDelphiVersion : string = ''; const _bUninstall : Boolean = False)
				: Integer;
			constructor Create; // override;
			destructor Destroy; override;
	end;

implementation

uses
	System.SysUtils,
	u_dzMiscUtils,
	u_dzConvertUtils;

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

procedure TPackageInstallMain.handlePackage(_DelphiVer : TDelphiVersion; const _Package : string; _Uninstall : Boolean);
var
	pkg : u_dzFileUtils.TFilename;
begin
	pkg := _Package;
	if pkg.Directory = '' then
		pkg.ReplaceDirectory(_DelphiVer.GetBplDir);
	if pkg.Extension = '' then
		pkg.ReplaceExtension('.bpl');

	if _Uninstall then begin
		uninstallPackage(_DelphiVer, pkg);
	end else begin
		installPackage(_DelphiVer, pkg);
	end;
end;

function TPackageInstallMain.Execute(const _sPackagePath : string; const _sDelphiVersion : string = ''; const _bUninstall : Boolean = False)
	: Integer;
var
	DVer : TDelphiVersion;
	i : Integer;
	PackageName : string;
	VerSuffix : string;
begin
	PackageName := ChangeFileExt(ExtractFileName(_sPackagePath), '');
	i := Length(PackageName);
	while (i > 0) and isDecDigit(PackageName[i]) do
		Dec(i);
	VerSuffix := Copy(PackageName, i + 1);
	if VerSuffix <> '100' then
		FDelphiVersions.FindSuffix(VerSuffix, DVer)
	else
		DVer := nil;

	if not _sDelphiVersion.IsEmpty then begin
		if not Assigned(DVer) then begin
			if VerSuffix = '100' then
				raise Exception.Create
					('BPL version suffix "100" was used for Delphi 2006 and Delphi 2007. You must pass a --DelphiVersion.');
			raise Exception.Create('You must pass a --DelphiVersion.');
		end;
	end else begin
		if not FDelphiVersions.Find(_sDelphiVersion, DVer) then
			raise Exception.CreateFmt('"%s" was not recognized as a Delphi version.', [_sDelphiVersion]);
	end;

	DVer.CheckInstalled;

	handlePackage(DVer, _sPackagePath, _bUninstall);

	Result := 0;
end;

procedure TPackageInstallMain.installPackage(const _DelphiVer : TDelphiVersion; var _pkg : u_dzFileUtils.TFilename);
var
	Description : string;
	Handle : THandle;
	LastError : Cardinal;
	Flags : Integer;
	OrigName : string;
begin
	if not TFileSystem.FileExists(_pkg.Full) then begin
		OrigName := _pkg.Full;
		if not _DelphiVer.TryApplyBplSuffix(_pkg) then
			raise Exception.Create('Could not find package file.' + #13#10 + _pkg.Full);
		if not TFileSystem.FileExists(_pkg.Full) then
			raise Exception.Create('Could not find package file.' + #13#10 + _pkg.Full + #13#10 + OrigName);
	end;

	Description := GetPackageDescription(PChar(_pkg.Full));
	// StdOut.WriteLn(_('Installing "%s"'), [_pkg.Filename]);
	// StdOut.WriteLn(_('Description: "%s"'), [Description]);

	Handle := LoadLibraryEx(PChar(_pkg.Full), 0, LOAD_LIBRARY_AS_DATAFILE);
	if Handle = 0 then begin
		LastError := GetLastError;
		RaiseLastOSErrorEx(LastError, Format('LoadLibrary failed for package "%s". (Error %%1:s (%%0:d))', [_pkg.Filename]));
	end;
	try
		GetPackageInfo(Handle, nil, Flags, PackageInfoProc);
	finally
		FreeLibrary(Handle);
	end;

	if (Flags and pfRunOnly) <> 0 then
		raise Exception.Create('This is a runtime only package that cannot be installed into the Delphi IDE.');

	if _DelphiVer.IsKnownPackage(_pkg.Full, OrigName) then begin
		// StdOut.Hint.WriteLn(Format('The package %s is already installed (%s)', [_pkg.Filename, OrigName]));
	end else begin
		_DelphiVer.AddKnownPackage(_pkg.Full, Description);
		// StdOut.Success.WriteLn(_('The package has been installed successfully.'));
	end;
end;

procedure TPackageInstallMain.uninstallPackage(const _DelphiVer : TDelphiVersion; var _pkg : u_dzFileUtils.TFilename);
var
	Description : string;
begin
	if _DelphiVer.IsKnownPackage(_pkg.Full, Description) then begin
		_DelphiVer.RemoveKnownPackage(_pkg.Full);
		// StdOut.Success.WriteLn(_('The package has been uninstalled successfully.'));
	end else if not _DelphiVer.TryApplyBplSuffix(_pkg) then begin
		// StdOut.Hint.WriteLn(_('The package is not installed.'));
	end else if _DelphiVer.IsKnownPackage(_pkg.Full, Description) then begin
		_DelphiVer.RemoveKnownPackage(_pkg.Full);
		// StdOut.Success.WriteLn(_('The package has been uninstalled successfully.'));
	end else begin
		// StdOut.Hint.WriteLn(_('The package is not installed.'));
	end;
end;

end.
