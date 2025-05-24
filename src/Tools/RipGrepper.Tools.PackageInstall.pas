unit RipGrepper.Tools.PackageInstall;

interface

uses
	RipGrepper.Tools.DelphiVersions,
	Winapi.Windows,
	ArrayEx,
	System.Classes;

type
	EInstallResult = (
		{ } irInstallError = -2,
		{ } irUnInstallError = -1,
		{ } irNotSet = 0,
		{ } irInstallSuccess,
		{ } irAlreadyInstalled,
		{ } irUninstallSuccess,
		{ } irNotInstalled);

	TPackageInstallMain = class // (TDefaultMain)
		private
			FDelphiVersions : TDelphiVersions;
			function handleExtension(const _delphiVersion : TDelphiVersion; const _sFullPath : string; var _sExpertDesc : string;
				const _bUninstall : Boolean) : EInstallResult;
			function installExpertDll(const _delphiVersion : TDelphiVersion; const _dllPath : string; var _sExpertDesc : string)
				: EInstallResult;
			function uninstallExpertDll(const _DelphiVer : TDelphiVersion; const _sPath : string; var _sExpertDesc : string)
				: EInstallResult;

		public
			function Execute(const _sPath : string; const _delphiVersion : TDelphiVersion; var _sExpertDesc : string;
				const _bUninstall : Boolean = False) : EInstallResult;
			constructor Create; // override;
			destructor Destroy; override;
			function GetInstalledDelphiVersions(var _list : TStrings) : Boolean;
	end;

	/// <summary> Same as VCL RaiseLastWin32Error but can specify a format.
	/// This procedure does the same as the VCL RaiseLastWin32Error but you can
	/// specify a format string to use. With this string you can provide some
	/// additional information about where the error occured.
	/// It calls GetLastError to get the result code of the last Win32 api function.
	/// If it returns non 0 the function uses SysErrorMessage to retrieve an error
	/// message for the error code and raises raises an EWin32Error exception
	/// (to be compatible with the VCL function) with the Error message.
	/// NOTE: Do not pass a resource string as format parameter, since loading this
	/// string will reset the error code returned by GetLastError, so
	/// you always get 0. Use the overloaded Version that takes the error code
	/// as parameter and get it before using the resource string if you want that.
	/// @param Format The Format string to use. It must have %d and %s in it, to
	/// change the order, use %0:d and %1:s, e.g. 'Error %1:s (%0:d)'
	/// %d is replaced by the error code and %s is replaced by the
	/// error message string. </summary>
procedure RaiseLastOsErrorEx(const _Format : string); overload;

/// <summary> Same as VCL RaiseLastWin32Error but can specify a format.
/// This procedure does the same as the VCL RaiseLastWin32Error but you can
/// specify a format string to use. With this string you can provide some
/// additional information about where the error occured.
/// If ErrorCode <> 0 the function uses SysErrorMessage to retrieve an error
/// message for the error code and raises raises an EWin32Error exception
/// (to be compatible with the VCL function) with the Error message.
/// NOTE: If you pass a resource string as format parameter make sure you
/// call GetLastError before referencing the resource string, otherwise
/// loading the string will reset the error code returned by GetLastError, so
/// you always get 0.
/// @param ErrorCode is an error code returned from GetLastWin32Error
/// @param Format The Format string to use. It must have %d and %s in it, to
/// change the order, use %0:d and %1:s, e.g. 'Error %1:s (%0:d)'
/// %d is replaced by the error code and %s is replaced by the
/// error message string. </summary>
procedure RaiseLastOsErrorEx(_ErrorCode : Integer; const _Format : string); overload;

implementation

uses
	System.SysUtils,

	System.Win.Registry,
	RipGrepper.Helper.UI,
	System.IOUtils,
	RipGrepper.Common.Constants;

procedure PackageInfoProc(const _Name : string; _NameType : TNameType; _Flags : Byte; _Param : Pointer);
begin
	// do nothing, but we need this empty procedure beause we cannot pass NIL to GetPackageInfo
end;

procedure RaiseLastOsErrorEx(const _Format : string);
begin
	RaiseLastOsErrorEx(GetLastError, _Format);
end;

procedure RaiseLastOsErrorEx(_ErrorCode : Integer; const _Format : string); overload;
var
	Error : EOSError;
begin
	if _ErrorCode <> ERROR_SUCCESS then
		Error := EOSError.CreateFmt(_Format, [_ErrorCode, SysErrorMessage(_ErrorCode)])
	else
		Error := EOSError.CreateFmt(_Format, [_ErrorCode, 'unknown OS error']);
	Error.ErrorCode := _ErrorCode;
	raise Error;
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

function TPackageInstallMain.handleExtension(const _delphiVersion : TDelphiVersion; const _sFullPath : string; var _sExpertDesc : string;
	const _bUninstall : Boolean) : EInstallResult;
begin
	if _bUninstall then begin
		Result := EInstallResult.irUnInstallError;
	end else begin
		Result := EInstallResult.irInstallError;
	end;

	try
		if _bUninstall then begin
			Result := uninstallExpertDll(_delphiVersion, _sFullPath, _sExpertDesc);
		end else begin
			Result := installExpertDll(_delphiVersion, _sFullPath, _sExpertDesc);
		end;
	except
		on E : Exception do
			TMsgBox.ShowError(E.Message);
	end;
end;

function TPackageInstallMain.Execute(const _sPath : string; const _delphiVersion : TDelphiVersion; var _sExpertDesc : string;
	const _bUninstall : Boolean = False) : EInstallResult;
begin
	_delphiVersion.CheckInstalled;
	Result := handleExtension(_delphiVersion, TPath.GetFullPath(_sPath), _sExpertDesc, _bUninstall);
	var
	sFileName := TPath.GetFileName(_sPath);
	case Result of
		irInstallSuccess :
		TMsgBox.ShowInfo(Format('%s has been installed successfully.', [sFileName]));
		irAlreadyInstalled :
		TMsgBox.ShowInfo(Format('%s is already installed.', [sFileName]));
		irInstallError :
		TMsgBox.ShowInfo(Format('%s install failed.', [sFileName]));
		irUninstallSuccess :
		TMsgBox.ShowInfo(Format('%s has been uninstalled successfully.', [sFileName]));
		irNotInstalled :
		TMsgBox.ShowInfo(Format('%s is not installed.', [sFileName]));
		irUnInstallError :
		TMsgBox.ShowInfo(Format('%s uninstall failed.', [sFileName]));
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
		reg.free;
	end;
end;

function TPackageInstallMain.installExpertDll(const _delphiVersion : TDelphiVersion; const _dllPath : string; var _sExpertDesc : string)
	: EInstallResult;
var
	sDescription : string;
begin
	if not FileExists(_dllPath) then begin
		raise Exception.Create('Could not find expert dll file.' + #13#10 + _dllPath);
	end;

	sDescription := _sExpertDesc;
	if sDescription.IsEmpty then begin
		sDescription := GetPackageDescription(PChar(_dllPath));
		_sExpertDesc := sDescription;
	end;

	// StdOut.WriteLn(_('Installing "%s"'), [_dllPath.Filename]);
	// StdOut.WriteLn(_('Description: "%s"'), [Description]);

	if _delphiVersion.IsKnownExpert(_dllPath, sDescription) then begin
		Result := EInstallResult.irAlreadyInstalled;
	end else begin
		_delphiVersion.AddExpert(sDescription, _dllPath);
		Result := EInstallResult.irInstallSuccess;
	end;
end;

function TPackageInstallMain.uninstallExpertDll(const _DelphiVer : TDelphiVersion; const _sPath : string; var _sExpertDesc : string)
	: EInstallResult;
begin
	if _DelphiVer.IsKnownExpert(_sPath, _sExpertDesc) then begin
		_DelphiVer.RemoveExpertDll(_sPath, _sExpertDesc);
		Result := EInstallResult.irUninstallSuccess;
	end else begin
		Result := EInstallResult.irNotInstalled;
	end;
end;

end.
