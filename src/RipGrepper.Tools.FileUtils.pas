unit RipGrepper.Tools.FileUtils;

interface

uses
	System.SysUtils;

type
	TFileUtils = class(TObject)
		private
			class function GetAppName(const _exePath : string) : string;
			class function GetModuleVersion(Instance : THandle; out iMajor, iMinor, iRelease, iBuild : Integer) : Boolean;

		public
			class function FindExecutable(sFileName : string; out sOutpuPath : string) : Boolean;
			class function GetAppNameAndVersion(const _exePath : string) : string;
			class function GetAppVersion(const _exePath : string) : string;
			class function GetPackageNameAndVersion(Package : HMODULE) : string;
			class function ShortToLongPath(const ShortPathName : string) : string;
	end;

procedure GetPackageNameInfoProc(const Name : string; NameType : TNameType; Flags : Byte; Param : Pointer);

implementation

uses

	Winapi.ShellAPI,
	Winapi.Windows,
	RipGrepper.Tools.DebugUtils,
	System.IOUtils,
	RipGrepper.Common.Constants,
	System.Classes;

procedure GetPackageNameInfoProc(const Name : string; NameType : TNameType; Flags : Byte; Param : Pointer);
begin
	if NameType = ntDcpBpiName then begin
		PString(Param)^ := name;
	end;
end;

class function TFileUtils.FindExecutable(sFileName : string; out sOutpuPath : string) : Boolean;
var
	Buffer : array [0 .. MAX_PATH] of Char;
	res : integer;
begin
	Result := True;
	res := Winapi.ShellAPI.FindExecutable(PChar(sFileName), PChar(nil), &Buffer);
	case res of
		0, ERROR_FILE_NOT_FOUND, ERROR_PATH_NOT_FOUND :
		Result := False;
		else begin
			SetString(sOutpuPath, PChar(@Buffer[0]), Length(Buffer));
			SetLength(sOutpuPath, Pos(#0, sOutpuPath) - 1);
		end;
	end;

	TDebugUtils.DebugMessage('TFileUtils.FindExecutable: ' + sFileName + ' path:' + sOutpuPath);
	// WriteDebugMessage(sOutpuPath);
end;

class function TFileUtils.ShortToLongPath(const ShortPathName : string) : string;
var
	Retval : DWORD;
	Buff : array [0 .. MAX_PATH - 1] of Char;
begin
	Retval := GetLongPathName(PChar(ShortPathName), Buff, Length(Buff));
	Win32Check(Retval <> 0);
	Result := Buff;
end;

class function TFileUtils.GetAppName(const _exePath : string) : string;
begin
	Result := TPath.GetFileNameWithoutExtension(_exePath);
end;

class function TFileUtils.GetAppNameAndVersion(const _exePath : string) : string;
var
	imajor : integer;
	iminor : integer;
	irelease : integer;
	ibuild : integer;
	name : string;
begin
	name := GetAppName(_exePath);
	GetModuleVersion(0, imajor, iminor, irelease, ibuild);
	Result := Format(FORMAT_NAME_VERSION_INFO, [name, imajor, iminor, irelease]);
end;

class function TFileUtils.GetAppVersion(const _exePath : string) : string;
var
	imajor : integer;
	iminor : integer;
	irelease : integer;
	ibuild : integer;
begin
	GetModuleVersion(0, imajor, iminor, irelease, ibuild);
	Result := Format(FORMAT_VERSION_INFO, [imajor, iminor, irelease]);
end;

class function TFileUtils.GetModuleVersion(Instance : THandle; out iMajor, iMinor, iRelease, iBuild : Integer) : Boolean;
var
	fileInformation : PVSFIXEDFILEINFO;
	verlen : Cardinal;
	rs : TResourceStream;
	m : TMemoryStream;
	resource : HRSRC;
begin
	Result := False;
	// You said zero, but you mean "us"
	if Instance = 0 then
		Instance := HInstance;

	// UPDATE: Workaround bug in Delphi if resource doesn't exist
	resource := FindResource(Instance, PWideChar(1), RT_VERSION);
	if resource = 0 then begin
		iMajor := 0;
		iMinor := 0;
		iRelease := 0;
		iBuild := 0;
		Result := False;
		Exit;
	end;

	m := TMemoryStream.Create;
	try
		rs := TResourceStream.CreateFromID(Instance, 1, RT_VERSION);
		try
			m.CopyFrom(rs, rs.Size);
		finally
			rs.Free;
		end;

		m.Position := 0;
		if not VerQueryValue(m.Memory, '\', (* var *) Pointer(fileInformation), (* var *) verlen) then begin
			iMajor := 0;
			iMinor := 0;
			iRelease := 0;
			iBuild := 0;
			Exit;
		end;

		iMajor := fileInformation.dwFileVersionMS shr 16;
		iMinor := fileInformation.dwFileVersionMS and $FFFF;
		iRelease := fileInformation.dwFileVersionLS shr 16;
		iBuild := fileInformation.dwFileVersionLS and $FFFF;
	finally
		m.Free;
	end;

	Result := True;
end;

class function TFileUtils.GetPackageNameAndVersion(Package : HMODULE) : string;
var
	Flags : Integer;
	packageName : string;
begin
	// Flags should be an out param, but is a var, so this assignment is a little pointless
	Flags := 0;
	Result := '';
	GetPackageInfo(package, @packageName, Flags, GetPackageNameInfoProc);
	Result := GetAppNameAndVersion(packageName);
end;

end.
