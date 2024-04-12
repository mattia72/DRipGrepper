unit RipGrepper.Tools.FileUtils;

interface

uses
	System.SysUtils;

type
	TFileUtils = class(TObject)
		private
			class function GetFileVersionStr(const AFileName : string) : string;
		public
			class function FindExecutable(sFileName : string; out sOutpuPath : string) : Boolean;
			class function GetAppNameAndVersion(const _exePath : string) : string;
			class function GetPackageNameAndVersion(Package : HMODULE): string;
	end;

procedure GetPackageNameInfoProc(const Name : string; NameType : TNameType; Flags : Byte; Param : Pointer);

implementation

uses

	Winapi.ShellAPI,
	Winapi.Windows,
	RipGrepper.Tools.DebugTools,
	System.IOUtils,
	RipGrepper.Common.Constants;

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

	TDebugUtils.DebugMessage(sFileName + ' path:' + sOutpuPath);
	// WriteDebugMessage(sOutpuPath);
end;

class function TFileUtils.GetAppNameAndVersion(const _exePath : string) : string;
var
	major : Cardinal;
	minor : Cardinal;
	build : Cardinal;
	name : string;
begin
	GetProductVersion(_exePath, major, minor, build);
	name := TPath.GetFileNameWithoutExtension(_exePath);
	Result := Format(FORMAT_VERSION_INFO, [name, major, minor, build]);
end;

class function TFileUtils.GetFileVersionStr(const AFileName : string) : string;
var
	FileName : string;
	LinfoSize : DWORD;
	lpdwHandle : DWORD;
	lpData : Pointer;
	lplpBuffer : PVSFixedFileInfo;
	puLen : DWORD;
begin
	Result := '';
	FileName := AFileName;
	UniqueString(FileName);
	LinfoSize := GetFileVersionInfoSize(PChar(FileName), lpdwHandle);
	if LinfoSize <> 0 then begin
		GetMem(lpData, LinfoSize);
		try
			if GetFileVersionInfo(PChar(FileName), lpdwHandle, LinfoSize, lpData) then
				if VerQueryValue(lpData, '\', Pointer(lplpBuffer), puLen) then
					Result := Format('%d.%d.%d.%d', [HiWord(lplpBuffer.dwFileVersionMS), LoWord(lplpBuffer.dwFileVersionMS),
						HiWord(lplpBuffer.dwFileVersionLS), LoWord(lplpBuffer.dwFileVersionLS)]);
		finally
			FreeMem(lpData);
		end;
	end;

end;

class function TFileUtils.GetPackageNameAndVersion(Package : HMODULE): string;
var
	Flags : Integer;
begin
	// Flags should be an out param, but is a var, so this assignment is a little pointless
	Flags := 0;
	Result := '';
	GetPackageInfo(package, @Result, Flags, GetPackageNameInfoProc);
	Result := Result + ' ' + GetFileVersionStr(Result);
end;

end.
