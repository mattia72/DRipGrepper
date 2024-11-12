unit RipGrepper.Tools.FileUtils;

interface

uses
	System.SysUtils,
	System.Classes,
	System.Generics.Collections,
	ArrayEx;

type
	TCommandLineRec = record
		ExePath : string;
		Arguments : TArray<string>;
	end;

	TFileUtils = class(TObject)
		private
			class function GetAppName(const _exePath : string) : string;
			class function GetModuleVersion(Instance : THandle; out iMajor, iMinor, iRelease, iBuild : Integer) : Boolean;

		public
			class procedure CreateBackup(const fileName : string);
			class function FindExecutable(sFileName : string; out sOutpuPath : string) : Boolean;
			class function FindFileInSubDirs(const _dir : string; const _file : string) : string;
			class function GetAppNameAndVersion(const _exePath : string) : string;
			class function GetAppVersion(const _exePath : string) : string;
			class function ParseCommand(const _sCmd : string) : TCommandLineRec;
			class function GetPackageNameAndVersion(Package : HMODULE) : string;
			class function GetVsCodeDir : string;
			class function ShortToLongPath(const ShortPathName : string) : string;
	end;

	TReplaceData = record
		Row : integer;
		Line : string;
		class function New(const _row : Integer; const _line : string) : TReplaceData; static;
	end;

	TReplaceList = class
		Items : TDictionary<string, TArrayEx<TReplaceData>>;

		public
			constructor Create;
			destructor Destroy; override;
			procedure Add(fileName : string; row : integer; replaceLine : string);
	end;

procedure GetPackageNameInfoProc(const Name : string; NameType : TNameType; Flags : Byte; Param : Pointer);

implementation

uses

	Winapi.ShellAPI,
	Winapi.Windows,
	RipGrepper.Tools.DebugUtils,
	System.IOUtils,
	RipGrepper.Common.Constants,
	System.RegularExpressions,
	System.StrUtils,
	RipGrepper.Helper.UI,
	System.UITypes;

procedure GetPackageNameInfoProc(const Name : string; NameType : TNameType; Flags : Byte; Param : Pointer);
begin
	if NameType = ntDcpBpiName then begin
		PString(Param)^ := name;
	end;
end;

class procedure TFileUtils.CreateBackup(const fileName : string);
var
	backupFileName : string;
begin
	backupFileName := ChangeFileExt(fileName, FormatDateTime('.yyyymmddhhnn', Now) + '.bak');
	CopyFile(PWideChar(fileName), PWideChar(backupFileName), true);
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

class function TFileUtils.FindFileInSubDirs(const _dir : string; const _file : string) : string;
begin
	Result := '';
	var
	rgPath := TPath.Combine(_dir, _file);
	if FileExists(rgPath) then begin
		Result := rgPath;
	end else begin
		var
		dirs := TDirectory.GetDirectories(_dir);
		for var dir in dirs do begin
			Result := FindFileInSubDirs(dir, _file);
			if not Result.IsEmpty then begin
				break;
			end;
		end;
	end;
end;

class function TFileUtils.ShortToLongPath(const ShortPathName : string) : string;
var
	Retval : DWORD;
	Buff : array [0 .. MAX_PATH - 1] of Char;
begin
	Retval := GetLongPathName(PChar(ShortPathName), Buff, Length(Buff));
	{$WARN SYMBOL_PLATFORM OFF}
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
	Result := Format(FORMAT_NAME_VERSION_INFO, [name, APP_PLATFORM, imajor, iminor, irelease, ibuild]);
end;

class function TFileUtils.GetAppVersion(const _exePath : string) : string;
var
	imajor : integer;
	iminor : integer;
	irelease : integer;
	ibuild : integer;
begin
	GetModuleVersion(0, imajor, iminor, irelease, ibuild);
	Result := Format(FORMAT_VERSION_INFO, [imajor, iminor, irelease, ibuild]);
end;

class function TFileUtils.ParseCommand(const _sCmd : string) : TCommandLineRec;
var
	m : TMatch;
	r : TRegEx;
begin
	r := TRegex.Create('[''"]?(.*.(exe|com|bat))["'']? (.*)', [roIgnoreCase]);
	m := r.Match(_sCmd);

	if not m.Success then begin
		Exit;
	end;

	Result.ExePath := m.Groups[1].Value;
	Result.Arguments := m.Groups[3].Value.Split([' ']);
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

class function TFileUtils.GetVsCodeDir : string;
begin
	Result := '';
	var
		sCodePath : string;
	if TFileUtils.FindExecutable('code', sCodePath) then begin
		Result := TPath.GetDirectoryName(sCodePath);
	end;
end;

class function TReplaceData.New(const _row : Integer; const _line : string) : TReplaceData;
begin
	Result.Row := _row;
	Result.Line := _line;
end;

constructor TReplaceList.Create;
begin
	inherited;
	Items := TDictionary < string, TArrayEx < TReplaceData >>.Create;
end;

destructor TReplaceList.Destroy;
begin
	Items.Free;
	inherited;
end;

{ TReplaceList }

procedure TReplaceList.Add(fileName : string; row : integer; replaceLine : string);
var
	replaceList : TArrayEx<TReplaceData>;
begin
	if Items.TryGetValue(fileName, replaceList) then begin
		replaceList.Add(TReplaceData.New(row, replaceLine));
	end else begin
		replaceList := [TReplaceData.New(row, replaceLine)];
	end;
	Items.AddOrSetValue(fileName, replaceList);
end;

end.
