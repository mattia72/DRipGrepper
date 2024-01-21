unit RipGrepper.Tools.FileUtils;

interface

type
	TFileUtils = class(TObject)
		private
		public
			class function FindExecutable(sFileName : string; out sOutpuPath : string) : Boolean;
			class function GetAppNameAndVersion(const _exePath : string): string;
	end;

implementation

uses
  System.SysUtils, Winapi.ShellAPI, Winapi.Windows, RipGrepper.Tools.DebugTools,
  System.IOUtils;

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

class function TFileUtils.GetAppNameAndVersion(const _exePath : string): string;
var
	major : Cardinal;
	minor : Cardinal;
	build : Cardinal;
	name : string;
begin
	GetProductVersion(_exePath, major, minor, build);
	name := TPath.GetFileNameWithoutExtension(_exePath);
	Result := Format('%s v%d.%d.%d', [name, major, minor, build]);
end;

end.
