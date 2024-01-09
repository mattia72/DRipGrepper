unit RipGrepper.Tools.FileUtils;

interface

type
	TFileUtils = class(TObject)
		private
		public
			class function FindExecutable(sFileName : string; out sOutpuPath : string) : Boolean;
			class function GetValidPath(const aPath : string) : string;
	end;

implementation

uses
  System.SysUtils, Winapi.ShellAPI, Winapi.Windows, RipGrepper.Tools.DebugTools;

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

class function TFileUtils.GetValidPath(const aPath : string) : string;
begin
	Result := aPath;
	if Length(Result) > 0 then
		Result := IncludeTrailingPathDelimiter(Result);
end;

end.
