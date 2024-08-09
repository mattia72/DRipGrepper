unit RipGrepper.OpenWith.Params;

interface

type
	TOpenWithParams = record
		DirPath : string;
		FileName : string;
		Row : Integer;
		Column : Integer;
		IsEmpty : Boolean;

		private
		public
			class function GetParamsOfActiveFileInDelphiIde : TOpenWithParams; static;
			class operator Initialize(out Dest : TOpenWithParams);
			function ToString : string;
	end;

implementation

uses
	RipGrepper.Common.IOTAUtils,
	RipGrepper.Tools.DebugUtils,
	System.SysUtils,
	ToolsAPI;

class function TOpenWithParams.GetParamsOfActiveFileInDelphiIde : TOpenWithParams;
var
	sProjName : string;
	editPosition : IOTAEditPosition;
begin
	editPosition := IOTAUTils.GetEditPosition;
	if Assigned(editPosition) then begin
		Result.FileName := IOTAUtils.GxOtaGetCurrentSourceFile;;
		sProjName := IOTAUtils.GxOtaGetCurrentProjectName;
		TDebugUtils.DebugMessage((Format('TOpenWithParams.GetParamsOfActiveFileInDelphiIde proj: %s ', [sProjName])));
		if (sProjName <> '') then begin
			Result.DirPath := ExtractFileDir(sProjName);
		end else begin
			Result.DirPath := ExtractFileDir(Result.FileName);
		end;
		Result.Row := editPosition.Row;
		Result.Column := editPosition.Column;
		Result.IsEmpty := False;
	end;
end;

function TOpenWithParams.ToString : string;
begin
	Result := Format('%s%s(%d:%d)', [DirPath, FileName, Row, Column]);

end;

class operator TOpenWithParams.Initialize(out Dest : TOpenWithParams);
begin
	Dest.DirPath := '';
	Dest.FileName := '';
	Dest.Row := -1;
	Dest.Column := -1;
	Dest.IsEmpty := True;
end;

end.
