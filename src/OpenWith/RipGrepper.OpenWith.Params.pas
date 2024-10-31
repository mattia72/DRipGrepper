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
	{$IFNDEF STANDALONE} RipGrepper.Common.IOTAUtils, {$ENDIF}
	RipGrepper.Tools.DebugUtils,
	System.SysUtils;

class function TOpenWithParams.GetParamsOfActiveFileInDelphiIde : TOpenWithParams;
begin
	Result := default (TOpenWithParams);
	{$IFNDEF STANDALONE}
	var
	editPosition := IOTAUTils.GetEditPosition;
	if Assigned(editPosition) then begin
		Result.FileName := IOTAUtils.GxOtaGetCurrentSourceFile;;
		var
			sProjName : string := IOTAUtils.GxOtaGetCurrentProjectName;
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
	{$ENDIF}
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
