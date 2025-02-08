unit RipGrepper.OpenWith.Params;

interface

type
	TOpenWithParams = record
		RelativeBaseDirPath : string;
		FilePath : string;
		Row : Integer;
		Column : Integer;
		IsEmpty : Boolean;

		private
		public
			class function GetParamsOfActiveFileInDelphiIde : TOpenWithParams; static;
			function GetRelativePath : string;
			class operator Initialize(out Dest : TOpenWithParams);
			function ToString : string;
	end;

implementation

uses
	{$IFNDEF STANDALONE} RipGrepper.Common.IOTAUtils, {$ENDIF}
	RipGrepper.Tools.DebugUtils,
	System.SysUtils, RipGrepper.Common.Constants;

class function TOpenWithParams.GetParamsOfActiveFileInDelphiIde : TOpenWithParams;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithParams.GetParamsOfActiveFileInDelphiIde');
	Result := default (TOpenWithParams);
	{$IFNDEF STANDALONE}
	var
	editPosition := IOTAUTils.GetEditPosition;
	if Assigned(editPosition) then begin
		Result.FilePath := IOTAUtils.GxOtaGetCurrentSourceFile;;
		var
			sProjPath : string := IOTAUtils.GetActiveProjectFilePath;
		dbgMsg.MsgFmt('proj: %s ', [sProjPath]);
		if (sProjPath <> '') then begin
			Result.RelativeBaseDirPath := ExtractFileDir(sProjPath);
		end else begin
			Result.RelativeBaseDirPath := ExtractFileDir(Result.FilePath);
		end;
		Result.Row := editPosition.Row;
		Result.Column := editPosition.Column;
		Result.IsEmpty := False;
		dbgMsg.MsgFmt('dir: %s' + CRLF + 'file: %s ' + CRLF + 'row: %d col: %d ',
			{ } [Result.RelativeBaseDirPath, Result.FilePath, Result.Row, Result.Column]);
	end;
	{$ENDIF}
end;

function TOpenWithParams.GetRelativePath : string;
begin
	Result := ExtractRelativePath(RelativeBaseDirPath + '\', FilePath);
end;

function TOpenWithParams.ToString : string;
begin
	Result := Format('%s(%d:%d)', [GetRelativePath(), Row, Column]);
end;

class operator TOpenWithParams.Initialize(out Dest : TOpenWithParams);
begin
	Dest.RelativeBaseDirPath := '';
	Dest.FilePath := '';
	Dest.Row := -1;
	Dest.Column := -1;
	Dest.IsEmpty := True;
end;

end.
