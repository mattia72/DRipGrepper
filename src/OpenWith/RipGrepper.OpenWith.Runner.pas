unit RipGrepper.OpenWith.Runner;

interface

uses
	System.Types,
	RipGrepper.OpenWith.Params;

type
	TOpenWithRunner = class

		private
			class function BuildParams(const _owp : TOpenWithParams; const _sParams : string) : string;

		public
			class procedure RunEditorCommand(const _sEditorCmd : string; const _owp : TOpenWithParams);
	end;

implementation

uses
	Winapi.Windows,
	System.SysUtils,
	Winapi.ShellAPI,
	Vcl.Dialogs,
	RipGrepper.Common.Constants,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Helper.UI,
	RipGrepper.Tools.FileUtils;

class function TOpenWithRunner.BuildParams(const _owp : TOpenWithParams; const _sParams : string) : string;
var
	sCmdParams : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithRunner.BuildParams');

	sCmdParams := _sParams;

	sCmdParams := StringReplace(sCmdParams, '<DIR>', '%s', [rfReplaceAll]);
	sCmdParams := Format(sCmdParams, [_owp.RelativeBaseDirPath]);

	sCmdParams := StringReplace(sCmdParams, '<FILE>', '%s', [rfReplaceAll]);
	sCmdParams := Format(sCmdParams, [_owp.FilePath]);
	dbgMsg.MsgFmt('TOpenWithRunner.BuildParams Params: %s ', [sCmdParams]);

	sCmdParams := StringReplace(sCmdParams, '<LINE>', '%d', [rfReplaceAll]);
	sCmdParams := Format(sCmdParams, [_owp.Row]);
	dbgMsg.MsgFmt('TOpenWithRunner.BuildParams Params: %s ', [sCmdParams]);

	sCmdParams := StringReplace(sCmdParams, '<COL>', '%d', [rfReplaceAll]);
	Result := Format(sCmdParams, [_owp.Column]);

end;

class procedure TOpenWithRunner.RunEditorCommand(const _sEditorCmd : string; const _owp : TOpenWithParams);
var
	err : DWORD;
	sParams : string;
	cr : TCommandLineRec;
	sCmd : string;
begin
	cr := TFileUtils.ParseCommand(_sEditorCmd);
	sParams := string.Join(' ', cr.Arguments);
	sCmd := '"' + cr.ExePath + '"';
	sParams := BuildParams(_owp, sParams);

	TDebugUtils.DebugMessage((Format('TOpenWithRunner.RunEditorCommand cmd: %s %s ', [sCmd, sParams])));
	ShellExecute(0, 'OPEN', PChar(sCmd), PChar(sParams), nil, SW_SHOWNORMAL);
	err := GetLastError;
	if err <> 0 then begin
		TMsgBox.ShowError(Format('%s %s' + CRLF + CRLF + '%s', [sCmd, sParams, SysErrorMessage(err)]));
	end;
end;

end.
