unit RipGrepper.OpenWith.Runner;

interface

uses
	System.Types,
	RipGrepper.OpenWith.Params;

type
	TOpenWithRunner = class

		public
			class function BuildParams(const _owp : TOpenWithParams; const _sParams : string) : string;
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
	RipGrepper.Tools.FileUtils,
	System.Generics.Collections;

class function TOpenWithRunner.BuildParams(const _owp : TOpenWithParams; const _sParams : string) : string;
var
	sCmdParams : string;
	pPlaceholder : TPair<string, Variant>;
	arrPlaceHolders : TArray<TPair<string, Variant>>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOpenWithRunner.BuildParams');

	sCmdParams := _sParams;

	arrPlaceHolders := [
	{ } TPair<string, Variant>.Create('<DIR>', _owp.RelativeBaseDirPath),
	{ } TPair<string, Variant>.Create('<FILE>', _owp.FilePath),
	{ } TPair<string, Variant>.Create('<LINE>', _owp.Row),
	{ } TPair<string, Variant>.Create('<ROW>', _owp.Row),
	{ } TPair<string, Variant>.Create('<COL>', _owp.Column)];

	for pPlaceholder in arrPlaceHolders do begin
		sCmdParams := StringReplace(sCmdParams, pPlaceholder.Key, '%s', [rfReplaceAll]);
		sCmdParams := Format(sCmdParams, [pPlaceholder.Value]);
		dbgMsg.MsgFmt('%s: %s ', [pPlaceholder.Key, sCmdParams]);
	end;

	Result := sCmdParams;
	dbgMsg.MsgFmt('COL: %s ', [sCmdParams]);

end;

class procedure TOpenWithRunner.RunEditorCommand(const _sEditorCmd : string; const _owp : TOpenWithParams);
var
	err : DWORD;
	sParams : string;
	cr : TCommandLineRec;
	sCmd : string;
begin
	cr := TCommandLineRec.ParseCommand(_sEditorCmd);
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
