unit RipGrepper.OpenWith;

interface

uses

	System.Classes,
	Winapi.Windows,
	Winapi.ShellAPI,
	Vcl.Menus,
	RipGrepper.OpenWith.Params;

type
	TOpenWith = class
		private
			class function GetSelectedCmd(_owpTestFile : TOpenWithParams) : string;

		public
			class procedure Execute(const _owp : TOpenWithParams);
	end;

implementation

uses
	RipGrepper.OpenWith.ConfigForm,
	System.SysUtils,
	Vcl.Dialogs,
	RipGrepper.OpenWith.Runner,
	RipGrepper.OpenWith.CmdListForm,
	RipGrepper.Common.Settings.RipGrepperSettings,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Helper.UI;

class function TOpenWith.GetSelectedCmd(_owpTestFile : TOpenWithParams) : string;
begin
	var dbgMsg := TDebugMsgBeginEnd.New('OpenWithFunc.GetSelectedCmd');
	var
	settings := GSettings.OpenWithSettings;
	settings.TestFile := _owpTestFile;
	dbgMsg.MsgFmt('TestFile: %s ', [settings.TestFile.ToString]);
	Result := TOpenWithCmdList.CreateAndShow(settings);
	TDebugUtils.DebugMessage((Format('OpenWithFunc.GetSelectedCmd Result: "%s"', [Result])));
	settings.TestFile := default (TOpenWithParams);
end;

class procedure TOpenWith.Execute(const _owp : TOpenWithParams);
var
	iPos : Integer;
	sEditorCmd : string;
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TOpenWith.Execute');
	dbgMsg.MsgFmt('%s ', [_owp.ToString]);

	if FileExists(_owp.FileName) then begin
		sEditorCmd := GetSelectedCmd(_owp);

		if sEditorCmd.IsEmpty then begin
			exit;
		end;

		dbgMsg.MsgFmt('Cmd: %s ', [sEditorCmd]);

		iPos := Pos('.EXE', AnsiUppercase(sEditorCmd));
		if iPos = 0 then begin
			TMsgBox.ShowError('There is no executable configured!');
			exit;
		end;

		TOpenWithRunner.RunEditorCommand(sEditorCmd, _owp);
	end;
end;

end.
