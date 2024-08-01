unit RipGrepper.OpenWith;

interface

uses

	System.Classes,
	Winapi.Windows,
	Winapi.ShellAPI,
	Vcl.Menus,
	RipGrepper.OpenWith.SimpleTypes;

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
	var
	settings := GSettings.RipGrepperOpenWithSettings;
	settings.TestFile := _owpTestFile;
	Result := TOpenWithCmdList.CreateAndShow(settings);
	TDebugUtils.DebugMessage((Format('OpenWithFunc.GetSelectedCmd Result: "%s"', [Result])));
	settings.TestFile := default (TOpenWithParams);
end;

class procedure TOpenWith.Execute(const _owp : TOpenWithParams);
var
	iPos : Integer;
	sEditorCmd : string;
begin
	TDebugUtils.DebugMessage(Format('TOpenWith.Execute %s ', [_owp.FileName]));

	if FileExists(_owp.FileName) then begin
		sEditorCmd := GetSelectedCmd(_owp);

		if sEditorCmd.IsEmpty then begin
			exit;
		end;

		TDebugUtils.DebugMessage(Format('TOpenWith.Execute Cmd: %s ', [sEditorCmd]));

		iPos := Pos('.EXE', AnsiUppercase(sEditorCmd));
		if iPos = 0 then begin
			TMsgBox.ShowError('There is no executable configured!');
			exit;
		end;

		TOpenWithRunner.RunEditorCommand(sEditorCmd, _owp);
	end;

end;

end.
