unit RipGrepper.OpenWith;

interface

uses

	System.Classes,
	Winapi.Windows,
	Winapi.ShellAPI,
	Vcl.Menus, RipGrepper.OpenWith.SimpleTypes;

type
	TOpenWith = class
		private
			class function GetSelectedCmd : string;
		public
			class procedure Execute(const _owp: TOpenWithParams);
	end;

implementation

uses
	RipGrepper.OpenWith.ConfigForm,
	System.SysUtils,
	Vcl.Dialogs,
	RipGrepper.OpenWith.Runner,
	RipGrepper.OpenWith.CmdListForm,
	RipGrepper.Common.Settings,
	RipGrepper.Tools.DebugTools;

class function TOpenWith.GetSelectedCmd : string;
begin
	var
	settings := TRipGrepperSettingsInstance.Instance.RipGrepperOpenWithSettings;
	Result := TOpenWithCmdList.CreateAndShow(settings);
	OutputDebugString(PChar(Format('OpenWithFunc.GetSelectedCmd Result: "%s"', [Result])));
end;

class procedure TOpenWith.Execute(const _owp: TOpenWithParams);
var
	iPos : Integer;
	sEditorCmd : string;
begin
	TDebugUtils.DebugMessage(Format('TOpenWith.Execute %s ', [_owp.FileName]));

	if FileExists(_owp.FileName) then begin
		sEditorCmd := GetSelectedCmd();

		if sEditorCmd.IsEmpty then begin
			exit;
		end;

		TDebugUtils.DebugMessage(PChar(Format('TOpenWith.Execute Cmd: %s ', [sEditorCmd])));

		iPos := Pos('.EXE', AnsiUppercase(sEditorCmd));
		if iPos = 0 then begin
			MessageDlg('Editor soll in AGENDA Expert-Manager configuriert werden!', mtError, [mbOK], 0);
			exit;
		end;

		TOpenWithRunner.RunEditorCommand(sEditorCmd, _owp);
	end;

end;

end.
