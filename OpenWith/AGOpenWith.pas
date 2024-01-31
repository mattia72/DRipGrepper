unit AGOpenWith;

interface

uses

	System.Classes,
	Winapi.Windows,
	Winapi.ShellAPI,
	Vcl.Menus;

type
	TOpenWith = class
		private
			class function GetSelectedCmd : string;
		public
			class procedure Execute(const _sFilePath : string);
	end;

implementation

uses
	AGOpenWithConfig_Form,
	System.SysUtils,
	Vcl.Dialogs,
	AGOpenWithRunner,
	AGOpenWithList_Form,
	RipGrepper.OpenWith.Constants,
	RipGrepper.Common.Settings;

class function TOpenWith.GetSelectedCmd : string;
begin
	var
	settings := TRipGrepperSettingsInstance.Instance.RipGrepperOpenWithSettings;
	Result := TAGOpenWithList.CreateAndShow(settings);
	OutputDebugString(PChar(Format('OpenWithFunc.GetSelectedCmd Result: "%s"', [Result])));
end;

class procedure TOpenWith.Execute(const _sFilePath : string);
var
	iPos : Integer;
	sEditorCmd : string;
begin
	// sFileName := OtaGetCurrentSourceFile;
	OutputDebugString(PChar(Format('TOpenWith.Execute %s ', [_sFilePath])));

	if FileExists(_sFilePath) then begin
		sEditorCmd := GetSelectedCmd();

		if sEditorCmd.IsEmpty then begin
			exit;
		end;

		OutputDebugString(PChar(Format('TOpenWith.Execute Cmd: %s ', [sEditorCmd])));

		iPos := Pos('.EXE', AnsiUppercase(sEditorCmd));
		if iPos = 0 then begin
			MessageDlg('Editor soll in AGENDA Expert-Manager configuriert werden!', mtError, [mbOK], 0);
			exit;
		end;

		TOpenWithRunner.RunEditorCommand(sEditorCmd, _sFilePath);
	end;

end;

end.
