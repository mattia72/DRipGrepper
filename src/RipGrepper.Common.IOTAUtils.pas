unit RipGrepper.Common.IOTAUtils;

interface

uses
	Vcl.Menus;

type
	IOTAUTils = class

		public
			class function GetSettingFilePath : string;
			class function FindMenuItem(const Name : string) : TMenuItem;
			class function FindMenu(const Name : string) : TMenu;
			class function IsStandAlone: Boolean;
	end;

implementation

uses
	System.IOUtils,
	ToolsAPI,
	System.SysUtils,
	System.Classes,
	Vcl.Forms;

class function IOTAUTils.FindMenuItem(const Name : string) : TMenuItem;
var
	Comp : TComponent;
begin
	Comp := Application.MainForm.FindComponent(name);
	if (Comp <> nil) and (Comp is TMenuItem) then
		Result := TMenuItem(Comp)
	else
		Result := nil;
end;

class function IOTAUTils.FindMenu(const Name : string) : TMenu;
var
	Comp : TComponent;
begin
	Comp := Application.MainForm.FindComponent(name);
	if (Comp <> nil) and (Comp is TMenu) then
		Result := TMenu(Comp)
	else
		Result := nil;
end;

class function IOTAUTils.GetSettingFilePath : string;
var
	aIDEServices : IOTAServices;
begin
	aIDEServices := BorlandIDEServices as IOTAServices;
	Result := aIDEServices.GetLocalApplicationDataDirectory;
	if Result = '' then
		raise Exception.Create('GetSettingsFilePath: path not defined by IDEServices');
end;

class function IOTAUTils.IsStandAlone: Boolean;
begin
  Result := (BorlandIDEServices = nil);
end;

end.
