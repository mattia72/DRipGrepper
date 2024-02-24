unit DripExtension.IOTA.Utils;

interface

type
	IOTAUTils = class

		public
			class function GetSettingFilePath: string;
	end;

implementation

uses
	System.IOUtils,
	ToolsAPI,
	System.SysUtils;

class function IOTAUTils.GetSettingFilePath: string;
var
	aIDEServices : IOTAServices;
begin
	aIDEServices := BorlandIDEServices as IOTAServices;
	Result := aIDEServices.GetLocalApplicationDataDirectory;
	if Result = '' then
		raise Exception.Create('GetSettingsFilePath: path not defined by IDEServices');
end;

end.
