unit RipGrepper.OpenWith.Constants;

interface

const
	OPEN_WITH_SETTINGS = 'OpenWithSettings';
	OPENWITH_COMMAND_KEY = 'Command_';

	MAX_COMMAND_NUM = 10;
	// Tabulator
	SEPARATOR = #9;

	DEFAULT_EDITORS : TArray<string> = [
	{ } 'TRUE' + SEPARATOR + 'notepad.exe "<FILE>"',
	{ } 'TRUE' + SEPARATOR + 'explorer.exe /select,"<FILE>"',
	{ } 'FALSE' + SEPARATOR + 'code.exe --reuse-window "<DIR>" --goto "<FILE>:<LINE>:<COL>"',
	{ } 'FALSE' + SEPARATOR + 'notepad++.exe "<FILE>" -n<LINE> -c<COL>',
	{ } 'FALSE' + SEPARATOR + 'nvim-qt.exe "<FILE>" -- -c "+normal <LINE>G<COL>l"'];

implementation

end.
