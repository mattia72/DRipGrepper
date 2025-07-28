unit RipGrepper.OpenWith.Constants;

interface

const
	OPEN_WITH_SETTINGS = 'OpenWithSettings';

	MAX_COMMAND_NUM = 10;
	// Tabulator
	SEPARATOR = #9;

	VSCODE_EDITOR_SETTING = 
	{ } 'FALSE' + SEPARATOR + 'VSCode' + SEPARATOR + 'code.exe --reuse-window "<DIR>" --goto "<FILE>:<LINE>:<COL>"' + SEPARATOR + 'Open in existing VsCode instance';

	DEFAULT_EDITORS : TArray<string> = [
	{ } 'TRUE' + SEPARATOR +  'Notepad' + SEPARATOR + 'notepad.exe "<FILE>"' + SEPARATOR + 'Open in Notepad',
	{ } 'TRUE' + SEPARATOR +  'Explorer' + SEPARATOR + 'explorer.exe /select,"<FILE>"' + SEPARATOR + 'Show in Explorer',
	VSCODE_EDITOR_SETTING,
	{ } 'FALSE' + SEPARATOR + 'Notepad++' + SEPARATOR + 'notepad++.exe "<FILE>" -n<LINE> -c<COL>' + SEPARATOR + 'Open in Notepad++',
	{ } 'FALSE' + SEPARATOR + 'Neovim' + SEPARATOR + 'nvim-qt.exe "<FILE>" -- -c "+normal <LINE>G<COL>l"' + SEPARATOR + 'Open in Neovim'
	];

implementation

end.
