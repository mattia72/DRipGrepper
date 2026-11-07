unit RipGrepper.Common.IOTAUtils.Constants;

interface

const
	CompanyRegPrefix = 'Embarcadero\';

	{$IFDEF VER350} // Delphi/RAD Studio 11 Alexandria
	TTabDockHostFormClassContainer = 'designide280.bpl';
	MajorVersionNumberChar = '28';
	IDEEnglishName = 'RAD Studio 11';
	CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\22.0';
	ClassBrowserStorageFolder = 'Classes.RADStudio11.0';
	{$ENDIF VER350}
	{$IFDEF VER360} // Delphi/RAD Studio 12 "Athens"
	TTabDockHostFormClassContainer = 'designide290.bpl';
	MajorVersionNumberChar = '29';
	IDEEnglishName = 'RAD Studio 12';
	CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\23.0';
	ClassBrowserStorageFolder = 'Classes.RADStudio12.0';
	{$ENDIF VER360}
	{$IFDEF VER370} // Delphi/RAD Studio 13 Florence
	TTabDockHostFormClassContainer = 'designide370.bpl';
	MajorVersionNumberChar = '37';
	IDEEnglishName = 'RAD Studio 13';
	CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\37.0';
	ClassBrowserStorageFolder = 'Classes.RADStudio13.0';
	{$ENDIF VER370}
	{$IF CompilerVersion > 37} // new Delphi version
	{ } 'Add the information for the new Delphi version above and increase the CompilerVersion in this conditional'
{$IFEND}
// ---------------

	implementation

end.
