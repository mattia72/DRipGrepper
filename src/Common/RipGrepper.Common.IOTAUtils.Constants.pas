unit RipGrepper.Common.IOTAUtils.Constants;

interface

const
	CompanyRegPrefix = 'Embarcadero\';

	{$IFDEF VER350} // Delphi/RAD Studio 11 Alexandria
	GExpertsDll = 'GExpertsRS110.dll';
	TTabDockHostFormClassContainer = 'designide280.bpl';
	MajorVersionNumberChar = '28';
	IDEEnglishName = 'RAD Studio 11';
	CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\22.0';
	ClassBrowserStorageFolder = 'Classes.RADStudio11.0';
	{$ENDIF VER350}
	{$IFDEF VER360} // Delphi/RAD Studio 12 "Athens"
	GExpertsDll = 'GExpertsRS120.dll';
	TTabDockHostFormClassContainer = 'designide290.bpl';
	MajorVersionNumberChar = '29';
	IDEEnglishName = 'RAD Studio 12';
	CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\23.0';
	ClassBrowserStorageFolder = 'Classes.RADStudio12.0';
	{$ENDIF VER360}
	{$IF CompilerVersion > 36} // new Delphi version
	{ } 'Add the information for the new Delphi version above and increase the CompilerVersion in this conditional'
{$IFEND}
// ---------------

	implementation

end.
