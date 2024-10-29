unit GX_VerDepConst;

// This can't simply be replaces by using GX_dzCompilerAndRtlVersions because
// we also use the GX_BCB symbol below!
{$I GX_CondDefine.inc}

interface

const
  // This will likely never change again in future IDE versions, so it is global
  ExptIntfExpertEntryPoint = 'INITEXPERT0017';
  TTabDockHostFormName = 'TTabDockHostForm';

  {$IFDEF GX_VER220_up} 
  CompanyRegPrefix = 'Embarcadero\';
  {$ELSE}
    {$IFDEF GX_VER200_up}
    CompanyRegPrefix = 'CodeGear\';
    {$ELSE}
    CompanyRegPrefix = 'Borland\';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF GX_BCB}
    IdeProductName = 'BCB';
  {$ELSE}
    IdeProductName = 'Delphi';
  {$ENDIF GX_BCB}


  // IDE Major Versions
  {$IFDEF VER140} // Delphi/BCB 6
    GExpertsDll = 'GExpertsD6.dll';
    TTabDockHostFormClassContainer = 'designide60.bpl';
    MajorVersionNumberChar = '6';
    {$IFDEF BCB}
    IDEEnglishName = 'C++Builder 6';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'C++Builder\6.0';
    ClassBrowserStorageFolder = 'Storage6.BCB';
    {$ELSE}
    IDEEnglishName = 'Delphi 6';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'Delphi\6.0';
    ClassBrowserStorageFolder = 'Storage6';
    {$ENDIF}
  {$ENDIF VER140}

  {$IFDEF VER150} // Delphi/BCB 7
    GExpertsDll = 'GExpertsD7.dll';
    TTabDockHostFormClassContainer = 'designide70.bpl';
    MajorVersionNumberChar = '7';
    IDEEnglishName = 'Delphi 7';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'Delphi\7.0';
    ClassBrowserStorageFolder = 'Storage7';
  {$ENDIF VER150}

  {$IFDEF VER160} // Delphi.NET 8
    GExpertsDll = 'GExpertsD8.dll';
    TTabDockHostFormClassContainer = 'designide71.bpl';
    MajorVersionNumberChar = '8';
    IDEEnglishName = 'Delphi 8';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\2.0';
    ClassBrowserStorageFolder = 'Classes.D8';
  {$ENDIF VER160}

  {$IFDEF VER170} // Delphi 2005
    GExpertsDll = 'GExpertsDelphi2005.dll';
    TTabDockHostFormClassContainer = 'designide90.bpl';
    MajorVersionNumberChar = '9';
    IDEEnglishName = 'Delphi 2005';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\3.0';
    ClassBrowserStorageFolder = 'Classes.Delphi2005';
  {$ENDIF VER170}

  {$IFDEF VER180}
    TTabDockHostFormClassContainer = 'designide100.bpl';
    {$IFNDEF VER185} // BDS 2006
    GExpertsDll = 'GExpertsBDS2006.dll';
    MajorVersionNumberChar = '10';
    IDEEnglishName = 'BDS 2006';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\4.0';
    ClassBrowserStorageFolder = 'Classes.BDS2006';
    {$ELSE}         // Delphi/RAD Studio 2007
    MajorVersionNumberChar = '11';
    GExpertsDll = 'GExpertsDelphi2007.dll';
    IDEEnglishName = 'Delphi 2007';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\5.0';
    ClassBrowserStorageFolder = 'Classes.Delphi2007';
    {$ENDIF}
  {$ENDIF VER180}

  {$IFDEF VER200} // Delphi/RAD Studio 2009
    GExpertsDll = 'GExpertsRS2009.dll';
    TTabDockHostFormClassContainer = 'designide120.bpl';
    MajorVersionNumberChar = '12';
    IDEEnglishName = 'RAD Studio 2009';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\6.0';
    ClassBrowserStorageFolder = 'Classes.RADStudio2009';
  {$ENDIF VER200}

  {$IFDEF VER210} // Delphi/RAD Studio 2010
    GExpertsDll = 'GExpertsRS2010.dll';
    TTabDockHostFormClassContainer = 'designide140.bpl';
    MajorVersionNumberChar = '14';
    IDEEnglishName = 'RAD Studio 2010';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\7.0';
    ClassBrowserStorageFolder = 'Classes.RADStudio2010';
  {$ENDIF VER210}
  
  {$IFDEF VER220} // Delphi/RAD Studio XE 1
    GExpertsDll = 'GExpertsRSXE1.dll';
    TTabDockHostFormClassContainer = 'designide150.bpl';
    MajorVersionNumberChar = '15';
    IDEEnglishName = 'RAD Studio XE';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\8.0';
    ClassBrowserStorageFolder = 'Classes.RADStudioXE1';
  {$ENDIF VER220}
  
  {$IFDEF VER230} // Delphi/RAD Studio XE 2
    GExpertsDll = 'GExpertsRSXE2.dll';
    TTabDockHostFormClassContainer = 'designide160.bpl';
    MajorVersionNumberChar = '16';
    IDEEnglishName = 'RAD Studio XE 2';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\9.0';
    ClassBrowserStorageFolder = 'Classes.RADStudioXE2';
  {$ENDIF VER230}

  {$IFDEF VER240} // Delphi/RAD Studio XE 3
    GExpertsDll = 'GExpertsRSXE3.dll';
    TTabDockHostFormClassContainer = 'designide170.bpl';
    MajorVersionNumberChar = '17';
    IDEEnglishName = 'RAD Studio XE 3';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\10.0';
    ClassBrowserStorageFolder = 'Classes.RADStudioXE3';
  {$ENDIF VER240}

  {$IFDEF VER250} // Delphi/RAD Studio XE 4
    GExpertsDll = 'GExpertsRSXE4.dll';
    TTabDockHostFormClassContainer = 'designide180.bpl';
    MajorVersionNumberChar = '18';
    IDEEnglishName = 'RAD Studio XE 4';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\11.0';
    ClassBrowserStorageFolder = 'Classes.RADStudioXE4';
  {$ENDIF VER250}

  {$IFDEF VER260} // Delphi/RAD Studio XE 5
    GExpertsDll = 'GExpertsRSXE5.dll';
    TTabDockHostFormClassContainer = 'designide190.bpl';
    MajorVersionNumberChar = '19';
    IDEEnglishName = 'RAD Studio XE 5';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\12.0';
    ClassBrowserStorageFolder = 'Classes.RADStudioXE5';
  {$ENDIF VER260}

  {$IFDEF VER270} // Delphi/RAD Studio XE 6
    GExpertsDll = 'GExpertsRSXE6.dll';
    TTabDockHostFormClassContainer = 'designide200.bpl';
    MajorVersionNumberChar = '20';
    IDEEnglishName = 'RAD Studio XE 6';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\14.0'; // Not 13.0
    ClassBrowserStorageFolder = 'Classes.RADStudioXE6';
  {$ENDIF VER270}

  {$IFDEF VER280} // Delphi/RAD Studio XE 7
    GExpertsDll = 'GExpertsRSXE7.dll';
    TTabDockHostFormClassContainer = 'designide210.bpl';
    MajorVersionNumberChar = '21';
    IDEEnglishName = 'RAD Studio XE 7';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\15.0';
    ClassBrowserStorageFolder = 'Classes.RADStudioXE7';
  {$ENDIF VER280}

  {$IFDEF VER290} // Delphi/RAD Studio XE 8
    GExpertsDll = 'GExpertsRSXE8.dll';
    TTabDockHostFormClassContainer = 'designide220.bpl';
    MajorVersionNumberChar = '22';
    IDEEnglishName = 'RAD Studio XE 8';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\16.0';
    ClassBrowserStorageFolder = 'Classes.RADStudioXE8';
  {$ENDIF VER290}

  {$IFDEF VER300} // Delphi/RAD Studio 10 Seattle
    GExpertsDll = 'GExpertsRS10.dll';
    TTabDockHostFormClassContainer = 'designide230.bpl';
    MajorVersionNumberChar = '23';
    IDEEnglishName = 'RAD Studio 10';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\17.0';
    ClassBrowserStorageFolder = 'Classes.RADStudio10';
  {$ENDIF VER300}

  {$IFDEF VER310} // Delphi/RAD Studio 10.1 Berlin
    GExpertsDll = 'GExpertsRS101.dll';
    TTabDockHostFormClassContainer = 'designide240.bpl';
    MajorVersionNumberChar = '24';
    IDEEnglishName = 'RAD Studio 10.1';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\18.0';
    ClassBrowserStorageFolder = 'Classes.RADStudio10.1';
  {$ENDIF VER310}

  {$IFDEF VER320} // Delphi/RAD Studio 10.2 Tokyo
    GExpertsDll = 'GExpertsRS102.dll';
    TTabDockHostFormClassContainer = 'designide250.bpl';
    MajorVersionNumberChar = '25';
    IDEEnglishName = 'RAD Studio 10.2';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\19.0';
    ClassBrowserStorageFolder = 'Classes.RADStudio10.2';
  {$ENDIF VER320}

  {$IFDEF VER330} // Delphi/RAD Studio 10.3 Rio
    GExpertsDll = 'GExpertsRS103.dll';
    TTabDockHostFormClassContainer = 'designide260.bpl';
    MajorVersionNumberChar = '26';
    IDEEnglishName = 'RAD Studio 10.3';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\20.0';
    ClassBrowserStorageFolder = 'Classes.RADStudio10.3';
  {$ENDIF VER330}

  {$IFDEF VER340} // Delphi/RAD Studio 10.4 Sydney
    GExpertsDll = 'GExpertsRS104.dll';
    TTabDockHostFormClassContainer = 'designide270.bpl';
    MajorVersionNumberChar = '27';
    IDEEnglishName = 'RAD Studio 10.4';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\21.0';
    ClassBrowserStorageFolder = 'Classes.RADStudio10.4';
  {$ENDIF VER340}

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
  'Add the information for the new Delphi version above and increase the CompilerVersion in this conditional'
{$IFEND}

implementation

end.
