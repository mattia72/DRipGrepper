unit ToolsAPI;

interface

uses
  Windows,
  SysUtils;

type
  IOTAModule = interface
    function GetFileName: string;
  end;

type
  IOTAEditWriter = interface
    procedure DeleteTo(_Idx: Integer);
    procedure Insert(_Text: PAnsiChar);
  end;

type
  IOTASourceEditor = interface
    function Filename: string;
  end;

type
  IOTAFormEditor = interface
    function Filename: string;
  end;

type
  IOTAProject = interface
    function GetModule(_Idx: Integer): IOTAModule;
    function Filename: string;
    function GetModuleCount: Integer;
  end;

type
  IOTAProjectGroup = interface
    function GetProjects(_Idx: Integer): IOTAProject;
    function Filename: string;
    function ProjectCount: Integer;
    property Projects[_Idx: Integer]: IOTAProject read GetProjects;
  end;

type
  IOTASplashScreenServices = interface(IInterface)
    procedure AddPluginBitmap(const ACaption: string; ABitmap: HBITMAP;
      AIsUnRegistered: Boolean = False; const ALicenseStatus: string = '';
      const ASKUName: string = '');
  end;

type
  IOTAAboutBoxServices = interface(IInterface)
    // Note: This is not the same GUID as in ToolsAPI. That's on purpose
    ['{60AC3A29-E41A-4EA1-BE46-FD8FA90CF07C}']
    function AddPluginInfo(const ATitle, ADescription: string; AImage: HBITMAP;
      AIsUnRegistered: Boolean = False; const ALicenseStatus: string = '';
      const ASKUName: string = ''): Integer;
    procedure RemovePluginInfo(Index: Integer);
  end;

var
  BorlandIDEServices: IInterface = nil;
  SplashScreenServices: IOTASplashScreenServices;

implementation

end.
