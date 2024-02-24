unit GX_IdeDeskUtil;

interface

{$UNDEF DoNotCompileThis}
{$IFDEF DoNotCompileThis}

uses Forms, Classes, Registry, IniFiles, DeskForm;

type
  TLoadDesktopProc = procedure (DeskFile: TCustomIniFile);
  TSaveDesktopProc = procedure (DeskFile: TCustomIniFile; IsProject: Boolean);

function FocusWindow(Window: TForm): TForm;

procedure RegisterDesktopFormClass(AFormClass: TDesktopFormClass; 
  const Section, InstanceName: string);
procedure RegisterDesktopProcs(LoadProc: TLoadDesktopProc; 
  SaveProc: TSaveDesktopProc);
procedure UnregisterDesktopProcs(LoadProc: TLoadDesktopProc; 
  SaveProc: TSaveDesktopProc);
procedure LoadDesktopFormClasses(Desk: TCustomIniFile);
procedure SaveDesktopFormClasses(Desk: TCustomIniFile; IsProject: Boolean);
procedure DoDesktopLoadProcs(Desk: TCustomIniFile);
procedure DoDesktopSaveProcs(Desk: TCustomIniFile; IsProject: Boolean);

var
  IDEIniFile: function: TRegistryIniFile = nil;
  GetFieldAddress: function (const FieldName: string): Pointer = nil;
  RegisterFieldAddress: procedure (const FieldName: string; Address: Pointer) = nil;
  UnregisterFieldAddress: procedure (Address: Pointer) = nil;
  AddMainFormCreatedEvent: procedure (Event: TNotifyEvent) = nil;
  RemoveMainFormCreatedEvent: procedure (Event: TNotifyEvent) = nil;
  AddMainFormShownEvent: procedure (Event: TNotifyEvent) = nil;
  RemoveMainFormShownEvent: procedure (Event: TNotifyEvent) = nil;

{$ENDIF DoNotCompileThis}

implementation

end.
