unit GX_IdeDeskForm;

interface

{$UNDEF DoNotCompileThis}
{$IFDEF DoNotCompileThis}

uses
  SysUtils, Windows, Messages, Classes, Controls, Forms, Menus, DsgnIntf,
  IniFiles;

type
  TDesktopForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FAutoSave: Boolean;
    FDeskSection: string;
    FLocked: Boolean;
    FLoadedFromDesktop: Boolean;
    FSaveStateNecessary: Boolean;
    FWindowPlacement: TWindowPlacement;
    FWindowPlacementDirty: Boolean;
    procedure MainFormMadeVisible(Sender: TObject);
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
  protected
    FLastLoadedBounds: TRect;
    procedure DoMainFormShown; dynamic;
    function LoadDockClients(DeskTop: TMemIniFile; const Section: string;
      DockSite: TWinControl): Boolean; virtual;
    procedure LoadDockStream(DeskTop: TMemIniFile; const Section: string;
      DockSite: TWinControl); virtual;
    procedure SaveDockClients(DeskTop: TMemIniFile; const Section: string;
      DockSite: TWinControl); virtual;
    procedure SaveDockStream(DeskTop: TMemIniFile; const Section: string;
      DockSite: TWinControl); virtual;
    procedure UnlockUpdates; dynamic;
    property SaveStateNecessary: Boolean read FSaveStateNecessary write FSaveStateNecessary;
    procedure ZoomWindow; virtual;
    procedure Repaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditAction(Action: TEditAction); virtual;
    procedure GetEditState(var EditState: TEditState); virtual;
    procedure SaveWindowState(Desktop: TMemIniFile; isProject: Boolean); virtual;
    procedure LoadWindowState(Desktop: TMemIniFile); virtual;
    procedure LockUpdates; dynamic;
    property DeskSection: string read FDeskSection write FDeskSection;
    property AutoSave: Boolean read FAutoSave write FAutoSave;
    property LoadedFromDesktop: Boolean read FLoadedFromDesktop;
  end;

  TDesktopFormClass = class of TDesktopForm;

  TInitializeForm = procedure(Ident: TComponent);

var
  InitializeForm: TInitializeForm;
  RegisterMenu: procedure (AMenu: TMenu) of object = nil;
  UnregisterMenu: procedure (AMenu: TMenu) of object = nil;
  RegisterMainFormShown: procedure (Event: TNotifyEvent) = nil;
  UnregisterMainFormShown: procedure (Event: TNotifyEvent) = nil;
  GetDockable: function (const DeskSection: string): Boolean = nil;
  LoadedDesktopFormInstances: TStringList;

procedure BeginDesktopUpdate;
procedure EndDesktopUpdate;
function IsDesktopLocked: Boolean;

{$ENDIF DoNotCompileThis}

implementation

{$R *.dfm}

end.
