unit GX_IdeBaseDock;

interface

{$UNDEF DoNotCompileThis}
{$IFDEF DoNotCompileThis}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DockForm, IDEMessages, ActnList, ecoHackDockForm;

type
  TBaseDockHostForm = class(TDockableForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FLastFocusedClient: TDockableForm;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    function GetVisibleClientCount: Integer;
    procedure UMFinalUndock(var Message: TMessage); message UM_FINALUNDOCK;
    procedure UMNewClientFocused(var Message: TMessage); message UM_NEWCLIENTFOCUSED;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
  protected
    function CreateDockParent(var Message: TCMDockClient): Boolean; override;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
    function GetDialogCharParentClass: TWinControlClass; virtual; abstract;
    function GetDockSiteControl: TWinControl; virtual;
    procedure SetDockable(const Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ManualDockClient(Client: TControl; DropControl: TControl;
      ControlSide: TAlign; Replace: Boolean); virtual;
    procedure ResetCaption;
    procedure ResetStayOnTop;
    property VisibleClientCount: Integer read GetVisibleClientCount;
  end;

  TBaseDockHostFormClass = class of TBaseDockHostForm;

var
  HostDockFormList: TList;

procedure DestroyDockHosts;

{$ENDIF DoNotCompileThis}

implementation

{$R *.dfm}

end.
