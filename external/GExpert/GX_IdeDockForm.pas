unit GX_IdeDockForm;

interface

{$UNDEF DoNotCompileThis}
{$IFDEF DoNotCompileThis}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DeskForm, IDEMessages, IniFiles, ActnList, Menus, ecoHackDeskForm,
  GX_IdeDeskForm;

type
  TIDEDockType = (dtJoin, dtTab);

  TDockableForm = class(TDesktopForm)
    DockActionList: TActionList;
    DockableCmd: TAction;
    StayOnTopCmd: TAction;
    ZoomWindowCmd: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure DockableCmdExecute(Sender: TObject);
    procedure DockableCmdUpdate(Sender: TObject);
    procedure StayOnTopCmdExecute(Sender: TObject);
    procedure StayOnTopCmdUpdate(Sender: TObject);
    procedure ZoomWindowCmdExecute(Sender: TObject);
  private
    FIDEDockType: TIDEDockType;
    FDockEdge: TAlign;
    FAboutToDestroy: Boolean;
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
  protected
    FDockable: Boolean;
    function CreateDockParent(var Message: TCMDockClient): Boolean; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure SetDockable(const Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ForceShow; virtual;
    procedure GetTabDockRect(var ARect: TRect);
    function ToggleDockable: Boolean;
    procedure SaveWindowState(Desktop: TMemIniFile; isProject: Boolean); override;
    procedure LoadWindowState(Desktop: TMemIniFile); override;
    property AboutToDestroy: Boolean read FAboutToDestroy write FAboutToDestroy;
    property Dockable: Boolean read FDockable write SetDockable;
  end;

procedure PumpDockingMessages;

{$ENDIF DoNotCompileThis}

implementation

{$R *.dfm}

end.
