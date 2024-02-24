unit GX_IdeDock;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  SysUtils, Classes, Forms, Controls,
  MenuBar, Menus, Messages,
{$IFDEF IDE_IS_HIDPI_AWARE}
  u_dzDpiScaleUtils,
{$ELSE}
  u_dzDpiScaleUtilsDummy,
{$ENDIF}
  // You must link to the DesignIde package to compile this unit
  DockForm;

type
{$IFDEF EnableIdeDockingSupport}
  TDummyPopupMenu = class(TPopupMenu)
  private
    OwnerMenu: TMenu;
  public
    function IsShortCut(var Message: TWMKey): Boolean; override;
  end;
{$ENDIF EnableIdeDockingSupport}

{$UNDEF TrickTheIdeAncestorForm}  // this must always be undefined, so that
{$IFDEF TrickTheIdeAncestorForm}  // <--- this define is always false
  TfmIdeDockForm = class(TDummyIdeDockForm);
{$ELSE}
  TfmIdeDockForm = class(TDockableForm)
{$ENDIF TrickTheIdeAncestorForm}
  protected
    FMenuBar: TMenuBar;
    FScaler: TFormDpiScaler;
{$IFDEF IDE_IS_HIDPI_AWARE}
    procedure WMDpiChanged(var _Msg: TWMDpi); message WM_DPICHANGED;
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); virtual;
    procedure ArrangeControls; virtual;
{$ENDIF}
  protected
    procedure InitDpiScaler;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    {$IFDEF EnableIdeDockingSupport}
    {$ENDIF EnableIdeDockingSupport}
  end;

type
  TIdeDockFormClass = class of TfmIdeDockForm;

type
  EIdeDockError = class(Exception);

  IIdeDockManager = interface(IUnknown)
  ['{408FC1B1-BD7A-4401-93C2-B41E1D19580B}']
    // Note: IdeDockFormName must be IDE-unique
    procedure RegisterDockableForm(IdeDockFormClass: TIdeDockFormClass;
       var IdeDockFormVar; const IdeDockFormName: string);
    procedure UnRegisterDockableForm(
       var IdeDockFormVar; const IdeDockFormName: string);

    procedure ShowForm(Form: TForm);
  end;

function IdeDockManager: IIdeDockManager;

implementation

{$R *.dfm}

uses
  DeskForm, DeskUtil,
  u_dzVclUtils,
  GX_GenericClasses, GX_GxUtils;

type
  TIdeDockManager = class(TSingletonInterfacedObject, IIdeDockManager)
  public
    // Note: IdeDockFormName must be IDE-unique
    procedure RegisterDockableForm(IdeDockFormClass: TIdeDockFormClass;
       var IdeDockFormVar; const IdeDockFormName: string);
    procedure UnRegisterDockableForm(
       var IdeDockFormVar; const IdeDockFormName: string);

    procedure ShowForm(Form: TForm);
  end;

{ TIdeDockManager }

procedure TIdeDockManager.ShowForm(Form: TForm);
begin
  {$IFDEF EnableIdeDockingSupport}
  with Form as TDockableForm do
  begin
    if not Floating then
    begin
      ForceShow;
      FocusWindow(Form);
    end
    else
      Show;
  end;
  {$ELSE}
    Form.Show;
  {$ENDIF EnableIdeDockingSupport}
end;

procedure TIdeDockManager.RegisterDockableForm(IdeDockFormClass: TIdeDockFormClass;
       var IdeDockFormVar; const IdeDockFormName: string);
begin
  {$IFDEF EnableIdeDockingSupport}
  if Assigned(RegisterFieldAddress) then
    RegisterFieldAddress(IdeDockFormName, @IdeDockFormVar);

  RegisterDesktopFormClass(IdeDockFormClass, IdeDockFormName, IdeDockFormName);
  {$ENDIF EnableIdeDockingSupport}
end;

procedure TIdeDockManager.UnRegisterDockableForm(var IdeDockFormVar; const IdeDockFormName: string);
{$IFDEF EnableIdeDockingSupport}
{$ENDIF EnableIdeDockingSupport}
begin
  {$IFDEF EnableIdeDockingSupport}
  if Assigned(UnregisterFieldAddress) then
    UnregisterFieldAddress(@IdeDockFormVar);
  {$ENDIF EnableIdeDockingSupport}
end;

var
  PrivateIdeDockManager: TIdeDockManager = nil;

function IdeDockManager: IIdeDockManager;
begin
  Result := PrivateIdeDockManager as IIdeDockManager;
end;

{ TfmIdeDockForm }

constructor TfmIdeDockForm.Create(AOwner: TComponent);
begin
  inherited;
  GxSetDefaultFont(Self);

  {$IFDEF EnableIdeDockingSupport}
  if Menu <> nil then
  begin
    FMenuBar := TMenuBar.Create(Self);
    FMenuBar.Parent := Self;
    FMenuBar.Menu := Menu;
    FMenuBar.Height := GetSystemMetrics(SM_CYMENU) + 2;
    Menu := nil;
  end;
  if (PopupMenu = nil) and (FMenuBar <> nil) then
  begin
    PopupMenu := TDummyPopupMenu.Create(Self);
    TDummyPopupMenu(PopupMenu).OwnerMenu := FMenuBar.Menu;
  end;

  DeskSection := Name;
  AutoSave := True;
  SaveStateNecessary := True;
  {$ENDIF EnableIdeDockingSupport}
end;

destructor TfmIdeDockForm.Destroy;
begin
  {$IFDEF EnableIdeDockingSupport}
  SaveStateNecessary := True;
  {$ENDIF EnableIdeDockingSupport}
  FreeAndNil(FScaler);
  inherited;
end;

{$IFDEF IDE_IS_HIDPI_AWARE}
procedure TfmIdeDockForm.WMDpiChanged(var _Msg: TWMDpi);
begin
  inherited;
  ApplyDpi(_Msg.YDpi, _Msg.ScaledRect);
  _Msg.Result := 0;
end;

procedure TfmIdeDockForm.ApplyDpi(_NewDpi: integer; _NewBounds: PRect);
begin
  if Assigned(FScaler) then
    FScaler.ApplyDpi(_NewDpi, _NewBounds);
  ArrangeControls;
end;

procedure TfmIdeDockForm.ArrangeControls;
begin
  // do nothing
end;
{$ENDIF}

procedure TfmIdeDockForm.InitDpiScaler;
begin
  FScaler := TFormDpiScaler.Create(Self);
{$IFDEF IDE_IS_HIDPI_AWARE}
  ApplyDpi(TScreen_GetDpiForForm(Self), nil);
{$ENDIF}
end;

procedure TfmIdeDockForm.Loaded;
begin
  inherited;
  Scaled := False;
{$IFDEF HAS_PROPERTY_OLDCREATEORDER}
  // Delphi 11 removes this property from all dfm files, but unfortunately its default value
  // is True, so older Delphi versions will add it again as True which will break several
  // forms that rely on it to be False.
  OldCreateOrder := False;
{$ENDIF}
end;

{$IFDEF EnableIdeDockingSupport}

{ TDummyPopupMenu }

function TDummyPopupMenu.IsShortCut(var Message: TWMKey): Boolean;
begin
  // Call the form's IsShortCut so docked forms can use main menu shortcuts
  Result := (OwnerMenu <> nil) and OwnerMenu.IsShortCut(Message);
end;
{$ENDIF EnableIdeDockingSupport}

initialization
  PrivateIdeDockManager := TIdeDockManager.Create;
finalization
  FreeAndNil(PrivateIdeDockManager);
end.
