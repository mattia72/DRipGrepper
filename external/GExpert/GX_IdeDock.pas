///<summary>
/// This is a replacement for the GExperts IdeDockForm for stand alone experts </summary>
unit GX_IdeDock;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  u_dzDpiScaleUtils;

type
  TfmIdeDockForm = class(TForm)
  private
  protected
    FScaler: TFormDpiScaler;
{$IFDEF IDE_IS_HIDPI_AWARE}
    procedure WMDpiChanged(var _Msg: TWMDpi); message WM_DPICHANGED;
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); virtual;
    procedure ArrangeControls; virtual;
{$ENDIF}
    procedure InitDpiScaler;
  public
  end;

type
  TIdeDockFormClass = class of TfmIdeDockForm;
type
  IIdeDockManager = interface(IUnknown)
    // Note: IdeDockFormName must be IDE-unique
    procedure RegisterDockableForm(IdeDockFormClass: TIdeDockFormClass;
      var IdeDockFormVar; const IdeDockFormName: string);
    procedure UnRegisterDockableForm(var IdeDockFormVar; const IdeDockFormName: string);
    procedure ShowForm(Form: TForm);
  end;

function IdeDockManager: IIdeDockManager;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

type
  TIdeDockManager = class(TInterfacedObject, IIdeDockManager)
    // Note: IdeDockFormName must be IDE-unique
    procedure RegisterDockableForm(IdeDockFormClass: TIdeDockFormClass;
      var IdeDockFormVar; const IdeDockFormName: string);
    procedure UnRegisterDockableForm(var IdeDockFormVar; const IdeDockFormName: string);
    procedure ShowForm(Form: TForm);
  end;

function IdeDockManager: IIdeDockManager;
begin
  Result := TIdeDockManager.Create;
end;

{ TfmIdeDockForm }

procedure TfmIdeDockForm.InitDpiScaler;
begin
  FScaler := TFormDpiScaler.Create(Self);
{$IFDEF IDE_IS_HIDPI_AWARE}
  ApplyDpi(TScreen_GetDpiForForm(Self), nil);
{$ENDIF}
end;

{$IFDEF IDE_IS_HIDPI_AWARE}
procedure TfmIdeDockForm.WMDpiChanged(var _Msg: TWMDpi);
begin
  inherited;
  ApplyDpi(_Msg.YDpi, _Msg.ScaledRect);
  _Msg.Result := 0;
end;

procedure TfmIdeDockForm.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
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

{ TIdeDockManager }

procedure TIdeDockManager.RegisterDockableForm(IdeDockFormClass: TIdeDockFormClass;
  var IdeDockFormVar; const IdeDockFormName: string);
begin
  // do nothing
end;

procedure TIdeDockManager.ShowForm(Form: TForm);
begin
  // do nothing
end;

procedure TIdeDockManager.UnRegisterDockableForm(var IdeDockFormVar; const IdeDockFormName: string);
begin
  // do nothign
end;

end.

