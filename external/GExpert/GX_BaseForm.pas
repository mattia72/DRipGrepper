unit GX_BaseForm;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  // Controls must be listed afer Forms because Forms.THintInfo has been deprecaded in newer
  // Delphi versions, but Controls.THintInfo does not exist in Delphi 6
  Controls,
  Dialogs,
  AppEvnts,
  u_dzDpiScaleUtils,
  u_dzVclUtils,
  GX_HintWindow;

type
  // All forms except docking forms must descend from this class.
  // Changes here must also be made to TfmIdeDockForm, since it must descend
  // from the IDE-internal TDockableForm class.
  TfmBaseForm = class(TForm)
  protected
    procedure DoShow; override;
    procedure Loaded; override;
  protected
    FGxHintCustomDataRec: TGxHintCustomDataRec;
    FScaler: TFormDpiScaler;
    FApplicationEvents: TApplicationEvents;
    procedure HandleApplicationEventsShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure WMDpiChanged(var _Msg: TWMDpi); message WM_DPICHANGED;
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); virtual;
    procedure ArrangeControls; virtual;
  protected
    procedure InitDpiScaler;
  public
    class function Execute(_Owner: TComponent): Boolean; overload; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScaleHint(_HintWindow: TGxHintWindow);
  end;

implementation

{$R *.dfm}

//uses
  //GX_GxUtils;

class function TfmBaseForm.Execute(_Owner: TComponent): Boolean;
var
  frm: TfmBaseForm;
begin
  frm := Self.Create(_Owner);
  try
    Result := (frm.ShowModal = mrOk);
  finally
    frm.Free;
  end;
end;

procedure TfmBaseForm.doShow;
begin
  ApplyDpi(TScreen_GetDpiForForm(Self), nil);
  inherited;
end;

procedure TfmBaseForm.InitDpiScaler;
var
  DpiForForm: Integer;
begin
  FScaler := TFormDpiScaler.Create(Self);
  DpiForForm := TScreen_GetDpiForForm(Self);
  ApplyDpi(DpiForForm, nil);
end;

procedure TfmBaseForm.WMDpiChanged(var _Msg: TWMDpi);
begin
  inherited;
{$IFDEF GX_TWMDPI_HAS_SCALLEDRECT_TYPO}
  ApplyDpi(_Msg.YDpi, _Msg.ScalledRect);
{$ELSE}
  ApplyDpi(_Msg.YDpi, _Msg.ScaledRect);
{$ENDIF}
  _Msg.Result := 0;
end;

procedure TfmBaseForm.HandleApplicationEventsShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
begin
  TGxHintWindow.HandleShowHintEvent(FApplicationEvents, @FGxHintCustomDataRec,
    HintStr, CanShow, HintInfo);
end;

procedure TfmBaseForm.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
begin
  if Assigned(FScaler) then begin
    FScaler.ApplyDpi(_NewDpi, _NewBounds);
  end;
  ArrangeControls;
{$IFDEF GX_IDE_IS_HIDPI_AWARE}
  FCurrentPPI := _NewDpi;
{$ENDIF}
end;

procedure TfmBaseForm.ArrangeControls;
begin
end;

procedure TfmBaseForm.Loaded;
begin
  inherited;
  Scaled := False;
end;

procedure TfmBaseForm.ScaleHint(_HintWindow: TGxHintWindow);
begin
  _HintWindow.Canvas.Font.Size := FScaler.Calc(Screen.HintFont.Size);
end;

constructor TfmBaseForm.Create(AOwner: TComponent);
begin
  inherited;

  FApplicationEvents := TApplicationEvents.Create(Self);
  FApplicationEvents.OnShowHint := HandleApplicationEventsShowHint;

  FGxHintCustomDataRec.ScaleHint := Self.ScaleHint;
  CopyMemory(@(FGxHintCustomDataRec.ID), @GxHintCustomDataId, SizeOf(FGxHintCustomDataRec.ID));
  //GxSetDefaultFont(Self);
end;

destructor TfmBaseForm.Destroy;
begin
  FreeAndNil(FScaler);
  inherited;
end;

end.
