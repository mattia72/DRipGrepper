unit GX_HintWindow;

interface

{$I GX_CondDefine.inc}

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  Forms,
  Controls,
  AppEvnts,
  u_dzTypes;

{$IF not declared(TCustomData)}
type
  TCustomData = Pointer;
{$IFEND}

const
  GxHintCustomDataId: array[0..7] of AnsiChar = 'GXSCHINT';

type
  TGxHintWindow = class;

  TScaleHintMethod = procedure(_HintWindow: TGxHintWindow) of object;
  PGxHintCustomDataRec = ^TGxHintCustomDataRec;
  TGxHintCustomDataRec = record
    ID: array[0..7] of AnsiChar;
    ScaleHint: TScaleHintMethod;
  end;

  TGxHintWindow = class(THintWindow)
  private
  public
    class procedure HandleShowHintEvent(_ApplicationEvents: TApplicationEvents; _HintData: TCustomData;
      var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    constructor Create(AOwner: TComponent); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: TCustomData): TRect; override;
    procedure ActivateHintData(Rect: TRect; const AHint: string; AData: TCustomData); override;
  end;

implementation

uses
  GX_BaseForm;

{ TGxHintWindow }

class procedure TGxHintWindow.HandleShowHintEvent(_ApplicationEvents: TApplicationEvents; _HintData: TCustomData;
  var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
var
  ctrl: TControl;
  frm: TComponent;
begin
  // unfortunately this is called for every single hint shown inside the IDE, not just the ones
  // for controls on this form, so we need to check whether we want to handle it.

  if HintInfo.HintWindowClass = TGxHintWindow then begin
    // we don't want to interfere with other hints
    HintInfo.HintWindowClass := nil;
  end;

  ctrl := HintInfo.HintControl;
  if not Assigned(ctrl) then
    Exit; //==>

  frm := GetParentForm(ctrl);
  if not (frm = _ApplicationEvents.Owner) then
    Exit; //==>

  // OK, it is a a control on the calling form
  HintInfo.HintData := _HintData;
  HintInfo.HintWindowClass := TGxHintWindow;

  _ApplicationEvents.CancelDispatch;
end;

constructor TGxHintWindow.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TGxHintWindow.ActivateHintData(Rect: TRect; const AHint: string; AData: TCustomData);
var
  Data: PGxHintCustomDataRec;
begin
  if Assigned(AData) then begin
    Data := AData;
    if CompareMem(@(Data^.ID), @GxHintCustomDataId, SizeOf(Data^.ID)) then begin
      if Assigned(Data^.ScaleHint) then
        Data^.ScaleHint(Self);
    end;
  end;
  inherited;
end;

function TGxHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: TCustomData): TRect;
begin
  Result := Types.Rect(0, 0, MaxWidth, 0);
  DrawText(Canvas.Handle, PChar(AHint), -1, Result, DT_CALCRECT or DT_LEFT or
    DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);
  Inc(Result.Right, 6);
  Inc(Result.Bottom, 2);
end;

end.

