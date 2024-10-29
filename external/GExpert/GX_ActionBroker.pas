unit GX_ActionBroker;

{ All editor toolbar buttons will get their actions from this broker.
  This broker also provides transparent access to the Open
  Tools API's ActionList.

  Note: Except for adding a method or two to facilitate notification of
        dynamic menu item enabling and disabling, these interfaces
        should be stable and cover all use cases. }

interface

{$I GX_CondDefine.inc}

uses
  Windows, SysUtils, Classes, ActnList, Graphics, Actions,
  GX_Actions;

type
  IGxActionBroker = interface(IUnknown)
    ['{EC7632E1-D1A3-11D3-A944-DA3A65000000}']
    ///<summary>
    /// Access to all actions available to the broker; this
    /// includes GExperts's own actions as well as the IDE's
    /// actions. Do not hold on to an action instance for
    /// any prolonged period of time. </summary>
    function GetActions(Index: Integer): TContainedAction;
    function GetActionCount: Integer;
    ///<summary>
    /// Finds an action in the list of available actions
    /// Includes all actions registered / requested via
    /// GExperts and the actions contained in the IDE's
    /// action list.
    /// @param ActionName is the name of the action to find, including(!) its category
    /// @Note that by finding an IDE action and querying for its category name,
    ///       it is possible to group a GExperts action into the same category as a
    ///       given IDE action. </summary>
    function FindAction(const ActionName: string): TContainedAction;
    ///<summary>
    /// Finds a GExperts tools action. This is short for calling
    /// FindAction(GenerateActionName(ActionName)) </summary>
    function FindGExpertsAction(const ActionName: string): TContainedAction;
    ///<summary>
    /// Request and register a newly created action interface
    /// instance that represents an activity that does not live in
    /// the GExperts menu.
    /// After requesting, in particular Caption and OnExecute
    /// need to be set to make the Action "useful" </summary>
    function RequestAction(const ActionName: string; Bitmap: Graphics.TBitmap = nil): IGxAction;
    ///<summary>
    /// Request and register an action interface instance
    /// that represents an activity that lives in the GExperts menu.
    /// After requesting, in particular Caption and OnExecute
    /// need to be set to make the Action "useful" </summary>
    function RequestMenuAction(const ActionName: string; Bitmap: Graphics.TBitmap): IGxMenuAction;
    ///<summary>
    /// Fill Categories with the all of the action categories </summary>
    procedure GetCategories(Categories: TStrings);
    ///<summary>
    /// Generates the name for a menu action as set by RequestAction
    /// (by prefixing GExpertsActionCategory ('GExperts') </summary>
    function GenerateActionName(const AActionName: string): string;
    ///<summary>
    /// Goes through the list of experts and editor experts and adds all their icons to the
    /// IDEs image list. Must only be called once on startup.
    /// This a workaround for a bug in the Delphi 12 INTAServices.AddMasked OTAPI.
    /// It does nothing if GX_ACTION_BROKER_IMAGELIST_FIX_ENABLED is not defined. </summary>
    procedure AddExpertImagesToIde;
  end;


// Get an instance to the GExperts action broker.
function GxActionBroker: IGxActionBroker;

resourcestring
  SNoButtonCategory = '(None)';
  SAllButtonsCategory = '(All)';

implementation

uses
  Forms, Menus, Dialogs,
  ToolsAPI,
  Rescaler,
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  GX_TimedCallback, GX_GxUtils, GX_IdeUtils, GX_OtaUtils, GX_KbdShortCutBroker,
  GX_GenericClasses, GX_GenericUtils, Controls, GX_Experts,
  GX_EditorExpert, GX_BaseExpert;

type
  // Special action that implements menu actions.
  TGxMenuAction = class(TGxCustomAction, IUnknown, IGxAction, IGxMenuAction)
  private
    FAssociatedMenuItem: TMenuItem;
{$IFDEF GX_KEYBOARD_BINDING_FIX}
    FTimedValue: TShortCut;
    FTimedSet: TTimedCallback;
    procedure HandleDelayedSetShortcut(Sender: TObject);
{$ENDIF}
    procedure doSetShortcut(Value: TShortCut);
  protected
    procedure SetShortCut(Value: TShortCut); {$IFDEF GX_VER240_up} override; {$ENDIF}
    function GetAssociatedMenuItem: TMenuItem;
  public
    constructor Create(_Owner: TComponent; const _Name: string); reintroduce;
    destructor Destroy; override;
  end;

type
  TGxToolsAction = class(TGxCustomAction, IUnknown, IGxAction)
  private
{$IFDEF GX_KEYBOARD_BINDING_FIX}
    FTimedValue: TShortCut;
    FTimedSet: TTimedCallback;
    procedure HandleDelayedSetShortcut(Sender: TObject);
{$ENDIF}
    procedure doSetShortcut(Value: TShortCut);
  protected
    procedure SetShortCut(Value: TShortCut); {$IFDEF GX_VER240_up} override; {$ENDIF}
    procedure doOnExecute(Sender: TObject);
  public
    destructor Destroy; override;
  end;

type
  TGxActionBroker = class(TSingletonInterfacedObject, IGxActionBroker)
  private
{$IFDEF GX_ACTION_BROKER_IMAGELIST_FIX_ENABLED}
    FBitmapNames: TStringList;
    FImageIndexBase: Integer;
{$ENDIF GX_ACTION_BROKER_IMAGELIST_FIX_ENABLED}
    procedure AddExpertImagesToIde;
    function GetIdeActionList: TCustomActionList;
    function GetActionOwner: TCustomForm;
    procedure RegisterActionWithIde(const AAction: TCustomAction; Bitmap: Graphics.TBitmap);
  protected
    // IGxActionBroker
    function GetActions(Index: Integer): TContainedAction;
    function GetActionCount: Integer;
    function FindAction(const Name: string): TContainedAction;
    function FindGExpertsAction(const ActionName: string): TContainedAction;
    function RequestAction(const ActionName: string; Bitmap: Graphics.TBitmap = nil): IGxAction;
    function RequestMenuAction(const ActionName: string; Bitmap: Graphics.TBitmap): IGxMenuAction;
    procedure GetCategories(Categories: TStrings);
    function GenerateActionName(const AActionName: string): string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  PrivateGxActionBroker: TGxActionBroker;

function GxActionBroker: IGxActionBroker;
begin
  if not Assigned(PrivateGxActionBroker) then
    PrivateGxActionBroker := TGxActionBroker.Create;

  Result := PrivateGxActionBroker;
end;

procedure FreeGxActionBroker;
begin
  FreeAndNil(PrivateGxActionBroker);
end;

{ TGxActionBroker }

constructor TGxActionBroker.Create;
begin
  inherited Create;
{$IFDEF GX_ACTION_BROKER_IMAGELIST_FIX_ENABLED}
  FBitmapNames := TStringList.Create;
{$ENDIF}
end;

destructor TGxActionBroker.Destroy;
begin
{$IFDEF GX_ACTION_BROKER_IMAGELIST_FIX_ENABLED}
  FreeAndNil(FBitmapNames);
{$ENDIF}
  inherited Destroy;
end;

function TGxActionBroker.FindAction(const Name: string): TContainedAction;
var
  TempAction: TContainedAction;
  ActionList: TCustomActionList;
  i: Integer;
begin
  Result := nil;

  ActionList := GetIdeActionList;
  Assert(Assigned(ActionList));

  for i := 0 to ActionList.ActionCount-1 do
  begin
    TempAction := ActionList.Actions[i];
    Assert(Assigned(TempAction));

    if SameText(TempAction.Name, Name) then
    begin
      Result := TempAction;
      Break;
    end;
  end;
end;

function TGxActionBroker.FindGExpertsAction(const ActionName: string): TContainedAction;
begin
  Result := FindAction(GenerateActionName(ActionName));
end;

function TGxActionBroker.GenerateActionName(const AActionName: string): string;
begin
  Result := GExpertsActionCategory + AActionName;
end;

function TGxActionBroker.GetActionCount: Integer;
begin
  Result := GetIdeActionList.ActionCount;
end;

function TGxActionBroker.GetActionOwner: TCustomForm;
begin
  Result := GetIdeMainForm;
end;

function TGxActionBroker.GetActions(Index: Integer): TContainedAction;
begin
  Result := GetIdeActionList.Actions[Index];
end;

function TGxActionBroker.GetIdeActionList: TCustomActionList;
var
  NTAServices: INTAServices;
begin
  Assert(Assigned(BorlandIDEServices));
  NTAServices := BorlandIDEServices as INTAServices;

  Assert(Assigned(NTAServices));
  Result := NTAServices.ActionList;

  Assert(Assigned(Result));
end;

function CreateScaledBitmap(Bitmap: Graphics.TBitmap): Graphics.TBitmap;
const
  RequiredWidth = 16;
  RequiredHeight = 16;
var
  R: TRect;
  TempBitmap: Graphics.TBitmap;
  w, h: Integer;
begin
  w := Bitmap.Width;
  h := Bitmap.Height;
  Result := Graphics.TBitmap.Create;
  if (w = RequiredWidth) and (h = RequiredHeight) then begin
    Result.Assign(Bitmap);
    Exit; //==>
  end;

  // TempBitmap stores a copy of the bitmap but with a transparent color
  // of clBtnFace.  This prevents the rescaling of the image edges
  // from being discolored by the (usually odd) transparent color.
  TempBitmap := Graphics.TBitmap.Create;
  try
    TempBitmap.Height := h;
    TempBitmap.Width := w;
    TempBitmap.Canvas.Brush.Color := clBtnFace;
    TempBitmap.Transparent := True;
    R := Rect(0, 0, w + 1, h + 1);
    TempBitmap.Canvas.FillRect(R);
    Bitmap.Transparent := True;
    TempBitmap.Canvas.Draw(0, 0, Bitmap);
    Result.Width := RequiredWidth;
    Result.Height := RequiredHeight;
    Result.Transparent := True;
    if not Rescaler.Rescale(TempBitmap, Result, False) then
      Result.Canvas.StretchDraw(Rect(0, 0, Result.Width, Result.Height), TempBitmap);
  finally
    FreeAndNil(TempBitmap);
  end;
end;

procedure ScaleBitmap(var _bmp: Graphics.TBitmap);
var
  TmpBmp: Graphics.TBitmap;
begin
  TmpBmp := _bmp;
  _bmp := CreateScaledBitmap(TmpBmp);
  TmpBmp.Free;
end;

const
  GxBitmapSuffix = 'GxImage'; // Do not localize.

procedure TGxActionBroker.AddExpertImagesToIde;
{$IFNDEF GX_ACTION_BROKER_IMAGELIST_FIX_ENABLED}
begin
  // do nothing
end;
{$ELSE}

  procedure HandleBitmap(_il: TImageList; var _bmp: TBitmap; _ActionName: string);
  var
    BitmapName: string;
  begin
    if Assigned(_bmp) then begin
      BitmapName := GenerateActionName(_ActionName) + GxBitmapSuffix;
      ScaleBitmap(_bmp);
{$IFDEF GX_VER170_up}
        // Prevent invisible enabled icons in XE6 (disabled ones might still be invisible/ghosted)
      _bmp.Transparent := False;
{$ENDIF}
      if _bmp.Transparent then
        _il.AddMasked(_bmp, _bmp.TransparentColor)
      else
        _il.AddMasked(_bmp, GXTransparencyColor);
      FBitmapNames.Add(BitmapName);
    end;
  end;

  procedure HandleExpert(_il: Timagelist; ExpertClass: TGX_BaseExpertClass);
  var
    bmp: Graphics.TBitmap;
  begin
    bmp := ExpertClass.LoadBitmap;
    try
      HandleBitmap(_il, bmp, ExpertClass.GetActionName);
    finally
      FreeAndNil(bmp);
    end;
  end;

  procedure HandleAdditinoalBitmap(_il: TImageList; const _ActionName: string);
  var
    bmp: Graphics.TBitmap;
  begin
    bmp := TBitmap.Create;
    if GxLoadBitmapForExpert(_ActionName, bmp) then begin
      try
        HandleBitmap(_il, bmp, _ActionName);
      finally
        FreeAndNil(bmp);
      end;
    end;
  end;

var
  NTAServices: INTAServices;
  il: TImageList;
  i: Integer;
  ExpertClass: TGX_BaseExpertClass;
begin
  Assert(FImageIndexBase = 0);

  il := TImageList.Create(nil);
  try
    for i := 0 to GX_ExpertList.Count - 1 do begin
      ExpertClass := GetGX_ExpertClassByIndex(i);
      HandleExpert(il, ExpertClass);
    end;
    for i := 0 to EditorExpertClassList.Count - 1 do begin
      ExpertClass := GetEditorExpertClassByIndex(i);
      HandleExpert(il, ExpertClass);
    end;

    // these are actions only available for a toolbar
    HandleAdditinoalBitmap(il, 'UnitDropDown');
    HandleAdditinoalBitmap(il, 'FormDropDown');
    HandleAdditinoalBitmap(il, 'ComponentDropDown');
    HandleAdditinoalBitmap(il, 'UsesDropDown');
    HandleAdditinoalBitmap(il, 'PositionDropDown');

    NTAServices := BorlandIDEServices as INTAServices;
    Assert(Assigned(NTAServices));
    FImageIndexBase := NTAServices.AddImages(il, 'GExperts');
  finally
    FreeAndNil(il);
  end;
end;
{$ENDIF GX_ACTION_BROKER_IMAGELIST_FIX_ENABLED}

procedure TGxActionBroker.RegisterActionWithIde(const AAction: TCustomAction; Bitmap: Graphics.TBitmap);
var
  NTAServices: INTAServices;
  ReadyBitmap: Graphics.TBitmap;
  BitmapName: string;
  Idx: Integer;
begin
  if IsStandAlone then
    Exit;

  Assert(Assigned(AAction));
  AAction.ActionList := GetIdeActionList;

  if Assigned(Bitmap) then begin
    BitmapName := AAction.Name + GxBitmapSuffix;
{$IFDEF GX_ACTION_BROKER_IMAGELIST_FIX_ENABLED}
    Idx := FBitmapNames.IndexOf(BitmapName);
    if Idx <> -1 then begin
      AAction.ImageIndex := FImageIndexBase + Idx;
      Exit; //==>
    end;

{$IFOPT D+}
    SendDebugFmtEx('TGxActionBroker.RegisterActionWithIde call which would have call INTAServices.AddMasked for action %s (%s)',
      [AAction.Name, BitmapName], mtError);
{$ENDIF}
    Exit;
{$ENDIF GX_ACTION_BROKER_IMAGELIST_FIX_ENABLED}

    NTAServices := BorlandIDEServices as INTAServices;
    Assert(Assigned(NTAServices));

    ReadyBitmap := CreateScaledBitmap(Bitmap);
    try
      {$IFDEF GX_VER170_up}
      ReadyBitmap.Transparent := False; // Prevent invisible enabled icons in XE6 (disabled ones might still be invisible/ghosted)
      {$ENDIF}
      if ReadyBitmap.Transparent then
        Idx := NTAServices.AddMasked(ReadyBitmap, ReadyBitmap.TransparentColor, BitmapName)
      else
        Idx := NTAServices.AddMasked(ReadyBitmap, GXTransparencyColor, BitmapName);
      AAction.ImageIndex := Idx;
    finally
      FreeAndNil(ReadyBitmap);
    end;
  end;
end;

function TGxActionBroker.RequestAction(const ActionName: string; Bitmap: Graphics.TBitmap = nil): IGxAction;
var
  Action: TGxToolsAction;
begin
  Assert(IsValidIdent(ActionName));

  Action := TGxToolsAction.Create(GetActionOwner, GenerateActionName(ActionName));

  RegisterActionWithIde(Action, Bitmap);

  Result := Action;
end;

function TGxActionBroker.RequestMenuAction(const ActionName: string; Bitmap: Graphics.TBitmap): IGxMenuAction;
var
  Action: TGxMenuAction;
begin
  Assert(IsValidIdent(ActionName));

  Action := TGxMenuAction.Create(GetActionOwner, GenerateActionName(ActionName));

  RegisterActionWithIde(Action, Bitmap);

  Result := Action;
end;

procedure TGxActionBroker.GetCategories(Categories: TStrings);
var
  i: Integer;
  Category: string;
begin
  Assert(Assigned(Categories));
  Categories.Clear;
  for i := 0 to GxActionBroker.GetActionCount - 1 do
  begin
    Category := GxActionBroker.GetActions(i).Category;
    if Trim(Category) = '' then
      Category := SNoButtonCategory;
    EnsureStringInList(Categories, Category);
  end;
end;

{ TGxMenuAction }

constructor TGxMenuAction.Create(_Owner: TComponent; const _Name: string);
const
  MenuItemAppendix = '_MenuItem'; // Do not localize.
begin
  inherited Create(_Owner, _Name);

  FAssociatedMenuItem := TMenuItem.Create(Self);
  FAssociatedMenuItem.Name := _Name + MenuItemAppendix;
  FAssociatedMenuItem.Action := Self;
end;

destructor TGxMenuAction.Destroy;
begin
{$IFDEF GX_KEYBOARD_BINDING_FIX}
  FreeAndNil(FTimedSet);
{$ENDIF GX_KEYBOARD_BINDING_FIX}
  inherited;
end;

function TGxMenuAction.GetAssociatedMenuItem: TMenuItem;
begin
  Result := FAssociatedMenuItem;
end;

procedure TGxMenuAction.SetShortCut(Value: TShortCut);
begin
{(*)}{$IFOPT D+} SendDebugWarning(ClassName + '.SetShortCut'); {$ENDIF}{*)}
  // RAD Studio 12.1: IOTAKeyboardBinding.BindKeyboard is received when another 'ToolsApi' package is re-build (Unload/Install).
  // This case is not handled in GX_KbdShortCutBroker.pas and 'GExperts' disappear from 'Tools/Editor/Key Mappings'.
  // Then Action ShortCut is first cleared (nil) then re-assigned - the check below fails: if (ShortCut <> Value) ...
  // Fix by using timed callback for updating the ShortCut.
{$IFDEF GX_KEYBOARD_BINDING_FIX}
  FreeAndNil(FTimedSet);
  FTimedValue := Value;
  if Value = 0 then
    FTimedSet := TTimedCallback.Create(HandleDelayedSetShortcut, 50, True)
  else
{$ENDIF GX_KEYBOARD_BINDING_FIX}
    doSetShortcut(Value);
end;

{$IFDEF GX_KEYBOARD_BINDING_FIX}
procedure TGxMenuAction.HandleDelayedSetShortcut(Sender: TObject);
var
  Value: TShortCut;
begin
{(*)}{$IFOPT D+} SendDebugWarning(ClassName + '.HandleDelayedSetShortcut'); {$ENDIF}{*)}

  Value := FTimedValue;
  FTimedSet := nil;

  if Assigned(IdeShortCut) and (IdeShortCut.ShortCut <> Value) then begin
{$IFOPT D+}
    if not Assigned(Sender) and (Value = 0) then
      SendDebugWarning(ClassName + ': not timed SetShortCut(0)!');
{$ENDIF}
    doSetShortcut(Value);
  end;
end;
{$ENDIF GX_KEYBOARD_BINDING_FIX}

procedure TGxMenuAction.doSetShortcut(Value: TShortCut);
begin
{(*)}{$IFOPT D+} SendDebugWarning(ClassName + '.doSetShortcut'); {$ENDIF}{*)}

  IdeShortCut := nil; // Unregisters the shortcut with the IDE

  if Value <> 0 then begin
    if Assigned(FAssociatedMenuItem) and not Assigned(IdeShortCut) then
    begin
      IdeShortCut := GxKeyboardShortCutBroker.RequestMenuShortCut(OnExecute, FAssociatedMenuItem);

      Assert(Assigned(IdeShortCut));
      IdeShortCut.ShortCut := Value;
    end;
  end;

  inherited SetShortCut(Value);
end;

{ TGxToolsAction }

destructor TGxToolsAction.Destroy;
begin
{$IFDEF GX_KEYBOARD_BINDING_FIX}
  FreeAndNil(FTimedSet);
{$ENDIF GX_KEYBOARD_BINDING_FIX}
  inherited;
end;

procedure TGxToolsAction.doOnExecute(Sender: TObject);
begin
  if Assigned(OnExecute) then
    OnExecute(Sender);
end;

procedure TGxToolsAction.SetShortCut(Value: TShortCut);
begin
{(*)}{$IFOPT D+} SendDebugWarning(ClassName + '.SetShortCut'); {$ENDIF}{*)}
  // RAD Studio 12.1: IOTAKeyboardBinding.BindKeyboard is received when another 'ToolsApi' package is re-build (Unload/Install).
  // This case is not handled in GX_KbdShortCutBroker.pas and 'GExperts' disappear from 'Tools/Editor/Key Mappings'.
  // Then Action ShortCut is first cleared (nil) then re-assigned - the check below fails: if (ShortCut <> Value) ...
  // Fix by using timed callback for updating the ShortCut.
{$IFDEF GX_KEYBOARD_BINDING_FIX}
  FreeAndNil(FTimedSet);
  FTimedValue := Value;
  if Value = 0 then
    FTimedSet := TTimedCallback.Create(HandleDelayedSetShortcut, 50, True)
  else
{$ENDIF GX_KEYBOARD_BINDING_FIX}
  doSetShortcut(Value);
end;

{$IFDEF GX_KEYBOARD_BINDING_FIX}
procedure TGxToolsAction.HandleDelayedSetShortcut(Sender: TObject);
var
  Value: TShortCut;
begin
{(*)}{$IFOPT D+} SendDebugWarning(ClassName + '.HandleDelayedSetShortcut'); {$ENDIF}{*)}
  Value := FTimedValue;
  FTimedSet := nil;
  doSetShortcut(Value);
end;
{$ENDIF GX_KEYBOARD_BINDING_FIX}

procedure TGxToolsAction.doSetShortcut(Value: TShortCut);
begin
{(*)}{$IFOPT D+} SendDebugWarning(ClassName + '.doSetShortcut'); {$ENDIF}{*)}
  // Not necessary under Delphi 5/6 since the callbacks never happen anyway
  if RunningDelphi7OrGreater then
  begin
    if Assigned(IdeShortCut) and (IdeShortCut.ShortCut <> Value) then begin
      IdeShortCut := nil; // Unregisters the shortcut with the IDE
    end;

    if Value <> 0 then begin
      if not Assigned(IdeShortCut) then
      begin
        {$IFOPT D+} if not Assigned(OnExecute) then
          SendDebugError(Self.ClassName + '(' + Self.Caption + ') tried to register a shortcut but OnExecute was not assigned.'); {$ENDIF}
        IdeShortCut := GxKeyboardShortCutBroker.RequestOneKeyShortCut(doOnExecute, Value);

        Assert(Assigned(IdeShortCut));
      end;
    end;
  end;

  inherited SetShortCut(Value);
end;

initialization

finalization

  FreeGxActionBroker;

end.

