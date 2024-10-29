unit GX_GExperts;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils, Classes, ToolsAPI, Controls, ExtCtrls,
  GX_EditorExpertManager, GX_Experts;

type
  TGExperts = class(TNotifierObject, IOTANotifier, IOTAWizard)
  private
    FEditorExpertsManager: TGxEditorExpertManager;
    FExpertList: TList;
    FStartingUp: Boolean;
{$IFDEF GX_STARTUP_LAYOUT_FIX_ENABLED}
    FLastDesktopName: string;
    procedure ForceStartupDesktop(Sender: TObject);
{$ENDIF GX_STARTUP_LAYOUT_FIX_ENABLED}
    procedure InstallAddIn;
    function GetExpert(const Index: Integer): TGX_Expert;
    function GetExpertCount: Integer;
    procedure InitializeGExperts;
    procedure OnCloseMessageViewTimer(_Sender: TObject);
  protected
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadEditorExperts;
    procedure FreeEditorExperts;

    function FindExpert(const ExpertName: string; out Idx: Integer): Boolean; overload;
    function FindExpert(const ExpertName: string; out Expert: TGX_Expert): Boolean; overload;
    ///<summary>
    /// Uses FindExpert to locate the expert with the given name and returns its Active property
    /// @returns True if the expert is found and active, False otherwise </summary>
    function IsExpertActive(const ExpertName: string): Boolean;

    property EditorExpertManager: TGxEditorExpertManager read FEditorExpertsManager;
    property ExpertList[const Index: Integer]: TGX_Expert read GetExpert;
    property ExpertCount: Integer read GetExpertCount;
    property StartingUp: Boolean read FStartingUp;
    procedure RefreshExpertShortCuts;
    procedure ShowConfigurationForm;
    procedure DoAfterIDEInitialized(Sender: TObject);

    function GetScaledSharedImages(_Dpi: Integer): TImageList;
    function GetScaledSharedDisabledImages(_Dpi: Integer): TImageList;
    function GetSharedImages: TImageList;
    function GetExpertList: TList;
    procedure TimedCloseMessageView;
  end;

///<summary>
/// @param CheckValid, if true, raises an exception if the instance is NIL </summary>
function GExpertsInst(CheckValid: Boolean = False): TGExperts;
procedure ShowGXAboutForm;
procedure ShowGXConfigurationForm;
procedure InitSharedResources;
procedure FreeSharedResources;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Dialogs, Forms,
  GX_GenericUtils, GX_GetIdeVersion, GX_About, GX_MenuActions, GX_MessageBox,
  GX_ConfigurationInfo, GX_Configure, GX_KbdShortCutBroker, GX_SharedImages,
  GX_IdeUtils, GX_IdeEnhance, GX_EditorChangeServices, GX_ToolbarDropDown,
  GX_TimedCallback, GX_ActionBroker;

type
  TUnsupportedIDEMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
    function ShouldShow: Boolean; override;
  end;

var
  FPrivateGExpertsInst: TGExperts = nil;
  SharedImages: TdmSharedImages = nil;

function GExpertsInst(CheckValid: Boolean): TGExperts;
begin
  if CheckValid and (not Assigned(FPrivateGExpertsInst)) then
    raise Exception.Create('GExpertsInst is not a valid reference');
  Result := FPrivateGExpertsInst;
end;

procedure ShowGXAboutForm;
begin
  gblAboutFormClass.Execute(nil);
end;

procedure ShowGXConfigurationForm;
begin
  TfmConfiguration.Execute(nil);
end;

procedure InitSharedResources;
begin
  if not Assigned(SharedImages) then
    SharedImages := TdmSharedImages.Create(nil);
end;

procedure FreeSharedResources;
begin
  {$IFOPT D+} SendDebug('Freeing shared images'); {$ENDIF}
  FreeAndNil(SharedImages);
end;

{ TGExperts }

constructor TGExperts.Create;
begin
  {$IFOPT D+} SendDebug('TGExperts.Create'); {$ENDIF}
  inherited Create;
  FStartingUp := True;
  InitializeGExperts;

  // No idea where these 1200 ms came from, but basically it means that the
  // method will be called when the application handles events the first time.
  TTimedCallback.Create(DoAfterIDEInitialized, 1200, True);

  gblAboutFormClass.AddToAboutDialog;
{$IFDEF GX_STARTUP_LAYOUT_FIX_ENABLED}
  FLastDesktopName := GetIdeDesktopName;
  {$IFOPT D+} SendDebug('LastDesktopName:' + FLastDesktopName);  {$ENDIF}
{$ENDIF}
end;

procedure TGExperts.InitializeGExperts;
resourcestring
  SInitError = 'Initialization Error:' + sLineBreak;
begin
  FExpertList := TList.Create;

  FPrivateGExpertsInst := Self;
  InitSharedResources;

  GxActionBroker.AddExpertImagesToIde;

  // Create the action manager.
  {$IFOPT D+} SendDebug('Creating GXActionManager'); {$ENDIF}
  CreateGXMenuActionManager;
  try
    {$IFOPT D+} SendDebug('Installing AddIn'); {$ENDIF}
    InstallAddIn;
    {$IFOPT D+} SendDebug('Successfully installed AddIn'); {$ENDIF}
  except
    on E: Exception do
    begin
      GxLogException(E);
      MessageDlg(SInitError + E.Message, mtError, [mbOK], 0);

      { Swallow the exception; at least D5 and BCB5 are allergic to
        exceptions when loading *packages*. Better safe than sorry
        for DLLs. }
    end;
  end;
end;

destructor TGExperts.Destroy;
resourcestring
  SDestructionError = 'GExperts destruction error: ';
var
  i: Integer;
  ExpName : string;
  Expert: TGX_Expert;
  IgnoreDestructionErrors: Boolean;
begin
  try
    {$IFOPT D+} SendDebug('Destroying GExperts'); {$ENDIF}

    gblAboutFormClass.RemoveFromAboutDialog;
    GxKeyboardShortCutBroker.BeginUpdate;
    try
      try
        {$IFOPT D+} SendDebug('Destroying Experts'); {$ENDIF}
        if FExpertList <> nil then
        begin
          IgnoreDestructionErrors := False;
          for i := 0 to FExpertList.Count - 1 do
          begin
            Expert := ExpertList[i];
            if Expert <> nil then begin
              ExpName := Expert.GetName;
              UniqueString(ExpName);
              {$IFOPT D+}SendDebug('Destroying Expert: ' + ExpName); {$ENDIF}
              try
                Expert.Free;
                {$IFOPT D+}SendDebug('done Destroying Expert: ' + ExpName); {$ENDIF}
              except
                on E: Exception do
                begin
                  // Report the exception and continue to destroy the other experts
                  if not IgnoreDestructionErrors then begin
                    if mrIgnore = MessageDlg(Format('GExperts error destroying expert %d - %s: %s', [i, ExpName, E.Message]) + #13#10
                      + 'Press Ignore to ignore any further errors while destroying experts.', mtError, [mbOK, mbIgnore], 0) then
                      IgnoreDestructionErrors := True;
                  end;
                  {$IFOPT D+} SendDebugError(Format('Error destroying expert %d - %s: %s', [i, ExpName, E.Message])); {$ENDIF}
                end;
              end;
            end;
          end;
          {$IFOPT D+} SendDebug('Done freeing experts'); {$ENDIF}
          FreeAndNil(FExpertList);
        end;
      finally
        // Release the editor expert manager and the editor experts
        {$IFOPT D+} SendDebug('Releasing editor expert manager'); {$ENDIF}
        FreeEditorExperts;
        FreeIdeEnhancements;
        ReleaseEditorChangeServices;
        FreeGXToolBarDropDowns;

        // Free the action manager and remove any registered keybindings
        {$IFOPT D+} SendDebug('Freeing Action manager'); {$ENDIF}
        FreeGXMenuActionManager;
        FreeSharedResources;
      end;
    finally
      GxKeyboardShortCutBroker.EndUpdate;
    end;

    FPrivateGExpertsInst := nil;
    inherited Destroy;
    {$IFOPT D+} SendDebug('done Destroying GExperts'); {$ENDIF}
  except
    on E: Exception do
    begin
      {$IFOPT D+} SendDebugError('TGExperts.Destroy Error ' + E.Message); {$ENDIF}
      GxLogAndShowException(E, SDestructionError);
      raise;
    end;
  end;
end;

procedure TGExperts.Execute;
begin //FI:W519
  // Do nothing. We install menu and other items to trigger actions.
end;

function TGExperts.FindExpert(const ExpertName: string; out Idx: Integer): boolean;
var
  i: Integer;
begin
  for i := 0 to ExpertCount - 1 do
  begin
    if SameText(ExpertList[i].GetName, ExpertName) then
    begin
      Idx := i;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TGExperts.FindExpert(const ExpertName: string; out Expert: TGX_Expert): Boolean;
var
  Idx: Integer;
begin
  Result := FindExpert(ExpertName, Idx);
  if Result then
    Expert := ExpertList[Idx];
end;

procedure TGExperts.FreeEditorExperts;
begin
  FreeAndNil(FEditorExpertsManager);
end;

function TGExperts.GetExpert(const Index: Integer): TGX_Expert;
begin
  Result := TGX_Expert(FExpertList.Items[Index]);
end;

function TGExperts.GetExpertCount: Integer;
begin
  Result := FExpertList.Count;
end;

function TGExperts.GetExpertList: TList;
begin
  Result := FExpertList;
end;

function TGExperts.GetIDString: string;
begin
  Result := 'GExperts.GExperts'; // Do not localize.
end;

function TGExperts.GetName: string;
begin
  Result := 'GExperts'; // Do not localize.
end;

function TGExperts.GetSharedImages: TImageList;
begin
  Result := nil;
  if Assigned(SharedImages) then
    Result := SharedImages.Images;
end;

function TGExperts.GetScaledSharedImages(_Dpi: Integer): TImageList;
begin
  Result := nil;
  if Assigned(SharedImages) then
    Result := SharedImages.GetScaledImages(_Dpi);
end;

function TGExperts.GetScaledSharedDisabledImages(_Dpi: Integer): TImageList;
begin
  Result := nil;
  if Assigned(SharedImages) then
    Result := SharedImages.GetScaledDisabledImages(_Dpi);
end;

function TGExperts.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TGExperts.InstallAddIn;
resourcestring
  SExpertCreationFailed = 'Expert "%s" could not be created.' + sLineBreak +
  'Reason: %s';
var
  Expert: TGX_Expert;
  ExpertClass: TGX_ExpertClass;
  i: Integer;
begin
  GxKeyboardShortCutBroker.BeginUpdate;
  try
    for i := 0 to GX_ExpertList.Count - 1 do
    begin
      ExpertClass := GetGX_ExpertClassByIndex(i);
      try
        Expert := ExpertClass.Create;
        FExpertList.Add(Expert);

        Expert.LoadSettings;
      except
        on E: Exception do
        begin
          MessageDlg(Format(SExpertCreationFailed, [ExpertClass.ClassName, E.Message]), mtError, [mbOK], 0);
          // Eat the exception and load other experts (is this safe?)
        end;
      end;
    end;

    if ConfigInfo.EditorExpertsEnabled then
      LoadEditorExperts;

    IdeEnhancements.Initialize;
  finally
    GxKeyboardShortCutBroker.EndUpdate;
  end;

  ShowGxMessageBox(TUnsupportedIDEMessage);
end;

function TGExperts.IsExpertActive(const ExpertName: string): Boolean;
var
  Expert: TGX_Expert;
begin
  Result := FindExpert(ExpertName, Expert);
  Result := Result and Expert.Active;
end;

procedure TGExperts.LoadEditorExperts;
begin
  FEditorExpertsManager := TGxEditorExpertManager.Create;
end;

procedure TGExperts.RefreshExpertShortCuts;
var
  i: Integer;
  Expert: TGX_Expert;
begin
  for i := 0 to GetExpertCount - 1 do
  begin
    Expert := GetExpert(i);
    Expert.ShortCut := Expert.ShortCut; //FI:W503 - Assignment has side effects
  end;
end;

procedure TGExperts.ShowConfigurationForm;
begin
  TfmConfiguration.Execute(nil);
end;

procedure TGExperts.DoAfterIDEInitialized(Sender: TObject);
var
  i: Integer;
begin
  FStartingUp := False;
  for i := 0 to FExpertList.Count - 1 do
    ExpertList[i].AfterIDEInitialized;
  if RunningDelphi8OrGreater then
    GxKeyboardShortCutBroker.DoUpdateKeyBindings(True);

  GXMenuActionManager.MoveMainMenuItems;
{$IFDEF GX_STARTUP_LAYOUT_FIX_ENABLED}
  if ConfigInfo.GetForceDesktopOnStartup then begin
    // Unfortunatly the 1200 ms delay used to call DoAfterIDEInitialized are not always
    // enough to also set the startup desktop, resulting in an access violation.
    // Waiting an additional 500 ms seems to fix the issue.
    TTimedCallback.Create(ForceStartupDesktop, 500, True);
  end;
{$ENDIF}
end;

{$IFDEF GX_STARTUP_LAYOUT_FIX_ENABLED}
procedure TGExperts.ForceStartupDesktop(Sender: TObject);
var
  s: string;
begin
  // see also GX_EditorChangeServices
  s := ConfigInfo.GetForcedStartupDesktop;
  if s = '' then
    s := FLastDesktopName;
  SetIdeDesktop(s);
end;
{$ENDIF}

procedure TGExperts.TimedCloseMessageView;
var
  TheTimer: TTimer;
begin
  TheTimer :=  TTimer.Create(nil);
  TheTimer.OnTimer := OnCloseMessageViewTimer;
end;

procedure TGExperts.OnCloseMessageViewTimer(_Sender: TObject);
var
  Timer: TTimer;
begin
  try
    Timer := _Sender as TTimer;
    Timer.Enabled := False;
    FreeAndNil(Timer);
    TryCloseMessageView;
  except
    on E: Exception do begin
{$IFOPT D+}SendDebugError(E.Message + ' in TGExperts.OnCloseMessageViewTimer');
{$ENDIF}
    end;
  end;
end;

{ TUnsupportedIDEMessage }

function TUnsupportedIDEMessage.GetMessage: string;
resourcestring
  SBadIDEVersion =
    'You are currently using an outdated version of this IDE that has ' +
    'patches or update packs available from Embarcadero.  GExperts might work, but is ' +
    'unsupported running under your IDE.  We recommend you upgrade ' +
    'using the downloads available on the Embarcadero web site: '+
    'http://cc.embarcadero.com/myreg';
begin
  Result := SBadIDEVersion;
end;

function TUnsupportedIDEMessage.ShouldShow: Boolean;
begin
  Result := (GetBorlandIdeVersion in [
      // List IDEs here that have OTA/IDE bugs that bother GExperts
      ideD600, ideD601R, ideD601F,
      ideD800, ideD801,
      ideRS2010, ideRS2010U1 // Keyboard macro streaming broken
    ]);
end;

initialization

end.

