unit GX_LibrarySource;

{$I GX_CondDefine.inc}

{$IFDEF BCB}
  {$OBJEXPORTALL ON}
{$ENDIF BCB}

interface

uses
  Classes,
  ToolsAPI; // Errors here indicate that you didn't link to the DesignIde package

var
  // used to detect duplicate GExperts.dlls loaded into the same IDE instance
  GExpertsDllMarker: TComponent = nil;

// This function needs to be interface-visible, otherwise
// C++Builder 5 complains about a missing EXTDEF symbol
// for this export.  Do not call this function yourself.
function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;

implementation

uses
  Windows, Forms, GX_GxUtils, GX_OtaUtils, GX_VerDepConst,
  GX_DbugIntf,  GX_GExperts, GX_MessageBox, GX_CodeLib, GX_GrepExpert, GX_ExpertManager,
  GX_DummyWizard;

const
  InvalidIndex = -1;

var
  FExpertIndex: Integer = InvalidIndex;

{ Remove the wizard from the IDE. }
procedure FinalizeWizard;
var
  WizardServices: IOTAWizardServices;
begin
  if FExpertIndex <> InvalidIndex then
  begin
    Assert(Assigned(BorlandIDEServices));

    WizardServices := BorlandIDEServices as IOTAWizardServices;
    Assert(Assigned(WizardServices));

    WizardServices.RemoveWizard(FExpertIndex);

    FExpertIndex := InvalidIndex;
  end;
end;

{ Register the wizard. }
function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;
var
  WizardServices: IOTAWizardServices;
  GExpertsDllMarker2: TComponent;
begin
  Result := (BorlandIDEServices <> nil);

  if Result and (not BuiltWithPackages) then
  begin
    Result := False;
    ShowNoPackagesError;
  end;

  if not Result then
    Exit; //==>


  Assert(ToolsAPI.BorlandIDEServices = BorlandIDEServices);

  // Second check that the GExperts DLL is not loaded twice in the same IDE instance.
  // The first check in CreateInstanceMutexes only works if two different GExperts DLLs are
  // loaded into the IDE. If the same one is loaded twice, the initialization section
  // (and thus CreateInstanceMutexes) is only called once.
  // So we now do the same GExpertsDLLMarker trick again, with a different name this time
  if Application.FindComponent('GExpertsDllMarker2') <> nil then begin
      // OK, it is indeed the second (or third ...) call to InitWizard in the same DLL
      // simply return true and exit.
      // todo: Maybe we should display an error message?
    Result := True;
    Exit; //==>
  end;

  GExpertsDllMarker2 := TComponent.Create(Application);
  GExpertsDllMarker2.Name := 'GExpertsDllMarker2';

  Terminate := FinalizeWizard;

  WizardServices := BorlandIDEServices as IOTAWizardServices;
  Assert(Assigned(WizardServices));

  if (GExpertsDllMarker <> nil) then begin
    FExpertIndex := WizardServices.AddWizard(TGExperts.Create as IOTAWizard);
    {$IFOPT D+} SendDebugFmt('Expert added with index %d', [FExpertIndex]); {$ENDIF}
  end else begin
    // register a dummy wizard so we can fail gracefully
    FExpertIndex := WizardServices.AddWizard(TDummyWizard.Create as IOTAWizard);
    {$IFOPT D+} SendDebugFmt('GExperts is already active, added dummy expert with index %d', [FExpertIndex]); {$ENDIF}
  end;

  // some code on the Internet checks for <> 0 here, but this seems to be wrong.
  // I have definitely seen AddWizard return 0 and the Expert was active.
  // Unfortunately there seems to be no official documentation on the return code's value.
  Result := (FExpertIndex >= 0);
end;

exports
{$IFDEF GX_BCB}
  InitWizard name WizardEntryPoint;
{$ELSE}
  InitWizard name WizardEntryPoint,
  ShowExpertManager,
  InstallGExperts,
  RemoveGExperts,
  ShowCodeLib,
  ShowGrep,
  ShowGrepEx;
{$ENDIF GX_BCB}

// ---------------------------------------------

type
  TGxMultipleInstancesMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

{ TGxMultipleInstancesMessage }

function TGxMultipleInstancesMessage.GetMessage: string;
resourcestring
  SMultipleGExpertsInstances =
    'Multiple instances of this version of GExperts are currently running.  ' +
    'This is usually caused by multiple instances of the same IDE running at once (check in the Task Manager if you don''t see them all).  ' +
    'Please be aware that the last IDE instance to save any settings ' +
    'will overwrite any previously saved settings from other instances.';
begin
  Result := SMultipleGExpertsInstances;
end;

var
  GXGeneralMutex: THandle;
  GXVersionMutex: THandle;

procedure CreateInstanceMutexes;
begin
  // First, check that the GExperts DLL is not loaded twice in the same IDE instance.
  // This would play havoc with many of the functions.
  // Unfortunately this only works if two different DLLs are loaded into the IDE.
  // If the same one is loaded twice, the initialization (and thus CreateInstanceMutexes)
  // is only called once. So we need another way of preventing this.
  // But nothing is preventing us from doing the GExpertsDLLMarker trick twice, with
  // a different name in InitWizard above.
  if Application.FindComponent('GExpertsDllMarker') <> nil then begin
    // Another instance of GExperts has already been loaded.
    // -> Fail silently and do not create an instance of TGExperts in InitWizard.
    Exit;
  end;

  GExpertsDllMarker := TComponent.Create(Application);
  GExpertsDllMarker.Name := 'GExpertsDllMarker';

  // This mutex signals that at least one copy of GExperts is running
  // The installer uses this to determine if it should allow installation
  GXGeneralMutex := CreateMutex(nil, False, 'GExperts.Addin.For.Borland.IDEs');

  GXVersionMutex := CreateMutex(nil, False, PChar('GExperts.Addin.For.Borland.IDEs.Under.' +
    GxOtaGetIDEProductIdentifier + MajorVersionNumberChar));
  if (GXVersionMutex <> 0) and (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    // The mutex already exists, so there is a good chance that a copy
    // of this version of GExperts is already loaded into memory.
    ShowGxMessageBox(TGxMultipleInstancesMessage);
  end;
end;

procedure DestroyInstanceMutexes;
begin
  if GXGeneralMutex <> 0 then
    CloseHandle(GXGeneralMutex);
  if GXVersionMutex <> 0 then
    CloseHandle(GXVersionMutex);
end;

initialization
  CreateInstanceMutexes;

finalization
  DestroyInstanceMutexes;

end.
