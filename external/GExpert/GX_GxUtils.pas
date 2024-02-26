unit GX_GxUtils;

{ This unit encapsulates a few utility routines that are
  specific to GExperts.

  Other units, such as GX_GenericUtils and GX_IdeUtils,
  contain routines that do not make use of any GExperts-
  specific functionality.
}

interface

{$I GX_CondDefine.inc}

uses
  Graphics,
  {$IFNDEF GX_VER170_up}GXHtmlHelp,{$ENDIF}
  Controls;

// Returns True if the binary this function is called from
// has been linked against various packages; at this time,
// these packages are VCL, VCLX, DESIGNIDE, etc.
function BuiltWithPackages: Boolean;
// Show a message box notifying the user that an application
// has not been built with packages.
procedure ShowNoPackagesError;

// Load bitmap with name FileName from disk; returns True
// if loading was successful ; False otherwise with TargetBitmap
// being undefined.
function GxLoadBitmapForExpert(const BitmapFile: string; var TargetBitmap: Graphics.TBitmap): Boolean;
function GxLoadBitmapFromResource(const ResName: string; var TargetBitmap: Graphics.TBitmap): Boolean;
function GxLoadBitmapFromFile(FileName: string; var TargetBitmap: Graphics.TBitmap): Boolean;

procedure GxContextHelp(const HelpOwner: TWinControl; const ContextID: Integer);
procedure GxContextHelpContents(const HelpOwner: TWinControl);

procedure GxSetDefaultFont(Control: TControl);

// Determine what runtime features are available to be exposed or disabled
// These are dependent on the IDE version, etc.
function ComponentPaletteAvailable: Boolean;
function MultilineTabDockHostPossible: Boolean;
function EditorEnhancementsPossible: Boolean;

const
  GXTransparencyColor = $00FF00FF; // Defines the background of some bitmaps

implementation

uses
  Windows, SysUtils, Forms,
  DdeMan, // Needed to test for linking against VCLX package.
  GX_GenericUtils, GX_ConfigurationInfo, GX_IdeUtils;

function BuiltWithPackages: Boolean;
var
  IsNotInInstance: Boolean;
begin
  Result := True;

  IsNotInInstance := (LongWord(FindClassHInstance(TForm)) <> HINSTANCE); // VCL and VisualClx
  Result := Result and IsNotInInstance;

  IsNotInInstance := (LongWord(FindClassHInstance(TDdeServerConv)) <> HINSTANCE); // VCLX
  Result := Result and IsNotInInstance;
end;

procedure ShowNoPackagesError;
const
  // Text description of packages with which GExperts must be compiled to work
  RequiredPackageText = '  vcl, vclx, and designide';
resourcestring
  SGxBuildError = 'DRipExtensions build error';
  SNotBuiltWithRequiredPackages =
 'DRipExtensions has not been built with the required runtime packages ' + sLineBreak +
 sLineBreak +
 RequiredPackageText + sLineBreak +
 sLineBreak +
 'Please add these packages to the list of used runtime packages, '+
 'check the "Build with runtime packages" box, and rebuild GExperts.' + sLineBreak +
 sLineBreak +
 'DRipExtensions will not be installed into the IDE until this has been done.';
begin
  Windows.MessageBox(0, PChar(SNotBuiltWithRequiredPackages),
    PChar(SGxBuildError), MB_OK or MB_ICONERROR);
end;

function GxLoadBitmapForExpert(const BitmapFile: string; var TargetBitmap: Graphics.TBitmap): Boolean;
begin
  Result := GxLoadBitmapFromFile(BitmapFile, TargetBitmap);
  if not Result then
    Result := GxLoadBitmapFromResource(BitmapFile, TargetBitmap);
  if not Result then
    FreeAndNil(TargetBitmap);
end;

function GxLoadBitmapFromResource(const ResName: string; var TargetBitmap: Graphics.TBitmap): Boolean;
begin
  Result := False;
  try
    FreeAndNil(TargetBitmap);
    TargetBitmap := Graphics.TBitmap.Create;
    TargetBitmap.LoadFromResourceName(HInstance, ResName);
    Result := True;
  except
    // Swallow exceptions and report the failure
  end;  //FI:W501 Empty except block
end;

function GxLoadBitmapFromFile(FileName: string; var TargetBitmap: Graphics.TBitmap): Boolean;
begin
  Result := False;
  try
    FileName := AddSlash(ConfigInfo.GExpertsPath) +
                AddSlash('Icons') + FileName;

    if ExtractFileExt(FileName) = '' then
      FileName := ChangeFileExt(FileName, '.bmp');

    if FileExists(FileName) then
    begin
      FreeAndNil(TargetBitmap);

      TargetBitmap := Graphics.TBitmap.Create;
      TargetBitmap.LoadFromFile(FileName);

      Result := True;
    end;
  except
    // Swallow exceptions and report the failure
  end;  //FI:W501 Empty except block
end;

procedure CallWinHelp(const Command, ContextID: Integer; const HelpOwner: TWinControl);
begin
  if FileExists(ConfigInfo.HelpFile) then
    // The 0 allows the help to drop behind the IDE
    HtmlHelp(0, PChar(ConfigInfo.HelpFile), Command, ContextID)
  else
    raise Exception.Create('The configured help file is missing: ' + ConfigInfo.HelpFile);
end;

procedure GxContextHelpContents(const HelpOwner: TWinControl);
begin
  CallWinHelp(HH_DISPLAY_INDEX, 0, HelpOwner);
end;

procedure GxContextHelp(const HelpOwner: TWinControl; const ContextID: Integer);
begin
  CallWinHelp(HH_HELP_CONTEXT, ContextID, HelpOwner);
end;

function ComponentPaletteAvailable: Boolean;
begin
  Result := not RunningDelphi8OrGreater;
end;

function MultilineTabDockHostPossible: Boolean;
begin
  Result := not RunningDelphi8OrGreater;
end;

function EditorEnhancementsPossible: Boolean;
begin
  {$IFDEF GX_EditorEnhancements}
  Result := True;
  {$ELSE not GX_EditorEnhancements}
  Result := False;
  {$ENDIF}
end;

type
  TControlCracker = class(TControl);

procedure GxSetDefaultFont(Control: TControl);
begin
{$IFNDEF GX_STANDALONE}
  if ConfigInfo.EnableCustomFont then
    TControlCracker(Control).Font.Assign(ConfigInfo.CustomFont)
  else
{$ENDIF}
    SetDefaultFont(Control);
end;

end.
