///<summary>
/// Declares dummies for functions from GX_OtaUtils that return values appropriate for
/// failure (e.g. empty strings, Nil, False) so there are no compile errors for
/// stand alone tools that do not have access to the OTAPI </summary>
unit GX_OtaUtils;

interface

uses
  SysUtils,
  Classes,
  ToolsAPI,
  GX_StringList;

///<summary>
/// @returns True </summary>
function IsStandAlone: Boolean;

///<summary>
/// @returns False </summary>
function RunningInsideIDE: Boolean;

///<summary>
/// Fake function, always returs nil </summary>
function GxOtaGetModule(const _Filename: string): IOTAModule;

// Transform dfm, hpp, etc. references to the base .pas/cpp file name
function GxOtaGetBaseModuleFileName(const FileName: string): string;

// Returns the IOTASourceEditor interface for a module
// if there is a file that supports one; returns nil
// if there is no IOTASourceEditor
function GxOtaGetSourceEditorFromModule(Module: IOTAModule; const FileName: string = ''): IOTASourceEditor;

procedure GXOtaLoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList;
  var WasBinary: Boolean); overload;
procedure GXOtaLoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList); overload;

// raises an exception
function GxOtaGetEditWriterForSourceEditor(SourceEditor: IOTASourceEditor = nil): IOTAEditWriter;

// always returns False
function GxOtaIsFileOpen(const AFileName: string; UseBase: Boolean = False): Boolean;

function GXOtaGetIdeBaseRegistryKey: string;

// Determine is a file exists or the file's module is currently loaded in the IDE
function GxOtaFileOrModuleExists(const AFileName: string; UseBase: Boolean = False): Boolean;

// returns Nil
function GxOtaGetProjectGroup: IOTAProjectGroup;

///<summary>
/// @returns an empty string </summary>
function GxOtaGetProjectFileName(Project: IOTAProject; NormalizeBdsProj: Boolean = False): string;

///<summary>
/// @returns False </summary>
function GxOtaGetCurrentMapFileName(out MapFile: string): Boolean;

///<summary>
/// does nothing </summary>
procedure GxOtaGetAllPossiblePaths(Paths: TStrings; Project: IOTAProject = nil; AdditionalPaths: TStrings = nil);

// Returns an empty string
function GxOtaGetCurrentSourceFile: string;

// returns Nil
function GxOtaGetCurrentProject: IOTAProject;

function GxOtaCurrentProjectIsNativeCpp: Boolean;

function GxOtaCurrentProjectIsCSharp: Boolean;

// returns Nil
function GxOtaGetFormEditorFromModule(const Module: IOTAModule): IOTAFormEditor;

procedure GxOtaGoToFileLineColumn(const FileName: string; Line: Integer;
  StartColumn: Integer = 0; StopColumn: Integer = 0; ShowInMiddle: Boolean = True);

// returns False
function GxOtaGetActiveEditorText(Lines: TGXUnicodeStringList; UseSelection: Boolean = True): Boolean;

// Returns an empty string
function GxOtaGetCurrentSelection(IncludeTrailingCRLF: Boolean = True): string;

// returns Nil
function GxOtaGetCurrentEditorAsSourceEditor: IOTASourceEditor;

///<sumamry>
/// @returns an emptry string if no or more than 1 component is selected. </summary>
function GxOtaSelectedComponentName: string;

// Returns an empty string
function GxOtaGetCurrentIdent: string;

// Returns an empty string
function GxOtaGetCurrentProjectName: string;

// Returns an empty string
function GxOtaGetProjectGroupFileName: string;

// Returns an empty string
function GxOtaGetFileNameOfCurrentModule: string;

// returns False
function GxOtaHaveCPPSupport: Boolean;

// returns False
function GxOtaHaveCSharpSupport: Boolean;

function GxOtaGetIDEProductIdentifier: string;

implementation

uses
  GX_GenericUtils,
  GX_VerDepConst;

function IsStandAlone: Boolean;
begin
  Result := True;
end;

function RunningInsideIDE: Boolean;
begin
  Result := False;
end;

function GxOtaGetModule(const _Filename: string): IOTAModule;
begin
  Result := nil;
end;

function GxOtaGetBaseModuleFileName(const FileName: string): string;
var
  AltName: string;
begin
  Result := FileName;
  if IsForm(FileName) then begin
    AltName := ChangeFileExt(FileName, '.cpp');
    if FileExists(AltName) then
      Result := AltName;
    AltName := ChangeFileExt(FileName, '.pas');
    if FileExists(AltName) then
      Result := AltName;
  end;
end;

function GxOtaGetSourceEditorFromModule(Module: IOTAModule; const FileName: string = ''): IOTASourceEditor;
begin
  Result := nil;
end;

procedure GXOtaLoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList;
  var WasBinary: Boolean);
begin
  Assert(Assigned(Data));
  Data.Clear;
  if IsEmpty(FileName) then
    raise Exception.Create('Blank filenames are not allowed');

  LoadDiskFileToUnicodeStrings(FileName, Data, WasBinary)
end;

procedure GXOtaLoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList); overload;
var
  WasBinary: Boolean;
begin
  GXOtaLoadFileToUnicodeStrings(FileName, Data, WasBinary);
end;

function GxOtaGetEditWriterForSourceEditor(SourceEditor: IOTASourceEditor = nil): IOTAEditWriter;
begin
  raise Exception.Create('Stand alone version does not support ToolsAPI calls (GxOtaGetEditWriterForSourceEditor)');
end;

function GxOtaIsFileOpen(const AFileName: string; UseBase: Boolean = False): Boolean;
begin
  Result := False;
end;

function GXOtaGetIdeBaseRegistryKey: string;
begin
  Result := 'Software\' + CompilerDefinedProductRegistryKey;

  if Length(Result) > 0 then begin
    // Windows 2000 is allergic to a leading backslash
    // in the registry key - NT4, for instance, is not.
    if Result[1] = '\' then
      Delete(Result, 1, 1);

    Assert(Result[Length(Result)] <> '\');
  end;
end;

function GxOtaFileOrModuleExists(const AFileName: string; UseBase: Boolean): Boolean;
begin
  Result := FileExists(AFileName);
end;

function GxOtaGetProjectGroup: IOTAProjectGroup;
begin
  Result := nil;
end;

function GxOtaGetProjectFileName(Project: IOTAProject; NormalizeBdsProj: Boolean = False): string;
begin
  Result := '';
end;

function GxOtaGetCurrentMapFileName(out MapFile: string): Boolean;
begin
  Result := False;
end;

procedure GxOtaGetAllPossiblePaths(Paths: TStrings; Project: IOTAProject = nil; AdditionalPaths: TStrings = nil);
begin
  // do nothing
end;

function GxOtaGetCurrentSourceFile: string;
begin
  Result := '';
end;

function GxOtaGetCurrentProject: IOTAProject;
begin
  Result := nil;
end;

function GxOtaCurrentProjectIsNativeCpp: Boolean;
begin
  Result := False;
end;

function GxOtaCurrentProjectIsCSharp: Boolean;
begin
  Result := False;
end;

function GxOtaGetFormEditorFromModule(const Module: IOTAModule): IOTAFormEditor;
begin
  Result := nil;
end;

procedure GxOtaGoToFileLineColumn(const FileName: string; Line: Integer; StartColumn: Integer = 0;
  StopColumn: Integer = 0; ShowInMiddle: Boolean = True);
begin
  // do nothing
end;

function GxOtaGetActiveEditorText(Lines: TGXUnicodeStringList; UseSelection: Boolean = True): Boolean;
begin
  Result := False;
end;

function GxOtaGetCurrentSelection(IncludeTrailingCRLF: Boolean = True): string;
begin
  Result := '';
end;

function GxOtaGetCurrentEditorAsSourceEditor: IOTASourceEditor;
begin
  Result := nil;
end;

function GxOtaSelectedComponentName: string;
begin
  Result := '';
end;

function GxOtaGetCurrentIdent: string;
begin
  Result := '';
end;

function GxOtaGetCurrentProjectName: string;
begin
  Result := '';
end;

function GxOtaGetProjectGroupFileName: string;
begin
  Result := '';
end;

function GxOtaGetFileNameOfCurrentModule: string;
begin
  Result := '';
end;

function GxOtaHaveCPPSupport: Boolean;
begin
  Result := False;
end;

function GxOtaHaveCSharpSupport: Boolean;
begin
  Result := False;
end;

function GxOtaGetIDEProductIdentifier: string;
begin
  Result := '';
end;

end.
