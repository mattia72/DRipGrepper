unit GX_OtaUtils;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils, Classes, Controls, Contnrs, Forms, ActnList, ImgList, Menus, ToolsAPI,
  Actions, Graphics,
  GX_GenericUtils, GX_StringList, GX_IdeUtils;

// Idea: These functions could be encapsulated as class functions which would reduce the
// pollution of the namespace and also make for better code completion.
// and with a short class name that would not mean more typing.
// E.g.:
//type
//  // no T-prefix since this is only a container for class methods
//  Ota = class
//    class function GetNativeObject(const AComponent: IOTAComponent): TObject;
//  end;
//
// instead of
//   x := GxOtaGetNativeObject(bla);
// we'd type
//   x := Ota.GetNativeObject(bla);
// which would even save one character. ;-)
// It would also improve readability because it's easier to ignore a prefix which
// is separated by a '.'.
// On top of that
//   Ota.TryGetWhatever()
// is a lot more readable than
//   GxOtaTryGetWhatever()

// Returns the TObject represented by an IOTAComponent, if possible
function GxOtaGetNativeObject(const AComponent: IOTAComponent): TObject;
// Returns the TPersistent represented by an IOTAComponent, if possible
function GxOtaGetNativePersistent(const AComponent: IOTAComponent): TPersistent;
// Returns the TComponent represented by an IOTAComponent, if possible
function GxOtaGetNativeComponent(const AComponent: IOTAComponent): TComponent;
// Returns if the component is inherited from a parent form
function GxOtaIsInheritedComponent(const AComponent: IOTAComponent): Boolean;
// Returns the name of the component
function GxOtaGetComponentName(const AComponent: IOTAComponent): WideString;
// Sets the name of the component
function GxOtaSetComponentName(const AComponent: IOTAComponent; const Name: WideString): Boolean;
// Returns the name of the component's parent if the component has a parent
function GxOtaGetComponentParentName(const AComponent: IOTAComponent): WideString;
// See if the two IOTAComponents are the same component
function GxOtaComponentsAreEqual(Comp1, Comp2: IOTAComponent): Boolean;

procedure GxOtaGetInstalledComponentList(Components: TStrings; ActiveOnly: Boolean);

function GxOtaGetComponentPropertyAsString(const AComponent: IOTAComponent;
  const PropertyName: string; PreferStrings: Boolean = False): WideString; overload;
function GxOtaSetComponentPropertyAsString(const AComponent: IOTAComponent;
  const PropertyName: string; const Value: WideString): Boolean; overload;
function GxOtaNormalizePropertyValue(Component: IOTAComponent; const PropName, Value: string): string;
function GxOtaGetPropertyIndexByName(const AComponent: IOTAComponent;
  const PropertyName: WideString): Integer;

function GxOtaEditorIsRecordingMacro: Boolean;
function GxOtaEditorIsPlayingMacro: Boolean;
function GxOtaEditorIsPausedMacro: Boolean;
procedure GxOtaSetEditorKeyTracingEnabled(Enabled: Boolean);
function GxOtaGetEditorKeyTracingEnabled: Boolean;

const
  // Invalid notifier index
  InvalidNotifierIndex = -1;
  dNFM = 'nfm';  // Not present in the Delphi 8/9/10 ToolsAPI.pas
  // todo: These are only the English and German names, what about French and Japanese?
  FileVersionOptionNames:     array [0..1] of string = ('FileVersion', 'Dateiversion');
  ProductNameOptionNames:     array [0..1] of string = ('ProductName', 'Produktname');
  InternalNameOptionNames:    array [0..1] of string = ('InternalName', 'Interner Name');
  FileDescriptionOptionNames: array [0..1] of string = ('FileDescription', 'Beschreibung');
  ProductVersionOptionNames:  array [0..1] of string = ('ProductVersion', 'Produktversion');

  CodeInsightKeyCodeCompletion   = #0;
  CodeInsightKeyParameterInsight = #1;
  CodeInsightKeyBrowse           = #2;
  CodeInsightKeySymbolHint       = #3;

  {$IFNDEF GX_VER160_up}
  sDefaultPersonality = 'Default.Personality';
  sDelphiPersonality = 'Delphi.Personality';
  sDelphiDotNetPersonality = 'DelphiDotNet.Personality';
  sCBuilderPersonality = 'CPlusPlusBuilder.Personality';
  sCSharpPersonality = 'CSharp.Personality';
  sVBPersonality = 'VB.Personality';
  sDesignPersonality = 'Design.Personality';
  {$ENDIF}

  OPTION_NAME_GEN_DLL =  'GenDLL';
  OPTION_NAME_LIB_PREFIX = 'SOPrefix';
  OPTION_NAME_LIB_SUFFIX = 'SOSuffix';
  OPTION_NAME_LIB_VERSION = 'SOVersion';
  OPTION_NAME_GEN_PACKAGE = 'GenPackage';
  // The following should really be 'DCC_xxxx' for newer IDEs
  // (see unit DCCStrs in the ToolsApi directory),
  // but the old names still work so we keep using them until they break or we
  // drop support for older IDEs.
  // It's more complicated: The old names seem to work for IOTAProjectOptions.Values[], but not for
  // IOTABuildConfiguration.Values[] and vice versa (tested with Delphi 2007 and 10.2)
  // -- 2021-11-21 twm
  // 'DCC_BplOutput'
  OPTION_NAME_PKG_DLL_DIR = 'PkgDllDir';
  // 'DCC_ExeOutput'
  OPTION_NAME_OUTPUT_DIR = 'OutputDir';
  // 'DCC_DefaultNamespace'
  OPTION_NAME_DEFAULT_NAMESPACE = 'DefaultNamespace';
  // 'DCC_NameSpace'
  OPTION_NAME_NAMESPACE_PREFIX = 'NamespacePrefix';
  // 'DCC_UnitSearchPath'
  OPTION_NAME_UNIT_SEARCH_PATH = 'SrcDir';
  // 'DCC_UnitAlias'
  OPTION_NAME_UNIT_ALIASES = 'UnitAliases';

const
  // from ToolsApi\PlatormConst.pas
  { Universal platform ids }
  // If you add new platform ID, you have to update
  //  app\delphicore\PasMgr.pas
  //
  cWin32Platform = 'Win32';
  cWinNX32Platform = 'WinNX32';
  cWinIoT32Platform = 'WinIoT32';
  cWinARMPlatform = 'WinARM';
  cOSX32Platform = 'OSX32';
  cWin64Platform = 'Win64';
  cLinux32Platform = 'Linux32';
  cAndroidPlatform = 'Android';
  ciOSSimulatorPlatform = 'iOSSimulator';
  ciOSDevice32Platform = 'iOSDevice32';
  ciOSDevice64Platform = 'iOSDevice64';
  ciOSDevicePlatform = ciOSDevice32Platform;
  cLinux64Platform = 'Linux64';
  cOSX64Platform = 'OSX64';
  cLinux32ARMPlatform = 'Linux32ARM';
  cLinux64ARMPlatform = 'Linux64ARM';
  cAndroid64Platform = 'Android64';

const
  // from ToolsApi\DesignConst.pas
  sAllConfigurations = 'All configurations';
  sAllPlatforms = 'All platforms';
  sPlatform = ' platform';
  sConfiguration = ' configuration';

///<summary>
/// Returns the full name of the given platform, e.g.
/// '32-bit Windows platform' for 'Win32'
/// for en empty string the result is 'All platforms'
/// for an unknown platform the result is Platform + ' platform' </summary>
function GxOtaGetPlatformCaption(const _Platform: string): string;

// returns an IOTAEditReader for the given or the current IOTASourceEditor if none is specified
function GxOtaGetEditReaderForSourceEditor(SourceEditor: IOTASourceEditor = nil): IOTAEditReader;

// Save an edit reader to a stream
procedure GxOtaSaveReaderToStream(EditReader: IOTAEditReader; Stream: TStream; TrailingNull: Boolean = True);

// Like above, but return a string
function GxOtaReadEditorTextToString(EditReader: IOTAEditReader): string;

// If UseSelection is True, get the selected text in the current edit view
// (if any) and return True or get all of the editor's text (if no selection)
// and return False
function GxOtaGetActiveEditorText(Lines: TGXUnicodeStringList; UseSelection: Boolean = True): Boolean;

// Returns the editor's active text as a string, similar to above
function GxOtaGetActiveEditorTextAsString(var Text: string; UseSelection: Boolean = True): Boolean;
function GxOtaGetActiveEditorTextAsUnicodeString(var Text: TGXUnicodeString; UseSelection: Boolean = True): Boolean;

// Get current identifier under cursor in the active edit buffer
// Scans current line from cursor position to left and right for identifier name.
function GxOtaGetCurrentIdent: string;

// Replace the source editor's text with the passed text
procedure GxOtaReplaceEditorText(SourceEditor: IOTASourceEditor; const AText: string);
procedure GxOtaReplaceEditorTextWithUnicodeString(SourceEditor: IOTASourceEditor; Text: TGXUnicodeString);

// Returns current line from edit buffer
// StartOffset is the start offset of line (zero-based)
// ColumnNo is the current column number (zero-based, can be > than number of chars in line)
// LineNo is the current line number (1 based, yes, that's inconsistent)
function GxOtaGetCurrentLineData(var StartOffset, ColumnNo, LineNo: Integer; ByteBased: Boolean = False): string;
// Calls GxOtaGetCurrentLineData but only returns the line as a string
function GxOtaGetCurrentLine: string;
function GxOtaGetEditorLine(View: IOTAEditView; LineNo: Integer): UTF8String;
function GxOtaGetEditorLineAsString(View: IOTAEditView; LineNo: Integer): TGXUnicodeString;

// Get the identifier at the cursor position, similar to above.
// If an identifier is found then:
// - Ident is the identifier text (if one is under the cursor)
// - IdentOffset is the starting offset of the identifier in the character stream
// - StartPos is the position where the identifier starts (line/col, 1-based)
// - CurrentPos is the current cursor position (line/col)
// - AfterLen is the length of the part of the identifier that is after cursor
procedure GxOtaGetCurrentIdentEx(var Ident: string; var IdentOffset: Integer;
  var StartPos: TOTAEditPos; var CurrentPos: TOTAEditPos; var AfterLen: Integer; ByteBased: Boolean = False);
// Get the identifier and starting position only.  Return True on success.
function GxOtaGetCurrentIdentData(var Ident: string; var StartEditPos: TOTAEditPos): Boolean;

// Get all of the characters in the current line that are before the cursor
function GxOtaGetPreceedingCharactersInLine(View: IOTAEditView): string;

// Select the current identifier under the cursor
function GxOtaSelectCurrentIdent(const SourceEditor: IOTASourceEditor; MakeVisible: Boolean = True): Boolean;
// Select the current line of text
procedure GxOtaSelectCurrentLine(const SourceEditor: IOTASourceEditor; MakeVisible: Boolean = True);

// Insert a string into the current IOTASourceEditor
// This should be used when the Text is a single line
// It replaces the current selection (if any) and doesn't
// require multiple undo operations to undo the insert
procedure GxOtaInsertLineIntoEditor(const Text: string);

// Insert any text into the current IOTASourceEditor
// This should be used when the Text should not replace the
// current selection and when it can contain more than one
// line.  An undo might require multiple undo operations.
// This procedure ensures the cursor is still visible.
procedure GxOtaInsertTextIntoEditor(const Text: string);

// Get an edit writer for the current source editor (raise an exception if not availble)
// Use the current source editor if none is specified
function GxOtaGetEditWriterForSourceEditor(SourceEditor: IOTASourceEditor = nil): IOTAEditWriter;

// Insert a string into a file at a some character position
// Uses the current source editor if none is specified
procedure GxOtaInsertTextIntoEditorAtCharPos(const Text: string; Position: Longint;
  SourceEditor: IOTASourceEditor = nil);

// Insert a string into a file at a some buffer position
// Uses the current source editor if none is specified
procedure GxOtaInsertTextIntoEditorAtBufferPos(const Text: string; Position: Longint;
  SourceEditor: IOTASourceEditor = nil);

// Set the EditView's cursor position based on a character index or a
// TOTAEditPos.  Uses the topmost EditView if none is specified
procedure GxOtaGotoPosition(Position: Longint; EditView: IOTAEditView = nil;
  Middle: Boolean = True);
procedure GxOtaGotoEditPos(EditPos: TOTAEditPos; EditView: IOTAEditView = nil;
  Middle: Boolean = True);

// This function converts a character index into a TOTACharPos, since
// IOTAEditView.PosToCharPos doesn't work in D5/D6 (it always AVs)
function GxOtaGetCharPosFromPos(Position: Longint; EditView: IOTAEditView): TOTACharPos;

// Get the TOTAEditPos for the edit view (defaults to the topmost view)
function GxOtaGetCurrentEditPos(EditView: IOTAEditView = nil): TOTAEditPos;
function GxOtaTryGetCurrentEditPos(out _EditPos: TOTAEditPos): Boolean;
// Get the position in the character array for the edit view (defaults to the topmost view)
function GxOtaGetCurrentEditBufferPos(EditView: IOTAEditView = nil): Integer;

// Delete "Count" bytes starting from "StartIndex" in some source editor.
// Uses the current source editor if none is specified.
procedure GxOtaDeleteByteFromPos(StartIndex, Count: Longint; SourceEditor: IOTASourceEditor = nil);

// Delete Count characters starting from StartPos in some source editor
// Uses the current source editor if none is specified
procedure GxOtaDeleteTextFromPos(StartPos, Count: Longint; SourceEditor: IOTASourceEditor = nil);

// Replace an edit view's current selection with a string
procedure GxOtaReplaceSelection(const Editor: IOTASourceEditor; ViewNum: Integer;
  const Text: string);

// Load a file/form opened into the IDE to a unciode string list
procedure GxOtaLoadIDEFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList; var WasBinary: Boolean);
procedure GxOtaLoadSourceEditorToUnicodeStrings(SourceEditor: IOTASourceEditor; Data: TGXUnicodeStringList);
procedure GxOtaLoadFormEditorToUnicodeStrings(FormEditor: IOTAFormEditor; Data: TGXUnicodeStringList);


// Read the requested file's contents into a unicode string list.  This supports
// open/closed files, UTF-8 in the edior, and UTF-8/16/32 on disk, and form files.
procedure GxOtaLoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList); overload;
procedure GxOtaLoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList; var WasBinary: Boolean); overload;

// Test whether the last attribute before the current one is part of another
// one passed in InElement - or something else.  This is only called if
// GetAttributeAtPos returns atWhiteSpace - it is not clear, then, whether
// the cursor is inside another element.  Currently, this code is used for
// atComment and atString, see below.
function GxOtaIsWhiteSpaceInElement(const InElement: Integer;
  EV: IOTAEditView; EditorPosition: TOTAEditPos): Boolean;
function GxOtaIsWhiteSpaceInComment(const EditView: IOTAEditView; const EditorPosition: TOTAEditPos): Boolean;
function GxOtaIsWhiteSpaceInString(const EditView: IOTAEditView; const EditorPosition: TOTAEditPos): Boolean;

type
  TUnitInfo = class(TObject)
  private
    FFormName: string;
    FSourceName: string;
    FFileName: string;
  public
    property SourceName: string read FSourceName write FSourceName;
    property FormName: string read FFormName write FFormName;
    property FileName: string read FFileName write FFileName;
  end;

  TCompInfo = class(TObject)
  private
    FCompName: WideString;
    FCompType: WideString;
  public
    property CompName: WideString read FCompName write FCompName;
    property CompType: WideString read FCompType write FCompType;
  end;

procedure GxOtaFillComponentInfoList(const List: TList);

///<summary>
/// Fills List with TUnitInfo objects for all modules in the given project. </summary>
procedure GxOtaFillUnitInfoListForProject(List: TList; _Project: IOTAProject);

//
//
//

procedure GxOtaSelectComponentOnCurrentForm(const ComponentName: string);
procedure GxOtaClearSelectionOnCurrentForm;
procedure GxOtaGetSelectedComponents(Form: IOTAFormEditor; Components: TComponentList; ComponentClass: TComponentClass = nil);
procedure GxOtaGetComponentList(Form: IOTAFormEditor; Components: TComponentList; ComponentClass: TComponentClass = nil);

// Get a reference to the actual form instance being designed
function GxOtaGetCurrentDesignForm: TCustomForm;
// Refresh the object inspector's view into a designer
function GxOtaRefreshCurrentDesigner: Boolean;

// Write a simple (bold) title message to the IDE Messages window
procedure GxOtaWriteTitleMessage(const Title: string);
// Write a tool message to the IDE Messages window
procedure GxOtaWriteToolMessage(const FileName, MessageStr, PrefixStr: string;
  LineNumber, ColumnNumber: Integer);
// Clears all tool and title messages in the IDE Messages window
procedure GxOtaClearToolMessages;

// Ses if the IDE is curently debugging an application
function GxOtaCurrentlyDebugging: Boolean;

// Get an IDE services interface.  Succeeds or raises an exception.
function GxOtaGetIDEServices: IOTAServices;
function GxOtaGetDebuggerServices: IOTADebuggerServices;

// Determine if we are currently editing a VCL/CLX/NFM
function GxOtaGetActiveDesignerType: string;
function GxOtaActiveDesignerIsVCL: Boolean;
function GxOtaActiveDesignerIsCLX: Boolean;
function GxOtaActiveDesignerIsNFM: Boolean;
function GxOtaActiveDesignerIsFMX: Boolean;

// Get the personality identifier string for the project, or blank for none
function GxOtaGetProjectPersonality(Project: IOTAProject): string;
function GxOtaGetCurrentProjectPersonality: string;

// See if a project is of a specific personality
function GxOtaProjectIsDotNet(Project: IOTAProject): Boolean;
function GxOtaProjectIsDelphiDotNet(Project: IOTAProject): Boolean;
function GxOtaProjectIsNativeCpp(Project: IOTAProject): Boolean;
function GxOtaProjectIsNativeDelphi(Project: IOTAProject): Boolean;
function GxOtaProjectIsEitherDelphi(Project: IOTAProject): Boolean;
function GxOtaProjectIsCSharp(Project: IOTAProject): Boolean;
// See if the current project is of a specific personality
function GxOtaCurrentProjectIsDotNet: Boolean;
function GxOtaCurrentProjectIsDelphiDotNet: Boolean;
function GxOtaCurrentProjectIsNativeCpp: Boolean;
function GxOtaCurrentProjectIsNativeDelphi: Boolean;
function GxOtaCurrentProjectIsEitherDelphi: Boolean;
function GxOtaCurrentProjectIsCSharp: Boolean;

// Check if the IDE supports a certain personality
function GxOtaIdeSupportsPersonality(const Personality: string): Boolean;
function GxOtaHaveCPPSupport: Boolean;
function GxOtaHaveDelphiSupport: Boolean;
function GxOtaHaveCSharpSupport: Boolean;

// Get the preferred hex number display prefix characters for our personality
function GxOtaGetHexPrefix: string;

// See if the user is editing a form
function GxOtaCurrentlyEditingForm: Boolean;

// See if the user is editing a source file
function GxOtaCurrentlyEditingSource: Boolean;

// Obtain and focus the IDE's internal edit control wrapper
function GxOtaGetCurrentIDEEditControl: TWinControl;
function GxOtaFocusCurrentIDEEditControl: Boolean;

// Returns reference to currently active project;
// returns nil if there is no (active) project.
function GxOtaGetCurrentProject: IOTAProject;
function GxOtaTryGetCurrentProject(out _Project: IOTAProject): Boolean;

// See if there is an active project
function GxOtaHaveCurrentProject: Boolean;

///<summary>
/// Get the file name of the given project
/// @parram Project is the project whose file name is requested
/// @param NormalizeBdsProj = True means to get the the dpr instead of the bdsproj/dproj for Delphi projects
/// @returns an empty string if Project is nil, otherwise the full file name of the project </summary>
function GxOtaGetProjectFileName(Project: IOTAProject; NormalizeBdsProj: Boolean = False): string;

///<summary>
/// Get the file name of the current project
/// @param NormalizeBdsProj = True means to get the the dpr instead of the bdsproj/dproj for Delphi projects
/// @returns the full file name of the project or an empty string if there is no current project </summary>
function GxOtaGetCurrentProjectFileName(NormalizeBdsProj: Boolean = False): string;

///<summary>
/// Returns the name of the currently active project; returns
/// an empty string if there is no (active) project.
/// The name of a project is the project's file name without
/// a file extension and without path information. </summary>
function GxOtaGetCurrentProjectName: string;

// Returns the output directory of the given project
// taken from http://cc.embarcadero.com/item/19823
// (Ondrey Kelle)
function GxOtaGetProjectOutputDir(Project: IOTAProject): string;

///<summary>
/// Tries to find the current project's map file
/// @param MapFile will contain full path name of the map file, only valid if result is true
/// @returns true, if a current project exists and the map file could be found </summary>
function GxOtaGetCurrentMapFileName(out MapFile: string): Boolean;

// Returns reference to the IDE's project group;
// returns Nil if there is no project group.
function GxOtaGetProjectGroup: IOTAProjectGroup;

// Returns the full path and file name of IDE's project group.
// Returns an empty string if there is no project group.
function GxOtaGetProjectGroupFileName: string;

// Get the list of files in the project
procedure GxOtaGetProjectFileNames(Project: IOTAProject; Files: TStrings);

// Returns the file name of the currently "active"
// module; if no module is active, an empty
// string is returned.
function GxOtaGetFileNameOfCurrentModule: string;

///<summary>
/// Tries to get the options of the given or the active project
/// @param ProjectOptions will contain the options, only valid if Result is True
/// @param Project is the project whose options to get, it nil the current project will be used
/// @returns True, if the options could be retrieved, False otherwise (e.g. if there is no active project) </summary>
function GxOtaTryGetProjectOptions(out _ProjectOptions: IOTAProjectOptions; _Project: IOTAProject = nil): Boolean;

// Get the IOTAProjectOptions interface for the active project
function GxOtaGetActiveProjectOptions: IOTAProjectOptions;
///<summary>
/// Tries to get a configuration option from the active configuration of the given project
/// @param Option is the name of the option to retrieve
/// @param Value will return the value of the given option, only valid if Result is True
/// @param Project is the project for which to get the option, if nil the current project will be used
/// @returns True, if the option could be retrieved, False otherwise (e.g. if there is no active project) </summary>
function GxOtaTryGetProjectOption(const Option: string; var Value: Variant; _Project: IOTAProject = nil): Boolean;
// Get the value of a version info keys string
function GxOtaGetVersionInfoKeysOption(const Option: string; var Value: Variant): Boolean;
// Get the first value of a list of version info keys strings
function GxOtaGetVersionInfoKeysOptions(const Options: array of string; var Value: Variant): Boolean;
// Get all of the version info keys
function GxOtaGetVersionInfoKeysStrings(Strings: TStrings): Boolean;
function GxOtaGetVersionInfoKeysString: string;

// Set all the version info keys
function GxOtaSetVersionInfoKeysStrings(Strings: TStrings): Boolean;

// Get the selected text in the top edit view, if any
function GxOtaGetCurrentSelection(IncludeTrailingCRLF: Boolean = True): string;

function GxOtaMoveEditCursorColumn(EditView: IOTAEditView; RelativePos: Integer): Boolean;

// Returns -1, 0, 1 depending on the position relationship between two edit positions
function GxOtaCompareEditPos(AEditPos1, AEditPos2: TOTAEditPos): Integer;

// Select a range of text in the source editor
procedure GxOtaSelectBlock(const Editor: IOTASourceEditor; const Start, After: TOTACharPos); overload;
procedure GxOtaSelectBlock(const Editor: IOTASourceEditor; Start, After: TOTAEditPos); overload;
///<summary>
/// Gets the currently selected block
/// @param SelStart and SelLength are byte offsets (not character offsets) </summary>
function GxOtaGetSelection(const EditView: IOTAEditView;
  var BlockStart, BlockEnd: TOTAEditPos; var SelStart, SelLength: Integer): Boolean;

// Un-fold all regions or the nearest region in a file
procedure GxOtaUnfoldAllRegions(const View: IOTAEditView);
procedure GxOtaUnfoldNearestRegion(const View: IOTAEditView);

// Delete the selected text in the editor
function GxOtaDeleteSelection(const EditView: IOTAEditView = nil): Boolean;

// Close FileName; returns True on success, False otherwise.
function GxOtaCloseFile(const FileName: string): Boolean;
// Close the current edit view tab
procedure GxOtaCloseCurrentEditorTab;

// Open FileName in IDE; returns True on success, False otherwise.
function GxOtaOpenFile(const FileName: string): Boolean;
function GxOtaOpenFileOrForm(const FileName: string): Boolean;

// Returns True if FileName is an open file in the IDE;
// returns False otherwise.
// FileName must be a fully qualified file name, including
// the path name.  UseBase determines whether things like .dfm/.hpp map
// to their parent file type.  Otherwise, they will not be located.
function GxOtaIsFileOpen(const AFileName: string; UseBase: Boolean = False): Boolean;

// Determine is a file exists or the file's module is currently loaded in the IDE
function GxOtaFileOrModuleExists(const AFileName: string; UseBase: Boolean = False): Boolean;

// Make sure the given filename's source is visible.
// This swaps source/form views as necessary
function GxOtaMakeSourceVisible(const FileName: string): Boolean;
function GxOtaModuleIsShowingFormSource(Module: IOTAModule): Boolean;
procedure GxOtaGoToFileLine(const FileName: string; Line: Integer);
procedure GxOtaGoToFileLineColumn(const FileName: string; Line: Integer; StartColumn: Integer = 0; StopColumn: Integer = 0; ShowInMiddle: Boolean = True);
function GxOtaConvertColumnCharsToBytes(LineData: UTF8String; CharIndex: Integer; EndByte: Boolean): Integer;
function GxOtaConvertColumnBytesToChars(LineData: UTF8String; ByteIndex: Integer): Integer;

// Transform dfm, hpp, etc. references to the base .pas/cpp file name
function GxOtaGetBaseModuleFileName(const FileName: string): string;

// Get the environment options interface.  Succeeds or raises an exception.
function GxOtaGetEnvironmentOptions: IOTAEnvironmentOptions;
// Return the IDE's environment string value that is
// associated with EnvironmentStringName
function GxOtaGetIdeEnvironmentString(const EnvironmentStringName: string): string;
// Return all of the IDE's environment settings names
procedure GxOtaGetIdeEnvironmentStrings(Settings: TStrings);
// Return the IDE's global library path
function GxOtaGetIdeLibraryPath: string;
// Return the IDE's global browsing path
function GxOtaGetIdeBrowsingPath: string;

///<summary>
/// Returns the project's current platform, if any (and supported), or an empty string </summary>
function GxOtaGetProjectPlatform(Project: IOTAProject = nil): string;

///<summary>
/// Returns the project's current config, if any (and supported), or an empty string </summary>
function GxOtaGetProjectConfig(Project: IOTAProject = nil): string;

///<summary>
/// Returns the project's framework, if any (and supported), or an empty string </summary>
function GxOtaGetProjectFrameworkType(Project: IOTAProject = nil): string;
///<summary>
/// reads the project's namespaces </summary>
procedure GxOtaGetProjectNamespaces(var DefaultNamespace: string; SearchPath: TStrings; Project: IOTAProject = nil);
///<summary>
/// Return project specific search path, with the directory containing the project
// file first.
/// @params if DoProcessing is true, the paths are macro expanded and non-existing
///                         paths removed. </summary>
procedure GxOtaGetProjectSourcePathStrings(Paths: TStrings;
  Project: IOTAProject = nil; DoProcessing: Boolean = True);

///<summary>
/// Tries to get the project's unit search path
/// @param Path is the contents of the unit search path, only valid if Result is True
/// @param Project is the project whose search path to get, if nil, the current project is used
/// @returns True, if it could be retrieved, False otherwise (e.g. there is no active project)
function GxOtaTryGetProjectSearchPath(out _Path: string; _Project: IOTAProject = nil): Boolean;
///<summary>
/// Tries to set the project's unit search path
/// @param Path is the new contents of the unit search path
/// @param Project is the project whose search path to set, if nil, the current project is used
/// @returns True, if it could be set, False otherwise (e.g. there is no active project)
function GxOtaTrySetProjectSearchPath(const _Path: string; _Project: IOTAProject = nil): Boolean;

///<summary>
/// Return the global IDE library path (without the project specific paths).
/// @params if DoProcessing is true, the paths are macro expanded and non-existing
///                         paths removed. </summary>
procedure GxOtaGetIdeLibraryPathStrings(Paths: TStrings; DoProcessing: Boolean = true);
///<summary>
/// Return the effective library path, with the project specific paths
/// first and then the IDE's global library path.
/// @params if DoProcessing is true, the paths are macro expanded and non-existing
///                         paths removed.
/// @params AdditionalPaths are appended at the end and optionally processed too </summary>
procedure GxOtaGetEffectiveLibraryPath(Paths: TStrings;
  Project: IOTAProject = nil; DoProcessing: Boolean = True; AdditionalPaths: TStrings = nil);
///<summary>
/// Retrieve a guess at all possible paths where files in the current project
/// might be located by the compiler
/// @params AdditionalPaths are appended at the end and optionally processed too </summary>
procedure GxOtaGetAllPossiblePaths(Paths: TStrings; Project: IOTAProject = nil; AdditionalPaths: TStrings = nil);
// Locate a base file name in all possible paths (retrieved with GxOtaGetAllPossiblePaths)
function GxOtaTryFindPathToFile(const FileName: string; out FullFilename: string): Boolean;
function GxOtaFindPathToFile(const FileName: string): string;
// Try to open a file located anywhere in GxOtaGetAllPossiblePaths
function GxOtaOpenFileFromPath(const FileName: string): Boolean;
// Obtain a list of alias mappings in OldUnit=NewUnit format
procedure GxOtaGetUnitAliases(Aliases: TStrings);

// Returns IDE's base registry key, for instance
//   Software\Borland\Delphi\4.0
// The returned string is guaranteed to NOT have a
// backslash appended and it does NOT have a leading
// backslash either (Windows 2000 is allergic to that).
function GxOtaGetIdeBaseRegistryKey: string;

// Get a string describing the IDE such as 'Delphi' or 'C++Builder'
function GxOtaGetIDEProductIdentifier: string;

// Returns the IDE's image list.
function GxOtaGetIdeImageList: TCustomImageList;

// Return the IDE's main menu.
function GxOtaGetIdeMainMenu: TMainMenu;

// Return the IDE's main action list
function GxOtaGetIdeActionList: TCustomActionList;

// Find an action in the IDE's action list by name and return it.
function GxOtaGetIdeActionByName(const Name: string): TContainedAction;
function GxOtaTryGetIdeActionByName(const Name: string; out _Action: TContainedAction): Boolean;

function GxOtaGetEditorServices: IOTAEditorServices;

// Return the top-most edit view/buffer (globally or for a given source editor)
// Returns nil if none exists.
function GxOtaGetTopMostEditView: IOTAEditView; overload;
function GxOtaGetTopMostEditView(SourceEditor: IOTASourceEditor): IOTAEditView; overload;
function GxOtaGetTopMostEditBuffer: IOTAEditBuffer;
function GxOtaGetTopMostEditBufferFileName: string;

// calls GxOtaGetTopMostEditView and returns false if no edit view is available
function GxOtaTryGetTopMostEditView(out EditView: IOTAEditView): boolean; overload;
function GxOtaTryGetTopMostEditView(SourceEditor: IOTASourceEditor; out EditView: IOTAEditView): boolean; overload;

// Get the number of open modules
function GxOtaGetOpenModuleCount: Integer;

// Returns the current module; may return nil of there is no
// current module.
function GxOtaGetCurrentModule: IOTAModule;

// Returns the current IOTASourceEditor or nil if
// there is no current source editor.
// Note: This will return a source editor even if the currently active editor
// is a form editor! This is probably not what you want.
// See also GxOtaGetCurrentEditorAsSourceEditor
function GxOtaGetCurrentSourceEditor: IOTASourceEditor;
function GxOtaTryGetCurrentSourceEditor(out SourceEditor: IOTASourceEditor): boolean;
procedure GxOtaShowCurrentSourceEditor;

///<summary>
/// Get the current source code editor or nil if there is none (e.g. the current
/// editor is a form editor </summary>
function GxOtaGetCurrentEditorAsSourceEditor: IOTASourceEditor;
function GxOtaTryGetCurrentEditorAsSourceEditor(out SourceEditor: IOTASourceEditor): Boolean;

// Raise an exception if the source editor is readonly
procedure GxOtaAssertSourceEditorNotReadOnly(SourceEditor: IOTASourceEditor);

// Get the configured editor font.
procedure GxOtaGetEditorFont(const AFont: TFont; ASizeAdjust: Integer = 0);

// Returns a module interface for a given filename
// May return nil if no such module is open.
function GxOtaGetModule(const FileName: string): IOTAModule;
function GxOtaGetSourceEditor(const FileName: string): IOTASourceEditor;

// Returns a fully qualified name of the current file,
// which could either be a form or unit (.pas/.cpp/.dfm/.xfm etc.).
// Returns an empty string if no file is currently selected.
function GxOtaGetCurrentSourceFile: string;

// Returns the syntax highlighting enumeration constant that
// is applicable for the SourceEditor. Returns shNone if
// there is no syntax-highlighting (for instance, because it
// has been turned off in the IDE or because there is no
// current source editor).
function GxOtaGetCurrentSyntaxHighlighter(SourceEditor: IOTASourceEditor = nil): TGXSyntaxHighlighter;

// Returns True if property with name PropertyName exists for the component;
// False otherwise.
// Property names are insensitive to case.
function GxOtaPropertyExists(const Component: IOTAComponent; const PropertyName: string): Boolean;

function GxOtaGetFormEditorForFileName(const FileName: string): IOTAFormEditor;
// Gets the form editor for the active module, or nil if none is present
function GxOtaGetCurrentFormEditor: IOTAFormEditor;
function GxOtaTryGetCurrentFormEditor(out FormEditor: IOTAFormEditor): boolean;
// Gets the number of selected components on the current form editor (if any)
function GxOtaGetCurrentFormEditorSelectionCount: Integer;
// Determine if the curent form editor has a selected component (not the root designer)
function GxOtaFormEditorHasSelectedComponent: Boolean;
// Returns a form editor for Module if it exists; nil otherwise.
// Module can be nil, if it is , this function will return nil
function GxOtaGetFormEditorFromModule(const Module: IOTAModule): IOTAFormEditor;
// Shows the form editor for a module.  Returns True if successful.
function GxOtaShowFormForModule(const Module: IOTAModule): Boolean;
// Get an open form's text representation into a stream or as a string
procedure GxOtaGetFormAsText(Form: IOTAFormEditor; OutStream: TStream); overload;
function GxOtaGetFormAsText(Form: IOTAFormEditor): string; overload;

// Returns the IOTASourceEditor interface for a module
// if there is a file that supports one; returns nil
// if there is no IOTASourceEditor
function GxOtaGetSourceEditorFromModule(Module: IOTAModule; const FileName: string = ''): IOTASourceEditor;

// Get the active IOTAEditor (which can be an IOTASourceEditor or IOTAFormEditor
// (or maybe even more in future IDE versions)
function GxOtaGetCurrentEditor: IOTAEditor;

// Get an editor for the file indicated by FileName
// contained in the passed Module
function GxOtaGetEditorFromModule(Module: IOTAModule; const FileName: string): IOTAEditor;

// Get the Index-th IOTAEditor for the given module
// Works around a BCB 5 bug with simple units
function GxOtaGetFileEditorForModule(Module: IOTAModule; Index: Integer): IOTAEditor;

// Obtain the IOTAEditActions interface for a given module
function GxOtaGetEditActionsFromModule(Module: IOTAModule): IOTAEditActions;
// Obtain the IOTAEditActions interface for the current module
function GxOtaGetEditActions: IOTAEditActions;

// Returns True if the only selected component on FormEditor is the "root"
// of all components, i.e. the data module or form on which everything
// is hosted.
// Returns False if there is more than one component selected
// or if the currently selected component is not the root.
function GxOtaSelectedComponentIsRoot(const FormEditor: IOTAFormEditor): Boolean;

///<summary>
/// Retrieves the name of the current selected component on FormEditor.
/// @returns an emptry string if no or more than 1 component is selected. </summary>
function GxOtaSelectedComponentName: string;

//  Get the BiDiMode property for a VCL form
function GxOtaGetFormBiDiMode(Form: IOTAFormEditor): TBiDiMode;

// Returns True if FileName points to a valid expert DLL
// (valid wizard / expert entry point). False if not.
// Always returns True on Windows 9x, since Windows 9x does not
// support the DONT_RESOLVE_DLL_REFERENCES parameter for plain
// loading of the DLL as data.
function IsValidExpertDll(const FileName: string): Boolean;

function IsStandAlone: Boolean;
function RunningInsideIDE: Boolean;

// Consolidate with GxOtaGotoPosition?
procedure GxOtaSetCurrentSourcePosition(Position: Integer);

function GxOtaGetKeyboardServices: IOTAKeyboardServices;

// Get the configures tab stops as a string for the current editor file
function GxOtaGetTabStops: string;

// Returns tab size (single value)
// Note: works only for configurations with single value in "Tab Stops" option
function GxOtaGetTabWidth: Integer;

// Expand the tabs in the lines into spaces using the current editor tab witdh
procedure GxOtaExpandTabsInList(ALines: TStrings);

// Return the tab-expanded whitespace at the beginning of the current selection
function GxOtaGetLeadingWhiteSpaceInSelection: string;

// Show a dialog enumerating all of the current project's modules and editors
procedure GxOtaShowProjectModuleInformation;

// Show a dialog enumerating all of the IDE's actions with details
procedure GxOtaShowIDEActions;
procedure GxOtaShowEditViewDetails;

function HackBadIDEUTF8StringToString(const S: string): string;
// Convert an editor string (UTF-8) possibly improperly stored in an
// AnsiString to a native string type
function HackBadEditorStringToNativeString(const S: string): string;

type
  TBaseIdeNotifier = class(TNotifierObject, IOTAIDENotifier)
  private
    FNotifierIndex: Integer;
  public
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean); virtual;
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); virtual;
    procedure AfterCompile(Succeeded: Boolean); virtual;
    function AddNotifierToIDE: Boolean;
    function RemoveNotifierFromIDE: Boolean;
    destructor Destroy; override;
  end;

  TBaseDebuggerNotifier = class(TNotifierObject, IOTADebuggerNotifier)
  private
    FNotifierIndex: Integer;
  public
    function AddNotifierToIDE: Boolean;
    function RemoveNotifierFromIDE: Boolean;
    destructor Destroy; override;
    // IOTADebuggerNotifier
    procedure BreakpointAdded({$IFDEF GX_VER170_up} const {$ENDIF} Breakpoint: IOTABreakpoint); virtual;
    procedure BreakpointDeleted({$IFDEF GX_VER170_up} const {$ENDIF} Breakpoint: IOTABreakpoint); virtual;
    procedure ProcessCreated({$IFDEF GX_VER170_up} const {$ENDIF} Process: IOTAProcess); virtual;
    procedure ProcessDestroyed({$IFDEF GX_VER170_up} const {$ENDIF} Process: IOTAProcess); virtual;
  end;

type
  TProjectChangedNotifier = class(TBaseIdeNotifier)
  private
    FOnProjectChanged: TNotifyEvent;
    FLastProject: string;
  public
    constructor Create(_OnProjectChanged: TNotifyEvent);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean); override;
  end;

type
  EStandAloneUsage = class(Exception);

const
  NamePropertyName = 'Name';
  InvalidIntValue = -99999;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Variants, Windows, ActiveX, DesignIntf, TypInfo,
  GX_EditReader, GX_VerDepConst, SynUnicode, Math, StrUtils,
  GX_GetIdeVersion, u_dzFileUtils, u_dzClassUtils;

const
  EditReaderBufferSize = 1024 * 24;

procedure GxOtaFillUnitInfoListForProject(List: TList; _Project: IOTAProject);
var
  i: Integer;
  ModuleInfo: IOTAModuleInfo;
  UnitInfo: TUnitInfo;
begin
  Assert(Assigned(List));
  Assert(Assigned(_Project));

  TList_ClearAndFreeAllItems(List);

  for i := 0 to _Project.GetModuleCount - 1 do
  begin
    ModuleInfo := _Project.GetModule(i);
    Assert(Assigned(ModuleInfo));

    if ModuleInfo.FileName <> '' then
    begin
      UnitInfo := TUnitInfo.Create;
      List.Add(UnitInfo);

      UnitInfo.FileName := ModuleInfo.FileName;
      UnitInfo.FormName := ModuleInfo.FormName;
      UnitInfo.SourceName := ModuleInfo.Name;
    end;
  end;
end;

procedure GxOtaFillComponentInfoList(const List: TList);
var
  FormEditor: IOTAFormEditor;
  RootComponent: IOTAComponent;
  OwnedComponent: IOTAComponent;
  CompInfo: TCompInfo;
  i: Integer;
begin
  Assert(Assigned(List));

  if GxOtaTryGetCurrentFormEditor(FormEditor) then
  begin
    RootComponent := FormEditor.GetRootComponent;
    Assert(Assigned(RootComponent), 'FormEditor has no root component');

    for i := 0 to RootComponent.GetComponentCount-1 do
    begin
      OwnedComponent := RootComponent.GetComponent(i);
      if Assigned(OwnedComponent) then
      begin
        CompInfo := TCompInfo.Create;
        CompInfo.CompName := GxOtaGetComponentName(OwnedComponent);
        CompInfo.CompType := OwnedComponent.GetComponentType;
        List.Add(CompInfo);
      end;
    end;
  end;
end;

function GxOtaGetFormEditorForFileName(const FileName: string): IOTAFormEditor;
var
  Module: IOTAModule;
begin
  Module := GxOtaGetModule(GxOtaGetBaseModuleFileName(FileName));
  Result := GxOtaGetFormEditorFromModule(Module);
end;

function GxOtaGetCurrentFormEditor: IOTAFormEditor;
begin
  Result := GxOtaGetFormEditorFromModule(GxOtaGetCurrentModule);
end;

function GxOtaTryGetCurrentFormEditor(out FormEditor: IOTAFormEditor): boolean;
begin
  FormEditor := GxOtaGetCurrentFormEditor;
  Result := Assigned(FormEditor);
end;

function GxOtaGetCurrentFormEditorSelectionCount: Integer;
var
  FormEditor: IOTAFormEditor;
begin
  Result := 0;
  if GxOtaTryGetCurrentFormEditor(FormEditor) then
    Result := FormEditor.GetSelCount;
end;

function GxOtaFormEditorHasSelectedComponent: Boolean;
var
  FormEditor: IOTAFormEditor;
  FileName: string;
begin
  Result := False;
  FileName := GxOtaGetCurrentSourceFile;
  if not IsForm(FileName) then
    Exit;

  if GxOtaTryGetCurrentFormEditor(FormEditor) then
  begin
    Result := (FormEditor.GetSelCount > 0) and
      not GxOtaSelectedComponentIsRoot(FormEditor);
  end;
end;

function GxOtaGetFormEditorFromModule(const Module: IOTAModule): IOTAFormEditor;
var
  i: Integer;
  Editor: IOTAEditor;
  FormEditor: IOTAFormEditor;
begin
  Result := nil;
  if not Assigned(Module) then
    Exit;
  for i := 0 to Module.GetModuleFileCount-1 do
  begin
    Editor := GxOtaGetFileEditorForModule(Module, i);
    if Supports(Editor, IOTAFormEditor, FormEditor) then
    begin
      Assert(not Assigned(Result), 'There should never be more than one form for a module');
      Result := FormEditor;
      // In order to assert our assumptions that only one form
      // is ever associated with a module, do not call Break; here.
    end;
  end;
end;

function GxOtaShowFormForModule(const Module: IOTAModule): Boolean;
var
  FormEditor: IOTAFormEditor;
begin
  Result := False;
  Assert(Assigned(Module));
  FormEditor := GxOtaGetFormEditorFromModule(Module);
  if Assigned(FormEditor) then
  begin
    FormEditor.Show;
    Result := True;
  end;
end;

procedure GxOtaGetFormAsText(Form: IOTAFormEditor; OutStream: TStream); overload;
var
  MemoryStream: TMemoryStream;
  Stream: IStream;
  StreamFormat: TStreamOriginalFormat;
begin
  Assert(Assigned(OutStream));
  Assert(Assigned(Form));

  Stream := TStreamAdapter.Create(OutStream);
  Form.GetFormResource(Stream);
  StreamFormat := TestStreamFormat(OutStream);
  if StreamFormat = sofBinary then begin
    MemoryStream := TMemoryStream.Create;
    try
      MemoryStream.CopyFrom(OutStream, OutStream.Size);
      MemoryStream.Position := 0;
      OutStream.Position := 0;
      ObjectResourceToText(MemoryStream, OutStream);
    finally
      FreeAndNil(MemoryStream);
    end;
  end;
  OutStream.Position := 0;
end;

function GxOtaGetFormAsText(Form: IOTAFormEditor): string;
var
  StringStream: TStringStream;
begin
  Assert(Assigned(Form));
  StringStream := TStringStream.Create('');
  try
    GxOtaGetFormAsText(Form, StringStream);
    Result := StringStream.DataString;
  finally
    FreeAndNil(StringStream);
  end;
end;

procedure GxOtaSelectComponentOnCurrentForm(const ComponentName: string);
var
  FormEditor: IOTAFormEditor;
  FoundComponent: IOTAComponent;
begin
  if Length(ComponentName) = 0 then
    Exit;

  FormEditor := GxOtaGetCurrentFormEditor;
  Assert(Assigned(FormEditor));

  FoundComponent := FormEditor.FindComponent(ComponentName);
  if not Assigned(FoundComponent) then
    Exit;

  FoundComponent.Select(False); // Select, but do not add to an already existing selection
  FormEditor.Show;
end;

procedure GxOtaClearSelectionOnCurrentForm;
var
  FormEditor: IOTAFormEditor;
  PersistentForm: TPersistent;
  Designer: IDesigner;
begin
  if GxOtaTryGetCurrentFormEditor(FormEditor) then
  begin
    if Assigned(FormEditor.GetRootComponent) then
    begin
      PersistentForm := GxOtaGetNativePersistent(FormEditor.GetRootComponent);
      Designer := (FormEditor as INTAFormEditor).FormDesigner;
      if Assigned(Designer) then
        if Assigned(PersistentForm) then
          Designer.SelectComponent(PersistentForm);
    end;
  end;
end;

procedure GxOtaGetSelectedComponents(Form: IOTAFormEditor; Components: TComponentList; ComponentClass: TComponentClass);
var
  i: Integer;
  Component: IOTAComponent;
  NativeComponent: TComponent;
begin
  Assert(Assigned(Components));
  Components.Clear;
  if Form = nil then
    Form := GxOtaGetCurrentFormEditor;
  if Assigned(Form) then
  begin
    for i := 0 to Form.GetSelCount - 1 do
    begin
      Component := Form.GetSelComponent(i);
      NativeComponent := GxOtaGetNativeComponent(Component);
      if Assigned(NativeComponent) then
      begin
        if (not Assigned(ComponentClass)) or (Assigned(ComponentClass) and (NativeComponent is ComponentClass)) then
          Components.Add(NativeComponent);
      end;
    end;
  end;
end;

procedure GxOtaGetComponentList(Form: IOTAFormEditor; Components: TComponentList; ComponentClass: TComponentClass = nil);

  procedure GetControlsForParent(ParentControl: TWinControl);
  var
    j: Integer;
    ChildControl: TControl;
  begin
    for j := 0 to ParentControl.ControlCount - 1 do
    begin
      ChildControl := ParentControl.Controls[j];
      if (not Assigned(ComponentClass)) or (Assigned(ComponentClass) and (ChildControl is ComponentClass)) then
        Components.Add(ChildControl);
      if ChildControl is TWinControl then
        GetControlsForParent(ChildControl as TWinControl);
    end;
  end;

var
  Root: TComponent;
  RootControl: TWinControl;
begin
  Assert(Assigned(Components));
  Components.Clear;
  if Form = nil then
    Form := GxOtaGetCurrentFormEditor;
  if Assigned(Form) then
  begin
    Root := GxOtaGetNativeComponent(Form.GetRootComponent);
    if Assigned(Root) and (Root is TWinControl) then
    begin
      RootControl := Root as TWinControl;
      GetControlsForParent(RootControl);
    end;
  end;
end;

function GxOtaGetCurrentDesignForm: TCustomForm;
var
  FormEditor: IOTAFormEditor;
  FormDesigner: IDesigner;
  Root: TComponent;
begin
  Result := nil;

  if not GxOtaTryGetCurrentFormEditor(FormEditor) then
    Exit;

  FormDesigner := (FormEditor as INTAFormEditor).FormDesigner;
  if not Assigned(FormDesigner) then
    Exit;

  Root := FormDesigner.Root;
  if Root is TCustomForm then
    Result := TCustomForm(Root)
  else if Assigned(Root) then
  begin
    Root := Root.Owner;
    if Root is TCustomForm then
      Result := TCustomForm(Root)
  end;
end;

function GxOtaRefreshCurrentDesigner: Boolean;
var
  FormEditor: IOTAFormEditor;
  FormDesigner: IDesigner;
begin
  Result := False;

  if not GxOtaTryGetCurrentFormEditor(FormEditor) then
    Exit;

  FormDesigner := (FormEditor as INTAFormEditor).FormDesigner;
  if Assigned(FormDesigner) then
  begin
    FormDesigner.Modified;
    Result := True;
  end;
end;

procedure GxOtaWriteTitleMessage(const Title: string);
var
  IMessageServices: IOTAMessageServices;
begin
  IMessageServices := BorlandIdeServices as IOTAMessageServices;
  Assert(Assigned(IMessageServices));

  IMessageServices.AddTitleMessage(Title);
end;

procedure GxOtaWriteToolMessage(const FileName, MessageStr, PrefixStr: string;
  LineNumber, ColumnNumber: Integer);
var
  IMessageServices: IOTAMessageServices;
begin
  IMessageServices := BorlandIdeServices as IOTAMessageServices;
  Assert(Assigned(IMessageServices));

  IMessageServices.AddToolMessage(FileName, MessageStr, PrefixStr,
    LineNumber, ColumnNumber);
end;

procedure GxOtaClearToolMessages;
var
  IMessageServices: IOTAMessageServices;
begin
  IMessageServices := BorlandIdeServices as IOTAMessageServices;
  if Assigned(IMessageServices) then
    IMessageServices.ClearToolMessages;
end;

function GxOtaCurrentlyDebugging: Boolean;
var
  DebuggerServices: IOTADebuggerServices;
begin
  Result := False;
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
    Result := DebuggerServices.ProcessCount > 0;
end;

function GxOtaGetIDEServices: IOTAServices;
begin
  Result := BorlandIDEServices as IOTAServices;
  if not Assigned(Result) then
    raise Exception.Create('IOTAServices not implemented');
end;

function GxOtaGetDebuggerServices: IOTADebuggerServices;
begin
  Result := BorlandIDEServices as IOTADebuggerServices;
  if not Assigned(Result) then
    raise Exception.Create('IOTADebuggerServices not implemented');
end;

function GxOtaGetActiveDesignerType: string;
begin
  Result := GxOtaGetIDEServices.GetActiveDesignerType;
end;

function GxOtaActiveDesignerIsVCL: Boolean;
begin
  Result := (GxOtaGetActiveDesignerType = dVCL);
end;

function GxOtaActiveDesignerIsCLX: Boolean;
begin
  Result := (GxOtaGetActiveDesignerType = dCLX);
end;

function GxOtaActiveDesignerIsNFM: Boolean;
begin
  Result := (GxOtaGetActiveDesignerType = dNFM);
end;

function GxOtaActiveDesignerIsFMX: Boolean;
begin
  {$IFDEF GX_VER230_up}
  Result := (GxOtaGetActiveDesignerType = dFMX);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function GxOtaGetProjectPersonality(Project: IOTAProject): string;
begin
  Result := '';
  if Assigned(Project) then
  begin
    {$IFDEF GX_VER160_up} //Delphi 8+
    Result := Project.Personality;
    {$ELSE} // Delphi 7 or earlier
    if RunningCPPBuilder then
      Result := sCBuilderPersonality
    else
      Result := sDelphiPersonality;
    {$ENDIF}
  end;
end;

function GxOtaGetCurrentProjectPersonality: string;
begin
  Result := GxOtaGetProjectPersonality(GxOtaGetCurrentProject);
end;

function GxOtaProjectIsDotNet(Project: IOTAProject): Boolean;
begin
  Result := StringInArray(GxOtaGetProjectPersonality(Project), [sDelphiDotNetPersonality, sCSharpPersonality, sVBPersonality]);
end;

function GxOtaProjectIsDelphiDotNet(Project: IOTAProject): Boolean;
begin
  Result := SameText(GxOtaGetProjectPersonality(Project), sDelphiDotNetPersonality);
end;

function GxOtaProjectIsNativeCpp(Project: IOTAProject): Boolean;
begin
  Result := SameText(GxOtaGetProjectPersonality(Project), sCBuilderPersonality);
end;

function GxOtaProjectIsNativeDelphi(Project: IOTAProject): Boolean;
begin
  Result := SameText(GxOtaGetProjectPersonality(Project), sDelphiPersonality);
end;

function GxOtaProjectIsEitherDelphi(Project: IOTAProject): Boolean;
begin
  Result := StringInArray(GxOtaGetProjectPersonality(Project), [sDelphiPersonality, sDelphiDotNetPersonality]);
end;

function GxOtaProjectIsCSharp(Project: IOTAProject): Boolean;
begin
  Result := SameText(GxOtaGetProjectPersonality(Project), sCSharpPersonality);
end;

function GxOtaCurrentProjectIsDotNet: Boolean;
begin
  Result := GxOtaProjectIsDotNet(GxOtaGetCurrentProject);
end;

function GxOtaCurrentProjectIsDelphiDotNet: Boolean;
begin
  Result := GxOtaProjectIsDelphiDotNet(GxOtaGetCurrentProject);
end;

function GxOtaCurrentProjectIsNativeCpp: Boolean;
begin
  Result := GxOtaProjectIsNativeCpp(GxOtaGetCurrentProject);
end;

function GxOtaCurrentProjectIsNativeDelphi: Boolean;
begin
  Result := GxOtaProjectIsNativeDelphi(GxOtaGetCurrentProject);
end;

function GxOtaCurrentProjectIsEitherDelphi: Boolean;
begin
  Result := GxOtaProjectIsEitherDelphi(GxOtaGetCurrentProject);
end;

function GxOtaCurrentProjectIsCSharp: Boolean;
begin
  Result := GxOtaProjectIsCSharp(GxOtaGetCurrentProject);
end;

{$IFDEF GX_VER160_up}
function GxOtaGetPersonalityServices: IOTAPersonalityServices;
begin
  Result := PersonalityServices;
  if not Assigned(Result) then
    raise Exception.Create('Personality services not available');
end;

function GxOtaIdeSupportsPersonality(const Personality: string): Boolean;
var
  Personalities: IOTAPersonalityServices;
  i: Integer;
begin
  Result := False;
  Personalities := GxOtaGetPersonalityServices;
  for i := 0 to Personalities.PersonalityCount - 1 do begin
    if SameText(Personalities.Personalities[i], Personality) then begin
      Result := True;
      Break;
    end;
  end;
end;
{$ELSE Delphi 7-}
function GxOtaIdeSupportsPersonality(const Personality: string): Boolean;
begin
  if RunningCPPBuilder then
    Result := StringInArray(Personality, [sDelphiPersonality, sCBuilderPersonality])
  else
    Result := StringInArray(Personality, [sDelphiPersonality]);
end;
{$ENDIF}

function GxOtaHaveCPPSupport: Boolean;
begin
  Result := GxOtaIdeSupportsPersonality(sCBuilderPersonality);
end;

function GxOtaHaveDelphiSupport: Boolean;
begin
  Result := GxOtaIdeSupportsPersonality(sDelphiPersonality) or
    GxOtaIdeSupportsPersonality(sDelphiDotNetPersonality)
end;

function GxOtaHaveCSharpSupport: Boolean;
begin
  Result := GxOtaIdeSupportsPersonality(sCSharpPersonality);
end;

function GxOtaGetHexPrefix: string;
begin
  if StringInArray(GxOtaGetCurrentProjectPersonality, [sCBuilderPersonality, sCSharpPersonality]) then
    Result := '0x'
  else
    Result := '$';
end;

function GxOtaCurrentlyEditingForm: Boolean;
var
  Editor: IOTAEditor;
begin
  Result := False;
  Editor := GxOtaGetCurrentEditor;
  if Assigned(Editor) then
    Result := Supports(Editor, IOTAFormEditor);
end;

function GxOtaCurrentlyEditingSource: Boolean;
begin
  Result := IsIdeEditorForm(Screen.ActiveCustomForm) or IsEditControl(Screen.ActiveControl);
end;

function GxOtaGetCurrentIDEEditControl: TWinControl;
var
  EditView: IOTAEditView;
  EditWindow: INTAEditWindow;
  EditForm: TCustomForm;
begin
  Result := nil;
  EditView := GxOtaGetTopMostEditView;
  if Assigned(EditView) then
  begin
    EditWindow := EditView.GetEditWindow;
    if Assigned(EditWindow) then
    begin
      EditForm := EditWindow.Form;
      if Assigned(EditForm) then
        Result := GetIDEEditControl(EditForm);
    end;
  end;
end;

function GxOtaFocusCurrentIDEEditControl: Boolean;
var
  EditControl: TWinControl;
begin
  Result := False;
  EditControl := GxOtaGetCurrentIDEEditControl;
  TryFocusControl(EditControl)
end;

function GxOtaGetProjectFileName(Project: IOTAProject; NormalizeBdsProj: Boolean = False): string;

  function SearchProjectSourceViaModule(var AProjectFileName: string): Boolean;
  var
    i: Integer;
    Module: IOTAModule;
    Editor: IOTAEditor;
  begin
    Result := False;
    Module := Project as IOTAModule;
    for i := 0 to Module.ModuleFileCount - 1 do
    begin
      Editor := Module.ModuleFileEditors[i];
      if IsProjectSource(Editor.FileName) then
      begin
        Result := True;
        AProjectFileName := Editor.FileName;
        Exit;
      end;
    end;
  end;

  function SearchProjectSourceViaFileExt(var AProjectFileName: string): Boolean;
  var
    PackageFileName: string;
  begin
    Result := GxOtaProjectIsEitherDelphi(Project);
    if Result then
    begin
      AProjectFileName := ChangeFileExt(AProjectFileName, '.dpr');
      if not GxOtaFileOrModuleExists(AProjectFileName) then
      begin
        PackageFileName := ChangeFileExt(AProjectFileName, '.dpk');
        if GxOtaFileOrModuleExists(PackageFileName) then
          AProjectFileName := PackageFileName
        else
          Result := False;
      end;
    end;
  end;

begin
  Result := '';
  if Assigned(Project) then begin
    Result := Project.FileName;
    if NormalizeBdsProj and IsBdsprojOrDproj(Result) then begin
      // Use a two-step search to get the right dpr/dpk/... file.
      // First search the IOTAProject's module list for a file with the correct
      // extension.  If this doesn't work, replace the bdsproj/dproj file
      // extension with dpr or dpk.  The second search can give wrong results
      // if the *proj file and the project source file don't have the same base
      // name (e.g. Demo.dpr and DemoD11.dproj).
      if not SearchProjectSourceViaModule(Result) then
        SearchProjectSourceViaFileExt(Result);
    end;
  end;
end;

function GxOtaGetCurrentProjectFileName(NormalizeBdsProj: Boolean): string;
begin
  Result := GxOtaGetProjectFileName(GxOtaGetCurrentProject, NormalizeBdsProj);
end;

function GxOtaGetProjectGroup: IOTAProjectGroup;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  i: Integer;
begin
  Assert(Assigned(BorlandIDEServices));

  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(IModuleServices));

  Result := nil;
  for i := 0 to IModuleServices.ModuleCount - 1 do
  begin
    IModule := IModuleServices.Modules[i];
    if Supports(IModule, IOTAProjectGroup, Result) then
      Break;
  end;
end;

function GxOtaTryGetCurrentProject(out _Project: IOTAProject): Boolean;
var
  IProjectGroup: IOTAProjectGroup;
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  i: Integer;
begin
  Result := False;

  IProjectGroup := GxOtaGetProjectGroup;
  if not Assigned(IProjectGroup) then
  begin
    Assert(Assigned(BorlandIDEServices));
    IModuleServices := BorlandIDEServices as IOTAModuleServices;
    Assert(Assigned(IModuleServices));

    for i := 0 to IModuleServices.ModuleCount - 1 do
    begin
      IModule := IModuleServices.Modules[i];
      Result := Supports(IModule, IOTAProject, _Project);
      if Result then
        Break;
    end;
  end;

  try
    // This raises exceptions in D5 with .bat projects active
    if Assigned(IProjectGroup) and (not Result) then begin
      _Project := IProjectGroup.ActiveProject;
      Result := True;
    end;
  except
    // ignore
  end; //FI:W501 Empty except block
end;

function GxOtaGetCurrentProject: IOTAProject;
begin
  if not GxOtaTryGetCurrentProject(Result) then
    Result := nil;
end;

function GxOtaHaveCurrentProject: Boolean;
begin
  Result := GxOtaGetCurrentProject <> nil;
end;

function GxOtaGetCurrentProjectName: string;
var
  IProject: IOTAProject;
begin
  Result := '';

  IProject := GxOtaGetCurrentProject;
  if Assigned(IProject) then
  begin
    Result := ExtractFileName(IProject.FileName);
    Result := ChangeFileExt(Result, '');
  end;
end;

function GxOtaGetProjectOutputDir(Project: IOTAProject): string;
// taken from http://cc.embarcadero.com/item/19823
// (Ondrey Kelle)
begin
  if Project.ProjectOptions.Values[OPTION_NAME_GEN_PACKAGE] then begin
    // package project
    // use project options if specified
    Result := Project.ProjectOptions.Values[OPTION_NAME_PKG_DLL_DIR];
    // otherwise use environment options
    if Result = '' then
      Result := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions.Values['PackageDPLOutput'];
  end else begin
    // non-package project, use project options
    Result := Project.ProjectOptions.Values[OPTION_NAME_OUTPUT_DIR];
  end;

  // default is the project's path
  if Result = '' then
    Result := ExtractFilePath(Project.FileName);

  Result := IncludeTrailingPathDelimiter(Result);
end;


type
  TPathProcessor = class
  private
    FIdeBasePath: string;
    FConfigName: string;
    FPlatformName: string;
    FPrefix: string;
    FEnvironment: TStringList;
    class function ReplaceMacro(const Str, OldValue, NewValue: string): string;
  public
    ///<summary>
    /// @param Prefix will be used for relative paths
    /// @param Project will be used to determine Platform and Config, if not given, the
    ///                current project will be used. </summary>
    constructor Create(const _Prefix: string; _Project: IOTAProject = nil);
    destructor Destroy; override;
    function Process(const _Path: string): string;
    property PlatformName: string read FPlatformName write FPlatformName;
    property ConfigName: string read FConfigName write FConfigName;
  end;

{ TPathProcessor }

constructor TPathProcessor.Create(const _Prefix: string; _Project: IOTAProject = nil);

  procedure Init(_Project: IOTAProject);
{$IFDEF GX_VER230_up}
  begin
    if _Project = nil then
      _Project := GxOtaGetCurrentProject;
    if _Project <> nil then begin
      FPlatformName := _Project.CurrentPlatform;
      FConfigName := _Project.CurrentConfiguration;
    end;
  end;
{$ELSE}
  begin
    // do nothing
  end;
{$ENDIF}

begin
  inherited Create;
  FPrefix := _Prefix;
  Init(_Project);
  FIdeBasePath := RemoveSlash(GetIdeRootDirectory);
  FEnvironment := TStringList.Create;
  GetEnvironmentVariables(FEnvironment);
end;

destructor TPathProcessor.Destroy;
begin
  FreeAndNil(FEnvironment);
  inherited;
end;

function TPathProcessor.Process(const _Path: string): string;
const
  IDEBaseMacros: array [0..2] of string = ('BDS', 'DELPHI', 'BCB');
var
  i: Integer;
  EnvName: string;
  EnvValue: string;
begin
  Result := _Path;

  // Expand the IDE base folder names $([DELPHI,BCB,BDS])
  for i := Low(IDEBaseMacros) to High(IDEBaseMacros) do
    Result := ReplaceMacro(Result, IDEBaseMacros[i], FIdeBasePath);

  // Expand any environment variable macros
  for i := 0 to FEnvironment.Count - 1 do begin
    EnvName := FEnvironment.Names[i];
    EnvValue := FEnvironment.Values[EnvName];
    if (Trim(EnvName) <> '') and (Trim(EnvValue) <> '') then
      Result := ReplaceMacro(Result, EnvName, EnvValue);
  end;

  if FPlatformName <> '' then
    Result := ReplaceMacro(Result, 'Platform', FPlatformName);

  if FConfigName <> '' then
    Result := ReplaceMacro(Result, 'Config', FConfigName);

  if not IsPathAbsolute(Result) then begin
    if FPrefix <> '' then begin
      Result := TFileSystem.ExpandFileNameRelBaseDir(Result, FPrefix);
    end;
  end;
end;

class function TPathProcessor.ReplaceMacro(const Str, OldValue, NewValue: string): string;
var
  ReplaceVal: string;
begin
  ReplaceVal := '$(' + OldValue + ')';
  Result := StringReplace(Str, ReplaceVal, NewValue, [rfReplaceAll, rfIgnoreCase]);
end;

function GxOtaGetCurrentMapFileName(out MapFile: string): boolean;
type
  TExeType = (etExe, etDll, etBpl);
var
  Project: IOTAProject;
  TargetName: string;
{$IFNDEF GX_DELPHI2005_UP}
  OutputDir: string;
  ProjectFilename: string;
  PathProcessor: TPathProcessor;
  LibSuffix: string;
  LibPrefix: string;
  GenDLL: string;
  LibVersion: string;
  ExeType: TExeType;
  ExeExtension: string;
{$ENDIF}
begin
  Result := False;
  MapFile := '';
  Project := GxOtaGetCurrentProject;
  if not Assigned(Project) then begin
    // no project -> no .map file
    Exit; //==>
  end;

{$IFDEF GX_DELPHI2005_UP}
  // Delphi 2005 and up make this simple:
  // Just call Project.ProjectOptions.TargetName and replace the extension with .map
  TargetName := Project.ProjectOptions.TargetName;
{$ELSE}
  // Unfortunately Delphi 6 and 7 don't supply the TargetName property, so we try
  // to generate the file name ourselves.
  OutputDir := GxOtaGetProjectOutputDir(Project);
  ProjectFilename := GxOtaGetProjectFileName(Project, True);

  // There seems to be no way to get the actual Target File Extension (from the project options),
  // so we support only the most common ones here. Most likely this will also work
  // for .cpl and other library types, because nobody used the LibVersion setting outsid of the
  // Unix (Kylix) world, which we don't support here anyway.
  if IsPackage(ProjectFilename) then begin
    ExeType := etBpl;
    ExeExtension := '.bpl';
  end else begin
    GenDLL := Trim(VarToStr(Project.ProjectOptions.Values[OPTION_NAME_GEN_DLL]));
    if SameText(GenDLL, 'True') then begin
      ExeType := etDll;
      ExeExtension := '.dll';
    end else begin
      ExeType := etExe;
      ExeExtension := '.exe';
    end;
  end;
  TargetName := ExtractFileName(ProjectFilename);
  TargetName := TFileSystem.RemoveFileExtFull(TargetName);
  if exetype in [etDll, etBpl] then begin
    LibPrefix := Trim(VarToStr(Project.ProjectOptions.Values[OPTION_NAME_LIB_PREFIX]));
    LibSuffix := Trim(VarToStr(Project.ProjectOptions.Values[OPTION_NAME_LIB_SUFFIX]));
    LibVersion := Trim(VarToStr(Project.ProjectOptions.Values[OPTION_NAME_LIB_VERSION]));
    TargetName := LibPrefix + TargetName + LibSuffix + ExeExtension;
    if LibVersion <> '' then
      TargetName := TargetName + '.' + LibVersion;
  end;
  TargetName := AddSlash(OutputDir) + TargetName;

  PathProcessor := TPathProcessor.Create(ExtractFileDir(ProjectFilename), Project);
  try
    TargetName := PathProcessor.Process(TargetName);
  finally
    FreeAndNil(PathProcessor);
  end;
{$ENDIF}

  // Yes, this is correct: Delphi replaces the part after the last '.' with '.map'.
  // even for DLLs with a version suffix. So
  // project1.dll.1.0.0
  // becomes
  // project1.dll.1.0.map
  MapFile := TFileSystem.ChangeFileExtLast(TargetName, '.map');

  Result := FileExists(MapFile);
end;

function GxOtaGetProjectGroupFileName: string;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  IProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  Result := '';
  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  IProjectGroup := nil;
  for i := 0 to IModuleServices.ModuleCount - 1 do
  begin
    IModule := IModuleServices.Modules[i];
    if IModule.QueryInterface(IOTAProjectGroup, IProjectGroup) = S_OK then
      Break;
  end;
  // Delphi 5 does not return the file path when querying IOTAProjectGroup directly
  if IProjectGroup <> nil then
    Result := IModule.FileName;
end;

procedure GxOtaGetProjectFileNames(Project: IOTAProject; Files: TStrings);
var
  i: Integer;
begin
  Assert(Assigned(Files));
  Files.Clear;
  if not Assigned(Project) then
    Exit;
  for i := 0 to Project.GetModuleCount - 1 do
    if Trim(Project.GetModule(i).FileName) <> '' then
      Files.Add(Project.GetModule(i).FileName);
end;

function GxOtaGetFileNameOfCurrentModule: string;
var
  Module: IOTAModule;
begin
  Result := '';
  Module := GxOtaGetCurrentModule;
  if Assigned(Module) then
    Result := Module.FileName;
end;

function GxOtaTryGetProjectOptions(out _ProjectOptions: IOTAProjectOptions; _Project: IOTAProject = nil): Boolean;
begin
  if not Assigned(_Project) then
    _Project := GxOtaGetCurrentProject;
  if Assigned(_Project) then
    _ProjectOptions := _Project.GetProjectOptions;
  Result := Assigned(_ProjectOptions);
end;

function GxOtaGetActiveProjectOptions: IOTAProjectOptions;
begin
  if not GxOtaTryGetProjectOptions(Result) then
    Result := nil;
end;

function GxOtaTryGetProjectOption(const Option: string; var Value: Variant; _Project: IOTAProject = nil): Boolean;
var
  ProjectOptions: IOTAProjectOptions;

{$IFDEF GX_VER220_up} // RAD Studio XE 1 (16; BDS 8)
  // since Delphi XE there can be multiple configuration settings, we need to get the current one
  // because Embarcadero decided to not implement it for us.
  function HandleDelphiXEUpVersionInfo(Option: string; var Value: Variant): boolean;
  var
    ProjectConfigs: IOTAProjectOptionsConfigurations;
    cfg: IOTABuildConfiguration;
  begin
    Result := False;
    if Option = 'MajorVersion' then
      Option := 'VerInfo_MajorVer'
    else if Option = 'MinorVersion' then
      Option := 'VerInfo_MinorVer'
    else if Option = 'Release' then
      Option := 'VerInfo_Release'
    else if Option = 'Build' then
      Option := 'VerInfo_Build'
    else begin
      Exit; //==>
    end;
    ProjectConfigs := ProjectOptions as IOTAProjectOptionsConfigurations;
    if Assigned(ProjectConfigs) then begin
      cfg := ProjectConfigs.ActiveConfiguration;
      if Assigned(cfg) then begin
        Value := cfg.GetValue(Option, True);
        Result := True;
      end;
    end;
  end;
{$ELSE}
  function HandleDelphiXEUpVersionInfo(Option: string; var Value: Variant): boolean;
  begin
    Result := False;
  end;
{$ENDIF}

begin
  Result := False;
  Value := '';
  if GxOtaTryGetProjectOptions(ProjectOptions, _Project) then begin
    if HandleDelphiXEUpVersionInfo(Option, Value) then begin
      Result := True;
    end else begin
      Value := ProjectOptions.Values[Option];
      Result := True;
    end;
  end;
end;

function GxOtaGetVersionInfoKeysOption(const Option: string; var Value: Variant): Boolean;
var
  KeysStrings: TStrings;
  OptionValue: string;
begin
  Result := False;
  Value := Null;
  KeysStrings := TStringList.Create;
  try
    if GxOtaGetVersionInfoKeysStrings(KeysStrings) then
    begin
      OptionValue := KeysStrings.Values[Option];
      if OptionValue <> '' then
      begin
        Result := True;
        Value := OptionValue;
      end;
    end;
  finally
    FreeAndNil(KeysStrings);
  end;
end;

function GxOtaGetVersionInfoKeysOptions(const Options: array of string; var Value: Variant): Boolean; overload;
var
  i: Integer;
begin
  Result := False;
  for i := Low(Options) to High(Options) do
  begin
    Result := GxOtaGetVersionInfoKeysOption(Options[i], Value);
    if Result then
      Break;
  end;
end;

function GxOtaGetVersionInfoKeysStrings(Strings: TStrings): Boolean;
var
  VerStrings: TStrings;
{$IFDEF GX_VER230_up} // Delphi XE2 and up.
  ProjectOptions : IOTAProjectOptions;
  Configurations : IOTAProjectOptionsConfigurations;
  Configuration  : IOTABuildConfiguration;
  PropValue      : string;
{$ELSE}
  KeysValue: Variant;
  KeysInteger: Integer;
{$ENDIF}
begin
  // This has been changed to support Delphi XE2 and up which stores
  // version information in the configuration sets. Strings can no longer
  // be simply assigned but need to be passed into this function to be
  // set.
  Assert(Assigned(Strings));

  Result := False;

{$IFDEF GX_VER230_up} // Delphi XE2 and up.
  if  GxOtaTryGetProjectOptions(ProjectOptions) then
  begin
    if ProjectOptions.QueryInterface(IOTAProjectOptionsConfigurations, Configurations) = S_OK then
    begin
      Configuration := Configurations.ActiveConfiguration;
      if Assigned(Configuration) then
      begin
        if Configuration.PropertyExists('VerInfo_Keys') then
        begin
          PropValue := Configuration.GetValue('VerInfo_Keys', True);
          VerStrings := TStringList.Create;
          try
            VerStrings.Delimiter := ';';
            VerStrings.StrictDelimiter := True;
            VerStrings.DelimitedText := PropValue;
            Result := True;
          finally
            Strings.Clear;
            Strings.AddStrings(VerStrings);
            FreeAndNil(VerStrings);
          end;
        end;
      end;
    end;
  end;
{$ELSE}
  if GxOtaTryGetProjectOption('Keys', KeysValue) then
  begin
    if (not VarIsNull(KeysValue)) and (VarType(KeysValue) = varInteger) then
    begin
      KeysInteger := KeysValue;
      if TObject(KeysInteger) is TStrings then
      begin
        VerStrings := TStrings(KeysInteger);
        Strings.Assign(VerStrings);
        Result := True;
      end;
    end;
  end;
{$ENDIF}
end;

function GxOtaGetVersionInfoKeysString: string;
var
  Strings: TStrings;
begin
  Result := '';
  Strings := TStringList.Create;
  try
    if GxOtaGetVersionInfoKeysStrings(Strings) then
      Result := Strings.Text;
  finally
    FreeAndNil(Strings);
  end;
end;

function GxOtaSetVersionInfoKeysStrings(Strings: TStrings): Boolean;
var
  VerStrings: TStrings;
{$IFDEF GX_VER230_up} // Delphi XE2 and up.
  ProjectOptions : IOTAProjectOptions;
  Configurations : IOTAProjectOptionsConfigurations;
  Configuration  : IOTABuildConfiguration;
  PropValue      : string;
{$ELSE}
  KeysValue: Variant;
  KeysInteger: Integer;
{$ENDIF}
begin
  Assert(Assigned(Strings));

  Result := False;

{$IFDEF GX_VER230_up} // Delphi XE2 and up.
  ProjectOptions := GxOtaGetActiveProjectOptions;
  if Assigned(ProjectOptions) then
  begin
    if ProjectOptions.QueryInterface(IOTAProjectOptionsConfigurations, Configurations) = S_OK then
    begin
      Configuration := Configurations.ActiveConfiguration;
      if Assigned(Configuration) then
      begin
        VerStrings := TStringList.Create;
        try
          VerStrings.Assign(Strings);
          VerStrings.Delimiter := ';';
          VerStrings.StrictDelimiter := True;
          PropValue := VerStrings.DelimitedText;
          Configuration.SetValue('VerInfo_Keys', PropValue);
          Result := True;
        finally
          FreeAndNil(VerStrings);
        end;
      end;
    end;
  end;
{$ELSE}
  if GxOtaTryGetProjectOption('Keys', KeysValue) then
  begin
    if (not VarIsNull(KeysValue)) and (VarType(KeysValue) = varInteger) then
    begin
      KeysInteger := KeysValue;
      if TObject(KeysInteger) is TStrings then
      begin
        VerStrings := TStrings(KeysInteger);
        VerStrings.Assign(Strings);
        Result := True;
      end;
    end;
  end;
{$ENDIF}
end;

function GxOtaGetCurrentSelection(IncludeTrailingCRLF: Boolean): string;
var
  EditView: IOTAEditView;
  EditBlock: IOTAEditBlock;
begin
  Result := '';

  if not GxOtaTryGetTopMostEditView(EditView) then
    Exit;

  EditBlock := EditView.Block;
  if Assigned(EditBlock) then begin
    Result := IDEEditorStringToString(EditBlock.Text);
    Result := HackBadIDEUTF8StringToString(Result);

    if not IncludeTrailingCRLF and (EditBlock.Style in [btNonInclusive]) then
      RemoveLastEOL(Result);
  end;
end;

function GxOtaMoveEditCursorColumn(EditView: IOTAEditView; RelativePos: Integer): Boolean;
var
  CursorPos: TOTAEditPos;
begin
  Assert(Assigned(EditView));
  Result := False;
  CursorPos := EditView.CursorPos;
  if CursorPos.Col + RelativePos > 0 then
  begin
    CursorPos.Col := CursorPos.Col + RelativePos;
    EditView.CursorPos := CursorPos;
    Result := True;
  end;
end;

function GxOtaGetActiveEditorTextBytes(Stream: TStream; UseSelection: Boolean): Boolean;
var
  ISourceEditor: IOTASourceEditor;
  IEditView: IOTAEditView;
  IEditReader: IOTAEditReader;
  BlockSelText: string;
  HasSelection: Boolean;
begin
  Assert(Stream <> nil);

  Result := False;

  if not GxOtaTryGetCurrentSourceEditor(ISourceEditor) then
    Exit;

  if ISourceEditor.EditViewCount > 0 then
  begin
    IEditView := GxOtaGetTopMostEditView(ISourceEditor);
    Assert(IEditView <> nil);

    HasSelection := (IEditView.Block.Size > 0);
    if HasSelection then begin
      if UseSelection then begin
        BlockSelText := IDEEditorStringToString(IEditView.Block.Text);
        Stream.WriteBuffer(PAnsiChar(ConvertToIDEEditorString(BlockSelText))^, Length(BlockSelText) + SizeOf(Byte(0)));
        Result := True;
      end else begin
        // this is a workaround for an apparent bug in Delphi 7:
        // If a selection is active, IEditReader.GetText generates an Access Violation,
        // so we save the block, reset it and restore it after reading the text
        // This shouldn't cause problems for any other Delphi versions.
        IEditView.Block.Save;
        IEditView.Block.Reset;
      end;
    end;
    if not UseSelection or not HasSelection then begin
      IEditReader := ISourceEditor.CreateReader;
      GxOtaSaveReaderToStream(IEditReader, Stream);
      Result := False;
    end;
    if HasSelection then begin
      IEditView.Block.Restore;
    end;
  end;
end;

function GxOtaGetActiveEditorTextAsStringBytes(var Text: string; UseSelection: Boolean): Boolean;
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  try
    Result := GxOtaGetActiveEditorTextBytes(StringStream, UseSelection);
    Text := StringStream.DataString;
  finally
    FreeAndNil(StringStream);
  end;
end;

{$IFDEF GX_DELPHI8_UP}
// taken from http://stackoverflow.com/a/10388131/49925
function UTF8PosToCharIndex(const S: UTF8String; Index: Integer): Integer;
var
  I: Integer;
  P: PAnsiChar;
begin
  Result:= 0;
  if (Index <= 0) or (Index > Length(S)) then Exit;
  I:= 1;
  P:= PAnsiChar(S);
  while I <= Index do begin
    if Ord(P^) and $C0 <> $80 then Inc(Result);
    Inc(I);
    Inc(P);
  end;
end;

function CharIndexToUTF8Pos(const S: UTF8String; Index: Integer): Integer;
var
  P: PAnsiChar;
begin
  Result:= 0;
  P:= PAnsiChar(S);
  while (Result < Length(S)) and (Index > 0) do begin
    Inc(Result);
    if Ord(P^) and $C0 <> $80 then
      Dec(Index);
    Inc(P);
  end;
  if Index <> 0 then
    Result:= 0;  // char index not found
end;
{$ENDIF}

function GxOtaGetCurrentLineData(var StartOffset, ColumnNo, LineNo: Integer; ByteBased: Boolean): string;

  // Returns column, line number, and source of the active editor
  function GetCurrentSourceAndPos(var CursorPosition: Integer;
    var CursorLine: Integer; ByteBased: Boolean): string;
  var
    EditView: IOTAEditView;
    EditPos: TOTAEditPos;
    CharPos: TOTACharPos;
{$IFDEF GX_DELPHI8_UP}
    IdeString: UTF8String;
{$ENDIF}
  begin
    Result := '';
    CursorPosition := 0;

    EditView := GxOtaGetTopMostEditView;
    if Assigned(EditView) then
    begin
      // Hack for macro templates that want accurate byte counts instead of
      // accurately preserved characters.  This puts each UTF-8 byte in a
      // separate character in the result.
      if ByteBased then
        GxOtaGetActiveEditorTextAsStringBytes(Result, False)
      else
        GxOtaGetActiveEditorTextAsString(Result, False);
      EditPos := EditView.CursorPos;
      EditView.ConvertPos(True, EditPos, CharPos);
      CursorLine := CharPos.Line;
      CursorPosition := EditView.CharPosToPos(CharPos);
{$IFDEF GX_DELPHI8_UP}
      if not ByteBased then begin
        IdeString := ConvertToIDEEditorString(Result);
        CursorPosition := UTF8PosToCharIndex(IdeString, CursorPosition);
      end;
{$ENDIF}
    end;
  end;

var
  TotalSrc: string;
  Index: Integer;
  SrcLen, LineLen: Integer;
  CursorPos: Integer;
begin
  TotalSrc := GetCurrentSourceAndPos(CursorPos, LineNo, ByteBased);

  // Find the start of the line
  Index := CursorPos;
  while Index > 0 do
  begin
    if EOLSizeAtPos(TotalSrc, Index) > 0 then
      Break;
    Dec(Index);
  end;

  if Index < 0 then
    Index := 0;
  StartOffset := Index;

  // Search for the end of the line
  SrcLen := Length(TotalSrc);
  Index := CursorPos + 1;
  while Index < SrcLen do
  begin
    if EOLSizeAtPos(TotalSrc, Index) > 0 then
      Break;
    Inc(Index);
  end;

  LineLen := (Index - 1) - StartOffset;
  Result := Copy(TotalSrc, StartOffset + 1, LineLen);

  // The column number is the difference between the start-of-line offset
  // and the current-cursor-position offset, so it is zero-based
  ColumnNo := CursorPos - StartOffset;
end;

function GxOtaGetCurrentLine: string;
var
  StartOffset: Integer;
  ColumnNo: Integer;
  LineNo: Integer;
begin
  Result := GxOtaGetCurrentLineData(StartOffset, ColumnNo, LineNo)
end;

function GxOtaGetEditorLine(View: IOTAEditView; LineNo: Integer): UTF8String;
var
  Buffer: IOTAEditBuffer;
  LineStartByte: Integer;
  LineEndByte: Integer;
  Pos: TOTACharPos;
  LineData: AnsiString;
  Reader: IOTAEditReader;
  LineBytes: Integer;
begin
  Assert(Assigned(View));
  Result := '';
  Buffer := View.Buffer;
  if (LineNo > Buffer.GetLinesInBuffer) or (LineNo < 1) then
    Exit;
  Pos.CharIndex := 0;
  Pos.Line := LineNo;
  LineStartByte := View.CharPosToPos(Pos);
  Pos.CharIndex := MaxEditorCol;
  LineEndByte := View.CharPosToPos(Pos);
  Reader := Buffer.CreateReader;
  Assert(Assigned(Reader));
  LineBytes := LineEndByte - LineStartByte;
  SetLength(LineData, LineBytes);
  Reader.GetText(LineStartByte, PAnsiChar(LineData), LineBytes);
  Result := UTF8String(LineData);
end;

function GxOtaGetEditorLineAsString(View: IOTAEditView; LineNo: Integer): TGXUnicodeString;
begin
  {$IFDEF UNICODE} // Delphi 2009 +
  Result := string(GxOtaGetEditorLine(View, LineNo));
  {$ELSE}
    {$IFDEF GX_VER160_up} // Delphi 8+
    Result := UTF8Decode(GxOtaGetEditorLine(View, LineNo));
    {$ELSE} // Delphi 6-7
    Result := GxOtaGetEditorLine(View, LineNo);
    {$ENDIF}
  {$ENDIF}
end;

// TODO 3 -oAnyone -cCleanup: This claims to return TOTAEditPos values but really returns TOTACharPos values
procedure GxOtaGetCurrentIdentEx(var Ident: string; var IdentOffset: Integer;
  var StartPos: TOTAEditPos; var CurrentPos: TOTAEditPos; var AfterLen: Integer; ByteBased: Boolean);

  function IsEndOfWordChar(Char: Char): Boolean;
  begin
    if IsCharIdentifier(Char) then
      Result := False
    else
      Result := True;
  end;

var
  CurrentLine: string;
  IdentName, Tmp: string;
  IdentLen: Integer;
  HeadLen, WhiteCnt: Integer;
  CheckIdx: Integer;
  TailStartIdx: Integer;
  ColumnNo: Integer;
  StartOffset: Integer;
  LineNo: Integer;
  TailLen: Integer;
begin
  Ident := '';
  CurrentLine := GxOtaGetCurrentLineData(StartOffset, ColumnNo, LineNo, ByteBased);

  CurrentPos.Col := ColumnNo;
  CurrentPos.Line := LineNo;
  IdentOffset := -1;

  StartPos.Line := LineNo;
  StartPos.Col := 0;

  // The index of first character after the cursor
  TailStartIdx := ColumnNo + 1;

  // Search how many characters of Ident name are after cursor (TailLen)
  // ColumnNo is zero-based
  CheckIdx := TailStartIdx;
  while CheckIdx <= Length(CurrentLine) do
  begin
    if IsEndOfWordChar(CurrentLine[CheckIdx]) then
      Break;
    Inc(CheckIdx); // Move to next character
  end;

  // TailLen is the number of additional chars after the cursor pos
  TailLen := CheckIdx - TailStartIdx; // char after end - first char

  Tmp := Copy(CurrentLine, 1, ColumnNo + TailLen);

  // Search how many characters are for Ident name before cursor
  CheckIdx := TailStartIdx - 1; // character before cursor
  while CheckIdx > 0 do
  begin
    if IsEndOfWordChar(Tmp[CheckIdx]) then
      Break;
    Dec(CheckIdx); // Move to next character
  end;

  HeadLen := (TailStartIdx - 1) - CheckIdx;
  IdentOffset := StartOffset + CheckIdx;

  WhiteCnt := IdentOffset - StartOffset;
  // The number of characters before the identifier
  IdentLen := HeadLen + TailLen;

  IdentName := Copy(Tmp, WhiteCnt + 1, IdentLen);

  Ident := Trim(IdentName);

  if Ident = '' then // No identifier is present, but the cursor position can be returned
  begin
    if Length(CurrentLine) < ColumnNo then
    begin  // The line is too short to hold the current position
      IdentOffset := StartOffset;
      StartPos.Col := 0;
    end
    else
    begin // The line has enough characters
      IdentOffset := StartOffset + ColumnNo;
      StartPos.Col := ColumnNo;
    end;
  end
  else
    StartPos.Col := ColumnNo - HeadLen;

  Inc(StartPos.Col);
  AfterLen := TailLen;
end;

function GxOtaGetCurrentIdentData(var Ident: string; var StartEditPos: TOTAEditPos): Boolean;
var
  IdentOffset: Integer;
  StartPos: TOTACharPos;
  EStart: TOTAEditPos;
  ECurrent: TOTAEditPos;
  AfterLen: Integer;
  EditView: IOTAEditView;
begin
  // This claims to return TOTAEditPos, but actually returns TOTACharPos in disguise
  GxOtaGetCurrentIdentEx(Ident, IdentOffset, EStart, ECurrent, AfterLen);
  StartEditPos.Col := 0;
  StartEditPos.Line := 0;
  Result := Ident <> '';
  if Result then
  begin
    StartPos.CharIndex := EStart.Col - 1;
    StartPos.Line := EStart.Line;
    EditView := GxOtaGetTopMostEditView;
    Assert(Assigned(EditView));
    EditView.ConvertPos(False, StartEditPos, StartPos);
  end;
end;

function GxOtaGetCurrentIdent: string;
var
  IdentOffset: Integer;
  StartPos: TOTAEditPos;
  CurrentPos: TOTAEditPos;
  AfterLen: Integer;
begin
  GxOtaGetCurrentIdentEx(Result, IdentOffset, StartPos, CurrentPos, AfterLen);
end;

(* // A start at making this support the UTF-8 returned by the editor
function GxOtaGetCurrentIdent: string;
var
  Line: TGXUnicodeString;
  EditView: IOTAEditView;
  HaveIdent: Boolean;
  IdentStart: Integer;
  IdentEnd: Integer;
  EditPos: TOTAEditPos;
  CharPos: TOTACharPos;
begin
  Result := '';
  EditView := GxOtaGetTopmostEditView;
  if Assigned(EditView) then
  begin
    Line := GxOtaGetEditorLineAsString(EditView, EditView.Position.Row);
    if Length(Line) > 0 then
    begin
      EditPos := EditView.CursorPos;
      EditView.ConvertPos(True, EditPos, CharPos);
      IdentStart := EnforceMinMax(CharPos.CharIndex + 1, 1, Length(Line));
      HaveIdent := IsCharIdentifier(Line[IdentStart]) or ((IdentStart > 1) and (IsCharIdentifier(Line[IdentStart - 1])));
      if HaveIdent then
      begin
        if (not IsCharIdentifier(Line[IdentStart])) and ((IdentStart > 1) and (IsCharIdentifier(Line[IdentStart - 1]))) then
          Dec(IdentStart);
        IdentEnd := IdentStart;
        while (IdentStart > 1) and (IsCharIdentifier(Line[IdentStart - 1])) do
          Dec(IdentStart);
        while (IdentEnd < Length(Line)) and (IsCharIdentifier(Line[IdentEnd])) and (IsCharIdentifier(Line[IdentEnd + 1])) do
          Inc(IdentEnd);
        Result := Copy(Line, IdentStart, IdentEnd - IdentStart + 1);
        Result := Trim(Result);
      end;
    end;
  end;
end;
*)

procedure GxOtaReplaceEditorText(SourceEditor: IOTASourceEditor; const AText: string);
var
  Writer: IOTAEditWriter;
begin
  Assert(Assigned(SourceEditor));
  Writer := SourceEditor.CreateUndoableWriter;
  if not Assigned(Writer) then
    raise Exception.Create('No edit writer');
  Writer.DeleteTo(MaxLongint);
  Writer.Insert(PAnsiChar(ConvertToIDEEditorString(AText)));
  Writer := nil;
end;

procedure GxOtaReplaceEditorTextWithUnicodeString(SourceEditor: IOTASourceEditor; Text: TGXUnicodeString);
var
  Writer: IOTAEditWriter;
begin
  Assert(Assigned(SourceEditor));
  Writer := SourceEditor.CreateUndoableWriter;
  if not Assigned(Writer) then
    raise Exception.Create('No edit writer');
  Writer.DeleteTo(MaxLongint);
  Writer.Insert(PAnsiChar(ConvertToIDEEditorString(Text)));
  Writer := nil;
end;

function GxOtaCompareEditPos(AEditPos1, AEditPos2: TOTAEditPos): Integer;
begin
  if (AEditPos1.Line > AEditPos2.Line) or
    ((AEditPos1.Line = AEditPos2.Line) and (AEditPos1.Col > AEditPos2.Col))
  then
    Result := 1
  else if (AEditPos1.Line = AEditPos2.Line) and (AEditPos1.Col = AEditPos2.Col) then
    Result := 0
  else
    Result := -1;
end;

procedure GxOtaSelectBlock(const Editor: IOTASourceEditor; const Start, After: TOTACharPos);
begin
  Assert(Assigned(Editor));
  Editor.BlockVisible := False;
  try
    Editor.BlockType := btNonInclusive;
    Editor.BlockStart := Start;
    Editor.BlockAfter := After;
  finally
    Editor.BlockVisible := True;
  end;
end;

procedure GxOtaSelectBlock(const Editor: IOTASourceEditor; Start, After: TOTAEditPos); overload;
var
  EditView: IOTAEditView;
begin
  Assert(Assigned(Editor));
  EditView := GxOtaGetTopmostEditView(Editor);
  Assert(Assigned(EditView));

  EditView.CursorPos := Start;
  EditView.Block.BeginBlock;
  EditView.CursorPos := After;
  EditView.Block.EndBlock;
  if not EditView.Block.IsValid then begin
    // this is a work  around for the case that the After position is beyond
    // the end of file
    EditView.CursorPos := Start;
    EditView.Block.BeginBlock;
    After.Line := After.Line - 1;
    After.Col := MaxEditorCol;
    EditView.CursorPos := After;
    EditView.Block.EndBlock;
  end;
  EditView.Block.SetVisible(True);
  EditView.Paint;
end;

function GxOtaGetSelection(const EditView: IOTAEditView;
  var BlockStart, BlockEnd: TOTAEditPos; var SelStart, SelLength: Integer): Boolean;
var
  UseView: IOTAEditView;
  Block: IOTAEditBlock;
  CharPos: TOTACharPos;
begin
  Result := False;
  if Assigned(EditView) then
    UseView := EditView
  else
    UseView := GxOtaGetTopmostEditView;
  if Assigned(UseView) then
  begin
    Block := UseView.Block;
    if Assigned(Block) and (Block.Size > 0) then
    begin
      BlockStart.Col := Block.StartingColumn;
      BlockStart.Line := Block.StartingRow;
      BlockEnd.Col := Block.EndingColumn;
      BlockEnd.Line := Block.EndingRow;

      if GxOtaCompareEditPos(BlockStart, BlockEnd) < 0 then
        UseView.ConvertPos(True, BlockStart, CharPos)
      else
        UseView.ConvertPos(True, BlockEnd, CharPos);

      SelStart := UseView.CharPosToPos(CharPos);
      SelLength := Block.Size;

      Result := True;
    end;
  end;
end;

procedure GxOtaUnfoldAllRegions(const View: IOTAEditView);
{$IFDEF GX_VER160_up}
var
  ElideActions: IOTAElideActions;
begin
  Assert(Assigned(View));
  Assert(Assigned(View.Buffer));
  if IsBdsSourceFile(View.Buffer.FileName) then // SQL files, for example, can raise an exception below
    if Supports(View, IOTAElideActions, ElideActions) then
      ElideActions.UnElideAllBlocks;
end;
{$ELSE}
begin //FI:W519
end;
{$ENDIF}

procedure GxOtaUnfoldNearestRegion(const View: IOTAEditView);
{$IFDEF GX_VER160_up}
var
  ElideActions: IOTAElideActions;
begin
  Assert(Assigned(View));
  Assert(Assigned(View.Buffer));
  if IsBdsSourceFile(View.Buffer.FileName) then // SQL files, for example, can raise an exception below
    if Supports(View, IOTAElideActions, ElideActions) then
      ElideActions.UnElideNearestBlock;
end;
{$ELSE}
begin //FI:W519
end;
{$ENDIF}

function GxOtaDeleteSelection(const EditView: IOTAEditView): Boolean;
var
  UseView: IOTAEditView;
  Block: IOTAEditBlock;
begin
  Result := False;
  UseView := EditView;
  if not Assigned(UseView) then
    UseView := GxOtaGetTopmostEditView;
  if Assigned(UseView) then
  begin
    Block := UseView.Block;
    if Assigned(Block) then
    begin
      Block.Delete;
      Result := True;
    end;
  end;
end;

function GxOtaCloseFile(const FileName: string): Boolean;
var
  ActionServices: IOTAActionServices;
begin
  ActionServices := BorlandIDEServices as IOTAActionServices;
  Assert(Assigned(ActionServices));

  Result := ActionServices.CloseFile(FileName);
end;

procedure GxOtaCloseCurrentEditorTab;
var
  EditActions: IOTAEditActions;
begin
  EditActions := GxOtaGetEditActions;
  if Assigned(EditActions) then
    EditActions.ClosePage;
end;

function GxOtaOpenFile(const FileName: string): Boolean;
var
  ActionServices: IOTAActionServices;
  hWndSaved: HWND;
begin
  ActionServices := BorlandIDEServices as IOTAActionServices;
  Assert(Assigned(ActionServices));

  hWndSaved := GetForegroundWindow;
  Result := ActionServices.OpenFile(FileName);
  if hWndSaved <> 0 then
    SetForegroundWindow(hWndSaved);
end;

function GxOtaOpenFileOrForm(const FileName: string): Boolean;
begin
  if IsForm(FileName) then begin
    Result := GxOtaOpenFile(GxOtaGetBaseModuleFileName(FileName));
    GxOtaShowFormForModule(GxOtaGetCurrentModule);
  end
  else
    Result := GxOtaOpenFile(FileName);
end;

function GxOtaIsFileOpen(const AFileName: string; UseBase: Boolean): Boolean;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  FileEditor: IOTAEditor;
  i: Integer;
  FileName: string;
begin
  Result := False;

  if IsStandAlone then
    Exit;

  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));
  FileName := AFileName;
  if UseBase then
    FileName := GxOtaGetBaseModuleFileName(FileName);

  Module := ModuleServices.FindModule(FileName);
  if Assigned(Module) then
  begin
    for i := 0 to Module.GetModuleFileCount-1 do
    begin
      FileEditor := GxOtaGetFileEditorForModule(Module, i);
      Assert(Assigned(FileEditor));

      Result := SameFileName(FileName, FileEditor.FileName);
      if Result then
        Exit;
    end;
  end;
end;

function GxOtaFileOrModuleExists(const AFileName: string; UseBase: Boolean): Boolean;
begin
  Result := FileExists(AFileName) or GxOtaIsFileOpen(AFileName, UseBase);
end;

function GxOtaMakeSourceVisible(const FileName: string): Boolean;
var
  EditActions: IOTAEditActions;
  Module: IOTAModule;
  FormEditor: IOTAFormEditor;
  SourceEditor: IOTASourceEditor;
  FileEditor: IOTAEditor;
  i: Integer;
  BaseFileName: string;
begin
  BaseFileName := GxOtaGetBaseModuleFileName(FileName);
  Module := GxOtaGetModule(BaseFileName);
  if Module = nil then
    Module := GxOtaGetModule(FileName);
  if Module = nil then
    Module := GxOtaGetModule(ChangeFileExt(FileName, '.dfm'));

  if Module <> nil then
  begin
    if IsForm(FileName) then
    begin
      if not GxOtaModuleIsShowingFormSource(Module) then
      begin
        SourceEditor := GxOtaGetSourceEditorFromModule(Module, BaseFileName);
        if Assigned(SourceEditor) then
          SourceEditor.Show;
        SourceEditor := nil;
        EditActions := GxOtaGetEditActionsFromModule(Module);
        if EditActions <> nil then
        begin
          FormEditor := GxOtaGetFormEditorFromModule(Module);
          FormEditor.Show;
          EditActions.SwapSourceFormView;
        end;
      end
    end
    else // We are focusing a regular text file, not a form
    begin
      if GxOtaModuleIsShowingFormSource(Module) then
      begin
        SourceEditor := GxOtaGetSourceEditorFromModule(Module);
        if Assigned(SourceEditor) then
          SourceEditor.Show;
        SourceEditor := nil;
        EditActions := GxOtaGetEditActionsFromModule(Module);
        if EditActions <> nil then
          EditActions.SwapSourceFormView;
      end;
    end;
  end;

  // D5/BDS 2006 sometimes delay opening the file until messages are processed
  Application.ProcessMessages;

  if not (GxOtaIsFileOpen(BaseFileName) or GxOtaIsFileOpen(FileName)) then
    Result := GxOtaOpenFile(FileName)
  else
    Result := True;

  if Result then
  begin
    {$IFDEF VER160}
    // Delphi 8 can not open both the module and the form text, so stop here
    if IsForm(FileName) then
      Exit;
    {$ENDIF VER160}
    Module := GxOtaGetModule(BaseFileName);
    if Module = nil then
      Module := GxOtaGetModule(FileName);
    if Module <> nil then
    begin
      for i := 0 to Module.GetModuleFileCount-1 do
      begin
        FileEditor := Module.GetModuleFileEditor(i);
        Assert(Assigned(FileEditor));

        if SameFileName(FileEditor.FileName, FileName) then
        begin
          FileEditor.Show;
          Exit;
        end;
      end;
    end;
    Result := False;
  end;
end;

function GxOtaModuleIsShowingFormSource(Module: IOTAModule): Boolean;
var
  Editor: IOTAEditor;
  i: Integer;
begin
  Result := False;
  if not Assigned(Module) then
    Exit;
  for i := 0 to Module.GetModuleFileCount - 1 do
  begin
    Editor := Module.ModuleFileEditors[i];
    if Assigned(Editor) and IsForm(Editor.FileName) and Supports(Editor, IOTASourceEditor) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure GxOtaGoToFileLine(const FileName: string; Line: Integer);
var
  EditRead: TEditReader;
begin
  if not GxOtaIsFileOpen(FileName) then
    if not GxOtaOpenFile(FileName) then
      raise Exception.Create('Error opening ' + FileName);

  if Line < 0 then
    Exit;

  EditRead := TEditReader.Create(FileName);
  try
    EditRead.GotoLine(Line);
  finally
    FreeAndNil(EditRead);
  end;
end;

procedure GxOtaGoToFileLineColumn(const FileName: string; Line: Integer; StartColumn: Integer = 0; StopColumn: Integer = 0; ShowInMiddle: Boolean = True);
var
  EditView: IOTAEditView;
  Module: IOTAModule;
  SourceEditor: IOTASourceEditor;
  CurPos: TOTAEditPos;
  CharPos: TOTACharPos;
  EditPos: TOTAEditPos;
  MatchLength: Integer;
  LineData: UTF8String;
resourcestring
  SCouldNotOpenFile = 'Could not open file %s';
begin
  // Force the source editor to show the right file (cpp, pas, dfm, xfm, etc.)
  if not GxOtaMakeSourceVisible(FileName) then
    raise Exception.CreateFmt(SCouldNotOpenFile, [FileName]);

  Module := GxOtaGetModule(FileName);
  if not Assigned(Module) then
    Exit;

  SourceEditor := GxOtaGetSourceEditorFromModule(Module, FileName);
  if not Assigned(SourceEditor) then
    Exit;

  EditView := GxOtaGetTopMostEditView(SourceEditor);
  if not Assigned(EditView) then
    Exit;

  SourceEditor.Show;
  EditView.GetEditWindow.Form.Update;
  EditView.Block.SetVisible(False);

  // Set the top line of the edit view
  CurPos.Col := 1;
  CurPos.Line := Line;
  if ShowInMiddle then
    CurPos.Line := CurPos.Line - (EditView.ViewSize.cy div 2);
  if CurPos.Line < 1 then
   CurPos.Line := 1;
  EditView.TopPos := CurPos;

  Application.ProcessMessages;

  GxOtaFocusCurrentIDEEditControl;

  if RunningDelphi8OrGreater then
  begin
    LineData := GxOtaGetEditorLine(EditView, Line);
    StartColumn := GxOtaConvertColumnCharsToBytes(LineData, StartColumn, False);
    StopColumn := GxOtaConvertColumnCharsToBytes(LineData, StopColumn, True);
  end;

  // Position the cursor to the line and column of the match
  CharPos.CharIndex := StartColumn - 1;
  CharPos.Line := Line;
  EditView.ConvertPos(False, EditPos, CharPos);
  EditView.CursorPos := EditPos;
  // This is disabled since it causes crashes in D2007 jumping to matches in data modules with no opened project
  // inside EdScript.TOTAEditView.unElideNearestBlock
  //GxOtaUnfoldNearestRegion(EditView);
  EditView.CursorPos := EditPos;
  EditView.Block.BeginBlock;
  MatchLength := StopColumn - StartColumn + 1;
  // This calculation doesn't work when there are tabs inside the match text (rare)
  EditPos.Col := EditPos.Col + MatchLength;
  EditView.CursorPos := EditPos;
  EditView.Block.EndBlock;
  EditView.Block.SetVisible(True);
  EditView.Paint;
end;

function GxOtaConvertColumnCharsToBytes(LineData: UTF8String; CharIndex: Integer; EndByte: Boolean): Integer;
var
  UString: TGxUnicodeString;
  FinalUChar: TGXUnicodeString;
  UTF8Str: UTF8String;
begin
  if RunningDelphi8OrGreater then
  begin
    UString := UTF8ToUnicodeString(LineData);
    UString := Copy(UString, 1, CharIndex);
    UTF8Str := UTF8String(UTF8Encode(UString));
    Result := Length(UTF8Str);
    if not EndByte then
    begin
      if Length(UString) = 0 then
        Result := 0
      else
      begin
        FinalUChar := UString[Length(UString)];
        UTF8Str := Utf8String(UTF8Encode(FinalUChar));
        Result := Result - (Length(UTF8Str)) + 1;
      end;
    end;
  end
  else
    Result := CharIndex;
end;

function GxOtaConvertColumnBytesToChars(LineData: UTF8String; ByteIndex: Integer): Integer;
var
  UString: TGxUnicodeString;
  UTF8Str: UTF8String;
begin
  if RunningDelphi8OrGreater then
  begin
    UTF8Str := Copy(UTF8Str, 1, ByteIndex);
    UString := UTF8ToUnicodeString(UTF8Str);
    Result := Length(UString);
  end
  else
    Result := ByteIndex;
end;

function GxOtaGetBaseModuleFileName(const FileName: string): string;
var
  AltName: string;
begin
  Result := FileName;
  if IsForm(FileName) then
  begin
    if GxOtaHaveCPPSupport then
    begin
      AltName := ChangeFileExt(FileName, '.cpp');
      if GxOtaFileOrModuleExists(AltName) then
        Result := AltName;
    end;
    AltName := ChangeFileExt(FileName, '.pas');
    if GxOtaFileOrModuleExists(AltName) then
      Result := AltName;
  end;
end;

function GxOtaGetEnvironmentOptions: IOTAEnvironmentOptions;
begin
  Result := GxOtaGetIDEServices.GetEnvironmentOptions;
  Assert(Assigned(Result));
end;

function GxOtaGetIdeEnvironmentString(const EnvironmentStringName: string): string;
begin
  Result := GxOtaGetEnvironmentOptions.Values[EnvironmentStringName];
end;

procedure GxOtaGetIdeEnvironmentStrings(Settings: TStrings);
var
  EnvOptions: IOTAEnvironmentOptions;
  i: Integer;
  Options: TOTAOptionNameArray;
begin
  EnvOptions := GxOtaGetEnvironmentOptions;

  Settings.Clear;
  Options := EnvOptions.GetOptionNames; // Broken in Delphi 2005
  for i := 0 to Length(Options) - 1 do
    Settings.Add(Options[i].Name);
end;

function GxOtaGetIdeLibraryPath: string;
begin
   // Do not localize.
  if RunningBDS2006OrGreater and GxOtaCurrentProjectIsDelphiDotNet then
    Result := GxOtaGetIdeEnvironmentString('DotNetLibraryPath')
  else if RunningBDS2006OrGreater and GxOtaCurrentProjectIsNativeCpp then
    Result := GxOtaGetIdeEnvironmentString('CppSearchPath')
  else
    Result := GxOtaGetIdeEnvironmentString('LibraryPath');
end;

function GxOtaGetIdeBrowsingPath: string;
begin
   // Do not localize.
  if RunningBDS2006OrGreater and GxOtaCurrentProjectIsDelphiDotNet then
    Result := GxOtaGetIdeEnvironmentString('DotNetBrowsingPath')
  else if RunningBDS2006OrGreater and GxOtaCurrentProjectIsNativeCpp then
    Result := GxOtaGetIdeEnvironmentString('CppBrowsingPath')
  else if (not RunningBDS2006OrGreater) or GxOtaCurrentProjectIsNativeDelphi then
    Result := GxOtaGetIdeEnvironmentString('BrowsingPath')
  else
    Result := '';
end;

procedure SplitIdePath(Strings: TStrings; IdePathString: string);
var
  PathItem: string;
  SemicolonPos: Integer;
begin
  SemicolonPos := Pos(';', IdePathString);
  while SemicolonPos > 0 do
  begin
    PathItem := Trim(Copy(IdePathString, 1, SemicolonPos - 1));
    if PathItem <> '' then
      EnsureStringInList(Strings, PathItem);
    Delete(IdePathString, 1, SemicolonPos);
    SemicolonPos := Pos(';', IdePathString);
  end;
  IdePathString := Trim(IdePathString);
  if IdePathString <> '' then
    EnsureStringInList(Strings, IdePathString);
end;

function GxOtaGetPlatformCaption(const _Platform: string): string;
begin
  if _Platform = '' then
    Result := sAllPlatforms
  else begin
    if _Platform = cWin32Platform then
      Result := '32-bit Windows'
    else if _Platform = cWin64Platform then
      Result := '64-bit Windows'
    else if _Platform = cWinIoT32Platform then
      Result := '32-bit Windows IoT'
    else if _Platform = cWinARMPlatform then
      Result := 'Windows on ARM'
    else if _Platform = cOSX32Platform then
      Result := '32-bit OSX'
    else if _Platform = cOSX64Platform then
      Result := '64-bit OSX'
    else if _Platform = cLinux32Platform then
      Result := '32-bit Linux'
    else if _Platform = cLinux64Platform then
      Result := '64-bit Linux'
    else if _Platform = cLinux32ARMPlatform then
      Result := '32-bit Linux on ARM'
    else if _Platform = cLinux64ARMPlatform then
      Result := '64-bit Linux on ARM'
    else if _Platform = ciOSSimulatorPlatform then
      Result := 'iOS Simulator'
    else if _Platform = ciOSDevice32Platform then
      Result := '32-bit iOS'
    else if _Platform = ciOSDevice64Platform then
      Result := '64-bit iOS'
    else if _Platform = cAndroidPlatform then
      Result := '32-bit Android'
    else if _Platform = cAndroid64Platform then
      Result := '64-bit Android'
    else
      Result := _Platform;
    Result := Result + sPlatform;
  end;
end;

function GxOtaGetProjectPlatform(Project: IOTAProject = nil): string;
begin
  Result := '';
{$IFDEF GX_VER230_up} // RAD Studio XE 2 (16; BDS 9)
  if Project = nil then
    Project := GxOtaGetCurrentProject;
  if Project <> nil then
    Result := Project.CurrentPlatform;
{$ENDIF GX_VER230_up}
end;

function GxOtaGetProjectConfig(Project: IOTAProject = nil): string;
begin
  Result := '';
{$IFDEF GX_VER230_up} // RAD Studio XE 2 (16; BDS 9)
  if Project = nil then
    Project := GxOtaGetCurrentProject;
  if Project <> nil then
    Result := Project.CurrentConfiguration;
{$ENDIF GX_VER230_up}
end;

function GxOtaGetProjectFrameworkType(Project: IOTAProject = nil): string;
begin
{$IFDEF GX_VER230_up} // RAD Studio XE 2 (16; BDS 9)
  if Project = nil then
    Project := GxOtaGetCurrentProject;
  Result := Project.FrameworkType;
{$ELSE ~ GX_VER230_up}
  Result := '';
{$ENDIF GX_VER230_up}
end;

procedure ProcessPaths(Paths: TStrings; const Prefix: string; const PlatformName: string);
var
  i: Integer;
  PathItem: string;
  PathProcessor: TPathProcessor;
begin
  PathProcessor := TPathProcessor.Create(Prefix);
  try
    // todo: What about ConfigName?
    PathProcessor.PlatformName := PlatformName;
    for i := 0 to Paths.Count - 1 do begin
      PathItem := Paths[i];
      PathItem := PathProcessor.Process(PathItem);
      Paths[i] := PathItem;
    end;
  finally
    FreeAndNil(PathProcessor);
  end;
end;

procedure GxOtaGetProjectNamespaces(var DefaultNamespace: string; SearchPath: TStrings;
  Project: IOTAProject = nil);
var
  ProjectName: string;
  p: Integer;
  ProjectNS: string;
  ProjectOptions: IOTAProjectOptions;
  ProjectNamespaces: string;
  Framework: string;
//  OptionNames: TOTAOptionNameArray;
//  i: Integer;
begin
  Assert(Assigned(SearchPath));
  SearchPath.Clear;
  DefaultNamespace := '';
  if Project = nil then
    Project := GxOtaGetCurrentProject;
  if Assigned(Project) then begin
    ProjectName := ExtractFilename(Project.FileName);
    // remove .dpr/dproj
    ProjectName := ChangefileExt(ProjectName, '');
    // Get the project's namespace (if any)
    p := Pos('.', ProjectName);
    if p > 0 then begin
      ProjectNS := Copy(ProjectName, 1, p-1);
      // Add the current project namespace
      EnsureStringInList(SearchPath, ProjectName);
    end;
    // Then the project search path
    if GxOtaTryGetProjectOptions(ProjectOptions, Project) then begin

//      OptionNames := ProjectOptions.GetOptionNames;
//      for i := Low(OptionNames) to High(OptionNames) - 1 do
//        SendDebug(OptionNames[i].Name);
      DefaultNamespace := ProjectOptions.Values[OPTION_NAME_DEFAULT_NAMESPACE];
      ProjectNamespaces := ProjectOptions.Values[OPTION_NAME_NAMESPACE_PREFIX];
      SplitIdePath(SearchPath, ProjectNamespaces);

      // For FMX oddly the NamespacePrefix does not include 'FMX', so if the
      // prefix is removed, the project no longer compiles.
      // to prevent this, we add the framework type as the last entry to the
      // name spaces.
      Framework := GxOtaGetProjectFrameworkType;
      if Framework <> '' then
        EnsureStringInList(SearchPath, Framework);
    end;
  end;
end;

procedure GxOtaGetProjectSourcePathStrings(Paths: TStrings;
  Project: IOTAProject = nil; DoProcessing: Boolean = True);
var
  IdePathString: string;
  ProjectOptions: IOTAProjectOptions;
  ProjectDir: string;
  i: Integer;
  PlatformName: string;
begin
  Assert(Assigned(Paths));
  Paths.Clear;
  if Project = nil then
    Project := GxOtaGetCurrentProject;
  if Assigned(Project) then
  begin
    ProjectDir := ExtractFileDir(Project.FileName);
    // Add the current project directory first
    Paths.Add(ProjectDir);
    // Then the project search path
    if GxOtaTryGetProjectOptions(ProjectOptions, Project) then
    begin
      IdePathString := ProjectOptions.Values[OPTION_NAME_UNIT_SEARCH_PATH];
      // it is also possible to set the search path using the same property:
      SplitIdePath(Paths, IdePathString);
    end;
    if DoProcessing then begin
      PlatformName := GxOtaGetProjectPlatform(Project);
      ProcessPaths(Paths, ProjectDir, PlatformName);
    end;
    for i := 0 to Paths.Count - 1 do begin
      Paths[i] := AddSlash(Paths[i]);
    end;
  end;
end;

function GxOtaTryGetProjectSearchPath(out _Path: string; _Project: IOTAProject = nil): Boolean;
var
  ProjectOptions: IOTAProjectOptions;
begin
  Result := GxOtaTryGetProjectOptions(ProjectOptions, _Project);
  if Result then begin
    _Path := ProjectOptions.Values[OPTION_NAME_UNIT_SEARCH_PATH];
  end;
end;

function GxOtaTrySetProjectSearchPath(const _Path: string; _Project: IOTAProject = nil): Boolean;
var
  ProjectOptions: IOTAProjectOptions;
begin
  Result := GxOtaTryGetProjectOptions(ProjectOptions, _Project);
  if Result then begin
    ProjectOptions.Values[OPTION_NAME_UNIT_SEARCH_PATH] := _Path;
  end;
end;

procedure GxOtaGetIdeLibraryPathStrings(Paths: TStrings; DoProcessing: Boolean = true);
var
  IdePathString: string;
  i: Integer;
begin
  Assert(Assigned(Paths));
  Paths.Clear;
  IdePathString := GxOtaGetIdeLibraryPath;
  SplitIdePath(Paths, IdePathString);
  if DoProcessing then begin
    // todo: Is it correct to use GetCurrentDir here? Shouldn't that either be the
    //       IDE base path or the project's directory?
    ProcessPaths(Paths, GetCurrentDir, '');
  end;
  for i := 0 to Paths.Count - 1 do begin
    Paths[i] := AddSlash(Paths[i]);
  end;
end;

procedure GxOtaGetEffectiveLibraryPath(Paths: TStrings;
  Project: IOTAProject = nil; DoProcessing: Boolean = True; AdditionalPaths: TStrings = nil);
var
  LocalList: TStringList;
  i: Integer;
  PlatformName: string;
  ProjectDir: string;
begin
  Assert(Assigned(Paths));
  Paths.Clear;
  // DoProcessing = True uses the project file's directory as the base to expand relative paths
  GxOtaGetProjectSourcePathStrings(Paths, Project, DoProcessing);
  LocalList := TStringList.Create;
  try
    // DoProcessing = True expands any general macros and relative paths
    GxOtaGetIdeLibraryPathStrings(LocalList, DoProcessing);
    if DoProcessing then begin
      // do another processing, this time also expanding the Platform related macros
      PlatformName := GxOtaGetProjectPlatform(Project);
      // There shouldn't be any more relatives paths left, so passing GetCurrentDir is probably fine
      ProcessPaths(LocalList, GetCurrentDir, PlatformName);
    end;
    for i := 0 to LocalList.Count - 1 do begin
      EnsureStringInList(Paths, LocalList[i]);
    end;
    if Assigned(AdditionalPaths) then begin
      // create a local copy, so we don't change the list passed as parameter
      LocalList.Assign(AdditionalPaths);
      if DoProcessing then begin
        if not Assigned(Project) then
          Project := GxOtaGetCurrentProject;
        if Assigned(Project) then begin
          ProjectDir := ExtractFileDir(Project.FileName);
          PlatformName := GxOtaGetProjectPlatform(Project);
        end else begin
          ProjectDir := GetCurrentDir;
          PlatformName := '';
        end;
        ProcessPaths(LocalList, ProjectDir, PlatformName);
      end;
      for i := 0 to LocalList.Count - 1 do begin
        EnsureStringInList(Paths, LocalList[i]);
      end;
    end;
  finally
    FreeAndNil(LocalList);
  end;
end;

procedure GxOtaGetAllPossiblePaths(Paths: TStrings; Project: IOTAProject = nil; AdditionalPaths: TStrings = nil);

  function GetPath(const FileName: string): string;
  begin
    Result := '';

    if FileName <> '' then begin
      Result := ExtractFilePath(FileName);
      if Result <> '' then
        Result := AddSlash(Result);
    end;
  end;

  procedure AddAPath(const APath: string);
  begin
    if (APath <> '') and DirectoryExists(APath) then
      EnsureStringInList(Paths, APath);
  end;

  procedure AddVCLPaths;
  var
    BasePath: string;
    DirFinder: TFileFindThread;
    i: Integer;
  begin
    BasePath := AddSlash(GetIdeRootDirectory) + 'Source' + PathDelim;
    DirFinder := TFileFindThread.Create;
    try
      DirFinder.FileMasks.Add(AllFilesWildCard);
      DirFinder.RecursiveSearchDirs.Add(BasePath);
      DirFinder.DirectoriesOnly := True;
      // TODO: Cache the IDE source dir list between calls
      DirFinder.StartFind;
      Sleep(1);
      while not DirFinder.HasFinished do
        Sleep(1);
      DirFinder.LockResults;
      try
        for i := 0 to DirFinder.Results.Count - 1 do
          AddAPath(DirFinder.Results[i])
      finally
        DirFinder.Terminate;
        DirFinder.ReleaseResults;
      end;
    finally
      FreeAndNil(DirFinder);
    end;
  end;

var
  UnitList: TList;
  UnitInfo: TUnitInfo;
  i: Integer;
  LocalList: TStringList;
  ProjectDir: string;
  PlatformName: string;
begin
  Assert(Assigned(Paths));

  // Add path of the current project
  if not Assigned(Project) then
    Project := GxOtaGetCurrentProject;

  if Assigned(Project) then
    AddAPath(GetPath(GxOtaGetProjectFileName(Project)));

  // Add library search paths
  GxOtaGetEffectiveLibraryPath(Paths);

  // Add paths of all files included in the project
  if Assigned(Project) then begin
    UnitList := TList.Create;
    try
      GxOtaFillUnitInfoListForProject(UnitList, Project);
      for i := 0 to UnitList.Count - 1 do begin
        UnitInfo := TUnitInfo(UnitList[i]);
        AddAPath(GetPath(UnitInfo.FileName));
      end;
    finally
      TList_FreeWithItems(UnitList);
    end;
  end;

  // Add current file path
  AddAPath(GetPath(GxOtaGetFileNameOfCurrentModule));

  // Add path of the current source file (probably same as first one)
  AddAPath(GetPath(GxOtaGetCurrentSourceFile));

  // Add path of the project group
  AddAPath(GetPath(GxOtaGetProjectGroupFileName));

  // Add paths to VCL source (since we are smart)
  AddVCLPaths;

  if Assigned(AdditionalPaths) then begin
    LocalList := TStringList.Create;
    try
      // create a local copy, so we don't change the list passed as parameter
      LocalList.Assign(AdditionalPaths);
      if Assigned(Project) then begin
        ProjectDir := ExtractFileDir(Project.FileName);
        PlatformName := GxOtaGetProjectPlatform(Project);
      end else begin
        ProjectDir := GetCurrentDir;
        PlatformName := '';
      end;
      ProcessPaths(LocalList, ProjectDir, PlatformName);
      for i := 0 to LocalList.Count - 1 do begin
        AddAPath(LocalList[i]);
      end;
    finally
      FreeAndNil(LocalList);
    end;
  end;
end;

function GxOtaTryFindPathToFile(const FileName: string; out FullFilename: string): Boolean; overload;
var
  PathList: TStringList;
begin
  PathList := TStringList.Create;
  try
    GxOtaGetAllPossiblePaths(PathList);
    Result := TryFindPathToFile(FileName, FullFilename, PathList);
  finally
    FreeAndNil(PathList);
  end;
end;

function GxOtaFindPathToFile(const FileName: string): string;
begin
  if not GxOtaTryFindPathToFile(FileName, Result) then
    Result := FileName;
end;

function GxOtaOpenFileFromPath(const FileName: string): Boolean;
var
  FoundFile: string;
begin
  Result := False;
  FoundFile := GxOtaFindPathToFile(FileName);
  if FileExists(FoundFile) then
    Result := GxOtaOpenFile(FoundFile);
end;

procedure GxOtaGetUnitAliases(Aliases: TStrings);
var
  AliasItem, AliasString: string;
  VarAliases: Variant;
  SemicolonPos: Integer;
  FoundAliases: Boolean;
begin
  Assert(Assigned(Aliases));
  Aliases.Clear;
  FoundAliases := GxOtaTryGetProjectOption(OPTION_NAME_UNIT_ALIASES, VarAliases);
  if FoundAliases then
  begin
    AliasString := VarAliases;
    SemicolonPos := Pos(';', AliasString);  // Is this stored the same under Kylix?
    while SemicolonPos > 0 do
    begin
      AliasItem := AnsiLowerCase(Trim(Copy(AliasString, 1, SemicolonPos - 1)));
      if (AliasItem <> '') then
        EnsureStringInList(Aliases, AliasItem);
      Delete(AliasString, 1, SemicolonPos);
      SemicolonPos := Pos(';', AliasString);
    end;
    AliasString := Trim(AliasString);
    if (AliasString <> '') then
      EnsureStringInList(Aliases, AliasString);
  end;
end;

function GxOtaGetIdeBaseRegistryKey: string;
begin
  if IsStandAlone then
    Result := 'Software\' + CompilerDefinedProductRegistryKey
  else
    Result := GxOtaGetIDEServices.GetBaseRegistryKey;

  if Length(Result) > 0 then
  begin
    // Windows 2000 is allergic to a leading backslash
    // in the registry key - NT4, for instance, is not.
    if Result[1] = '\' then
      Delete(Result, 1, 1);

    Assert(Result[Length(Result)] <> '\');
  end;
end;

// This should only be used for a display that does not care about the format, it can change regularly
function GxOtaGetIDEProductIdentifier: string;
begin
  if IsStandAlone then
  begin
    if RunningCppBuilder then
      Result := 'C++Builder'
    else if RunningBDS2006 then
      Result := 'Borland Developer Studio'
    else
      Result := 'Delphi';  // Delphi 6/7/8/2005/2007/2009.
  end
  else
  begin
    Result := GxOtaGetIDEServices.GetProductIdentifier; // D2007: 'CodeGear Delphi for Microsoft Windows'
    Assert(NotEmpty(Result), 'GetProductIdentifier returns nothing');
  end;
end;

function GxOtaGetIdeImageList: TCustomImageList;
var
  NTAServices: INTAServices;
begin
  Assert(Assigned(BorlandIDEServices));
  NTAServices := BorlandIDEServices as INTAServices;

  Assert(Assigned(NTAServices));
  Result := NTAServices.ImageList;

  Assert(Assigned(Result));
end;

function GxOtaGetIdeMainMenu: TMainMenu;
var
  NTAServices: INTAServices;
begin
  Assert(Assigned(BorlandIDEServices), 'No BorlandIDEServices found');
  NTAServices := BorlandIDEServices as INTAServices;

  Assert(Assigned(NTAServices), 'No INTAServices found');
  Result := NTAServices.MainMenu;

  Assert(Assigned(Result), 'No IDE main menu found');
end;

function GxOtaGetIdeActionList: TCustomActionList;
var
  NTAServices: INTAServices;
begin
  Assert(Assigned(BorlandIDEServices));
  NTAServices := BorlandIDEServices as INTAServices;
  Assert(Assigned(NTAServices));
  Result := NTAServices.ActionList;
end;

function GxOtaTryGetIdeActionByName(const Name: string; out _Action: TContainedAction): Boolean;
var
  IdeActionList: TCustomActionList;
  InspectedAction: TContainedAction;
  i: Integer;
begin
  IdeActionList := GxOtaGetIdeActionList;
  Assert(Assigned(IdeActionList));
  Result := False;
  i := IdeActionList.ActionCount;
  while i > 0 do begin
    Dec(i);

    InspectedAction := IdeActionList.Actions[i];
    Assert(Assigned(InspectedAction));

    if SameText(InspectedAction.Name, Name) then begin
      _Action := InspectedAction;
      Result := True;
      Exit;
    end;
  end;
(*
  if not Assigned(Result) then
  begin
    MainForm := GetIdeMainForm;
    if Assigned(MainForm) then
    begin
      Action := MainForm.FindComponent(Name);
      if Action is TCustomAction then
        Result := Action as TCustomAction;
    end;
  end;
*)
end;


function GxOtaGetIdeActionByName(const Name: string): TContainedAction;
begin
  if not GxOtaTryGetIdeActionByName(Name, Result) then
    Result := nil;
(*
  if not Assigned(Result) then
  begin
    MainForm := GetIdeMainForm;
    if Assigned(MainForm) then
    begin
      Action := MainForm.FindComponent(Name);
      if Action is TCustomAction then
        Result := Action as TCustomAction;
    end;
  end;
*)
end;

function GxOtaGetTopMostEditView(SourceEditor: IOTASourceEditor): IOTAEditView;
begin
  if SourceEditor = nil then
    SourceEditor := GxOtaGetCurrentSourceEditor;
  if Assigned(SourceEditor) and (SourceEditor.EditViewCount > 0) then
    Result := SourceEditor.EditViews[0]
  else
    Result := nil;
end;

// Delphi 2005 Update 1 does not support IOTAEditorServices!
// (but we no longer care)
//{$IFDEF VER170}
//type
//  IOTAEditorServices = IOTAEditorServices70;
//{$ENDIF VER170}

function GxOtaGetEditorServices: IOTAEditorServices;
begin
  Result := (BorlandIDEServices as IOTAEditorServices);
  Assert(Assigned(Result), 'BorlandIDEServices is not assigned');
end;

procedure GxOtaGetEditorFont(const AFont: TFont; ASizeAdjust: Integer = 0);
// ASizeAdjust >  0: Use this font size
// ASizeAdjust <= 0: Reduce the configured font size by this value (e.G. -1)
var
  LServices : IOTAEditorServices;
begin
  LServices := GxOtaGetEditorServices;
  if Assigned(LServices) then begin
    AFont.Name := LServices.EditOptions.FontName;
    if ASizeAdjust > 0 then
      AFont.Size := ASizeAdjust
    else
      AFont.Size := LServices.EditOptions.FontSize + ASizeAdjust;
  end;
end;

function GxOtaGetTopMostEditView: IOTAEditView;
begin
  Result := nil;
  // Bug: Delphi 5/6 crash when calling TopView with no files open
  if GxOtaGetOpenModuleCount = 0 then
    Exit;
  Result := GxOTAGetEditorServices.TopView;
end;

function GxOtaTryGetTopMostEditView(out EditView: IOTAEditView): boolean;
begin
  EditView := GxOtaGetTopMostEditView;
  Result := Assigned(EditView);
end;

function GxOtaTryGetTopMostEditView(SourceEditor: IOTASourceEditor; out EditView: IOTAEditView): boolean; overload;
begin
  EditView := GxOtaGetTopMostEditView(SourceEditor);
  Result := Assigned(EditView);
end;

function GxOtaGetTopMostEditBuffer: IOTAEditBuffer;
begin
  Result := GxOTAGetEditorServices.TopBuffer;
end;

function GxOtaGetTopMostEditBufferFileName: string;
var
  Buffer: IOTAEditBuffer;
begin
  Result := '';
  Buffer := GxOtaGetTopMostEditBuffer;
  if Assigned(Buffer) then
    Result := Buffer.FileName;
end;

function GxOtaGetOpenModuleCount: Integer;
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));
  Result := ModuleServices.ModuleCount;
end;

function GxOtaGetCurrentModule: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));

  Result := ModuleServices.CurrentModule;
end;

function GxOtaGetCurrentSourceEditor: IOTASourceEditor;
var
  EditBuffer: IOTAEditBuffer;
begin
  Result := nil;
  EditBuffer := GxOtaGetTopMostEditBuffer;
  if Assigned(EditBuffer) and (EditBuffer.FileName <> '') then
    Result := GxOtaGetSourceEditorFromModule(GxOtaGetCurrentModule, EditBuffer.FileName);
  if Result = nil then
    Result := GxOtaGetSourceEditorFromModule(GxOtaGetCurrentModule);
end;

function GxOtaTryGetCurrentSourceEditor(out SourceEditor: IOTASourceEditor): boolean;
begin
  SourceEditor := GxOtaGetCurrentSourceEditor;
  Result := Assigned(SourceEditor);
end;

procedure GxOtaShowCurrentSourceEditor;
var
  Editor: IOTASourceEditor;
begin
  if GxOtaTryGetCurrentSourceEditor(Editor) then
    Editor.Show;
end;

procedure GxOtaAssertSourceEditorNotReadOnly(SourceEditor: IOTASourceEditor);
begin
  Assert(Assigned(SourceEditor));
  if Supports(SourceEditor, IOTAEditBuffer) then
    if (SourceEditor as IOTAEditBuffer).IsReadOnly then
      raise Exception.CreateFmt('%s is read only', [ExtractFileName(SourceEditor.FileName)]);
end;

function GxOtaGetModule(const FileName: string): IOTAModule;
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));

  Result := ModuleServices.FindModule(FileName);
end;

function GxOtaGetSourceEditor(const FileName: string): IOTASourceEditor;
var
  Module: IOTAModule;
begin
  Result := nil;
  Module := GxOtaGetModule(FileName);
  if Assigned(Module) then
    Result := GxOtaGetSourceEditorFromModule(Module, FileName)
end;

function GxOtaGetCurrentSourceFile: string;
var
  Module: IOTAModule;
  Editor: IOTAEditor;
begin
  Result := '';
  Module := GxOtaGetCurrentModule;
  if Module <> nil then
  begin
    Editor := Module.GetCurrentEditor;
    if Editor <> nil then
      Result := Editor.FileName
    else // C++Builder 6 returns nil for some old-style modules without DFMs
      Result := Module.FileName;
  end;
end;

function GxOtaGetCurrentSyntaxHighlighter(SourceEditor: IOTASourceEditor): TGXSyntaxHighlighter;
var
{$IFDEF GX_VER150_up}
  EditView: IOTAEditView;
  EditBuffer: IOTAEditBuffer;
  EditOptions: IOTAEditOptions;
{$ELSE not GX_VER150_up}
  FileName: string;
  Ext: string;
  Highlighter: TOTASyntaxHighlighter;
{$ENDIF not GX_VER150_up}
begin
  Result := gxpNone;
  if SourceEditor = nil then
    SourceEditor := GxOtaGetCurrentSourceEditor;
  if not Assigned(SourceEditor) then
    Exit;

// TODO 4 -cCleanup -oAnyone: Move mapping out of this routine, into separate lookup
{$IFDEF GX_VER150_up}
  // difficult case -> Get 'new style' IOTAEditOptions and determine style from that
  // in D7 and up, IOTASourceEditor.SetSyntaxHighlighter does not function any more.
  // so we have to use some heuristics from EditOptions to find out what
  // syntax highlighter is actually being used
  EditView := GxOtaGetTopMostEditView(SourceEditor);
  if not Assigned(EditView) then
    Exit;
  EditBuffer := EditView.Buffer;
  if not Assigned(EditBuffer) then
    Exit;
  EditOptions := EditBuffer.EditOptions;
  if not Assigned(EditOptions) then
    Exit;
  if EditOptions.IDString = cDefEdPascal then
    Result := gxpPAS
  else if EditOptions.IDString = cDefEdC then
    Result := gxpCPP
  else if EditOptions.IDString = cDefEdCSharp then
    Result := gxpCS
  else if EditOptions.IDString = cDefEdHTML then
    Result := gxpHTML
  else if EditOptions.IDString = cDefEdXML then
    Result := gxpXML
  else if EditOptions.IDString = cDefEdSQL then
    Result := gxpSQL;
  // Ignore: cDefEdIDL, cDefEdVisualBasic, cDefEdJavaScript, cDefEdStyleSheet, cDefEdINI, cDefEdPHP
{$ELSE not GX_VER150_up}
  FileName := SourceEditor.FileName;
  Highlighter := SourceEditor.SetSyntaxHighlighter(ToolsAPI.shQuery);
  case Highlighter of
    shPascal: Result := gxpPas;
    shC: Result := gxpCPP;
    shSQL: Result := gxpSQL;
    else begin
      Ext := ExtractUpperFileExt(FileName);
      if StringInArray(Ext, ['XML', 'BDSGROUP', 'BDSPROJ', 'DPROJ', 'GROUPPROJ']) then
        Result := gxpXML
      else if StringInArray(Ext, ['HTM', 'HTML', 'ASP']) then
        Result := gxpHTML
      else if StringInarray(Ext, ['CS']) then
        Result := gxpCS
    end;
  end
{$ENDIF GX_VER150_up}
end;

function GxOtaGetPropertyIndexByName(const AComponent: IOTAComponent; const PropertyName: WideString): Integer;
begin
  Assert(Assigned(AComponent));

  Result := AComponent.GetPropCount-1;
  while Result >= 0 do
  begin
    if SameText(PropertyName, AComponent.GetPropName(Result)) then
      Break;

    Dec(Result);
  end;
end;

function GxOtaEditorIsRecordingMacro: Boolean;
var
  CurRecord: IOTARecord;
begin
  Result := False;
  CurRecord := GxOtaGetKeyboardServices.CurrentRecord;
  if Assigned(CurRecord) then
    Result := CurRecord.IsRecording;
end;

function GxOtaEditorIsPlayingMacro: Boolean;
var
  CurPlayback: IOTARecord;
begin
  Result := False;
  CurPlayback := GxOtaGetKeyboardServices.CurrentPlayback;
  if Assigned(CurPlayback) then
    Result := CurPlayback.IsPlaying;
end;

function GxOtaEditorIsPausedMacro: Boolean;
var
  CurRecord: IOTARecord;
begin
  Result := False;
  CurRecord := GxOtaGetKeyboardServices.CurrentRecord;
  if Assigned(CurRecord) then
    Result := CurRecord.IsPaused;
end;

procedure GxOtaSetEditorKeyTracingEnabled(Enabled: Boolean);
begin
  (BorlandIDEServices as ToolsAPI.IOTAKeyboardDiagnostics).KeyTracing := Enabled;
end;

function GxOtaGetEditorKeyTracingEnabled: Boolean;
begin
  Result := (BorlandIDEServices as ToolsAPI.IOTAKeyboardDiagnostics).KeyTracing;
end;

type
  // TODO 3 -cCleanup -oAnyone: Remove this type, since it is confusing and is not .NET compatible
  TPropertyValBuffer = record
    case Integer of
      0: (RawBuffer: array[0..255] of Byte);
      1: (SString: ShortString);
      2: (Int: Integer);
      3: (Int64: Int64);
      4: (LString: PAnsiChar);
      5: (WString: PWChar);
      6: (VChar: Char);
      7: (VWChar: WideChar);
      8: (VFloat: Extended);
      9: (UString: PChar);
    end;

function GxOtaGetComponentPropertyAsString(const AComponent: IOTAComponent;
  const PropertyName: string; PreferStrings: Boolean): WideString;
var
  PropertyType: TTypeKind;
  VVariant: Variant;
  Buffer: TPropertyValBuffer;
  NativeObject: TObject;
begin
  Assert(Assigned(AComponent));
  Result := '';
  if Trim(PropertyName) = '' then
    Exit;
  PropertyType := AComponent.GetPropTypeByName(PropertyName);

  Buffer.LString := nil;
  Buffer.WString := nil;
  Buffer.Int64 := 0;
  if not AComponent.GetPropValueByName(PropertyName, Buffer) then
    Exit;

  case PropertyType of
    tkLString: Result := WideString(AnsiString(Buffer.LString));
    {$IFDEF GX_VER200_up}
    tkUString: Result := Buffer.UString;
    {$ENDIF}
    tkString:  Result := WideString(Buffer.SString);
    tkInteger: Result := IntToStr(Buffer.Int);
    tkInt64:   Result := IntToStr(Buffer.Int64);
    tkChar:    Result := Buffer.VChar;
    tkWChar:   Result := Buffer.VWChar;
    tkFloat:   Result := FloatToStr(Buffer.VFloat);
    // Delphi 2007 and earlier have a bug that returns a WideString as AnsiString data.
    // This will fail for characters outside of the current ANSI codepage.
    tkWString:
      begin
        if RunningDelphi2007OrLess then
          Result := WideString(Buffer.LString)
        else
          Result := Buffer.WString;
      end;
    tkSet:
      begin
        if PreferStrings and GxOtaActiveDesignerIsVCL then
        begin
          NativeObject := GxOtaGetNativeObject(AComponent);
          Assert(Assigned(NativeObject), 'No native object for property ' + PropertyName);
          Result := SetToString(GetPropInfo(NativeObject, PropertyName), Buffer.Int, True);
        end
        else
          Result := IntToStr(Buffer.Int);
      end;
    tkEnumeration:
      begin
        if PreferStrings and GxOtaActiveDesignerIsVCL then
        begin
          NativeObject := GxOtaGetNativeObject(AComponent);
          Assert(Assigned(NativeObject));
          Result := GetEnumName(FindTypeInfo(NativeObject, PropertyName), Buffer.Int);
        end
        else
          Result := IntToStr(Buffer.Int);
      end;
    tkVariant:
      begin
        if not AComponent.GetPropValueByName(PropertyName, VVariant) then
          Exit;
        Result := VVariant;
      end;
    // Unsupported: tkUnknown, tkMethod, tkInterface, tkDynArray, tkClass, tkArray, tkRecord
    else raise Exception.Create('Unsupported property type ' + GetEnumName(TypeInfo(TTypeKind), Ord(PropertyType)));
  end;
end;

function GxOtaNormalizePropertyValue(Component: IOTAComponent; const PropName, Value: string): string;
var
  PropType: TTypeKind;
  NativeObject: TObject;
begin
  Assert(Assigned(Component));
  Result := Value;
  PropType := Component.GetPropTypeByName(PropName);
  if PropType = tkEnumeration then
  begin
    NativeObject := GxOtaGetNativeObject(Component);
    if Assigned(NativeObject) then
      Result := IntToStr(GetEnumValueFromStr(NativeObject, PropName, Value))
    else
    begin // CLX, .net, etc?
      if PropType = tkEnumeration then begin
        if SameText(Value, 'True') then
          Result := '1'
        else if SameText(Value, 'False') then
          Result := '0';
      end;
    end;
  end;
end;

function GxOtaSetComponentPropertyAsString(const AComponent: IOTAComponent;
  const PropertyName: string; const Value: WideString): Boolean;
var
  PropertyType     : TTypeKind;
  PropertyTypeName : string;
  ComponentName    : WideString;
  NativeObject     : TObject;
  VInteger         : Integer;
  VInt64           : Int64;
  VVariant         : Variant;
  VAString         : AnsiString;
  {$IFDEF GX_VER200_up}
  VUString         : string;
  {$ENDIF}
  VShortString     : string;
  VFloat           : Extended;
  VAChar           : AnsiChar;
  VWChar           : WChar;
  VBoolean         : Boolean;
  TempInt: Integer;

  function PropertyDescription: string;
  begin
    Result := Format('%s.%s of type %s', [ComponentName, PropertyName, PropertyTypeName]);
  end;

begin
  Assert(Assigned(AComponent));
  Result := False;
  if Trim(PropertyName) = '' then
    Exit;
  ComponentName := GxOtaGetComponentName(AComponent);
  NativeObject := GxOtaGetNativeObject(AComponent);

  PropertyType := AComponent.GetPropTypeByName(PropertyName);
  PropertyTypeName := GetEnumName(TypeInfo(TTypeKind), Ord(PropertyType));
  case PropertyType of
    tkWString: begin
        VAString := AnsiString(Value);
        // Delphi 2007 and earlier have a bug that requires AnsiString data to set a WideString.
        // This will fail for characters outside of the current ANSI codepage.
        if RunningDelphi2007OrLess then
          Result := AComponent.SetPropByName(PropertyName, VAString)
        else
          Result := AComponent.SetPropByName(PropertyName, Value);
      end;

    tkLString: begin
        VAString := AnsiString(Value);
        Result := AComponent.SetPropByName(PropertyName, VAString);
      end;

    {$IFDEF GX_VER200_up}
    tkUString: begin
        VUString := Value;
        Result := AComponent.SetPropByName(PropertyName, VUString);
      end;
    {$ENDIF}

    tkString: begin
        VShortString := Value;
        Result := AComponent.SetPropByName(PropertyName, VShortString);
      end;

    tkInteger: begin
        VInteger := StrToInt(Value);
        Result := AComponent.SetPropByName(PropertyName, VInteger);
        if AComponent.GetPropValueByName(PropertyName, TempInt) then begin
          // Setting an integer property often (always?) fails, so we check the value here and if
          // it is different, we use the native object to set the value. This works.
          // (Example: Try to set the Interval property of a TTimer. It always gets set to 0.)
          // This happens in Delphi 2007 and 10.2, I haven't tested other versions.)
          // -- 2018-07-16 twm
          if TempInt <> VInteger then
            SetPropValue(NativeObject, PropertyName, VInteger);
        end;
      end;

    tkInt64: begin
        VInt64 := StrToInt64(Value);
        Result := AComponent.SetPropByName(PropertyName, VInt64);
      end;

    tkVariant: begin
        VVariant := Value;
        Result := AComponent.SetPropByName(PropertyName, VVariant);
      end;

    tkFloat: begin
        VFloat :=  StrToFloat(Value);
        Result := AComponent.SetPropByName(PropertyName, VFloat);
      end;

    tkChar: begin
        if Length(Value) > 0 then begin
          VAChar := AnsiChar(Value[1]);
          Result := AComponent.SetPropByName(PropertyName, VAChar);
        end;
      end;

    tkWChar: begin
        if Length(Value) > 0 then begin
          VWChar := Value[1];
          Result := AComponent.SetPropByName(PropertyName, VWChar);
        end;
      end;

    // Note: Booleans are of type tkEnumeration
    tkEnumeration: begin
        if Assigned(NativeObject) then begin
          VInteger := GetEnumValueFromStr(NativeObject, PropertyName, Value);
          Result := AComponent.SetPropByName(PropertyName, VInteger);
        end
        else begin // VCL.NET error in BDS 2006: "Invalid property typeinfo type": http://qc.codegear.com/wc/qcmain.aspx?d=42751
          if SameText(Value, 'TRUE') or SameText(Value, 'FALSE') then begin
            VBoolean := SameText(Value, 'TRUE');
            Result := AComponent.SetPropByName(PropertyName, VBoolean);
          end
          else begin
            VInteger := StrToIntDef(Value, -99998);
            if VInteger <> -99998 then
              Result := AComponent.SetPropByName(PropertyName, VInteger)
            else
              raise Exception.CreateFmt('Unsupported property value for %s: %s (try using the integer representing the enum value)', [PropertyDescription, Value]);
          end;
        end;
      end;

    // Support for sets is partial, because we don't support normalizing the values
    tkSet: begin
        if Assigned(NativeObject) then begin
          VInteger := ApplyValueToSetProperty(NativeObject, PropertyName, Value);
          SetOrdProp(NativeObject, PropertyName, VInteger);
        end
        else
          raise Exception.CreateFmt('No native object available to set component property %s', [PropertyDescription]);
      end;

    else // Unsupported: tkUnknown, tkMethod, tkInterface, tkDynArray, tkClass, tkArray, tkRecord
      raise Exception.CreateFmt('Unsupported property type for %s', [PropertyDescription]);
  end;
end;

function GxOtaGetNativeObject(const AComponent: IOTAComponent): TObject;
var
  CompHandle: TOTAHandle;
begin
  Result := nil;
  if (AComponent = nil) or ((GxOtaCurrentlyEditingForm) and (not (GxOtaActiveDesignerIsVCL or GxOtaActiveDesignerIsFMX))) then
    Exit;
  CompHandle := AComponent.GetComponentHandle;
  if Assigned(CompHandle) then
    Result := TObject(CompHandle);
end;

function GxOtaGetNativePersistent(const AComponent: IOTAComponent): TPersistent;
var
  ComponentObject: TObject;
begin
  Result := nil;
  ComponentObject := GxOtaGetNativeObject(AComponent);
  if Assigned(ComponentObject) then
  begin
    if GxOtaActiveDesignerIsNFM then begin
      {$IFOPT D+}SendDebugError('Attempt to get a native persistent for a .NET object');{$ENDIF}
      Exit;
    end;
    if ComponentObject is TPersistent then
      Result := ComponentObject as TPersistent;
  end;
end;

function GxOtaGetNativeComponent(const AComponent: IOTAComponent): TComponent;
var
  ComponentObject: TObject;
begin
  Result := nil;
  ComponentObject := GxOtaGetNativeObject(AComponent);
  if Assigned(ComponentObject) then
  begin
    if GxOtaActiveDesignerIsNFM then begin
      {$IFOPT D+}SendDebugError('Attempt to get a native component for a .NET object');{$ENDIF}
      Exit;
    end;
    if ComponentObject is TComponent then
      Result := ComponentObject as TComponent;
  end;
end;

function GxOtaIsInheritedComponent(const AComponent: IOTAComponent): Boolean;
var
  NativeComponent: TComponent;
begin
  Result := False;
  NativeComponent :=  GxOtaGetNativeComponent(AComponent);
  if Assigned(NativeComponent) then
    Result := (csAncestor in NativeComponent.ComponentState);
end;

function GxOtaGetComponentName(const AComponent: IOTAComponent): WideString;
var
  Component: TComponent;
begin
  Assert(Assigned(AComponent));
  Result := GxOtaGetComponentPropertyAsString(AComponent, NamePropertyName);
  if IsEmpty(Result) then
  begin
    Component := GxOtaGetNativeComponent(AComponent);
    if Assigned(Component) then
      Result := Component.Name;
  end;
end;

function GxOtaSetComponentName(const AComponent: IOTAComponent; const Name: WideString): Boolean;
begin
  Assert(Assigned(AComponent));
  Result := GxOtaSetComponentPropertyAsString(AComponent, NamePropertyName, Name);
end;

function GxOtaGetComponentParentName(const AComponent: IOTAComponent): WideString;
resourcestring
  SNoParent = 'No Parent';
var
  {$IFNDEF GX_VER160_up}
  Parent: TComponent;
  NativeComponent: TComponent;
  {$ELSE GX_VER160_up}
  Parent: IOTAComponent;
  {$ENDIF GX_VER160_up}
begin
  Assert(Assigned(AComponent));
  Result := SNoParent;

  // IOTAComponent.GetParent is broken in Delphi 5/6/7
  {$IFNDEF GX_VER160_up}
  NativeComponent :=  GxOtaGetNativeComponent(AComponent);
  if Assigned(NativeComponent) then
  begin
    Parent := NativeComponent.GetParentComponent;
    if Assigned(Parent) then
      Result := Parent.Name;
  end;
  {$ELSE GX_VER160_up}
  Parent := AComponent.GetParent;
  if Assigned(Parent) then
    Result := GxOtaGetComponentName(Parent);
  {$ENDIF GX_VER160_up}
end;

function GxOtaComponentsAreEqual(Comp1, Comp2: IOTAComponent): Boolean;
begin
  // Should we compare handles directly in Delphi 5/6/7?
  Result := False;
  if (not Assigned(Comp1)) or (not Assigned(Comp2)) then
    Exit;
  // Check if the parents are equal?
  Result := SameText(Comp1.GetComponentType, Comp2.GetComponentType) and
    SameText(GxOtaGetComponentName(Comp1), GxOtaGetComponentName(Comp2)) and
    SameText(GxOtaGetComponentParentName(Comp1), GxOtaGetComponentParentName(Comp2));
end;

procedure GxOtaGetInstalledComponentList(Components: TStrings; ActiveOnly: Boolean);
var
  PackageServices: IOTAPackageServices;
  PackageCounter: Integer;
  ComponentCounter: Integer;
  InstalledComponentName: string;
begin
  PackageServices := BorlandIDEServices as IOTAPackageServices;
  Assert(Assigned(PackageServices));

  Components.BeginUpdate;
  try
    Components.Clear;
    for PackageCounter := 0 to PackageServices.PackageCount-1 do
    begin
      for ComponentCounter := 0 to PackageServices.GetComponentCount(PackageCounter)-1 do
      begin
        InstalledComponentName := PackageServices.ComponentNames[PackageCounter, ComponentCounter];
        // Filter out duplicate components and filter CLX/VCL components
        if (Components.IndexOf(InstalledComponentName) = -1) then
        begin
          if (not ActiveOnly) or (GetClass(InstalledComponentName) <> nil) then
            Components.Add(InstalledComponentName);
        end;
      end;
    end;
  finally
    Components.EndUpdate;
  end;
  if Components is TStringList then
    (Components as TStringList).Sort;
end;

function GxOtaPropertyExists(const Component: IOTAComponent; const PropertyName: string): Boolean;
var
  i: Integer;
begin
  Result := False;

  Assert(Assigned(Component));
  for i := 0 to Component.GetPropCount-1 do
  begin
    Result := SameText(Component.GetPropName(i), PropertyName);
    if Result then
      Break;
  end;
end;

function GxOtaGetSourceEditorFromModule(Module: IOTAModule; const FileName: string): IOTASourceEditor;
var
  i: Integer;
  IEditor: IOTAEditor;
  ISourceEditor: IOTASourceEditor;
begin
  Result := nil;
  if not Assigned(Module) then
    Exit;

  for i := 0 to Module.GetModuleFileCount-1 do
  begin
    IEditor := GxOtaGetFileEditorForModule(Module, i);

    if Supports(IEditor, IOTASourceEditor, ISourceEditor) then
    begin
      if Assigned(ISourceEditor) then
      begin
        if (FileName = '') or SameFileName(ISourceEditor.FileName, FileName) then
        begin
          Result := ISourceEditor;
          Break;
        end;
      end;
    end;
  end;
end;

function GxOtaGetCurrentEditor: IOTAEditor;
var
  Module: IOTAModule;
  Editor: IOTAEditor;
  TopFile: string;
begin
  Result := nil;
  Module := GxOtaGetCurrentModule;
  if Module <> nil then
  begin
    Editor := Module.GetCurrentEditor;
    if Editor <> nil then
      TopFile := Editor.FileName
    else // C++Builder 6 returns nil for some old-style modules without DFMs
      TopFile := Module.FileName;
    Result := GxOtaGetEditorFromModule(Module, TopFile);
  end;
end;

function GxOtaGetCurrentEditorAsSourceEditor: IOTASourceEditor;
begin
  if not GxOtaTryGetCurrentEditorAsSourceEditor(Result) then
    Result := nil;
end;

function GxOtaTryGetCurrentEditorAsSourceEditor(out SourceEditor: IOTASourceEditor): Boolean;
var
  Editor: IOTAEditor;
begin
  Editor := GxOtaGetCurrentEditor;
  Result := (Editor <> nil) and Supports(Editor, IOTASourceEditor, SourceEditor);
end;

function GxOtaGetEditorFromModule(Module: IOTAModule; const FileName: string): IOTAEditor;
var
  i: Integer;
  Editor: IOTAEditor;
begin
  Result := nil;
  if Module = nil then
    Exit;

  for i := 0 to Module.GetModuleFileCount-1 do
  begin
    Editor := GxOtaGetFileEditorForModule(Module, i);
    if SameFileName(Editor.FileName, FileName) then
    begin
      Result := Editor;
      Break;
    end;
  end;
end;

function GxOtaGetFileEditorForModule(Module: IOTAModule; Index: Integer): IOTAEditor;
begin
  Assert(Assigned(Module));
  Result := Module.GetModuleFileEditor(Index);
end;

function GxOtaGetEditActionsFromModule(Module: IOTAModule): IOTAEditActions;
var
  i: Integer;
  EditView: IOTAEditView;
  SourceEditor: IOTASourceEditor;
begin
  Result := nil;
  if Module = nil then
    Exit;
  SourceEditor := GxOtaGetSourceEditorFromModule(Module);
  if SourceEditor = nil then
    Exit;
  for i := 0 to SourceEditor.GetEditViewCount - 1 do
  begin
    EditView := SourceEditor.GetEditView(i);
    if Supports(EditView, IOTAEditActions, Result) then
      Exit;
  end;
  Result := nil;
end;

function GxOtaGetEditActions: IOTAEditActions;
begin
  Result := GxOtaGetEditActionsFromModule(GxOtaGetCurrentModule);
end;

function GxOtaSelectedComponentIsRoot(const FormEditor: IOTAFormEditor): Boolean;
var
  SelCount: Integer;
  CurrentComponent: IOTAComponent;
  RootComponent: IOTAComponent;
begin
  Result := False;
  Assert(Assigned(FormEditor));

  // Note: No components are selected when the form editor isn't in focus
  SelCount := FormEditor.GetSelCount;

  // If SelCount = 1, either a "real" component is selected or
  // the form itself is selected.  If <> 1, the form is not selected.
  if SelCount = 1 then
  begin
    CurrentComponent := FormEditor.GetSelComponent(0);
    RootComponent := FormEditor.GetRootComponent;
    // Delphi 5 sometimes returns nil for one of the above (when the form isn't fully created?)
    if (CurrentComponent = nil) or (RootComponent = nil) then
      Exit;

    Result := SameText(RootComponent.GetComponentType, CurrentComponent.GetComponentType);
  end
end;

function GxOtaSelectedComponentName: string;
var
  FormEditor: IOTAFormEditor;
  CurrentComponent: IOTAComponent;
begin
  Result := '';
  if GxOtaTryGetCurrentFormEditor(FormEditor) then begin
    if FormEditor.GetSelCount = 1 then begin
      CurrentComponent := FormEditor.GetSelComponent(0);
      if Assigned(CurrentComponent) then
        Result := GxOtaGetComponentName(CurrentComponent);
    end;
  end;
end;

function GxOtaGetFormBiDiMode(Form: IOTAFormEditor): TBiDiMode;
var
  FormComponent: IOTAComponent;
  NativeForm: TComponent;
begin
  Result := bdLeftToRight;
  if Assigned(Form) then
  begin
    FormComponent := Form.GetRootComponent;
    if Assigned(FormComponent) then
    begin
      NativeForm := GxOtaGetNativeComponent(FormComponent);
      if Assigned(NativeForm) and (NativeForm is TCustomForm) then
        Result := (NativeForm as TCustomForm).BiDiMode;
    end;
  end;
end;

{$UNDEF LoadAndCheckEntryPoint}
{$DEFINE LoadAndCheckEntryPoint}

function IsValidExpertDll(const FileName: string): Boolean;
{$IFDEF LoadAndCheckEntryPoint}
var
  DllHandle: THandle;
{$ENDIF LoadAndCheckEntryPoint}
begin
  Result := True;

{$IFDEF LoadAndCheckEntryPoint}
  // Check that the DLL *really* is a valid expert or wizard DLL
  // (supported on Windows NT only).
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    DllHandle := LoadLibraryEx(PChar(FileName), 0, DONT_RESOLVE_DLL_REFERENCES {NT only!});
    if DllHandle <> 0 then
    begin
      try
        Result := (GetProcAddress(DllHandle, ExptIntfExpertEntryPoint) <> nil);
        if not Result then
          Result := (GetProcAddress(DllHandle, WizardEntryPoint) <> nil);
      finally
        FreeLibrary(DllHandle);
      end;
    end;
  end; {NT Check}
{$ENDIF LoadAndCheckEntryPoint}
end;

function IsStandAlone: Boolean;
begin
  Result := (BorlandIDEServices = nil);
end;

function RunningInsideIDE: Boolean;
begin
  Result := not IsStandAlone;
end;

procedure GxOtaSaveReaderToStream(EditReader: IOTAEditReader; Stream: TStream; TrailingNull: Boolean);
const
  // Leave typed constant as is - needed for streaming code.
  TerminatingNullChar: AnsiChar = #0;
var
  EditReaderPos: Integer;
  ReadDataSize: Integer;
  Buffer: array[0..EditReaderBufferSize] of AnsiChar; // Array of bytes, might be UTF-8
begin
  Assert(EditReader <> nil);
  Assert(Stream <> nil);

  EditReaderPos := 0;
  ReadDataSize := EditReader.GetText(EditReaderPos, @Buffer[0], EditReaderBufferSize);
  Inc(EditReaderPos, ReadDataSize);
  while ReadDataSize = EditReaderBufferSize do
  begin
    Stream.Write(Buffer, ReadDataSize);
    ReadDataSize := EditReader.GetText(EditReaderPos, Buffer, EditReaderBufferSize);
    Inc(EditReaderPos, ReadDataSize);
  end;
  Stream.Write(Buffer, ReadDataSize);
  if TrailingNull then
    Stream.Write(TerminatingNullChar, SizeOf(TerminatingNullChar)); // The source parsers need this
end;

function GxOtaGetActiveEditorText(Lines: TGXUnicodeStringList; UseSelection: Boolean): Boolean;
var
  ISourceEditor: IOTASourceEditor;
  IEditView: IOTAEditView;
begin
  Assert(Assigned(Lines));
  Lines.Clear;
  Result := False;

  if not GxOtaTryGetCurrentSourceEditor(ISourceEditor) then
    Exit;

  if ISourceEditor.EditViewCount > 0 then
  begin
    IEditView := GxOtaGetTopMostEditView(ISourceEditor);

    if UseSelection and Assigned(IEditView) and Assigned(IEditView.Block) and (IEditView.Block.Size > 0) then
      Lines.Text := GxOtaGetCurrentSelection(False)
    else
      GxOtaLoadSourceEditorToUnicodeStrings(ISourceEditor, Lines);
    Result := True;
  end;
end;

function GxOtaGetActiveEditorTextAsString(var Text: string; UseSelection: Boolean): Boolean;
var
  Lines: TGXUnicodeString;
begin
  Result := GxOtaGetActiveEditorTextAsUnicodeString(Lines, UseSelection);
  Text := Lines;
end;

function GxOtaGetActiveEditorTextAsUnicodeString(var Text: TGXUnicodeString; UseSelection: Boolean): Boolean;
var
  Lines: TGXUnicodeStringList;
begin
  Lines := TGXUnicodeStringList.Create;
  try
    Result := GxOtaGetActiveEditorText(Lines, UseSelection);
    Text := Lines.Text;
  finally
    FreeAndNil(Lines);
  end;
end;

function ReadEditorTextToString(EditReader: IOTAEditReader; LineStartPos: Integer; LineLength: Integer): string;
var
  AString: IDEEditBufferString;
begin
  SetLength(AString, LineLength);
  EditReader.GetText(Max(LineStartPos, 0), PAnsiChar(AString), LineLength);
  Result := IDEEditorStringToString(AString);
end;

function GxOtaReadEditorTextToString(EditReader: IOTAEditReader): string;
var
  Stream: TMemoryStream;
  Buffer: IDEEditBufferString;
begin
  Stream := TMemoryStream.Create;
  try
    GxOtaSaveReaderToStream(EditReader, Stream);
      SetLength(Buffer, Stream.Size);
      Stream.Position := 0;
      Stream.ReadBuffer(Buffer[1], Stream.Size);
  finally
    Stream.Free;
  end;
  Result := IDEEditorStringToString(Buffer);
end;

function GxOtaGetPreceedingCharactersInLine(View: IOTAEditView): string;
var
  CharPos: TOTACharPos;
  EditPos: TOTAEditPos;
  Position: Longint;
  EditReader: IOTAEditReader;
  LineStartPos: Integer;
  Offset: Integer;
  LineLength: Integer;
begin
  Assert(Assigned(View));
  Result := '';

  EditPos := View.CursorPos;
  View.ConvertPos(True, EditPos, CharPos);
  Position := View.CharPosToPos(CharPos);
  if Position < 0 then
    Exit; // Happens when you type past the 1024 character limit

  EditReader := View.Buffer.CreateReader;
  // If there are embedded tabs in the line, our offsets are off and we must
  // be sure to not request nagative indexes or return more than one line
  LineStartPos := Position - (CharPos.CharIndex);
  Offset := 0;
  if LineStartPos < 0 then // This happens with tabs at the beginning of the file
    Offset := Abs(LineStartPos);
  LineLength := CharPos.CharIndex - Offset;
  Assert(LineLength >= 0);
  if LineLength = 0 then
    Exit;
// Unicode patch by Achim Kalwa
{$IFDEF UNICODE}
  Result := Copy(
    IDEEditorStringToString(GxOtaGetEditorLine(View, EditPos.Line)),
    1, LineLength
  );
{$ELSE}
  Result := ReadEditorTextToString(EditReader, LineStartPos, LineLength);
  Result := LastLineOf(Result);
{$ENDIF}
end;

function GxOtaSelectCurrentIdent(const SourceEditor: IOTASourceEditor; MakeVisible: Boolean = True): Boolean;
var
  EStart: TOTAEditPos;
  EEnd: TOTAEditPos;
  Ident: string;
begin
  Result := GxOtaGetCurrentIdentData(Ident, EStart);
  if Result then
  begin
    EEnd.Line := EStart.Line;
    EEnd.Col := EStart.Col + Length(Ident);
    GxOtaSelectBlock(SourceEditor, EStart, EEnd);
  end;
end;

procedure GxOtaSelectCurrentLine(const SourceEditor: IOTASourceEditor; MakeVisible: Boolean);
var
  EditPos: TOTAEditPos;
  EditView: IOTAEditView;
begin
  Assert(Assigned(SourceEditor));
  EditView := GxOtaGetTopMostEditView(SourceEditor);
  if EditView = nil then
    Exit;
  EditPos.Line := EditView.Position.Row;
  EditPos.Col := 1;
  EditView.CursorPos := EditPos;
  EditView.Block.BeginBlock;
  EditView.Block.Style := btNonInclusive;
  if EditView.Position.MoveEOL then
    EditView.MoveViewToCursor;
  EditView.Block.EndBlock;
  if MakeVisible then
  begin
    EditView.Block.SetVisible(True);
    EditView.Paint;
  end;
end;

procedure GxOtaInsertLineIntoEditor(const Text: string);
var
  EditView: IOTAEditView;
resourcestring
  SUnableToWriteToEditor = 'Unable to write to source editor';
begin
  EditView := GxOtaGetTopMostEditView;
  Assert(Assigned(EditView), SUnableToWriteToEditor);
  {$IFDEF UNICODE}
  EditView.Position.InsertText(Text);
  {$ELSE}
  EditView.Position.InsertText(ConvertToIDEEditorString(Text));
  {$ENDIF}
  EditView.Paint;
end;

procedure GxOtaInsertTextIntoEditor(const Text: string);
var
  EditView: IOTAEditView;
  Position: Longint;
  CharPos: TOTACharPos;
  EditPos: TOTAEditPos;
begin
  EditView := GxOtaGetTopMostEditView;
  Assert(Assigned(EditView), 'No edit view found');
  EditPos := EditView.CursorPos;
  EditView.ConvertPos(True, EditPos, CharPos);
  Position := EditView.CharPosToPos(CharPos);
  GxOtaInsertTextIntoEditorAtBufferPos(Text, Position);
  EditView.MoveViewToCursor;
  EditView.Paint;
end;

function GxOtaGetEditWriterForSourceEditor(SourceEditor: IOTASourceEditor = nil): IOTAEditWriter;
resourcestring
  SEditWriterNotAvail = 'Edit writer not available';
begin
  if not Assigned(SourceEditor) then
    SourceEditor := GxOtaGetCurrentSourceEditor;
  if Assigned(SourceEditor) then
  begin
    GxOtaAssertSourceEditorNotReadOnly(SourceEditor);
    Result := SourceEditor.CreateUndoableWriter;
  end;
  Assert(Assigned(Result), SEditWriterNotAvail);
end;

function GxOtaGetEditReaderForSourceEditor(SourceEditor: IOTASourceEditor = nil): IOTAEditReader;
resourcestring
  SEditReaderNotAvail = 'Edit reader not available';
begin
  if not Assigned(SourceEditor) then
    SourceEditor := GxOtaGetCurrentSourceEditor;
  if Assigned(SourceEditor) then
  begin
    Result := SourceEditor.CreateReader;
  end;
  Assert(Assigned(Result), SEditReaderNotAvail);
end;

procedure GxOtaInsertTextIntoEditorAtBufferPos(const Text: string; Position: Longint;
  SourceEditor: IOTASourceEditor);
var
  EditWriter: IOTAEditWriter;
  IdeString: UTF8String;
begin
  if Text = '' then
    Exit;

  EditWriter := GxOtaGetEditWriterForSourceEditor(SourceEditor);
  IdeString :=  ConvertToIDEEditorString(Text);
  EditWriter.CopyTo(Position);
  EditWriter.Insert(PAnsiChar(IdeString));
end;


procedure GxOtaInsertTextIntoEditorAtCharPos(const Text: string; Position: Longint;
  SourceEditor: IOTASourceEditor);
var
  EditWriter: IOTAEditWriter;
{$IFDEF GX_DELPHI8_UP}
  Buffer: string;
{$ENDIF}
  IdeString: UTF8String;
begin
  if Text = '' then
    Exit;

{$IFDEF GX_DELPHI8_UP}
  // Position is a character position in a (possibly unicode) string
  // the EditWriter needs a byte position in a UTF-8 string (for Delphi >=8)
  // or in an ansistring (for Delphi <8)
  Buffer := GxOtaReadEditorTextToString(GxOtaGetEditReaderForSourceEditor(SourceEditor));
  IdeString := ConvertToIDEEditorString(Buffer);
  Position := CharIndexToUTF8Pos(IdeString, Position);
{$ENDIF}
  IdeString :=  ConvertToIDEEditorString(Text);
  EditWriter := GxOtaGetEditWriterForSourceEditor(SourceEditor);
  EditWriter.CopyTo(Position);
  EditWriter.Insert(PAnsiChar(IdeString));
end;

procedure GxOtaGotoPosition(Position: Longint; EditView: IOTAEditView; Middle: Boolean);
var
  CurPos: TOTAEditPos;
  CharPos: TOTACharPos;
begin
  if not Assigned(EditView) then
    EditView := GxOtaGetTopMostEditView;
  Assert(Assigned(EditView));

  CharPos := GxOtaGetCharPosFromPos(Position, EditView);
  CurPos.Col := CharPos.CharIndex + 1;
  CurPos.Line := CharPos.Line;
  GxOtaGotoEditPos(CurPos, EditView, Middle);
end;

procedure GxOtaGotoEditPos(EditPos: TOTAEditPos; EditView: IOTAEditView; Middle: Boolean);
var
  TopRow: TOTAEditPos;
begin
  if not Assigned(EditView) then
    EditView := GxOtaGetTopMostEditView;
  Assert(Assigned(EditView));

  if EditPos.Line < 1 then
    EditPos.Line := 1;
  TopRow := EditPos;
  if Middle then
    TopRow.Line := TopRow.Line - (EditView.ViewSize.cy div 2) + 1;
  if TopRow.Line < 1 then
    TopRow.Line := 1;
  TopRow.Col := 1;
  EditView.TopPos := TopRow;

  EditView.CursorPos := EditPos;
  Application.ProcessMessages;
  EditView.Paint;
end;

function GxOtaGetCharPosFromPos(Position: Longint; EditView: IOTAEditView): TOTACharPos;
var
  EditWriter : IOTAEditWriter;
begin
  Assert(Assigned(EditView));
  EditWriter := EditView.Buffer.CreateUndoableWriter;
  Assert(Assigned(EditWriter));
  EditWriter.CopyTo(Position);
  Result := EditWriter.CurrentPos;
end;

function GxOtaTryGetCurrentEditPos(out _EditPos: TOTAEditPos): Boolean;
var
  View: IOTAEditView;
begin
  Result := GxOtaTryGetTopMostEditView(View);
  if Result then
    _EditPos := View.CursorPos;
end;

function GxOtaGetCurrentEditPos(EditView: IOTAEditView): TOTAEditPos;
begin
  Result.Col := -1;
  Result.Line := -1;
  if EditView = nil then
    EditView := GxOtaGetTopMostEditView;
  if EditView = nil then
    Exit;
  Result := EditView.CursorPos;
end;

function GxOtaGetCurrentEditBufferPos(EditView: IOTAEditView = nil): Integer;
var
  EditPos: TOTAEditPos;
  CharPos: TOTACharPos;
begin
  if EditView = nil then
    EditView := GxOtaGetTopMostEditView;
  EditPos := GxOtaGetCurrentEditPos(EditView);
  EditView.ConvertPos(True, EditPos, CharPos);
  Result := EditView.CharPosToPos(CharPos);
end;

procedure GxOtaDeleteByteFromPos(StartIndex, Count: Longint; SourceEditor: IOTASourceEditor = nil);
var
  EditWriter: IOTAEditWriter;
begin
  EditWriter := GxOtaGetEditWriterForSourceEditor(SourceEditor);
  EditWriter.CopyTo(StartIndex);
  EditWriter.DeleteTo(StartIndex + Count);
end;

procedure GxOtaDeleteTextFromPos(StartPos, Count: Longint; SourceEditor: IOTASourceEditor = nil);
var
  EditWriter: IOTAEditWriter;
{$IFDEF GX_DELPHI8_UP}
  Buffer: string;
  IdeString: UTF8String;
{$ENDIF}
begin
{$IFDEF GX_DELPHI8_UP}
  // "StartPos" is a character position in a (possibly unicode) string
  // the EditWriter needs a byte position in a UTF-8 string (for Delphi >=8)
  // or in an ansistring (for Delphi <8)
  Buffer := GxOtaReadEditorTextToString(GxOtaGetEditReaderForSourceEditor(SourceEditor));
  IdeString := ConvertToIDEEditorString(Buffer);
  StartPos := CharIndexToUTF8Pos(IdeString, StartPos);
{$ENDIF}

  EditWriter := GxOtaGetEditWriterForSourceEditor(SourceEditor);
  EditWriter.CopyTo(StartPos);
  EditWriter.DeleteTo(StartPos + Count);
end;

{$DEFINE GX_WorkAroundFirstCharInLineSelectionBug}
// Adapted from Ray Lischner's ReplaceSelection, but ported to use ToolsAPI.pas
procedure GxOtaReplaceSelection(const Editor: IOTASourceEditor; ViewNum: Integer; const Text: string);
resourcestring
  SBlockNotCompatible = 'Unsupported block selection type for a replace operation';
var
  View: IOTAEditView;
  Start, After: TOTACharPos;

  procedure ReplaceColumns(Start, After: TOTAEditPos; RplText: string);
  begin
    raise Exception.Create('Replacing of columnar selections is no longer supported');
  end;

  // Replace the text between Start and After, inclusive, with Text.
  // IsInclusive determines whether the last selected character is part
  // of the replaced block or not.
  procedure ReplaceInclusiveExclusive(const IsInclusive: Boolean);
  var
    Writer: IOTAEditWriter;
    StartPos: Integer;
    AfterPos: Integer;
    DeleteToPos: Integer;

    {$IFDEF GX_WorkAroundFirstCharInLineSelectionBug}
      FirstCharInLineDeleted: Boolean;
    {$ENDIF GX_WorkAroundFirstCharInLineSelectionBug}
  begin
    if not IsInclusive then
    {$IFDEF GX_WorkAroundFirstCharInLineSelectionBug}
    begin
      FirstCharInLineDeleted := (After.CharIndex = 1);
      if After.CharIndex > 0 then
        Dec(After.CharIndex);
    end
    else
      FirstCharInLineDeleted := False;
    {$ELSE}
      if After.CharIndex > 0 then
        Dec(After.CharIndex);
    {$ENDIF GX_WorkAroundFirstCharInLineSelectionBug}

    StartPos := View.CharPosToPos(Start);
    Assert(StartPos >= 0, 'StartPos < 0');
    AfterPos := View.CharPosToPos(After);
    Assert(AfterPos >= 0, 'AfterPos < 0');
    Writer := Editor.CreateUndoableWriter;
    try
      // Copy the initial part of the file, up to the selection
      Writer.CopyTo(StartPos);
      // Delete the block if there is one to delete
      DeleteToPos := AfterPos;

      if (After.CharIndex = 0) and (After.Line - Start.Line = 1) then
      begin
        Dec(DeleteToPos, Length(#13#10));
        {$IFDEF GX_WorkAroundFirstCharInLineSelectionBug}
        if FirstCharInLineDeleted then
        begin
          Inc(DeleteToPos, Length(#13#10));
          Inc(DeleteToPos);
        end;
        {$ENDIF GX_WorkAroundFirstCharInLineSelectionBug}
      end
      else
      begin
        {$IFDEF GX_WorkAroundFirstCharInLineSelectionBug}
        if FirstCharInLineDeleted then
          Inc(DeleteToPos)
        else if After.CharIndex > 0 then
          Inc(DeleteToPos);
        {$ELSE}
        if After.CharIndex > 0 then
          Inc(DeleteToPos);
        {$ENDIF GX_WorkAroundFirstCharInLineSelectionBug}
      end;
      if DeleteToPos > StartPos then
        Writer.DeleteTo(DeleteToPos);
      // Insert the replacement text
      Writer.Insert(PAnsiChar(ConvertToIDEEditorString(Text)));
      // Copy the rest of the file
      Writer.CopyTo(High(Longint));
    finally
      Writer := nil;
    end;
  end;

begin
  Assert(Assigned(Editor));
  Assert(Editor.EditViewCount > ViewNum, 'Editor.EditViewCount <= passed in ViewNum');
  GxOtaAssertSourceEditorNotReadOnly(Editor);

  View := Editor.GetEditView(ViewNum);
  Assert(Assigned(View), 'No edit view available');
  Assert(Assigned(View.Block), 'No block available');

  Start := Editor.BlockStart;
  After := Editor.BlockAfter;

  case Editor.BlockType of
    btInclusive:
      ReplaceInclusiveExclusive(True);
    btNonInclusive:
      ReplaceInclusiveExclusive(False);
    btColumn:
      ReplaceColumns(TOTAEditPos(Start), TOTAEditPos(After), Text);
    btLine:
      begin
        Start.CharIndex := 0; // Start of line
        After.CharIndex := 1023; // Max line length
        ReplaceInclusiveExclusive(True);
      end;
    else
      raise Exception.Create(SBlockNotCompatible);
  end;
end;

procedure GxOtaLoadIDEFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList; var WasBinary: Boolean);
var
  BaseFileName: string;
  Module: IOTAModule;
  BaseModule: IOTAModule;
  SourceEditor: IOTASourceEditor;
  FormEditor: IOTAFormEditor;
begin
  Assert(Assigned(Data));
  Data.Clear;
  Module := GxOtaGetModule(FileName);
  BaseFileName := GxOtaGetBaseModuleFileName(FileName);
  BaseModule := GxOtaGetModule(BaseFileName);

  if IsForm(FileName) then
  begin
    // Is the form currently open as text?
    if GxOtaModuleIsShowingFormSource(Module) then
      GxOtaLoadSourceEditorToUnicodeStrings(GxOtaGetSourceEditor(FileName), Data)
    else if GxOtaIsFileOpen(BaseFileName) then
    begin
      FormEditor := GxOtaGetFormEditorFromModule(BaseModule);
      GxOtaLoadFormEditorToUnicodeStrings(FormEditor, Data);
    end
    else if FileExists(FileName) then
      LoadDiskFileToUnicodeStrings(FileName, Data, WasBinary);
  end
  else
  begin
    SourceEditor := GxOtaGetSourceEditorFromModule(BaseModule, FileName);
    // If there is no source editor for the file name, we can't load it
    // We might be trying to load a .pas with the .dfm open as text
    if Assigned(SourceEditor) then begin
      // TODO: Test bdsgroup, bdsproj, dproj
      GxOtaLoadSourceEditorToUnicodeStrings(GxOtaGetSourceEditor(FileName), Data);
    end
    else if FileExists(FileName) then // This might be an older version, but we have no choice
      LoadDiskFileToUnicodeStrings(FileName, Data, WasBinary);
  end;
end;

procedure GxOtaLoadSourceEditorToUnicodeStrings(SourceEditor: IOTASourceEditor; Data: TGXUnicodeStringList);
var
  MemStream: TMemoryStream;
begin
  Data.Clear;
  if not Assigned(SourceEditor) then
    raise Exception.Create('No source editor in GxOtaLoadSourceEditorToUnicodeStrings');
  //TODO: Check stream format for forms as text (Ansi with escaped unicode, or UTF-8) in Delphi 2007/2009
  MemStream := TMemoryStream.Create;
  try
    GxOtaSaveReaderToStream(SourceEditor.CreateReader, MemStream, False);
    MemStream.Position := 0;
    {$IFDEF UNICODE}
    Data.LoadFromStream(MemStream, TEncoding.UTF8);
    // For some Unicode characters (e.g. $E59C and $E280 SynUnicode.LoadFromStream works,
    // but TStringList.LoadFromStream doesn't.
    // Depending on the RTL version the latter fails silently and returns an empty string list.
    // So we check here for that condition and raise an error
    if (Data.Count = 0) and (MemStream.Size >0) then
      raise Exception.CreateFmt('%s.LoadFromFile failed to read %s.', [Data.ClassParent.ClassName, SourceEditor.FileName]);
    {$ELSE}
    if RunningDelphi8OrGreater then
      SynUnicode.LoadFromStream(Data, MemStream, seUTF8)
    else
      SynUnicode.LoadFromStream(Data, MemStream, seAnsi);
    {$ENDIF}
  finally
    FreeAndNil(MemStream);
  end;
end;

procedure GxOtaLoadFormEditorToUnicodeStrings(FormEditor: IOTAFormEditor; Data: TGXUnicodeStringList);
var
  RootComponent: IOTAComponent;
  Component: TComponent;
  FormStream, TextStream: TMemoryStream;
begin
  Assert(Assigned(Data));
  if not Assigned(FormEditor) then
    Exit;
  RootComponent := FormEditor.GetRootComponent;
  if RootComponent <> nil then
  begin
    Component := GxOtaGetNativeComponent(RootComponent);
    if Component <> nil then
    begin
      TextStream := nil;
      FormStream := TMemoryStream.Create;
      try
        TextStream := TMemoryStream.Create;
        FormStream.WriteComponent(Component);
        FormStream.Position := 0;
        ObjectBinaryToText(FormStream, TextStream);
        TextStream.Position := 0;
        Data.LoadFromStream(TextStream);
      finally;
        FreeAndNil(FormStream);
        FreeAndNil(TextStream);
      end;
    end;
  end;
end;

procedure GxOtaLoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList); overload;
var
  Dummy: Boolean;
begin
  GxOtaLoadFileToUnicodeStrings(FileName, Data, Dummy);
end;

procedure GxOtaLoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList; var WasBinary: Boolean); overload;
begin
  Assert(Assigned(Data));
  Data.Clear;
  if IsEmpty(FileName) then
    raise Exception.Create('Blank filenames are not allowed');
  WasBinary := False;

  if RunningInsideIDE and GxOtaIsFileOpen(FileName, True) then
    GxOtaLoadIDEFileToUnicodeStrings(FileName, Data, WasBinary)
  else
    LoadDiskFileToUnicodeStrings(FileName, Data, WasBinary)
end;

// Test whether the last attribute before the current one is part of another
// one passed in InElement - or something else.  This is only called if
// GetAttributeAtPos returns atWhiteSpace - it is not clear, then, whether
// the cursor is inside another element.  Currently, this code is used for
// atComment and atString, see below.
function GxOtaIsWhiteSpaceInElement(const InElement: Integer;
  EV: IOTAEditView; EditorPosition: TOTAEditPos): Boolean;
var
  Element: Integer;
  LineFlag: Integer;
begin
  Element := atWhiteSpace;

  // Strange enough, but this algorithm seems to be serving each situation,
  // even the case where we react the very beginning of the line (col = 0)?
  // The expected case would have been to climb the lines up and check further
  // from the end towards the beginning, but that somehow is not necessary??
  Dec(EditorPosition.Col);
  while EditorPosition.Col >= 0 do
  begin
    EV.GetAttributeAtPos(EditorPosition, False, Element, LineFlag);
    if Element <> atWhiteSpace then
      Break;
    Dec(EditorPosition.Col);
  end;

  Result := (Element = InElement);
end;

function GxOtaIsWhiteSpaceInComment(const EditView: IOTAEditView; const EditorPosition: TOTAEditPos): Boolean;
begin
  Result := GxOtaIsWhiteSpaceInElement(atComment, EditView, EditorPosition);
end;

function GxOtaIsWhiteSpaceInString(const EditView: IOTAEditView; const EditorPosition: TOTAEditPos): Boolean;
begin
  Result := GxOtaIsWhiteSpaceInElement(atString, EditView, EditorPosition);
end;

procedure GxOtaSetCurrentSourcePosition(Position: Integer);
var
  EditView: IOTAEditView;
  CharPos: TOTACharPos;
  EditPos: TOTAEditPos;
{$IFDEF GX_DELPHI8_UP}
  Buffer: string;
  IdeString: UTF8String;
{$ENDIF}
begin
  EditView := GxOtaGetTopMostEditView;
  if Assigned(EditView) then
  begin
{$IFDEF GX_DELPHI8_UP}
    // Convert Position from character postion in the unicode/ansi string to
    // character position in UTF-8 encoded buffer.
    // todo: This should really be a function as well as the reverse.
    if GxOtaGetActiveEditorTextAsString(Buffer, False) then begin
      IdeString := ConvertToIDEEditorString(Buffer);
      Position := CharIndexToUTF8Pos(IdeString, Position);
    end;
{$ENDIF}
    CharPos := GxOtaGetCharPosFromPos(Position - 1, EditView);
    EditView.ConvertPos(False, EditPos, CharPos);
    EditView.CursorPos := EditPos;
    EditView.MoveViewToCursor;
    EditView.Paint;
  end;
end;

function GxOtaGetKeyboardServices: IOTAKeyboardServices;
begin
  if not Supports(BorlandIDEServices, IOTAKeyboardServices, Result) then
    raise Exception.Create('IDE keyboard services are not available');
end;

// Note: This returns the tab stops in reverse order (last to first)
function GxOtaGetTabStops: string;
var
  EditBuffer: IOTAEditBuffer;
begin
  Result := '';
  EditBuffer := GxOtaGetTopMostEditBuffer;
  if Assigned(EditBuffer) and Assigned(EditBuffer.BufferOptions) then
    Result := Trim(EditBuffer.BufferOptions.TabStops);
end;

function GxOtaGetTabWidth: Integer;
const
  DefTabSize = 4;
var
  TabStops: string;
  SpacePos: Integer;
begin
  if not IsStandAlone then
    TabStops := GxOtaGetTabStops;
  SpacePos := LastCharPos(TabStops, ' ');
  if SpacePos > 0 then
    Result := StrToIntDef(Copy(TabStops, SpacePos + 1, 3), DefTabSize) - 1
  else if TabStops <> '' then
    // D6-D2005 return the value entered + 1 with only one tab stop defined
    Result := StrToIntDef(TabStops, DefTabSize) - 1
  else
    Result := DefTabSize;
  if Result > 16 then
    Result := 16;
  if Result < 1 then
    Result := 1;
end;

procedure GxOtaExpandTabsInList(ALines: TStrings);
var
  i: Integer;
  TabSize: Integer;
begin
  TabSize := GxOtaGetTabWidth;
  for i := 0 to ALines.Count - 1 do
    ALines[i] := ExpandTabsInLine(ALines[i], TabSize);
end;

function GxOtaGetLeadingWhiteSpaceInSelection: string;
var
  SelText: string;
  Lines: TStringList;
  MinIndex, MinSpaceCount: Integer;
begin
  SelText := GxOtaGetCurrentSelection(False);

  Lines := TStringList.Create;
  try
    Lines.Text := SelText;

    GxOtaExpandTabsInList(Lines);
    FindMinIndent(Lines, MinIndex, MinSpaceCount);

    if MinIndex >= 0 then
      Result := Copy(Lines[MinIndex], 1, MinSpaceCount)
    else
      Result := '';
  finally
    FreeAndNil(Lines);
  end;
end;

procedure GxOtaShowProjectModuleInformation;

  function GetEditorInterfaces(Editor: IOTAEditor): string;

    procedure AddEditor(Intf: TGuid; const Name: string);
    begin
      if Supports(Editor, Intf) then
      begin
        if Result = '' then
          Result := Name
        else
          Result := Result + ', ' + Name;
      end;
    end;

  begin
    Assert(Assigned(Editor));
    Result := '';
    AddEditor(IOTASourceEditor, 'IOTASourceEditor');
    AddEditor(IOTAFormEditor, 'IOTAFormEditor');
    {$IFDEF GX_VER160_up}
    AddEditor(IOTASourceEditor70, 'IOTASourceEditor70');
    {$ENDIF GX_VER160_up}
    {$IFDEF GX_VER170_up}
    AddEditor(IOTAEditorContent, 'IOTAEditorContent');
    {$ENDIF GX_VER170_up}
    AddEditor(IOTAProjectResource, 'IOTAProjectResource');
    AddEditor(IOTATypeLibEditor, 'IOTATypeLibEditor');
  end;

var
  Project: IOTAProject;
  ModuleInfo: IOTAModuleInfo;
  Module: IOTAModule;
  Editor: IOTAEditor;
  i: Integer;
  List: TStringList;
  ModuleServices: IOTAModuleServices;

  procedure AddEditorDetails(AModule: IOTAModule);
  var
    j: Integer;
  begin
    for j := 0 to AModule.ModuleFileCount - 1 do
    begin
      Editor := AModule.ModuleFileEditors[j];
      Assert(Assigned(Editor));
      List.Add(Format('    Editor: %s: (Implements: %s)', [Editor.FileName, GetEditorInterfaces(Editor)]));
    end;
  end;

begin
  List := TStringList.Create;
  try
    Project := GxOtaGetCurrentProject;
    if not Assigned(Project) then
    begin
      ModuleServices := BorlandIDEServices as IOTAModuleServices;
      for i := 0 to ModuleServices.ModuleCount - 1 do
      begin
        Module := ModuleServices.Modules[i];
        if Assigned(Module) then
        begin
          List.Add(Format('  Module: %s:', [Module.FileName]));
          AddEditorDetails(Module);
        end;
      end;
    end
    else
    begin
      List.Add(Format('Project: %s (Personality: %s):', [Project.FileName, GxOtaGetCurrentProjectPersonality]));
      AddEditorDetails(Project);
      List.Add('');

      for i := 0 to Project.GetModuleCount - 1 do
      begin
        ModuleInfo := Project.GetModule(i);
        Assert(Assigned(ModuleInfo));
        List.Add(Format('ModuleInfo: %s (Name: %s, FormName: %s, DesignClass: %s):', [ModuleInfo.FileName, ModuleInfo.Name, ModuleInfo.FormName, ModuleInfo.DesignClass]));
        Module := GxOtaGetModule(ModuleInfo.FileName);
        if not Assigned(Module) then
          List.Add('  <No IOTAModule>')
        else
        begin
          List.Add(Format('  Module: %s:', [Module.FileName]));
          AddEditorDetails(Module);
        end;
        List.Add('');
      end;
    end;
    MessageBox(0, PChar(List.Text), 'Modules and Editors', MB_ICONINFORMATION or MB_OK);
  finally
    FreeAndNil(List);
  end;
end;

procedure GxOtaShowIDEActions;
var
  Actions: TCustomActionList;
  Action: TContainedAction;
  Msg: string;
  ActionMsg: string;
  ActionCaption: string;
  ActionShortCut: TShortCut;
  i: Integer;
begin
  Actions := GxOtaGetIdeActionList;
  Assert(Assigned(Actions));
  Msg := '';
  for i := 0 to Actions.ActionCount - 1 do
  begin
    Action := Actions[i];
    ActionCaption := '';
    ActionShortCut := 0;
    if Action is TCustomAction then
    begin
      ActionCaption := (Action as TCustomAction).Caption;
      ActionShortCut := (Action as TCustomAction).ShortCut;
    end;
    ActionMsg := Format('%s: %s  Shortcut: %s (%d)  Category: %s  Class: %s', [Action.Name, ActionCaption, ShortCutToText(ActionShortCut), ActionShortCut, Action.Category, Action.ClassName]);
    if Msg = '' then
      Msg := ActionMsg
    else
      Msg := Msg + sLineBreak + ActionMsg;
  end;
  MessageBox(0, PChar(Msg), 'IDE Actions', MB_ICONINFORMATION or MB_OK)
end;

procedure GxOtaShowEditViewDetails;
var
  View: IOTAEditView;
  Buffer: IOTAEditBuffer;
  Msg: TStringList;
  Options: IOTABufferOptions;
  Window: INTAEditWindow;
  Form: TCustomForm;
  Block: IOTAEditBlock;
  Position: IOTAEditPosition;

  procedure Add(const Str: string);
  begin
    Msg.Add(Str);
  end;

  procedure AddFmt(const Str: string; const Args: array of const);
  begin
    Msg.Add(Format(Str, Args));
  end;
  
begin
  View := GxOtaGetTopMostEditView;
  if not Assigned(View) then
    raise Exception.Create('No active edit view');

  Msg := TStringList.Create;
  try
    Buffer := View.Buffer;
    if Assigned(Buffer) then
    begin
      AddFmt('FileName: %s', [Buffer.FileName]);
      AddFmt('Initial Date: %s', [DateTimeToStr(Buffer.GetInitialDate)]);
      AddFmt('Current Date: %s', [DateTimeToStr(Buffer.GetCurrentDate)]);

      Position := Buffer.EditPosition;
      if Assigned(Position) then
      begin
        AddFmt('Position.Character: %s', [Position.Character]);
        AddFmt('Position.Row: %d', [Position.Row]);
        AddFmt('Position.Column: %d', [Position.Column]);
        AddFmt('Position.IsSpecialCharacter: %s', [BooleanText(Position.IsSpecialCharacter)]);
        AddFmt('Position.IsWhiteSpace: %s', [BooleanText(Position.IsWhiteSpace)]);
        AddFmt('Position.IsWordCharacter: %s', [BooleanText(Position.IsWordCharacter)]);
        AddFmt('Position.LastRow: %d', [Position.LastRow]);
      end
      else
        Add('No IOTAEditView.Position');

      AddFmt('Buffer.IsModified: %s', [BooleanText(Buffer.IsModified)]);
      AddFmt('Buffer.IsReadOnly: %s', [BooleanText(Buffer.IsReadOnly)]);
      AddFmt('Buffer.BlockVisible: %s', [BooleanText(Buffer.BlockVisible)]);
      AddFmt('Buffer.Modified: %s', [BooleanText(Buffer.Modified)]);
      AddFmt('Buffer.GetLinesInBuffer: %d', [Buffer.GetLinesInBuffer]);
      AddFmt('Buffer.EditViewCount: %d', [Buffer.EditViewCount]);
      AddFmt('Buffer.BlockStart.CharIndex: %d', [Buffer.BlockStart.CharIndex]);
      AddFmt('Buffer.BlockStart.Line: %d', [Buffer.BlockStart.Line]);
      AddFmt('Buffer.BlockAfter.CharIndex: %d', [Buffer.BlockAfter.CharIndex]);
      AddFmt('Buffer.BlockAfter.Line: %d', [Buffer.BlockAfter.Line]);
      Options := Buffer.BufferOptions;
      if Assigned(Options) then
      begin
        AddFmt('AutoIndent: %s', [BooleanText(Options.AutoIndent)]);
        AddFmt('BackspaceUnindents: %s', [BooleanText(Options.BackspaceUnindents)]);
        AddFmt('CreateBackupFile: %s', [BooleanText(Options.CreateBackupFile)]);
        AddFmt('CursorThroughTabs: %s', [BooleanText(Options.CursorThroughTabs)]);
        AddFmt('GroupUndo: %s', [BooleanText(Options.GroupUndo)]);
        AddFmt('InsertMode: %s', [BooleanText(Options.InsertMode)]);
        AddFmt('KeepTrailingBlanks: %s', [BooleanText(Options.KeepTrailingBlanks)]);
        AddFmt('LeftGutterWidth: %d', [Options.LeftGutterWidth]);
        AddFmt('OverwriteBlocks: %s', [BooleanText(Options.OverwriteBlocks)]);
        AddFmt('PersistentBlocks: %s', [BooleanText(Options.PersistentBlocks)]);
        AddFmt('PreserveLineEnds: %s', [BooleanText(Options.PreserveLineEnds)]);
        AddFmt('RightMargin: %d', [Options.RightMargin]);
        AddFmt('SmartTab: %s', [BooleanText(Options.SmartTab)]);
        AddFmt('SyntaxHighlight: %s', [BooleanText(Options.SyntaxHighlight)]);
        AddFmt('TabStops: %s', [Options.TabStops]);
        AddFmt('UndoAfterSave: %s', [BooleanText(Options.UndoAfterSave)]);
        AddFmt('UndoLimit: %d', [Options.UndoLimit]);
        AddFmt('UseTabCharacter: %s', [BooleanText(Options.UseTabCharacter)]);
      end
      else
        Add('No buffer options');
    end
    else
      Add('No edit buffer filename');

    Block := View.Block;
    if Assigned(Block) then
    begin
      AddFmt('Block.IsValid: %s', [BooleanText(Block.IsValid)]);
      AddFmt('Block.Visible: %s', [BooleanText(Block.Visible)]);
      AddFmt('Block.EndingColumn: %d', [Block.EndingColumn]);
      AddFmt('Block.EndingRow: %d', [Block.EndingRow]);
      AddFmt('Block.StartingColumn: %d', [Block.StartingColumn]);
      AddFmt('Block.StartingRow: %d', [Block.StartingRow]);
      AddFmt('Block.Style: %s', [GetEnumName(TypeInfo(TOTABlockType), Ord(Block.Style))]);
      AddFmt('Block.Text: %s', [HackBadIDEUTF8StringToString(Block.Text)]);
    end
    else
      Add('No IOTAEditView.Block');

    AddFmt('View.BottomRow: %d', [View.BottomRow]);
    AddFmt('View.LastEditColumn: %d', [View.LastEditColumn]);
    AddFmt('View.LastEditRow: %d', [View.LastEditRow]);
    AddFmt('View.LeftColumn: %d', [View.LeftColumn]);
    AddFmt('View.RightColumn: %d', [View.RightColumn]);
    AddFmt('View.TopRow: %d', [View.TopRow]);
    AddFmt('View.CursorPos.Line: %d', [View.CursorPos.Line]);
    AddFmt('View.CursorPos.Col: %d', [View.CursorPos.Col]);
    AddFmt('View.TopPos.Line: %d', [View.TopPos.Line]);
    AddFmt('View.TopPos.Col: %d', [View.TopPos.Col]);
    AddFmt('View.ViewSize.cx: %d', [View.ViewSize.cx]);
    AddFmt('View.ViewSize.cy: %d', [View.ViewSize.cy]);

    if Supports(View, INTAEditWindow, Window) then
    begin
      Add('Supports INTAEditWindow');
      Form := Window.Form;
      if Assigned(Form) then
      begin
        AddFmt('Form Class: %s', [Form.ClassName]);
        AddFmt('Form Caption: %s', [Form.Caption]);
        AddFmt('Form Top: %d  Left: %d  Height: %d  Width: %d', [Form.Top, Form.Left, Form.Height, Form.Width]);
        if Assigned(Form.ActiveControl) then
          AddFmt('Form.ActiveControl ClassName: %s  TextLen: %d', [Form.ActiveControl.ClassName, Form.ActiveControl.GetTextLen]);
      end
      else
        Add('No INTAEditWindow.Form');
    end
    else
      Add('No support for INTAEditWindow');
  finally
    MessageBox(Application.Handle, PChar(Msg.Text), 'Edit View Details', 0);
  end;
end;

function HackBadIDEUTF8StringToString(const S: string): string;
begin
  // Work around the fact that IOTAEditBlock.Text claims to return a
  // string/UnicodeString but actually returns a corrupt UTF-8 string
  // In Delphi 2009, through update 2
  if GetBorlandIDEVersion in [ideRS2009, ideRS2009U1, ideRS2009U2] then
    Result := UTF8ToUnicodeString(PAnsiChar(AnsiString(S)))
  else
    Result := S;
end;

function HackBadEditorStringToNativeString(const S: string): string;
var
  len: Integer;
begin
  if IDEEditorEncodingIsUTF8 then
    Result := IDEEditorStringToString(UTF8String(Pointer(S)))
  else
    Result := S;
  len := Length(Result);
  while (len >0) and (Result[len] = #0) do begin
    Dec(len);
    Result := LeftStr(Result, len);
  end;
end;

{ TBaseIdeNotifier }

function TBaseIdeNotifier.AddNotifierToIDE: Boolean;
begin
  FNotifierIndex := GxOtaGetIDEServices.AddNotifier(Self);
  Result := (FNotifierIndex <> InvalidNotifierIndex);
end;

function TBaseIdeNotifier.RemoveNotifierFromIDE: Boolean;
var
  SavedIndex: Integer;
begin
  Result := False;
  if FNotifierIndex = InvalidNotifierIndex then
    Exit;
  SavedIndex := FNotifierIndex;
  FNotifierIndex := InvalidNotifierIndex;
  GxOtaGetIDEServices.RemoveNotifier(SavedIndex);
  Result := True;
end;

procedure TBaseIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
  // Nothing
end;

procedure TBaseIdeNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
  // Nothing
end;

procedure TBaseIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  // Nothing
end;

destructor TBaseIdeNotifier.Destroy;
begin
  if FNotifierIndex <> InvalidNotifierIndex then
    RemoveNotifierFromIDE;
  inherited Destroy;
end;

{ TBaseDebuggerNotifier }

function TBaseDebuggerNotifier.AddNotifierToIDE: Boolean;
begin
  FNotifierIndex := GxOtaGetDebuggerServices.AddNotifier(Self);
  Result := (FNotifierIndex <> InvalidNotifierIndex);
end;

function TBaseDebuggerNotifier.RemoveNotifierFromIDE: Boolean;
var
  SavedIndex: Integer;
begin
  Result := False;
  if FNotifierIndex = InvalidNotifierIndex then
    Exit;
  SavedIndex := FNotifierIndex;
  FNotifierIndex := InvalidNotifierIndex;
  GxOtaGetDebuggerServices.RemoveNotifier(SavedIndex);
  Result := True;
end;

destructor TBaseDebuggerNotifier.Destroy;
begin
  RemoveNotifierFromIDE;
  inherited;
end;

procedure TBaseDebuggerNotifier.BreakpointAdded({$IFDEF GX_VER170_up} const {$ENDIF} Breakpoint: IOTABreakpoint);
begin
  // Nothing
end;

procedure TBaseDebuggerNotifier.BreakpointDeleted({$IFDEF GX_VER170_up} const {$ENDIF} Breakpoint: IOTABreakpoint);
begin
  // Nothing
end;

procedure TBaseDebuggerNotifier.ProcessCreated({$IFDEF GX_VER170_up} const {$ENDIF} Process: IOTAProcess);
begin
  // Nothing
end;

procedure TBaseDebuggerNotifier.ProcessDestroyed({$IFDEF GX_VER170_up} const {$ENDIF} Process: IOTAProcess);
begin
  // Nothing
end;

{ TProjectChangedNotifier }

constructor TProjectChangedNotifier.Create(_OnProjectChanged: TNotifyEvent);
begin
  inherited Create;
  Assert(Assigned(_OnProjectChanged));
  FOnProjectChanged := _OnProjectChanged;
end;

procedure TProjectChangedNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
var
  ThisProject: string;
begin
  if NotifyCode = ofnActiveProjectChanged then begin
    // Check if the project actually has changed
    ThisProject := GxOtaGetCurrentProjectFileName;
    if ThisProject = FLastProject then
      Exit; //==>
    FLastProject := ThisProject;
    FOnProjectChanged(Self);
  end;
end;

initialization

finalization
  // We need to clear all tool messages due to a bug in the IDE's
  // implementation of IOTACustomMessage:
  // Messages are apparently not cleared upon shutdown of the
  // IDE - and if we write our own IOTACustomMessage, the IDE
  // will try to access these. But since the DLL is gone -> AV.
  //GxOtaClearToolMessages;

end.
