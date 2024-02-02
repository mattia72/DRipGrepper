unit GX_GenericUtils;

{$I GX_CondDefine.inc}

{$IFOPT D+}
{$MESSAGE WARN 'Compiling with Debug information turned on'}
{$ENDIF}

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  SysUtils, Classes, Dialogs, SyncObjs, Graphics, Controls, Forms, StdCtrls,
  UITypes, // if you get a compile error here, add UITypes=Dialogs to the unit aliases
  Types, CheckLst, TypInfo, ExtCtrls, ComCtrls,
  u_dzErrorThread,
  GX_StringList;

const
  AllFilesWildCard = '*.*';
  EmptyString = '';
  GxSentenceEndChars = ['.', '!', '?', #$bf, #$a1]; // last two are questiondown uand exclamdown
  MaxSmallInt = 32767;
  MaxEditorCol = {$IFDEF GX_VER160_up} MaxSmallInt {$ELSE} 1023 {$ENDIF};

resourcestring
  SAllAlphaNumericChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890';

type
  // NativeInt and NativeUInt are wrongly declared as 8 bytes in size in Delphi 6 to 2007,
  // so we can't use them. Instead we declare our own types which, since GExperts is a
  // 32 IDE plugin are both 32 bits.
  // Originally this simply redeclared NativeInt as Integer for older versions, but
  // that made it hard to find accidental usages of the wrong type.
  GXNativeInt = Integer;
  GXNativeUInt = Cardinal;
{$IFNDEF GX_DELPHI2009_UP}
  UInt32 = Cardinal;
{$ENDIF}

type
  {$IFDEF GX_VER160_up}
  IDEEditBufferString = UTF8String;
  {$ELSE}
  IDEEditBufferString = AnsiString;
  {$ENDIF}

  TGXSyntaxHighlighter = (gxpPlaceHolder, gxpNone, gxpPAS, gxpCPP, gxpHTML, gxpSQL, gxpCS, gxpXML);

  TGXSyntaxData = record
    Name: string;
    Identifier: string;
    Highlighter: TGXSyntaxHighlighter;
    Visible: Boolean;
    Extensions: string;
  end;

const
  GXSyntaxInfo: array[TGXSyntaxHighlighter] of TGXSyntaxData = (
    (
      Name: 'Place Holder';
      Identifier: '_';
      Highlighter: gxpPlaceHolder;
      Visible: False;
      Extensions: '';
    ),
    (
      Name: 'None';
      Identifier: 'N';
      Highlighter: gxpNone;
      Visible: True;
      Extensions: '*.txt;*.ini;*.me';
    ),
    (
      Name: 'Delphi';
      Identifier: 'P';
      Highlighter: gxpPAS;
      Visible: True;
      Extensions: '*.pas;*.dpr;*.inc';
    ),
    (
      Name: 'C++';
      Identifier: 'C';
      Highlighter: gxpCPP;
      Visible: True;
      Extensions: '*.cpp;*.h;*.c;*.hpp';
    ),
    (
      Name: 'HTML';
      Identifier: 'H';
      Highlighter: gxpHTML;
      Visible: True;
      Extensions: '*.html;*.htm;*.asp;*.php';
    ),
    (
      Name: 'SQL';
      Identifier: 'S';
      Highlighter: gxpSQL;
      Visible: True;
      Extensions: '*.sql';
    ),
    (
      Name: 'C#';
      Identifier: '#';
      Highlighter: gxpCS;
      Visible: True;
      Extensions: '*.cs';
    ),
    (
      Name: 'XML';
      Identifier: 'X';
      Highlighter: gxpXML;
      Visible: True;
      Extensions: '*.xml;*.dproj;*.bdsproj;*.groupproj;*.local;*.manifest;*.resx';
    ));

type
  TVersionNumber = packed record
    IsValid: Boolean;
    case Boolean of
      True:  ( dwFileVersionMS: DWORD;    { e.g. $00030075 = "3.75" }
               dwFileVersionLS: DWORD;    { e.g. $00000031 = "0.31" }
              );
      False: ( Minor: Word;
               Major: Word;
               Build: Word;
               Release: Word;
              );
    end;

  {$ALIGN ON}
  _OSVERSIONINFOEXA = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array [0..127] of AnsiChar;
    wServicePackMajor: Word;
    wServicePackMinor: Word;
    wSuiteMask: Word;
    wProductType: Byte;
    wReserved: Byte;
  end;
  TOSVersionInfoExA = _OSVERSIONINFOEXA;
  TOSVersionInfoEx = TOSVersionInfoExA;

  EVersionInfoNotFound = class(Exception);
  EGXFileNotFound = class(Exception);

  TListBoxCheckAction = (chAll, chNone, chInvert);

  TForEachControlProc = procedure(Control: TControl) of object;

//                     System
//
//

// Find SubString in S; do not consider case;
// this works exactly the same as the Pos function,
// except for case-INsensitivity.
function CaseInsensitivePos(const Pat, Text: string): Integer; overload;
function AnsiCaseInsensitivePos(const SubString, S: string): Integer;
// Same as CaseInsensitivePos, except that searching for the substring starts
// at (the 1-based) index StartIndex; occurences of the substring starting
// *before* StartIndex are ignored. E.g. CaseInsensitivePosFrom(Pat, Text, 1)
// always returns the same as CaseInsensitivePos(Pat, Text).
function CaseInsensitivePosFrom(const Pat, Text: string; StartIndex: Integer): Integer;
function PosFrom(const Pat, Text: string; StartIndex: Integer): Integer;

//  See if a string is in the array.  Case insensitive.
function StringInArray(const S: string; const SArray: array of string): Boolean;

// Tokenize Source with substrings separated by Delimiter
// into Dest stringlist.
// At termination Dest contains all substrings in Source
procedure AnsiStrTok(const Source, Delimiter: string; Dest: TStrings);

// Return the contents of the first ' or " delimited item in the string
// or the whole string, if ity is not quoted.  Strings with an unmatched
// quote return an empty string
function GetQuotedToken(const Str: string): string;

// Expand tabs into spaces.  The input should be a single line.
function ExpandTabsInLine(const AText: string; ATabSize: Integer): string;

// Find the line with the minimal space indent, ignoring empty lines.
// Note: This does not expand tabs, so the lines must be pre-expanded.
procedure FindMinIndent(ALines: TStrings; var LineIndex, SpaceCount: Integer);

const
  CR = #13;
  LF = #10;
  CRLF = CR + LF;
  CRLFLength = Length(CRLF);

// Determine if a character represents white space
function IsCharWhiteSpace(C: Char): Boolean;
function IsCharWhiteSpaceOrNull(C: Char): Boolean; {$IFDEF SupportsInline} inline; {$ENDIF}
function IsCharWhiteSpaceOrControl(C: Char): Boolean; {$IFDEF SupportsInline} inline; {$ENDIF}
function IsCharLineEnding(C: Char): Boolean; {$IFDEF SupportsInline} inline; {$ENDIF}
function IsCharLineEndingOrNull(C: Char): Boolean; overload; {$IFDEF SupportsInline} inline; {$ENDIF}
function IsCharAlphaNumeric(C: Char): Boolean; overload;
function IsCharNumeric(C: Char): Boolean; overload;
function IsCharHexDigit(C: Char): Boolean; {$IFDEF SupportsInline} inline; {$ENDIF}
function IsCharTab(C: Char): Boolean; {$IFDEF SupportsInline} inline; {$ENDIF}
function IsCharSymbol(C: Char): Boolean; overload; {$IFDEF SupportsInline} inline; {$ENDIF}
function IsCharControl(C: Char): Boolean; {$IFDEF SupportsInline} inline; {$ENDIF}
// See if a character is a valid locale identifier character, _, or 0..9
function IsCharIdentifier(C: Char): Boolean; overload;
function IsCharIdentifierStart(C: Char): Boolean; overload;
{$IFDEF UNICODE}
function IsCharIdentifierStart(C: AnsiChar): Boolean; overload;
function IsCharIdentifier(C: AnsiChar): Boolean; overload;
function IsCharSymbol(C: AnsiChar): Boolean; overload; {$IFDEF SupportsInline} inline; {$ENDIF}
function IsCharNumeric(C: AnsiChar): Boolean; overload; {$IFDEF SupportsInline} inline; {$ENDIF}
function IsCharAlphaNumeric(C: AnsiChar): Boolean; overload; {$IFDEF SupportsInline} inline; {$ENDIF}
function IsCharLineEndingOrNull(C: AnsiChar): Boolean; overload; {$IFDEF SupportsInline} inline; {$ENDIF}
function GetFileEncoding(const FileName: string): TEncoding;
{$ELSE ~UNICODE}
function CharInSet(C: Char; CSet: TSysCharSet): Boolean; {$IFDEF SupportsInline} inline; {$ENDIF}
function AnsiStrAlloc(Size: Cardinal): PChar; {$IFDEF SupportsInline} inline; {$ENDIF}
function IsLeadChar(C: Char): Boolean; {$IFDEF SupportsInline} inline; {$ENDIF}
function IsCharIdentifier(C: WideChar): Boolean; overload;
{$ENDIF UNICODE}

// Transforms all consecutive sequences of #10, #13, #32, and #9 in Str
// into a single space, and strips off whitespace at the beginning and
// end of the string
function CompressWhiteSpace(const Str: string): string;

// Left trim TrimChars from Value and return number of trimmed characters
function LeftTrimChars(var Value: string; const TrimChars: TSysCharSet = [#9, #32]): Integer;

// Left trim up to AMaxCount of TrimChars from Value and return the trimmed string
function LeftTrimNChars(const AValue: string; const TrimChars: TSysCharSet = [#9, #32];
  AMaxCount: Integer = 0): string;

// See if a string begins/ends with a specific substring
function StrBeginsWith(const SubStr, Str: string; CaseSensitive: Boolean = True): Boolean;
function StrEndsWith(const SubStr, Str: string; CaseSensitive: Boolean = True): Boolean;
// See is a string contains another substring
function StrContains(const SubStr, Str: string; CaseSensitive: Boolean = True): Boolean;
function StrContainsWhiteSpace(const Str: string): Boolean;
// Check for empty (when trimmed) strings
function NotEmpty(const Str: string): Boolean;
function IsEmpty(const Str: string): Boolean;

// Delete up to n of the rightmost characters in the string
function DeleteRight(const Value: string; NumChars: Integer): string;

// Remove surrounding characters, if present
function RemoveSurroundingChars(SurroundingChar: Char; const Str: string): string;

// Get the upper/lower case equivalent for a Char, dependent on the locale
function AnsiUpperChar(C: Char): Char;
function AnsiLowerChar(C: Char): Char;

// Change the case of a string
function ToggleCase(const S: string): string;
function SentenceCase(const S: string): string;
function TitleCase(const S: string): string;

// Get the index of the first alphebetic character in the string
function FirstAlphaInString(const S: string): Integer;

// Parse a string like TClass1.TClass2.MethodName into parts
procedure DecomposeClassMethodString(const S: string; var MethodName, ClassName, NestedClasses: string);
function GetMethodName(const S: string): string;

// Add an item to the top of a MRU string list.  Can optionally strip
// off a trailing path delimiter.
procedure AddMRUString(const Text: string; List: TStrings; DeleteTrailingDelimiter: Boolean; AllowBlank: Boolean = False); overload;
procedure AddMRUString(Text: string; List: TStrings; DeleteTrailingDelimiter: Boolean;
  MaxListCount, MaxTextLength: Integer; AllowBlank: Boolean = False); overload;
// Delete a string from a string list
procedure DeleteStringFromList(List: TStrings; const Item: string);
///<summary>
/// Ensure a string is in the given list.
/// @returns a string referencing the string stored in the list (either the original one
///          that was just added, or the one the was already stored). </summary>
function EnsureStringInList(List: TStrings; const Item: string): string;
///<summary>
/// checks whether the given string matches the given filter
/// @param Filter is the filter to use
/// @param s is the string to check
/// @param CaseSensitive if True, do a case sensitive match
/// @param MatchAnywhere if True, check for the filter anywhere in the string
///                         false, check for the filter at the begin of the string
/// @param returns True, if the string matches the filter
///                False, if it doesn't <summary>
//function StrMatchesFilter(const _Filter: string; _s: string;
//  _CaseSensitive: Boolean; _MatchAnywhere: Boolean): Boolean;
///<summary>
/// Calls StrMatchesFilter for each string in Source and adds it to Dest if it matches.
/// Also adds the Source objects for matching strings. </summary>
procedure FilterStringListMatchStart(Source, Dest: TStrings; const Filter: string;
  CaseSensitive: Boolean = True);
procedure FilterStringListMatchAnywhere(Source, Dest: TStrings; const Filter: string;
  CaseSensitive: Boolean = True);overload;
procedure FilterStringListMatchAnywhere(Source, Dest: TStrings; Filter: TStrings;
  CaseSensitive: Boolean = True); overload;
procedure AddStringsPresentInString(Source, Dest: TStrings; const FilterString: string);
// Return the comma text of a string list with a space after each comma
function SpacedCommaText(Source: TStrings): string;
// Sort a string list longest first then by a normal sort
function SortStringListByLength(List: TStringList; Index1, Index2: Integer): Integer;

// Adds a correct trailing slash to Directory if it is not present yet
function AddSlash(const Directory: string): string;

// Removes the trailing slash from Directory if it is present
function RemoveSlash(const Directory: string): string;

// Sees if a directory exists and can be written to.  It may create the directory.
function PrepareDirectoryForWriting(const Dir: string): Boolean;

// Determine if a passed in path/file is absolute or relative
function IsPathAbsolute(const FileName: string): Boolean;

function GXPathCombine(out ADest: string; const ADir, AFile: string): Boolean;

type
  TGXPathRelativePathToOption = (proOnlyInSubDir, proFromIsDirectory, proToIsDirectory);
  TGXPathRelativePathToOptions = set of TGXPathRelativePathToOption;

function GXPathRelativePathTo(out APath: string; const AFrom, ATo: string;
  AOptions: TGXPathRelativePathToOptions = [proOnlyInSubDir, proFromIsDirectory]): Boolean;

// Change to a directory.  Returns True on success.
function SafeChangeDirectory(const NewDir: string): Boolean;

// Return a complete file name with a path given a directory/file name
function BuildFileName(const Path, FileName: string): string;

// Returns AString padded with white space if
// StringLength > Length(AString).
// Truncates to StringLength, otherwise.
function GetPaddedString(const AString: string; const StringLength: Integer): string;

// Get the position of the last instance of a character in a string
function LastCharPos(const S: string; C: Char): Integer;

// Get character at some position or #0 for invalid positions
function StrCharAt(const S: string; Pos: Integer): Char;

// Returns the size of any EOL character(s) at a given position (0 if none)
function EOLSizeAtPos(const S: string; Pos: Integer): Integer;

// this checks for the case that either the formatter or the IDE
// removed or added a CRLF at the end of the file
// checking the length first prevents costly multiple
// comparisons of potentially large strings
function SourceDifferentApartFromTrailingLineBreak(const OrigSource, NewSource: TGXUnicodeString): Boolean;

// Returns True if S string contains an EOL on the end
function HasTrailingEOL(const S: string): Boolean;

// Remove the last EOL character from a string
// EOL can be one or two characters long
// Useful for processing strings read from memo controls
procedure RemoveLastEOL(var S: string);
function RemoveTrailingEOL(const S: string): string;

// Return the last line of a multiline string (embedded CR or LF)
function LastLineOf(const S: string): string;

// Remove the prefix characters in S that appear before the
// separator character, if the separator is present and then
// remove any found prefix from S.
function RemoveCharsBefore(var S: string; const PrefixSep: Char): string;

// Reverse the order of the characters in s. Example: For an input
// of "123", after calling ReverseString the sequence will be "321".
function ReverseString(const S: string): string;

// Appends Source to Destination. See TStrings.Append
procedure AppendStrings(const Destination, Source: TStrings);

// Sort AStrings case-sensitivitely in ascending order.
// TStringList.Sort is case-INsensitive.
procedure SortStringsCaseSensitive(const AStrings: TStrings);

function EnforceMinMax(Value, Min, Max: Integer): Integer;

// Find control named ControlName which is a direct
// or indirect child window of WindowContainer.
// In contrast to FindComponent, this function
// iterates over the Parent property of WindowContainer,
// not over the Owner property.
function FindChild(const WindowContainer: TWinControl; const ControlName: string): TControl;
procedure IterateOverControls(Control: TControl; ActionProc: TForEachControlProc);
function TryFocusControl(Control: TWinControl): Boolean;

procedure CenterLineInEdit(RichEdit: TRichEdit; LineNum: Integer);

// See is this component owns an object of the specified class name
function ComponentOwnsClass(Component: TComponent; const ClassName: string): Boolean;

// Add a horizontal scrollbar to a TCustomListBox if any of the items
// are longer than the current TCustomListBox width
procedure ListboxHorizontalScrollbar(Listbox: TCustomListBox);
// Get the width of a default scrollbar
function GetScrollbarWidth: Integer;

procedure SizeComboDropdownToItems(Combo: TCustomComboBox; AMax: Integer = 450);

// Make a Button/CheckBox multiline (WordWrap).  D6 does not have this property.
procedure MakeMultiline(Control: TButtonControl);

// Check, uncheck, or invert the checks in a TCheckListBox
procedure SetListBoxChecked(CheckList: TCheckListBox; Action: TListboxCheckAction);

procedure ListBoxDeleteSelected(ListBox: TListBox);
procedure ListViewDeleteSelected(ListView: TListView);
procedure ListViewSelectAll(ListView: TListView);
procedure ListViewResizeColumn(ListView: TListView; ColumnIndex: Integer);


function FindTreeNode(Tree: TTreeView; const NodeText: string): TTreeNode;

procedure SetToolbarGradient(ToolBar: TToolBar; Enabled: Boolean = True);

// Enable/disable a control and all children
procedure SetEnabledOnControlAndChildren(Control: TControl; Value: Boolean);

// Set a control's font to the default for this system
function SetDefaultFont(Control: TControl): string;
// Set a control's font to bold
procedure SetFontBold(Control: TControl);
// Set a control's font to underline
procedure SetFontUnderline(Control: TControl);
// Change a font size relative to the current size
procedure SetFontSize(Control: TControl; SizeChange: Integer);
// Change a font size relative to the current size
procedure SetFontColor(Control: TControl; Color: TColor);
// Get the average low-ASCII chracter witdh for the control's current font
function GetAverageCharWidth(Control: TControl): Integer;

// Sets the ParentBackground property, if supported by the current VCL version
// This needs to be false for colored TPanels that can appear in a themed app
procedure SetParentBackgroundValue(Panel: TCustomPanel; Value: Boolean); overload;
procedure SetParentBackgroundValue(GroupBox: TGroupBox; Value: Boolean); overload;

// Determine if the passed in class inherits from ClassName without
// linking in the units that contain ClassName
function InheritsFromClass(ClassType: TClass; const ClassName: string): Boolean;

// Return the text representation (i.e. "True" or "False")
// for Value.
function BooleanText(const Value: Boolean): string;

// Dump a list of components owned by StartComponent or that are children of
// StartComponent to the GExperts debug window.
procedure OutputComponentList(const StartComponent: TComponent; Owned: Boolean = True);

// Center Form in the current working area (Desktop)
procedure CenterForm(const Form: TCustomForm);

// Get the screen work area.  Optionally pass in a form to
// specify a specific monitor.
function GetScreenWorkArea(const Form: TCustomForm = nil): TRect;

// Ensure a form is completely within the visible working area
procedure EnsureFormVisible(const Form: TCustomForm);

// Get the window handle's class name as a Delphi string
function GetWindowClassName(WinHandle: THandle): string;

// Take a bitmap with a transparent color and convert it to a TIcon
procedure ConvertBitmapToIcon(const Bitmap: Graphics.TBitmap; Icon: TIcon);

// Copy the properties of a TPersistent to another TPersistent
procedure ClonePersistent(Source, Dest: TPersistent);
// Get a list of published properties
procedure GetPropertyNames(AClass: TClass; PropertyNames, PropertyTypeNames,
  PropertyTypesToLookFor: TStrings);

procedure AssertIsDprOrPas(const FileName: string);
procedure AssertIsDprOrPasOrInc(const FileName: string);
procedure AssertIsPasOrInc(const FileName: string);
function IsDprOrPas(const FileName: string): Boolean;
function IsPascalSourceFile(const FileName: string): Boolean;
function IsBdsProjectFile(const FileName: string): Boolean;
function IsProjectSource(const FileName: string): Boolean;
function IsBdsSourceFile(const FileName: string): Boolean;
function IsDpr(const FileName: string): Boolean;
function IsBpr(const FileName: string): Boolean;
function IsBpk(const FileName: string): Boolean;
function IsBpg(const FileName: string): Boolean;
function IsPas(const FileName: string): Boolean;
function IsInc(const FileName: string): Boolean;
function IsDfm(const FileName: string): Boolean;
function IsForm(const FileName: string): Boolean;
function IsXfm(const FileName: string): Boolean;
function IsNfm(const FileName: string): Boolean;
function IsDpk(const FileName: string): Boolean;
function IsDcp(const FileName: string): Boolean;
function IsBdsgroup(const FileName: string): Boolean;
function IsGroupproj(const FileName: string): Boolean;
function IsBdsproj(const FileName: string): Boolean;
function IsDproj(const FileName: string): Boolean;
function IsBdsprojOrDproj(const FileName: string): Boolean;
function IsPackage(const FileName: string): Boolean;
function IsDelphiPackage(const FileName: string): Boolean;
function IsCsproj(const FileName: string): Boolean;
function IsHtml(const FileName: string): Boolean;
function IsWebFile(const FileName: string): Boolean;
function IsCs(const FileName: string): Boolean;
function IsVbFile(const FileName: string): Boolean;
function IsExecutable(const FileName: string): Boolean;
function IsCppSourceModule(const FileName: string): Boolean;
function IsCpp(const FileName: string): Boolean;
function IsC(const FileName: string): Boolean;
function IsH(const FileName: string): Boolean;
function IsToDo(const FileName: string): Boolean;
function IsDcu(const FileName: string): Boolean;
function IsDcuil(const FileName: string): Boolean;
function IsTypeLibrary(const FileName: string): Boolean;
function IsKnownSourceFile(const FileName: string): Boolean;
function IsTextFile(const FileName: string): Boolean;
function IsXML(const FileName: string): Boolean;
function IsXMLFormat(const FileName: string): Boolean;
function IsSQL(const FileName: string): Boolean;

// RTTI helpers
function FindPropInfo(Instance: TObject; const PropName: string): PPropInfo;
function FindTypeInfo(Instance: TObject; const PropName: string): PTypeInfo;
function IsPropWriteable(Instance: TObject; const PropName: string): Boolean;
procedure RaisePropertyError(const PropName: string);
function ApplyValueToSetProperty(Obj: TObject; const APropertyName, Value: string): Integer;
function GetEnumValueFromStr(Obj: TObject; const PropertyName, Value: WideString): Integer;

// Returns True if the file extension of FileName matches
// an extension passed in the list of file extensions;
// FileExtensions is a semicolon delimited list of
// extensions without wildcards, e.g.
//   FileMatchesExtensions('MyFile.pas', '.pas;.dpr;.inc');
// The test for inclusion is case-insensitive.
function FileMatchesExtensions(const FileName, FileExtensions: string): Boolean; overload;
function FileMatchesExtensions(const FileName: string; FileExtensions: array of string): Boolean; overload;
function FileMatchesExtension(const FileName, FileExtension: string): Boolean;

// Return < 0 if S1 < S2. Return > 0 if S1 > S2.
// Return = 0 if S1 = S2. The comparison is case-insensitive.
function CompareFileName(const S1, S2: string): Integer;

// Returns True if S1 and S2 contain the same file name.
// The comparison is case-insensitive.
function SameFileName(const S1, S2: string): Boolean;

// Returns True if S1 and S2 contain the same path name.
// The comparison is case-insensitive and excludes trailing path delimiters.
function SamePathName(const S1, S2: string): Boolean;


//
// File utility functions.
//

// Return file size of FileName.
function GetFileSize(const FileName: string): Integer;
// Return file modified date of FileName.
function GetFileDate(const FileName: string): TDateTime;

// Get a text file's contents as a widestring.  Supports ASCII/ANSI, UTF8/16/32
function FileToWideString(const FileName: WideString): WideString;

// Convert an ANSI string stream to a WideString
procedure AnsiStreamToWideString(Stream: TStream; var Data: WideString);

// Get the Windows version information for a file
function GetFileVersionNumber(const FileName: string;
  MustExist: Boolean = True; MustHaveVersion: Boolean = True): TVersionNumber;
function GetFileVersionString(const FileName: string;
  MustExist: Boolean = True; MustHaveVersion: Boolean = True): string;

// Executes FileName and passes Parameters to the application
// If RaiseException is True, an exception is raised on failure
function GXShellExecute(const FileName, Parameters: string; const RaiseException: Boolean): Boolean;
// opens a file or url
function GXShellOpen(const _Parameters: string; const RaiseException: Boolean): Boolean;

// Get a Windows OS folder path.  Pass in one of the ShlObj.CSIDL_ constants
function GetSpecialFolderPath(const FolderID: Integer): string;
function GetUserApplicationDataFolder: string;
function GetUserLocalApplicationDataFolder: string;

///<summary>
/// Find the given file in the list of directories.
///  @returns true, if the file has been found </sumary>
function FindFileInSearchPath(const _fn: string; _SearchPath: TStrings; out _FullFn: string): Boolean;

// See if the current OS is Windows
function RunningWindows: Boolean;

// Get a string representing the OS and version
function GetOSString: string;
function IsWindowsVistaOrLater: Boolean;

// Get the currently logged-in username, or '' if it is not vailable
function GetCurrentUser: string;

// Extract the file extension from FileName and return it in all UPPERCASE.
function ExtractUpperFileExt(const FileName: string): string;

// Extract the pure file name and return it.
// The "pure" file name is the file name without path information
// and without a file extension.
function ExtractPureFileName(const FileName: string): string;

function FileIsReadOnly(const FileName: string): Boolean;
function CanWriteToDirectory(const Dir: string): Boolean;
function CanCreateFile(const FileName: string): Boolean;

///<summary>
/// Copies a single file to the Clipboard.
/// @raises Exception if anything goes wrong, in particular if the filename is empty
///                   or  the file does not exist. </summary>
procedure CopyFileToClipboard(const _Filename: string);

// Displays a directory selection box.
// Returns True if the user selected a directory; Dir then
// contains the selected directory.
// Returns False if the user did not select a directory.
function GetDirectory(var Dir: string; Owner: TCustomForm = nil): Boolean; overload;
function GetDirectory(const Caption: string; var Dir: string; Parent: TWinControl = nil): Boolean; overload;

// Displays the file selection box passed as Dialog.
// Returns True if the user selected a file
// Returns False if the user did not select a file.
// Under Kylix, locking across threads is performed.
// Also saves and restores the current directory.
function GetOpenSaveDialogExecute(Dialog: TOpenDialog): Boolean;

///<summary>
/// Shows a TOpenDialog with the given title.
/// @param Extension is the file extension without a leading dot.
/// @param Filter is a file filter of the form '<description>|<mask>', if emtpy
/// a file filter will be created for the given extension with the name "<extension> files".
/// If the user clicks OK, the function returns true and the filename will contain the selected file. </summary>
function ShowOpenDialog(const Title: string; const Extension: string; var Filename: string;
  const Filter: string = ''): boolean; overload;

///<summary>
/// Variant of ShowOpenDialog that allows to open multiple files </summary>
function ShowOpenDialog(const Title: string; const Extension: string; Files: TStrings;
  const Filter: string = ''): boolean; overload;

///<summary>
/// Shows a TSaveDialog with the given title.
/// @param Extension is the file extension without a leading dot.
/// @param Filter is a file filter of the form '<description>|<mask>', if emtpy
/// a file filter will be created for the given extension with the name "<extension> files".
/// If the user clicks OK, the function returns true and the filename will contain the selected file. </summary>
function ShowSaveDialog(const Title: string; const Extension: string; var Filename: string;
  const Filter: string = ''): boolean;

function MakeDialogExtensionString(const Ext: string): string;

// Returns True if FileWildcard matches FileName, False otherwise.
// If no file extension is given in FileWildcard, ".*" is assumed
// to be the (wildcard) extension.
// When IgnoreCase is True, character case is ignored.
function WildcardCompare(const FileWildcard, FileName: string; const IgnoreCase: Boolean): Boolean;

// See if a given file name contains any wildcards (looks for * or ?)
function FileNameHasWildcards(const FileName: string): Boolean;

// Retrieves the index into the Win32 system's image list
// that represents the icon for FileName (based upon the
// file extension).
// Returns zero on failure, a valid image index otherwise.
function GetSystemImageIndexForFile(const FileName: string): Integer;

procedure TGXUnicodeStringList_MakeIndex(_sl: TGXUnicodeStringList);

// Load a binary/text form file into a TStrings object
procedure LoadFormFileToStrings(const FileName: string; Strings: TGXUnicodeStringList; out WasBinary: Boolean); overload;
procedure LoadFormFileToStrings(const FileName: string; Strings: TGXUnicodeStringList); overload;
// Return whether the form is saved in binary format (only useful for saved files)
function IsBinaryForm(const FileName: string): Boolean;
// Load a file from disk to a unicode string list
procedure LoadDiskFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList; var WasBinary: Boolean);
// Search for a file in the given path, returns the first match
function TryFindPathToFile(const FileName: string; out FullFilename: string; Paths: TStrings): Boolean;

///<summary>
/// removes leading '..\', converts ':', '\' and '/' to @ </summary>
function MangleFilename(const _fn: string): string;

///<summary>
/// @returns True, if Fn1 is newer than FN2
/// @raises exeptions if either of the files does not exist </summary>
function FileIsNewerThan(const _Fn1, _Fn2: string): Boolean;

//
// Binary module utility functions.
//

// Returns the full path to and file name of the PE
// module this function is linked into.
function ThisDllName: string;

// Returns the HINSTANCE of the main VCL code; this
// is the HINSTANCE of the main VCL package
// (VCL30/VCL40/VCL50...) or the HINSTANCE of whatever
// contains the code (probably the "real" binary).
function VclInstance: LongWord;

//
// Exception handling utility functions.
//

// Shows the exception error message; if Msg is assigned,
// it is used as a prefix to the exception error message.
procedure GxLogAndShowException(const E: Exception; const Msg: string = '');

// Log an exception's stack trace to the debug log (with an optional header message)
procedure GxLogException(const E: Exception; const Msg: string = '');

// Show an error dialog if the error string is not empty
function ShowError(const Error: string): Boolean;

procedure GetEnvironmentVariables(Strings: TStrings);

function FindTextIdent(Id: string; const Source: string;
  LastPos: Integer; Prev: Boolean; var Pos: Integer): Boolean;

///<symmary>
/// Converts a line position (X/Y) into a character position, so that
/// StringList[LinePos.Y - 1][LinePos.X] = StringList.Text[CharPos]
/// LinePos is a 1-based X/Y position
/// Examples:
///   LinePosToCharPos(Point(5, 1), '12345') = 5
///   LinePosToCharPos(Point(1, 2), '12345'#13#10'6789') = 8
/// </summary>
function LinePosToCharPos(LinePos: TPoint; const Text: string): Integer;

///<symmary>
/// Converts a character position into line position (X/Y), so that
/// StringList[LinePos.Y - 1][LinePos.X] = StringList.Text[CharPos]
/// LinPos is a 1-based X/Y position
/// Examples:
///   CharPosToLinePos(5, '12345') = Point(5, 1)
///   CharPosToLinePos(8, '12345'#13#10'6789') = Point(1, 2)
/// </summary>
function CharPosToLinePos(CharPos: Integer; const Text: string): TPoint;

// Convert a Windows message number into a string description
function MessageName(Msg: Longint): string;

// ISO date/time functions
function IsoDateTimeToStr(DateTime: TDateTime): string;
function IsoStringToDateTime(const ISODateTime: string): TDateTime;
function IsoStringToDateTimeDef(const DateTime: string; Default: TDateTime): TDateTime;

function GetFirstCharPos(const AText: String; AChars: TSysCharSet; SearchThis: Boolean): Integer;
function GetLastCharPos(const AText: String; AChars: TSysCharSet; SearchThis: Boolean): Integer;

{$IFNDEF GX_VER160_up}
// Delphi 6/7: IDE is non-unicode, string is ansistring
function ConvertToIDEEditorString(const S: string): IDEEditBufferString;
function IDEEditorStringToString(const S: IDEEditBufferString): string;
function UTF8ToUnicodeString(S: UTF8String): WideString;
{$ELSE GX_VER160_up}
// Delphi 8+
{$IFNDEF GX_VER200_up}
// Delphi (8/)2005/2006/2007: IDE is unicode (UTF-8), string is ansistring (GX_VER160_up}
function ConvertToIDEEditorString(const S: string): IDEEditBufferString;
function IDEEditorStringToString(const S: IDEEditBufferString): string;
function UTF8ToUnicodeString(S: UTF8String): WideString;
{$ELSE GX_VER200_up}
// Delphi 2009+: IDE is unicode (UTF-8), string is unicodestring {GX_VER200_up}
function ConvertToIDEEditorString(const S: string): IDEEditBufferString;
function IDEEditorStringToString(const S: string): string; overload;
function IDEEditorStringToString(const S: IDEEditBufferString): string; overload;
{$ENDIF GX_VER200_up}
{$ENDIF GX_VER160_up}

procedure AddSCMDirsToIgnore(_List: TGXUnicodeStringList); overload;
procedure AddSCMDirsToIgnore(_List: TStringList); overload;

procedure AddDelphiDirsToIgnore(_List: TGXUnicodeStringList); overload;
procedure AddDelphiDirsToIgnore(_List: TStringList); overload;

type
  TFileFindThread = class(TErrorThread)
  private
    FFileMasks: TStringList;
    FResults: TStringList;
    FSearchDirs: TStringList;
    FRecursiveSearchDirs: TStringList;
    FFindComplete: TThreadMethod;
    FResultsLock: TCriticalSection;
    FDirectoriesOnly: Boolean;
    FDirsToIgnore: TStringList;
    procedure SetDirsToIgnore(const Value: TStringList);
  protected
    procedure doExecute; override;
    procedure FindFilesInDir(const Dir: string; Recursive: Boolean);
    procedure AddResult(const FileName: string); virtual;
  public
    property DirsToIgnore: TStringList read FDirsToIgnore write SetDirsToIgnore;
    property FileMasks: TStringList read FFileMasks;
    property SearchDirs: TStringList read FSearchDirs;
    property DirectoriesOnly: Boolean read FDirectoriesOnly write FDirectoriesOnly;
    property RecursiveSearchDirs: TStringList read FRecursiveSearchDirs;
    property OnFindComplete: TThreadMethod read FFindComplete write FFindComplete;
    property Results: TStringList read FResults;
    constructor Create;
    destructor Destroy; override;
    procedure AddDelphiDirsToIgnore;
    procedure AddSCMDirsToIgnore;
    procedure StartFind;
    procedure LockResults;
    procedure ReleaseResults;
  end;

implementation

uses
  Windows, Messages,
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
//  GX_Debug,
  {$IFDEF UNICODE} Character, {$ENDIF}
  ShLwApi,
  ShellAPI, ShlObj, ActiveX, StrUtils, Math, Clipbrd, FileCtrl,
  u_dzSelectDirectoryFix, u_dzOsUtils, u_dzStringUtils;

const
  shlwapi32 = 'shlwapi.dll';

function StrCmpLogicalW(psz1, psz2: PWideChar): Integer; stdcall;
  external shlwapi32 name 'StrCmpLogicalW';

var
  ASCIICharTable: array [#0..#255] of Byte;
  LocaleIdentifierChars: set of AnsiChar;
  RichEditVersion: Integer;

const
  RichEdit10ModuleName = 'RICHED32.DLL';
  RichEdit20ModuleName = 'RICHED20.DLL';

procedure MakeASCIICharTable;
var
  i: Integer;
begin
  for i := 0 to 255 do
  begin
    If (I > 64) and (I < 91) then
      ASCIICharTable[Char(I)] := i + 32
    else
      ASCIICharTable[Char(I)] := i;
  end;
end;

// FastCode PosIEx PosIEx_JOH_IA32_1_c from fastcode.org
function PosIEx(const SubStr, S: AnsiString; Offset: Integer = 1): Integer;
const
  LocalsSize = 32;
  _ebx =  0;
  _edi =  4;
  _esi =  8;
  _ebp = 12;
  _edx = 16;
  _ecx = 20;
  _end = 24;
  _tmp = 28;
asm
  sub     esp, LocalsSize  {Setup Local Storage}
  mov     [esp._ebx], ebx
  cmp     eax, 1
  sbb     ebx, ebx         {-1 if SubStr = '' else 0}
  sub     edx, 1           {-1 if S = ''}
  sbb     ebx, 0           {Negative if S = '' or SubStr = '' else 0}
  sub     ecx, 1           {Offset - 1}
  or      ebx, ecx         {Negative if S = '' or SubStr = '' or Offset < 1}
  jl      @@InvalidInput
  mov     [esp._edi], edi
  mov     [esp._esi], esi
  mov     [esp._ebp], ebp
  mov     [esp._edx], edx
  mov     edi, [eax-4]     {Length(SubStr)}
  mov     esi, [edx-3]     {Length(S)}
  add     ecx, edi
  cmp     ecx, esi
  jg      @@NotFound       {Offset to High for a Match}
  test    edi, edi
  jz      @@NotFound       {Length(SubStr = 0)}
  add     esi, edx         {Last Character Position in S}
  add     eax, edi         {Last Character Position in SubStr + 1}
  mov     [esp._end], eax  {Save SubStr End Positiom}
  add     edx, ecx         {Search Start Position in S for Last Character}
  movzx   eax, [eax-1]     {Last Character of SubStr}
  mov     bl, al           {Convert Character into Uppercase}
  add     bl, $9f
  sub     bl, $1a
  jnb     @@UC1
  sub     al, $20
@@UC1:
  mov     ah, al
  neg     edi              {-Length(SubStr)}
  mov     ecx, eax
  shl     eax, 16
  or      ecx, eax         {All 4 Bytes = Uppercase Last Character of SubStr}
@@MainLoop:
  add     edx, 4
  cmp     edx, esi
  ja      @@Remainder      {1 to 4 Positions Remaining}
  mov     eax, [edx-4]     {Check Next 4 Bytes of S}
  mov     ebx, eax         {Convert All 4 Characters into Uppercase}
  or      eax, $80808080
  mov     ebp, eax
  sub     eax, $7B7B7B7B
  xor     ebp, ebx
  or      eax, $80808080
  sub     eax, $66666666
  and     eax, ebp
  shr     eax, 2
  xor     eax, ebx
  xor     eax, ecx         {Zero Byte at each Matching Position}
  lea     ebx, [eax-$01010101]
  not     eax
  and     eax, ebx
  and     eax, $80808080   {Set Byte to $80 at each Match Position else $00}
  jz      @@MainLoop       {Loop Until any Match on Last Character Found}
  bsf     eax, eax         {Find First Match Bit}
  shr     eax, 3           {Byte Offset of First Match (0..3)}
  lea     edx, [eax+edx-3] {Address of First Match on Last Character + 1}
@@Compare:
  cmp     edi, -4
  jle     @@Large
  cmp     edi, -1
  je      @@SetResult      {Exit with Match if Lenght(SubStr) = 1}
  mov     eax, [esp._end]  {SubStr End Position}
  movzx   eax, word ptr [edi+eax] {Last Char Matches - Compare First 2 Chars}
  cmp     ax, [edi+edx]
  je      @@SetResult      {Same - Skip Uppercase Conversion}
  mov     ebx, eax         {Convert Characters into Uppercase}
  or      eax, $80808080
  mov     ebp, eax
  sub     eax, $7B7B7B7B
  xor     ebp, ebx
  or      eax, $80808080
  sub     eax, $66666666
  and     eax, ebp
  shr     eax, 2
  xor     eax, ebx
  mov     [esp._tmp], eax  {Save Converted Characters}
  movzx   eax, word ptr [edi+edx]
  mov     ebx, eax         {Convert Characters into Uppercase}
  or      eax, $80808080
  mov     ebp, eax
  sub     eax, $7B7B7B7B
  xor     ebp, ebx
  or      eax, $80808080
  sub     eax, $66666666
  and     eax, ebp
  shr     eax, 2
  xor     eax, ebx
  cmp     eax, [esp._tmp]
  jne     @@MainLoop       {No Match on First 2 Characters}
@@SetResult:               {Full Match}
  lea     eax, [edx+edi]   {Calculate and Return Result}
  sub     eax, [esp._edx]  {Subtract Start Position}
  jmp     @@Done
@@NotFound:
  xor     eax, eax         {No Match Found - Return 0}
@@Done:
  mov     ebx, [esp._ebx]
  mov     edi, [esp._edi]
  mov     esi, [esp._esi]
  mov     ebp, [esp._ebp]
  add     esp, LocalsSize  {Release Local Storage}
  ret
@@Large:
  mov     eax, [esp._end]  {SubStr End Position}
  mov     eax, [eax-4]     {Compare Last 4 Characters of S and SubStr}
  cmp     eax, [edx-4]
  je      @@LargeCompare   {Same - Skip Uppercase Conversion}
  mov     ebx, eax         {Convert All 4 Characters into Uppercase}
  or      eax, $80808080
  mov     ebp, eax
  sub     eax, $7B7B7B7B
  xor     ebp, ebx
  or      eax, $80808080
  sub     eax, $66666666
  and     eax, ebp
  shr     eax, 2
  xor     eax, ebx
  mov     [esp._tmp], eax  {Save Converted Characters}
  mov     eax, [edx-4]
  mov     ebx, eax         {Convert All 4 Characters into Uppercase}
  or      eax, $80808080
  mov     ebp, eax
  sub     eax, $7B7B7B7B
  xor     ebp, ebx
  or      eax, $80808080
  sub     eax, $66666666
  and     eax, ebp
  shr     eax, 2
  xor     eax, ebx
  cmp     eax, [esp._tmp]  {Compare Converted Characters}
  jne     @@MainLoop       {No Match on Last 4 Characters}
@@LargeCompare:
  mov     ebx, edi         {Offset}
  mov     [esp._ecx], ecx  {Save ECX}
@@CompareLoop:             {Compare Remaining Characters}
  add     ebx, 4           {Compare 4 Characters per Loop}
  jge     @@SetResult      {All Characters Matched}
  mov     eax, [esp._end]  {SubStr End Positiob}
  mov     eax, [ebx+eax-4]
  cmp     eax, [ebx+edx-4]
  je      @@CompareLoop    {Same - Skip Uppercase Conversion}
  mov     ecx, eax         {Convert All 4 Characters into Uppercase}
  or      eax, $80808080
  mov     ebp, eax
  sub     eax, $7B7B7B7B
  xor     ebp, ecx
  or      eax, $80808080
  sub     eax, $66666666
  and     eax, ebp
  shr     eax, 2
  xor     eax, ecx
  mov     [esp._tmp], eax
  mov     eax, [ebx+edx-4]
  mov     ecx, eax         {Convert All 4 Characters into Uppercase}
  or      eax, $80808080
  mov     ebp, eax
  sub     eax, $7B7B7B7B
  xor     ebp, ecx
  or      eax, $80808080
  sub     eax, $66666666
  and     eax, ebp
  shr     eax, 2
  xor     eax, ecx
  cmp     eax, [esp._tmp]
  je      @@CompareLoop    {Match on Next 4 Characters}
  mov     ecx, [esp._ecx]  {Restore ECX for Next Main Loop}
  jmp     @@MainLoop       {No Match}
@@Remainder:               {Check Last 1 to 4 Characters}
  mov     eax, [esi-3]     {Last 4 Characters of S - May include Length Bytes}
  mov     ebx, eax         {Convert All 4 Characters into Uppercase}
  or      eax, $80808080
  mov     ebp, eax
  sub     eax, $7B7B7B7B
  xor     ebp, ebx
  or      eax, $80808080
  sub     eax, $66666666
  and     eax, ebp
  shr     eax, 2
  xor     eax, ebx
  xor     eax, ecx         {Zero Byte at each Matching Position}
  lea     ebx, [eax-$01010101]
  not     eax
  and     eax, ebx
  and     eax, $80808080   {Set Byte to $80 at each Match Position else $00}
  jz      @@NotFound       {No Match Possible}
  sub     edx, 3           {Start Position for Next Loop}
  movzx   eax, [edx-1]
  mov     bl, al           {Convert Character into Uppercase}
  add     bl, $9f
  sub     bl, $1a
  jnb     @@UC2
  sub     al, $20
@@UC2:
  cmp     al, cl
  je      @@Compare        {Match}
  cmp     edx, esi
  ja      @@NotFound
  add     edx, 1
  movzx   eax, [edx-1]
  mov     bl, al           {Convert Character in AL into Uppercase}
  add     bl, $9f
  sub     bl, $1a
  jnb     @@UC3
  sub     al, $20
@@UC3:
  cmp     al, cl
  je      @@Compare        {Match}
  cmp     edx, esi
  ja      @@NotFound
  add     edx, 1
  movzx   eax, [edx-1]
  mov     bl, al           {Convert Character in AL into Uppercase}
  add     bl, $9f
  sub     bl, $1a
  jnb     @@UC4
  sub     al, $20
@@UC4:
  cmp     al, cl
  je      @@Compare        {Match}
  cmp     edx, esi
  ja      @@NotFound
  add     edx, 1
  jmp     @@Compare        {Match}
@@InvalidInput:
  xor     eax, eax         {Return 0}
  mov     ebx, [esp._ebx]
  add     esp, LocalsSize  {Release Local Storage}
end; {PosIEx}

function CaseInsensitivePos(const Pat, Text: string): Integer;
begin
  {$IFDEF UNICODE}
  Result := Pos(AnsiUpperCase(Pat), AnsiUpperCase(Text));
  {$ELSE}
  Result := PosIEx(Pat, Text);
  {$ENDIF}
end;

//TODO: Optimize PosFrom* calls to not require a Copy call
function CaseInsensitivePosFrom(const Pat, Text: string; StartIndex: Integer): Integer;
var
  S: string;
begin
  if StartIndex > 1 then
  begin
    S := Copy(Text, StartIndex);
    Result := CaseInsensitivePos(Pat, S);
  end else
    Result := CaseInsensitivePos(Pat, Text);
  if Result > 0 then
     Inc(Result, Pred(StartIndex));
end;

function PosFrom(const Pat, Text: string; StartIndex: Integer): Integer;
var
  S: string;
begin
  if StartIndex > 1 then
  begin
    S := Copy(Text, StartIndex);
    Result := Pos(Pat, S);
  end else
    Result := Pos(Pat, Text);
  if Result > 0 then
     Inc(Result, Pred(StartIndex));
end;

function AnsiCaseInsensitivePos(const SubString, S: string): Integer;
begin
  Result := AnsiPos(AnsiUpperCase(SubString), AnsiUpperCase(S));
end;

// Case insensitive
function StringInArray(const S: string; const SArray: array of string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(SArray) - 1 do
    if SameText(S, SArray[i]) then begin
      Result := True;
      Break;
    end;
end;

procedure AnsiStrTok(const Source, Delimiter: string; Dest: TStrings);
var
  i: Integer;
  SubString: string;
  Temp: string;
begin
  if (Source = '') or (Delimiter = '') or (Dest = nil) then
    Exit;

  Dest.Clear;
  SubString := Source;
  repeat
    i := AnsiPos(Delimiter, SubString);

    if i = 0 then
      Temp := SubString
    else
      Temp := Copy(SubString, 1, i - 1);

    if Temp <> '' then
      Dest.Add(Temp);

    SubString := Copy(SubString, i + Length(Delimiter), Length(SubString) - i);
  until i = 0;
end;


function GetQuotedToken(const Str: string): string;

  function QuotedToken(const Str: string; Quote: Char): string;
  var
    Token: string;
    EndQuotePos: Integer;
  begin
    Assert(Length(Str) > 0);
    Assert(Str[1] = Quote);
    Result := '';
    Token := Copy(Str, 2);
    EndQuotePos := Pos(Quote, Token);
    if EndQuotePos > 0 then
      Result := Copy(Token, 1, EndQuotePos - 1);
  end;

const
  SingleQuote = #39;
var
  StartChar: string;
begin
  Result := Trim(Str);
  StartChar := LeftStr(Trim(Result), 1);
  if StartChar = SingleQuote then
    Result := QuotedToken(Result, SingleQuote)
  else if StartChar = '"' then
    Result := QuotedToken(Result, '"');
  Result := Trim(Result);
end;

function ExpandTabsInLine(const AText: string; ATabSize: Integer): string;
var
  i: Integer;
  ResultLen, SpaceLen: Integer;
begin
  Result := '';
  ResultLen := 0;

  for i := 1 to Length(AText) do
  begin
    if AText[i] <> #9 then
    begin
      Result := Result + AText[i];
      Inc(ResultLen);
    end
    else
    begin
      SpaceLen := ATabSize - (ResultLen mod ATabSize);
      Result := Result + StringOfChar(' ', SpaceLen);
      Inc(ResultLen, SpaceLen);
    end;
  end;
end;

procedure FindMinIndent(ALines: TStrings; var LineIndex, SpaceCount: Integer);
var
  LineLen: Integer;
  i, j: Integer;
  LineItem: string;
begin
  LineIndex := -1;
  SpaceCount := -1;

  for i := 0 to ALines.Count - 1 do
  begin
    LineItem := ALines[i];
    LineLen := Length(LineItem);

    j := 1;
    while (j < LineLen) and IsCharWhiteSpace(LineItem[j]) do
    begin
      if (not IsCharWhiteSpace(LineItem[j + 1])) and ((SpaceCount < 0) or (SpaceCount > j)) then
      begin
        LineIndex := i;
        SpaceCount := j;
      end;
      Inc(j);
    end; // while j
  end; // for i
end;

{$IFDEF UNICODE}
function IsLeadChar(C: AnsiChar): Boolean;
begin
  Result := C in LeadBytes;
end;

function IsCharLineEndingOrNull(C: AnsiChar): Boolean;
begin
  Result := C in [#0, #10, #13];
end;

function IsCharAlphaNumeric(C: AnsiChar): Boolean; overload;
begin
  Result := Windows.IsCharAlphaNumericA(C);
end;

function IsCharNumeric(C: AnsiChar): Boolean;
begin
  Result := C in ['0'..'9'];
end;

function IsCharSymbol(C: AnsiChar): Boolean;
begin
  Result := CharInSet(C, ['#', '$', '&', #39, '(', ')', '*', '+', ',', '-', '.', '/', ':', ';',
      '<', '=', '>', '@', '[', ']', '^']);
end;

function IsCharIdentifierStart(C: AnsiChar): Boolean; overload;
begin
  Result := (C in LocaleIdentifierChars) and not (C in ['0'..'9']);
end;

function IsCharIdentifier(C: AnsiChar): Boolean; overload;
begin
  Result := C in LocaleIdentifierChars;
end;

function GetFileEncoding(const FileName: string): TEncoding;
const
  MaxBOMLength = 100;
var
  Stream: TStream;
  Buffer: TBytes;
begin
  Result := nil;
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Buffer, MaxBOMLength);
    Stream.Read(Buffer[0], MaxBOMLength);
    TEncoding.GetBufferEncoding(Buffer, Result);
  finally
    Stream.Free;
  end;
end;

{$ELSE not UNICODE}
function CharInSet(C: Char; CSet: TSysCharSet): Boolean;
begin
  Result := C in CSet;
end;

function AnsiStrAlloc(Size: Cardinal): PChar;
begin
  Result := StrAlloc(Size);
end;

function IsLeadChar(C: Char): Boolean;
begin
  Result := C in LeadBytes;
end;

function IsCharIdentifier(C: WideChar): Boolean; overload;
begin
  {$IFDEF UNICODE}
  Result := TCharacter.IsLetterOrDigit(C) or (C = '_');
  {$ELSE not UNICODE}
  Result := AnsiChar(C) in LocaleIdentifierChars; // Includes '_'
  {$ENDIF}
end;

{$ENDIF UNICODE}

function IsCharWhiteSpace(C: Char): Boolean;
const
  HorizontalTab = #9;
  VerticalTab = #11;
  FormFeed = #12;
begin
  {$IFDEF UNICODE}
    {$IFDEF GX_VER250_up} // XE4+
    Result := C.IsWhiteSpace;
    {$ELSE} // 2009 - XE3
    Result := TCharacter.IsWhiteSpace(C);
    {$ENDIF}
  {$ELSE not UNICODE}
  Result := C in [HorizontalTab, LF, VerticalTab, FormFeed, CR, #32];
  {$ENDIF}
end;

function IsCharWhiteSpaceOrNull(C: Char): Boolean;
begin
  Result := (C = #0) or IsCharWhiteSpace(C);
end;

// Inlined method must be implemented before it is used
function IsCharControl(C: Char): Boolean;
begin
  {$IFDEF UNICODE}
    {$IFDEF GX_VER250_up} // XE4+
    Result := C.IsControl;
    {$ELSE} // 2009 - XE3
    Result := TCharacter.IsControl(C);
    {$ENDIF}
  {$ELSE not UNICODE}
  Result := C in [#0..#8, #14..#31];
  {$ENDIF}
end;


function IsCharWhiteSpaceOrControl(C: Char): Boolean;
begin
  Result := IsCharWhiteSpace(C) or IsCharControl(C);
end;

function IsCharLineEnding(C: Char): Boolean;
begin
  Result := CharInSet(C, [LF, CR]);
end;

function IsCharLineEndingOrNull(C: Char): Boolean;
begin
  Result := CharInSet(C, [#0, LF, CR]);
end;

function IsCharAlphaNumeric(C: Char): Boolean;
begin
  {$IFDEF UNICODE}
    {$IFDEF GX_VER250_up} // XE4+
    Result := C.IsLetterOrDigit;
    {$ELSE} // 2009 - XE3
    Result := TCharacter.IsLetterOrDigit(C);
    {$ENDIF}
  {$ELSE not UNICODE}
  Result := Windows.IsCharAlphaNumeric(C);
  {$ENDIF}
end;

function IsCharNumeric(C: Char): Boolean;
begin
  {$IFDEF UNICODE}
    {$IFDEF GX_VER250_up} // XE4+
    Result := C.IsDigit;
    {$ELSE} // 2009 - XE3
    Result := TCharacter.IsDigit(C);
    {$ENDIF}
  {$ELSE not UNICODE}
  Result := CharInSet(C, ['0'..'9']);
  {$ENDIF}
end;

function IsCharHexDigit(C: Char): Boolean;
begin
  Result := IsCharNumeric(C) or CharInSet(C, ['A'..'F', 'a'..'f']);
end;

function IsCharTab(C: Char): Boolean;
begin
  Result := C = #8;
end;

function IsCharSymbol(C: Char): Boolean;
begin
  {$IFDEF UNICODE}
    {$IFDEF GX_VER250_up} // XE4+
    Result := C.IsSymbol or C.IsPunctuation;
    {$ELSE} // 2009 - XE3
    Result := TCharacter.IsSymbol(C) or TCharacter.IsPunctuation(C);
    {$ENDIF}
  {$ELSE not UNICODE}
  Result := C in ['#', '$', '&', #39, '(', ')', '*', '+', ',', '-', '.', '/', ':', ';',
    '<', '=', '>', '@', '[', ']', '^'];
  {$ENDIF}
end;

function IsCharIdentifierStart(C: Char): Boolean; overload;
begin
  {$IFDEF UNICODE}
    {$IFDEF GX_VER250_up} // XE4+
    Result := C.IsLetter or (C = '_');
    {$ELSE} // 2009 - XE3
    Result := TCharacter.IsLetter(C) or (C = '_');
    {$ENDIF}
  {$ELSE not UNICODE}
  Result := (C in LocaleIdentifierChars) and not (C in ['0'..'9']);
  {$ENDIF}
end;

function IsCharIdentifier(C: Char): Boolean;
begin
  {$IFDEF UNICODE}
    {$IFDEF GX_VER250_up} // XE4+
    Result := C.IsLetterOrDigit or (C = '_');
    {$ELSE} // 2009 - XE3
    Result := TCharacter.IsLetterOrDigit(C) or (C = '_');
    {$ENDIF}
  {$ELSE not UNICODE}
  Result := C in LocaleIdentifierChars; // Includes '_'
  {$ENDIF}
end;

// This function is impossible to read, but it is very fast
function CompressWhiteSpace(const Str: string): string;
var
  i: Integer;
  Len: Integer;
  NextResultChar: Integer;
  CheckChar: Char;
  NextChar: Char;
begin
  Len := Length(Str);
  NextResultChar := 1;
  SetLength(Result, Len);

  for i := 1 to Len do
  begin
    CheckChar := Str[i];
    {$RANGECHECKS OFF}
    NextChar := Str[i + 1];
    {$RANGECHECKS ON}
    case CheckChar of
      #9, #10, #13, #32:
        begin
          if IsCharWhiteSpaceOrNull(NextChar) or (NextResultChar = 1) then
            Continue
          else
          begin
            Result[NextResultChar] := #32;
            Inc(NextResultChar);
          end;
        end;
      else
        begin
          Result[NextResultChar] := Str[i];
          Inc(NextResultChar);
        end;
    end;
  end;
  if Len = 0 then
    Exit;
  SetLength(Result, NextResultChar - 1);
end;

function LeftTrimChars(var Value: string; const TrimChars: TSysCharSet = [#9, #32]): Integer;
begin
  Result := 0;
  while (Length(Value) > Result) and CharInSet(Value[Result+1], TrimChars) do
    Inc(Result);

  System.Delete(Value, 1, Result);
end;

function LeftTrimNChars(const AValue: string; const TrimChars: TSysCharSet; AMaxCount: Integer): string;
var
  CharCnt: Integer;
begin
  CharCnt := 0;
  while (Length(AValue) > CharCnt) and CharInSet(AValue[CharCnt+1], TrimChars) and
        ((CharCnt < AMaxCount) or (AMaxCount = 0)) do
    Inc(CharCnt);

  Result := AValue;
  if CharCnt > 0 then
    System.Delete(Result, 1, CharCnt);
end;

function StrBeginsWith(const SubStr, Str: string; CaseSensitive: Boolean): Boolean;
begin
  if CaseSensitive then
    Result := StartsStr(SubStr, Str)
  else
    Result := StartsText(SubStr, Str);
end;

function StrEndsWith(const SubStr, Str: string; CaseSensitive: Boolean): Boolean;
begin
  if CaseSensitive then
    Result := EndsStr(SubStr, Str)
  else
    Result := EndsText(SubStr, Str);
end;

function DeleteRight(const Value: string; NumChars: Integer): string;
begin
  Result := '';
  if NumChars < Length(Value) then
  begin
    Result := Value;
    System.Delete(Result, Length(Result) - NumChars + 1, NumChars);
  end;
end;

function RemoveSurroundingChars(SurroundingChar: Char; const Str: string): string;
begin
  Result := Str;
  if Length(Str) > 1 then
    if (Str[1] = SurroundingChar) and (Str[Length(Str)] = SurroundingChar) then
      Result := Copy(Str, 2, Length(Str) - 2);
end;

function AnsiUpperChar(C: Char): Char;
var
  Str: string;
begin
  SetLength(Str, 1);
  Str[1] := C;
  Str := AnsiUpperCase(Str);
  Result := Str[1];
end;

function AnsiLowerChar(C: Char): Char;
var
  Str: string;
begin
  SetLength(Str, 1);
  Str[1] := C;
  Str := AnsiLowerCase(Str);
  Result := Str[1];
end;

function FirstAlphaInString(const S: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 1 to Length(S) do
  begin
    if IsCharAlpha(S[i]) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure DecomposeClassMethodString(const S: string; var MethodName, ClassName, NestedClasses: string);
var
  TempStr: string;
  Identifiers: TStringList;
  AssignedCount: Integer;
  i: Integer;
begin
  TempStr := Trim(S);
  MethodName := '';
  ClassName := '';
  NestedClasses := '';

  Identifiers := TStringList.Create;
  try
   AnsiStrTok(TempStr, '.', Identifiers);
   AssignedCount := 0;
   for i := Identifiers.Count - 1 downto 0 do
   begin
     if AssignedCount = 0 then
     begin
       MethodName := Identifiers[i];
       Inc(AssignedCount);
     end
     else if AssignedCount = 1 then
     begin
       ClassName := Identifiers[i];
       Inc(AssignedCount);
     end
     else begin
       if NestedClasses = '' then
         NestedClasses := Identifiers[i]
       else
         NestedClasses := Identifiers[i] + '.' + NestedClasses;
     end;
   end;
  finally
    FreeAndNil(Identifiers);
  end;
end;

function GetMethodName(const S: string): string;
var
  MethodName, ClassName, NestedClasses: string;
begin
  DecomposeClassMethodString(S, MethodName, ClassName, NestedClasses);
  Result := MethodName;
end;

function ToggleCase(const S: string): string;
var
  i: Integer;
begin
  Result := S;
  for i := 1 to Length(Result) do
  begin
    if IsCharAlpha(Result[i]) then
    begin
      if IsCharUpper(Result[i]) then
        Result[i] := AnsiLowerChar(Result[i])
      else if IsCharLower(Result[i]) then
        Result[i] := AnsiUpperChar(Result[i]);
    end;
  end;
end;

function SentenceCase(const S: string): string;

  function PreviousCharsEndSentence(i: Integer): Boolean;
  begin
    Result := False;
    while (i > 0) and (IsCharWhiteSpace(S[i])) do
      Dec(i);
    if (i > 0) and CharInSet(S[i], GxSentenceEndChars) then
      Result := True;
  end;

  function FollowsSentenceEnd(i: Integer): Boolean;
  begin
    Result := False;
    if i < 3 then
      Exit;
    if IsCharWhiteSpace(S[i - 1]) and PreviousCharsEndSentence(i-1) then
      Result := True;
  end;

  function CharacterBeginsSentence(i: Integer): Boolean;
  begin
    Result := False;
    if FirstAlphaInString(S) = i then
      Result := True
    else if IsCharAlpha(S[i]) and FollowsSentenceEnd(i) then
      Result := True;
  end;

var
  i: Integer;
begin
  Result := AnsiLowerCase(S);
  if Length(Result) < 1 then
    Exit;

  for i := 1 to Length(Result) do
    if CharacterBeginsSentence(i) then
      Result[i] := AnsiUpperChar(Result[i]);
end;

function TitleCase(const S: string): string;
var
  i: Integer;
begin
  Result := AnsiLowerCase(S);

  i := FirstAlphaInString(Result);
  if i > 0 then begin
    Result[i] := AnsiUpperChar(Result[i]);
    while i < Length(Result) do
    begin
      if (i > 1) and IsCharAlpha(Result[i]) then
        if (not IsCharAlpha(Result[i - 1])) then
          Result[i] := AnsiUpperChar(Result[i]);
      Inc(i);
    end;
  end;
end;

procedure AddMRUString(Text: string; List: TStrings; DeleteTrailingDelimiter: Boolean;
  MaxListCount, MaxTextLength: Integer; AllowBlank: Boolean);
begin
  if (Trim(Text) = '') and (not AllowBlank) then
    Exit;
  if ( MaxTextLength >= 0 ) and ( Length(Text) > MaxTextLength ) then
    Exit;

  if DeleteTrailingDelimiter then
    Text := RemoveSlash(Text);

  DeleteStringFromList(List, Text);

  if List.Count = 0 then
    List.Add(Text)
  else
    List.Insert(0, Text);

  if ( MaxListCount >= 0) and ( List.Count > MaxListCount ) then
    List.Delete(List.Count - 1);
end;

procedure AddMRUString(const Text: string; List: TStrings; DeleteTrailingDelimiter: Boolean; AllowBlank: Boolean);
begin
  AddMRUString(Text, List, DeleteTrailingDelimiter, 20, 300, AllowBlank);
end;

procedure DeleteStringFromList(List: TStrings; const Item: string);
var
  Index: Integer;
begin
  Assert(Assigned(List));
  Index := List.IndexOf(Item);
  if Index >= 0 then
    List.Delete(Index);
end;

function EnsureStringInList(List: TStrings; const Item: string): string;
var
  Index: Integer;
begin
  Assert(Assigned(List));
  Index := List.IndexOf(Item);
  if Index = -1 then
    Index := List.Add(Item);
  Result := List[Index]
end;

procedure FilterStringListMatchStart(Source, Dest: TStrings; const Filter: string;
  CaseSensitive: Boolean = True);
var
  i: Integer;
begin
  Assert(Assigned(Source) and Assigned(Dest));
  Dest.BeginUpdate;
  try
    if Filter = '' then
      Dest.Assign(Source)
    else begin
      Dest.Clear;
      for i := 0 to Source.Count - 1 do begin
        if StrBeginsWith(Filter, Source[i], CaseSensitive) then
          Dest.AddObject(Source[i], Source.Objects[i]);
      end;
    end;
  finally
    Dest.EndUpdate;
  end;
end;

//function StrMatchesFilter(const _Filter: string; _s: string;
//  _CaseSensitive: Boolean; _MatchAnywhere: Boolean): Boolean;
//begin
//  Result := (_Filter = '')
//    or (_MatchAnywhere and StrContains(_Filter, _s, _CaseSensitive))
//    or ((not _MatchAnywhere) and StrBeginsWith(_Filter, _s, _CaseSensitive));
//end;

procedure FilterStringListMatchAnywhere(Source, Dest: TStrings; const Filter: string;
  CaseSensitive: Boolean = True);
var
  i: Integer;
  s: string;
begin
  Assert(Assigned(Source) and Assigned(Dest));
  Dest.BeginUpdate;
  try
    if Filter = '' then begin
      Dest.Assign(Source);
    end else begin
      Dest.Clear;
      for i := 0 to Source.Count - 1 do begin
        s := Source[i];
        if StrContains(Filter, s, CaseSensitive) then
          Dest.AddObject(s, Source.Objects[i]);
      end;
    end;
  finally
    Dest.EndUpdate;
  end;
end;

procedure FilterStringListMatchAnywhere(Source, Dest: TStrings; Filter: TStrings;
  CaseSensitive: boolean);

  function StrMatchesAll(const _s: string; _Filter: TStrings; _CaseSensitive: Boolean): boolean;
  var
    i: integer;
  begin
    for i := 0 to _Filter.Count - 1 do begin
      if not StrContains(_Filter[i], _s, _CaseSensitive) then begin
        Result := False;
        Exit; //==>
      end;
    end;
    Result := True;
  end;

var
  i: Integer;
  s: string;
begin
  Assert(Assigned(Source) and Assigned(Dest) and Assigned(Filter));
  Dest.BeginUpdate;
  try
    if Filter.Count = 0 then
      Dest.Assign(Source)
    else begin
      for i := 0 to Source.Count - 1 do begin
        s := Source[i];
        if StrMatchesAll(s, Filter, CaseSensitive) then
          Dest.AddObject(s, Source.Objects[i]);
      end;
    end;
  finally
    Dest.EndUpdate;
  end;
end;

procedure AddStringsPresentInString(Source, Dest: TStrings; const FilterString: string);
var
  i: Integer;
  SearchStr: string;
begin
  Dest.Clear;
  for i := 0 to Source.Count - 1 do
  begin
    SearchStr := Source[i];
    if StrContains(SearchStr, FilterString, False) then
      Dest.Add(SearchStr);
  end;
end;

function SpacedCommaText(Source: TStrings): string;
begin
  Assert(Assigned(Source));
  Result := StringReplace(Source.CommaText, ',', ', ', [rfReplaceAll]);
end;

function SortStringListByLength(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := Length(List[Index2]) - Length(List[Index1]);
  if Result = 0 then
    Result := CompareText(List[Index1], List[Index2]);
end;

function StrContains(const SubStr, Str: string; CaseSensitive: Boolean): Boolean;
begin
  if CaseSensitive then
    Result := Pos(SubStr, Str) > 0
  else
    Result := CaseInsensitivePos(SubStr, Str) > 0;
end;

{function StrContains(const SubStr, Str: string; CaseSensitive: Boolean): Boolean;
begin
  if CaseSensitive then
    Result := Pos(SubStr, Str) > 0
  else
    Result := CaseInsensitivePos(SubStr, Str) > 0;
end;}

function StrContainsWhiteSpace(const Str: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(Str) do
    if IsCharWhiteSpace(Str[i]) then
    begin
      Result := True;
      Break;
    end;
end;

function NotEmpty(const Str: string): Boolean;
begin
  Result := Trim(Str) <> '';
end;

function IsEmpty(const Str: string): Boolean;
begin
  Result := Trim(Str) = '';
end;

function AddSlash(const Directory: string): string;
begin
  Result := IncludeTrailingPathDelimiter(Directory);
end;

function RemoveSlash(const Directory: string): string;
begin
  Result := ExcludeTrailingPathDelimiter(Directory);
end;

function PrepareDirectoryForWriting(const Dir: string): Boolean;
begin
  if not SysUtils.DirectoryExists(Dir) then
  begin
    Result := SysUtils.ForceDirectories(Dir);
    if not Result then
      Exit;
  end;
  Result := CanWriteToDirectory(Dir);
end;

function IsPathAbsolute(const FileName: string): Boolean;
begin
  Result := ExtractFileDrive(FileName) <> '';
end;

function GXPathCombine(out ADest: string; const ADir, AFile: string): Boolean;
begin
  SetLength(ADest, MAX_PATH);
  Result := PathCombine(PChar(ADest), PChar(ADir), PChar(AFile)) <> nil;
  if Result then
    SetLength(ADest, StrLen(PChar(ADest)))
  else
    ADest := '';
end;

function GXPathRelativePathTo(out APath: string; const AFrom, ATo: string;
  AOptions: TGXPathRelativePathToOptions = [proOnlyInSubDir, proFromIsDirectory]): Boolean;
var
  dwAttrFrom, dwAttrTo: DWORD;
begin
  if proFromIsDirectory in AOptions then
    dwAttrFrom := FILE_ATTRIBUTE_DIRECTORY
  else
    dwAttrFrom := 0;
  if proToIsDirectory in AOptions then
    dwAttrTo := FILE_ATTRIBUTE_DIRECTORY
  else
    dwAttrTo := 0;
  SetLength(APath, MAX_PATH);
  Result := PathRelativePathTo(PChar(APath), PChar(ExtractFilePath(AFrom)), dwAttrFrom,
    PChar(ExpandFileName(ATo)), dwAttrTo);
  if Result then
  begin
    SetLength(APath, StrLen(PChar(APath)));
    if (proOnlyInSubDir in AOptions) and StrBeginsWith('..' + PathDelim, APath) then
      APath := ATo; // Return unchanged ATo if not in subdirectory
  end
  else
    APath := '';
end;

{$UNDEF IPlusOn}
{$IFOPT I+}
{$DEFINE IPlusOn}
{$ENDIF}
{$I-}
function SafeChangeDirectory(const NewDir: string): Boolean;
begin
  Result := True;
  if not SamePathName(GetCurrentDir, NewDir) then
  begin
    ChDir(NewDir);
    if (IOResult <> 0) then
      Result := False;
  end;
end;
{$IFDEF IPlusOn}
{$I+}
{$ENDIF}
{$UNDEF IPlusOn}

function BuildFileName(const Path, FileName: string): string;
var
  FileNameHasDelimiter: Boolean;
  PathHasDelimiter: Boolean;
begin
  if Trim(Path) <> '' then
  begin
    PathHasDelimiter := RightStr(Path, 1) = PathDelim;
    FileNameHasDelimiter := LeftStr(FileName, 1) = PathDelim;
    if PathHasDelimiter and FileNameHasDelimiter then
      Result := RemoveSlash(Path) + FileName
    else if PathHasDelimiter or FileNameHasDelimiter then
      Result := Path + FileName
    else // neither
      Result := AddSlash(Path) + FileName;
  end
  else
    Result := FileName;
end;

function GetPaddedString(const AString: string; const StringLength: Integer): string;
var
 i: Integer;
 Pad: string;
begin
  Result := AString;
  if Length(Result) > StringLength then
  begin
    System.Delete(Result, StringLength + 1, Length(Result));
    Exit;
  end;

  // Pad := '';
  for i := 0 to StringLength - Length(AString) do //FI:W528
    Pad := Pad + ' ';
  Result := Result + Pad;
end;

function LastCharPos(const S: string; C: Char): Integer;
var
  i: Integer;
begin
  i := Length(S);
  while (i > 0) and (S[i] <> C) do
    Dec(i);
  Result := i;
end;

function StrCharAt(const S: string; Pos: Integer): Char;
begin
  if (Pos >= 1) and (Pos <= Length(S)) then
    Result := S[Pos]
  else
    Result := #0;
end;

// Returns the size of any EOL characters at a given position
// Supports the following EOL formats:
//   #10#13 (?)
//   #13#10 (PC / Win)
//   #10    (Unix)
//   #13    (Macintosh, Amiga)
// Note: Pos must be at the beginning of the EOL characters
function EOLSizeAtPos(const S: string; Pos: Integer): Integer;
begin
  if IsCharLineEnding(StrCharAt(S, Pos)) then
  begin
    Result := 1;
    if (IsCharLineEnding(StrCharAt(S, Pos + 1)) and
        (StrCharAt(S, Pos) <> StrCharAt(S, Pos + 1))) then
      Inc(Result);
  end
  else
    Result := 0;
end;

function SourceDifferentApartFromTrailingLineBreak(const OrigSource, NewSource: TGXUnicodeString): Boolean;
var
  NewLen: Integer;
  OrigLen: Integer;
begin
  // this checks for the case that either the formatter or the IDE
  // removed or added a CRLF at the end of the file
  // checking the length first prevents costly multiple
  // comparisons of potentially large strings
  OrigLen := Length(OrigSource);
  NewLen := Length(NewSource);
  if OrigLen = NewLen then
    Result := (OrigSource <> NewSource)
  else if OrigLen = NewLen + CRLFLength then
    Result := (OrigSource <> NewSource + CRLF)
  else if OrigLen + CRLFLength = NewLen then
    Result := (OrigSource + CRLF <> NewSource)
  else
    Result := True;
end;

function HasTrailingEOL(const S: string): Boolean;
begin
  Result := (EOLSizeAtPos(S, Length(S)) > 0);
end;

procedure RemoveLastEOL(var S: string);
var
  CurrLen: Integer;
  EOLSize: Integer;
begin
  CurrLen := Length(S);
  if CurrLen > 0 then
  begin
    EOLSize := EOLSizeAtPos(S, CurrLen);
    if EOLSize > 0 then
    begin
      Dec(CurrLen);
      if EOLSizeAtPos(S, CurrLen) > EOLSize then
        // one more character found for EOL
        Dec(CurrLen);
      SetLength(S, CurrLen);
    end;
  end;
end;

function RemoveTrailingEOL(const S: string): string;
begin
  Result := S;
  RemoveLastEOL(Result);
end;

function LastLineOf(const S: string): string;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := S;
    if Lines.Count < 1 then
      Result := ''
    else
      Result := Lines.Strings[Lines.Count - 1];
  finally
    FreeAndNil(Lines);
  end;
end;

function RemoveCharsBefore(var S: string; const PrefixSep: Char): string;
var
  i: Integer;
begin
  i := Pos(PrefixSep, S);
  if i > 0 then
  begin // separator found
    Result := Copy(S, 1, i - 1);
    S := Copy(S, i + 1, Length(S) - i);
  end
  else
  begin // separator not found - column is a whole string
    Result := S;
    S := '';
  end;
end;

function ReverseString(const S: string): string;
var
  Counter: Integer;
  n: Integer;
  c: Char;
  Temp: Integer;
begin
  Result := S;
  n := Length(Result);
  // This will work fine for strings of odd length since
  // the middle character does not need swapping.
  for Counter := 1 to (n div 2) do
  begin
    c := Result[Counter];

    Temp := n - Counter + 1;
    Result[Counter] := Result[Temp];
    Result[Temp] := c;
  end;
end;

procedure AppendStrings(const Destination, Source: TStrings);
var
  i: Integer;
begin
  Assert(Assigned(Source));
  Assert(Assigned(Destination));

  for i := 0 to Source.Count - 1 do
    Destination.Add(Source[i]);
end;

procedure SortStringsCaseSensitive(const AStrings: TStrings);

  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P, S: string;
    O: TObject;
  begin
    repeat
      I := L;
      J := R;
      P := AStrings.Strings[(L + R) shr 1];
      repeat
        while AnsiCompareStr(AStrings.Strings[I], P) < 0 do Inc(I);
        while AnsiCompareStr(AStrings.Strings[J], P) > 0 do Dec(J);
        if I <= J then
        begin
          S := AStrings.Strings[J];
          AStrings.Strings[J] := AStrings.Strings[I];
          AStrings.Strings[I] := S;
          O := AStrings.Objects[J];
          AStrings.Objects[J] := AStrings.Objects[I];
          AStrings.Objects[I] := O;
          Inc(I);
          Dec(J);
        end;
      until I > J;

      if L < J then
        QuickSort(L, J);

      L := I;
    until I >= R;
  end;

begin
  Assert(Assigned(AStrings));

  if AStrings.Count > 1 then
    QuickSort(0, AStrings.Count - 1);
end;

function EnforceMinMax(Value, Min, Max: Integer): Integer;
begin
  Result := Value;
  if Result < Min then
    Result := Min;
  if Result > Max then
    Result := Max;
end;

function FindChild(const WindowContainer: TWinControl; const ControlName: string): TControl;
var
  i: Integer;
  AControl: TControl;
begin
  Assert(Assigned(WindowContainer));

  Result := nil;
  for i := 0 to WindowContainer.ControlCount - 1 do
  begin
    AControl := WindowContainer.Controls[i];
    if SameText(AControl.Name, ControlName) then
    begin
      Result := AControl;
      Break;
    end;
  end;
end;

procedure IterateOverControls(Control: TControl; ActionProc: TForEachControlProc);
var
  WinControl: TWinControl;
  i: Integer;
begin
  Assert(Assigned(ActionProc));
  Assert(Assigned(Control));
  ActionProc(Control);
  if (Control is TWinControl) then
  begin
    WinControl := Control as TWinControl;
    for i := 0 to WinControl.ControlCount - 1 do
      IterateOverControls(WinControl.Controls[i], ActionProc);
  end;
end;

function TryFocusControl(Control: TWinControl): Boolean;
begin
  Result := False;
  if Assigned(Control) then begin
    if Control.CanFocus and Control.Visible then begin
      try
        Control.SetFocus;
        Result := True;
      except
        // Ignore focus errors
      end;//FI:W501
    end;
  end;
end;

// Based on function TJvCustomRichEdit.GetCharPos(CharIndex: Integer): TPoint;
function GetCharPos(RichEdit: TRichEdit; CharIndex: Integer): TPoint;
var
  Res: Longint;
begin
  FillChar(Result, SizeOf(Result), 0);
  if RichEdit.HandleAllocated then
  begin
    if RichEditVersion = 2 then
    begin
      Res := SendMessage(RichEdit.Handle, Messages.EM_POSFROMCHAR, CharIndex, 0);
      Result.X := LoWord(Res);
      Result.Y := HiWord(Res);
    end
    else { RichEdit 1.0 and 3.0 }
      SendMessage(RichEdit.Handle, Messages.EM_POSFROMCHAR, WPARAM(@Result), CharIndex);
  end;
end;

procedure CenterLineInEdit(RichEdit: TRichEdit; LineNum: Integer);
// I don't know the reason, but the RichEdit 2 control in VCL does not
// respond to the EM_SCROLLCARET in Richedit.h but it does so to the
// constant in WinUser.h
const
  EM_SCROLLCARET  = $00B7;
var
  TextPos: lResult;
  Pos: TPoint;
begin
  TextPos := SendMessage(RichEdit.Handle, EM_LINEINDEX, LineNum, 0);

  if TextPos <> -1 then begin
    // Go to top
    SendMessage(RichEdit.Handle, EM_SETSEL, 0, 0);
    SendMessage(RichEdit.Handle, EM_SCROLLCARET, 0, 0);

    // Get the coordinates for the beginning of the line
    Pos := GetCharPos(RichEdit, TextPos);

    // Scroll from the top
    SendMessage(RichEdit.Handle, WM_VSCROLL,
        MakeWParam(SB_THUMBPOSITION, Max(0, Pos.y - (RichEdit.ClientHeight div 2))), 0);

    // Optionally set the caret to the beginning of the line
    SendMessage(RichEdit.Handle, EM_SETSEL, TextPos, TextPos);
  end;
end;

function ComponentOwnsClass(Component: TComponent; const ClassName: string): Boolean;
var
  i: Integer;
begin
  Assert(Assigned(Component));
  Assert(ClassName <> '');
  Result := False;

  for i := 0 to Component.ComponentCount - 1 do
  begin
    if Component.Components[i].ClassName = ClassName then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure ListboxHorizontalScrollbar(Listbox: TCustomListBox);
var
   i: Integer;
   Width, MaxWidth: Integer;
begin
  Assert(Assigned(Listbox));
  MaxWidth := 0;
  for i := 0 to Listbox.Items.Count - 1 do
  begin
    Width := Listbox.Canvas.TextWidth(Listbox.Items[i]) + 4;
    if Width > MaxWidth then
      MaxWidth := Width;
  end;
  if ListBox is TCheckListBox then
    Inc(MaxWidth, GetSystemMetrics(SM_CXMENUCHECK) + 2);
  SendMessage(Listbox.Handle, LB_SETHORIZONTALEXTENT, MaxWidth, 0);
end;

function GetScrollbarWidth: Integer;
begin
  Result := GetSystemMetrics(SM_CXVSCROLL);
end;

type
  TComboCracker = class(TCustomComboBox);

procedure SizeComboDropdownToItems(Combo: TCustomComboBox; AMax: Integer);
var
  i: Integer;
  MaxWidth: Integer;
  Bitmap: Graphics.TBitmap;
begin
  MaxWidth := Combo.Width;
  Bitmap := Graphics.TBitmap.Create;
  try
    Bitmap.Canvas.Font.Assign(TComboCracker(Combo).Font);
    for i := 0 to Combo.Items.Count - 1 do
      MaxWidth := Max(MaxWidth, Bitmap.Canvas.TextWidth(Combo.Items[i]) + 10);
  finally;
    FreeAndNil(Bitmap);
  end;
  if Combo.Items.Count > TComboCracker(Combo).DropDownCount then
    Inc(MaxWidth, GetSystemMetrics(SM_CXVSCROLL));
  MaxWidth := Min(AMax, MaxWidth);
  if MaxWidth > Combo.Width then
    SendMessage(Combo.Handle, CB_SETDROPPEDWIDTH, MaxWidth, 0)
  else
    SendMessage(Combo.Handle, CB_SETDROPPEDWIDTH, 0, 0)
end;

procedure MakeMultiline(Control: TButtonControl);
begin
  SetWindowLong(Control.Handle, GWL_STYLE, GetWindowLong(Control.Handle, GWL_STYLE) or BS_MULTILINE);
end;

procedure SetListBoxChecked(CheckList: TCheckListBox; Action: TListBoxCheckAction);
var
  i: Integer;
begin
  Assert(Assigned(CheckList));
  for i := 0 to CheckList.Items.Count - 1 do
  begin
    case Action of
      chAll: CheckList.Checked[i] := True;
      chNone: CheckList.Checked[i] := False;
      chInvert: CheckList.Checked[i] := not CheckList.Checked[i];
    end;
  end;
end;

procedure ListBoxDeleteSelected(ListBox: TListBox);
var
  i: Integer;
begin
  Assert(Assigned(ListBox));
  if ListBox.MultiSelect then
  begin
    for i := ListBox.Items.Count - 1 downto 0 do
      if ListBox.Selected[i] then
        ListBox.Items.Delete(I);
  end
  else
    if ListBox.ItemIndex <> -1 then
      ListBox.Items.Delete(ListBox.ItemIndex);
end;

procedure ListViewDeleteSelected(ListView: TListView);
var
  i: Integer;
begin
  ListView.Items.BeginUpdate;
  try
    for i := ListView.Items.Count - 1 downto 0 do
      if ListView.Items[i].Selected then
        ListView.Items.Delete(i);
  finally
    ListView.Items.EndUpdate;
  end;
end;

procedure ListViewSelectAll(ListView: TListView);
var
  i: Integer;
begin
  for i := 0 to ListView.Items.Count - 1 do
    ListView.Items[i].Selected := True;
end;

procedure ListViewResizeColumn(ListView: TListView; ColumnIndex: Integer);
var
  TotalWidth: Integer;
  i: Integer;
  RemainingWidth: Integer;
begin
  Assert(Assigned(ListView));
  Assert(ColumnIndex < ListView.Columns.Count);
  TotalWidth := 0;
  for i := 0 to ListView.Columns.Count - 1 do
    Inc(TotalWidth, ListView.Column[i].Width);
  RemainingWidth := ListView.ClientWidth - TotalWidth;// - GetScrollbarWidth;
  ListView.Column[ColumnIndex].Width := Max(ListView.Column[ColumnIndex].Width + RemainingWidth, 10);
end;

function FindTreeNode(Tree: TTreeView; const NodeText: string): TTreeNode;
var
  Node: TTreeNode;
begin
  Assert(Assigned(Tree));
  Result := nil;
  Node := Tree.Items.GetFirstNode;
  while Assigned(Node) do
  begin
    if SameText(Node.Text, NodeText) then begin
      Result := Node;
      Exit;
    end
    else
      Node := Node.GetNext;
  end;
end;

procedure SetToolBarGradient(ToolBar: TToolBar; Enabled: Boolean);
begin {$IFNDEF GX_VER180_up} {FI:W519} {$ENDIF}
  {$IFDEF GX_VER180_up}
  if Enabled then
    Toolbar.DrawingStyle := dsGradient
  else
    Toolbar.DrawingStyle := dsNormal;
  {$ENDIF GX_VER180_up}
end;

procedure SetEnabledOnControlAndChildren(Control: TControl; Value: Boolean);
var
  i: Integer;
  WinControl: TWinControl;
begin
  Assert(Assigned(Control));
  Control.Enabled := Value;
  if Control is TWinControl then begin
    WinControl := (Control as TWinControl);
    for i := 0 to WinControl.ControlCount - 1 do
      SetEnabledOnControlAndChildren(WinControl.Controls[i], Value);
  end;
end;

type
  TControlCracker = class(TControl);

function SetDefaultFont(Control: TControl): string;
begin
  Result := '';
  if Assigned(Control) then begin
    if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) then
      TControlCracker(Control).Font.Name := 'MS Shell Dlg 2'
    else
      TControlCracker(Control).Font.Name := 'MS Shell Dlg';
    Result := TControlCracker(Control).Font.Name;
    if IsWindowsVistaOrLater then // Vista and later default the UI to 9 point (not 8)
      TControlCracker(Control).Font.Size := 9
    else
      TControlCracker(Control).Font.Size := 8;
  end;
end;

procedure SetFontBold(Control: TControl);
begin
  TControlCracker(Control).Font.Style := TControlCracker(Control).Font.Style + [fsBold];
end;

procedure SetFontUnderline(Control: TControl);
begin
  TControlCracker(Control).Font.Style := TControlCracker(Control).Font.Style + [fsUnderline];
end;

procedure SetFontSize(Control: TControl; SizeChange: Integer);
begin
  TControlCracker(Control).Font.Size := TControlCracker(Control).Font.Size + SizeChange;
end;

procedure SetFontColor(Control: TControl; Color: TColor);
begin
  TControlCracker(Control).Font.Color := Color;
end;

function GetAverageCharWidth(Control: TControl): Integer;
var
  Width: Integer;
  Bitmap: Graphics.TBitmap;
begin
  Assert(Assigned(Control));

  Bitmap := Graphics.TBitmap.Create;
  try
    Bitmap.Canvas.Font.Assign(TControlCracker(Control).Font);
    Width := Bitmap.Canvas.TextWidth(SAllAlphaNumericChars);
    Result := Round(Width / Length(SAllAlphaNumericChars));
  finally
    FreeAndNil(Bitmap);
  end;
end;

procedure SetParentBackgroundValue(Panel: TCustomPanel; Value: Boolean);
begin {$IFNDEF GX_VER150_up} {FI:W519} {$ENDIF}
  {$IFDEF GX_VER150_up} // Delphi 7+
  Assert(Assigned(Panel));
  Panel.ParentBackground := Value;
  {$ENDIF}
end;

procedure SetParentBackgroundValue(GroupBox: TGroupBox; Value: Boolean);
begin {$IFNDEF GX_VER150_up} {FI:W519} {$ENDIF}
  {$IFDEF GX_VER150_up} // Delphi 7+
  Assert(Assigned(GroupBox));
  GroupBox.ParentBackground := Value;
  {$ENDIF}
end;

function InheritsFromClass(ClassType: TClass; const ClassName: string): Boolean;
begin
  Result := True;
  while ClassType <> nil do
  begin
    if SameText(ClassType.ClassName, ClassName) then
      Exit;
    ClassType := ClassType.ClassParent;
  end;
  Result := False;
end;

function BooleanText(const Value: Boolean): string;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

procedure OutputComponentList(const StartComponent: TComponent; Owned: Boolean);

  function GetComponentProperties(Comp: TComponent): string;
  var
    Names: TStringList;
    Types: TStringList;
  begin
    Types := nil;
    Names := TStringList.Create;
    try
      Types := TStringList.Create;
      GetPropertyNames(Comp.ClassType, Names, Types, nil);
      Result := Names.CommaText;
    finally
      FreeAndNil(Names);
      FreeAndNil(Types);
    end;
  end;

  procedure OutputChildren(Parent: TComponent);
  var
    StartControl: TWinControl;
    AControl: TControl;
    i: Integer;
  begin
    if Parent is TWinControl then
    begin
      {$IFOPT D+} SendIndent; {$ENDIF}
      try
        StartControl := Parent as TWinControl;
        for i := 0 to StartControl.ControlCount - 1 do
        begin
          AControl := StartControl.Controls[i];
          Assert(Assigned(AControl));
          {$IFOPT D+} SendDebugFmt('%s: %s  Top: %d  Left: %d  Height: %d  Width: %d  Visible: %d  ParentClass: %s  ParentParent: %s  Properties: %s', [AControl.Name, AControl.ClassName, AControl.Top, AControl.Left, AControl.Height, AControl.Width, Ord(AControl.Visible), AControl.ClassType.ClassParent.ClassName, AControl.ClassType.ClassParent.ClassParent.ClassName, GetComponentProperties(AControl)]); {$ENDIF}
          OutputChildren(AControl);
        end;
      finally
        {$IFOPT D+} SendUnIndent; {$ENDIF}
      end; //FI:W502
    end;
  end;

var
  i: Integer;
  AComponent: TComponent;
begin
  Assert(StartComponent <> nil);

  if Owned then begin
    {$IFOPT D+} SendDebugFmt('Writing owned component list for "%s"...', [StartComponent.Name]); {$ENDIF}
    for i := 0 to StartComponent.ComponentCount - 1 do
    begin
      AComponent := StartComponent.Components[i];
      Assert(Assigned(AComponent));
      {$IFOPT D+} SendDebug(AComponent.Name + ': ' + AComponent.ClassName); {$ENDIF}
    end;
  end
  else
  begin
    {$IFOPT D+} SendDebugFmt('Child components for %s: %s', [StartComponent.Name, StartComponent.ClassName]); {$ENDIF}
    OutputChildren(StartComponent);
  end;
end;

procedure CenterForm(const Form: TCustomForm);
var
  Rect: TRect;
begin
  if Form = nil then
    Exit;

  if not Form.Floating then
    Exit;

  Rect := GetScreenWorkArea(Form);
  Form.SetBounds(Rect.Left + (Rect.Right - Rect.Left - Form.Width) div 2,
    Rect.Top + (Rect.Bottom - Rect.Top - Form.Height) div 2, Form.Width, Form.Height);
end;

function GetScreenWorkArea(const Form: TCustomForm = nil): TRect;
var
  Monitor: TMonitor;
begin
   if Assigned(Form) then
   begin
     Monitor := Screen.MonitorFromWindow(Form.Handle, mdNearest);
     if Assigned(Monitor) then
     begin
       Result := Monitor.WorkareaRect;
       Exit; //==>
     end;
   end;
   Result := Screen.WorkAreaRect;
end;

procedure EnsureFormVisible(const Form: TCustomForm);
var
  Rect: TRect;
begin
  Assert(Assigned(Form));
  if not Form.Floating then
    Exit;
  Rect := GetScreenWorkArea(Form);
  if (Form.Left + Form.Width > Rect.Right) then
    Form.Left := Form.Left - ((Form.Left + Form.Width) - Rect.Right);
  if (Form.Top + Form.Height > Rect.Bottom) then
    Form.Top := Form.Top - ((Form.Top + Form.Height) - Rect.Bottom);
  if Form.Left < Rect.Left then
    Form.Left := Rect.Left;
  if Form.Top < Rect.Top then
    Form.Top := Rect.Top;
end;

function GetWindowClassName(WinHandle: THandle): string;
const
  ClassNameBufferSize = 254;
var
  ClassNameBuffer: array[0..ClassNameBufferSize-1] of Char;
  ApiResult: Integer;
begin
  Result := '';
  if (WinHandle > 0) then
  begin
    ApiResult := GetClassName(WinHandle, ClassNameBuffer, SizeOf(ClassNameBuffer));
    Win32Check(ApiResult <> 0);
    Assert(ApiResult < ClassNameBufferSize, 'Found class name larger than fixed buffer size');
    Result := ClassNameBuffer;
  end;
end;

procedure ConvertBitmapToIcon(const Bitmap: Graphics.TBitmap; Icon: TIcon);
var
  il: TImageList;
begin
  if (not Assigned(Bitmap)) or (not Assigned(Icon)) then
  begin
    {$IFOPT D+} SendDebugError('No valid bitmap or icon object passed to ConvertBitmapToIcon'); {$ENDIF}
    Exit;
  end;

  Assert(Assigned(Bitmap));
  Assert(Assigned(Icon));

  il := TImageList.CreateSize(Bitmap.Width, Bitmap.Height);
  try
    il.AddMasked(Bitmap, Bitmap.TransparentColor);
    il.GetIcon(0, Icon);
  finally
    il.Free;
  end;
end;

procedure ClonePersistent(Source, Dest: TPersistent);
var
  i: Integer;
  PropCount: Integer;
  PropList: TPropList;
  PropName: string;
begin
  Assert(Assigned(Source) and Assigned(Dest));

  PropCount := GetPropList(Source.ClassInfo, tkAny, @PropList);
  for i := 0 to PropCount - 1 do
  begin
    PropName := string(PropList[i].Name);
    case PropList[i].PropType^.Kind of
      tkLString, tkWString, tkVariant {$IFDEF GX_VER200_up}, tkUString {$ENDIF}:
        SetPropValue(Dest, PropName, GetPropValue(Source, PropName));
      tkInteger, tkChar, tkWChar, tkClass:
        SetOrdProp(Dest, PropName, GetOrdProp(Source, PropName));
      tkFloat:
        SetFloatProp(Dest, PropName, GetFloatProp(Source, PropName));
      tkString:
        SetStrProp(Dest, PropName, GetStrProp(Source, PropName));
      tkMethod:
        SetMethodProp(Dest, PropName, GetMethodProp(Source, PropName));
      tkInt64:
        SetInt64Prop(Dest, PropName, GetInt64Prop(Source, PropName));
      tkEnumeration:
        SetEnumProp(Dest, PropName, GetEnumProp(Source, PropName));
      tkSet:
        SetSetProp(Dest, PropName, GetSetProp(Source, PropName));
      //tkRecord, tkArray, tkDynArray, tkUnknown are ignored for now
    end;
  end;
end;

// Based on a procedure by Bob Swart for Delphi 1: http://www.drbob42.com/uk-bug/hood-01.htm
procedure GetPropertyNames(AClass: TClass; PropertyNames, PropertyTypeNames, PropertyTypesToLookFor: TStrings);
var
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  PropList: PPropList;
  ParentName: string;
  PropertyName: string;
  PropertyTypeName: string;
  IndexProperties: Integer;
begin
  PropertyNames.Clear;
  PropertyTypeNames.Clear;
  TypeInfo := AClass.ClassInfo;
  if (TypeInfo^.Kind = tkClass) then
  begin
    TypeData := GetTypeData(TypeInfo);
    ParentName := string(TypeData^.ParentInfo^.Name);
    if (ParentName <> string(TypeInfo^.Name)) and (GetClass(ParentName) <> nil) then
      GetPropertyNames(GetClass(ParentName), PropertyNames, PropertyTypeNames, PropertyTypesToLookFor);
    if (TypeData^.PropCount > 0) then
    begin
      New(PropList);
      GetPropInfos(TypeInfo, PropList);
      for IndexProperties := 0 to Pred(TypeData^.PropCount) do
      begin
        PropertyTypeName := string(PropList^[IndexProperties]^.PropType^.Name);
        if (PropertyTypesToLookFor = nil) or (PropertyTypesToLookFor.IndexOf(PropertyTypeName) > -1) then
        begin
          PropertyName := string(PropList^[IndexProperties]^.Name);
          if (PropertyNames.IndexOf(PropertyName) < 0) then
          begin
            PropertyNames.Add(PropertyName);
            PropertyTypeNames.Add(PropertyTypeName)
          end;
        end;
      end;
      Dispose(PropList)
    end;
  end;
end;

procedure AssertIsDprOrPas(const FileName: string);
resourcestring
  SExpertForPasOrDprOnly = 'This expert is for use in .pas or .dpr files only';
begin
  if not IsDprOrPas(FileName) then
    raise Exception.Create(SExpertForPasOrDprOnly);
end;

procedure AssertIsPasOrInc(const FileName: string);
resourcestring
  SExpertForPasOrIncOnly = 'This expert is for use in .pas or .inc files only';
begin
  if not (IsPas(FileName) or IsInc(FileName)) then
    raise Exception.Create(SExpertForPasOrIncOnly);
end;

procedure AssertIsDprOrPasOrInc(const FileName: string);
resourcestring
  SExpertForDprOrPasOrIncOnly = 'This expert is for use in .pas, .dpr, and .inc files only';
begin
  if not (IsDprOrPas(FileName) or IsInc(FileName)) then
    raise Exception.Create(SExpertForDprOrPasOrIncOnly);
end;

function IsDprOrPas(const FileName: string): Boolean;
begin
  Result := FileMatchesExtensions(FileName, ['.pas', '.dpr']);
end;

function IsPascalSourceFile(const FileName: string): Boolean;
begin
  Result := IsDprOrPas(FileName) or IsInc(FileName) or IsDelphiPackage(FileName);
end;

function IsBdsProjectFile(const FileName: string): Boolean;
begin
  Result := IsDpr(FileName)
    or IsBpg(FileName)
    or IsBpr(FileName)
    or IsBdsgroup(FileName)
    or IsBdsproj(FileName)
    or IsDproj(FileName)
    or IsPackage(FileName)
    or IsCsproj(FileName);
end;

function IsProjectSource(const FileName: string): Boolean;
begin
  Result := IsDpr(FileName) or IsBpr(FileName) or IsPackage(FileName);
end;

function IsBdsSourceFile(const FileName: string): Boolean;
begin
  Result := IsBdsProjectFile(FileName)
    or IsPascalSourceFile(FileName)
    or IsCppSourceModule(FileName)
    or IsWebFile(FileName)
    or IsCs(FileName)
    or IsCsProj(FileName)
    or IsVbFile(FileName);
end;

function IsDpr(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.dpr');
end;

function IsBpr(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.bpr');
end;

function IsBpk(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.bpk');
end;

function IsBpg(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.bpg');
end;

function IsPas(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.pas');
end;

function IsInc(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.inc');
end;

function IsDfm(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.dfm');
end;

function IsForm(const FileName: string): Boolean;
begin
  Result := FileMatchesExtensions(FileName, ['.dfm', '.xfm', '.nfm', '.fmx']);
end;

function IsXfm(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.xfm');
end;

function IsNfm(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.nfm');
end;

function IsDpk(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.dpk');
end;

function IsDcp(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.dcp');
end;

function IsBdsgroup(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.bdsgroup');
end;

function IsGroupproj(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.groupproj');
end;

function IsBdsproj(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.bdsproj');
end;

function IsDproj(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.dproj');
end;

function IsBdsprojOrDproj(const FileName: string): Boolean;
begin
  Result := IsBdsproj(FileName) or IsDproj(FileName);
end;

function IsPackage(const FileName: string): Boolean;
begin
  Result := FileMatchesExtensions(FileName, ['.dpk', '.dpkw', '.bpk']);
end;

function IsDelphiPackage(const FileName: string): Boolean;
begin
  Result := FileMatchesExtensions(FileName, ['.dpk', '.dpkw']);
end;

function IsCsproj(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.csproj');
end;

function IsHtml(const FileName: string): Boolean;
begin
  Result := FileMatchesExtensions(FileName, ['.html', '.htm'])
end;

function IsWebFile(const FileName: string): Boolean;
begin
  Result := IsHtml(FileName) or FileMatchesExtensions(FileName, ['.js', '.css', '.php', '.php3', '.asp', '.aspx']);
end;

function IsCs(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.cs');
end;

function IsVbFile(const FileName: string): Boolean;
begin
  Result := FileMatchesExtensions(FileName, ['.vb', '.vbproj']);
end;

function IsExecutable(const FileName: string): Boolean;
begin
  Result := FileMatchesExtensions(FileName, ['.dll', '.exe', '.bpl', '.com', '.ocx']);
end;

function IsCppSourceModule(const FileName: string): Boolean;
begin
  Result := FileMatchesExtensions(FileName, ['.cpp', '.c', '.hpp', '.h', '.cxx', '.cc', '.hxx', '.hh', '.asm', '.bpf']);
end;

function IsCpp(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.cpp');
end;

function IsC(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.c');
end;

function IsH(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.h');
end;

function IsToDo(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.todo');
end;

function IsTypeLibrary(const FileName: string): Boolean;
begin
  Result := FileMatchesExtensions(FileName, ['.tlb', '.olb', '.ocx', '.dll', 'exe']);
end;

function IsKnownSourceFile(const FileName: string): Boolean;
begin
  Result := IsDprOrPas(FileName) or IsCppSourceModule(FileName);
end;

function IsTextFile(const FileName: string): Boolean;
const
  TextFileTypes: array[0..38] of string = ('.TXT', '.ASC', '.ME', '.INI', '.DIZ',
    '.BPG', '.PY', '.BAT', '.RC', '.DOF', '.KOF', '.DSK', '.ISS', '.CFG', '.CONF',
    '.MAP', '.DEBUGLOG', '.LOG', '.DRC', '.DIFF', '.EXTRA', '.FBPINF', '.FBP',
    '.FBP2', '.FBP3', '.FBP4', '.FBP5', '.FBP6', '.IDL', '.CSV', '.TSV', '.SLN',
    '.PHP', '.PHP3', '.USED', '.VB', '.VBE', '.VBS', '.REJ');
begin
  Result := FileMatchesExtensions(FileName, TextFileTypes);
end;

function IsXML(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.xml');
end;

function IsXMLFormat(const FileName: string): Boolean;
begin
  Result := IsXML(FileName) or IsBDSGroup(FileName) or IsBpr(FileName) or IsBDSProj(FileName)
    or IsDproj(FileName) or IsGroupProj(FileName) or IsCsproj(FileName)
    or FileMatchesExtensions(FileName, ['.local', '.manifest', '.resx', '.cmbx', '.prjx', '.user']);
end;

function IsSQL(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.sql');
end;

function IsDcu(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.dcu');
end;

function IsDcuil(const FileName: string): Boolean;
begin
  Result := FileMatchesExtension(FileName, '.dcuil');
end;

function FindPropInfo(Instance: TObject; const PropName: string): PPropInfo;
begin
  Result := GetPropInfo(Instance, PropName);
  if Result = nil then
    RaisePropertyError(PropName);
end;

procedure RaisePropertyError(const PropName: string);
resourcestring
  PropNotFound = 'Property %s not found';
begin
  raise EPropertyError.CreateFmt(PropNotFound, [PropName]);
end;

function FindTypeInfo(Instance: TObject; const PropName: string): PTypeInfo;
var
  PropInfo: PPropInfo;
begin
  Assert(Assigned(Instance));
  PropInfo := FindPropInfo(Instance, PropName);
  Assert(Assigned(PropInfo));
  Result := PPropInfo(PropInfo)^.PropType^;
  Assert(Assigned(Result));
end;

function IsPropWriteable(Instance: TObject; const PropName: string): Boolean;
var
  PropInfo: PPropInfo;
begin
  Result := False;
  if Assigned(Instance) and NotEmpty(PropName) then
  begin
    if not IsPublishedProp(Instance, PropName) then
      Exit;
    PropInfo := FindPropInfo(Instance, PropName);
    if Assigned(PropInfo) then
      Result := Assigned(PropInfo.SetProc);
  end;
end;

function GetMembersValues(TypeInfo: PTypeInfo; const ChangeString: string; var Members: TStringList): Integer;
var
  Sep, Blank: TSysCharSet;
  i, Value: Integer;
begin
  Result := -1;
  Sep := [',', '-', '+', '[', ']'];
  Blank := [' '];

  ExtractStrings(Sep, Blank, PChar(ChangeString), Members);
  if StringInArray(CompressWhiteSpace(ChangeString), ['[]', '[ ]']) then
    Result := 0;

  For i := 0 to Members.Count - 1 do
  begin
    Value := GetEnumValue(TypeInfo, Members.Strings[i]);
    if Value = -1 then
    begin
      Result := -1;
      Break;
    end
    else
    begin
      if Result = -1 then
        TIntegerSet(Result) := [Value]
      else
        TIntegerSet(Result) := TIntegerSet(Result) + [Value];
    end;
  end;
end;

function UpdateSetFromStringDesc(CurrentSetVal: Integer;
  const ChangeString: string; PropInfo: PPropInfo): Integer;
var
  Prefix: string;
  vTypeInfo: PTypeInfo;
  Value: Integer;
  Members: TStringList;
begin
  Result := -1;

  Prefix := LeftStr(ChangeString, 1);
  vTypeInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;

  Members := TStringList.Create;
  try
    Value := GetMembersValues(vTypeInfo, Trim(ChangeString), Members);

    if Value <> -1 then
    begin
      if Prefix = '-' then
         TIntegerSet(Result) := TIntegerSet(CurrentSetVal) - TIntegerSet(Value)
      else if Prefix = '[' then
         TIntegerSet(Result) := TIntegerSet(Value)
      else
         TIntegerSet(Result) := TIntegerSet(CurrentSetVal) + TIntegerSet(Value);
    end;
  except
    FreeAndNil(Members);
  end;
end;

function ApplyValueToSetProperty(Obj: TObject; const APropertyName, Value: string): Integer;
var
  PropInfo: PPropInfo;
  MemberValue: Integer;
begin
  Assert(Assigned(Obj));
  PropInfo := FindPropInfo(Obj, APropertyName);
  Result := GetOrdProp(Obj, PropInfo);
  MemberValue := UpdateSetFromStringDesc(Result, Value, PropInfo);
  if MemberValue > -1 then
    TIntegerSet(Result) := TIntegerSet(MemberValue)
  else
    raise Exception.CreateFmt('%s is not a valid set member for the set %s', [Value, APropertyName]);
end;

function GetEnumValueFromStr(Obj: TObject; const PropertyName, Value: WideString): Integer;
var
  TypeInfo: PTypeInfo;
begin
  if IsValidIdent(Value) and Assigned(Obj) then
  begin
    TypeInfo := FindTypeInfo(Obj, PropertyName);
    Result := GetEnumValue(TypeInfo, Value);
  end
  else
    Result := StrToIntDef(Trim(Value), -1);
  if Result < 0 then
    raise Exception.CreateFmt('%s is not a valid value for the property %s', [Value, PropertyName]);
end;

function FileMatchesExtensions(const FileName, FileExtensions: string): Boolean;
begin
  Result := (AnsiCaseInsensitivePos(ExtractFileExt(FileName), FileExtensions) <> 0);
end;

function FileMatchesExtensions(const FileName: string; FileExtensions: array of string): Boolean;
begin
  Result := StringInArray(ExtractFileExt(FileName), FileExtensions);
end;

function FileMatchesExtension(const FileName, FileExtension: string): Boolean;
begin
  Result := FileMatchesExtensions(FileName, [FileExtension]);
end;

function CompareFileName(const S1, S2: string): Integer;
begin
  Result := CompareText(S1, S2);
end;

function SameFileName(const S1, S2: string): Boolean;
begin
  Result := (CompareText(S1, S2) = 0);
end;

function SamePathName(const S1, S2: string): Boolean;
begin
  Result := (CompareText(RemoveSlash(S1), RemoveSlash(S2)) = 0);
end;

function GetFileSize(const FileName: string): Integer;
var
  F: file of Byte;
  OldFileMode: Byte;
begin
  AssignFile(F, FileName);
  try
    OldFileMode := FileMode;
    FileMode := 0; // Read-only

    Reset(F);
    try
      Result := FileSize(F);
    finally
      CloseFile(F);
      FileMode := OldFileMode;
    end;
  except
    on E: Exception do
      Result := 0;
  end;
end;

function GetFileDate(const FileName: string): TDateTime;
begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt('File %s does not exist to get the modified date', [FileName]);
  {$IFDEF GX_VER180_up}
  if not FileAge(FileName, Result) then
    raise Exception.Create('Unable to get modified date for ' + FileName);
  {$ELSE not GX_VER180_up}
  Result := FileDateToDateTime(FileAge(FileName));
  {$ENDIF}
end;

// UNTESTED NOT COMPLETE !!!!!!!!!!!!!!!!!!!!!!!!
function FileToWideString(const FileName: WideString): WideString;
var
  FileStream: TFileStream;
  Word1: Word;
  Word2: Word;
  i: Integer;
  BytesRead: Integer;
  BOMSize: Integer;
  UCS4Str: UCS4String;
  StringBytes: Integer;
  ExtraBytes: Integer;

  procedure RewindStreamPastByte(BytesFromStart: Integer);
  begin
    FileStream.Seek(BytesFromStart, soBeginning);
  end;

  procedure ReadFullStreamAsASCIIANSI;
  var
    Str: AnsiString;
  begin
    RewindStreamPastByte(0);
    SetLength(Str, FileStream.Size);
    if FileStream.Size > 0 then
      FileStream.Read(Str[1], FileStream.Size);
    Result := WideString(Str);
  end;

const
  ExtraBytesMsg = 'UNICODE BOM claims to be %s, but contains %d extra bytes at the end (total file size is %d bytes)';
begin
  Result := '';
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Word1 := 0;
    Word2 := 0;
    BytesRead := FileStream.Read(Word1, SizeOf(Word1));
    BytesRead := BytesRead + FileStream.Read(Word2, SizeOf(Word2));
    if BytesRead >= 2 then
    begin
      // UTF-32/UCS-4 (4 byte BOM, 4 byte chars)
      if (BytesRead = 4) and (((Word1 = $FFFE) and (Word2 = $0000)) or ((Word1 = $0000) and (Word2 = $FEFF))) then
      begin
        // The file stream points to the first byte of the text (byte 5)
        BOMSize := 4;
        StringBytes := FileStream.Size - BOMSize;
        ExtraBytes := StringBytes mod 4;
        if ExtraBytes <> 0 then
          raise Exception.CreateFmt(ExtraBytesMsg, ['UTF-32/UCS-4', ExtraBytes, FileStream.Size]);
        if FileStream.Size > BOMSize then
        begin
          SetLength(UCS4Str, StringBytes div 4);
          FileStream.Read(UCS4Str[0], StringBytes);
          Result := UCS4StringToWideString(UCS4Str);
          if Word1 = $0000 then // Little endian BOM, needs byte swap
            for i := 1 to Length(Result) do
            begin
              raise Exception.Create('Little endian UTF-32/UCS-4 byte swap not implemented');
              //Result[i] := UCS4Char(Swap(Result[i]));
            end;
        end;
      end
      // UTF-8 (3 byte BOM, 1 byte chars)
      else if (BytesRead >= 3) and (Word1 = $BBEF) and (Lo(Word2) = $BF) then // UTF-8
      begin
        BOMSize := 3;
        StringBytes := FileStream.Size - BOMSize;
        if FileStream.Size > BOMSize then
        begin
          RewindStreamPastByte(BOMSize);
          SetLength(Result, StringBytes);
          FileStream.Read(Result[1], StringBytes);
        end;
      end
      // UTF-16/UCS-2 (2 byte BOM, 2 byte chars)
      else if (BytesRead >= 2) and ((Word1 = $FEFF) or (Word1 = $FFFE)) then
      begin
        BOMSize := 2;
        StringBytes := FileStream.Size - BOMSize;
        ExtraBytes := StringBytes mod 2;
        if ExtraBytes <> 0 then
          raise Exception.CreateFmt(ExtraBytesMsg, ['UTF-16/UCS-2', ExtraBytes, FileStream.Size]);
        if FileStream.Size > BOMSize then
        begin
          RewindStreamPastByte(BOMSize);
          SetLength(Result, (FileStream.Size - BOMSize) div 2);
          FileStream.Read(Result[1], FileStream.Size - BOMSize);
          if Word1 = $FFFE then // Little endian BOM, needs byte swap
            for i := 1 to Length(Result) do
              Result[i] := WideChar(Swap(Word(Result[i])));
        end;
      end
      else
        ReadFullStreamAsASCIIANSI;
    end
    else
      ReadFullStreamAsASCIIANSI;
  finally
    FreeAndNil(FileStream);
  end;
end;

// UNTESTED !!!!!!!!!!!!!!!!!!!!!!!!
procedure AnsiStreamToWideString(Stream: TStream; var Data: WideString);
var
  AnsiStr: string;
begin
  Stream.Position := 0;
  SetLength(AnsiStr, Stream.Size);
  Stream.Write(AnsiStr[1], Stream.Size);
  Data := AnsiStr; // Let the RTL do the convesion for us
end;

function GetFileVersionNumber(const FileName: string;
  MustExist: Boolean = True; MustHaveVersion: Boolean = True): TVersionNumber;
var
  VersionInfoBufferSize: DWORD;
  dummyHandle: DWORD;
  VersionInfoBuffer: Pointer;
  FixedFileInfoPtr: PVSFixedFileInfo;
  VersionValueLength: UINT;
begin
  Result.IsValid := False;
  if not FileExists(FileName) then begin
    if MustExist then
      raise Exception.Create(ExpandFileName(FileName) + ' does not exist to obtain VersionInfo');
    Exit;
  end;

  VersionInfoBufferSize := GetFileVersionInfoSize(PChar(FileName), dummyHandle);
  if VersionInfoBufferSize = 0 then begin
    if MustHaveVersion then
      raise EVersionInfoNotFound.Create(SysErrorMessage(GetLastError));
    Exit;
  end;

  GetMem(VersionInfoBuffer, VersionInfoBufferSize);
  try
    if not GetFileVersionInfo(PChar(FileName), dummyHandle,
                              VersionInfoBufferSize, VersionInfoBuffer) then begin
      if MustHaveVersion then
        RaiseLastOSError;
      Exit;
    end;

    // Retrieve root block / VS_FIXEDFILEINFO
    if not VerQueryValue(VersionInfoBuffer, '\',
                         Pointer(FixedFileInfoPtr), VersionValueLength) then begin
      if MustHaveVersion then
        RaiseLastOSError;
      Exit;
    end;

    Result.dwFileVersionMS := FixedFileInfoPtr^.dwFileVersionMS;
    Result.dwFileVersionLS := FixedFileInfoPtr^.dwFileVersionLS;
    Result.IsValid := True;
  finally
    FreeMem(VersionInfoBuffer);
  end;
end;

function GetFileVersionString(const FileName: string;
  MustExist: Boolean = True; MustHaveVersion: Boolean = True): string;
resourcestring
  SUnknown = '<unknown>';
var
  Version: TVersionNumber;
begin
  Version := GetFileVersionNumber(FileName, MustExist, MustHaveVersion);
  if Version.IsValid then
    Result := Format('%d.%d.%d.%d', [Version.Major, Version.Minor, Version.Release, Version.Build])
  else
    Result := SUnknown;
end;

function FindFileInSearchPath(const _fn: string; _SearchPath: TStrings; out _FullFn: string): Boolean;
var
  i: Integer;
  dir: string;
begin
  Assert(Assigned(_SearchPath));
  Result := True;
  for i := 0 to _SearchPath.Count - 1 do begin
    dir := AddSlash(_SearchPath[i]);
    if FileExists(dir + _fn) then begin
      _FullFn := dir + _fn;
      Exit; //==>
    end;
  end;
  Result := False;
end;

function GXShellExecute(const FileName, Parameters: string; const RaiseException: Boolean): Boolean;
var
  ReturnVal: Integer;
begin
  ReturnVal := ShellExecute(Application.Handle, nil, PChar(FileName), PChar(Parameters),
                            PChar(ExtractFilePath(FileName)), SW_SHOWNORMAL);
  Result := (ReturnVal > 32);
  if (not Result) and RaiseException then
    raise Exception.CreateFmt('%s (%s)', [SysErrorMessage(GetLastError), FileName]);
end;

function GXShellOpen(const _Parameters: string; const RaiseException: Boolean): Boolean;
var
  ReturnVal: Integer;
begin
  ReturnVal := ShellExecute(Application.Handle, 'open', PChar(_Parameters), nil,
    PChar(ExtractFilePath(Application.ExeName)), SW_SHOWNORMAL);
  Result := (ReturnVal > 32);
  if (not Result) and RaiseException then
    raise Exception.CreateFmt('%s (%s)', [SysErrorMessage(GetLastError), _Parameters]);
end;

procedure FreeItemIDList(var IDList: PItemIDList);
var
  Malloc: IMalloc;
begin
  if Succeeded(SHGetMalloc(Malloc)) then
  begin
    if Malloc.DidAlloc(IdList) = 1 then
    begin
      Malloc.Free(IDList);
      IDList := nil;
    end;
  end;
end;

function GetSpecialFolderPath(const FolderID: Integer): string;
var
  IDList: PItemIDList;
begin
  Result := '';
  if SHGetSpecialFolderLocation(0, FolderID, IDList) = NOERROR then
  try
    SetLength(Result, MAX_PATH);
    if SHGetPathFromIDList(IDList, PChar(Result)) then
      SetLength(Result, StrLen(PChar(Result)));
  finally
    FreeItemIDList(IDList);
  end;
  if IsEmpty(Result) then
    raise Exception.Create('Unable to get Windows path for system folder: ' + IntToStr(FOlderID));
end;

function GetUserApplicationDataFolder: string;
begin
  Result := GetSpecialFolderPath(CSIDL_APPDATA);
end;

function GetUserLocalApplicationDataFolder: string;
{$if not defined(CSIDL_LOCAL_APPDATA)}
const
  CSIDL_LOCAL_APPDATA = $001c; { <user name>\Local Settings\Application Data (non roaming) }
{$ifend}
begin
  Result := GetSpecialFolderPath(CSIDL_LOCAL_APPDATA)
end;

function RunningWindows: Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := True;
  {$ELSE not MSWINDOWS}
  Result := False;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
function GetVersionExA(lpVersionInformation: Pointer): BOOL; stdcall;
  external kernel32 name 'GetVersionExA';

function GetOSString: string;
var
  OSPlatform: string;
  BuildNumber: Integer;
  SysInfo: TSystemInfo;
  VerInfoEx: TOSVersionInfoEx;
begin
  Result := 'Unknown Windows Version';
  OSPlatform := 'Windows';
  BuildNumber := 0;
  ZeroMemory(@SysInfo, SizeOf(SysInfo));
  GetSystemInfo(SysInfo);

  ZeroMemory(@VerInfoEx, SizeOf(VerInfoEx));
  VerInfoEx.dwOSVersionInfoSize := SizeOf(TOSVersionInfoEx);
  if not GetVersionExA(@VerInfoEx) then
    VerInfoEx.dwOSVersionInfoSize := 0;

  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS:
      begin
        BuildNumber := Win32BuildNumber and $0000FFFF;
        case Win32MinorVersion of
          0..9:
            begin
              if Trim(Win32CSDVersion) = 'B' then
                OSPlatform := 'Windows 95 OSR2'
              else
                OSPlatform := 'Windows 95';
            end;
          10..89:
            begin
              if Trim(Win32CSDVersion) = 'A' then
                OSPlatform := 'Windows 98'
              else
                OSPlatform := 'Windows 98 SE';
            end;
          90:
            OSPlatform := 'Windows ME';
        end;
      end;
    VER_PLATFORM_WIN32_NT:
      begin
        Result := GetOsVersionString;
        Exit; //==>
      end;
    VER_PLATFORM_WIN32s:
      begin
        OSPlatform := 'Win32s';
        BuildNumber := Win32BuildNumber;
      end;
  end;
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) or
    (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    if Trim(Win32CSDVersion) = '' then
      Result := Format('%s %d.%d (Build %d)', [OSPlatform, Win32MajorVersion,
        Win32MinorVersion, BuildNumber])
    else
      Result := Format('%s %d.%d (Build %d: %s)', [OSPlatform, Win32MajorVersion,
        Win32MinorVersion, BuildNumber, Win32CSDVersion]);
  end
  else
    Result := Format('%s %d.%d', [OSPlatform, Win32MajorVersion, Win32MinorVersion])
end;
{$ENDIF MSWINDOWS}

function IsWindowsVistaOrLater: Boolean;
begin
  Result := RunningWindows and (Win32MajorVersion >= 6);
end;

{$IFDEF MSWINDOWS}
function GetCurrentUser: string;
var
  NameBufferSize: DWORD;
  NameBuffer: array[0..256] of Char;
begin
  NameBufferSize := SizeOf(NameBuffer);
  if Windows.GetUserName(NameBuffer, NameBufferSize) then
    Result := NameBuffer
  else
    Result := '';
end;
{$ENDIF MSWINDOWS}

function ExtractUpperFileExt(const FileName: string): string;
begin
  Result := AnsiUpperCase(ExtractFileExt(FileName));
end;

function ExtractPureFileName(const FileName: string): string;
begin
  Result := ExtractFileName(FileName);
  if Result = '' then
    Exit;
  Result := ChangeFileExt(Result, '');
end;

function FileIsReadOnly(const FileName: string): Boolean;
begin
  Result := False;
  if FileExists(FileName) then
    Result := SysUtils.FileIsReadOnly(FileName);
end;

function CanWriteToDirectory(const Dir: string): Boolean;
begin
  Result := False;
  if SysUtils.DirectoryExists(Dir) then
    Result := CanCreateFile(AddSlash(Dir) + 'GExpertsDirectoryWritePermissionsTest.xyzz');
end;

function CanCreateFile(const FileName: string): Boolean;
var
  Handle: THandle;
begin
  Result := False;
  if SysUtils.DirectoryExists(ExtractFileDir(FileName)) then begin
    Handle := FileCreate(FileName);
    Result := Handle > 0;
    if Result then begin
      FileClose(Handle);
      SysUtils.DeleteFile(FileName);
    end;
  end;
end;

function GetDirectory(var Dir: string; Owner: TCustomForm): Boolean;
resourcestring
  SSelDir = 'Select a Directory';
begin
  Result := GetDirectory(SSelDir,  Dir, Owner);
end;

function GetDirectory(const Caption: string; var Dir: string; Parent: TWinControl = nil): Boolean;
var
  OldErrorMode: UINT;
  BrowseRoot: WideString;
begin
  if Parent = nil then
    Parent := Screen.ActiveCustomForm;
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    BrowseRoot := '';
    Result := dzSelectDirectory(Caption, BrowseRoot, Dir, [sdNewUI], Parent);
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

function ShowOpenDialog(const Title: string; const Extension: string; Files: TStrings;
  const Filter: string = ''): boolean;
var
  Dlg: TOpenDialog;
  fn: string;
  dir: string;
begin
  Dlg := TOpenDialog.Create(nil);
  try
    fn := '';
    if Files.Count > 0 then
      fn := Files[0];
    Dlg.FileName := fn;
    Dlg.DefaultExt := Extension;
    if Filter = '' then
      Dlg.Filter := MakeDialogExtensionString(Extension)
    else
      Dlg.Filter := Format('%s|All Files (%s)|%s', [Filter, AllFilesWildCard, AllFilesWildCard]);
    Dlg.Options := Dlg.Options + [ofPathMustExist, ofFileMustExist, ofAllowMultiSelect, ofDontAddToRecent];
    Dlg.Title := Title;
    dir := ExtractFilePath(fn);
    if dir <> '' then
      Dlg.InitialDir := dir;
    Result := GetOpenSaveDialogExecute(Dlg);
    if Result then
      Files.Assign(Dlg.Files);
  finally
    FreeAndNil(Dlg);
  end;
end;

function ShowOpenDialog(const Title: string; const Extension: string; var Filename: string;
  const Filter: string = ''): boolean;
var
  Dlg: TOpenDialog;
  dir: string;
begin
  Dlg := TOpenDialog.Create(nil);
  try
    Dlg.FileName := Filename;
    Dlg.DefaultExt := Extension;
    if Filter = '' then
      Dlg.Filter := MakeDialogExtensionString(Extension)
    else
      Dlg.Filter := Format('%s|All Files (%s)|%s', [Filter, AllFilesWildCard, AllFilesWildCard]);
    Dlg.Options := Dlg.Options + [ofPathMustExist, ofFileMustExist];
    Dlg.Title := Title;
    dir := ExtractFilePath(Filename);
    if dir <> '' then
      Dlg.InitialDir := dir;
    Result := GetOpenSaveDialogExecute(Dlg);
    if Result then
      Filename := Dlg.FileName;
  finally
    FreeAndNil(Dlg);
  end;
end;

function ShowSaveDialog(const Title: string; const Extension: string; var Filename: string;
  const Filter: string = ''): boolean;
var
  Dlg: TSaveDialog;
  dir: string;
begin
  Dlg := TSaveDialog.Create(nil);
  try
    Dlg.FileName := Filename;
    Dlg.DefaultExt := Extension;
    if Filter = '' then
      Dlg.Filter := MakeDialogExtensionString(Extension)
    else
      Dlg.Filter := Format('%s|All Files (%s)|%s', [Filter, AllFilesWildCard, AllFilesWildCard]);
    Dlg.Options := Dlg.Options + [ofPathMustExist];
    Dlg.Title := Title;
    dir := ExtractFilePath(Filename);
    if dir <> '' then
      Dlg.InitialDir := dir;
    Result := GetOpenSaveDialogExecute(Dlg);
    if Result then
      Filename := Dlg.FileName;
  finally
    FreeAndNil(Dlg);
  end;
end;

function GetOpenSaveDialogExecute(Dialog: TOpenDialog): Boolean;
var
  CurrentIdeFolder: string;
begin
  CurrentIdeFolder := GetCurrentDir;
  try
    Result := Dialog.Execute;
  finally
    SetCurrentDir(CurrentIdeFolder);
  end;
end;

function MakeDialogExtensionString(const Ext: string): string;
begin
  Result := Format('%s Files (*.%s)|*.%s|All Files (%s)|%s', [AnsiUpperCase(Ext), Ext, Ext, AllFilesWildCard, AllFilesWildCard]);
end;

function WildcardCompare(const FileWildcard, FileName: string; const IgnoreCase: Boolean): Boolean;

  function WildCompare(var WildS, IstS: string): Boolean;
  var
    WildPos, FilePos, l, p: Integer;
  begin
    // Start at the first wildcard/filename character
    WildPos := 1;  // Wildcard position.
    FilePos := 1;  // FileName position.
    while (WildPos <= Length(WildS)) do
    begin
      // '*' matches any sequence of characters.
      if WildS[WildPos] = '*' then
      begin
        // We've reached the end of the wildcard string with a * and are done.
        if WildPos = Length(WildS) then
        begin
          Result := True;
          Exit;
        end
        else
        begin
          l := WildPos + 1;
          // Anything after a * in the wildcard must match literally.
          while (l < Length(WildS)) and (WildS[l + 1] <> '*') do
            Inc(l);
          // Check for the literal match immediately after the current position.
          p := Pos(Copy(WildS, WildPos + 1, l - WildPos), IstS);
          if p > 0 then
            FilePos := p - 1
          else
          begin
            Result := False;
            Exit;
          end;
        end;
      end
      // '?' matches any character - other characters must literally match.
      else if (WildS[WildPos] <> '?') and ((Length(IstS) < WildPos) or
         (WildS[WildPos] <> IstS[FilePos])) then
      begin
        Result := False;
        Exit;
      end;
      // Match is OK so far - check the next character.
      Inc(WildPos);
      Inc(FilePos);
    end;
    Result := (FilePos > Length(IstS));
  end;

var
  NameWild, NameFile, ExtWild, ExtFile: string;
  DotPos: Integer;
begin
  // Parse to find the extension and name base of filename and wildcard.
  DotPos := LastCharPos(FileWildcard, '.');
  if DotPos = 0 then
  begin
    // Assume .* if an extension is missing
    NameWild := FileWildcard;
    ExtWild := '*';
  end
  else
  begin
    NameWild := Copy(FileWildcard, 1, DotPos - 1);
    ExtWild := Copy(FileWildcard, DotPos + 1);
  end;
  // We could probably modify this to use ExtractFileExt, etc.
  DotPos := LastCharPos(FileName, '.');
  if DotPos = 0 then
    DotPos := Length(FileName) + 1;

  NameFile := Copy(FileName, 1, DotPos - 1);
  ExtFile := Copy(FileName, DotPos + 1);
  // Case insensitive check
  if IgnoreCase then
  begin
    NameWild := AnsiUpperCase(NameWild);
    NameFile := AnsiUpperCase(NameFile);
    ExtWild  := AnsiUpperCase(ExtWild);
    ExtFile  := AnsiUpperCase(ExtFile);
  end;
  // Both the extension and the filename must match
  Result := WildCompare(NameWild, NameFile) and WildCompare(ExtWild, ExtFile);
end;

function FileNameHasWildcards(const FileName: string): Boolean;
begin
  Result := (Pos('*', FileName) > 0) or (Pos('?', FileName) > 0);
end;

function GetSystemImageIndexForFile(const FileName: string): Integer;
var
  FileInfo: TSHFileInfo;
begin
  if SHGetFileInfo(PChar(FileName), 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SMALLICON or SHGFI_SYSICONINDEX) <> 0 then
  begin
    Result := FileInfo.IIcon;
  end
  else
    Result := 0;
end;

procedure TGXUnicodeStringList_MakeIndex(_sl: TGXUnicodeStringList);
var
  i: Integer;
begin
  Assert(Assigned(_sl));

  _sl.Sorted := False;
  for i := 0 to _sl.Count - 1 do
    _sl.Objects[i] := Pointer(i + 1);
  _sl.Sorted := True;
end;

procedure LoadFormFileToStrings(const FileName: string; Strings: TGXUnicodeStringList; out WasBinary: Boolean);
var
  Src: TStream;
  Dest: TStream;
  Format: TStreamOriginalFormat;
begin
  Assert(Assigned(Strings));
  Strings.Clear;

  Dest := nil;
  Src := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Dest := TMemoryStream.Create;
    Src.Position := 0;
    Format := TestStreamFormat(Src);
    Src.Position := 0;
    WasBinary := (Format = sofBinary);
    case Format of
      sofUnknown:
        raise Exception.CreateFmt('Invalid stream format for form file: %s.  (sofUnknown)', [FileName]);
      sofBinary:
        begin
          ObjectResourceToText(Src, Dest, Format);
          Dest.Position := 0;
          {$IFDEF GX_VER200_up}
          if Format = sofUTF8Text then
            Strings.LoadFromStream(Dest, TEncoding.UTF8)
          else
          {$ENDIF}
            Strings.LoadFromStream(Dest {$IFDEF GX_VER200_up}, TEncoding.Default{$ENDIF});
        end;
      sofText:
        Strings.LoadFromStream(Src);
      {$IFDEF GX_VER200_up}
      sofUTF8Text:
        Strings.LoadFromStream(Src, TEncoding.UTF8);
      {$ENDIF}
      else
        raise Exception.Create('Unknown form file format: ' + IntToStr(Ord(Format)));
    end;
  finally
    FreeAndNil(Src);
    FreeAndNil(Dest);
  end;
end;

procedure LoadFormFileToStrings(const FileName: string; Strings: TGXUnicodeStringList); overload;
var
  WasBinary: Boolean;
begin
  LoadFormFileToStrings(FileName, Strings, WasBinary);
end;

function IsBinaryForm(const FileName: string): Boolean;
var
  FormStream: TFileStream;
  Buf: array[0..2] of Byte;
begin
  Result := False;
  if FileExists(FileName) then
  begin
    FormStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      FormStream.Position := 0;
      FormStream.ReadBuffer(Buf, SizeOf(Buf));
      FormStream.Position := 0;
      Assert((Low(Buf) = 0) and (High(Buf) = 2));
      if (Buf[0] = $FF) and (Buf[1] = $0A) and (Buf[2] = $00) then
        Result := True;
    finally
      FreeAndNil(FormStream);
    end;
  end;
end;

procedure LoadDiskFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList; var WasBinary: Boolean);
begin
  if not FileExists(FileName) then
    raise EGXFileNotFound.CreateFmt('The file %s does not exist.', [FileName]);
  if IsForm(FileName) then
    LoadFormFileToStrings(FileName, Data, WasBinary)
  else begin
    // This handles ANSI, UTF-8, and UTF-16BE/LE (but not UTF-7 or UTF-32)
    Data.LoadFromFile(FileName);
    if Data.Count = 0 then begin
      // TStringList and TUnicodeStringList.LoadFromFile has issues with some UnicodeCharacters
      if GetFileSize(FileName) > 0 then begin
        raise Exception.CreateFmt('%s.LoadFromFile failed to read %s.', [Data.ClassParent.ClassName, FileName]);
      end;
    end;
  end;
end;

function TryFindPathToFile(const FileName: string; out FullFilename: string; Paths: TStrings): Boolean;

  function MakeFilename(const Path, FileName: string): string;
  begin
    if Path = '' then
      Result := FileName
    else if Path[Length(Path)] = PathDelim then
      Result := Path + FileName
    else
      Result := Path + PathDelim + FileName;
  end;

var
  i: Integer;
  NewFileName: string;
begin
  Assert(Assigned(Paths));

  if IsPathAbsolute(FileName) then begin
    FullFilename := FileName;
    Result := FileExists(FileName);
    Exit; //==>
  end;

  for i := 0 to Paths.Count - 1 do
  begin
    NewFileName := MakeFilename(Paths[i], FileName);
    if FileExists(NewFileName) then
    begin
      Result := True;
      FullFilename := NewFileName;
      Exit; //==>
    end;
  end;

  Result := False;
end;

function MangleFilename(const _fn: string): string;
var
  i: Integer;
begin
  Result := _fn;
  UniqueString(Result);
  while StartsStr('..\', Result) do
    Result := Copy(Result, 4);
  for i := 1 to Length(Result) - 1 do begin
    case Result[i] of
      ':': Result[i] := '@';
      '\': Result[i] := '@';
      '/': Result[i] := '@';
    end;
  end;
end;

function FileIsNewerThan(const _Fn1, _Fn2: string): Boolean;
var
  Age1: TDateTime;
  Age2: TDateTime;
begin
  Age1:=  GetFileDate(_Fn1);
  Age2 := GetFileDate(_Fn2);
  Result := (Age1 > Age2);
end;

function ThisDllName: string;
begin
  Result := u_dzOsUtils.GetModuleFileName(HINSTANCE);
end;

function VclInstance: LongWord;
begin
  Result := FindClassHInstance(TObject);
end;

procedure GxLogAndShowException(const E: Exception; const Msg: string);
begin
  Assert(Assigned(E));

  GxLogException(E, Msg);
  MessageDlg(Msg + E.Message, mtError, [mbOK], 0);
end;

procedure GxLogException(const E: Exception; const Msg: string);
begin //FI:W519
  {$IFDEF GX_DEBUGLOG}
  GxAddExceptionToLog(E, Msg);
  {$ENDIF GX_DEBUGLOG}
end;

function ShowError(const Error: string): Boolean;
begin
  Result := Trim(Error) <> '';
  if Result then
    MessageDlg(Error, mtError, [mbOK], 0);
end;

procedure GetEnvironmentVariables(Strings: TStrings);
var
  EnvStart: Pointer;
  EnvPos: PChar;
begin
  Assert(Assigned(Strings));
  Strings.Clear;
  EnvStart := GetEnvironmentStrings;
  try
    EnvPos := EnvStart;
    while StrLen(EnvPos) > 0 do
    begin
      Strings.Add(EnvPos);
      EnvPos := StrEnd(EnvPos) + 1;
    end;
  finally
    FreeEnvironmentStrings(EnvStart);
  end;
end;

procedure Initialize;
var
  i: AnsiChar;
begin
  for i := Low(AnsiChar) to High(AnsiChar) do
    if Windows.IsCharAlphaNumericA(i) then
      Include(LocaleIdentifierChars, i);
  Include(LocaleIdentifierChars, '_');
  MakeASCIICharTable;
end;

function IsStrCaseIns(const Str: string; Pos: Integer; const SubStr: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Pos + Length(SubStr) - 1 <= Length(Str) then
  begin
    for i := 1 to Length(SubStr) do
      if AnsiUpperCase(Str[Pos + i - 1]) <> AnsiUpperCase(SubStr[i]) then
        Exit;
  end
  else
    Exit;
  Result := True;
end;

function FindTextIdent(Id: string; const Source: string;
  LastPos: Integer; Prev: Boolean; var Pos: Integer): Boolean;
var
  StartPos: Integer;

  function GoNext: Boolean;
  begin
    if Prev then
      Dec(StartPos)
    else
      Inc(StartPos);
    Result := (StartPos >= 1) and (StartPos <= Length(Source));
  end;

var
  PrevChar: Char;
  NextChar: Char;
begin
  Result := False;
  if Id = '' then
    Exit;
  // todo: This is problematic since some locales do not have an upper case representation
  //       of special characters like the French accented characters. The accents will get lost
  //       in the conversion. It might find some occurrences which are not the same character.
  Id := AnsiUpperCase(Id);
  StartPos := LastPos;

  while GoNext do
    if AnsiUpperCase(Source[StartPos]) = Id[1] then
      if IsStrCaseIns(Source, StartPos, Id) then
      begin
        if (StartPos - 1) < 1 then
          PrevChar := ' '
        else
          PrevChar := Source[StartPos - 1];
        if (StartPos + Length(Id)) > Length(Source) then
          NextChar := ' '
        else
          NextChar := Source[StartPos + Length(Id)];

        if (not IsCharIdentifier(PrevChar)) and (not IsCharIdentifier(NextChar)) then
        begin
          Pos := StartPos;
          Result := True;
          Break;
        end;
      end;
end;

function LinePosToCharPos(LinePos: TPoint; const Text: string): Integer;
var
  Offset: Integer;
  LineIdx: Integer;
  sl: TStringList;
  LineLen: Integer;
begin
  Assert(LinePos.X > 0);
  Assert(LinePos.Y > 0);
  sl := TStringList.Create;
  try
    sl.Text := Text;
    Offset := 0;
    // add the line lengths up to the line previous to the current line (and adjust for LineNo vs. LineIdx)
    for LineIdx := 0 to LinePos.Y - 2 do begin
      LineLen := Length(sl[LineIdx]);
      Offset := Offset + LineLen + CRLFLength;
    end;
    Result := Offset + LinePos.X;
  finally
    FreeAndNil(sl);
  end;
end;

function CharPosToLinePos(CharPos: Integer; const Text: string): TPoint;
var
  sl: TStringList;
  LineIdx: Integer;
  Offset: Integer;
  LineLen: Integer;
  LeftLinePart: string;
  ColumnError: Integer;
begin
  Assert(CharPos > 0);

  sl := TStringList.Create;
  try
    sl.Text := Text;
    LineIdx := 0;
    Offset := 0;
    while LineIdx < sl.Count do begin
      LineLen := Length(sl[LineIdx]) + CRLFLength;
      if Offset + LineLen > CharPos then begin
        Result.Y := LineIdx + 1;
        Result.X := CharPos - Offset;
        LeftLinePart := Copy(sl[LineIdx], 1, Result.X - 1);
        ColumnError := Length(ConvertToIDEEditorString(LeftLinePart)) - Length(LeftLinePart);
        Result.X := Result.X + ColumnError;
        Exit; //==>
      end else begin
        Inc(LineIdx);
        Offset := Offset + LineLen;
      end;
    end;
  finally
    FreeAndNil(sl);
  end;
  Result := Point(-1, -1);
end;


function MessageName(Msg: Longint): string;
begin
  Result := 'Error';
  if Msg < 0 then
    Exit;
  if Msg >= $0400 then
    Result := 'WM_USER + $'+ IntToHex(Msg - 1024, 3)
  else
    case Msg of
      $0000: Result :=  'WM_NULL';
      $0001: Result :=  'WM_CREATE';
      $0002: Result :=  'WM_DESTROY';
      $0003: Result :=  'WM_MOVE';
      $0005: Result :=  'WM_SIZE';
      $0006: Result :=  'WM_ACTIVATE';
      $0007: Result :=  'WM_SETFOCUS';
      $0008: Result :=  'WM_KILLFOCUS';
      $000A: Result :=  'WM_ENABLE';
      $000B: Result :=  'WM_SETREDRAW';
      $000C: Result :=  'WM_SETTEXT';
      $000D: Result :=  'WM_GETTEXT';
      $000E: Result :=  'WM_GETTEXTLENGTH';
      $000F: Result :=  'WM_PAINT';
      $0010: Result :=  'WM_CLOSE';
      $0011: Result :=  'WM_QUERYENDSESSION';
      $0012: Result :=  'WM_QUIT';
      $0013: Result :=  'WM_QUERYOPEN';
      $0014: Result :=  'WM_ERASEBKGND';
      $0015: Result :=  'WM_SYSCOLORCHANGE';
      $0016: Result :=  'WM_ENDSESSION';
      $0017: Result :=  'WM_SYSTEMERROR';
      $0018: Result :=  'WM_SHOWWINDOW';
      $0019: Result :=  'WM_CTLCOLOR';
      $001A: Result :=  'WM_WININICHANGE';
      $001B: Result :=  'WM_DEVMODECHANGE';
      $001C: Result :=  'WM_ACTIVATEAPP';
      $001D: Result :=  'WM_FONTCHANGE';
      $001E: Result :=  'WM_TIMECHANGE';
      $001F: Result :=  'WM_CANCELMODE';
      $0020: Result :=  'WM_SETCURSOR';
      $0021: Result :=  'WM_MOUSEACTIVATE';
      $0022: Result :=  'WM_CHILDACTIVATE';
      $0023: Result :=  'WM_QUEUESYNC';
      $0024: Result :=  'WM_GETMINMAXINFO';
      $0026: Result :=  'WM_PAINTICON';
      $0027: Result :=  'WM_ICONERASEBKGND';
      $0028: Result :=  'WM_NEXTDLGCTL';
      $002A: Result :=  'WM_SPOOLERSTATUS';
      $002B: Result :=  'WM_DRAWITEM';
      $002C: Result :=  'WM_MEASUREITEM';
      $002D: Result :=  'WM_DELETEITEM';
      $002E: Result :=  'WM_VKEYTOITEM';
      $002F: Result :=  'WM_CHARTOITEM';
      $0030: Result :=  'WM_SETFONT';
      $0031: Result :=  'WM_GETFONT';
      $0032: Result :=  'WM_SETHOTKEY';
      $0033: Result :=  'WM_GETHOTKEY';
      $0037: Result :=  'WM_QUERYDRAGICON';
      $0039: Result :=  'WM_COMPAREITEM';
      $003D: Result :=  'WM_GETOBJECT';
      $0041: Result :=  'WM_COMPACTING';
      $0044: Result :=  'WM_COMMNOTIFY';
      $0046: Result :=  'WM_WINDOWPOSCHANGING';
      $0047: Result :=  'WM_WINDOWPOSCHANGED';
      $0048: Result :=  'WM_POWER';
      $004A: Result :=  'WM_COPYDATA';
      $004B: Result :=  'WM_CANCELJOURNAL';
      $004E: Result :=  'WM_NOTIFY';
      $0050: Result :=  'WM_INPUTLANGCHANGEREQUEST';
      $0051: Result :=  'WM_INPUTLANGCHANGE';
      $0052: Result :=  'WM_TCARD';
      $0053: Result :=  'WM_HELP';
      $0054: Result :=  'WM_USERCHANGED';
      $0055: Result :=  'WM_NOTIFYFORMAT';
      $007B: Result :=  'WM_CONTEXTMENU';
      $007C: Result :=  'WM_STYLECHANGING';
      $007D: Result :=  'WM_STYLECHANGED';
      $007E: Result :=  'WM_DISPLAYCHANGE';
      $007F: Result :=  'WM_GETICON';
      $0080: Result :=  'WM_SETICON';
      $0081: Result :=  'WM_NCCREATE';
      $0082: Result :=  'WM_NCDESTROY';
      $0083: Result :=  'WM_NCCALCSIZE';
      $0084: Result :=  'WM_NCHITTEST';
      $0085: Result :=  'WM_NCPAINT';
      $0086: Result :=  'WM_NCACTIVATE';
      $0087: Result :=  'WM_GETDLGCODE';
      $00A0: Result :=  'WM_NCMOUSEMOVE';
      $00A1: Result :=  'WM_NCLBUTTONDOWN';
      $00A2: Result :=  'WM_NCLBUTTONUP';
      $00A3: Result :=  'WM_NCLBUTTONDBLCLK';
      $00A4: Result :=  'WM_NCRBUTTONDOWN';
      $00A5: Result :=  'WM_NCRBUTTONUP';
      $00A6: Result :=  'WM_NCRBUTTONDBLCLK';
      $00A7: Result :=  'WM_NCMBUTTONDOWN';
      $00A8: Result :=  'WM_NCMBUTTONUP';
      $00A9: Result :=  'WM_NCMBUTTONDBLCLK';
      $0100: Result :=  'WM_KEYDOWN';
      $0101: Result :=  'WM_KEYUP';
      $0102: Result :=  'WM_CHAR';
      $0103: Result :=  'WM_DEADCHAR';
      $0104: Result :=  'WM_SYSKEYDOWN';
      $0105: Result :=  'WM_SYSKEYUP';
      $0106: Result :=  'WM_SYSCHAR';
      $0107: Result :=  'WM_SYSDEADCHAR';
      $0108: Result :=  'WM_KEYLAST';
      $0110: Result :=  'WM_INITDIALOG';
      $0111: Result :=  'WM_COMMAND';
      $0112: Result :=  'WM_SYSCOMMAND';
      $0113: Result :=  'WM_TIMER';
      $0114: Result :=  'WM_HSCROLL';
      $0115: Result :=  'WM_VSCROLL';
      $0116: Result :=  'WM_INITMENU';
      $0117: Result :=  'WM_INITMENUPOPUP';
      $011F: Result :=  'WM_MENUSELECT';
      $0120: Result :=  'WM_MENUCHAR';
      $0121: Result :=  'WM_ENTERIDLE';
      $0122: Result :=  'WM_MENURBUTTONUP';
      $0123: Result :=  'WM_MENUDRAG';
      $0124: Result :=  'WM_MENUGETOBJECT';
      $0125: Result :=  'WM_UNINITMENUPOPUP';
      $0126: Result :=  'WM_MENUCOMMAND';
      $0127: Result :=  'WM_CHANGEUISTATE';
      $0128: Result :=  'WM_UPDATEUISTATE';
      $0129: Result :=  'WM_QUERYUISTATE';
      $0132: Result :=  'WM_CTLCOLORMSGBOX';
      $0133: Result :=  'WM_CTLCOLOREDIT';
      $0134: Result :=  'WM_CTLCOLORLISTBOX';
      $0135: Result :=  'WM_CTLCOLORBTN';
      $0136: Result :=  'WM_CTLCOLORDLG';
      $0137: Result :=  'WM_CTLCOLORSCROLLBAR';
      $0138: Result :=  'WM_CTLCOLORSTATIC';
      $0200: Result :=  'WM_MOUSEMOVE';
      $0201: Result :=  'WM_LBUTTONDOWN';
      $0202: Result :=  'WM_LBUTTONUP';
      $0203: Result :=  'WM_LBUTTONDBLCLK';
      $0204: Result :=  'WM_RBUTTONDOWN';
      $0205: Result :=  'WM_RBUTTONUP';
      $0206: Result :=  'WM_RBUTTONDBLCLK';
      $0207: Result :=  'WM_MBUTTONDOWN';
      $0208: Result :=  'WM_MBUTTONUP';
      $0209: Result :=  'WM_MBUTTONDBLCLK';
      $020A: Result :=  'WM_MOUSEWHEEL';
      $0210: Result :=  'WM_PARENTNOTIFY';
      $0211: Result :=  'WM_ENTERMENULOOP';
      $0212: Result :=  'WM_EXITMENULOOP';
      $0213: Result :=  'WM_NEXTMENU';
      $0214: Result :=  'WM_SIZING';
      $0215: Result :=  'WM_CAPTURECHANGED';
      $0216: Result :=  'WM_MOVING';
      $0218: Result :=  'WM_POWERBROADCAST';
      $0219: Result :=  'WM_DEVICECHANGE';
      $010D: Result :=  'WM_IME_STARTCOMPOSITION';
      $010E: Result :=  'WM_IME_ENDCOMPOSITION';
      $010F: Result :=  'WM_IME_COMPOSITION';
      $0281: Result :=  'WM_IME_SETCONTEXT';
      $0282: Result :=  'WM_IME_NOTIFY';
      $0283: Result :=  'WM_IME_CONTROL';
      $0284: Result :=  'WM_IME_COMPOSITIONFULL';
      $0285: Result :=  'WM_IME_SELECT';
      $0286: Result :=  'WM_IME_CHAR';
      $0288: Result :=  'WM_IME_REQUEST';
      $0290: Result :=  'WM_IME_KEYDOWN';
      $0291: Result :=  'WM_IME_KEYUP';
      $0220: Result :=  'WM_MDICREATE';
      $0221: Result :=  'WM_MDIDESTROY';
      $0222: Result :=  'WM_MDIACTIVATE';
      $0223: Result :=  'WM_MDIRESTORE';
      $0224: Result :=  'WM_MDINEXT';
      $0225: Result :=  'WM_MDIMAXIMIZE';
      $0226: Result :=  'WM_MDITILE';
      $0227: Result :=  'WM_MDICASCADE';
      $0228: Result :=  'WM_MDIICONARRANGE';
      $0229: Result :=  'WM_MDIGETACTIVE';
      $0230: Result :=  'WM_MDISETMENU';
      $0231: Result :=  'WM_ENTERSIZEMOVE';
      $0232: Result :=  'WM_EXITSIZEMOVE';
      $0233: Result :=  'WM_DROPFILES';
      $0234: Result :=  'WM_MDIREFRESHMENU';
      $02A1: Result :=  'WM_MOUSEHOVER';
      $02A3: Result :=  'WM_MOUSELEAVE';
      $0300: Result :=  'WM_CUT';
      $0301: Result :=  'WM_COPY';
      $0302: Result :=  'WM_PASTE';
      $0303: Result :=  'WM_CLEAR';
      $0304: Result :=  'WM_UNDO';
      $0305: Result :=  'WM_RENDERFORMAT';
      $0306: Result :=  'WM_RENDERALLFORMATS';
      $0307: Result :=  'WM_DESTROYCLIPBOARD';
      $0308: Result :=  'WM_DRAWCLIPBOARD';
      $0309: Result :=  'WM_PAINTCLIPBOARD';
      $030A: Result :=  'WM_VSCROLLCLIPBOARD';
      $030B: Result :=  'WM_SIZECLIPBOARD';
      $030C: Result :=  'WM_ASKCBFORMATNAME';
      $030D: Result :=  'WM_CHANGECBCHAIN';
      $030E: Result :=  'WM_HSCROLLCLIPBOARD';
      $030F: Result :=  'WM_QUERYNEWPALETTE';
      $0310: Result :=  'WM_PALETTEISCHANGING';
      $0311: Result :=  'WM_PALETTECHANGED';
      $0312: Result :=  'WM_HOTKEY';
      $0317: Result :=  'WM_PRINT';
      $0318: Result :=  'WM_PRINTCLIENT';
      $0358: Result :=  'WM_HANDHELDFIRST';
      $035F: Result :=  'WM_HANDHELDLAST';
      $0380: Result :=  'WM_PENWINFIRST';
      $038F: Result :=  'WM_PENWINLAST';
      $0390: Result :=  'WM_COALESCE_FIRST';
      $039F: Result :=  'WM_COALESCE_LAST';
      $03E0: Result :=  'WM_DDE_INITIATE';
      $03E1: Result :=  'WM_DDE_TERMINATE';
      $03E2: Result :=  'WM_DDE_ADVISE';
      $03E3: Result :=  'WM_DDE_UNADVISE';
      $03E4: Result :=  'WM_DDE_ACK';
      $03E5: Result :=  'WM_DDE_DATA';
      $03E6: Result :=  'WM_DDE_REQUEST';
      $03E7: Result :=  'WM_DDE_POKE';
      $03E8: Result :=  'WM_DDE_EXECUTE';
      $03E9: Result :=  'WM_DDE_LAST';
      else
        Result := 'Unknown';
    end; // of case
end;

function IsoDateTimeToStr(DateTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', DateTime);
end;

function IsoStringToDateTime(const ISODateTime: string): TDateTime;
const
  ISOShortLen = 19;
  ISOFullLen = 23;
var
  y, m, d, h, n, s, z: Word;
begin
  // ISODateTime should be in one of these formats:
  // YYYY-MM-DDTHH:NN:SS, YYYY-MM-DD HH:NN:SS
  // YYYY-MM-DDTHH:NN:SS.ZZZ, YYYY-MM-DD HH:NN:SS.ZZZ
  if (Length(ISODateTime) <> ISOShortLen) and (Length(ISODateTime) <> ISOFullLen) then
    raise EConvertError.Create('Invalid ISO date time string: ' + ISODateTime);
  y := SysUtils.StrToInt(Copy(ISODateTime,  1, 4));
  m := SysUtils.StrToInt(Copy(ISODateTime,  6, 2));
  d := SysUtils.StrToInt(Copy(ISODateTime,  9, 2));
  h := SysUtils.StrToInt(Copy(ISODateTime, 12, 2));
  n := SysUtils.StrToInt(Copy(ISODateTime, 15, 2));
  s := SysUtils.StrToInt(Copy(ISODateTime, 18, 2));
  z := StrToIntDef(Copy(ISODateTime, 21, 3),  0); // Optional
  Result := EncodeDate(y, m, d) + EncodeTime(h, n, s, z);
end;

function IsoStringToDateTimeDef(const DateTime: string; Default: TDateTime): TDateTime;
begin
  Result := Default;
  try
    Result := IsoStringToDateTime(DateTime);
  except
  end; //FI:W501
end;

function GetFirstCharPos(const AText: String; AChars: TSysCharSet; SearchThis: Boolean): Integer;
begin
  for Result := 1 to Length(AText) do
    if CharInSet(AText[Result], AChars) = SearchThis then
      Exit;
  Result := 0;
end;

function GetLastCharPos(const AText: String; AChars: TSysCharSet; SearchThis: Boolean): Integer;
begin
  for Result := Length(AText) downto 1 do
    if CharInSet(AText[Result], AChars) = SearchThis then
      Exit;
  Result := 0;
end;

{$IFNDEF GX_VER160_up}
// Delphi 6/7: IDE is non-unicode, string is ansistring

// no conversion necessary
function ConvertToIDEEditorString(const S: string): string;
begin
  Result := S;
end;

// no conversion necessary
function IDEEditorStringToString(const S: string): string;
begin
  Result := S;
end;

// converting AnsiString to WideString is done automatically on assignment
function UTF8ToUnicodeString(S: UTF8String): WideString;
begin
  Result := Utf8ToAnsi(S);
end;

{$ELSE GX_VER160_up}
// Delphi 8+
{$IFNDEF GX_VER200_up}
// Delphi (8/)2005/2006/2007: IDE is unicode (UTF-8), string is ansistring (GX_VER160_up}

function ConvertToIDEEditorString(const S: string): UTF8String;
begin
  Result := AnsiToUtf8(S);
end;

function IDEEditorStringToString(const S: UTF8String): string;
begin
  Result := Utf8ToAnsi(S);
end;

function UTF8ToUnicodeString(S: UTF8String): WideString;
begin
  Result := UTF8Decode(S);
end;

{$ELSE GX_VER200_up}
// Delphi 2009+: IDE is unicode (UTF-8), string is unicodestring {GX_VER200_up}

function ConvertToIDEEditorString(const S: string): UTF8String;
begin
  Result := UTF8Encode(s);
end;

function IDEEditorStringToString(const S: string): string;
begin
  Result := S;
end;

function IDEEditorStringToString(const S: IDEEditBufferString): string; overload;
begin
  Result := UTF8ToString(S);
end;

function UTF8ToUnicodeString(S: UTF8String): WideString;
begin
  Result := UTF8ToWideString(S);
end;
{$ENDIF GX_VER200_up}
{$ENDIF GX_VER160_up}

procedure CopyFileToClipboard(const _Filename: string);
var
  DropFiles: PDropFiles;
  hGlobal: THandle;
  ThisLen: Integer;
begin
  if _Filename = '' then
    raise Exception.Create('Filename parameter must not be empty');

  if not FileExists(_Filename) then
    raise Exception.Create('File does not exist: ' + _Filename);

  ThisLen := Length(_Filename);
  hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT,
    SizeOf(TDropFiles) + ((ThisLen + 2) * SizeOf(Char)));
  if hGlobal = 0 then
    RaiseLastOsError;

  try
    DropFiles := GlobalLock(hGlobal);
    if DropFiles = nil then
      RaiseLastOsError;
    try
      DropFiles^.pFiles := SizeOf(TDropFiles);
      DropFiles^.fWide := (SizeOf(Char) = SizeOf(WideChar));

      Move(_Filename[1], PByte(Cardinal(DropFiles) + SizeOf(TDropFiles))^, ThisLen * SizeOf(Char));
    finally
      GlobalUnlock(hGlobal);
    end;
    Clipboard.SetAsHandle(CF_HDROP, hGlobal);
  except
    GlobalFree(hGlobal);
  end;
end;

procedure AddDelphiDirsToIgnore(_List: TGXUnicodeStringList);
begin
  _List.Add('__history');
  _List.Add('__recovery');
end;

procedure AddDelphiDirsToIgnore(_List: TStringList);
begin
  _List.Add('__history');
  _List.Add('__recovery');
end;

procedure AddSCMDirsToIgnore(_List: TGXUnicodeStringList);
begin
  _List.Add('.svn');
  _List.Add('.hg');
  _List.Add('.git');
end;

procedure AddSCMDirsToIgnore(_List: TStringList);
begin
  _List.Add('.svn');
  _List.Add('.hg');
  _List.Add('.git');
end;

{ TFileFindThread }

procedure TFileFindThread.AddDelphiDirsToIgnore;
begin
  GX_GenericUtils.AddDelphiDirsToIgnore(FDirsToIgnore);
end;

procedure TFileFindThread.AddResult(const FileName: string);
begin
  LockResults;
  try
    FResults.Add(FileName);
  finally
    ReleaseResults;
  end;
end;

procedure TFileFindThread.AddSCMDirsToIgnore;
begin
  GX_GenericUtils.AddSCMDirsToIgnore(FDirsToIgnore);
end;

constructor TFileFindThread.Create;
begin
  inherited Create(True);
  FFileMasks := TStringList.Create;
  FResults := TStringList.Create;
  FResultsLock := TCriticalSection.Create;
  FSearchDirs := TStringList.Create;
  FRecursiveSearchDirs := TStringList.Create;
  FResults.Duplicates := dupIgnore;
  FResults.Sorted := True;
  FDirsToIgnore := TStringList.Create;
  FDirsToIgnore.Duplicates := dupIgnore;
  FDirsToIgnore.Sorted := True;
  FDirsToIgnore.Add('.');
  FDirsToIgnore.Add('..');
end;

destructor TFileFindThread.Destroy;
begin
  inherited;
  FreeAndNil(FDirsToIgnore);
  FreeAndNil(FFileMasks);
  FreeAndNil(FResults);
  FreeAndNil(FResultsLock);
  FreeAndNil(FSearchDirs);
  FreeAndNil(FRecursiveSearchDirs);
end;

procedure TFileFindThread.doExecute;
var
  i: Integer;
begin
  inherited;

  try
    LockResults;
    try
      FResults.Clear;
    finally
      ReleaseResults;
    end;
    for i := 0 to FSearchDirs.Count - 1 do
    begin
      FSearchDirs[i] := AddSlash(FSearchDirs[i]);
      FindFilesInDir(FSearchDirs[i], False);
      if Terminated then
        Exit;
    end;
    if Terminated then
      Exit;
    for i := 0 to FRecursiveSearchDirs.Count - 1 do
    begin
      FRecursiveSearchDirs[i] := AddSlash(FRecursiveSearchDirs[i]);
      FindFilesInDir(FRecursiveSearchDirs[i], True);
      if Terminated then
        Exit;
    end;
    if Terminated then
      Exit;
    if Assigned(FFindComplete) then
      Synchronize(FFindComplete);
  except
    on E: Exception do
      MessageBox(0, PChar(E.Message), 'File Search Thread', MB_OK + MB_ICONERROR + MB_APPLMODAL);
  end;
end;

procedure TFileFindThread.FindFilesInDir(const Dir: string; Recursive: Boolean);
var
  SearchRec: TSearchRec;
  i: Integer;
  Idx: Integer;
begin
  if Recursive then
  begin
    if FindFirst(Dir + AllFilesWildCard, faAnyFile, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Attr and faDirectory) <> 0 then
        begin
          if not FDirsToIgnore.Find(SearchRec.Name, Idx) then
            FindFilesInDir(AddSlash(BuildFileName(Dir, SearchRec.Name)), Recursive);
        end;
      until (FindNext(SearchRec) <> 0) or Terminated;
    finally
      SysUtils.FindClose(SearchRec);
    end;
  end;

  if Terminated then
    Exit;

  Assert(Assigned(FFileMasks));
  for i := 0 to FFileMasks.Count - 1 do
  begin
    if FindFirst(Dir + FileMasks[i], faAnyFile, SearchRec) = 0 then
    try
      repeat
        if DirectoriesOnly then
        begin
          if ((SearchRec.Attr and faDirectory) <> 0) and not FDirsToIgnore.Find(SearchRec.Name, Idx) then
            AddResult(BuildFileName(Dir, SearchRec.Name));
        end
        else
          AddResult(BuildFileName(Dir, SearchRec.Name));
      until (FindNext(SearchRec) <> 0) or Terminated;
    finally
      SysUtils.FindClose(SearchRec);
    end;
  end;
end;

procedure TFileFindThread.LockResults;
begin
  FResultsLock.Acquire;
end;

procedure TFileFindThread.ReleaseResults;
begin
  FResultsLock.Release;
end;

procedure TFileFindThread.SetDirsToIgnore(const Value: TStringList);
begin
  FDirsToIgnore.Assign(Value);
  FDirsToIgnore.Add('.');
  FDirsToIgnore.Add('..');
end;

procedure TFileFindThread.StartFind;
begin
  {$IFDEF GX_VER210_up}
  Start;
  {$ELSE}
  Resume;
  {$ENDIF}
end;

// Initialization variables
var
  OldError: Longint;
  FLibHandle: THandle;
  Ver: TOsVersionInfo;

initialization
{$IFDEF GX_DELPHI_SYDNEY_UP}
  // Delphi 10.4.2 calls CoCreateInstance(CLSID_WICImagingFactory, ...) in the
  // initialization of Vcl.WinXCtrls (through several intermediate calls)
  // This fails if CoInitialize has not been called, which apparently the VCL doesn't do,
  // so we call it here which will hopefully be executed before Vcl.WinXCtrls
  // for details see: https://en.delphipraxis.net/topic/4585-runtime-error-217-when-installing-gexperts/
  // This is actually only necessary to support installing GExperts via
  // rundll32.exe GExpertsRS104.dll,InstallGExperts
  // Lets hope that there will be no side effects for normal operation.
  CoInitialize(nil);
{$ENDIF}

  Initialize;

  // Get the RichEditVersion.  Code modified from JvRichEd.
  RichEditVersion := 1;
  OldError := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
    FLibHandle := LoadLibrary(RichEdit20ModuleName);
    if (FLibHandle > 0) and (FLibHandle < HINSTANCE_ERROR) then FLibHandle := 0;
    if FLibHandle = 0 then begin
      FLibHandle := LoadLibrary(RichEdit10ModuleName);
      if (FLibHandle > 0) and (FLibHandle < HINSTANCE_ERROR) then FLibHandle := 0;
    end
    else begin
      RichEditVersion := 2;
      Ver.dwOSVersionInfoSize := SizeOf(Ver);
      GetVersionEx(Ver);
      if (Ver.dwPlatformId = VER_PLATFORM_WIN32_NT) and
        (Ver.dwMajorVersion >= 5) then
        RichEditVersion := 3;
    end;
  finally
    SetErrorMode(OldError);
  end;

finalization
  if FLibHandle <> 0 then FreeLibrary(FLibHandle);

end.
