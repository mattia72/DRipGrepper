unit RipGrepper.Common.IOTAUtils;
{$IFNDEF STANDALONE}

interface

uses
	Vcl.Menus,
	ToolsAPI,
	System.Classes,
	Vcl.Controls,
	Vcl.Forms,
	RipGrepper.Helper.Types,
	Vcl.Graphics,
	ArrayEx,
	System.Win.Registry,
	Winapi.Windows,
	RipGrepper.Common.IOTAUtils.Constants,
	RipGrepper.Common.RegistryUtils;

type

	IOTAUtils = class(TObject)
		const
			// 'DCC_UnitSearchPath'
			OPTION_NAME_UNIT_SEARCH_PATH = 'SrcDir';

		private
			class procedure addProjectDefineMacros(var _defineValue : string; _macros : TStrings);
			// Returns IDE's base registry key, for instance
			// Software\Borland\Delphi\4.0
			// The returned string is guaranteed to NOT have a
			// backslash appended and it does NOT have a leading
			// backslash either (Windows 2000 is allergic to that).
			class function GxOtaGetIdeBaseRegistryKey() : string;
			/// <summary>
			/// Returns the project's current platform, if any (and supported), or an empty string </summary>
			class function GxOtaGetProjectPlatform(Project : IOTAProject = nil) : string;
			class procedure processPaths(var _paths : TArrayEx<string>; var _nonExistsPaths : TArrayEx<string>;
				const _prefix, _platformName : string);
			/// <summary>
			/// Shows a message box with non-existing paths to inform the user
			/// @param NonExistsPaths list of paths that do not exist </summary>
			class procedure showNonExistingPathsMessage(NonExistsPaths : TStrings);
			/// <summary>
			/// Get all available _macros that can be used in ReplaceMacros including Platform, Config,
			/// environment variables, IDE base paths, and preprocessor constants (defines)
			/// @param _macros will contain all available _macros in "Name=Value" format
			/// @param _project is the _project to get options from, if nil the current _project will be used </summary>
			class procedure getAllAvailableMacros(_macros : TStrings; _project : IOTAProject = nil);
			/// <summary>
			/// Get all preprocessor constants (conditional _defines) for the given _project
			/// @param _defines will contain all conditional _defines in "Name=Value" format
			/// @param _project is the _project to get _defines from, if nil the current _project will be used </summary>
			class procedure getPreprocessorConstants(_defines : TStrings; _project : IOTAProject = nil);

		public
			class function AddToImageList(_bmp : Vcl.Graphics.TBitmap; const _identText : string) : Integer;
			// Returns the size of any EOL character(s) at a given position (0 if none)
			class function EOLSizeAtPos(const S : string; Pos : Integer) : Integer;
			class function FileMatchesExtension(const FileName, FileExtension : string) : Boolean;
			class function FileMatchesExtensions(const FileName : string; FileExtensions : array of string) : Boolean; overload;
			class function GetSettingFilePath : string;
			class function FindMenuItem(const name : string) : TMenuItem;
			class function FindMenu(const name : string) : TMenu;
			class function FindInMainMenu(const name : string) : TMenuItem;
			class function FindInMenu(_menu : TMenuItem; const _menuName : string) : TMenuItem;
			class function GetEditPosition : IOTAEditPosition;
			// Get the actual TEditControl embedded in the given IDE editor form
			class function GetIDEEditControl(Form : TCustomForm) : TWinControl;

			class function GetOpenedEditorFiles : TArray<string>;
			class function GetOpenedEditBuffers : TArray<string>;
			class function GetModifiedEditBuffers : TArray<string>;

			class function GetProjectFiles : TArray<string>;
			class function GetActiveProjectFilePath() : string;
			class function GetActiveProjectDirectory() : string;
			// Raise an exception if the source editor is readonly
			class procedure GxOtaAssertSourceEditorNotReadOnly(SourceEditor : IOTASourceEditor);

			class function GxOtaConvertColumnCharsToBytes(LineData : UTF8String; CharIndex : Integer; EndByte : Boolean) : Integer;
			class function GxOtaCurrentProjectIsDelphiDotNet() : Boolean;
			class function GxOtaCurrentProjectIsNativeCpp() : Boolean;
			// Determine is a file exists or the file's module is currently loaded in the IDE
			class function GxOtaFileOrModuleExists(const AFileName : string; UseBase : Boolean = False) : Boolean;
			class function GxOtaFocusCurrentIDEEditControl : Boolean;
			// If UseSelection is True, get the selected text in the current edit view
			// (if any) and return True or get all of the editor's text (if no selection)
			// and return False
			class function GxOtaGetActiveEditorText(Lines : TStringList; UseSelection : Boolean = True) : Boolean;
			// Returns the editor's active text as a string, similar to above
			class function GxOtaGetActiveEditorTextAsString(var Text : string; UseSelection : Boolean = True) : Boolean;
			// Returns the editor's active text as a string, similar to above
			class function GxOtaGetActiveEditorTextAsMultilineString(var Text : TMultiLineString; UseSelection : Boolean = True) : Boolean;
			class function GxOtaGetActiveEditorTextAsUnicodeString(var Text : string; UseSelection : Boolean = True) : Boolean;
			// Transform dfm, hpp, etc. references to the base .pas/cpp file name
			class function GxOtaGetBaseModuleFileName(const FileName : string) : string;
			// Obtain and focus the IDE's internal edit control wrapper
			class function GxOtaGetCurrentIDEEditControl : TWinControl;
			// Returns the current module; may return nil of there is no
			// current module.
			class function GxOtaGetCurrentModule : IOTAModule;
			// Returns reference to currently active project;
			// returns nil if there is no (active) project.
			class function GxOtaGetCurrentProject : IOTAProject;
			/// <summary>
			/// Get the file name of the current project
			/// @param NormalizeBdsProj = True means to get the the dpr instead of the bdsproj/dproj for Delphi projects
			/// @returns the full file name of the project or an empty string if there is no current project </summary>
			class function GxOtaGetCurrentProjectFileName(NormalizeBdsProj : Boolean = False) : string;
			/// <summary>
			/// Returns the name of the currently active project; returns
			/// an empty string if there is no (active) project.
			/// The name of a project is the project's file name without
			/// a file extension and without path information. </summary>
			class function GxOtaGetCurrentProjectName : string;
			// Get the selected text in the top edit view, if any
			class function GxOtaGetCurrentSelection(IncludeTrailingCRLF : Boolean = True) : string;
			// Returns the current IOTASourceEditor or nil if
			// there is no current source editor.
			// Note: This will return a source editor even if the currently active editor
			// is a form editor! This is probably not what you want.
			// See also GxOtaGetCurrentEditorAsSourceEditor
			class function GxOtaGetCurrentSourceEditor : IOTASourceEditor;
			// Returns a fully qualified name of the current file,
			// which could either be a form or unit (.pas/.cpp/.dfm/.xfm etc.).
			// Returns an empty string if no file is currently selected.
			class function GxOtaGetCurrentSourceFile : string;
			// Obtain the IOTAEditActions interface for a given module
			class function GxOtaGetEditActionsFromModule(Module : IOTAModule) : IOTAEditActions;
			class function GxOtaGetEditorLine(View : IOTAEditView; LineNo : Integer) : UTF8String;
			class function GxOtaGetEditorServices : IOTAEditorServices;
			// Get an edit writer for the current source editor (raise an exception if not availble)
			// Use the current source editor if none is specified
			class function GxOtaGetEditWriterForSourceEditor(SourceEditor : IOTASourceEditor = nil) : IOTAEditWriter;
			/// <summary>
			/// Return the effective library path, with the _project specific paths
			/// first and then the IDE's global library path.
			/// @params if _shouldDoProcessing is true, the paths are macro expanded and non-existing
			/// paths removed.
			/// @params AdditionalPaths are appended at the end and optionally processed too </summary>
			class function GxOtaGetEffectiveLibraryPath(_project : IOTAProject; var _errList : TArrayEx<string>;
				const _shouldDoProcessing : Boolean = True) : TArrayEx<string>;
			// Get the environment options interface.  Succeeds or raises an exception.
			class function GxOtaGetEnvironmentOptions() : IOTAEnvironmentOptions;
			// Get the Index-th IOTAEditor for the given module
			// Works around a BCB 5 bug with simple units
			class function GxOtaGetFileEditorForModule(Module : IOTAModule; index : Integer) : IOTAEditor;
			// Returns a form editor for Module if it exists; nil otherwise.
			// Module can be nil, if it is , this function will return nil
			class function GxOtaGetFormEditorFromModule(const Module : IOTAModule) : IOTAFormEditor;
			// Return the IDE's environment string value that is
			// associated with EnvironmentStringName
			class function GxOtaGetIdeEnvironmentString(const EnvironmentStringName : string) : string;
			// Return all of the IDE's environment settings names
			class procedure GxOtaGetIdeEnvironmentStrings(Settings : TStrings);
			// Return the IDE's global library path
			class function GxOtaGetIdeLibraryPath() : string;
			/// <summary>
			/// Return the global IDE library path (without the project specific paths).
			/// @params if _shouldDoProcessing is true, the paths are macro expanded and non-existing
			/// paths removed. </summary>
			class function GxOtaGetIdeLibraryPathStrings(var _errList : TArrayEx<string>; _shouldDoProcessing : Boolean = true)
				: TArrayEx<string>;
			// Returns a module interface for a given filename
			// May return nil if no such module is open.
			class function GxOtaGetModule(const FileName : string) : IOTAModule;
			// Get the number of open modules
			class function GxOtaGetOpenModuleCount : Integer;
			/// <summary>
			/// Get the file name of the given project
			/// @parram Project is the project whose file name is requested
			/// @param NormalizeBdsProj = True means to get the the dpr instead of the bdsproj/dproj for Delphi projects
			/// @returns an empty string if Project is nil, otherwise the full file name of the project </summary>
			class function GxOtaGetProjectFileName(Project : IOTAProject; NormalizeBdsProj : Boolean = False) : string;
			// Returns reference to the IDE's project group;
			// returns Nil if there is no project group.
			class function GxOtaGetProjectGroup : IOTAProjectGroup;
			// Get the personality identifier string for the project, or blank for none
			class function GxOtaGetProjectPersonality(Project : IOTAProject) : string;
			/// <summary>
			/// Returns the project's current platform, if any (and supported), or an empty string </summary>
			class function GxOtaGetProjectPlatform1(Project : IOTAProject = nil) : string;
			/// <summary>
			/// Return _project specific search path, with the directory containing the _project
			// file first.
			/// @params if _shouldDoProcessing is true, the paths are macro expanded and non-existing
			/// paths removed. </summary>
			class function GxOtaGetProjectSourcePathStrings(_project : IOTAProject; _errList : TArrayEx<string>;
				_shouldDoProcessing : Boolean = True) : TArrayEx<string>;
			// Returns the IOTASourceEditor interface for a module
			// if there is a file that supports one; returns nil
			// if there is no IOTASourceEditor
			class function GxOtaGetSourceEditorFromModule(Module : IOTAModule; const FileName : string = '') : IOTASourceEditor;
			class function GxOtaGetTopMostEditBuffer : IOTAEditBuffer;
			class function GxOtaGetTopMostEditView(SourceEditor : IOTASourceEditor) : IOTAEditView; overload;
			// Return the top-most edit view/buffer (globally or for a given source editor)
			// Returns nil if none exists.
			class function GxOtaGetTopMostEditView : IOTAEditView; overload;
			class procedure GxOtaGoToFileLineColumn(const FileName : string; Line : Integer; StartColumn : Integer = 0;
				StopColumn : Integer = 0; ShowInMiddle : Boolean = True);
			// Returns True if _sFilePath is an open file in the IDE;
			// _bUseBase determines whether things like .dfm/.hpp map
			// to their parent file type.  Otherwise, they will not be located.
			class function IsFileOpen(const _sFilePath : string; const _bUseBase : Boolean = False) : Boolean;
			class procedure GxOtaLoadSourceEditorToUnicodeStrings(_editor : IOTASourceEditor; _content : TStringList);
			// Make sure the given filename's source is visible.
			// This swaps source/form views as necessary
			class function GxOtaMakeSourceVisible(const FileName : string) : Boolean;
			class function GxOtaModuleIsShowingFormSource(Module : IOTAModule) : Boolean;
			// Open FileName in IDE; returns True on success, False otherwise.
			class function GxOtaOpenFile(const FileName : string) : Boolean;
			class function GxOtaProjectIsDelphiDotNet(Project : IOTAProject) : Boolean;
			class function GxOtaProjectIsEitherDelphi(Project : IOTAProject) : Boolean;
			class function GxOtaProjectIsNativeCpp(Project : IOTAProject) : Boolean;
			// Save an edit reader to a stream
			class procedure GxOtaSaveReaderToStream(EditReader : IOTAEditReader; Stream : TStream; TrailingNull : Boolean = True);
			class function GxOtaTryGetCurrentProject(out _Project : IOTAProject) : Boolean;
			class function GxOtaTryGetCurrentSourceEditor(out SourceEditor : IOTASourceEditor) : boolean;
			/// <summary>
			/// Tries to get the options of the given or the active project
			/// @param ProjectOptions will contain the options, only valid if Result is True
			/// @param Project is the project whose options to get, it nil the current project will be used
			/// @returns True, if the options could be retrieved, False otherwise (e.g. if there is no active project) </summary>
			class function GxOtaTryGetProjectOptions(out _ProjectOptions : IOTAProjectOptions; _Project : IOTAProject = nil) : Boolean;
			// calls GxOtaGetTopMostEditView and returns false if no edit view is available
			class function GxOtaTryGetTopMostEditView(out EditView : IOTAEditView) : boolean; overload;
			class function IDEEditorStringToString(const S : string) : string; overload;
			class function IsBdsproj(const FileName : string) : Boolean;
			class function IsBdsprojOrDproj(const FileName : string) : Boolean;
			class function IsBpr(const FileName : string) : Boolean;
			class function IsCharLineEnding(C : Char) : Boolean;
			class function IsDpr(const FileName : string) : Boolean;
			class function IsDproj(const FileName : string) : Boolean;
			class function IsForm(const FileName : string) : Boolean;
			class function IsPackage(const FileName : string) : Boolean;
			class function IsProjectSource(const FileName : string) : Boolean;
			class function IsStandAlone : Boolean;
			// Remove the last EOL character from a string
			// EOL can be one or two characters long
			// Useful for processing strings read from memo controls
			class procedure RemoveLastEOL(var S : string);
			// Get character at some position or #0 for invalid positions
			class function StrCharAt(const S : string; Pos : Integer) : Char;
			class function TryFocusControl(Control : TWinControl) : Boolean;
	end;

	TIdeUtils = class
		public
			// Return the IDE's root directory (the installation directory).
			// Returns an empty string if the information could not be retrieved.
			class function GetIdeRootDirectory() : string;
	end;

implementation

uses
	System.IOUtils,
	System.SysUtils,
	System.StrUtils,
	RipGrepper.Common.Constants,

	RipGrepper.Tools.DebugUtils,
	RipGrepper.Tools.FileUtils,
	u_dzClassUtils,
	System.Variants,
	System.Math;

type
	TPathProcessor = class
		private
			FIdeBasePath : string;
			FConfigName : string;
			FPlatformName : string;
			FPrefix : string;
			FEnvironment : TStringList;
			FProjectOptions : IOTAProjectOptions;
			procedure Init(_Project : IOTAProject);
			class function ReplaceMacro(const _str, _oldValue, _newValue : string) : string;

		public
			/// <summary>
			/// @param _Prefix will be used for relative paths
			/// @param _Project will be used to determine Platform and Config, if not given, the
			/// current project will be used. </summary>
			constructor Create(const _Prefix : string; _Project : IOTAProject = nil);
			destructor Destroy; override;
			procedure GetEnvironmentVariables(Strings : TStrings);
			procedure GetAllProjectOptions(Options : TStrings);
			function GetProjectOption(const OptionName : string) : string;
			function Process(const _Path : string) : string;
			property PlatformName : string read FPlatformName write FPlatformName;
			property ConfigName : string read FConfigName write FConfigName;
	end;

class function IOTAUtils.AddToImageList(_bmp : Vcl.Graphics.TBitmap; const _identText : string) : Integer;
var
	Services : INTAServices;
begin
	Supports(BorlandIDEServices, INTAServices, Services);
	Result := Services.AddMasked(_bmp, _bmp.TransparentColor, _identText);
end;

// Returns the size of any EOL characters at a given position
// Supports the following EOL formats:
// #10#13 (?)
// #13#10 (PC / Win)
// #10    (Unix)
// #13    (Macintosh, Amiga)
// Note: Pos must be at the beginning of the EOL characters
class function IOTAUtils.EOLSizeAtPos(const S : string; Pos : Integer) : Integer;
begin
	if IsCharLineEnding(StrCharAt(S, Pos)) then begin
		Result := 1;
		if (IsCharLineEnding(StrCharAt(S, Pos + 1)) and (StrCharAt(S, Pos) <> StrCharAt(S, Pos + 1))) then
			Inc(Result);
	end
	else
		Result := 0;
end;

class function IOTAUtils.FileMatchesExtension(const FileName, FileExtension : string) : Boolean;
begin
	Result := FileMatchesExtensions(FileName, [FileExtension]);
end;

class function IOTAUtils.FileMatchesExtensions(const FileName : string; FileExtensions : array of string) : Boolean;
begin
	Result := MatchStr(ExtractFileExt(FileName), FileExtensions);
end;

class function IOTAUtils.FindMenuItem(const Name : string) : TMenuItem;
var
	Comp : TComponent;
begin
	Comp := Application.MainForm.FindComponent(name);
	if (Comp <> nil) and (Comp is TMenuItem) then
		Result := TMenuItem(Comp)
	else
		Result := nil;
end;

class function IOTAUtils.FindMenu(const Name : string) : TMenu;
var
	Comp : TComponent;
begin
	Comp := Application.MainForm.FindComponent(name);
	if (Comp <> nil) and (Comp is TMenu) then
		Result := TMenu(Comp)
	else
		Result := nil;
end;

class function IOTAUtils.FindInMainMenu(const Name : string) : TMenuItem;
begin
	Result := nil;
	var
	mainMenu := (BorlandIDEServices as INTAServices).MainMenu;
	for var i := 0 to mainMenu.Items.Count - 1 do begin
		var
		m := mainMenu.Items[i];
		TDebugUtils.DebugMessage('IOTAUtils.FindInMainMenu - ' + m.Name);
		if CompareText(m.Name, name) = 0 then begin
			Result := m;
			break;
		end;
	end;
end;

class function IOTAUtils.FindInMenu(_menu : TMenuItem; const _menuName : string) : TMenuItem;
begin
	Result := nil;
	for var i := 0 to _menu.Count - 1 do begin
		var
		m := _menu.Items[i];
		TDebugUtils.DebugMessage('IOTAUtils.FindInMenu - check ' + m.Name);
		if CompareText(m.Name, _menuName) = 0 then begin
			TDebugUtils.DebugMessage('IOTAUtils.FindInMenu - found ' + m.Name);
			Result := m;
			break;
		end;
	end;
end;

class function IOTAUtils.GetEditPosition : IOTAEditPosition;
var
	aEditorServices : IOTAEditorServices;
	aEditBuffer : IOTAEditBuffer;
	aEditBlock : IOTAEditBlock;
begin
	Result := nil;
	aEditorServices := BorlandIDEServices as IOTAEditorServices;
	if Assigned(aEditorServices) and Assigned(aEditorServices.TopView) then begin
		aEditBlock := aEditorServices.TopView.GetBlock;
		aEditBuffer := aEditorServices.TopView.GetBuffer;
		if Assigned(aEditBlock) and Assigned(aEditBuffer) then begin
			Result := aEditBuffer.EditPosition;
		end;

	end;
end;

class function IOTAUtils.GetIDEEditControl(Form : TCustomForm) : TWinControl;
const
	SEditorControlName = 'Editor';
var
	Component : TComponent;
begin
	Assert(Assigned(Form));
	Result := nil;
	Component := (Form.FindComponent(SEditorControlName) as TWinControl);
	if Assigned(Component) then
		if Component is TWinControl then
			Result := Component as TWinControl;
end;

class function IOTAUtils.GetOpenedEditorFiles : TArray<string>;
var
	module : IOTAModule;
	editor : IOTAEditor;
	service : IOTAModuleServices;
begin
	Result := [];
	service := (BorlandIDEServices as IOTAModuleServices);
	if Assigned(service) then begin
		for var i := 0 to service.ModuleCount - 1 do begin
			module := service.Modules[i];
			for var j := 0 to module.GetModuleFileCount - 1 do begin
				editor := module.GetModuleFileEditor(j);
				TDebugUtils.DebugMessage('IOTAUtils.GetOpenedEditorFiles FileName=' + editor.FileName);
				Result := Result + [editor.FileName];
			end;
		end;
	end;
end;

class function IOTAUtils.GetOpenedEditBuffers : TArray<string>;
var
	service : IOTAEditorServices;
	it : IOTAEditBufferIterator;
	buffer : IOTAEditBuffer;
begin
	Result := [];
	service := (BorlandIDEServices as IOTAEditorServices);
	if Assigned(service) then begin
		if (service.GetEditBufferIterator(it)) then begin
			for var i := 0 to it.Count - 1 do begin
				buffer := it.EditBuffers[i];
				TDebugUtils.DebugMessage('IOTAUtils.GetOpenedEditBuffers FileName=' + buffer.FileName + ' ViewCount=' +
					buffer.EditViewCount.ToString);
				if buffer.EditViewCount > 0 then begin
					Result := Result + [buffer.FileName];
				end;
			end;
		end;
	end;
end;

class function IOTAUtils.GetModifiedEditBuffers : TArray<string>;
var
	service : IOTAEditorServices;
	it : IOTAEditBufferIterator;
	buffer : IOTAEditBuffer;
begin
	Result := [];
	service := (BorlandIDEServices as IOTAEditorServices);
	if Assigned(service) then begin
		if (service.GetEditBufferIterator(it)) then begin
			for var i := 0 to it.Count - 1 do begin
				buffer := it.EditBuffers[i];
				TDebugUtils.DebugMessage('IOTAUtils.GetModifiedEditBuffers FileName=' + buffer.FileName + ' ViewCount=' +
					buffer.EditViewCount.ToString);
				if ((buffer.EditViewCount > 0) and buffer.IsModified) then begin
					Result := Result + [buffer.FileName];
				end;
			end;
		end;
	end;
end;

class function IOTAUtils.GetProjectFiles : TArray<string>;
var
	fn : string;
	project : IOTAProject;
begin
	Result := [];
	project := GxOtaGetCurrentProject;
	if not Assigned(project) then
		Exit;
	for var i : integer := 0 to project.GetModuleCount - 1 do begin
		fn := project.GetModule(i).GetFileName;
		if not fn.IsEmpty then begin
			TDebugUtils.DebugMessage('IOTAUtils.GetProjectFiles FileName=' + fn);
			Result := Result + [fn]
		end;
	end;
end;

class function IOTAUtils.GetActiveProjectFilePath() : string;
var
	project : IOTAProject;
begin
	Result := '';
	project := GxOtaGetCurrentProject;
	if Assigned(project) then begin
		Result := project.FileName;
	end;
end;

class function IOTAUtils.GetActiveProjectDirectory() : string;
begin
	Result := ExtractFileDir(GetActiveProjectFilePath());
end;

class function IOTAUtils.GetSettingFilePath : string;
var
	aIDEServices : IOTAServices;
begin
	aIDEServices := BorlandIDEServices as IOTAServices;
	Result := aIDEServices.GetLocalApplicationDataDirectory;
	if Result = '' then
		raise Exception.Create('GetSettingsFilePath: path not defined by IDEServices');
end;

class procedure IOTAUtils.GxOtaAssertSourceEditorNotReadOnly(SourceEditor : IOTASourceEditor);
begin
	Assert(Assigned(SourceEditor));
	if Supports(SourceEditor, IOTAEditBuffer) then
		if (SourceEditor as IOTAEditBuffer).IsReadOnly then
			raise Exception.CreateFmt('%s is read only', [ExtractFileName(SourceEditor.FileName)]);
end;

class function IOTAUtils.GxOtaConvertColumnCharsToBytes(LineData : UTF8String; CharIndex : Integer; EndByte : Boolean) : Integer;
var
	UString : string;
	FinalUChar : string;
	UTF8Str : UTF8String;
begin
	UString := UTF8ToUnicodeString(LineData);
	UString := Copy(UString, 1, CharIndex);
	UTF8Str := UTF8String(UTF8Encode(UString));
	Result := Length(UTF8Str);
	if not EndByte then begin
		if Length(UString) = 0 then
			Result := 0
		else begin
			FinalUChar := UString[Length(UString)];
			UTF8Str := Utf8String(UTF8Encode(FinalUChar));
			Result := Result - (Length(UTF8Str)) + 1;
		end;
	end;
end;

class function IOTAUtils.GxOtaCurrentProjectIsDelphiDotNet() : Boolean;
begin
	Result := GxOtaProjectIsDelphiDotNet(GxOtaGetCurrentProject);
end;

class function IOTAUtils.GxOtaCurrentProjectIsNativeCpp() : Boolean;
begin
	Result := GxOtaProjectIsNativeCpp(GxOtaGetCurrentProject);
end;

class function IOTAUtils.GxOtaFileOrModuleExists(const AFileName : string; UseBase : Boolean = False) : Boolean;
begin
	Result := FileExists(AFileName) or IsFileOpen(AFileName, UseBase);
end;

class function IOTAUtils.GxOtaFocusCurrentIDEEditControl : Boolean;
var
	EditControl : TWinControl;
begin
	Result := False;
	EditControl := GxOtaGetCurrentIDEEditControl;
	TryFocusControl(EditControl)
end;

class function IOTAUtils.GxOtaGetActiveEditorText(Lines : TStringList; UseSelection : Boolean = True) : Boolean;
var
	ISourceEditor : IOTASourceEditor;
	IEditView : IOTAEditView;
begin
	Assert(Assigned(Lines));
	Lines.Clear;
	Result := False;

	if not GxOtaTryGetCurrentSourceEditor(ISourceEditor) then
		Exit;

	if ISourceEditor.EditViewCount > 0 then begin
		IEditView := GxOtaGetTopMostEditView(ISourceEditor);

		if UseSelection and Assigned(IEditView) and Assigned(IEditView.Block) and (IEditView.Block.Size > 0) then begin
			Lines.Text := GxOtaGetCurrentSelection(False);
		end else begin
			// GxOtaLoadSourceEditorToUnicodeStrings(ISourceEditor, Lines);  don't Read whole file
		end;
		Result := True;
	end;
end;

class function IOTAUtils.GxOtaGetActiveEditorTextAsString(var Text : string; UseSelection : Boolean = True) : Boolean;
var
	Lines : string;
begin
	Result := GxOtaGetActiveEditorTextAsUnicodeString(Lines, UseSelection);
	Text := Lines;
end;

class function IOTAUtils.GxOtaGetActiveEditorTextAsMultilineString(var Text : TMultiLineString; UseSelection : Boolean = True) : Boolean;
var
	Lines : string;
begin
	Result := GxOtaGetActiveEditorTextAsUnicodeString(Lines, UseSelection);
	Text := Lines;
end;

class function IOTAUtils.GxOtaGetActiveEditorTextAsUnicodeString(var Text : string; UseSelection : Boolean = True) : Boolean;
var
	Lines : TStringList;
begin
	Lines := TStringList.Create;
	try
		Result := GxOtaGetActiveEditorText(Lines, UseSelection);
		Text := Lines.Text;
	finally
		FreeAndNil(Lines);
	end;
end;

class function IOTAUtils.GxOtaGetBaseModuleFileName(const FileName : string) : string;
var
	AltName : string;
begin
	Result := FileName;
	if IsForm(FileName) then begin
		// if GxOtaHaveCPPSupport then
		// begin
		// AltName := ChangeFileExt(FileName, '.cpp');
		// if GxOtaFileOrModuleExists(AltName) then
		// Result := AltName;
		// end;
		AltName := ChangeFileExt(FileName, '.pas');
		if GxOtaFileOrModuleExists(AltName) then
			Result := AltName;
	end;
end;

class function IOTAUtils.GxOtaGetCurrentIDEEditControl : TWinControl;
var
	EditView : IOTAEditView;
	EditWindow : INTAEditWindow;
	EditForm : TCustomForm;
begin
	Result := nil;
	EditView := GxOtaGetTopMostEditView;
	if Assigned(EditView) then begin
		EditWindow := EditView.GetEditWindow;
		if Assigned(EditWindow) then begin
			EditForm := EditWindow.Form;
			if Assigned(EditForm) then
				Result := GetIDEEditControl(EditForm);
		end;
	end;
end;

class function IOTAUtils.GxOtaGetCurrentModule : IOTAModule;
var
	ModuleServices : IOTAModuleServices;
begin
	ModuleServices := BorlandIDEServices as IOTAModuleServices;
	Assert(Assigned(ModuleServices));

	Result := ModuleServices.CurrentModule;
end;

class function IOTAUtils.GxOtaGetCurrentProject : IOTAProject;
begin
	if not GxOtaTryGetCurrentProject(Result) then
		Result := nil;
end;

class function IOTAUtils.GxOtaGetCurrentProjectFileName(NormalizeBdsProj : Boolean = False) : string;
begin
	Result := GxOtaGetProjectFileName(GxOtaGetCurrentProject, NormalizeBdsProj);
end;

class function IOTAUtils.GxOtaGetCurrentProjectName : string;
var
	IProject : IOTAProject;
begin
	Result := '';

	IProject := GxOtaGetCurrentProject;
	if Assigned(IProject) then begin
		Result := ExtractFileName(IProject.FileName);
		Result := ChangeFileExt(Result, '');
	end;
end;

class function IOTAUtils.GxOtaGetCurrentSelection(IncludeTrailingCRLF : Boolean = True) : string;
var
	EditView : IOTAEditView;
	EditBlock : IOTAEditBlock;
begin
	Result := '';

	if not GxOtaTryGetTopMostEditView(EditView) then
		Exit;

	EditBlock := EditView.Block;
	if Assigned(EditBlock) then begin
		Result := IDEEditorStringToString(EditBlock.Text);
		// Result := HackBadIDEUTF8StringToString(Result);

		if not IncludeTrailingCRLF and (EditBlock.Style in [btNonInclusive]) then
			RemoveLastEOL(Result);
	end;
end;

class function IOTAUtils.GxOtaGetCurrentSourceEditor : IOTASourceEditor;
var
	EditBuffer : IOTAEditBuffer;
begin
	Result := nil;
	EditBuffer := GxOtaGetTopMostEditBuffer;
	if Assigned(EditBuffer) and (EditBuffer.FileName <> '') then
		Result := GxOtaGetSourceEditorFromModule(GxOtaGetCurrentModule, EditBuffer.FileName);
	if Result = nil then
		Result := GxOtaGetSourceEditorFromModule(GxOtaGetCurrentModule);
end;

class function IOTAUtils.GxOtaGetCurrentSourceFile : string;
var
	Module : IOTAModule;
	Editor : IOTAEditor;
begin
	Result := '';
	Module := GxOtaGetCurrentModule;
	if Module <> nil then begin
		Editor := Module.GetCurrentEditor;
		if Editor <> nil then
			Result := Editor.FileName
		else // C++Builder 6 returns nil for some old-style modules without DFMs
			Result := Module.FileName;
	end;
end;

class function IOTAUtils.GxOtaGetEditActionsFromModule(Module : IOTAModule) : IOTAEditActions;
var
	i : Integer;
	EditView : IOTAEditView;
	SourceEditor : IOTASourceEditor;
begin
	Result := nil;
	if Module = nil then
		Exit;
	SourceEditor := GxOtaGetSourceEditorFromModule(Module);
	if SourceEditor = nil then
		Exit;
	for i := 0 to SourceEditor.GetEditViewCount - 1 do begin
		EditView := SourceEditor.GetEditView(i);
		if Supports(EditView, IOTAEditActions, Result) then
			Exit;
	end;
	Result := nil;
end;

class function IOTAUtils.GxOtaGetEditorLine(View : IOTAEditView; LineNo : Integer) : UTF8String;
var
	Buffer : IOTAEditBuffer;
	LineStartByte : Integer;
	LineEndByte : Integer;
	Pos : TOTACharPos;
	LineData : AnsiString;
	Reader : IOTAEditReader;
	LineBytes : Integer;
begin
	Assert(Assigned(View));
	Result := '';
	Buffer := View.Buffer;
	if (LineNo > Buffer.GetLinesInBuffer) or (LineNo < 1) then
		Exit;
	Pos.CharIndex := 0;
	Pos.Line := LineNo;
	LineStartByte := View.CharPosToPos(Pos);
	Pos.CharIndex := smallint.MaxValue;
	LineEndByte := View.CharPosToPos(Pos);
	Reader := Buffer.CreateReader;
	Assert(Assigned(Reader));
	LineBytes := LineEndByte - LineStartByte;
	SetLength(LineData, LineBytes);
	Reader.GetText(LineStartByte, PAnsiChar(LineData), LineBytes);
	Result := UTF8String(LineData);
end;

// Delphi 2005 Update 1 does not support IOTAEditorServices!
// (but we no longer care)
// {$IFDEF VER170}
// type
// IOTAEditorServices = IOTAEditorServices70;
// {$ENDIF VER170}

class function IOTAUtils.GxOtaGetEditorServices : IOTAEditorServices;
begin
	Result := (BorlandIDEServices as IOTAEditorServices);
	Assert(Assigned(Result), 'BorlandIDEServices is not assigned');
end;

class function IOTAUtils.GxOtaGetEditWriterForSourceEditor(SourceEditor : IOTASourceEditor = nil) : IOTAEditWriter;
resourcestring
	SEditWriterNotAvail = 'Edit writer not available';
begin
	if not Assigned(SourceEditor) then
		SourceEditor := GxOtaGetCurrentSourceEditor;
	if Assigned(SourceEditor) then begin
		GxOtaAssertSourceEditorNotReadOnly(SourceEditor);
		Result := SourceEditor.CreateUndoableWriter;
	end;
	Assert(Assigned(Result), SEditWriterNotAvail);
end;

class function IOTAUtils.GxOtaGetEffectiveLibraryPath(_project : IOTAProject; var _errList : TArrayEx<string>;
	const _shouldDoProcessing : Boolean = True) : TArrayEx<string>;
var
	i : Integer;
	platformName : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('IOATAUtils.GxOtaGetEffectiveLibraryPath');
	// _shouldDoProcessing = True uses the _project file's directory as the base to expand relative paths
	Result := GxOtaGetProjectSourcePathStrings(_project, _errList, _shouldDoProcessing);
	dbgMsg.MsgFmt('Project source paths: %s', [string.Join(CRLF, Result.Items)]);

	var
	localList := GxOtaGetIdeLibraryPathStrings(_errList, _shouldDoProcessing);
	if _shouldDoProcessing then begin
		// do another processing, this time also expanding the Platform related macros
		platformName := GxOtaGetProjectPlatform(_project);
		dbgMsg.MsgFmt('platformName: %s', [platformName]);

		// There shouldn't be any more relatives paths left, so passing GetCurrentDir is probably fine
		processPaths(localList, _errList, GetCurrentDir, platformName);
	end;
	for i := 0 to localList.Count - 1 do begin
		Result.AddIfNotContains(localList[i]);
	end;
end;

class function IOTAUtils.GxOtaGetEnvironmentOptions() : IOTAEnvironmentOptions;
begin
	var
	services := BorlandIDEServices as IOTAServices;
	Result := services.GetEnvironmentOptions;
	Assert(Assigned(Result));
end;

class function IOTAUtils.GxOtaGetFileEditorForModule(Module : IOTAModule; index : Integer) : IOTAEditor;
begin
	Assert(Assigned(Module));
	Result := Module.GetModuleFileEditor(index);
end;

class function IOTAUtils.GxOtaGetFormEditorFromModule(const Module : IOTAModule) : IOTAFormEditor;
var
	i : Integer;
	Editor : IOTAEditor;
	FormEditor : IOTAFormEditor;
begin
	Result := nil;
	if not Assigned(Module) then
		Exit;
	for i := 0 to Module.GetModuleFileCount - 1 do begin
		Editor := GxOtaGetFileEditorForModule(Module, i);
		if Supports(Editor, IOTAFormEditor, FormEditor) then begin
			Assert(not Assigned(Result), 'There should never be more than one form for a module');
			Result := FormEditor;
			// In order to assert our assumptions that only one form
			// is ever associated with a module, do not call Break; here.
		end;
	end;
end;

class function IOTAUtils.GxOtaGetIdeBaseRegistryKey() : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('IOTAUtils.GxOtaGetIdeBaseRegistryKey');

	if IsStandAlone then
		Result := 'Software\' + CompilerDefinedProductRegistryKey
	else
		Result := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey;

	if Length(Result) > 0 then begin
		// Windows 2000 is allergic to a leading backslash
		// in the registry key - NT4, for instance, is not.
		if Result[1] = '\' then
			Delete(Result, 1, 1);

		Assert(Result[Length(Result)] <> '\');
	end;
	dbgMsg.MsgFmt('Result: %s', [Result]);
end;

class function IOTAUtils.GxOtaGetIdeEnvironmentString(const EnvironmentStringName : string) : string;
begin
	Result := GxOtaGetEnvironmentOptions.Values[EnvironmentStringName];
end;

class procedure IOTAUtils.GxOtaGetIdeEnvironmentStrings(Settings : TStrings);
var
	EnvOptions : IOTAEnvironmentOptions;
	i : Integer;
	Options : TOTAOptionNameArray;
begin
	EnvOptions := GxOtaGetEnvironmentOptions;

	Settings.Clear;
	Options := EnvOptions.GetOptionNames; // Broken in Delphi 2005
	for i := 0 to Length(Options) - 1 do
		Settings.Add(Options[i].Name);
end;

class function IOTAUtils.GxOtaGetIdeLibraryPath() : string;
begin
	// Do not localize.
	var
	bRunningDelphi11OrGreater := {$IF CompilerVersion >= 35} TRUE {$ELSE} FALSE
	{$ENDIF};
	if bRunningDelphi11OrGreater and GxOtaCurrentProjectIsDelphiDotNet then
		Result := GxOtaGetIdeEnvironmentString('DotNetLibraryPath')
	else if bRunningDelphi11OrGreater and GxOtaCurrentProjectIsNativeCpp then
		Result := GxOtaGetIdeEnvironmentString('CppSearchPath')
	else
		Result := GxOtaGetIdeEnvironmentString('LibraryPath');
end;

class function IOTAUtils.GxOtaGetIdeLibraryPathStrings(var _errList : TArrayEx<string>; _shouldDoProcessing : Boolean = true)
	: TArrayEx<string>;
var
	idePathString : string;
begin
	idePathString := GxOtaGetIdeLibraryPath;
	Result := idePathString.Split([';']);
	if _shouldDoProcessing then begin
		// todo: Is it correct to use GetCurrentDir here? Shouldn't that either be the
		// IDE base path or the project's directory?
		processPaths(Result, _errList, GetCurrentDir, '');
	end;
end;

class function IOTAUtils.GxOtaGetModule(const FileName : string) : IOTAModule;
var
	ModuleServices : IOTAModuleServices;
begin
	ModuleServices := BorlandIDEServices as IOTAModuleServices;
	Assert(Assigned(ModuleServices));

	Result := ModuleServices.FindModule(FileName);
end;

class function IOTAUtils.GxOtaGetOpenModuleCount : Integer;
var ModuleServices : IOTAModuleServices;
begin
	ModuleServices := BorlandIDEServices as IOTAModuleServices;
	Assert(Assigned(ModuleServices));
	Result := ModuleServices.ModuleCount;
end;

class function IOTAUtils.GxOtaGetProjectFileName(Project : IOTAProject; NormalizeBdsProj : Boolean = False) : string;

	function SearchProjectSourceViaModule(var AProjectFileName : string) : Boolean;
	var i : Integer; Module : IOTAModule;
		Editor : IOTAEditor;
	begin
		Result := False;
		Module := Project as IOTAModule;
		for i := 0 to Module.ModuleFileCount - 1 do begin
			Editor := Module.ModuleFileEditors[i];
			if IsProjectSource(Editor.FileName) then begin
				Result := True;
				AProjectFileName := Editor.FileName;
				Exit;
			end;
		end;
	end;

	function SearchProjectSourceViaFileExt(var AProjectFileName : string) : Boolean;
	var PackageFileName : string;
	begin
		Result := GxOtaProjectIsEitherDelphi(Project);
		if Result then begin
			AProjectFileName := ChangeFileExt(AProjectFileName, '.dpr');
			if not GxOtaFileOrModuleExists(AProjectFileName) then begin
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

class function IOTAUtils.GxOtaGetProjectGroup : IOTAProjectGroup;
var IModuleServices : IOTAModuleServices; IModule : IOTAModule;
	i : Integer;
begin
	Assert(Assigned(BorlandIDEServices));

	IModuleServices := BorlandIDEServices as IOTAModuleServices;
	Assert(Assigned(IModuleServices));

	Result := nil;
	for i := 0 to IModuleServices.ModuleCount - 1 do begin
		IModule := IModuleServices.Modules[i];
		if Supports(IModule, IOTAProjectGroup, Result) then
			Break;
	end;
end;

class function IOTAUtils.GxOtaGetProjectPersonality(Project : IOTAProject) : string;
begin
	Result := '';
	if Assigned(Project) then begin
		Result := Project.Personality;
	end;
end;

class function IOTAUtils.GxOtaGetProjectPlatform(Project : IOTAProject = nil) : string;
begin
	Result := '';
	if Project = nil then
		Project := GxOtaGetCurrentProject;
	if Project <> nil then
		Result := Project.CurrentPlatform;
end;

class function IOTAUtils.GxOtaGetProjectPlatform1(Project : IOTAProject = nil) : string;
begin
	Result := '';
	{$IFDEF GX_VER230_up} // RAD Studio XE 2 (16; BDS 9)
	if Project = nil then
		Project := GxOtaGetCurrentProject;
	if Project <> nil then
		Result := Project.CurrentPlatform;
	{$ENDIF GX_VER230_up}
end;

class function IOTAUtils.GxOtaGetProjectSourcePathStrings(_project : IOTAProject; _errList : TArrayEx<string>;
	_shouldDoProcessing : Boolean = True) : TArrayEx<string>;
var idePathString : string; projectOptions : IOTAProjectOptions;
	projectDir : string; platformName : string;
begin
	if _project = nil then
		_project := GxOtaGetCurrentProject;
	if Assigned(_project) then begin
		projectDir := ExtractFileDir(_project.FileName);
		// Add the current _project directory first
		Result.Add(projectDir);
		// Then the _project search path
		if GxOtaTryGetProjectOptions(projectOptions, _project) then begin
			idePathString := projectOptions.Values[OPTION_NAME_UNIT_SEARCH_PATH];
			Result.AddRange(idePathString.Split([';']));
		end;
		if _shouldDoProcessing then begin
			platformName := GxOtaGetProjectPlatform(_project);
			processPaths(Result, _errList, projectDir, platformName);
		end;
	end;
end;

class function IOTAUtils.GxOtaGetSourceEditorFromModule(Module : IOTAModule; const FileName : string = '') : IOTASourceEditor;
var i : Integer; IEditor : IOTAEditor; ISourceEditor : IOTASourceEditor;
begin
	Result := nil;
	if not Assigned(Module) then
		Exit;

	for i := 0 to Module.GetModuleFileCount - 1 do begin
		IEditor := GxOtaGetFileEditorForModule(Module, i);

		if Supports(IEditor, IOTASourceEditor, ISourceEditor) then begin
			if Assigned(ISourceEditor) then begin
				if (FileName = '') or SameFileName(ISourceEditor.FileName, FileName) then begin
					Result := ISourceEditor;
					Break;
				end;
			end;
		end;
	end;
end;

class function IOTAUtils.GxOtaGetTopMostEditBuffer : IOTAEditBuffer;
begin
	Result := GxOTAGetEditorServices.TopBuffer;
end;

class function IOTAUtils.GxOtaGetTopMostEditView(SourceEditor : IOTASourceEditor) : IOTAEditView;
begin
	if SourceEditor = nil then
		SourceEditor := GxOtaGetCurrentSourceEditor;
	if Assigned(SourceEditor) and (SourceEditor.EditViewCount > 0) then
		Result := SourceEditor.EditViews[0]
	else
		Result := nil;
end;

class function IOTAUtils.GxOtaGetTopMostEditView : IOTAEditView;
begin
	Result := nil;
	// Bug: Delphi 5/6 crash when calling TopView with no files open
	if GxOtaGetOpenModuleCount = 0 then
		Exit;
	Result := GxOTAGetEditorServices.TopView;
end;

class procedure IOTAUtils.GxOtaGoToFileLineColumn(const FileName : string; Line : Integer; StartColumn : Integer = 0;
	StopColumn : Integer = 0; ShowInMiddle : Boolean = True);
var EditView : IOTAEditView; Module : IOTAModule;
	SourceEditor : IOTASourceEditor; CurPos : TOTAEditPos; CharPos : TOTACharPos; EditPos : TOTAEditPos; MatchLength : Integer;
	LineData : UTF8String;
resourcestring SCouldNotOpenFile = 'Could not open file %s';
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

	LineData := GxOtaGetEditorLine(EditView, Line);
	StartColumn := GxOtaConvertColumnCharsToBytes(LineData, StartColumn, False);
	StopColumn := GxOtaConvertColumnCharsToBytes(LineData, StopColumn, True);

	// Position the cursor to the line and column of the match
	CharPos.CharIndex := StartColumn - 1;
	CharPos.Line := Line;
	EditView.ConvertPos(False, EditPos, CharPos);
	EditView.CursorPos := EditPos;
	// This is disabled since it causes crashes in D2007 jumping to matches in data modules with no opened project
	// inside EdScript.TOTAEditView.unElideNearestBlock
	// GxOtaUnfoldNearestRegion(EditView);
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

class function IOTAUtils.IsFileOpen(const _sFilePath : string; const _bUseBase : Boolean = False) : Boolean;
var ModuleServices : IOTAModuleServices; Module : IOTAModule; FileEditor : IOTAEditor; i : Integer; FileName : string;
begin
	Result := False;

	if IsStandAlone then
		Exit;

	ModuleServices := BorlandIDEServices as IOTAModuleServices;
	Assert(Assigned(ModuleServices));
	FileName := _sFilePath;
	if _bUseBase then
		FileName := GxOtaGetBaseModuleFileName(FileName);

	Module := ModuleServices.FindModule(FileName);
	if Assigned(Module) then begin
		for i := 0 to Module.GetModuleFileCount - 1 do begin
			FileEditor := GxOtaGetFileEditorForModule(Module, i);
			Assert(Assigned(FileEditor));

			Result := SameFileName(FileName, FileEditor.FileName);
			if Result then
				Exit;
		end;
	end;
end;

class procedure IOTAUtils.GxOtaLoadSourceEditorToUnicodeStrings(_editor : IOTASourceEditor; _content : TStringList);
var MemStream : TMemoryStream;
begin
	_content.Clear;
	if not Assigned(_editor) then
		raise Exception.Create('No source editor in GxOtaLoadSourceEditorToUnicodeStrings');
	// TODO: Check stream format for forms as text (Ansi with escaped unicode, or UTF-8) in Delphi 2007/2009
	MemStream := TMemoryStream.Create;
	try
		GxOtaSaveReaderToStream(_editor.CreateReader, MemStream, False);
		MemStream.Position := 0;
		{$IFDEF UNICODE}
		_content.LoadFromStream(MemStream, TEncoding.UTF8);
		// For some Unicode characters (e.g. $E59C and $E280 SynUnicode.LoadFromStream works,
		// but TStringList.LoadFromStream doesn't.
		// Depending on the RTL version the latter fails silently and returns an empty string list.
		// So we check here for that condition and raise an error
		if (_content.Count = 0) and (MemStream.Size > 0) then
			raise Exception.CreateFmt('%s.LoadFromFile failed to read %s.', [_content.ClassParent.ClassName, _editor.FileName]);
		{$ELSE}
		SynUnicode.LoadFromStream(_content, MemStream, seUTF8);
		{$ENDIF}
	finally
		FreeAndNil(MemStream);
	end;
end;

class function IOTAUtils.GxOtaMakeSourceVisible(const FileName : string) : Boolean;
var EditActions : IOTAEditActions;
	Module : IOTAModule; FormEditor : IOTAFormEditor; SourceEditor : IOTASourceEditor; FileEditor : IOTAEditor; i : Integer;
	BaseFileName : string;
begin
	BaseFileName := GxOtaGetBaseModuleFileName(FileName);
	Module := GxOtaGetModule(BaseFileName);
	if Module = nil then
		Module := GxOtaGetModule(FileName);
	if Module = nil then
		Module := GxOtaGetModule(ChangeFileExt(FileName, '.dfm'));

	if Module <> nil then begin
		if IsForm(FileName) then begin
			if not GxOtaModuleIsShowingFormSource(Module) then begin
				SourceEditor := GxOtaGetSourceEditorFromModule(Module, BaseFileName);
				if Assigned(SourceEditor) then
					SourceEditor.Show;
				SourceEditor := nil;
				EditActions := GxOtaGetEditActionsFromModule(Module);
				if EditActions <> nil then begin
					FormEditor := GxOtaGetFormEditorFromModule(Module);
					FormEditor.Show;
					EditActions.SwapSourceFormView;
				end;
			end
		end
		else // We are focusing a regular text file, not a form
		begin
			if GxOtaModuleIsShowingFormSource(Module) then begin
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

	if not(IsFileOpen(BaseFileName) or IsFileOpen(FileName)) then
		Result := GxOtaOpenFile(FileName)
	else
		Result := True;

	if Result then begin
		{$IFDEF VER160}
		// Delphi 8 can not open both the module and the form text, so stop here
		if IsForm(FileName) then
			Exit;
		{$ENDIF VER160}
		Module := GxOtaGetModule(BaseFileName);
		if Module = nil then
			Module := GxOtaGetModule(FileName);
		if Module <> nil then begin
			for i := 0 to Module.GetModuleFileCount - 1 do begin
				FileEditor := Module.GetModuleFileEditor(i);
				Assert(Assigned(FileEditor));

				if SameFileName(FileEditor.FileName, FileName) then begin
					FileEditor.Show;
					Exit;
				end;
			end;
		end;
		Result := False;
	end;
end;

class function IOTAUtils.GxOtaModuleIsShowingFormSource(Module : IOTAModule) : Boolean;
var Editor : IOTAEditor; i : Integer;
begin
	Result := False;
	if not Assigned(Module) then
		Exit;
	for i := 0 to Module.GetModuleFileCount - 1 do begin
		Editor := Module.ModuleFileEditors[i];
		if Assigned(Editor) and IsForm(Editor.FileName) and Supports(Editor, IOTASourceEditor) then begin
			Result := True;
			Break;
		end;
	end;
end;

class function IOTAUtils.GxOtaOpenFile(const FileName : string) : Boolean;
var ActionServices : IOTAActionServices; hWndSaved : HWND;
begin
	ActionServices := BorlandIDEServices as IOTAActionServices;
	Assert(Assigned(ActionServices));

	hWndSaved := GetForegroundWindow;
	Result := ActionServices.OpenFile(FileName);
	if hWndSaved <> 0 then
		SetForegroundWindow(hWndSaved);
end;

class function IOTAUtils.GxOtaProjectIsDelphiDotNet(Project : IOTAProject) : Boolean;
begin
	Result := SameText(GxOtaGetProjectPersonality(Project), sDelphiDotNetPersonality);
end;

class function IOTAUtils.GxOtaProjectIsEitherDelphi(Project : IOTAProject) : Boolean;
begin
	Result := MatchStr(GxOtaGetProjectPersonality(Project), [sDelphiPersonality, sDelphiDotNetPersonality]);
end;

class function IOTAUtils.GxOtaProjectIsNativeCpp(Project : IOTAProject) : Boolean;
begin
	Result := SameText(GxOtaGetProjectPersonality(Project), sCBuilderPersonality);
end;

class procedure IOTAUtils.GxOtaSaveReaderToStream(EditReader : IOTAEditReader; Stream : TStream; TrailingNull : Boolean = True);
const
	// Leave typed constant as is - needed for streaming code.
	NULL_CHAR : AnsiChar = #0; BUFFER_SIZE = 1024 * 24;
var EditReaderPos : Integer; ReadDataSize : Integer;
	Buffer : array [0 .. BUFFER_SIZE] of AnsiChar; // Array of bytes, might be UTF-8
begin
	Assert(EditReader <> nil);
	Assert(Stream <> nil);

	EditReaderPos := 0;
	ReadDataSize := EditReader.GetText(EditReaderPos, @Buffer[0], BUFFER_SIZE);
	Inc(EditReaderPos, ReadDataSize);
	while ReadDataSize = BUFFER_SIZE do begin
		Stream.Write(Buffer, ReadDataSize);
		ReadDataSize := EditReader.GetText(EditReaderPos, Buffer, BUFFER_SIZE);
		Inc(EditReaderPos, ReadDataSize);
	end;
	Stream.Write(Buffer, ReadDataSize);
	if TrailingNull then
		Stream.Write(NULL_CHAR, SizeOf(NULL_CHAR)); // The source parsers need this
end;

class function IOTAUtils.GxOtaTryGetCurrentProject(out _Project : IOTAProject) : Boolean;
var IProjectGroup : IOTAProjectGroup;
	IModuleServices : IOTAModuleServices; IModule : IOTAModule; i : Integer;
begin
	Result := False;

	IProjectGroup := GxOtaGetProjectGroup;
	if not Assigned(IProjectGroup) then begin
		Assert(Assigned(BorlandIDEServices));
		IModuleServices := BorlandIDEServices as IOTAModuleServices;
		Assert(Assigned(IModuleServices));

		for i := 0 to IModuleServices.ModuleCount - 1 do begin
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
	end; // FI:W501 Empty except block
end;

class function IOTAUtils.GxOtaTryGetCurrentSourceEditor(out SourceEditor : IOTASourceEditor) : boolean;
begin
	SourceEditor := GxOtaGetCurrentSourceEditor;
	Result := Assigned(SourceEditor);
end;

class function IOTAUtils.GxOtaTryGetProjectOptions(out _ProjectOptions : IOTAProjectOptions; _Project : IOTAProject = nil) : Boolean;
begin
	if not Assigned(_Project) then
		_Project := GxOtaGetCurrentProject;
	if Assigned(_Project) then
		_ProjectOptions := _Project.GetProjectOptions;
	Result := Assigned(_ProjectOptions);
end;

class function IOTAUtils.GxOtaTryGetTopMostEditView(out EditView : IOTAEditView) : boolean;
begin
	EditView := GxOtaGetTopMostEditView;
	Result := Assigned(EditView);
end;

class function IOTAUtils.IDEEditorStringToString(const S : string) : string;
begin
	Result := S;
end;

class function IOTAUtils.IsBdsproj(const FileName : string) : Boolean;
begin
	Result := FileMatchesExtension(FileName, '.bdsproj');
end;

class function IOTAUtils.IsBdsprojOrDproj(const FileName : string) : Boolean;
begin
	Result := IsBdsproj(FileName) or IsDproj(FileName);
end;

class function IOTAUtils.IsBpr(const FileName : string) : Boolean;
begin
	Result := FileMatchesExtension(FileName, '.bpr');
end;

class function IOTAUtils.IsCharLineEnding(C : Char) : Boolean;
begin
	Result := CharInSet(C, [LF, CR]);
end;

class function IOTAUtils.IsDpr(const FileName : string) : Boolean;
begin
	Result := FileMatchesExtension(FileName, '.dpr');
end;

class function IOTAUtils.IsDproj(const FileName : string) : Boolean;
begin
	Result := FileMatchesExtension(FileName, '.dproj');
end;

class function IOTAUtils.IsForm(const FileName : string) : Boolean;
begin
	Result := FileMatchesExtensions(FileName, ['.dfm', '.xfm', '.nfm', '.fmx']);
end;

class function IOTAUtils.IsPackage(const FileName : string) : Boolean;
begin
	Result := FileMatchesExtensions(FileName, ['.dpk', '.dpkw', '.bpk']);
end;

class function IOTAUtils.IsProjectSource(const FileName : string) : Boolean;
begin
	Result := IsDpr(FileName) or IsBpr(FileName) or IsPackage(FileName);
end;

class function IOTAUtils.IsStandAlone : Boolean;
begin
	Result := (BorlandIDEServices = nil);
end;

class procedure IOTAUtils.processPaths(var _paths : TArrayEx<string>; var _nonExistsPaths : TArrayEx<string>;
	const _prefix, _platformName : string);
var
	i : Integer;
	pathItem : string;
	pathProcessor : TPathProcessor;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('IOTAUtils.processPaths');
	pathProcessor := TPathProcessor.Create(_prefix);
	try
		// todo: What about ConfigName?
		pathProcessor.PlatformName := _platformName;
		for i := 0 to _paths.Count - 1 do begin
			pathItem := _paths[i];
			pathItem := pathProcessor.Process(pathItem);
			if DirectoryExists(pathItem) then begin
				// Only add valid directories
				_paths[i] := pathItem;
			end else begin
				dbgMsg.ErrorMsgFmt('Path does not exist: %s', [pathItem]);
				_nonExistsPaths.Add(pathItem);
			end;
		end;
	finally
		FreeAndNil(pathProcessor);
	end;
end;

class procedure IOTAUtils.getAllAvailableMacros(_macros : TStrings; _project : IOTAProject = nil);
const
	IDE_BASE_MACROS : array [0 .. 3] of string = ('BDS', 'DELPHI', 'BCB', 'CompilerVersion');
var pathProcessor : TPathProcessor;
	i : Integer;
	defineValue : string;
	ideBasePath : string;
begin
	Assert(Assigned(_macros));
	_macros.Clear;

	pathProcessor := TPathProcessor.Create('', _project);
	try
		// Add IDE base paths
		ideBasePath := TIdeUtils.GetIdeRootDirectory;
		for i := low(IDE_BASE_MACROS) to high(IDE_BASE_MACROS) do begin
			_macros.Add(IDE_BASE_MACROS[i] + '=' + ideBasePath);
		end;

		// Add environment variables
		pathProcessor.GetEnvironmentVariables(_macros);

		// Add Platform and Config
		if pathProcessor.PlatformName <> '' then begin
			_macros.Add('Platform=' + pathProcessor.PlatformName);
		end;
		if pathProcessor.ConfigName <> '' then begin
			_macros.Add('Config=' + pathProcessor.ConfigName);
		end;

		// Add preprocessor constants (defines)
		defineValue := pathProcessor.GetProjectOption('DCC_Define');
		if defineValue <> '' then begin
			addProjectDefineMacros(defineValue, _macros);
		end;
	finally
		FreeAndNil(pathProcessor);
	end;
end;

class procedure IOTAUtils.RemoveLastEOL(var S : string);
var CurrLen : Integer; EOLSize : Integer;
begin
	CurrLen := Length(S);
	if CurrLen > 0 then begin
		EOLSize := EOLSizeAtPos(S, CurrLen);
		if EOLSize > 0 then begin
			Dec(CurrLen);
			if EOLSizeAtPos(S, CurrLen) > EOLSize then
				// one more character found for EOL
				Dec(CurrLen);
			SetLength(S, CurrLen);
		end;
	end;
end;

class function IOTAUtils.StrCharAt(const S : string; Pos : Integer) : Char;
begin
	if (Pos >= 1) and (Pos <= Length(S)) then
		Result := S[Pos]
	else
		Result := #0;
end;

class function IOTAUtils.TryFocusControl(Control : TWinControl) : Boolean;
begin
	Result := False;
	if Assigned(Control) then begin
		if Control.CanFocus and Control.Visible then begin
			try
				Control.SetFocus;
				Result := True;
			except
				// Ignore focus errors
			end; // FI:W501
		end;
	end;
end;

{ TPathProcessor }

constructor TPathProcessor.Create(const _Prefix : string; _Project : IOTAProject = nil);

begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPathProcessor.Process');

	inherited Create;
	FPrefix := _Prefix;
	dbgMsg.MsgFmt('FPrefix = %s', [FPrefix]);
	Init(_Project);
	FIdeBasePath := ExcludeTrailingPathDelimiter(TIdeUtils.GetIdeRootDirectory);
	// FIdeBasePath := ExcludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
	dbgMsg.MsgFmt('IdeBasePath = %s', [FIdeBasePath]);
	FEnvironment := TStringList.Create;
	GetEnvironmentVariables(FEnvironment);
end;

destructor TPathProcessor.Destroy;
begin
	FreeAndNil(FEnvironment);
	inherited;
end;

procedure TPathProcessor.GetEnvironmentVariables(Strings : TStrings);
var EnvStart : Pointer; EnvPos : PChar;
begin
	Assert(Assigned(Strings));
	Strings.Clear;
	EnvStart := GetEnvironmentStrings;
	try
		EnvPos := EnvStart;
		while StrLen(EnvPos) > 0 do begin
			Strings.Add(EnvPos);
			EnvPos := StrEnd(EnvPos) + 1;
		end;
	finally
		FreeEnvironmentStrings(EnvStart);
	end;
end;

procedure TPathProcessor.GetAllProjectOptions(Options : TStrings);
begin
	Assert(Assigned(Options));
	Options.Clear;
	if Assigned(FProjectOptions) then begin
		var
		OptionNames := FProjectOptions.GetOptionNames;
		for var i := 0 to Length(OptionNames) - 1 do begin
			var
			OptionName := OptionNames[i].Name;
			var
			OptionValue := VarToStr(FProjectOptions.Values[OptionName]);
			Options.Add(OptionName + '=' + OptionValue);
		end;
	end;
end;

function TPathProcessor.GetProjectOption(const OptionName : string) : string;
begin
	Result := '';
	if Assigned(FProjectOptions) then begin
		Result := VarToStr(FProjectOptions.Values[OptionName]);
	end;
end;

procedure TPathProcessor.Init(_Project : IOTAProject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPathProcessor.Init');
	if _Project = nil then begin
		_Project := IOTAUtils.GxOtaGetCurrentProject;
	end;
	if _Project <> nil then begin
		FPlatformName := _Project.CurrentPlatform;
		dbgMsg.MsgFmt('FPlatformName = %s', [FPlatformName]);
		FConfigName := _Project.CurrentConfiguration;
		dbgMsg.MsgFmt('FConfigName = %s', [FConfigName]);
		FProjectOptions := _Project.GetProjectOptions;
		dbgMsg.MsgFmt('FProjectOptions length %d', [Length(FProjectOptions.GetOptionNames)]);
	end;
end;

function TPathProcessor.Process(const _Path : string) : string;
const
	IDEBaseMacros : array [0 .. 2] of string = ('BDS', 'DELPHI', 'BCB');
var
	i : Integer;
	EnvName : string;
	EnvValue : string;
	DefineList : TStringList;
	DefineValue : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPathProcessor.Process');

	Result := _Path;

	// Expand the IDE base folder names $([DELPHI,BCB,BDS])
	for i := low(IDEBaseMacros) to high(IDEBaseMacros) do begin
		Result := ReplaceMacro(Result, IDEBaseMacros[i], FIdeBasePath);
	end;

	// Expand any environment variable macros
	for i := 0 to FEnvironment.Count - 1 do begin
		EnvName := Trim(FEnvironment.Names[i]);
		EnvValue := Trim(FEnvironment.Values[EnvName]);
		if (EnvName <> '') and (EnvValue <> '') then begin
			Result := ReplaceMacro(Result, EnvName, EnvValue);
		end;
	end;

	// Expand project preprocessor constants (DCC_Define)
	if Assigned(FProjectOptions) then begin
		DefineValue := VarToStr(FProjectOptions.Values['DCC_Define']);
		if DefineValue <> '' then begin
			DefineList := TStringList.Create;
			try
				DefineList.Delimiter := ';';
				DefineList.DelimitedText := DefineValue;
				for i := 0 to DefineList.Count - 1 do begin
					var
					Define := DefineList[i];
					var
					EqualPos := Pos('=', Define);
					if EqualPos > 0 then begin
						// Define with value: SYMBOL=VALUE
						var
						SymbolName := Copy(Define, 1, EqualPos - 1);
						var
						SymbolValue := Copy(Define, EqualPos + 1, Length(Define));
						Result := ReplaceMacro(Result, SymbolName, SymbolValue);
					end else begin
						// Define without value: SYMBOL (assume empty string or '1')
						Result := ReplaceMacro(Result, Define, '1');
					end;
				end;
			finally
				FreeAndNil(DefineList);
			end;
		end;
	end;

	if FPlatformName <> '' then begin
		Result := ReplaceMacro(Result, 'Platform', FPlatformName);
	end;

	if FConfigName <> '' then begin
		Result := ReplaceMacro(Result, 'Config', FConfigName);
	end;

	if ( not FPrefix.IsEmpty )  and (not TFileUtils.IsPathAbsolute(Result) )then begin
		Result := TFileUtils.ExpandFileNameRelBaseDir(Result, FPrefix);
	end;
end;

class function TPathProcessor.ReplaceMacro(const _str, _oldValue, _newValue : string) : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPathProcessor.ReplaceMacro', True);
	var
	replaceVal := '$(' + _oldValue + ')';
	if _str.Contains(replaceVal) then begin
		dbgMsg.MsgFmt('Path: %s', [_str]);
		Result := StringReplace(_str, replaceVal, _newValue, [rfReplaceAll, rfIgnoreCase]);
		dbgMsg.MsgFmt('Replacing: %s with: %s = %s', [replaceVal, _newValue, Result]);
	end else begin
		Result := _str;
	end;
end;

class procedure IOTAUtils.addProjectDefineMacros(var _defineValue : string; _macros : TStrings);
var defineList : TStringList;
begin
	defineList := TStringList.Create;
	try
		defineList.Delimiter := ';';
		defineList.DelimitedText := _defineValue;
		for var i := 0 to defineList.Count - 1 do begin
			var
			Define := defineList[i];
			var
			EqualPos := Pos('=', Define);
			if EqualPos > 0 then begin
				// Define with value: SYMBOL=VALUE
				_macros.Add(Define);
			end else begin
				// Define without value: SYMBOL (assume '1')
				_macros.Add(Define + '=1');
			end;
		end;
	finally
		FreeAndNil(defineList);
	end;
end;

class procedure IOTAUtils.getPreprocessorConstants(_defines : TStrings; _project : IOTAProject = nil);
var pathProcessor : TPathProcessor;
	defineValue : string; defineList : TStringList; i : Integer;
begin
	Assert(Assigned(_defines));
	_defines.Clear;

	pathProcessor := TPathProcessor.Create('', _project);
	try
		// Get preprocessor constants (_defines)
		defineValue := pathProcessor.GetProjectOption('DCC_Define');
		if defineValue <> '' then begin
			defineList := TStringList.Create;
			try
				defineList.Delimiter := ';';
				defineList.DelimitedText := defineValue;
				for i := 0 to defineList.Count - 1 do begin
					var
					Define := defineList[i];
					var
					EqualPos := Pos('=', Define);
					if EqualPos > 0 then begin
						// Define with value: SYMBOL=VALUE
						_defines.Add(Define);
					end else begin
						// Define without value: SYMBOL (assume '1')
						_defines.Add(Define + '=1');
					end;
				end;
			finally
				FreeAndNil(defineList);
			end;
		end;
	finally
		FreeAndNil(pathProcessor);
	end;
end;

class procedure IOTAUtils.showNonExistingPathsMessage(NonExistsPaths : TStrings);
var MessageText : string; PathsList : string;
const MAX_PATHS_TO_SHOW = 20; // Limit the number of paths shown in the message
begin
	Assert(Assigned(NonExistsPaths));

	if NonExistsPaths.Count = 0 then
		Exit;

	if NonExistsPaths.Count = 1 then
		MessageText := 'The following path does not exist:' + #13#10#13#10
	else
		MessageText := Format('The following %d paths do not exist:', [NonExistsPaths.Count]) + #13#10#13#10;

	// Build list of paths to show (limit to avoid too large message boxes)
	var
	PathCount := Min(NonExistsPaths.Count, MAX_PATHS_TO_SHOW);
	for var i := 0 to PathCount - 1 do begin
		PathsList := PathsList + '• ' + NonExistsPaths[i] + #13#10;
	end;

	if NonExistsPaths.Count > MAX_PATHS_TO_SHOW then
		PathsList := PathsList + Format('... and %d more paths', [NonExistsPaths.Count - MAX_PATHS_TO_SHOW]);

	MessageText := MessageText + PathsList + #13#10 + 'These paths will be ignored when searching for files. ' +
		'Please check your project and IDE library path settings.';

	// Show message box (use MessageBox for simple display)
	{$IFDEF MSWINDOWS}
	MessageBox(0, PChar(MessageText), 'Non-existing Paths Found', MB_OK or MB_ICONWARNING);
	{$ELSE}
	// Fallback for other platforms if needed
	Application.MessageBox(PChar(MessageText), 'Non-existing Paths Found', MB_OK or MB_ICONWARNING);
	{$ENDIF}
end;

class function TIdeUtils.GetIdeRootDirectory() : string;
const
	BIN_DIR_POSTFIX = PathDelim + 'Bin';
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIdeUtils.GetIdeRootDirectory');
    var ideBaseReg := IOTAUtils.GxOtaGetIdeBaseRegistryKey();
	if TRegistryUtils.TryReadString(ideBaseReg, 'RootDir', Result, HKEY_LOCAL_MACHINE) or
       TRegistryUtils.TryReadString(ideBaseReg, 'RootDir', Result, HKEY_CURRENT_USER)
    then begin
		dbgMsg.MsgFmt('RootDir from registry: %s', [Result]);
		if DirectoryExists(Result) then begin
			Result := IncludeTrailingPathDelimiter(Result);
			Exit;
		end;
	end;

	// There is no entry in the registry or it is invalid -> use the application's exename
	Result := ExcludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
	dbgMsg.MsgFmt('Application.ExeName: %s', [Result]);
	if SameText(RightStr(Result, Length(BIN_DIR_POSTFIX)), BIN_DIR_POSTFIX) then begin
		var
		len := Length(BIN_DIR_POSTFIX);
		Result := Result.Substring(Length(Result) - len + 1, len);
		Result := IncludeTrailingPathDelimiter(Result);
		dbgMsg.MsgFmt('Result %s', [Result]);
	end else begin
		Result := '';
		dbgMsg.Msg('Result empty');
	end;
end;

{$ELSE}

interface

implementation

{$ENDIF}

end.
