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
	RipGrepper.Common.RegistryUtils,
	RipGrepper.Common.IOTAUtils.PathProcessor;

type

	IIdeProjectPathHelper = interface(IInterface)
		['{200BE41D-8762-43FB-929D-877236B4FBC4}']
		function GetActiveProjectDirectory() : string;
		function GetActiveProjectFilePath() : string;
		function GetProjectFiles() : TArray<string>;
		function GetProjectFilesDirs() : TArray<string>;
		function GetOpenedEditBuffers() : TArray<string>;
		function GetCurrentSourceFile() : string;
		/// <summary>
		/// Return the effective library path, with the _project specific paths
		/// first and then the IDE's global library path.
		/// @params if _shouldProcess is true, the paths are macro expanded and non-existing
		/// paths removed.
		/// @params AdditionalPaths are appended at the end and optionally processed too </summary>
		function GetEffectiveLibraryPath(var _errDirList : TArrayEx<string>; const _shouldProcess : Boolean = True) : TArrayEx<string>;

	end;

	TIdeProjectPathHelper = class(TInterfacedObject, IIdeProjectPathHelper)
		const
			// 'DCC_UnitSearchPath'
			OPTION_NAME_UNIT_SEARCH_PATH = 'SrcDir';

		strict private
			FProject : IOTAProject;

		private
			// Get the environment options interface.  Succeeds or raises an exception.
			class function getEnvironmentOptions() : IOTAEnvironmentOptions;
			// Return the IDE's environment string value that is
			// associated with EnvironmentStringName
			class function getIdeEnvironmentString(const _sEnvName : string) : string;
			// Return the IDE's global library path
			class function getIdeLibraryPath() : string;
			/// <summary>
			/// Return the global IDE library path (without the project specific paths).
			/// @params if _shouldDoProcessing is true, the paths are macro expanded and non-existing
			/// paths removed. </summary>
			function getIdeLibraryPathStrings() : TArrayEx<string>;
			/// <summary>
			/// Return _project specific search path, with the directory containing the _project
			// file first.
			/// @params if _shouldDoProcessing is true, the paths are macro expanded and non-existing
			/// paths removed. </summary>
			function getProjectSourcePathStrings() : TArrayEx<string>;
			class function isCurrentProjectIsDelphiDotNet() : Boolean;
			class function isCurrentProjectNativeCpp() : Boolean;
			class function IsProjectDelphiDotNet(Project : IOTAProject) : Boolean;
			class function isProjectNativeCpp(Project : IOTAProject) : Boolean;
			function processPaths(const _paths : TArrayEx<string>; var _nonExistsPaths : TArrayEx<string>; const _rootDir : string)
				: TArrayEx<string>;
			/// <summary>
			/// Tries to get the options of the given or the active project
			/// @param ProjectOptions will contain the options, only valid if Result is True
			/// @param Project is the project whose options to get, it nil the current project will be used
			/// @returns True, if the options could be retrieved, False otherwise (e.g. if there is no active project) </summary>
			function tryGetProjectOptions(out _ProjectOptions : IOTAProjectOptions) : Boolean;
			{$REGION  usefull functions} { {
			  /// <summary>
			  /// Get all available _macros that can be used in ReplaceMacros including Platform, Config,
			  /// environment variables, IDE base paths, and preprocessor constants (defines)
			  /// @param _macros will contain all available _macros in "Name=Value" format
			  /// @param _project is the _project to get options from, if nil the current _project will be used </summary>
			  class procedure getAllAvailableMacros(_macros : TStrings; _project : IOTAProject = nil);
			  // Return all of the IDE's environment settings names
			  class procedure getIdeEnvironmentStrings(Settings : TStrings);
			  /// <summary>
			  /// Get all preprocessor constants (conditional _defines) for the given _project
			  /// @param _defines will contain all conditional _defines in "Name=Value" format
			  /// @param _project is the _project to get _defines from, if nil the current _project will be used </summary>
			  class procedure getPreprocessorConstants(_defines : TStrings; _project : IOTAProject = nil);
			  class procedure addProjectDefineMacros(var _defineValue : string; _macros : TStrings);
			  class function getIdeRootDirectoryFromRegistry() : string;
			  // Returns IDE's base registry key, for instance
			  // Software\Borland\Delphi\4.0
			  // The returned string is guaranteed to NOT have a
			  // backslash appended and it does NOT have a leading
			  // backslash either (Windows 2000 is allergic to that).
			  class function getIdeBaseRegistryKey() : string;

			}  {$ENDREGION  usefull functions}

		public
			constructor Create();
			function GetActiveProjectDirectory() : string;
			function GetActiveProjectFilePath() : string;
			/// <summary>
			/// Return the effective library path, with the _project specific paths
			/// first and then the IDE's global library path.
			/// @params if _shouldProcess is true, the paths are macro expanded and non-existing
			/// paths removed.
			/// @params AdditionalPaths are appended at the end and optionally processed too </summary>
			function GetEffectiveLibraryPath(var _errDirList : TArrayEx<string>; const _shouldProcess : Boolean = True) : TArrayEx<string>;
			// Returns a fully qualified name of the current file,
			// which could either be a form or unit (.pas/.cpp/.dfm/.xfm etc.).
			// Returns an empty string if no file is currently selected.
			function GetCurrentSourceFile() : string;
			function GetOpenedEditBuffers() : TArray<string>;
			function GetProjectFiles() : TArray<string>;
			function GetProjectFilesDirs() : TArray<string>;
	end;

	IOTAUtils = class(TObject)

		private
			// Returns the size of any EOL character(s) at a given position (0 if none)
			class function getEOLSizeAtPos(const S : string; Pos : Integer) : Integer;
			// Raise an exception if the source editor is readonly
			class procedure assertSourceEditorNotReadOnly(_sourceEditor : IOTASourceEditor);
			class function convertColumnCharsToBytes(_sLineData : UTF8String; _iCharIndex : Integer; _bEndByte : Boolean) : Integer;
			// Determine is a file exists or the file's module is currently loaded in the IDE
			class function fileOrModuleExists(const _sFileName : string; _bUseBase : Boolean = False) : Boolean;
			class function ideEditorStringToString(const S : string) : string; overload;
			// Remove the last EOL character from a string
			// EOL can be one or two characters long
			// Useful for processing strings read from memo controls
			class procedure removeLastEOL(var S : string);
			// Get character at some position or #0 for invalid positions
			class function strCharAt(const S : string; Pos : Integer) : Char;
			class function tryFocusControl(_ctrl : TWinControl) : Boolean;
			/// <summary>
			/// Common functionality for building/compiling the active project
			/// </summary>
			class procedure executeProjectCompilation(const _compileMode : TOTACompileMode);

		public
			class function AddToImageList(_bmp : Vcl.Graphics.TBitmap; const _identText : string) : Integer;
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

			class function GetOpenedEditorFiles(const _bModifiedOnly : Boolean) : TArray<string>;
			class function GetExternallyModifiedFiles : TArray<string>;

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
			// Obtain the IOTAEditActions interface for a given module
			class function GxOtaGetEditActionsFromModule(Module : IOTAModule) : IOTAEditActions;
			class function GxOtaGetEditorLine(View : IOTAEditView; LineNo : Integer) : UTF8String;
			class function GxOtaGetEditorServices : IOTAEditorServices;
			// Get an edit writer for the current source editor (raise an exception if not availble)
			// Use the current source editor if none is specified
			class function GxOtaGetEditWriterForSourceEditor(SourceEditor : IOTASourceEditor = nil) : IOTAEditWriter;
			// Get the Index-th IOTAEditor for the given module
			// Works around a BCB 5 bug with simple units
			class function GxOtaGetFileEditorForModule(Module : IOTAModule; index : Integer) : IOTAEditor;
			// Returns a form editor for Module if it exists; nil otherwise.
			// Module can be nil, if it is , this function will return nil
			class function GxOtaGetFormEditorFromModule(const Module : IOTAModule) : IOTAFormEditor;
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
			class function GxOtaProjectIsEitherDelphi(Project : IOTAProject) : Boolean;
			// Save an edit reader to a stream
			class procedure GxOtaSaveReaderToStream(EditReader : IOTAEditReader; Stream : TStream; TrailingNull : Boolean = True);
			class function GxOtaTryGetCurrentProject(out _Project : IOTAProject) : Boolean;
			class function GxOtaTryGetCurrentSourceEditor(out SourceEditor : IOTASourceEditor) : boolean;
			// calls GxOtaGetTopMostEditView and returns false if no edit view is available
			class function GxOtaTryGetTopMostEditView(out EditView : IOTAEditView) : boolean; overload;
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
			class procedure BuildActiveProject();
			class procedure CompileActiveProject();
			// Save sFileName in IDE; returns True on success, False otherwise.
			class function SaveFile(const sFileName : string) : Boolean;
			class procedure ReloadModifiedFiles();
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
	System.Math,
	Spring,

	System.UITypes;

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
class function IOTAUtils.getEOLSizeAtPos(const S : string; Pos : Integer) : Integer;
begin
	if IsCharLineEnding(strCharAt(S, Pos)) then begin
		Result := 1;
		if (IsCharLineEnding(strCharAt(S, Pos + 1)) and (strCharAt(S, Pos) <> strCharAt(S, Pos + 1))) then
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

class function IOTAUtils.GetOpenedEditorFiles(const _bModifiedOnly : Boolean) : TArray<string>;
var
	module : IOTAModule;
	editor : IOTAEditor;
	service : IOTAModuleServices;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('IOTAUtils.GetOpenedEditorFiles');
	Result := [];
	service := (BorlandIDEServices as IOTAModuleServices);
	if Assigned(service) then begin
		for var i := 0 to service.ModuleCount - 1 do begin
			module := service.Modules[i];
			for var j := 0 to module.GetModuleFileCount - 1 do begin
				editor := module.GetModuleFileEditor(j);
				if _bModifiedOnly and (not editor.Modified) then begin
					dbgMsg.Msg('Skipping non-modified: ' + editor.FileName);
					continue;
				end else begin
					dbgMsg.Msg('Add to result: ' + editor.FileName);
					Result := Result + [editor.FileName];
				end;
			end;
		end;
	end;
end;

class function IOTAUtils.GetExternallyModifiedFiles : TArray<string>;
var
	service : IOTAEditorServices;
	it : IOTAEditBufferIterator;
	buffer : IOTAEditBuffer;
	fileTime, bufferTime : TDateTime;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('IOTAUtils.GetExternallyModifiedFiles');

	Result := [];
	service := (BorlandIDEServices as IOTAEditorServices);
	if Assigned(service) then begin
		if (service.GetEditBufferIterator(it)) then begin
			for var i := 0 to it.Count - 1 do begin
				buffer := it.EditBuffers[i];
				// Only check files that are actually open in views
				if { (buffer.EditViewCount > 0) and } (buffer.FileName = '') then begin
					continue;
				end;
				dbgMsg.Msg(Format('Check file %s ViewCount:%d', [buffer.FileName, buffer.EditViewCount]));
				try
					if FileAge(buffer.FileName, fileTime) then begin
						bufferTime := buffer.GetInitialDate;
						if fileTime > bufferTime then begin
							dbgMsg.Msg(Format('File externally modified: %s (Disk: %s, Buffer: %s)',
								[buffer.FileName, DateTimeToStr(fileTime), DateTimeToStr(bufferTime)]));
							Result := Result + [buffer.FileName];
						end;
					end;
				except
					on E : Exception do
						dbgMsg.ErrorMsg('Error checking file modification time for ' + buffer.FileName + ': ' + E.Message);
				end;
			end;
		end;
	end;
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

class procedure IOTAUtils.assertSourceEditorNotReadOnly(_sourceEditor : IOTASourceEditor);
begin
	Assert(Assigned(_sourceEditor));
	if Supports(_sourceEditor, IOTAEditBuffer) then
		if (_sourceEditor as IOTAEditBuffer).IsReadOnly then
			raise Exception.CreateFmt('%s is read only', [ExtractFileName(_sourceEditor.FileName)]);
end;

class function IOTAUtils.convertColumnCharsToBytes(_sLineData : UTF8String; _iCharIndex : Integer; _bEndByte : Boolean) : Integer;
var
	UString : string;
	FinalUChar : string;
	UTF8Str : UTF8String;
begin
	UString := UTF8ToUnicodeString(_sLineData);
	UString := Copy(UString, 1, _iCharIndex);
	UTF8Str := UTF8String(UTF8Encode(UString));
	Result := Length(UTF8Str);
	if not _bEndByte then begin
		if Length(UString) = 0 then
			Result := 0
		else begin
			FinalUChar := UString[Length(UString)];
			UTF8Str := Utf8String(UTF8Encode(FinalUChar));
			Result := Result - (Length(UTF8Str)) + 1;
		end;
	end;
end;

class function IOTAUtils.fileOrModuleExists(const _sFileName : string; _bUseBase : Boolean = False) : Boolean;
begin
	Result := FileExists(_sFileName) or IsFileOpen(_sFileName, _bUseBase);
end;

class function IOTAUtils.GxOtaFocusCurrentIDEEditControl : Boolean;
var
	EditControl : TWinControl;
begin
	Result := False;
	EditControl := GxOtaGetCurrentIDEEditControl;
	tryFocusControl(EditControl)
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
		// if fileOrModuleExists(AltName) then
		// Result := AltName;
		// end;
		AltName := ChangeFileExt(FileName, '.pas');
		if fileOrModuleExists(AltName) then
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
		Result := ideEditorStringToString(EditBlock.Text);
		// Result := HackBadIDEUTF8StringToString(Result);

		if not IncludeTrailingCRLF and (EditBlock.Style in [btNonInclusive]) then
			removeLastEOL(Result);
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
		assertSourceEditorNotReadOnly(SourceEditor);
		Result := SourceEditor.CreateUndoableWriter;
	end;
	Assert(Assigned(Result), SEditWriterNotAvail);
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

	if Module.GetModuleFileCount <= 1 then begin
		Exit;
	end;
	var
	dbgMsg := TDebugMsgBeginEnd.New('IOTAUtils.GxOtaGetFormEditorFromModule', True);

	dbgMsg.MsgFmt('Checking module %s for form editors', [Module.FileName]);
	for i := 0 to Module.GetModuleFileCount - 1 do begin
		Editor := GxOtaGetFileEditorForModule(Module, i);
		if Supports(Editor, IOTAFormEditor, FormEditor) then begin
			Result := FormEditor;
			dbgMsg.Msg('Found form editor: ' + Result.FileName + ' is modified=' +
				{ } BoolToStr(FormEditor.Modified, True));
		end;
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
var
	ModuleServices : IOTAModuleServices;
begin
	ModuleServices := BorlandIDEServices as IOTAModuleServices;
	Assert(Assigned(ModuleServices));
	Result := ModuleServices.ModuleCount;
end;

class function IOTAUtils.GxOtaGetProjectFileName(Project : IOTAProject; NormalizeBdsProj : Boolean = False) : string;

	function SearchProjectSourceViaModule(var AProjectFileName : string) : Boolean;
	var
		i : Integer;
		Module : IOTAModule;
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
	var
		PackageFileName : string;
	begin
		Result := GxOtaProjectIsEitherDelphi(Project);
		if Result then begin
			AProjectFileName := ChangeFileExt(AProjectFileName, '.dpr');
			if not fileOrModuleExists(AProjectFileName) then begin
				PackageFileName := ChangeFileExt(AProjectFileName, '.dpk');
				if fileOrModuleExists(PackageFileName) then
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
var
	IModuleServices : IOTAModuleServices;
	IModule : IOTAModule;
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
var
	EditView : IOTAEditView;
	Module : IOTAModule;
	SourceEditor : IOTASourceEditor;
	CurPos : TOTAEditPos;
	CharPos : TOTACharPos;
	EditPos : TOTAEditPos;
	MatchLength : Integer;
	LineData : UTF8String;
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

	LineData := GxOtaGetEditorLine(EditView, Line);
	StartColumn := convertColumnCharsToBytes(LineData, StartColumn, False);
	StopColumn := convertColumnCharsToBytes(LineData, StopColumn, True);

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
var
	ModuleServices : IOTAModuleServices;
	Module : IOTAModule;
	FileEditor : IOTAEditor;
	i : Integer;
	FileName : string;
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
var
	MemStream : TMemoryStream;
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
var
	EditActions : IOTAEditActions;
	Module : IOTAModule;
	FormEditor : IOTAFormEditor;
	SourceEditor : IOTASourceEditor;
	FileEditor : IOTAEditor;
	i : Integer;
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
var
	Editor : IOTAEditor;
	i : Integer;
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
var
	ActionServices : IOTAActionServices;
	hWndSaved : HWND;
begin
	ActionServices := BorlandIDEServices as IOTAActionServices;
	Assert(Assigned(ActionServices));

	hWndSaved := GetForegroundWindow;
	Result := ActionServices.OpenFile(FileName);
	if hWndSaved <> 0 then
		SetForegroundWindow(hWndSaved);
end;

class function IOTAUtils.GxOtaProjectIsEitherDelphi(Project : IOTAProject) : Boolean;
begin
	Result := MatchStr(GxOtaGetProjectPersonality(Project), [sDelphiPersonality, sDelphiDotNetPersonality]);
end;

class procedure IOTAUtils.GxOtaSaveReaderToStream(EditReader : IOTAEditReader; Stream : TStream; TrailingNull : Boolean = True);
const
	// Leave typed constant as is - needed for streaming code.
	NULL_CHAR : AnsiChar = #0;
	BUFFER_SIZE = 1024 * 24;
var
	EditReaderPos : Integer;
	ReadDataSize : Integer;
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
		Stream.Write(NULL_CHAR, SizeOf(NULL_CHAR));
	// The source parsers need this
end;

class function IOTAUtils.GxOtaTryGetCurrentProject(out _Project : IOTAProject) : Boolean;
var
	IProjectGroup : IOTAProjectGroup;
	IModuleServices : IOTAModuleServices;
	IModule : IOTAModule;
	i : Integer;
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

class function IOTAUtils.GxOtaTryGetTopMostEditView(out EditView : IOTAEditView) : boolean;
begin
	EditView := GxOtaGetTopMostEditView;
	Result := Assigned(EditView);
end;

class function IOTAUtils.ideEditorStringToString(const S : string) : string;
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

class procedure IOTAUtils.removeLastEOL(var S : string);
var
	CurrLen : Integer;
	EOLSize : Integer;
begin
	CurrLen := Length(S);
	if CurrLen > 0 then begin
		EOLSize := getEOLSizeAtPos(S, CurrLen);
		if EOLSize > 0 then begin
			Dec(CurrLen);
			if getEOLSizeAtPos(S, CurrLen) > EOLSize then
				// one more character found for EOL
				Dec(CurrLen);
			SetLength(S, CurrLen);
		end;
	end;
end;

class function IOTAUtils.strCharAt(const S : string; Pos : Integer) : Char;
begin
	if (Pos >= 1) and (Pos <= Length(S)) then
		Result := S[Pos]
	else
		Result := #0;
end;

class function IOTAUtils.tryFocusControl(_ctrl : TWinControl) : Boolean;
begin
	Result := False;
	if Assigned(_ctrl) then begin
		if _ctrl.CanFocus and _ctrl.Visible then begin
			try
				_ctrl.SetFocus;
				Result := True;
			except
				// Ignore focus errors
			end; // FI:W501
		end;
	end;
end;

class function IOTAUtils.GxOtaGetSourceEditorFromModule(Module : IOTAModule; const FileName : string = '') : IOTASourceEditor;
var
	i : Integer;
	IEditor : IOTAEditor;
	ISourceEditor : IOTASourceEditor;
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

constructor TIdeProjectPathHelper.Create();
begin
	inherited;
	FProject := IOTAUtils.GxOtaGetCurrentProject;
end;

{$REGION  usefull functions} {
  class procedure TIdeProjectPathHelper.addProjectDefineMacros(var _defineValue : string; _macros : TStrings);
  var
  defineList : TStringList;
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
  end; }
{$ENDREGION  usefull functions}

function TIdeProjectPathHelper.GetActiveProjectDirectory() : string;
begin
	Result := ExtractFileDir(GetActiveProjectFilePath());
end;

function TIdeProjectPathHelper.GetActiveProjectFilePath() : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIdeProjectPathHelper.GetActiveProjectFilePath');

	Result := '';
	if Assigned(FProject) then begin
		Result := FProject.FileName;
	end;
	dbgMsg.MsgFmt('Result = %s', [Result]);
end;

{$REGION  usefull functions}  {
  class procedure TIdeProjectPathHelper.getAllAvailableMacros(_macros : TStrings; _project : IOTAProject = nil);
  const
  IDE_BASE_MACROS : array [0 .. 3] of string = ('BDS', 'DELPHI', 'BCB', 'CompilerVersion');
  var
  pathProcessor : TPathProcessor;
  i : Integer;
  defineValue : string;
  ideBasePath : string;
  begin
  var
  dbgMsg := TDebugMsgBeginEnd.New('TIdeProjectPathHelper.getAllAvailableMacros');

  _macros.Clear;

  pathProcessor := TPathProcessor.Create('', _project);
  try
  // Add IDE base paths
  ideBasePath := TIdeProjectPathHelper.getIdeRootDirectoryFromRegistry;
  if ideBasePath.IsEmpty then begin
  ideBasePath := TIdeUtils.GetIdeRootDirectory;
  end;
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
  dbgMsg.MsgFmt('Macros: %s', [_macros.Text]);
  FreeAndNil(pathProcessor);
  end;
  end;

  class function TIdeProjectPathHelper.getIdeRootDirectoryFromRegistry() : string;
  begin
  var
  dbgMsg := TDebugMsgBeginEnd.New('TIdeProjectPathHelper.getIdeRootDirectoryFromRegistry');
  Result := '';
  var
  ideBaseReg := TIdeProjectPathHelper.getIdeBaseRegistryKey();
  if TRegistryUtils.TryReadString(ideBaseReg, 'RootDir', Result, HKEY_LOCAL_MACHINE) or
  TRegistryUtils.TryReadString(ideBaseReg, 'RootDir', Result, HKEY_CURRENT_USER) then begin
  dbgMsg.MsgFmt('RootDir from registry: %s', [Result]);
  if DirectoryExists(Result) then begin
  Result := IncludeTrailingPathDelimiter(Result);
  Exit;
  end;
  end;
  end;


  class function TIdeProjectPathHelper.getIdeBaseRegistryKey() : string;
  begin
  var
  dbgMsg := TDebugMsgBeginEnd.New('TIdeProjectPathHelper.getIdeBaseRegistryKey');

  if isStandAlone then
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
}
{$ENDREGION}

function TIdeProjectPathHelper.GetEffectiveLibraryPath(var _errDirList : TArrayEx<string>; const _shouldProcess : Boolean = True)
	: TArrayEx<string>;
var
	pathList : TArrayEx<string>;
	pathList2 : TArrayEx<string>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIdeProjectPathHelper.GetEffectiveLibraryPath');
	// _shouldProcess = True uses the _project file's directory as the base to expand relative paths
	pathList := getProjectSourcePathStrings();
	pathList2 := getIdeLibraryPathStrings();
	pathList.AddRange(pathList2);
	dbgMsg.MsgFmt('Combined paths: %s', [string.Join(CRLF + TAB, pathList.Items)]);
	if _shouldProcess then begin
		Result := processPaths(pathList, _errDirList, GetActiveProjectDirectory);
	end;
end;

class function TIdeProjectPathHelper.getIdeEnvironmentString(const _sEnvName : string) : string;
begin
	Result := getEnvironmentOptions.Values[_sEnvName];
end;

class function TIdeProjectPathHelper.getEnvironmentOptions() : IOTAEnvironmentOptions;
begin
	var
	services := BorlandIDEServices as IOTAServices;
	Result := services.GetEnvironmentOptions;
	Assert(Assigned(Result));
end;

{$REGION  usefull functions} {
  class procedure TIdeProjectPathHelper.getIdeEnvironmentStrings(Settings : TStrings);
  var
  EnvOptions : IOTAEnvironmentOptions;
  i : Integer;
  Options : TOTAOptionNameArray;
  begin
  EnvOptions := getEnvironmentOptions;

  Settings.Clear;
  Options := EnvOptions.GetOptionNames; // Broken in Delphi 2005
  for i := 0 to Length(Options) - 1 do
  Settings.Add(Options[i].Name);
  end;
} {$ENDREGION}

class function TIdeProjectPathHelper.isCurrentProjectNativeCpp() : Boolean;
begin
	Result := isProjectNativeCpp(IOTAUtils.GxOtaGetCurrentProject);
end;

class function TIdeProjectPathHelper.getIdeLibraryPath() : string;
begin
	// Do not localize.
	var
	bRunningDelphi11OrGreater := {$IF CompilerVersion >= 35} TRUE {$ELSE} FALSE
	{$ENDIF};
	if bRunningDelphi11OrGreater and isCurrentProjectIsDelphiDotNet then
		Result := getIdeEnvironmentString('DotNetLibraryPath')
	else if bRunningDelphi11OrGreater and isCurrentProjectNativeCpp then
		Result := getIdeEnvironmentString('CppSearchPath')
	else
		Result := getIdeEnvironmentString('LibraryPath');
end;

function TIdeProjectPathHelper.getIdeLibraryPathStrings() : TArrayEx<string>;
var
	idePathString : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIdeProjectPathHelper.getIdeLibraryPathStrings');
	idePathString := getIdeLibraryPath;
	Result := idePathString.Split([';']);
	dbgMsg.MsgFmt('Result: %s', [string.Join(CRLF + TAB, Result.Items)]);
end;

function TIdeProjectPathHelper.getProjectSourcePathStrings() : TArrayEx<string>;
var
	idePathString : string;
	projectOptions : IOTAProjectOptions;
	projectDir : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIdeProjectPathHelper.getProjectSourcePathStrings');
	if Assigned(FProject) then begin
		projectDir := GetActiveProjectDirectory();
		// Add the current _project directory first
		Result.Add(projectDir);
		// Then the _project search path
		if tryGetProjectOptions(projectOptions) then begin
			idePathString := projectOptions.Values[OPTION_NAME_UNIT_SEARCH_PATH];
			Result.AddRange(idePathString.Split([';']));
		end;
	end;
	dbgMsg.MsgFmt('Result: %s', [string.Join(CRLF + TAB, Result.Items)]);
end;

function TIdeProjectPathHelper.GetCurrentSourceFile() : string;
var
	Module : IOTAModule;
	Editor : IOTAEditor;
begin
	Result := '';
	Module := IOTAUtils.GxOtaGetCurrentModule;
	if Module <> nil then begin
		Editor := Module.GetCurrentEditor;
		if Editor <> nil then
			Result := Editor.FileName
		else
			// C++Builder 6 returns nil for some old-style modules without DFMs
			Result := Module.FileName;
	end;
end;

function TIdeProjectPathHelper.GetOpenedEditBuffers() : TArray<string>;
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
				TDebugUtils.DebugMessage('TIdeProjectPathHelper.GetOpenedEditBuffers FileName=' + buffer.FileName + ' ViewCount=' +
					buffer.EditViewCount.ToString);
				if buffer.EditViewCount > 0 then begin
					Result := Result + [buffer.FileName];
				end;
			end;
		end;
	end;
end;

{$REGION  usefull functions} {
  class procedure TIdeProjectPathHelper.getPreprocessorConstants(_defines : TStrings; _project : IOTAProject = nil);
  var
  pathProcessor : TPathProcessor;
  defineValue : string;
  defineList : TStringList;
  i : Integer;
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
  end; }
{$ENDREGION}

function TIdeProjectPathHelper.GetProjectFiles() : TArray<string>;
var
	fn : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIdeProjectPathHelper.GetProjectFiles');

	Result := [];
	if not Assigned(FProject) then
		Exit;
	for var i : integer := 0 to FProject.GetModuleCount - 1 do begin
		fn := FProject.GetModule(i).GetFileName;
		if not fn.IsEmpty then begin
			TDebugUtils.DebugMessage('Add FileName=' + fn);
			Result := Result + [fn]
		end;
	end;
end;

function TIdeProjectPathHelper.GetProjectFilesDirs() : TArray<string>;
var
	fn : string;
	arr : TArrayEx<string>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIdeProjectPathHelper.GetProjectFilesDirs');

	if not Assigned(FProject) then
		Exit;
	for var i : integer := 0 to FProject.GetModuleCount - 1 do begin
		fn := FProject.GetModule(i).GetFileName;
		if not fn.IsEmpty then begin
			var
			dir := TPath.GetDirectoryName(fn);
			TDebugUtils.DebugMessage('Add DirName=' + dir);
			arr.AddIfNotContains(dir);
		end;
	end;
	Result := arr.Items;
end;

class function TIdeProjectPathHelper.IsProjectDelphiDotNet(Project : IOTAProject) : Boolean;
begin
	Result := SameText(IOTAUtils.GxOtaGetProjectPersonality(Project), sDelphiDotNetPersonality);
end;

function TIdeProjectPathHelper.tryGetProjectOptions(out _ProjectOptions : IOTAProjectOptions) : Boolean;
begin
	if Assigned(FProject) then
		_ProjectOptions := FProject.GetProjectOptions;
	Result := Assigned(_ProjectOptions);
end;

class function TIdeProjectPathHelper.isProjectNativeCpp(Project : IOTAProject) : Boolean;
begin
	Result := SameText(IOTAUtils.GxOtaGetProjectPersonality(Project), sCBuilderPersonality);
end;

class function TIdeProjectPathHelper.isCurrentProjectIsDelphiDotNet() : Boolean;
begin
	Result := IsProjectDelphiDotNet(IOTAUtils.GxOtaGetCurrentProject);
end;

function TIdeProjectPathHelper.processPaths(const _paths : TArrayEx<string>; var _nonExistsPaths : TArrayEx<string>;
	const _rootDir : string) : TArrayEx<string>;
var
	i : Integer;
	pathItem : string;
	pathProcessor : IShared<TPathProcessor>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIdeProjectPathHelper.processPaths');
	pathProcessor := Shared.Make<TPathProcessor>(TPathProcessor.Create(_rootDir, FProject));

	for i := 0 to _paths.Count - 1 do begin
		pathItem := pathProcessor.Process(_paths[i]);
		if not pathItem.IsEmpty then begin
			Result.AddIfNotContains(pathItem);
		end;
		dbgMsg.MsgFmt('Original path: %s', [_paths[i]], tftVerbose);
		dbgMsg.MsgFmt('Processed path: %s', [pathItem], tftVerbose);
	end;

	_nonExistsPaths.AddRange(pathProcessor.NonExistsPaths.ToStringArray);
end;

class procedure IOTAUtils.executeProjectCompilation(const _compileMode : TOTACompileMode);
var
	projectGroup : IOTAProjectGroup;
	activeProject : IOTAProject;
	compileServices : IOTACompileServices;
	dbgMsg : TDebugMsgBeginEnd;
	operationName : string;
begin
	// Derive operation name from compile mode
	case _compileMode of
		cmOTAMake :
		operationName := 'Compiling';
		cmOTABuild :
		operationName := 'Building';
		cmOTACheck :
		operationName := 'Checking';
		cmOTAClean :
		operationName := 'Cleaning';
		else
		operationName := 'Unknown operation';
	end;

	dbgMsg := TDebugMsgBeginEnd.New('IOTAUtils.executeProjectCompilation: ' + operationName);

	try
		// Get the current project group
		projectGroup := GxOtaGetProjectGroup();
		if not Assigned(projectGroup) then begin
			dbgMsg.Msg('No project group found');
			Exit;
		end;

		// Get the active project
		activeProject := projectGroup.ActiveProject;
		if not Assigned(activeProject) then begin
			dbgMsg.Msg('No active project found');
			Exit;
		end;

		dbgMsg.Msg(operationName + ' project: ' + activeProject.FileName);

		// Get compile services and execute the operation
		if Supports(BorlandIDEServices, IOTACompileServices, compileServices) then begin
			compileServices.CompileProjects([activeProject], _compileMode, True, False);
			dbgMsg.Msg(operationName + ' command sent');
		end else begin
			dbgMsg.Msg('Could not get compile services');
		end;
	except
		on E : Exception do
			dbgMsg.Msg('Exception during ' + operationName + ': ' + E.Message);
	end;
end;

class procedure IOTAUtils.BuildActiveProject();
begin
	executeProjectCompilation(cmOTABuild);
end;

class procedure IOTAUtils.CompileActiveProject();
begin
	executeProjectCompilation(cmOTAMake);
end;

class function IOTAUtils.SaveFile(const sFileName : string) : Boolean;
var
	actionServices : IOTAActionServices;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('IOTAUtils.SaveFile');

	actionServices := BorlandIDEServices as IOTAActionServices;
	Assert(Assigned(actionServices));
	dbgMsg.MsgFmt('File: %s', [sFileName]);
	Result := actionServices.SaveFile(sFileName);
	dbgMsg.MsgIf(not Result, 'Couldn''t save file!', tftError);
end;

class procedure IOTAUtils.ReloadModifiedFiles();
var
	modifiedFiles : TArray<string>;
	fileName : string;
	actionServices : IOTAActionServices;
	dbgMsg : TDebugMsgBeginEnd;
begin
	dbgMsg := TDebugMsgBeginEnd.New('IOTAUtils.ReloadModifiedFiles');

	// Get list of files modified externally (on disk)
	modifiedFiles := GetExternallyModifiedFiles();

	if Length(modifiedFiles) = 0 then begin
		dbgMsg.Msg('No externally modified files to reload');
		Exit;
	end;

	dbgMsg.MsgFmt('Found %d externally modified files to reload', [Length(modifiedFiles)]);

	// Get action services for closing/opening files
	actionServices := BorlandIDEServices as IOTAActionServices;
	if not Assigned(actionServices) then begin
		dbgMsg.Msg('Could not get action services');
		Exit;
	end;

	// Close and reopen each modified file
	for fileName in modifiedFiles do begin
		dbgMsg.MsgFmt('Reloading file: %s', [fileName]);
		try
			// Close the file first
			if actionServices.CloseFile(fileName) then begin
				// Reopen the file
				if not actionServices.OpenFile(fileName) then begin
					dbgMsg.MsgFmt('Failed to reopen file: %s', [fileName]);
				end;
			end else begin
				dbgMsg.MsgFmt('Failed to close file: %s', [fileName]);
			end;
		except
			on E : Exception do
				dbgMsg.MsgFmt('Exception reloading file %s: %s', [fileName, E.Message]);
		end;
	end;
	dbgMsg.Msg('File reload completed');
end;

{$ELSE}

interface

implementation

{$ENDIF}

end.
