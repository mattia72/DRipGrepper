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
	Vcl.Graphics;

type
	IOTAUTils = class
		private
		public
			class function AddToImageList(_bmp : Vcl.Graphics.TBitmap; const _identText : string) : Integer;
			// Returns the size of any EOL character(s) at a given position (0 if none)
			class function EOLSizeAtPos(const S : string; Pos : Integer) : Integer;
			class function FileMatchesExtension(const FileName, FileExtension : string) : Boolean;
			class function FileMatchesExtensions(const FileName : string; FileExtensions : array of string) : Boolean; overload;
			class function GetSettingFilePath : string;
			class function FindMenuItem(const Name : string) : TMenuItem;
			class function FindMenu(const Name : string) : TMenu;
			class function GetEditPosition: IOTAEditPosition;
			// Get the actual TEditControl embedded in the given IDE editor form
			class function GetIDEEditControl(Form : TCustomForm) : TWinControl;

			class function GetOpenedEditorFiles : TArray<string>;
			class function GetOpenedEditBuffers : TArray<string>;

			class function GetProjectFiles : TArray<string>;

			class function GxOtaConvertColumnCharsToBytes(LineData : UTF8String; CharIndex : Integer; EndByte : Boolean) : Integer;
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
			// Get the Index-th IOTAEditor for the given module
			// Works around a BCB 5 bug with simple units
			class function GxOtaGetFileEditorForModule(Module : IOTAModule; Index : Integer) : IOTAEditor;
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
			// Returns True if FileName is an open file in the IDE;
			// returns False otherwise.
			// FileName must be a fully qualified file name, including
			// the path name.  UseBase determines whether things like .dfm/.hpp map
			// to their parent file type.  Otherwise, they will not be located.
			class function GxOtaIsFileOpen(const AFileName : string; UseBase : Boolean = False) : Boolean;
			class procedure GxOtaLoadSourceEditorToUnicodeStrings(SourceEditor : IOTASourceEditor; Data : TStringList);
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

implementation

uses
	System.IOUtils,
	System.SysUtils,
	System.StrUtils,
	RipGrepper.Common.Constants,
	Winapi.Windows,
	RipGrepper.Tools.DebugUtils;

class function IOTAUTils.AddToImageList(_bmp : Vcl.Graphics.TBitmap; const _identText : string): Integer;
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
class function IOTAUTils.EOLSizeAtPos(const S : string; Pos : Integer) : Integer;
begin
	if IsCharLineEnding(StrCharAt(S, Pos)) then begin
		Result := 1;
		if (IsCharLineEnding(StrCharAt(S, Pos + 1)) and (StrCharAt(S, Pos) <> StrCharAt(S, Pos + 1))) then
			Inc(Result);
	end
	else
		Result := 0;
end;

class function IOTAUTils.FileMatchesExtension(const FileName, FileExtension : string) : Boolean;
begin
	Result := FileMatchesExtensions(FileName, [FileExtension]);
end;

class function IOTAUTils.FileMatchesExtensions(const FileName : string; FileExtensions : array of string) : Boolean;
begin
	Result := MatchStr(ExtractFileExt(FileName), FileExtensions);
end;

class function IOTAUTils.FindMenuItem(const Name : string) : TMenuItem;
var
	Comp : TComponent;
begin
	Comp := Application.MainForm.FindComponent(name);
	if (Comp <> nil) and (Comp is TMenuItem) then
		Result := TMenuItem(Comp)
	else
		Result := nil;
end;

class function IOTAUTils.FindMenu(const Name : string) : TMenu;
var
	Comp : TComponent;
begin
	Comp := Application.MainForm.FindComponent(name);
	if (Comp <> nil) and (Comp is TMenu) then
		Result := TMenu(Comp)
	else
		Result := nil;
end;

class function IOTAUTils.GetEditPosition: IOTAEditPosition;
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

class function IOTAUTils.GetIDEEditControl(Form : TCustomForm) : TWinControl;
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

class function IOTAUTils.GetOpenedEditorFiles : TArray<string>;
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
				TDebugUtils.DebugMessage('IOTAUTils.GetOpenedEditorFiles FileName=' + editor.FileName);
				Result := Result + [editor.FileName];
			end;
		end;
	end;
end;

class function IOTAUTils.GetOpenedEditBuffers : TArray<string>;
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
				TDebugUtils.DebugMessage('IOTAUTils.GetOpenedEditBuffers FileName=' + buffer.FileName + ' ViewCount=' +
					buffer.EditViewCount.ToString);
				if buffer.EditViewCount > 0 then begin
					Result := Result + [buffer.FileName];
				end;
			end;
		end;
	end;
end;

class function IOTAUTils.GetProjectFiles : TArray<string>;
var
	fn : string;
	project : IOTAProject;
begin
	Result := [];
	project := GxOtaGetCurrentProject;
	for var i : integer := 0 to project.GetModuleCount - 1 do begin
		fn := project.GetModule(i).GetFileName;
		TDebugUtils.DebugMessage('IOTAUTils.GetProjectFiles FileName=' + fn);
		Result := Result + [fn]
	end;
end;

class function IOTAUTils.GetSettingFilePath : string;
var
	aIDEServices : IOTAServices;
begin
	aIDEServices := BorlandIDEServices as IOTAServices;
	Result := aIDEServices.GetLocalApplicationDataDirectory;
	if Result = '' then
		raise Exception.Create('GetSettingsFilePath: path not defined by IDEServices');
end;

class function IOTAUTils.GxOtaConvertColumnCharsToBytes(LineData : UTF8String; CharIndex : Integer; EndByte : Boolean) : Integer;
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

class function IOTAUTils.GxOtaFileOrModuleExists(const AFileName : string; UseBase : Boolean = False) : Boolean;
begin
	Result := FileExists(AFileName) or GxOtaIsFileOpen(AFileName, UseBase);
end;

class function IOTAUTils.GxOtaFocusCurrentIDEEditControl : Boolean;
var
	EditControl : TWinControl;
begin
	Result := False;
	EditControl := GxOtaGetCurrentIDEEditControl;
	TryFocusControl(EditControl)
end;

class function IOTAUTils.GxOtaGetActiveEditorText(Lines : TStringList; UseSelection : Boolean = True) : Boolean;
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

class function IOTAUTils.GxOtaGetActiveEditorTextAsString(var Text : string; UseSelection : Boolean = True) : Boolean;
var
	Lines : string;
begin
	Result := GxOtaGetActiveEditorTextAsUnicodeString(Lines, UseSelection);
	Text := Lines;
end;

class function IOTAUTils.GxOtaGetActiveEditorTextAsMultilineString(var Text : TMultiLineString; UseSelection : Boolean = True) : Boolean;
var
	Lines : string;
begin
	Result := GxOtaGetActiveEditorTextAsUnicodeString(Lines, UseSelection);
	Text := Lines;
end;

class function IOTAUTils.GxOtaGetActiveEditorTextAsUnicodeString(var Text : string; UseSelection : Boolean = True) : Boolean;
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

class function IOTAUTils.GxOtaGetBaseModuleFileName(const FileName : string) : string;
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

class function IOTAUTils.GxOtaGetCurrentIDEEditControl : TWinControl;
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

class function IOTAUTils.GxOtaGetCurrentModule : IOTAModule;
var
	ModuleServices : IOTAModuleServices;
begin
	ModuleServices := BorlandIDEServices as IOTAModuleServices;
	Assert(Assigned(ModuleServices));

	Result := ModuleServices.CurrentModule;
end;

class function IOTAUTils.GxOtaGetCurrentProject : IOTAProject;
begin
	if not GxOtaTryGetCurrentProject(Result) then
		Result := nil;
end;

class function IOTAUTils.GxOtaGetCurrentProjectFileName(NormalizeBdsProj : Boolean = False) : string;
begin
	Result := GxOtaGetProjectFileName(GxOtaGetCurrentProject, NormalizeBdsProj);
end;

class function IOTAUTils.GxOtaGetCurrentProjectName : string;
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

class function IOTAUTils.GxOtaGetCurrentSelection(IncludeTrailingCRLF : Boolean = True) : string;
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

class function IOTAUTils.GxOtaGetCurrentSourceEditor : IOTASourceEditor;
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

class function IOTAUTils.GxOtaGetCurrentSourceFile : string;
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

class function IOTAUTils.GxOtaGetEditActionsFromModule(Module : IOTAModule) : IOTAEditActions;
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

class function IOTAUTils.GxOtaGetEditorLine(View : IOTAEditView; LineNo : Integer) : UTF8String;
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

class function IOTAUTils.GxOtaGetEditorServices : IOTAEditorServices;
begin
	Result := (BorlandIDEServices as IOTAEditorServices);
	Assert(Assigned(Result), 'BorlandIDEServices is not assigned');
end;

class function IOTAUTils.GxOtaGetFileEditorForModule(Module : IOTAModule; index : Integer) : IOTAEditor;
begin
	Assert(Assigned(Module));
	Result := Module.GetModuleFileEditor(index);
end;

class function IOTAUTils.GxOtaGetFormEditorFromModule(const Module : IOTAModule) : IOTAFormEditor;
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

class function IOTAUTils.GxOtaGetModule(const FileName : string) : IOTAModule;
var
	ModuleServices : IOTAModuleServices;
begin
	ModuleServices := BorlandIDEServices as IOTAModuleServices;
	Assert(Assigned(ModuleServices));

	Result := ModuleServices.FindModule(FileName);
end;

class function IOTAUTils.GxOtaGetOpenModuleCount : Integer;
var
	ModuleServices : IOTAModuleServices;
begin
	ModuleServices := BorlandIDEServices as IOTAModuleServices;
	Assert(Assigned(ModuleServices));
	Result := ModuleServices.ModuleCount;
end;

class function IOTAUTils.GxOtaGetProjectFileName(Project : IOTAProject; NormalizeBdsProj : Boolean = False) : string;

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

class function IOTAUTils.GxOtaGetProjectGroup : IOTAProjectGroup;
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

class function IOTAUTils.GxOtaGetProjectPersonality(Project : IOTAProject) : string;
begin
	Result := '';
	if Assigned(Project) then begin
		Result := Project.Personality;
	end;
end;

class function IOTAUTils.GxOtaGetSourceEditorFromModule(Module : IOTAModule; const FileName : string = '') : IOTASourceEditor;
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

class function IOTAUTils.GxOtaGetTopMostEditBuffer : IOTAEditBuffer;
begin
	Result := GxOTAGetEditorServices.TopBuffer;
end;

class function IOTAUTils.GxOtaGetTopMostEditView(SourceEditor : IOTASourceEditor) : IOTAEditView;
begin
	if SourceEditor = nil then
		SourceEditor := GxOtaGetCurrentSourceEditor;
	if Assigned(SourceEditor) and (SourceEditor.EditViewCount > 0) then
		Result := SourceEditor.EditViews[0]
	else
		Result := nil;
end;

class function IOTAUTils.GxOtaGetTopMostEditView : IOTAEditView;
begin
	Result := nil;
	// Bug: Delphi 5/6 crash when calling TopView with no files open
	if GxOtaGetOpenModuleCount = 0 then
		Exit;
	Result := GxOTAGetEditorServices.TopView;
end;

class procedure IOTAUTils.GxOtaGoToFileLineColumn(const FileName : string; Line : Integer; StartColumn : Integer = 0;
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

class function IOTAUTils.GxOtaIsFileOpen(const AFileName : string; UseBase : Boolean = False) : Boolean;
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
	FileName := AFileName;
	if UseBase then
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

class procedure IOTAUTils.GxOtaLoadSourceEditorToUnicodeStrings(SourceEditor : IOTASourceEditor; Data : TStringList);
var
	MemStream : TMemoryStream;
begin
	Data.Clear;
	if not Assigned(SourceEditor) then
		raise Exception.Create('No source editor in GxOtaLoadSourceEditorToUnicodeStrings');
	// TODO: Check stream format for forms as text (Ansi with escaped unicode, or UTF-8) in Delphi 2007/2009
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
		if (Data.Count = 0) and (MemStream.Size > 0) then
			raise Exception.CreateFmt('%s.LoadFromFile failed to read %s.', [Data.ClassParent.ClassName, SourceEditor.FileName]);
		{$ELSE}
		SynUnicode.LoadFromStream(Data, MemStream, seUTF8);
		{$ENDIF}
	finally
		FreeAndNil(MemStream);
	end;
end;

class function IOTAUTils.GxOtaMakeSourceVisible(const FileName : string) : Boolean;
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

	if not(GxOtaIsFileOpen(BaseFileName) or GxOtaIsFileOpen(FileName)) then
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

class function IOTAUTils.GxOtaModuleIsShowingFormSource(Module : IOTAModule) : Boolean;
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

class function IOTAUTils.GxOtaOpenFile(const FileName : string) : Boolean;
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

class function IOTAUTils.GxOtaProjectIsEitherDelphi(Project : IOTAProject) : Boolean;
begin
	Result := MatchStr(GxOtaGetProjectPersonality(Project), [sDelphiPersonality, sDelphiDotNetPersonality]);
end;

class procedure IOTAUTils.GxOtaSaveReaderToStream(EditReader : IOTAEditReader; Stream : TStream; TrailingNull : Boolean = True);
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
		Stream.Write(NULL_CHAR, SizeOf(NULL_CHAR)); // The source parsers need this
end;

class function IOTAUTils.GxOtaTryGetCurrentProject(out _Project : IOTAProject) : Boolean;
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

class function IOTAUTils.GxOtaTryGetCurrentSourceEditor(out SourceEditor : IOTASourceEditor) : boolean;
begin
	SourceEditor := GxOtaGetCurrentSourceEditor;
	Result := Assigned(SourceEditor);
end;

class function IOTAUTils.GxOtaTryGetTopMostEditView(out EditView : IOTAEditView) : boolean;
begin
	EditView := GxOtaGetTopMostEditView;
	Result := Assigned(EditView);
end;

class function IOTAUTils.IDEEditorStringToString(const S : string) : string;
begin
	Result := S;
end;

class function IOTAUTils.IsBdsproj(const FileName : string) : Boolean;
begin
	Result := FileMatchesExtension(FileName, '.bdsproj');
end;

class function IOTAUTils.IsBdsprojOrDproj(const FileName : string) : Boolean;
begin
	Result := IsBdsproj(FileName) or IsDproj(FileName);
end;

class function IOTAUTils.IsBpr(const FileName : string) : Boolean;
begin
	Result := FileMatchesExtension(FileName, '.bpr');
end;

class function IOTAUTils.IsCharLineEnding(C : Char) : Boolean;
begin
	Result := CharInSet(C, [LF, CR]);
end;

class function IOTAUTils.IsDpr(const FileName : string) : Boolean;
begin
	Result := FileMatchesExtension(FileName, '.dpr');
end;

class function IOTAUTils.IsDproj(const FileName : string) : Boolean;
begin
	Result := FileMatchesExtension(FileName, '.dproj');
end;

class function IOTAUTils.IsForm(const FileName : string) : Boolean;
begin
	Result := FileMatchesExtensions(FileName, ['.dfm', '.xfm', '.nfm', '.fmx']);
end;

class function IOTAUTils.IsPackage(const FileName : string) : Boolean;
begin
	Result := FileMatchesExtensions(FileName, ['.dpk', '.dpkw', '.bpk']);
end;

class function IOTAUTils.IsProjectSource(const FileName : string) : Boolean;
begin
	Result := IsDpr(FileName) or IsBpr(FileName) or IsPackage(FileName);
end;

class function IOTAUTils.IsStandAlone : Boolean;
begin
	Result := (BorlandIDEServices = nil);
end;

class procedure IOTAUTils.RemoveLastEOL(var S : string);
var
	CurrLen : Integer;
	EOLSize : Integer;
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

class function IOTAUTils.StrCharAt(const S : string; Pos : Integer) : Char;
begin
	if (Pos >= 1) and (Pos <= Length(S)) then
		Result := S[Pos]
	else
		Result := #0;
end;

class function IOTAUTils.TryFocusControl(Control : TWinControl) : Boolean;
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
{$ELSE}
interface
implementation
{$ENDIF}
end.
