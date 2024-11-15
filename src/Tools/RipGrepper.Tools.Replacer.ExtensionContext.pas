unit RipGrepper.Tools.Replacer.ExtensionContext;

interface

{$IFDEF STANDALONE}
{$MESSAGE ERROR 'This unit should not included in STANDALONE version '}
{$ENDIF }

uses
	System.Classes,
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.EncodedStringList,
	RipGrepper.Common.Constants,
	ToolsAPI;

type
	TReplaceContext = class(TInterfacedObject, IReplaceContext)
		private
			Module : IOTAModule;
			SourceEditor : IOTASourceEditor;
			procedure GetFileLinesFromIDE(const _sFileName : string; _listFile : TEncodedStringList);
			procedure WriteFileLinesInIDE(const _sFileName : string; _listFile : TEncodedStringList);

		public
			// Returns the size of any EOL character(s) at a given position (0 if none)
			function EOLSizeAtPos(const S : string; Pos : Integer) : Integer;
			procedure GetFileLines(_file : string; _list : TEncodedStringList);
			function IsCharLineEnding(C : Char) : Boolean;
			// Remove the last EOL character from a string
			// EOL can be one or two characters long
			// Useful for processing strings read from memo controls
			procedure RemoveLastEOL(var S : string);
			function RemoveTrailingEOL(const S : string) : string;
			// Get character at some position or #0 for invalid positions
			function StrCharAt(const S : string; Pos : Integer) : Char;
			procedure WriteFileLines(_file : string; _list : TEncodedStringList);
	end;

resourcestring
	MSG_FILEChangedAbort = '%s' + sLineBreak + 'has changed since it was searched.  Replacement aborted.' + sLineBreak + 'Expected: %s' +
		sLineBreak + 'Found: %s';
	SUnableToOpen = 'Unable to open ';
	SNoOpenForms = 'Replacing strings in open forms is not possible. Please close the form first.';
	MSG_FILESkipped = 'The following file will be skipped:';

implementation

uses
	RipGrepper.Common.IOTAUtils,
	RipGrepper.Common.SimpleTypes,
	System.SysUtils;

// Returns the size of any EOL characters at a given position
// Supports the following EOL formats:
// #10#13 (?)
// #13#10 (PC / Win)
// #10    (Unix)
// #13    (Macintosh, Amiga)
// Note: Pos must be at the beginning of the EOL characters
function TReplaceContext.EOLSizeAtPos(const S : string; Pos : Integer) : Integer;
begin
	if IsCharLineEnding(StrCharAt(S, Pos)) then begin
		Result := 1;
		if (IsCharLineEnding(StrCharAt(S, Pos + 1)) and (StrCharAt(S, Pos) <> StrCharAt(S, Pos + 1))) then
			Inc(Result);
	end
	else
		Result := 0;
end;

{ TReplaceContext }

procedure TReplaceContext.GetFileLines(_file : string; _list : TEncodedStringList);
begin
	GetFileLinesFromIDE(_file, _list);
end;

procedure TReplaceContext.WriteFileLines(_file : string; _list : TEncodedStringList);
begin
	WriteFileLinesInIDE(_file, _list);
end;

procedure TReplaceContext.GetFileLinesFromIDE(const _sFileName : string; _listFile : TEncodedStringList);
begin
	Module := nil;
	SourceEditor := nil;
	if IOTAUtils.IsFileOpen(_sFileName) then begin
		if IOTAUtils.IsForm(_sFileName) then
			raise ESkipFileReplaceException.Create(SNoOpenForms);
		Module := IOTAUtils.GxOtaGetModule(IOTAUtils.GxOtaGetBaseModuleFileName(_sFileName));
		if not Assigned(Module) then
			raise EFileOpenException.Create(SUnableToOpen + _sFileName);
		SourceEditor := IOTAUtils.GxOtaGetSourceEditorFromModule(Module, _sFileName);
		if not Assigned(SourceEditor) then
			raise EFileOpenException.Create(SUnableToOpen + _sFileName);
		_listFile.LoadFromFile(SourceEditor.FileName);
	end else begin
		_listFile.LoadFromFile(_sFileName);
	end;
end;

function TReplaceContext.IsCharLineEnding(C : Char) : Boolean;
begin
	Result := CharInSet(C, [LF, CR]);
end;

procedure TReplaceContext.RemoveLastEOL(var S : string);
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

function TReplaceContext.RemoveTrailingEOL(const S : string) : string;
begin
	Result := S;
	RemoveLastEOL(Result);
end;

function TReplaceContext.StrCharAt(const S : string; Pos : Integer) : Char;
begin
	if (Pos >= 1) and (Pos <= Length(S)) then
		Result := S[Pos]
	else
		Result := #0;
end;

procedure TReplaceContext.WriteFileLinesInIDE(const _sFileName : string; _listFile : TEncodedStringList);
var
	FormFile : TFileStream;
	FormSource : TStringStream;
	Module : IOTAModule;
	EditWriter : IOTAEditWriter;
begin
	if Assigned(SourceEditor) then begin
		EditWriter := IOTAUtils.GxOtaGetEditWriterForSourceEditor(SourceEditor);
		EditWriter.DeleteTo(MaxInt);
		// RemoveLastEOL is necessary because TStringList.Text adds an extra CRLF on the end
		EditWriter.Insert(PAnsiChar(UTF8Encode(RemoveTrailingEOL(_listFile.Text))));
		EditWriter := nil;
		Module := nil;
		SourceEditor := nil;
	end else begin
		if IOTAUtils.IsForm(_sFileName) and _listFile.IsBinary then begin
			FormFile := nil;
			FormSource := TStringStream.Create(_listFile.Text);
			try
				FormSource.Seek(0, soFromBeginning);
				FormFile := TFileStream.Create(_sFileName, fmOpenWrite or fmShareDenyWrite);
				FormFile.Seek(0, soFromBeginning);
				ObjectTextToResource(FormSource, FormFile);
				FormFile.Size;
			finally
				FreeAndNil(FormSource);
				FreeAndNil(FormFile);
			end;
		end
		else
			_listFile.SaveToFile(_sFileName);
	end;
end;

end.
