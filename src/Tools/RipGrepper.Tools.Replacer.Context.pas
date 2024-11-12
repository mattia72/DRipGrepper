unit RipGrepper.Tools.Replacer.Context;

interface

uses
	System.Classes,
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.EncodedStringList;

type
	TReplaceContext = class(TInterfacedObject, IReplaceContext)
		{$IFNDEF STANDALONE}
		private
			procedure GetFileLinesFromIDE;
			procedure WriteFileLinesInIDE;
			{$ENDIF}

		public
			procedure GetFileLines(_file : string; _list : TEncodedStringList);
			procedure WriteFileLines(_file : string; _list : TEncodedStringList);
	end;

resourcestring
	MSG_FILEChangedAbort = '%s' + sLineBreak + 'has changed since it was searched.  Replacement aborted.' + sLineBreak + 'Expected: %s' +
		sLineBreak + 'Found: %s';
	SUnableToOpen = 'Unable to open ';
	SNoOpenForms = 'Replacing strings in open forms is not possible. Please close the form first.';
	MSG_FILESkipped = 'The following file will be skipped:';

implementation

{ TReplaceContext }

procedure TReplaceContext.GetFileLines(_file : string; _list : TEncodedStringList);
begin
	{$IFDEF STANDALONE}
	_list.LoadFromFile(_file);
	{$ELSE}
	GetFileLinesFromIDE();
	{$ENDIF}
end;

procedure TReplaceContext.WriteFileLines(_file : string; _list : TEncodedStringList);
begin
	{$IFDEF STANDALONE}
	_list.SaveToFile(_file);
	{$ELSE}
	WriteFileLinesInIDE();
	{$ENDIF}
end;

{$IFNDEF STANDALONE}

procedure TReplaceContext.GetFileLinesFromIDE;
begin
	if IOTAUtils.then begin if IOTAUtils.IsForm(MatchFile) then
		raise ESkipFileReplaceException.Create(SNoOpenForms);
	Module := GxOtaGetModule(GxOtaGetBaseModuleFileName(MatchFile));
	if not Assigned(Module) then
		raise Exception.Create(SUnableToOpen + MatchFile);
	SourceEditor := GxOtaGetSourceEditorFromModule(Module, MatchFile);
	if not Assigned(SourceEditor) then
		raise Exception.Create(SUnableToOpen + MatchFile);
	GxOtaLoadFileToUnicodeStrings(SourceEditor.FileName, TempFile, WasBinary);
end
else
	GxOtaLoadFileToUnicodeStrings(MatchFile, TempFile, WasBinary);
end;

procedure TReplaceContext.WriteFileLinesInIDE;
var
	FormFile : TFileStream;
	FormSource : TStringStream;
	Module : IOTAModule;
	EditWriter : IOTAEditWriter;
	SourceEditor : IOTASourceEditor;
begin
	if InMemory then begin
		EditWriter := GxOtaGetEditWriterForSourceEditor(SourceEditor);
		EditWriter.DeleteTo(MaxInt);
		// RemoveLastEOL is necessary because TStringList.Text adds an extra CRLF on the end
		EditWriter.Insert(PAnsiChar(ConvertToIDEEditorString(RemoveTrailingEOL(TempFile.Text))));
		EditWriter := nil;
		Module := nil;
		SourceEditor := nil;
	end else begin
		if IsForm(MatchFile) and WasBinary then begin
			FormFile := nil;
			FormSource := TStringStream.Create(TempFile.Text);
			try
				FormSource.Seek(0, soFromBeginning);
				FormFile := TFileStream.Create(MatchFile, fmOpenWrite or fmShareDenyWrite);
				FormFile.Seek(0, soFromBeginning);
				ObjectTextToResource(FormSource, FormFile);
				FormFile.Size;
			finally
				FreeAndNil(FormSource);
				FreeAndNil(FormFile);
			end;
		end
		else
			TempFile.SaveToFile(MatchFile);
	end;
end;
{$ENDIF}

end.
