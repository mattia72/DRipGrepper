unit RipGrepper.Tools.Replacer;

interface

uses
	System.Classes,
	System.IniFiles,
	RipGrepper.Tools.FileUtils,
	RegExprReplaceUnit,
	System.SysUtils;

type

	TReplaceHelper = class
		private
			class function ShowWarningBeforeSave(_list : TReplaceList) : Boolean;

		public
			class procedure ReplaceLineInFile(const _fileName : string; const _row : Integer; const _replaceLine : string;
				const _createBackup : Boolean = True);
			class procedure ReplaceLineInFiles(_list : TReplaceList; const _createBackup : Boolean = True);
	end;

implementation

uses
	RipGrepper.Helper.UI,
	System.UITypes,
	{$IFDEF STANDALONE}
	RipGrepper.Tools.Replacer.StandaloneContext,
	{$ELSE}
	RipGrepper.Tools.Replacer.ExtensionContext,
	{$ENDIF}
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.EncodedStringList;

class procedure TReplaceHelper.ReplaceLineInFile(const _fileName : string; const _row : Integer; const _replaceLine : string;
	const _createBackup : Boolean = True);
var
	list : TReplaceList;
begin
	list := TReplaceList.Create;
	try
		list.AddUnique(_fileName, _row, _replaceLine);
		TReplaceHelper.ReplaceLineInFiles(list);
	finally
		list.Free;
	end;
end;

class procedure TReplaceHelper.ReplaceLineInFiles(_list : TReplaceList; const _createBackup : Boolean = True);
var
	fileLines : TEncodedStringList;
	context : IReplaceContext;
begin
	if not ShowWarningBeforeSave(_list) then
		Exit;

	context := TReplaceContext.Create();
	for var fileName in _list.Items.Keys do begin
		if _createBackup then begin
			TFileUtils.CreateBackup(fileName);
		end;
		fileLines := TEncodedStringList.Create;
		try
			context.GetFileLines(fileName, fileLines);
			for var rd : TReplaceData in _list.Items[fileName] do begin
				if (rd.Row >= 0) and (rd.Row < fileLines.Count) then begin
					fileLines[rd.Row - 1] := rd.Line;
				end;
			end;
			context.WriteFileLines(fileName, fileLines);
		finally
			fileLines.Free;
		end;
	end;
end;

class function TReplaceHelper.ShowWarningBeforeSave(_list : TReplaceList) : Boolean;
var
	replaceCount : Integer;
begin
	replaceCount := 0;

	for var fileName in _list.Items.Keys do begin
		for var rd : TReplaceData in _list.Items[fileName] do begin
			Inc(replaceCount);
		end;
	end;
	Result := mrYes = TMsgBox.ShowQuestion(Format('Are you sure to change %d line(s) in %d file(s)?',
		[replaceCount, _list.Items.Keys.Count]));
end;

end.
