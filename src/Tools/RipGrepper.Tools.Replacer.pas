unit RipGrepper.Tools.Replacer;

interface

uses
	System.Classes,
	System.IniFiles,
	RipGrepper.Tools.FileUtils,
	// RegExprReplaceUnit,
	System.SysUtils,
	ArrayEx,
	System.Generics.Collections,
	System.Generics.Defaults,
	Spring,
	RipGrepper.Common.SimpleTypes;

type
	TReplaceData = record
		Row : integer;
		Col : integer;
		ReplacedLine : string;
		OrigLine : TNullableString;
		class function New(const _row, _col : Integer; const _origLine, _replacedLine : string) : TReplaceData; static;
		class operator Initialize(out Dest : TReplaceData);
	end;

	TFailedReplaceDataItem = TPair<string, TReplaceData>;
	TFailedReplaceData = TArrayEx<TFailedReplaceDataItem>;

	TFilePathReplaceData = TDictionary<string, TArrayEx<TReplaceData>>;

	TFileReplaceCounters = record
		FileCount : integer;
		LineCount : Integer;
		ReplaceCount : integer;
	end;

	TReplaceList = class
		Items : TFilePathReplaceData;

		private
			FComparer : IComparer<TReplaceData>;

		public
			constructor Create;
			destructor Destroy; override;
			procedure AddUnique(const fileName : string; const row, col : integer; const origLine, replaceLine : string);
			function TryGet(const fileName : string; const row : integer; const col : integer; var replaceLine : string) : boolean;
			function Contains(fileName : string; row, col : integer) : Boolean;
			function GetCounters() : TFileReplaceCounters;
			function Update(const fileName : string; const row, col : integer; const line : string) : Boolean;
			function Remove(const fileName : string; const row, col : integer; const line : string) : Boolean;
			procedure Sort;
	end;

	TReplaceHelper = class
		private
			class function GetLengthDiffOfOrigAndChanged(var actLine, origLine : string) : Integer;

		public
			class function ReplaceLineInFile(const _fileName : string; const _row, _col : Integer; const _origLine, _replaceLine : string;
				const _createBackup : Boolean = True) : Boolean;
			class procedure ReplaceLineInFiles(_list : TReplaceList; { } var _failed : TFailedReplaceData;
				{ } const _createBackup : Boolean = True);
			class function ReplaceString(const _input, _pattern, _replacement : string; const _fromCol : Integer;
				const _mode : TReplaceModes) : string;
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
	RipGrepper.Common.EncodedStringList,

	System.RegularExpressions,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants;

class function TReplaceHelper.GetLengthDiffOfOrigAndChanged(var actLine, origLine : string) : Integer;
var
	actLength : Integer;
	colActLineEnd : Integer;
	colLineStart : Integer;
	colOrigLineEnd : Integer;
	origLength : Integer;
begin
	colActLineEnd := Length(actLine);
	colOrigLineEnd := Length(origLine);
	while actLine[colActLineEnd] = origLine[colOrigLineEnd] do begin
		Dec(colActLineEnd);
		Dec(colOrigLineEnd);
	end;

	colLineStart := 1;
	while actLine[colLineStart] = origLine[colLineStart] do begin
		Inc(colLineStart);
	end;

	origLength := colOrigLineEnd - colLineStart;
	actLength := colActLineEnd - colLineStart;
	Result := actLength - origLength;
end;

class function TReplaceHelper.ReplaceLineInFile(const _fileName : string; const _row, _col : Integer;
	const _origLine, _replaceLine : string; const _createBackup : Boolean = True) : Boolean;
var
	failedReplace : TFailedReplaceData;
	list : TReplaceList;
begin
	list := TReplaceList.Create;
	try
		list.AddUnique(_fileName, _row, _col, _origLine, _replaceLine);
		TReplaceHelper.ReplaceLineInFiles(list, failedReplace);
	finally
		list.Free;
		Result := (failedReplace.Count = 0);
	end;
end;

class procedure TReplaceHelper.ReplaceLineInFiles(_list : TReplaceList;
	{ } var _failed : TFailedReplaceData;
	{ } const _createBackup : Boolean = True);
var
	actLine, origLine : string;
	bFileMismatch : Boolean;
	fileLines : TEncodedStringList;
	context : IReplaceContext;
	diff : Integer;
	failedItem : TPair<string, TReplaceData>;
	fileLine : string;
	iCheckedRow : Integer;
	lineEnd : string;
	lineStart : string;
	prevRow : TReplaceData;
	replacedLine : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TReplaceHelper.ReplaceLineInFiles');

	_list.Sort(); // sort rows and cols

	context := TReplaceContext.Create();
	for var fileName in _list.Items.Keys do begin
		fileLines := TEncodedStringList.Create;
		try
			context.GetFileLines(fileName, fileLines);
			prevRow := default (TReplaceData);
			bFileMismatch := False;
			iCheckedRow := -1;
			for var rd : TReplaceData in _list.Items[fileName] do begin
				if (rd.Row >= 0) and (rd.Row <= fileLines.Count) then begin

					fileLine := fileLines[rd.Row - 1];
					if (iCheckedRow <> rd.Row) and (rd.OrigLine <> fileLine) then begin
						dbgMsg.Msg('orig:' + rd.OrigLine);
						dbgMsg.Msg('act:' + fileLine);
						failedItem := TPair<string, TReplaceData>.Create(fileName, rd);
						_failed.Add(failedItem);
						bFileMismatch := True;
						break;
					end else begin
						iCheckedRow := rd.Row;
					end;

					if prevRow.Row = rd.Row then begin
						actLine := fileLine;
						// Get difference of orig and changed text length
						diff := GetLengthDiffOfOrigAndChanged(actLine, origLine);
						lineStart := actLine.Substring(0, rd.Col - 1 + diff);
						lineEnd := rd.ReplacedLine.Substring(rd.Col - 1);
						replacedLine := lineStart + lineEnd;
					end else begin
						origLine := fileLine;
						replacedLine := rd.ReplacedLine;
					end;
					fileLines[rd.Row - 1] := replacedLine;
				end;
				prevRow := rd;
			end;

			if not bFileMismatch then begin
				if _createBackup then begin
					TFileUtils.CreateBackup(fileName);
				end;

				context.WriteFileLines(fileName, fileLines);
			end;
		finally
			fileLines.Free;
		end;
	end;
end;

class function TReplaceHelper.ReplaceString(const _input, _pattern, _replacement : string; const _fromCol : Integer;
	const _mode : TReplaceModes) : string;
var
	postfixStr : string;
	prefixStr : string;
	op : TRegexOptions;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TReplaceHelper.ReplaceString', True);

	prefixStr := _input.Substring(0, _fromCol - 1);
	postfixStr := _input.Substring(_fromCol - 1);

	try
		if rmUseRegex in _mode then begin
			op := [];
			if rmIgnoreCase in _mode then begin
				op := [roIgnoreCase];
			end;
			Result := prefixStr + TRegEx.Replace(postfixStr, _pattern, _replacement, op);
		end else if rmIgnoreCase in _mode then begin
			Result := prefixStr + System.SysUtils.StringReplace(postfixStr, _pattern, _replacement, [rfIgnoreCase]);
		end else begin
			Result := prefixStr + System.SysUtils.StringReplace(postfixStr, _pattern, _replacement, []);
		end;
	except
		on E : Exception do
			dbgMsg.ErrorMsgFmt(E.Message + CRLF + 'in:%s, pattern:%s, repl: %s', [postfixStr, _pattern, _replacement]);
	end;
end;

class function TReplaceData.New(const _row, _col : Integer; const _origLine, _replacedLine : string) : TReplaceData;
begin
	Result.Row := _row;
	Result.Col := _col;
	Result.OrigLine := _origLine;
	Result.ReplacedLine := _replacedLine;
end;

class operator TReplaceData.Initialize(out Dest : TReplaceData);
begin
	Dest.Row := -1;
	Dest.Col := -1;
	Dest.OrigLine := nil;
	Dest.ReplacedLine := '';
end;

{ TReplaceList }

constructor TReplaceList.Create;
begin
	inherited;
	Items := TDictionary < string, TArrayEx < TReplaceData >>.Create;
	FComparer := TComparer<TReplaceData>.Construct(
		function(const Left, Right : TReplaceData) : Integer
		begin
			Result := TComparer<integer>.Default.Compare(Left.Row, Right.Row);
			if (0 = Result) then begin
				Result := TComparer<integer>.Default.Compare(Left.Col, Right.Col);
			end;
		end);
end;

destructor TReplaceList.Destroy;
begin
	Items.Free;
	inherited;
end;

procedure TReplaceList.AddUnique(const fileName : string; const row, col : integer; const origLine, replaceLine : string);
var
	replaceList : TArrayEx<TReplaceData>;
begin
	if Items.TryGetValue(fileName, replaceList) then begin
		if not replaceList.Contains(TReplaceData.New(row, col, origLine, replaceLine), FComparer)
		{ } then begin
			replaceList.Add(TReplaceData.New(row, col, origLine, replaceLine));
		end;
	end else begin
		replaceList := [TReplaceData.New(row, col, origLine, replaceLine)];
	end;
	Items.AddOrSetValue(fileName, replaceList);
end;

function TReplaceList.TryGet(const fileName : string; const row : integer; const col : integer; var replaceLine : string) : boolean;
var
	replaceList : TArrayEx<TReplaceData>;
begin
	Result := False;
	if Items.TryGetValue(fileName, replaceList) then begin
		var
		idx := replaceList.IndexOf(TReplaceData.New(row, col, '', ''), FComparer);
		if idx >= 0 then begin
			replaceLine := replaceList[idx].ReplacedLine;
			Result := True;
		end;
	end;
end;

function TReplaceList.Contains(fileName : string; row, col : integer) : Boolean;
var
	replaceList : TArrayEx<TReplaceData>;
begin
	Result := False;
	if Items.TryGetValue(fileName, replaceList) then begin
		Result := replaceList.Contains(TReplaceData.New(row, col, '', ''), FComparer);
	end;

end;

function TReplaceList.GetCounters() : TFileReplaceCounters;
var
	prevRow : Integer;
begin
	Result.FileCount := Items.Keys.Count;
	Result.ReplaceCount := 0;
	Result.LineCount := 0;

	for var fileName in Items.Keys do begin
		prevRow := -1;
		for var rd : TReplaceData in Items[fileName] do begin
			Inc(Result.ReplaceCount);
			if prevRow <> rd.Row then begin
				Inc(Result.LineCount);
			end;
			prevRow := rd.Row;
		end;
	end;
end;

function TReplaceList.Update(const fileName : string; const row, col : integer; const line : string) : Boolean;
var
	replaceList : TArrayEx<TReplaceData>;
	rd : TReplaceData;
begin
	Result := False;
	if Items.TryGetValue(fileName, replaceList) then begin
		var
		idx := replaceList.IndexOf(TReplaceData.New(row, col, '', ''), FComparer);
		if idx >= 0 then begin
			rd.Row := row;
			rd.ReplacedLine := line;
			replaceList.Delete(idx);
			replaceList.Add(rd);
			Items.AddOrSetValue(fileName, replaceList);
			Result := True;
		end;
	end;
end;

function TReplaceList.Remove(const fileName : string; const row, col : integer; const line : string) : Boolean;
var
	replaceList : TArrayEx<TReplaceData>;
begin
	Result := False;
	if Items.TryGetValue(fileName, replaceList) then begin
		var
		idx := replaceList.IndexOf(TReplaceData.New(row, col, '', ''), FComparer);
		if idx >= 0 then begin
			replaceList.Delete(idx);
			Result := True;
		end;
	end;
end;

procedure TReplaceList.Sort;
begin
	for var pair in Items do begin
		pair.Value.Sort(FComparer);
	end;
end;

end.
