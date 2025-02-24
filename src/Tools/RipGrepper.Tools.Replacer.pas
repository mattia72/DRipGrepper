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
	System.Generics.Defaults;

type

	EReplaceMode = (rmUseRegex, rmIgnoreCase);
	TReplaceModes = set of EReplaceMode;

	TReplaceData = record
		Row : integer;
		Col : integer;
		Line : string;
		class function New(const _row : Integer; const _col : Integer; const _line : string) : TReplaceData; static;
		class operator Initialize(out Dest : TReplaceData);
	end;

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
			procedure AddUnique(const fileName : string; const row : integer; const col : integer; const replaceLine : string);
			function TryGet(const fileName : string; const row : integer; const col : integer; var replaceLine : string) : boolean;
			function Contains(fileName : string; row, col : integer) : Boolean;
			function GetCounters() : TFileReplaceCounters;
			function Update(const fileName : string; const row, col : integer; const line : string) : Boolean;
			function Remove(const fileName : string; const row, col : integer; const line : string) : Boolean;
			procedure Sort;
	end;

	TReplaceHelper = class
		public
			class procedure ReplaceLineInFile(const _fileName : string; const _row : Integer; const _col : Integer;
				const _replaceLine : string; const _createBackup : Boolean = True);
			class procedure ReplaceLineInFiles(_list : TReplaceList; const _createBackup : Boolean = True);
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

	System.RegularExpressions;

class procedure TReplaceHelper.ReplaceLineInFile(const _fileName : string; const _row : Integer; const _col : Integer;
	const _replaceLine : string; const _createBackup : Boolean = True);
var
	list : TReplaceList;
begin
	list := TReplaceList.Create;
	try
		list.AddUnique(_fileName, _row, _col, _replaceLine);
		TReplaceHelper.ReplaceLineInFiles(list);
	finally
		list.Free;
	end;
end;

class procedure TReplaceHelper.ReplaceLineInFiles(_list : TReplaceList; const _createBackup : Boolean = True);
var
	fileLines : TEncodedStringList;
	context : IReplaceContext;
	linePostFix : string;
	linePrefix : string;
	prevRow : Integer;
begin
	_list.Sort(); // sort rows and cols

	context := TReplaceContext.Create();
	for var fileName in _list.Items.Keys do begin
		if _createBackup then begin
			TFileUtils.CreateBackup(fileName);
		end;
		fileLines := TEncodedStringList.Create;
		try
			context.GetFileLines(fileName, fileLines);
            prevRow := -1;
			for var rd : TReplaceData in _list.Items[fileName] do begin
				if (rd.Row >= 0) and (rd.Row < fileLines.Count) then begin
					if prevRow = rd.Row then begin
						linePrefix := fileLines[rd.Row - 1].Substring(0, rd.Col - 1);
						linePostFix := rd.Line.Substring(rd.Col - 1);
						fileLines[rd.Row - 1] := linePrefix + linePostFix;
					end else begin
						fileLines[rd.Row - 1] := rd.Line;
					end;
				end;
				prevRow := rd.Row;
			end;
			context.WriteFileLines(fileName, fileLines);
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
	prefixStr := _input.Substring(0, _fromCol - 1);
	postfixStr := _input.Substring(_fromCol - 1);

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
end;

class function TReplaceData.New(const _row : Integer; const _col : Integer; const _line : string) : TReplaceData;
begin
	Result.Row := _row;
	Result.Col := _col;
	Result.Line := _line;
end;

class operator TReplaceData.Initialize(out Dest : TReplaceData);
begin
	Dest.Row := -1;
	Dest.Col := -1;
	Dest.Line := '';
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

procedure TReplaceList.AddUnique(const fileName : string; const row : integer; const col : integer; const replaceLine : string);
var
	replaceList : TArrayEx<TReplaceData>;
begin
	if Items.TryGetValue(fileName, replaceList) then begin
		if not replaceList.Contains(TReplaceData.New(row, col, replaceLine), FComparer)
		{ } then begin
			replaceList.Add(TReplaceData.New(row, col, replaceLine));
		end;
	end else begin
		replaceList := [TReplaceData.New(row, col, replaceLine)];
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
		idx := replaceList.IndexOf(TReplaceData.New(row, col, ''), FComparer);
		if idx >= 0 then begin
			replaceLine := replaceList[idx].Line;
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
		Result := replaceList.Contains(TReplaceData.New(row, col, ''), FComparer);
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
		idx := replaceList.IndexOf(TReplaceData.New(row, col, ''), FComparer);
		if idx >= 0 then begin
			rd.Row := row;
			rd.Line := line;
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
		idx := replaceList.IndexOf(TReplaceData.New(row, col, ''), FComparer);
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
