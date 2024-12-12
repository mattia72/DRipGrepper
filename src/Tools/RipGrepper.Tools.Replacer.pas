unit RipGrepper.Tools.Replacer;

interface

uses
	System.Classes,
	System.IniFiles,
	RipGrepper.Tools.FileUtils,
	RegExprReplaceUnit,
	System.SysUtils,
	ArrayEx, System.Generics.Collections;

type
	TReplaceData = record
		Row : integer;
		Line : string;
		class function New(const _row : Integer; const _line : string) : TReplaceData; static;
	end;

	TReplaceList = class
		Items : TDictionary<string, TArrayEx<TReplaceData>>;

		public
			constructor Create;
			destructor Destroy; override;
			procedure AddUnique(const fileName : string; const row : integer; const replaceLine : string);
			function TryGet(const fileName : string; const row : integer; var replaceLine : string) : boolean;
			function Contains(fileName : string; row : integer) : Boolean;
			function Update(const fileName : string; const row : integer; const line : string) : Boolean;
			function Remove(const fileName : string; const row : integer; const line : string) : Boolean;
	end;

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
	RipGrepper.Common.EncodedStringList, System.Generics.Defaults;

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

class function TReplaceData.New(const _row : Integer; const _line : string) : TReplaceData;
begin
	Result.Row := _row;
	Result.Line := _line;
end;

{ TReplaceList }

constructor TReplaceList.Create;
begin
	inherited;
	Items := TDictionary < string, TArrayEx < TReplaceData >>.Create;
end;

destructor TReplaceList.Destroy;
begin
	Items.Free;
	inherited;
end;

procedure TReplaceList.AddUnique(const fileName : string; const row : integer; const replaceLine : string);
var
	replaceList : TArrayEx<TReplaceData>;
begin
	if Items.TryGetValue(fileName, replaceList) then begin
		if not replaceList.Contains(TReplaceData.New(row, replaceLine), TComparer<TReplaceData>.Construct(
			function(const Left, Right : TReplaceData) : Integer
			begin
				Result := TComparer<integer>.Default.Compare(Left.Row, Right.Row);
			end))
		{ } then begin
			replaceList.Add(TReplaceData.New(row, replaceLine));
		end;
	end else begin
		replaceList := [TReplaceData.New(row, replaceLine)];
	end;
	Items.AddOrSetValue(fileName, replaceList);
end;

function TReplaceList.TryGet(const fileName : string; const row : integer; var replaceLine : string) : boolean;
var
	replaceList : TArrayEx<TReplaceData>;
begin
	Result := False;
	if Items.TryGetValue(fileName, replaceList) then begin
		var
		idx := replaceList.IndexOf(TReplaceData.New(row, ''), TComparer<TReplaceData>.Construct(
			function(const Left, Right : TReplaceData) : Integer
			begin
				Result := TComparer<integer>.Default.Compare(Left.Row, Right.Row);
			end));
		if idx >= 0 then begin
			replaceLine := replaceList[idx].Line;
			Result := True;
		end;
	end;
end;

function TReplaceList.Contains(fileName : string; row : integer) : Boolean;
var
	replaceList : TArrayEx<TReplaceData>;
begin
	Result := False;
	if Items.TryGetValue(fileName, replaceList) then begin
		Result := replaceList.Contains(TReplaceData.New(row, ''), TComparer<TReplaceData>.Construct(
			function(const Left, Right : TReplaceData) : Integer
			begin
				Result := TComparer<integer>.Default.Compare(Left.Row, Right.Row);
			end));
	end;

end;

function TReplaceList.Update(const fileName : string; const row : integer; const line : string) : Boolean;
var
	replaceList : TArrayEx<TReplaceData>;
	rd : TReplaceData;
begin
	Result := False;
	if Items.TryGetValue(fileName, replaceList) then begin
		var
		idx := replaceList.IndexOf(TReplaceData.New(row, ''), TComparer<TReplaceData>.Construct(
			function(const Left, Right : TReplaceData) : Integer
			begin
				Result := TComparer<integer>.Default.Compare(Left.Row, Right.Row);
			end));
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

function TReplaceList.Remove(const fileName : string; const row : integer; const line : string) : Boolean;
var
	replaceList : TArrayEx<TReplaceData>;
begin
	Result := False;
	if Items.TryGetValue(fileName, replaceList) then begin
		var
		idx := replaceList.IndexOf(TReplaceData.New(row, ''), TComparer<TReplaceData>.Construct(
			function(const Left, Right : TReplaceData) : Integer
			begin
				Result := TComparer<integer>.Default.Compare(Left.Row, Right.Row);
			end));
		if idx >= 0 then begin
			replaceList.Delete(idx);
			Result := True;
		end;
	end;
end;

end.
