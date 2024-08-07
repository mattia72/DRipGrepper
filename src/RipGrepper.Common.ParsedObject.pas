unit RipGrepper.Common.ParsedObject;

interface

uses
	ArrayEx,
	System.Generics.Collections,
	System.Generics.Defaults,
	System.Classes,
	RipGrepper.Common.Constants;

type
	TVSMatchData = record
		Row : integer;
		Col : integer;
		MatchLength : integer;
		LineText : string;

		public
			function IsEmpty : Boolean;
			class function New(_row, _col, _colEnd : Integer; _matchText : string) : TVSMatchData; static;
			class operator Initialize(out Dest : TVSMatchData);
	end;

	PVSMatchData = ^TVSMatchData;

	TVSFileNodeData = record
		FilePath : string;
		// Icon?
		MatchData : TVSMatchData;
		function GetText(const _bTrimLeft : Boolean; var _iSpaceCount, _iTabCount : Integer) : string;

		public
			class function New(_file : string; _row, _col : Integer; _textBefore, _matchText, _textAfter : string) : TVSFileNodeData;
				overload; static;
			class function New(_file : string; _row : Integer = -1; _col : Integer = -1; _matchText : string = '') : TVSFileNodeData;
				overload; static;
	end;

	PVSFileNodeData = ^TVSFileNodeData;

	// ---------
	TColumnData = record
		Title : string;
		Text : string;

		public
			class function New(const _Title : EColumnIndex; const _Text : string) : TColumnData; overload; static;
	end;

	{$M+}

	IParsedObject = interface
		['{5DDAF1CE-48EF-42C0-B87F-102E081B3D16}']
		function GetColumns : TArrayEx<TColumnData>;
		procedure SetColumns(const Value : TArrayEx<TColumnData>);
		property Columns : TArrayEx<TColumnData> read GetColumns write SetColumns;
	end;

	IParsedObjectRow = interface(IParsedObject)
		['{85536634-C591-43F1-B348-BC93E4E62942}']
		function GetErrorText : string;
		function GetColumnText(const _idx:integer) : string;
		function GetIsError : Boolean;
		function GetParserType : TParserType;
		function GetRowNr : Integer;
		procedure SetErrorText(const Value : string);
		procedure SetIsError(const Value : Boolean);
		procedure SetParserType(const Value : TParserType);
		procedure SetRowNr(const Value : Integer);
		property ErrorText : string read GetErrorText write SetErrorText;
		property IsError : Boolean read GetIsError write SetIsError;
		property ParserType : TParserType read GetParserType write SetParserType;
		property RowNr : Integer read GetRowNr write SetRowNr;

	end;

type
	// {$DEFINE THREADSAFE_LIST} We don't need it

	{$IFDEF THREADSAFE_LIST}
	TThreadListType = TThreadList<IParsedObjectRow>;
	TListType = TList<IParsedObjectRow>;
	{$ELSE}
	TParsedObjectRow = class;
	TListType = TList<IParsedObjectRow>;
	{$ENDIF}

	IParsedObjectRowCollection = interface
		['{03AA4D67-689A-43A0-8D07-FAB44124E032}']
		function GetItems : TListType;
		property Items : TListType read GetItems;
		{$IFDEF THREADSAFE_LIST}
		procedure Unlock;
		{$ENDIF}
	end;

	IParsedObjectGroupRowCollection = interface(IParsedObjectRowCollection)
		['{154707F1-9ECB-4FAE-943D-249A9DB6FFAF}']
		function GetGroupId : Integer;
		procedure SetGroupId(const Value : Integer);
		property GroupId : Integer read GetGroupId write SetGroupId;
	end;

	{$M-}

	TParsedObjectRow = class(TInterfacedObject, IParsedObjectRow)

		private
			FErrorText : string;
			FIsError : Boolean;
			FColumns : TArrayEx<TColumnData>;
			FParserType : TParserType;
			FRowNr : Integer;
			function GetColumns : TArrayEx<TColumnData>;
			procedure SetColumns(const Value : TArrayEx<TColumnData>);
			function GetErrorText : string;
			function GetIsError : Boolean;
			function GetRowNr : Integer;
			procedure SetErrorText(const Value : string);
			procedure SetIsError(const Value : Boolean);
			procedure SetRowNr(const Value : Integer);
			function GetParserType : TParserType;
			procedure SetParserType(const Value : TParserType);

		public
			constructor Create(const _por : IParsedObjectRow; _parserType : TParserType); overload;
			destructor Destroy; override;
			procedure CopyTo(var _por : TParsedObjectRow);
			function GetColumnText(const _idx:integer): string;
			property Columns : TArrayEx<TColumnData> read GetColumns write SetColumns;
			property ErrorText : string read GetErrorText write SetErrorText;
			property IsError : Boolean read GetIsError write SetIsError;
			property RowNr : Integer read GetRowNr write SetRowNr;
			property ParserType : TParserType read GetParserType write SetParserType;
	end;

	TParsedObjectRowCollection = class(TNoRefCountObject, IParsedObjectRowCollection)
		private
			{$IFDEF THREADSAFE_LIST}
			FItems : TThreadListType;
			{$ELSE}
			FItems : TListType;
			{$ENDIF}
			function GetItems : TListType;
			// procedure SetItems(const Value : TListType);

		public
			constructor Create;
			destructor Destroy; override;
			property Items : TListType read GetItems; // write SetItems;
			{$IFDEF THREADSAFE_LIST}
			procedure Unlock;
			{$ENDIF}
	end;

	TParsedObjectGroupedRowCollection = class(TParsedObjectRowCollection, IParsedObjectGroupRowCollection)
		private
			FGroupId : Integer;
			function GetGroupId : Integer;
			procedure SetGroupId(const Value : Integer);

		public
			property GroupId : Integer read GetGroupId write SetGroupId;
	end;

	TParsedObjectGroupCollection = class
		Groups : TList<TParsedObjectGroupedRowCollection>;
	end;

implementation

uses
	System.SysUtils,
	RipGrepper.Helper.Types;

class function TColumnData.New(const _Title : EColumnIndex; const _Text : string) : TColumnData;
begin
	Result.Title := TDefaults.ColumnTitle[_Title];
	Result.Text := _Text;
end;

constructor TParsedObjectRow.Create(const _por : IParsedObjectRow; _parserType : TParserType);
begin
	inherited Create();
	ErrorText := _por.ErrorText;
	Columns := _por.Columns;
	RowNr := _por.RowNr;
	IsError := _por.IsError;
	ParserType := _parserType;
end;

destructor TParsedObjectRow.Destroy;
begin
	inherited;
end;

procedure TParsedObjectRow.CopyTo(var _por : TParsedObjectRow);
begin
	_por.ErrorText := ErrorText;
	_por.FColumns := Columns;
	_por.RowNr := RowNr;
	_por.IsError := IsError;
end;

function TParsedObjectRow.GetColumns : TArrayEx<TColumnData>;
begin
	Result := FColumns;
end;

function TParsedObjectRow.GetColumnText(const _idx:integer): string;
begin
	Result := '';
	if Columns.Count > _idx then begin
		Result := FColumns[_idx].Text;
    end;
end;

function TParsedObjectRow.GetErrorText : string;
begin
	Result := FErrorText;
end;

function TParsedObjectRow.GetIsError : Boolean;
begin
	Result := FIsError;
end;

function TParsedObjectRow.GetParserType : TParserType;
begin
	Result := FParserType;
end;

function TParsedObjectRow.GetRowNr : Integer;
begin
	Result := FRowNr;
end;

procedure TParsedObjectRow.SetColumns(const Value : TArrayEx<TColumnData>);
begin
	FColumns := Value;
end;

procedure TParsedObjectRow.SetErrorText(const Value : string);
begin
	FErrorText := Value;
end;

procedure TParsedObjectRow.SetIsError(const Value : Boolean);
begin
	FIsError := Value;
end;

procedure TParsedObjectRow.SetParserType(const Value : TParserType);
begin
	FParserType := Value;
end;

procedure TParsedObjectRow.SetRowNr(const Value : Integer);
begin
	FRowNr := Value;
end;

constructor TParsedObjectRowCollection.Create;
begin
	inherited;
	{$IFDEF THREADSAFE_LIST}
	FItems := TThreadListType.Create();
	{$ELSE}
	FItems := TListType.Create();
	{$ENDIF}
end;

destructor TParsedObjectRowCollection.Destroy;
begin
	FItems.Free;
	inherited;
end;

function TParsedObjectRowCollection.GetItems : TListType;
begin
	{$IFDEF THREADSAFE_LIST}
	Result := FItems.LockList;
	{$ELSE}
	Result := FItems;
	{$ENDIF}
end;

{$IFDEF THREADSAFE_LIST}

procedure TParsedObjectRowCollection.Unlock;
begin
	FItems.UnlockList;
end;
{$ENDIF}
// procedure TParsedObjectRowCollection.SetItems(const Value : TListType);
// begin
// FItems.LockList := Value;
// end;

function TParsedObjectGroupedRowCollection.GetGroupId : Integer;
begin
	Result := FGroupId;
end;

procedure TParsedObjectGroupedRowCollection.SetGroupId(const Value : Integer);
begin
	FGroupId := Value;
end;

function TVSFileNodeData.GetText(const _bTrimLeft : Boolean; var _iSpaceCount, _iTabCount : Integer) : string;
var
	sTrimmed : string;
begin
	_iSpaceCount := 0;
	_iTabCount := 0;
	if _bTrimLeft then begin
		sTrimmed := MatchData.LineText.TrimLeft();
		for var ch in MatchData.LineText.ToCharArray() do begin
			if ch = ' ' then begin
				PostInc(_iSpaceCount);
			end else if ch = #9 then begin
				PostInc(_iTabCount);
			end else begin
				break;
			end;
		end;
		Result := sTrimmed;
	end else begin
		Result := MatchData.LineText;
	end;

end;

class function TVSFileNodeData.New(_file : string; _row, _col : Integer; _textBefore, _matchText, _textAfter : string) : TVSFileNodeData;
var
	matchLength : integer;
	text : string;
begin
	Result.FilePath := _file;
	text := _textBefore + _matchText + _textAfter;
	matchLength := Length(_matchText);
	Result.MatchData := TVSMatchData.New(_row, _col, matchLength, text);
end;

class function TVSFileNodeData.New(_file : string; _row : Integer = -1; _col : Integer = -1; _matchText : string = '') : TVSFileNodeData;
begin
	Result.FilePath := _file;
	Result.MatchData := TVSMatchData.New(_row, _col, -1, _matchText);
end;

function TVSMatchData.IsEmpty : Boolean;
begin
	Result := (Row = -1) and (Col = -1) and (MatchLength = -1) and (LineText.IsEmpty);
end;

class function TVSMatchData.New(_row, _col, _colEnd : Integer; _matchText : string) : TVSMatchData;
begin
	Result.Row := _row;
	Result.Col := _col;
	Result.MatchLength := _colEnd;
	Result.LineText := _matchText;
end;

class operator TVSMatchData.Initialize(out Dest : TVSMatchData);
begin
	Dest.Row := -1;
	Dest.Col := -1;
	Dest.MatchLength := -1;
	Dest.LineText := '';
end;

end.
