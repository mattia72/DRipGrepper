unit RipGrepper.Common.ParsedObject;

interface

uses
	ArrayEx,
	System.Generics.Collections,
	System.Generics.Defaults,
	System.Classes,
	RipGrepper.Common.Constants,
	RipGrepper.Common.SimpleTypes,
	Spring.Collections,
	RipGrepper.Common.Interfaces.StreamPersistable;

type

	TColumnData = record
		Title : string;
		Text : string;

		public
			class function New(const _idxTitle : EColumnIndex; const _Text : string) : TColumnData; overload; static;
	end;

	{$M+}

	IParsedObject = interface
		['{5DDAF1CE-48EF-42C0-B87F-102E081B3D16}']
		function GetColumnByTitle(const _title : string) : TColumnData;
		function GetColumns : TArrayEx<TColumnData>;
		procedure SetColumns(const Value : TArrayEx<TColumnData>);
		property Columns : TArrayEx<TColumnData> read GetColumns write SetColumns;
	end;

	IParsedObjectRow = interface(IParsedObject)
		['{85536634-C591-43F1-B348-BC93E4E62942}']
		function GetErrorText : string;
		function GetColumnText(const _idx : integer) : string;
		function GetIsError : Boolean;
		function GetIsStatsLine : Boolean;
		function GetParserType : TParserType;
		function GetRowNr : Integer;
		procedure SetErrorText(const Value : string);
		procedure SetIsError(const Value : Boolean);
		procedure SetIsStatsLine(const Value : Boolean);
		procedure SetParserType(const Value : TParserType);
		procedure SetRowNr(const Value : Integer);
		property ErrorText : string read GetErrorText write SetErrorText;
		property IsError : Boolean read GetIsError write SetIsError;
		property IsStatsLine : Boolean read GetIsStatsLine write SetIsStatsLine;
		property ParserType : TParserType read GetParserType write SetParserType;
		property RowNr : Integer read GetRowNr write SetRowNr;

	end;

type

	TParsedObjectRow = class;
	TListType = IList<IParsedObjectRow>;

	IParsedObjectRowCollection = interface
		['{03AA4D67-689A-43A0-8D07-FAB44124E032}']
		function GetItems : TListType;
		property Items : TListType read GetItems;
	end;

	IParsedObjectGroupRowCollection = interface(IParsedObjectRowCollection)
		['{154707F1-9ECB-4FAE-943D-249A9DB6FFAF}']
		function GetGroupId : Integer;
		procedure SetGroupId(const Value : Integer);
		property GroupId : Integer read GetGroupId write SetGroupId;
	end;

	{$M-}

	TParsedObjectRow = class(TInterfacedObject, IParsedObjectRow, IParsedObject)
		strict private
			FErrorText : string;
			FIsError : Boolean;
			FColumns : TArrayEx<TColumnData>;
			FIsStatsLine : Boolean;
			FParserType : TParserType;
			FRowNr : Integer;
			function GetColumns : TArrayEx<TColumnData>;
			procedure SetColumns(const Value : TArrayEx<TColumnData>);
			function GetErrorText : string;
			function GetIsStatsLine : Boolean;
			function GetIsError : Boolean;
			function GetRowNr : Integer;
			procedure SetErrorText(const Value : string);
			procedure SetIsError(const Value : Boolean);
			procedure SetRowNr(const Value : Integer);
			function GetParserType : TParserType;
			procedure SetIsStatsLine(const Value : Boolean);
			procedure SetParserType(const Value : TParserType);

		public
			constructor Create(const _por : IParsedObjectRow; _parserType : TParserType); overload;
			destructor Destroy; override;
			procedure CopyTo(var _por : TParsedObjectRow);
			function GetColumnByTitle(const _title : string) : TColumnData;
			function GetColumnText(const _idx : integer) : string;
			property Columns : TArrayEx<TColumnData> read GetColumns write SetColumns;
			property ErrorText : string read GetErrorText write SetErrorText;
			property IsStatsLine : Boolean read GetIsStatsLine write SetIsStatsLine;
			property IsError : Boolean read GetIsError write SetIsError;
			property RowNr : Integer read GetRowNr write SetRowNr;
			property ParserType : TParserType read GetParserType write SetParserType;
	end;

	TParsedObjectRowCollection = class(TNoRefCountObject, IParsedObjectRowCollection, IStreamReaderWriterPersistable)
		private
			FItems : TListType;
			function GetItems : TListType;
			// procedure SetItems(const Value : TListType);

		public
			constructor Create;
			destructor Destroy; override;
			procedure LoadFromStreamReader(_sr : TStreamReader);
			procedure SaveToStreamWriter(_sw : TStreamWriter);
			property Items : TListType read GetItems; // write SetItems;
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
	RipGrepper.Helper.Types,
	RipGrepper.Helper.StreamReaderWriter;

class function TColumnData.New(const _idxTitle : EColumnIndex; const _Text : string) : TColumnData;
begin
	Result.Title := TDefaults.ColumnTitle[_idxTitle];
	Result.Text := _Text;
end;

constructor TParsedObjectRow.Create(const _por : IParsedObjectRow; _parserType : TParserType);
begin
	inherited Create();
	ErrorText := _por.ErrorText;
	Columns := _por.Columns;
	RowNr := _por.RowNr;
	IsError := _por.IsError;
	IsStatsLine := _por.IsStatsLine;
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
	_por.IsStatsLine := IsStatsLine;
end;

function TParsedObjectRow.GetColumnByTitle(const _title : string) : TColumnData;
var
	idxColumnData : Integer;
	idxTitle : Integer;
begin
	idxTitle := TDefaults.ColumnIndex[_title];
	idxColumnData := FColumns.IndexOf(TColumnData.New(EColumnIndex(idxTitle), ''), TComparer<TColumnData>.Construct(
		function(const Left, Right : TColumnData) : Integer
		begin
			Result := TComparer<string>.Default.Compare(Left.Title, Right.Title);
		end));

	Result := FColumns.SafeItem[idxColumnData];
end;

function TParsedObjectRow.GetColumns : TArrayEx<TColumnData>;
begin
	Result := FColumns;
end;

function TParsedObjectRow.GetColumnText(const _idx : integer) : string;
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

function TParsedObjectRow.GetIsStatsLine : Boolean;
begin
	Result := FIsStatsLine;
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

procedure TParsedObjectRow.SetIsStatsLine(const Value : Boolean);
begin
	FIsStatsLine := Value;
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
	FItems := TCollections.CreateList<IParsedObjectRow>();
end;

destructor TParsedObjectRowCollection.Destroy;
begin
	inherited;
end;

function TParsedObjectRowCollection.GetItems : TListType;
begin
	Result := FItems;
end;

procedure TParsedObjectRowCollection.LoadFromStreamReader(_sr : TStreamReader);
var
	cd : TColumnData;
	row : IParsedObjectRow;
	columns : TArrayEx<TColumnData>;
	idx : Integer;
	line : string;
	itemsCount : Integer;
	text : string;
begin
	FItems.Clear;
	itemsCount := _sr.ReadLineAsInteger;
	for var i : Integer := 0 to itemsCount - 1 do begin
		columns.Clear;
		row := TParsedObjectRow.Create( { nil, TParserType(0) } );
		row.ParserType := TParserType(_sr.ReadLineAsInteger());
		for var sTitle in TREEVIEW_COLUMN_TITLES do begin
			line := _sr.ReadLine;
			idx := line.IndexOf(ARRAY_SEPARATOR);
			if idx > 0 then begin
				text := line.Substring(idx + 1);
				cd := TColumnData.New(EColumnIndex(TDefaults.ColumnIndex[sTitle]), text);
				columns.Add(cd);
			end;
		end;
		row.Columns := columns;
		FItems.Add(row);
	end;
end;

procedure TParsedObjectRowCollection.SaveToStreamWriter(_sw : TStreamWriter);
begin
	_sw.WriteLine(FItems.Count);
	for var row : IParsedObjectRow in FItems do begin
		_sw.WriteLine(Ord(row.ParserType));
		for var sTitle in TREEVIEW_COLUMN_TITLES do begin
			var
			cd := row.GetColumnByTitle(sTitle);
			_sw.WriteLine(cd.Title + ARRAY_SEPARATOR + cd.Text);
			// ParserType?
		end;
	end;
end;

function TParsedObjectGroupedRowCollection.GetGroupId : Integer;
begin
	Result := FGroupId;
end;

procedure TParsedObjectGroupedRowCollection.SetGroupId(const Value : Integer);
begin
	FGroupId := Value;
end;

end.
