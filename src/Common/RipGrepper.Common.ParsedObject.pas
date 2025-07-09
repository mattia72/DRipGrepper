unit RipGrepper.Common.ParsedObject;

interface

uses
	ArrayEx,
	RipGrepper.Common.Constants,
	RipGrepper.Common.Interfaces.StreamPersistable,
	RipGrepper.Common.SimpleTypes,
	Spring.Collections,
	System.Classes,
	System.Generics.Collections,
	System.Generics.Defaults;

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
			function getColumns : TArrayEx<TColumnData>;
			procedure setColumns(const Value : TArrayEx<TColumnData>);
			function getErrorText : string;
			function getIsStatsLine : Boolean;
			function getIsError : Boolean;
			function getRowNr : Integer;
			procedure setErrorText(const Value : string);
			procedure setIsError(const Value : Boolean);
			procedure setRowNr(const Value : Integer);
			function getParserType : TParserType;
			procedure setIsStatsLine(const Value : Boolean);
			procedure setParserType(const Value : TParserType);

		public
			constructor Create(const _por : IParsedObjectRow; _parserType : TParserType); overload;
			destructor Destroy; override;
			procedure CopyTo(var _por : TParsedObjectRow);
			function GetColumnByTitle(const _title : string) : TColumnData;
			function GetColumnText(const _idx : integer) : string;

			property Columns : TArrayEx<TColumnData> read getColumns write setColumns;
			property ErrorText : string read getErrorText write setErrorText;
			property IsStatsLine : Boolean read getIsStatsLine write setIsStatsLine;
			property IsError : Boolean read getIsError write setIsError;
			property RowNr : Integer read getRowNr write setRowNr;
			property ParserType : TParserType read getParserType write setParserType;
	end;

	TParsedObjectRowCollection = class(TNoRefCountObject, IParsedObjectRowCollection, IStreamReaderWriterPersistable)
		private
			FItems : TListType;
			function getItems : TListType;

		public
			constructor Create;
			destructor Destroy; override;
			procedure LoadFromStreamReader(_sr : TStreamReader);
			procedure SaveToStreamWriter(_sw : TStreamWriter);
			function GetFileCount : Integer;
			function GetErrorCount : Integer;
			function GetTotalMatchCount : Integer;

			property Items : TListType read getItems;
	end;

	TParsedObjectGroupedRowCollection = class(TParsedObjectRowCollection, IParsedObjectGroupRowCollection)
		private
			FGroupId : Integer;
			function getGroupId : Integer;
			procedure setGroupId(const Value : Integer);

		public
			property GroupId : Integer read getGroupId write setGroupId;
	end;

	TParsedObjectGroupCollection = class
		Groups : TList<TParsedObjectGroupedRowCollection>;
	end;

implementation

uses
	RipGrepper.Helper.StreamReaderWriter,
	RipGrepper.Helper.Types,
	System.SysUtils;

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

function TParsedObjectRow.getColumns : TArrayEx<TColumnData>;
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

function TParsedObjectRow.getErrorText : string;
begin
	Result := FErrorText;
end;

function TParsedObjectRow.getIsStatsLine : Boolean;
begin
	Result := FIsStatsLine;
end;

function TParsedObjectRow.getIsError : Boolean;
begin
	Result := FIsError;
end;

function TParsedObjectRow.getParserType : TParserType;
begin
	Result := FParserType;
end;

function TParsedObjectRow.getRowNr : Integer;
begin
	Result := FRowNr;
end;

procedure TParsedObjectRow.setColumns(const Value : TArrayEx<TColumnData>);
begin
	FColumns := Value;
end;

procedure TParsedObjectRow.setErrorText(const Value : string);
begin
	FErrorText := Value;
end;

procedure TParsedObjectRow.setIsStatsLine(const Value : Boolean);
begin
	FIsStatsLine := Value;
end;

procedure TParsedObjectRow.setIsError(const Value : Boolean);
begin
	FIsError := Value;
end;

procedure TParsedObjectRow.setParserType(const Value : TParserType);
begin
	FParserType := Value;
end;

procedure TParsedObjectRow.setRowNr(const Value : Integer);
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

function TParsedObjectRowCollection.getItems : TListType;
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

function TParsedObjectRowCollection.GetFileCount : Integer;
var
	uniqueFiles : TArrayEx<string>;
begin
	FItems.All(
		function(const Item : IParsedObjectRow) : Boolean
		begin
			uniqueFiles.AddIfNotContains(Item.GetColumnByTitle('File').Text);
			Result := True;
		end);
	Result := uniqueFiles.Count;
end;

function TParsedObjectRowCollection.GetErrorCount : Integer;
begin
	Result := 0;
	for var i in FItems do begin
		if i.IsError then begin
			Inc(Result);
		end;
	end;
end;

function TParsedObjectRowCollection.GetTotalMatchCount : Integer;
begin
	Result := FItems.Count;
end;

function TParsedObjectGroupedRowCollection.getGroupId : Integer;
begin
	Result := FGroupId;
end;

procedure TParsedObjectGroupedRowCollection.setGroupId(const Value : Integer);
begin
	FGroupId := Value;
end;

end.
