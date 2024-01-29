unit RipGrepper.Common.ParsedObject;

interface

uses
	ArrayHelper,
	System.Generics.Collections, System.Generics.Defaults;

type
	TColumnData = record
		Title : string;
		Text : string;

		public
			class function New(const _Title, _Text : string) : TColumnData; static;
	end;

	{$M+}

	IParsedObject = interface
		['{5DDAF1CE-48EF-42C0-B87F-102E081B3D16}']
		function GetColumns : TArrayRecord<TColumnData>;
		procedure SetColumns(const Value : TArrayRecord<TColumnData>);
		property Columns : TArrayRecord<TColumnData> read GetColumns write SetColumns;
	end;

	IParsedObjectRow = interface(IParsedObject)
		['{85536634-C591-43F1-B348-BC93E4E62942}']
		function GetErrorText : string;
		function GetIsError : Boolean;
		function GetRowNr : Integer;
		procedure SetErrorText(const Value : string);
		procedure SetIsError(const Value : Boolean);
		procedure SetRowNr(const Value : Integer);
		property ErrorText : string read GetErrorText write SetErrorText;
		property IsError : Boolean read GetIsError write SetIsError;
		property RowNr : Integer read GetRowNr write SetRowNr;

	end;

	IParsedObjectRowCollection = interface
		['{03AA4D67-689A-43A0-8D07-FAB44124E032}']
		function GetItems : TList<IParsedObjectRow>;
		procedure SetItems(const Value : TList<IParsedObjectRow>);
		property Items : TList<IParsedObjectRow> read GetItems write SetItems;

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
			FColumns : TArrayRecord<TColumnData>;
			FRowNr : Integer;
			function GetColumns : TArrayRecord<TColumnData>;
			procedure SetColumns(const Value : TArrayRecord<TColumnData>);
			function GetErrorText : string;
			function GetIsError : Boolean;
			function GetRowNr : Integer;
			procedure SetErrorText(const Value : string);
			procedure SetIsError(const Value : Boolean);
			procedure SetRowNr(const Value : Integer);

		public
			property Columns : TArrayRecord<TColumnData> read GetColumns write SetColumns;
			property ErrorText : string read GetErrorText write SetErrorText;
			property IsError : Boolean read GetIsError write SetIsError;
			property RowNr : Integer read GetRowNr write SetRowNr;
	end;

	TParsedObjectRowCollection = class(TSingletonImplementation, IParsedObjectRowCollection)
		private
			FItems : TList<IParsedObjectRow>;
			function GetItems : TList<IParsedObjectRow>;
			procedure SetItems(const Value : TList<IParsedObjectRow>);

		public
			constructor Create;
			destructor Destroy; override;
			property Items : TList<IParsedObjectRow> read GetItems write SetItems;
	end;

	TParsedObjectGroupedRowCollection = class(TParsedObjectRowCollection, IParsedObjectGroupRowCollection)
		private
			FGroupId: Integer;
			function GetGroupId: Integer;
			procedure SetGroupId(const Value: Integer);

		public
			property GroupId: Integer read GetGroupId write SetGroupId;
	end;

	TParsedObjectGroupCollection = class
		Groups : TList<TParsedObjectGroupedRowCollection>;
	end;

implementation

class function TColumnData.New(const _Title, _Text : string) : TColumnData;
begin
	Result.Title := _Title;
	Result.Text := _Text;
end;

function TParsedObjectRow.GetColumns : TArrayRecord<TColumnData>;
begin
	Result := FColumns;
end;

function TParsedObjectRow.GetErrorText : string;
begin
	Result := FErrorText;
end;

function TParsedObjectRow.GetIsError : Boolean;
begin
	Result := FIsError;
end;

function TParsedObjectRow.GetRowNr : Integer;
begin
	Result := FRowNr;
end;

procedure TParsedObjectRow.SetColumns(const Value : TArrayRecord<TColumnData>);
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

procedure TParsedObjectRow.SetRowNr(const Value : Integer);
begin
	FRowNr := Value;
end;

constructor TParsedObjectRowCollection.Create;
begin
	inherited;
	FItems := TList<IParsedObjectRow>.Create();
end;

destructor TParsedObjectRowCollection.Destroy;
begin
	FItems.Free;
	inherited;
end;

function TParsedObjectRowCollection.GetItems : TList<IParsedObjectRow>;
begin
	Result := FItems;
end;

procedure TParsedObjectRowCollection.SetItems(const Value : TList<IParsedObjectRow>);
begin
	FItems := Value;
end;

function TParsedObjectGroupedRowCollection.GetGroupId: Integer;
begin
	Result := FGroupId;
end;

procedure TParsedObjectGroupedRowCollection.SetGroupId(const Value: Integer);
begin
	FGroupId := Value;
end;

end.
