unit RipGrepper.Data.HistoryItemObject;

interface

uses
	Vcl.ComCtrls,
	System.Generics.Defaults,
	System.Classes,
	ArrayEx,
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.ParsedObject,
	RipGrepper.Settings.AppSettings,
	RipGrepper.Common.Constants,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Settings.SearchFormSettings,
	RipGrepper.Common.SearchTextWithOptions,
	Spring,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Common.Interfaces.StreamPersistable,
	RipGrepper.Settings.RipGrepArguments;

type
	// THistoryItemObject = class(TNoRefCountObject, IHistoryItemObject)
	THistoryItemObject = class(TInterfacedObject, IHistoryItemObject, IStreamPersistable, IStreamReaderWriterPersistable)

		strict private
		private
			FElapsedTimeText : string;
			FErrorCounters : TErrorCounters;
			FFileCount : integer;
			FHasResult : Boolean;
			FMatches : TParsedObjectRowCollection;
			FNoMatchFound : Boolean;
			FParserType : TParserType;
			FRipGrepResult : Integer;
			FTotalMatchCount : integer;

			// saved setting items
			FGuiSearchTextParams : IShared<TGuiSearchTextParams>;
			FIsLoadedFromStream : Boolean;
			FRipGrepArguments : IShared<TRipGrepArguments>;
			FSearchFormSettings : TSearchFormSettings;

			function GetElapsedTimeText() : string;
			function GetErrorCounters() : TErrorCounters;
			function GetFileCount() : integer;
			function GetGuiSearchTextParams : IShared<TGuiSearchTextParams>;
			function GetIsReplaceMode() : Boolean;
			function GetMatches() : TParsedObjectRowCollection;
			function GetNoMatchFound() : Boolean;
			function GetRipGrepArguments : IShared<TRipGrepArguments>;
			function GetTotalMatchCount() : integer;
			procedure SetFileCount(const Value : integer);
			procedure SetMatches(const Value : TParsedObjectRowCollection);
			procedure SetRipGrepArguments(const Value : IShared<TRipGrepArguments>);
			function GetParserType : TParserType;
			function GetSearchFormSettings : TSearchFormSettings;
			function GetRipGrepResult : Integer;
			procedure SetParserType(const Value : TParserType);
			function GetSearchText : string;
			procedure Initialize();
			procedure SetElapsedTimeText(const Value : string);
			procedure SetGuiSearchTextParams(const Value : IShared<TGuiSearchTextParams>);
			procedure SetNoMatchFound(const Value : Boolean);
			procedure SetSearchFormSettings(const Value : TSearchFormSettings);
			procedure SetRipGrepResult(const Value : Integer);

		public
			constructor Create;
			destructor Destroy; override;
			procedure SetErrorCounters(const Value : TErrorCounters);
			procedure ClearMatches;
			procedure CopyToSettings(const _settings : TRipGrepperSettings);
			function GetIsLoadedFromStream() : Boolean;
			function GetReplaceText : string;
			function GetSearchTextWithOptions() : IShared<TSearchTextWithOptions>;
			function HasResult : Boolean;
			procedure LoadFromSettings(const _settings : TRipGrepperSettings);
			procedure LoadFromStream(_stream : TStream);
			procedure LoadFromStreamReader(_sr : TStreamReader);
			procedure RefreshCounters(_errorCounters : TErrorCounters; _fileCount : Integer);
			procedure SaveToStream(_stream : TStream);
			procedure SaveToStreamWriter(_sw : TStreamWriter);
			function UpdateParserType : TParserType;

			property FileCount : integer read GetFileCount write SetFileCount;
			property Matches : TParsedObjectRowCollection read GetMatches write SetMatches;
			property RipGrepArguments : IShared<TRipGrepArguments> read GetRipGrepArguments write SetRipGrepArguments;
			property TotalMatchCount : integer read GetTotalMatchCount;
			property ElapsedTimeText : string read GetElapsedTimeText write SetElapsedTimeText;
			property GuiSearchTextParams : IShared<TGuiSearchTextParams> read GetGuiSearchTextParams write SetGuiSearchTextParams;
			property IsLoadedFromStream : Boolean read GetIsLoadedFromStream;
			property IsReplaceMode : Boolean read GetIsReplaceMode;
			property NoMatchFound : Boolean read GetNoMatchFound write SetNoMatchFound;
			property RipGrepResult : Integer read GetRipGrepResult write SetRipGrepResult;
			property ParserType : TParserType read GetParserType write SetParserType;
			property SearchFormSettings : TSearchFormSettings read GetSearchFormSettings write SetSearchFormSettings;
			property SearchText : string read GetSearchText;
			property ReplaceText : string read GetReplaceText;

	end;

	THistoryObjectArray = TArrayEx<IHistoryItemObject>;

	TVSHistoryReplaceNodeData = record
		private
			FReplaceText : string;
			function GetReplaceText() : string;
			procedure SetReplaceText(const Value : string);

		public
			IsReplaceMode : Boolean;
			property ReplaceText : string read GetReplaceText write SetReplaceText;
	end;

	TVSHistoryNodeData = record
		SearchText : string;
		IsFromStream : Boolean;
		ReplaceData : TVSHistoryReplaceNodeData;

		public
			class operator Initialize(out Dest : TVSHistoryNodeData);
	end;

	PVSHistoryNodeData = ^TVSHistoryNodeData;

implementation

uses

	System.SysUtils,
	RipGrepper.Parsers.Factory,
	RipGrepper.Helper.Types,
	RipGrepper.Settings.RipGrepParameterSettings,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Helper.UI,
	RipGrepper.Helper.StreamReaderWriter;

procedure THistoryItemObject.LoadFromSettings(const _settings : TRipGrepperSettings);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('THistoryItemObject.LoadFromSettings');

	var
	args := _settings.GetRipGrepArguments();
	RipGrepArguments.Assign(args());
	GuiSearchTextParams.Copy(_settings.RipGrepParameters.GuiSearchTextParams());
	SearchFormSettings.Copy(_settings.SearchFormSettings);
	_settings.LastReplaceText := ReplaceText;
	dbgMsg.MsgFmt('LastReplaceText = %s', [ReplaceText])
end;

function THistoryItemObject.GetFileCount : integer;
begin
	Result := FFileCount;
end;

function THistoryItemObject.GetMatches : TParsedObjectRowCollection;
begin
	Result := FMatches;
end;

function THistoryItemObject.GetRipGrepArguments : IShared<TRipGrepArguments>;
begin
	Result := FRipGrepArguments;
end;

function THistoryItemObject.GetTotalMatchCount : integer;
begin
	Result := FMatches.Items.Count - FErrorCounters.FSumOfErrors - FErrorCounters.FStatLineCount;
	{$IFDEF THREADSAFE_LIST}
	FMatches.Unlock;
	{$ENDIF}
end;

procedure THistoryItemObject.SetFileCount(const Value : integer);
begin
	FFileCount := Value;
end;

procedure THistoryItemObject.SetMatches(const Value : TParsedObjectRowCollection);
begin
	FMatches := Value;
end;

procedure THistoryItemObject.SetRipGrepArguments(const Value : IShared<TRipGrepArguments>);
begin
	FRipGrepArguments := Value;
end;

destructor THistoryItemObject.Destroy;
begin
	(FMatches as TParsedObjectRowCollection).Free;
	FSearchFormSettings.Free;
	// FRipGrepArguments.Free;
	inherited;
end;

constructor THistoryItemObject.Create;
begin
	inherited;
	Initialize;
end;

procedure THistoryItemObject.ClearMatches;
begin
	FFileCount := 0;
	FTotalMatchCount := 0;
	FErrorCounters.Reset;
	FNoMatchFound := False;
	FMatches.Items.Clear;
	{$IFDEF THREADSAFE_LIST}
	FMatches.Unlock;
	{$ENDIF}
end;

procedure THistoryItemObject.CopyToSettings(const _settings : TRipGrepperSettings);
begin
	_settings.RipGrepParameters.RipGrepArguments.Assign(RipGrepArguments());
	_settings.RipGrepParameters.GuiSearchTextParams.Copy(GuiSearchTextParams());
	_settings.SearchFormSettings.Copy(SearchFormSettings);
	_settings.LastSearchText := GuiSearchTextParams().SearchTextWithOptions.SearchTextOfUser; { GetSearchTextForUser }
end;

function THistoryItemObject.GetElapsedTimeText : string;
begin
	Result := FElapsedTimeText;
end;

function THistoryItemObject.GetErrorCounters : TErrorCounters;
begin
	Result := FErrorCounters;
end;

function THistoryItemObject.GetGuiSearchTextParams : IShared<TGuiSearchTextParams>;
begin
	Result := FGuiSearchTextParams;
end;

function THistoryItemObject.GetIsLoadedFromStream() : Boolean;
begin
	Result := FIsLoadedFromStream;
end;

function THistoryItemObject.GetIsReplaceMode : Boolean;
begin
	Result := GuiSearchTextParams.IsReplaceMode;
end;

function THistoryItemObject.GetNoMatchFound : Boolean;
begin
	Result := FNoMatchFound;
end;

function THistoryItemObject.GetParserType : TParserType;
begin
	if FParserType = ptEmpty then begin
		UpdateParserType();
	end;
	Result := FParserType;
end;

function THistoryItemObject.GetReplaceText : string;
begin
	Result := GuiSearchTextParams.ReplaceText;
end;

function THistoryItemObject.GetSearchFormSettings : TSearchFormSettings;
begin
	Result := FSearchFormSettings;
end;

function THistoryItemObject.GetRipGrepResult : Integer;
begin
	Result := FRipGrepResult;
end;

function THistoryItemObject.GetSearchText : string;
begin
	Result := GuiSearchTextParams.GetSearchText;
end;

function THistoryItemObject.GetSearchTextWithOptions() : IShared<TSearchTextWithOptions>;
begin
	Result := FGuiSearchTextParams.SearchTextWithOptions;
end;

function THistoryItemObject.HasResult : Boolean;
begin
	Result := FHasResult;
end;

procedure THistoryItemObject.Initialize();
begin
	if not Assigned(FMatches) then begin
		FMatches := TParsedObjectRowCollection.Create();
	end;
	if not Assigned(FSearchFormSettings) then begin
		FSearchFormSettings := TSearchFormSettings.Create();
	end else begin
		FSearchFormSettings.Init;
	end;
	FGuiSearchTextParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create(TRipGrepParameterSettings.INI_SECTION));
	FRipGrepArguments := Shared.Make<TRipGrepArguments>();
	FParserType := ptEmpty;
	ClearMatches;
	FHasResult := False;
	FIsLoadedFromStream := False;
end;

procedure THistoryItemObject.LoadFromStream(_stream : TStream);
var
	sr : IShared<TStreamReader>;
begin
	sr := Shared.Make<TStreamReader>(TStreamReader.Create(_stream, TEncoding.UTF8));
	LoadFromStreamReader(sr);
end;

procedure THistoryItemObject.LoadFromStreamReader(_sr : TStreamReader);
var
	count : integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('THistoryItemObject.LoadFromStreamReader');
	try
		GuiSearchTextParams.LoadFromStreamReader(_sr);
		count := _sr.ReadLineAsInteger;
		dbgMsg.MsgFmt('RipGrepArguments.Count = %d', [count]);
		for var i := 0 to count - 1 do begin
			RipGrepArguments.Add(_sr.ReadLine);
		end;
		dbgMsg.MsgFmt('RipGrepArguments = %s', [RipGrepArguments.Text]);

		SearchFormSettings.LoadFromStreamReader(_sr);


		FIsLoadedFromStream := True;
	except
		on E : Exception do begin
			dbgMsg.ErrorMsg('Error loading saved searches from file stream');
			Initialize;
			raise;
		end;
	end;
end;

procedure THistoryItemObject.RefreshCounters(_errorCounters : TErrorCounters; _fileCount : Integer);
begin
	SetErrorCounters(_errorCounters);
	FileCount := _fileCount;
	// FIsLoadedFromStream := False; // so counters are updated on gui
end;

procedure THistoryItemObject.SaveToStream(_stream : TStream);
var
	sw : IShared<TStreamWriter>;
begin
	sw := Shared.Make<TStreamWriter>(TStreamWriter.Create(_stream));
	SaveToStreamWriter(sw);
end;

procedure THistoryItemObject.SaveToStreamWriter(_sw : TStreamWriter);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('THistoryItemObject.SaveToStreamWriter');

	GuiSearchTextParams.SaveToStreamWriter(_sw);
	dbgmsg.MsgFmt('RipGrepArguments.Count = %d', [RipGrepArguments.Count]);
	_sw.WriteLine(RipgrepArguments.Count);
	for var s in RipGrepArguments() do begin
		dbgMsg.MsgFmt('RipGrepArguments = %s', [s]);
		_sw.WriteLine(s);
	end;
	SearchFormSettings.SaveToStreamWriter(_sw);
end;

procedure THistoryItemObject.SetElapsedTimeText(const Value : string);
begin
	FElapsedTimeText := Value;
end;

procedure THistoryItemObject.SetErrorCounters(const Value : TErrorCounters);
begin
	FErrorCounters := Value;
end;

procedure THistoryItemObject.SetGuiSearchTextParams(const Value : IShared<TGuiSearchTextParams>);
begin
	FGuiSearchTextParams := Value;
end;

procedure THistoryItemObject.SetNoMatchFound(const Value : Boolean);
begin
	FNoMatchFound := Value;
end;

procedure THistoryItemObject.SetParserType(const Value : TParserType);
begin
	FParserType := Value;
end;

procedure THistoryItemObject.SetSearchFormSettings(const Value : TSearchFormSettings);
begin
	FSearchFormSettings := Value;
end;

procedure THistoryItemObject.SetRipGrepResult(const Value : Integer);
begin
	FRipGrepResult := Value;
	FHasResult := True;
	FIsLoadedFromStream := False; // so counters are updated on gui
end;

function THistoryItemObject.UpdateParserType : TParserType;
begin
	FParserType := TRipGrepperParsersFactory.TryGetParserType(TArrayEx<string>.Create(RipGrepArguments.GetValues()));
	Result := FParserType;
end;

function TVSHistoryReplaceNodeData.GetReplaceText() : string;
begin
	if IsReplaceMode then begin
		Result := FReplaceText;
	end else begin
		Result := '';
	end;
end;

procedure TVSHistoryReplaceNodeData.SetReplaceText(const Value : string);
begin
	FReplaceText := Value;
end;

class operator TVSHistoryNodeData.Initialize(out Dest : TVSHistoryNodeData);
begin
	Dest.SearchText := '';
	Dest.IsFromStream := False;
	Dest.ReplaceData.ReplaceText := '';
	Dest.ReplaceData.IsReplaceMode := False;
end;

end.
