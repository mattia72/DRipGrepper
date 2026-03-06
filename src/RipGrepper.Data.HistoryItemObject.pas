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
		const
			STREAM_FORMAT_VERSION = 2;
			STREAM_VERSION_PREFIX = 'HIO_V:';

		strict private
			FElapsedTimeText : string;
			FErrorCounters : TErrorCounters;
			FFileCount : integer;
			FHasResult : Boolean;
			FMatches : TParsedObjectRowCollection;
			FNoMatchFound : Boolean;
			FParserType : TParserType;
			FResultsTruncated : Boolean;
			FRipGrepResult : Integer;
			FTotalMatchCount : integer;

			// saved setting items
			FGuiSearchTextParams : IShared<TGuiSearchTextParams>;
			FIsLoadedFromStream : Boolean;
			FStreamFormatVersion : Integer;
			FRipGrepArguments : IShared<TRipGrepArguments>;
			FSearchFormSettings : TSearchFormSettings;
			FShouldSaveResult : Boolean;

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
			function GetResultsTruncated() : Boolean;
			function GetSearchFormSettings : TSearchFormSettings;
			function GetRipGrepResult : Integer;
			procedure SetParserType(const Value : TParserType);
			function GetSearchText : string;
			procedure Initialize();
			procedure SetElapsedTimeText(const Value : string);
			procedure SetGuiSearchTextParams(const Value : IShared<TGuiSearchTextParams>);
			procedure SetNoMatchFound(const Value : Boolean);
			procedure SetResultsTruncated(const Value : Boolean);
			procedure SetSearchFormSettings(const Value : TSearchFormSettings);
			procedure SetRipGrepResult(const Value : Integer);

		private
			FIsExpertMode: Boolean;
			function GetIsExpertMode(): Boolean;
			function GetShouldSaveResult() : Boolean;
			procedure loadGuiSearchTextParamsLegacy(_sr : TStreamReader; const _searchTextOfUser : string);
			procedure SetIsExpertMode(const Value: Boolean);
			procedure SetShouldSaveResult(const Value : Boolean);

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
			property IsExpertMode: Boolean read GetIsExpertMode write SetIsExpertMode;
			property IsLoadedFromStream : Boolean read GetIsLoadedFromStream;
			property IsReplaceMode : Boolean read GetIsReplaceMode;
			property NoMatchFound : Boolean read GetNoMatchFound write SetNoMatchFound;
			property ResultsTruncated : Boolean read GetResultsTruncated write SetResultsTruncated;
			property RipGrepResult : Integer read GetRipGrepResult write SetRipGrepResult;
			property ParserType : TParserType read GetParserType write SetParserType;
			property SearchFormSettings : TSearchFormSettings read GetSearchFormSettings write SetSearchFormSettings;
			property SearchText : string read GetSearchText;
			property ReplaceText : string read GetReplaceText;
			property ShouldSaveResult : Boolean read GetShouldSaveResult write SetShouldSaveResult;

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
	FResultsTruncated := False;
	FMatches.Items.Clear;
	FIsLoadedFromStream := False;
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

function THistoryItemObject.GetIsExpertMode(): Boolean;
begin
  Result := FIsExpertMode;
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

function THistoryItemObject.GetResultsTruncated() : Boolean;
begin
	Result := FResultsTruncated;
end;

function THistoryItemObject.GetSearchText : string;
begin
	Result := GuiSearchTextParams.GetSearchText;
end;

function THistoryItemObject.GetSearchTextWithOptions() : IShared<TSearchTextWithOptions>;
begin
	Result := FGuiSearchTextParams.SearchTextWithOptions;
end;

function THistoryItemObject.GetShouldSaveResult() : Boolean;
begin
	Result := FShouldSaveResult;
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
	FStreamFormatVersion := STREAM_FORMAT_VERSION;
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

procedure THistoryItemObject.loadGuiSearchTextParamsLegacy(_sr : TStreamReader; const _searchTextOfUser : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('THistoryItemObject.loadGuiSearchTextParamsLegacy');

	// _searchTextOfUser was already consumed from stream (it was the first line, before version header was introduced)
	GuiSearchTextParams.SearchTextWithOptions.SearchTextOfUser := _searchTextOfUser;
	dbgMsg.MsgFmt('SearchTextOfUser = %s', [_searchTextOfUser]);

	// Read SearchOptions (from TSearchTextWithOptions)
	var
	searchOptsStr := _sr.ReadLineAsString(false, 'SearchOptions');
	GuiSearchTextParams.SearchTextWithOptions.UpdateSearchOptions(searchOptsStr);
	dbgMsg.MsgFmt('SearchOptions = %s', [searchOptsStr]);

	// Read ExpertOptions (TOptionStrings record)
	var
	expertOpts := GuiSearchTextParams.ExpertOptions;
	expertOpts.LoadFromStreamReader(_sr);
	GuiSearchTextParams.ExpertOptions := expertOpts;

	// Read IsReplaceMode and ReplaceText
	GuiSearchTextParams.IsReplaceMode := _sr.ReadLineAsBool('IsReplaceMode');
	GuiSearchTextParams.ReplaceText := _sr.ReadLineAsString(true, 'ReplaceText');
	dbgMsg.MsgFmt('IsReplaceMode = %s, ReplaceText = %s', [BoolToStr(GuiSearchTextParams.IsReplaceMode, True), GuiSearchTextParams.ReplaceText]);
end;

procedure THistoryItemObject.LoadFromStreamReader(_sr : TStreamReader);
var
	count : integer;
	firstLine : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('THistoryItemObject.LoadFromStreamReader');
	try
		// Read first line to check for version header
		firstLine := _sr.ReadLineAsString(false, 'VersionOrSearchText');

		if firstLine.StartsWith(STREAM_VERSION_PREFIX) then begin
			FStreamFormatVersion := StrToIntDef(firstLine.Substring(Length(STREAM_VERSION_PREFIX)), 1);
			dbgMsg.MsgFmt('StreamFormatVersion = %d', [FStreamFormatVersion]);
			GuiSearchTextParams.LoadFromStreamReader(_sr);
		end else begin
			// Legacy format (version 1): first line is SearchTextOfUser, no version header
			FStreamFormatVersion := 1;
			dbgMsg.Msg('Legacy stream format detected (version 1)');
			loadGuiSearchTextParamsLegacy(_sr, firstLine);
		end;

		count := _sr.ReadLineAsInteger('RipGrepArguments.Count');
		dbgMsg.MsgFmt('RipGrepArguments.Count = %d', [count]);
		for var i := 0 to count - 1 do begin
			RipGrepArguments.Add(_sr.ReadLineAsString(true, 'RipGrepArguments[' + i.ToString + ']')); // RipGrep arguments can be empty
		end;
		dbgMsg.MsgFmt('RipGrepArguments = %s', [RipGrepArguments.Text]);

		SearchFormSettings.LoadFromStreamReader(_sr);
		FIsExpertMode := _sr.ReadLineAsBool('IsExpertMode');
		dbgMsg.MsgFmt('IsExpertMode = %s', [BoolToStr(FIsExpertMode, True)]);

		// ResultsTruncated was added in stream format version 2
		if FStreamFormatVersion >= 2 then begin
			FResultsTruncated := _sr.ReadLineAsBool('ResultsTruncated');
			dbgMsg.MsgFmt('ResultsTruncated = %s', [BoolToStr(FResultsTruncated, True)]);
		end else begin
			FResultsTruncated := False;
		end;

		// Read the flag indicating whether matches data follows
		FShouldSaveResult := _sr.ReadLineAsBool('ShouldSaveResult');
		dbgMsg.MsgFmt('ShouldSaveResult = %s', [BoolToStr(FShouldSaveResult, True)]);
		if FShouldSaveResult then begin
			Matches.LoadFromStreamReader(_sr);
			FHasResult := not Matches.Items.IsEmpty;
			if HasResult then begin
				FileCount := Matches.GetFileCount;
				// TotalMatchCount := Matches.GetTotalMatchCount;
				// TODO ErrorCounters ?
			end;
		end;

		FIsLoadedFromStream := True;
	except
		on E : Exception do begin
			dbgMsg.ErrorMsg('Error loading saved searches from file stream.');
			Initialize;
			raise;
		end;
	end;
end;

procedure THistoryItemObject.RefreshCounters(_errorCounters : TErrorCounters; _fileCount : Integer);
begin
	SetErrorCounters(_errorCounters);
	FileCount := _fileCount;
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

	// Write stream format version header
	_sw.WriteLineAsString(STREAM_VERSION_PREFIX + STREAM_FORMAT_VERSION.ToString, false, 'StreamFormatVersion');
	dbgMsg.MsgFmt('StreamFormatVersion = %d', [STREAM_FORMAT_VERSION]);

	GuiSearchTextParams.SaveToStreamWriter(_sw);
	dbgmsg.MsgFmt('RipGrepArguments.Count = %d', [RipGrepArguments.Count]);
	_sw.WriteLineAsInteger(RipgrepArguments.Count, 'RipgrepArguments.Count');
	for var s in RipGrepArguments() do begin
		dbgMsg.MsgFmt('RipGrepArguments = %s', [s]);
		_sw.WriteLineAsString(s, false, 'RipGrepArguments=''' + s + '''');
	end;
	SearchFormSettings.SaveToStreamWriter(_sw);
	_sw.WriteLineAsBool(IsExpertMode, 'IsExpertMode');
	dbgMsg.MsgFmt('IsExpertMode = %s', [BoolToStr(IsExpertMode, True)]);
	_sw.WriteLineAsBool(ResultsTruncated, 'ResultsTruncated');
	dbgMsg.MsgFmt('ResultsTruncated = %s', [BoolToStr(ResultsTruncated, True)]);
	// Always save a flag indicating whether matches data follows
	_sw.WriteLineAsBool(ShouldSaveResult, 'ShouldSaveResult');
	dbgMsg.MsgFmt('HasMatchesData = %s', [BoolToStr(ShouldSaveResult, True)]);
	if ShouldSaveResult then begin
		Matches.SaveToStreamWriter(_sw);
	end;
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

procedure THistoryItemObject.SetIsExpertMode(const Value: Boolean);
begin
    FIsExpertMode := Value;
end;

procedure THistoryItemObject.SetNoMatchFound(const Value : Boolean);
begin
	FNoMatchFound := Value;
end;

procedure THistoryItemObject.SetResultsTruncated(const Value : Boolean);
begin
	FResultsTruncated := Value;
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

procedure THistoryItemObject.SetShouldSaveResult(const Value : Boolean);
begin
	FShouldSaveResult := Value;
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
