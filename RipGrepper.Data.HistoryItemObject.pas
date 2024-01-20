unit RipGrepper.Data.HistoryItemObject;

interface

uses
	RipGrepper.Common.Interfaces,
	ArrayHelper,
	RipGrepper.Data.Matches,
	RipGrepper.Common.Settings;

type
	THistoryItemObject = record // class(TInterfacedObject, IHistoryItem)
		private
			FFileCount : integer;
			FMatches : TRipGrepMatchLineCollection;
			FRipGrepArguments : TArrayRecord<string>;
			FTotalMatchCount : integer;
			function GetFileCount : integer;
			function GetMatches : TRipGrepMatchLineCollection;
			function GetRipGrepArguments : TArrayRecord<string>;
			function GetTotalMatchCount : integer;
			procedure SetFileCount(const Value : integer);
			procedure SetMatches(const Value : TRipGrepMatchLineCollection);
			procedure SetRipGrepArguments(const Value : TArrayRecord<string>);
			procedure SetTotalMatchCount(const Value : integer);

		public
			procedure CopyData(const _data : TRipGrepperData);
			procedure CopySettings(const _settings : TRipGrepperSettings);
			class operator Initialize(out Dest : THistoryItemObject);
			property FileCount : integer read GetFileCount write SetFileCount;
			property Matches : TRipGrepMatchLineCollection read GetMatches write SetMatches;
			property RipGrepArguments : TArrayRecord<string> read GetRipGrepArguments write SetRipGrepArguments;
			property TotalMatchCount : integer read GetTotalMatchCount write SetTotalMatchCount;
	end;

	PHistoryItemObject = ^THistoryItemObject;

implementation

procedure THistoryItemObject.CopyData(const _data : TRipGrepperData);
begin
	Matches := _data.Matches;
	TotalMatchCount := _data.TotalMatchCount;
	FileCount := _data.FileCount;
end;

procedure THistoryItemObject.CopySettings(const _settings : TRipGrepperSettings);
begin
	RipGrepArguments.AddRange(_settings.RipGrepArguments.ToStringArray());
end;

function THistoryItemObject.GetFileCount : integer;
begin
	Result := FFileCount;
end;

function THistoryItemObject.GetMatches : TRipGrepMatchLineCollection;
begin
	Result := FMatches;
end;

function THistoryItemObject.GetRipGrepArguments : TArrayRecord<string>;
begin
	Result := FRipGrepArguments;
end;

function THistoryItemObject.GetTotalMatchCount : integer;
begin
	Result := FTotalMatchCount;
end;

procedure THistoryItemObject.SetFileCount(const Value : integer);
begin
	FFileCount := Value;
end;

procedure THistoryItemObject.SetMatches(const Value : TRipGrepMatchLineCollection);
begin
	FMatches := Value;
end;

procedure THistoryItemObject.SetRipGrepArguments(const Value : TArrayRecord<string>);
begin
	FRipGrepArguments := Value;
end;

procedure THistoryItemObject.SetTotalMatchCount(const Value : integer);
begin
	FTotalMatchCount := Value;
end;

class operator THistoryItemObject.Initialize(out Dest : THistoryItemObject);
begin
	Dest.FFileCount := 0;
	Dest.FMatches.Clear;
	Dest.FTotalMatchCount := 0;
	Dest.FRipGrepArguments.Clear;
end;

end.
