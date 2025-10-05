unit RipGrepper.Common.LoadHistoryMode;

interface

type
	ELoadHistoryMode = (
		{ } lhmAll = 0, // should be index 0 because of radio button group
		{ } lhmHasResultOnly,
		{ } lhmMaxCount,
		{ } lhmLoadLastSearchHistories,
		{ } lhmSaveResults);

	TLoadHistoryModeSet = set of ELoadHistoryMode;

	TLoadHistoryModes = class
		const ENUM_PREFIX = 'lhm';

		strict private
			FModes : TLoadHistoryModeSet;
			function GetIsSaveHistoryActive() : Boolean;

		public
			function ToInt() : integer;
			procedure AddModeFromInt(const _i : integer);
			function ToString() : string; reintroduce;
			procedure FromString(const _s : string);
			procedure AddMode(const _e : ELoadHistoryMode); overload;
			procedure RemoveMode(const _e : ELoadHistoryMode); overload;
			procedure CleanModes(const _bModesOnly : Boolean);
			function IsSet(_e : ELoadHistoryMode) : Boolean;
			constructor Create(const _m : ELoadHistoryMode);
			property IsSaveHistoryActive : Boolean read GetIsSaveHistoryActive;
			property Modes : TLoadHistoryModeSet read FModes write FModes;
	end;

implementation

uses
	ArrayEx,
	RipGrepper.Helper.Types,
	System.SysUtils,
	System.Math;

constructor TLoadHistoryModes.Create(const _m : ELoadHistoryMode);
begin
	FModes := [_m];
end;

procedure TLoadHistoryModes.CleanModes(const _bModesOnly : Boolean);
begin
	var
	tmpModes := FModes;
	FModes := [];
	if _bModesOnly then begin
		if (lhmSaveResults in tmpModes) then begin
			Include(FModes, lhmSaveResults);
		end;
		if (lhmLoadLastSearchHistories in tmpModes) then begin
			Include(FModes, lhmLoadLastSearchHistories);
		end;
	end;
end;

function TLoadHistoryModes.GetIsSaveHistoryActive() : Boolean;
begin
	Result := not((FModes = []) or (FModes = [lhmSaveResults]));
end;

function TLoadHistoryModes.ToInt() : integer;
var
	bFound : Boolean;
	intVal : Integer;
begin
	intVal := 0;
	bFound := False;
	for var i := low(ELoadHistoryMode) to high(ELoadHistoryMode) do begin
		if i in FModes then begin
			bFound := True;
			break;
		end;
		Inc(intVal);
	end;
	Result := IfThen(bFound and (intVal <= Ord(lhmMaxCount)), intVal, -1);
end;

procedure TLoadHistoryModes.AddModeFromInt(const _i : integer);
begin
	CleanModes(True);
	for var i := low(ELoadHistoryMode) to high(ELoadHistoryMode) do begin
		if Ord(i) = _i then begin
			Include(FModes, i);
			break;
		end;
	end;
end;

function TLoadHistoryModes.ToString() : string;
var
	arr : TArrayEx<string>;
begin
	Result := '';
	for var i := low(ELoadHistoryMode) to high(ELoadHistoryMode) do begin
		if i in FModes then begin
			arr.Add(TConversions<ELoadHistoryMode>.EnumerationToString(i));
		end;
	end;
	Result := '[' + string.Join(',', arr.Items) + ']';
end;

procedure TLoadHistoryModes.FromString(const _s : string);
begin
	FModes := [];
	for var i := low(ELoadHistoryMode) to high(ELoadHistoryMode) do begin
		if _s.Contains(TConversions<ELoadHistoryMode>.EnumerationToString(i)) then begin
			Include(FModes, i);
		end;
	end;
end;

procedure TLoadHistoryModes.AddMode(const _e : ELoadHistoryMode);
begin
	Include(FModes, _e);
end;

procedure TLoadHistoryModes.RemoveMode(const _e : ELoadHistoryMode);
begin
	Exclude(FModes, _e);
end;

function TLoadHistoryModes.IsSet(_e : ELoadHistoryMode) : Boolean;
begin
	Result := _e in FModes;
end;

end.
