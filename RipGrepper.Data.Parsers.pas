unit RipGrepper.Data.Parsers;

interface

uses
	RipGrepper.Data.Matches,
	RipGrepper.Common.Interfaces,
	System.RegularExpressions,
	RipGrepper.Common.Types;

type
	TRipGrepLineBase = class(TInterfacedObject, ILineParser)
		private

			FLineNr : Integer;
			function GetLineNr : Integer;
			procedure SetLineNr(const Value : Integer);

		protected
			FErrorText : string;
			FIsError : Boolean;
			FLineParseRegex : TRegex;
			FParserType : TParserType;
			function GetLineParseRegex : TRegex;
			procedure SetLineParseRegex(const Value : TRegex);
			function GetParserType : TParserType;
			procedure SetParserType(const Value : TParserType);
			function GetErrorText : string;
			function GetIsError : Boolean;
		public
			procedure ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False); virtual;
			property LineNr : Integer read GetLineNr write SetLineNr;
			property LineParseRegex : TRegex read GetLineParseRegex write SetLineParseRegex;
			property IsError : Boolean read GetIsError;
			property ParserType : TParserType read GetParserType write SetParserType;
			property ErrorText : string read GetErrorText;

	end;

	TRipGrepErrorLineParser = class(TRipGrepLineBase)

	end;

	TRipGrepHelpLineParser = class(TRipGrepLineBase)

	end;

	TRipGrepMatchLineParser = class(TRipGrepLineBase, IRipGrepMatchLineGroup)
		function GetGroupId : Integer;
		procedure SetGroupId(const Value : Integer);

		private
			FCol : Integer;
			FFileName : string;
			FGroupId : Integer;
			FRow : Integer;
			FText : string;
			procedure SetRgResultLineParseError(const _sLine : string);
			function GetFileName : string; stdcall;
			procedure SetFileName(const Value : string); stdcall;
			function GetRow : Integer; stdcall;
			procedure SetRow(const Value : Integer); stdcall;
			function GetCol : Integer; stdcall;

			function GetText : string; stdcall;
			procedure SetCol(const Value : Integer); stdcall;
			procedure SetText(const Value : string); stdcall;
			function Validate : Boolean;
			function ValidatePath : Boolean;

		protected
		public
			procedure ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False); override;
			destructor Destroy; override;
			constructor Create;
			property Col : Integer read GetCol write SetCol;
			property FileName : string read GetFileName write SetFileName;
			property GroupId : Integer read GetGroupId write SetGroupId;
			property Row : Integer read GetRow write SetRow;
			property Text : string read GetText write SetText;
	end;

implementation

uses
	System.IOUtils,
	RipGrepper.Tools.DebugTools,
	System.Classes,
	System.SysUtils;

function TRipGrepMatchLineParser.ValidatePath : Boolean;
begin
	if FileName.StartsWith(':') then begin
		FErrorText := 'Begins with '':''';
	end else if not TPath.HasValidPathChars(FileName, False) then begin
		FErrorText := 'Invalid chars in path' + FileName;
	end else if not(TPath.IsDriveRooted(FileName) or TPath.IsRelativePath(FileName)) then begin
		FErrorText := 'Not an abs or relative path' + FileName;
	end;
	Result := FErrorText = '';
end;

procedure TRipGrepMatchLineParser.ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False);
var
	m : TMatch;
begin
	inherited;

	m := FLineParseRegex.Match(_s);
	if m.Success then begin
		// TDebugUtils.DebugMessage(_s);
		FileName := m.Groups[1].Value + m.Groups[2].Value;
		Row := StrToIntDef(m.Groups[3].Value, -1);
		Col := StrToIntDef(m.Groups[4].Value, -1);
		Text := m.Groups[5].Value;
		FIsError := not Validate;
	end else begin
		SetRgResultLineParseError(_s);
	end;

	if (FIsError) then begin
		TDebugUtils.DebugMessage('Error parsing line: ' + CRLF +
			{ } _s + CRLF +
			{ } 'File: ' + FileName + CRLF +
			{ } 'Row: ' + Row.ToString + CRLF +
			{ } 'Col: ' + Col.ToString + CRLF +
			{ } 'Text: ' + Text + CRLF +
			{ } 'ErrorText: ' + ErrorText);
	end;
end;

procedure TRipGrepMatchLineParser.SetRgResultLineParseError(const _sLine : string);
begin
	FileName := _sLine;
	Text := '';
	FErrorText := 'rg.exe result line couldn''t parsed.';
	FIsError := True;
end;

function TRipGrepMatchLineParser.Validate : Boolean;
begin
	Result := False;
	FIsError := not ValidatePath();
	if FIsError then
		Exit;

	FIsError := FIsError and (Row > 0);
	if FIsError then begin
		FErrorText := 'Invalid Row:' + Row.ToString;
		Exit;
	end;
	FIsError := FIsError and (Col > 0);
	if FIsError then begin
		FErrorText := 'Invalid Col:' + Col.ToString;
		Exit
	end;
	// GroupId?
	Result := True;
end;

destructor TRipGrepMatchLineParser.Destroy;
begin
	// TDebugUtils.DebugMessage('Destroy ' + BoolToStr(IsError, True) + ' ' + FileName)
	inherited;
end;

constructor TRipGrepMatchLineParser.Create;
begin
	FLineNr := 0;
	FFileName := '';
	FRow := -1;
	FCol := -1;
	FIsError := True;
	FGroupId := -1;
	FText := '';
	FErrorText := '';
	FLineParseRegex := TRegex.Create(RG_MATCH_LINE_REGEX);
end;

function TRipGrepMatchLineParser.GetCol : Integer;
begin
	Result := FCol;
end;

function TRipGrepMatchLineParser.GetFileName : string;
begin
	Result := FFileName;
end;

function TRipGrepMatchLineParser.GetGroupId : Integer;
begin
	Result := FGroupId;
end;

function TRipGrepMatchLineParser.GetRow : Integer;
begin
	Result := FRow;
end;

function TRipGrepMatchLineParser.GetText : string;
begin
	Result := FText;
end;

procedure TRipGrepMatchLineParser.SetCol(const Value : Integer);
begin
	FCol := Value;
end;

procedure TRipGrepMatchLineParser.SetFileName(const Value : string);
begin
	FFileName := Value;
end;

procedure TRipGrepMatchLineParser.SetGroupId(const Value : Integer);
begin
	FGroupId := Value;
end;

procedure TRipGrepMatchLineParser.SetRow(const Value : Integer);
begin
	FRow := Value;
end;

procedure TRipGrepMatchLineParser.SetText(const Value : string);
begin
	FText := Value;
end;

function TRipGrepLineBase.GetErrorText : string;
begin
	Result := FErrorText;
end;

function TRipGrepLineBase.GetIsError : Boolean;
begin
	Result := FIsError;
end;

function TRipGrepLineBase.GetLineNr : Integer;
begin
	Result := FLineNr;
end;

function TRipGrepLineBase.GetLineParseRegex : TRegex;
begin
	Result := FLineParseRegex;
end;

function TRipGrepLineBase.GetParserType : TParserType;
begin
	Result := FParserType;
end;

{ TRipGrepLineBase }

procedure TRipGrepLineBase.ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False);
begin
	LineNr := _iLnNr;
	// rest should be done in the subclass
end;

procedure TRipGrepLineBase.SetLineNr(const Value : Integer);
begin
	FLineNr := Value;
end;

procedure TRipGrepLineBase.SetLineParseRegex(const Value : TRegex);
begin
	FLineParseRegex := Value;
end;

procedure TRipGrepLineBase.SetParserType(const Value : TParserType);
begin
	FParserType := Value;
end;

end.
