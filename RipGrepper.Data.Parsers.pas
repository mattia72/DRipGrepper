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
			FIsError : Boolean;
			FLineNr : Integer;
			function GetIsError : Boolean;
			procedure SetIsError(const Value : Boolean);
			function GetLineNr : Integer;
			procedure SetLineNr(const Value : Integer);

		protected
			FLineParseRegex: TRegex;
			FParserType : TParserType;
			function GetLineParseRegex: TRegex;
			procedure SetLineParseRegex(const Value: TRegex);
			function GetParserType : TParserType;
			procedure SetParserType(const Value : TParserType);

		public
			procedure ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False); virtual;
			property LineNr : Integer read GetLineNr write SetLineNr;
			property LineParseRegex: TRegex read GetLineParseRegex write SetLineParseRegex;
			property IsError : Boolean read GetIsError write SetIsError;
			property ParserType : TParserType read GetParserType write SetParserType;

	end;

	TRipGrepErrorLine = class(TRipGrepLineBase)

	end;

	TRipGrepHelpLine = class(TRipGrepLineBase)

	end;

	TRipGrepMatchLine = class(TRipGrepLineBase, IRipGrepMatchLineGroup)

		IsError : Boolean;
		ErrorText : string;
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

		protected
		public
			procedure ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False); override;
			function Validate : Boolean;
			function ValidatePath : Boolean;
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

function TRipGrepMatchLine.ValidatePath : Boolean;
begin
	if FileName.StartsWith(':') then begin
		ErrorText := 'Begins with '':''';
	end else if not TPath.HasValidPathChars(FileName, False) then begin
		ErrorText := 'Invalid chars in path' + FileName;
	end else if not(TPath.IsDriveRooted(FileName) or TPath.IsRelativePath(FileName)) then begin
		ErrorText := 'Not an abs or relative path' + FileName;
	end;
	Result := ErrorText = '';
end;

procedure TRipGrepMatchLine.ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False);
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
		IsError := not Validate;
	end else begin
		SetRgResultLineParseError(_s);
	end;

	if (IsError) then begin
		TDebugUtils.DebugMessage('Error parsing line: ' + CRLF +
			{ } _s + CRLF +
			{ } 'File: ' + FileName + CRLF +
			{ } 'Row: ' + Row.ToString + CRLF +
			{ } 'Col: ' + Col.ToString + CRLF +
			{ } 'Text: ' + Text + CRLF +
			{ } 'ErrorText: ' + ErrorText);
	end;
end;

procedure TRipGrepMatchLine.SetRgResultLineParseError(const _sLine : string);
begin
	FileName := '';
	Text := _sLine;
	ErrorText := 'rg.exe result line couldn''t parsed.';
	IsError := True;
end;

function TRipGrepMatchLine.Validate : Boolean;
begin
	Result := False;
	IsError := not ValidatePath();
	if IsError then
		Exit;

	IsError := IsError and (Row > 0);
	if IsError then begin
		ErrorText := 'Invalid Row:' + Row.ToString;
		Exit;
	end;
	IsError := IsError and (Col > 0);
	if IsError then begin
		ErrorText := 'Invalid Col:' + Col.ToString;
		Exit
	end;
	// GroupId?
	Result := True;
end;

destructor TRipGrepMatchLine.Destroy;
begin
	// Dest.FLineParseRegex;
end;

constructor TRipGrepMatchLine.Create;
begin
	LineNr := 0;
	FileName := '';
	Row := -1;
	Col := -1;
	IsError := True;
	GroupId := -1;
	Text := '';
	ErrorText := '';
	FLineParseRegex := TRegex.Create(RG_MATCH_LINE_REGEX);
end;

function TRipGrepMatchLine.GetCol : Integer;
begin
	Result := FCol;
end;

function TRipGrepMatchLine.GetFileName : string;
begin
	Result := FFileName;
end;

function TRipGrepMatchLine.GetGroupId : Integer;
begin
	Result := FGroupId;
end;

function TRipGrepMatchLine.GetRow : Integer;
begin
	Result := FRow;
end;

function TRipGrepMatchLine.GetText : string;
begin
	Result := FText;
end;

procedure TRipGrepMatchLine.SetCol(const Value : Integer);
begin
	FCol := Value;
end;

procedure TRipGrepMatchLine.SetFileName(const Value : string);
begin
	FFileName := Value;
end;

procedure TRipGrepMatchLine.SetGroupId(const Value : Integer);
begin
	FGroupId := Value;
end;

procedure TRipGrepMatchLine.SetRow(const Value : Integer);
begin
	FRow := Value;
end;

procedure TRipGrepMatchLine.SetText(const Value : string);
begin
	FText := Value;
end;

function TRipGrepLineBase.GetIsError : Boolean;
begin
	Result := FIsError;
end;

function TRipGrepLineBase.GetLineNr : Integer;
begin
	Result := FLineNr;
end;

function TRipGrepLineBase.GetLineParseRegex: TRegex;
begin
	Result := FLineParseRegex ;
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

procedure TRipGrepLineBase.SetIsError(const Value : Boolean);
begin
	FIsError := Value;
end;

procedure TRipGrepLineBase.SetLineNr(const Value : Integer);
begin
	FLineNr := Value;
end;

procedure TRipGrepLineBase.SetLineParseRegex(const Value: TRegex);
begin
	FLineParseRegex := Value;
end;

procedure TRipGrepLineBase.SetParserType(const Value : TParserType);
begin
	FParserType := Value;
end;

end.
