unit RipGrepper.Tools.FileUtils.Replacer;

interface

uses
	System.Classes,
	System.IniFiles,
	RipGrepper.Tools.FileUtils,
    RegExprReplaceUnit;

type

	TGrepAction = (gaCurrentOnlyGrep, gaProjGrep, gaOpenFilesGrep, gaDirGrep, gaProjGroupGrep, gaResults);

	TGrepSearchState = (gssNormal, gssRefresh, gssRefreshAll, gssRefreshSelected, gssSearchAgain, gssModifySearchSettings,
		gssSearchEmbedded);

	TGrepSaveOption = (gsoSaveSettingsAndResults, gsoOnlySaveSettings, gsoNoSave);

	TGrepOnlySaveSettingsAction = (gossaShowSearchWindow, gossaShowEmbeddedSearch, gossaAutoRefresh, gossaAutoRefreshDouble,
		gossaEmptyList);

	// Saved grep settings (used for refresh)
	TGrepSettings = record
		IncludeComments : Boolean;
		IncludeCode : Boolean;
		IncludeStrings : Boolean;
		SectionInterface : Boolean;
		SectionImplementation : Boolean;
		SectionInitialization : Boolean;
		SectionFinalization : Boolean;
		CaseSensitive : Boolean;
		WholeWord : Boolean;
		RegEx : Boolean;
		IncludeSubdirs : Boolean;
		MinDepth : Integer;
		MaxDepth : Integer;
		Directories : string;
		ExcludedDirs : string;
		ExcludedDirsIsRegEx : Boolean;
		Mask : string;
		Pattern : string;
		Replace : string;
		GrepAction : TGrepAction;
		CanRefresh : Boolean;
		IncludeForms : Boolean;
		HandleFormMultiline : Boolean;
		HandleFormSpecialChars : Boolean;
		IncludeSQLs : Boolean;
		SaveOption : TGrepSaveOption;
		UseMapFile : Boolean;
	end;

	// Individual grep match in a line
	TMatchResult = class(TCollectionItem)
		private
			FSPos : Integer;
			FEPos : Integer;
			FShowBold : Boolean;

		public
			class function SubKeyName : string;

			constructor Create(Collection : TCollection); override;
			function Length : Integer;
			procedure LoadFromIni(AIni : TCustomIniFile; const ASection, ASubKey : string);
			procedure WriteToIni(AIni : TCustomIniFile; const ASection, ASubKey : string);

			property SPos : Integer read FSPos write FSPos;
			property EPos : Integer read FEPos write FEPos;
			property ShowBold : Boolean read FShowBold write FShowBold;
	end;

	// Collection of TMatchResult
	// Collection of all matches in a line
	TLineMatches = class(TCollection)
		private
			function GetItem(index : Integer) : TMatchResult;
			procedure SetItem(index : Integer; Value : TMatchResult);

		public
			class function SubKeyName : string;

			constructor Create;
			function Add : TMatchResult;
			property Items[index : Integer] : TMatchResult read GetItem write SetItem; default;
	end;

	// A single line that has a match from a file
	// One collection item per line with any number of matches
	TLineResult = class(TCollectionItem)
		private
			FLine : string;
			FLineNo : Integer; // 1-based
			FMatches : TLineMatches;

		public
			constructor Create(Collection : TCollection); override;
			destructor Destroy; override;
			function Add : TMatchResult;
			procedure LoadFromIni(AIni : TCustomIniFile; const ASection : string);
			procedure WriteToIni(AIni : TCustomIniFile; const ASection : string);

			property Line : string read FLine write FLine;
			property LineNo : Integer read FLineNo write FLineNo; // 1-based
			// Collection of all matches in a line
			property Matches : TLineMatches read FMatches;
	end;

	TMatchArray = array of TMatchResult;

	// Contains collection of all lines in a single source file that match.
	TFileResult = class(TCollection)
		private
			FExpanded : Boolean;
			FExpandState : Boolean;
			FFileName : string;
			FRelativeFileName : string;
			FLastLineResult : Integer; // Last LineNo added to result set
			FLastIndex : Integer; // Index of last added result
			FTotalMatches : Integer; // Total matches in file
			function GetItem(index : Integer) : TLineResult;
			procedure SetItem(index : Integer; Value : TLineResult);

		public
			class function SubKeyName : string;

			constructor Create;
			function Add : TLineResult;
			procedure GetMatchesOnLine(Line : Integer; var Matches : TMatchArray);
			function LoadFromIni(AIni : TCustomIniFile; const ASection : string) : Boolean;
			procedure WriteToIni(AIni : TCustomIniFile; const ASection : string);

			property Expanded : Boolean read FExpanded write FExpanded;
			property ExpandState : Boolean read FExpandState write FExpandState;
			property FileName : string read FFileName write FFileName;
			property RelativeFileName : string read FRelativeFileName write FRelativeFileName;
			property LastIndex : Integer read FLastIndex write FLastIndex;
			property LastLineResult : Integer read FLastLineResult write FLastLineResult;
			property Items[index : Integer] : TLineResult read GetItem write SetItem; default;
			property TotalMatches : Integer read FTotalMatches write FTotalMatches;
	end;

	TReplacer = class

		private
			FALineResult : TLineResult;
			FGrepSettings : TGrepSettings;
			FInMemory: Boolean;
			FLineMode : Boolean;
			FLineResult : TLineResult;
			FMatchFile : string;
			FTempFile : TEncodedStringList;
			procedure DoReplacement;
			procedure GetFileLines;
			function ReplacePatternInString(CurrentLine: TLineResult; GrepSettings:
				TGrepSettings): string;
			function ReplacePatternInStringWithRegEx(CurrentLine: TLineResult;
				GrepSettings: TGrepSettings; RegEx: TRegExpr): string;
			procedure WriteResults;

		public
			function InternalReplace(LineMode : Boolean; ALineResult : TLineResult; AFileResult : TFileResult; GrepSettings : TGrepSettings)
				: Integer;
			property ALineResult : TLineResult read FALineResult write FALineResult;
			property GrepSettings : TGrepSettings read FGrepSettings write FGrepSettings;
			property InMemory: Boolean read FInMemory write FInMemory;
			property LineMode : Boolean read FLineMode write FLineMode;
			property LineResult : TLineResult read FLineResult write FLineResult;
			property MatchFile : string read FMatchFile write FMatchFile;
			property TempFile : TEncodedStringList read FTempFile write FTempFile;
	end;

resourcestring
	SFileChangedAbort = '%s' + sLineBreak + 'has changed since it was searched.  Replacement aborted.' + sLineBreak + 'Expected: %s' +
		sLineBreak + 'Found: %s';
	SUnableToOpen = 'Unable to open ';
	SNoOpenForms = 'Replacing strings in open forms is not possible.  Please close the form first.';
	SFileSkipped = 'The following file will be skipped:';

implementation

uses
  System.SysUtils;

// Replaces the string between SPos and EPos with the replace string from TGrepSettings
function TReplacer.ReplacePatternInString(CurrentLine: TLineResult;
	GrepSettings: TGrepSettings): string;
var
  i: Integer;
  FindPos: Integer;
  FindLen: Integer;
  CurrentMatch: TMatchResult;
begin
  Result := CurrentLine.Line;
  for i := CurrentLine.Matches.Count - 1 downto 0 do
  begin
	CurrentMatch := CurrentLine.Matches.Items[i];
	FindPos := CurrentMatch.SPos;
	FindLen := CurrentMatch.EPos - CurrentMatch.SPos + 1;
	Delete(Result, FindPos, FindLen);
	Insert(GrepSettings.Replace, Result, FindPos);
	CurrentMatch.ShowBold := False;
  end;
end;

function TReplacer.ReplacePatternInStringWithRegEx(CurrentLine: TLineResult;
	GrepSettings: TGrepSettings; RegEx: TRegExpr): string;
var
  i: Integer;
begin
  Result := RegEx.Replace(CurrentLine.Line, GrepSettings.Replace, True);
  for i := CurrentLine.Matches.Count - 1 downto 0 do
	CurrentLine.Matches[i].ShowBold := False;
end;

procedure TReplacer.DoReplacement;
var
	i : Integer;
	FileLine : string;
	TempString: string;
begin
	if LineMode then begin
		i := ALineResult.LineNo;
		Assert(TempFile.Count >= (LineResult.LineNo - 1));
		FileLine := TempFile.Strings[LineResult.LineNo - 1];
		if LineResult.Line <> FileLine then
			raise Exception.CreateFmt(SFileChangedAbort, [MatchFile, LineResult.Line, FileLine]);

		if GrepSettings.RegEx then
			TempString := ReplacePatternInStringWithRegEx(LineResult, GrepSettings, RegEx)
		else
			TempString := ReplacePatternInString(LineResult, GrepSettings);
		TempFile.Strings[i - 1] := TempString;
		Inc(Result, LineResult.Matches.Count);
	end else begin
		for i := AFileResult.Count - 1 downto 0 do begin
			LineResult := AFileResult.Items[i];
			Inc(Result, LineResult.Matches.Count);
			Assert(TempFile.Count >= (LineResult.LineNo - 1));
			FileLine := TempFile.Strings[LineResult.LineNo - 1];
			if LineResult.Line <> FileLine then
				raise Exception.CreateFmt(SFileChangedAbort, [MatchFile, LineResult.Line, FileLine]);

			if GrepSettings.RegEx then
				TempString := ReplacePatternInStringWithRegEx(LineResult, GrepSettings, RegEx)
			else
				TempString := ReplacePatternInString(LineResult, GrepSettings);
			TempFile.Strings[LineResult.LineNo - 1] := TempString;
		end;
	end;
end;

procedure TReplacer.GetFileLines;
begin
	if InMemory then begin
		if IsForm(MatchFile) then
			raise ESkipFileReplaceException.Create(SNoOpenForms);
		Module := GxOtaGetModule(GxOtaGetBaseModuleFileName(MatchFile));
		if not Assigned(Module) then
			raise Exception.Create(SUnableToOpen + MatchFile);
		SourceEditor := GxOtaGetSourceEditorFromModule(Module, MatchFile);
		if not Assigned(SourceEditor) then
			raise Exception.Create(SUnableToOpen + MatchFile);
		GxOtaLoadFileToUnicodeStrings(SourceEditor.FileName, TempFile, WasBinary);
	end
	else
		GxOtaLoadFileToUnicodeStrings(MatchFile, TempFile, WasBinary);
end;

function TReplacer.InternalReplace(LineMode : Boolean; ALineResult : TLineResult; AFileResult : TFileResult; GrepSettings : TGrepSettings)
	: Integer;
var
	InMemory : Boolean;
	MatchFile : string;
	Module : IOTAModule;
	EditWriter : IOTAEditWriter;
	SourceEditor : IOTASourceEditor;
	RegEx : TRegExpr;
	WasBinary : Boolean;

begin
	Result := 0;
	WasBinary := False;
	if LineMode then begin
		LineResult := ALineResult;
		MatchFile := TFileResult(LineResult.Collection).FileName;
	end
	else
		MatchFile := AFileResult.FileName;

	RegEx := nil;
	TempFile := TGXUnicodeStringList.Create;
	try
		if GrepSettings.RegEx then begin
			RegEx := TRegExpr.Create;
			RegEx.Expression := GrepSettings.Pattern;
			RegEx.ModifierG := True;
			RegEx.ModifierI := not GrepSettings.CaseSensitive;
			RegEx.Compile;
		end;

		InMemory := GxOtaIsFileOpen(MatchFile, True);
		try
			GetFileLines;
		except
			on E : ESkipFileReplaceException do begin
				E.Message := E.Message + sLineBreak + SFileSkipped + sLineBreak + MatchFile;
				if MessageDlg(E.Message, mtWarning, [mbOK, mbCancel], 0) = mrCancel then
					Abort
				else
					Exit;
			end;
		end;
		DoReplacement;
		WriteResults;
	finally
		FreeAndNil(RegEx);
		FreeAndNil(TempFile);
	end;
end;

procedure TReplacer.WriteResults;
var
	FormFile : TFileStream;
	FormSource : TStringStream;
begin
	if InMemory then begin
		EditWriter := GxOtaGetEditWriterForSourceEditor(SourceEditor);
		EditWriter.DeleteTo(MaxInt);
		// RemoveLastEOL is necessary because TStringList.Text adds an extra CRLF on the end
		EditWriter.Insert(PAnsiChar(ConvertToIDEEditorString(RemoveTrailingEOL(TempFile.Text))));
		EditWriter := nil;
		Module := nil;
		SourceEditor := nil;
	end else begin
		if IsForm(MatchFile) and WasBinary then begin
			FormFile := nil;
			FormSource := TStringStream.Create(TempFile.Text);
			try
				FormSource.Seek(0, soFromBeginning);
				FormFile := TFileStream.Create(MatchFile, fmOpenWrite or fmShareDenyWrite);
				FormFile.Seek(0, soFromBeginning);
				ObjectTextToResource(FormSource, FormFile);
				FormFile.Size;
			finally
				FreeAndNil(FormSource);
				FreeAndNil(FormFile);
			end;
		end
		else
			TempFile.SaveToFile(MatchFile);
	end;
end;

{ TLineResult }

constructor TLineResult.Create(Collection : TCollection);
begin
	inherited Create(Collection);
	FMatches := TLineMatches.Create;
end;

destructor TLineResult.Destroy;
begin
	if Assigned(FMatches) then begin
		FMatches.Clear;
		FreeAndNil(FMatches);
	end;
	inherited Destroy;
end;

function TLineResult.Add : TMatchResult;
begin
	Result := Matches.Add;
end;

procedure TLineResult.LoadFromIni(AIni : TCustomIniFile; const ASection : string);
var
	I, ACount : Integer;
	ASubKey : string;
begin
	// INI file trims when reading back in, so a magic marker is used but we need to strip it when reading in.
	Line := Copy(AIni.ReadString(ASection, 'Line', '#' + Line), 2);
	LineNo := AIni.ReadInteger(ASection, 'LineNo', LineNo);

	// MatchList
	ASubKey := TMatchResult.SubKeyName;
	Matches.Clear;
	ACount := AIni.ReadInteger(ASection, ASubKey + cIniSubKeyCount, 0);
	for I := 0 to ACount - 1 do
		Add.LoadFromIni(AIni, ASection, Format('%s%d', [ASubKey, I]));
end;

procedure TLineResult.WriteToIni(AIni : TCustomIniFile; const ASection : string);
var
	I : Integer;
	ASubKey : string;
begin
	// INI file trims when reading back in, so a magic marker is used to keep the read contents.
	AIni.WriteString(ASection, 'Line', '#' + Line);
	AIni.WriteInteger(ASection, 'LineNo', LineNo);

	// MatchList
	ASubKey := TMatchResult.SubKeyName;
	AIni.WriteInteger(ASection, ASubKey + cIniSubKeyCount, 0);
	for I := 0 to Matches.Count - 1 do
		Matches.Items[I].WriteToIni(AIni, ASection, Format('%s%d', [ASubKey, I]));
	AIni.WriteInteger(ASection, ASubKey + cIniSubKeyCount, Matches.Count);
end;

{ TFileResult }

class function TFileResult.SubKeyName : string;
begin
	Result := 'File';
end;

constructor TFileResult.Create;
begin
	inherited Create(TLineResult);
	FLastLineResult := -1;
	FTotalMatches := 0;
end;

function TFileResult.Add : TLineResult;
begin
	Result := TLineResult(inherited Add);
end;

function TFileResult.GetItem(Index : Integer) : TLineResult;
begin
	Result := TLineResult(inherited GetItem(index));
end;

procedure TFileResult.SetItem(Index : Integer; Value : TLineResult);
begin
	inherited SetItem(index, Value);
end;

procedure TFileResult.GetMatchesOnLine(Line : Integer; var Matches : TMatchArray);
var
	i, j : Integer;
	LineMatches : TLineResult;
	MR : TMatchResult;
begin
	SetLength(Matches, 0);
	for i := 0 to Count - 1 do begin
		LineMatches := GetItem(i);
		if LineMatches.FLineNo = Line then begin
			for j := 0 to LineMatches.Matches.Count - 1 do begin
				SetLength(Matches, Length(Matches) + 1);
				MR := LineMatches.Matches.GetItem(j);
				Matches[Length(Matches) - 1] := MR;
			end;
		end;
	end;
end;

function TFileResult.LoadFromIni(AIni : TCustomIniFile; const ASection : string) : Boolean;
var
	I, ACount : Integer;
	ASubKey : string;
begin
	Result := False;
	FileName := AIni.ReadString(ASection, 'FileName', FileName);
	if Trim(FileName) = '' then
		Exit;

	ExpandState := AIni.ReadBool(ASection, 'ExpandState', False);
	RelativeFileName := AIni.ReadString(ASection, 'RelativeFileName', RelativeFileName);
	LastLineResult := AIni.ReadInteger(ASection, 'LastLineResult', LastLineResult);
	LastIndex := AIni.ReadInteger(ASection, 'LastIndex', LastIndex);
	TotalMatches := AIni.ReadInteger(ASection, 'TotalMatches', TotalMatches);

	// LineList
	ASubKey := TLineMatches.SubKeyName;
	Clear;
	ACount := AIni.ReadInteger(ASection, ASubKey + cIniSubKeyCount, 0);
	for I := 0 to ACount - 1 do
		Add.LoadFromIni(AIni, ASection + PathDelim + Format('%s%d', [ASubKey, I]));
	Result := True;
end;

procedure TFileResult.WriteToIni(AIni : TCustomIniFile; const ASection : string);
var
	I : Integer;
	ASubKey : string;
begin
	AIni.WriteBool(ASection, 'ExpandState', ExpandState);
	AIni.WriteString(ASection, 'FileName', FileName);
	AIni.WriteString(ASection, 'RelativeFileName', RelativeFileName);
	AIni.WriteInteger(ASection, 'LastLineResult', LastLineResult);
	AIni.WriteInteger(ASection, 'LastIndex', LastIndex);
	AIni.WriteInteger(ASection, 'TotalMatches', TotalMatches);

	// LineList
	ASubKey := TLineMatches.SubKeyName;
	AIni.WriteInteger(ASection, ASubKey + cIniSubKeyCount, 0);
	for I := 0 to Count - 1 do
		Items[I].WriteToIni(AIni, ASection + PathDelim + Format('%s%d', [ASubKey, I]));
	AIni.WriteInteger(ASection, ASubKey + cIniSubKeyCount, Count);
end;

{ TLineMatches }

class function TLineMatches.SubKeyName : string;
begin
	Result := 'Line';
end;

constructor TLineMatches.Create;
begin
	inherited Create(TMatchResult);
end;

function TLineMatches.Add : TMatchResult;
begin
	Result := TMatchResult(inherited Add);
end;

function TLineMatches.GetItem(Index : Integer) : TMatchResult;
begin
	Result := TMatchResult(inherited GetItem(index));
end;

procedure TLineMatches.SetItem(Index : Integer; Value : TMatchResult);
begin
	inherited SetItem(index, Value);
end;

{ TMatchResult }

class function TMatchResult.SubKeyName : string;
begin
	Result := 'Match';
end;

constructor TMatchResult.Create(Collection : TCollection);
begin
	inherited;
	ShowBold := True;
end;

function TMatchResult.Length : Integer;
begin
	Result := EPos - SPos + 1;
end;

procedure TMatchResult.LoadFromIni(AIni : TCustomIniFile; const ASection, ASubKey : string);
begin
	SPos := AIni.ReadInteger(ASection, ASubKey + 'SPos', SPos);
	EPos := AIni.ReadInteger(ASection, ASubKey + 'EPos', EPos);
	ShowBold := AIni.ReadBool(ASection, ASubKey + 'ShowBold', ShowBold);
end;

procedure TMatchResult.WriteToIni(AIni : TCustomIniFile; const ASection, ASubKey : string);
begin
	AIni.WriteInteger(ASection, ASubKey + 'SPos', SPos);
	AIni.WriteInteger(ASection, ASubKey + 'EPos', EPos);
	AIni.WriteBool(ASection, ASubKey + 'ShowBold', ShowBold);
end;

end.
