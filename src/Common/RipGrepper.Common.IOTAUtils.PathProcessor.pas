unit RipGrepper.Common.IOTAUtils.PathProcessor;

interface

uses
	System.Classes,
	ToolsAPI;

type
	TPathProcessor = class

		private
			FCompilerVersion : string;
			FIdeBasePath : string;
			FConfigName : string;
			FPlatformName : string;
			FRootDir : string;
			FEnvironment : TStringList;
			FNonExistsPaths : TStringList;
			FProjectOptions : IOTAProjectOptions;
			procedure Init(_Project : IOTAProject);
			function ProcessProjectDefines(const _Path : string) : string;

		public
			/// <summary>
			/// @param _sRootDir will be used for relative paths
			/// @param _Project will be used to determine Platform and Config, if not given, the
			/// current project will be used. </summary>
			constructor Create(const _sRootDir : string; _Project : IOTAProject = nil);
			destructor Destroy; override;
			procedure GetEnvironmentVariables(Strings : TStrings);
			procedure GetAllProjectOptions(Options : TStrings);
			function GetProjectOption(const OptionName : string) : string;
			function Process(const _sPath : string; const _bSkipProjectDefines : Boolean = False) : string;
			class function ReplaceMacro(const _str, _oldValue, _newValue : string) : string;
			property PlatformName : string read FPlatformName write FPlatformName;
			property ConfigName : string read FConfigName write FConfigName;
			property Environment : TStringList read FEnvironment;
			property IdeBasePath : string read FIdeBasePath write FIdeBasePath;
			property NonExistsPaths : TStringList read FNonExistsPaths;
	end;

type
	TIdeUtils = class
		public
			// Return the IDE's root directory (the installation directory).
			// Returns an empty string if the information could not be retrieved.
			class function GetIdeRootDirectory() : string;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.SysUtils,
	Winapi.Windows,
	System.Variants,
	RipGrepper.Tools.FileUtils,
	RipGrepper.Common.RegistryUtils,
	Vcl.Forms,
	System.StrUtils,
	RipGrepper.Common.Constants;

{ TPathProcessor }

constructor TPathProcessor.Create(const _sRootDir : string; _Project : IOTAProject = nil);

begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPathProcessor.Process');

	inherited Create;
	FRootDir := _sRootDir;
	dbgMsg.MsgFmt('FRootDir = %s', [FRootDir]);
	Init(_Project);
	FIdeBasePath := ExcludeTrailingPathDelimiter(TIdeUtils.GetIdeRootDirectory);
	// FIdeBasePath := ExcludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
	dbgMsg.MsgFmt('IdeBasePath = %s', [FIdeBasePath]);
	FEnvironment := TStringList.Create(TDuplicates.dupIgnore, True, false);
	FNonExistsPaths := TStringList.Create(TDuplicates.dupIgnore, True, false);
	GetEnvironmentVariables(FEnvironment);
end;

destructor TPathProcessor.Destroy;
begin
	FreeAndNil(FEnvironment);
	FreeAndNil(FNonExistsPaths);
	inherited;
end;

procedure TPathProcessor.GetEnvironmentVariables(Strings : TStrings);
var
	EnvStart : Pointer;
	EnvPos : PChar;
begin
	Assert(Assigned(Strings));
	Strings.Clear;
	EnvStart := GetEnvironmentStrings;
	try
		EnvPos := EnvStart;
		while StrLen(EnvPos) > 0 do begin
			Strings.Add(EnvPos);
			EnvPos := StrEnd(EnvPos) + 1;
		end;
	finally
		FreeEnvironmentStrings(EnvStart);
	end;
end;

procedure TPathProcessor.GetAllProjectOptions(Options : TStrings);
begin
	Assert(Assigned(Options));
	Options.Clear;
	if Assigned(FProjectOptions) then begin
		var
		OptionNames := FProjectOptions.GetOptionNames;
		for var i := 0 to Length(OptionNames) - 1 do begin
			var
			OptionName := OptionNames[i].Name;
			var
			OptionValue := VarToStr(FProjectOptions.Values[OptionName]);
			Options.Add(OptionName + '=' + OptionValue);
		end;
	end;
end;

function TPathProcessor.GetProjectOption(const OptionName : string) : string;
begin
	Result := '';
	if Assigned(FProjectOptions) then begin
		Result := VarToStr(FProjectOptions.Values[OptionName]);
	end;
end;

procedure TPathProcessor.Init(_Project : IOTAProject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPathProcessor.Init');
	// if _Project = nil then begin
	// _Project := IOTAUtils.GxOtaGetCurrentProject;
	// end;
	if _Project <> nil then begin
		FPlatformName := _Project.CurrentPlatform;
		dbgMsg.MsgFmt('FPlatformName = %s', [FPlatformName]);
		FConfigName := _Project.CurrentConfiguration;
		dbgMsg.MsgFmt('FConfigName = %s', [FConfigName]);
		FProjectOptions := _Project.GetProjectOptions;
		{$IF CompilerVersion = COMPILER_VERSION_DELPHI_11}
		FCompilerVersion := '22.0';
		{$ENDIF}
		{$IF CompilerVersion = COMPILER_VERSION_DELPHI_12}
		FCompilerVersion := '23.0';
		{$ENDIF}
		dbgMsg.MsgFmt('FCompilerVersion = %s', [FCompilerVersion]);
		dbgMsg.MsgFmt('FProjectOptions length %d', [Length(FProjectOptions.GetOptionNames)]);
	end;
end;

function TPathProcessor.Process(const _sPath : string; const _bSkipProjectDefines : Boolean = False) : string;
const
	IDEBaseMacros : array [0 .. 2] of string = ('BDS', 'DELPHI', 'BCB');
var
	i : Integer;
	EnvName : string;
	EnvValue : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPathProcessor.Process');
	Result := _sPath;

	// Expand the IDE base folder names $([DELPHI,BCB,BDS])
	for i := low(IDEBaseMacros) to high(IDEBaseMacros) do begin
		Result := ReplaceMacro(Result, IDEBaseMacros[i], FIdeBasePath);
	end;

	// Expand any environment variable macros
	for i := 0 to FEnvironment.Count - 1 do begin
		EnvName := Trim(FEnvironment.Names[i]);
		EnvValue := Trim(FEnvironment.Values[EnvName]);
		if (EnvName <> '') and (EnvValue <> '') then begin
			Result := ReplaceMacro(Result, EnvName, EnvValue);
		end;
	end;

	// Expand project preprocessor constants (DCC_Define)
	if not _bSkipProjectDefines then begin
		Result := ProcessProjectDefines(Result);
	end;

	if FPlatformName <> '' then begin
		Result := ReplaceMacro(Result, 'Platform', FPlatformName);
	end;

	if FConfigName <> '' then begin
		Result := ReplaceMacro(Result, 'Config', FConfigName);
	end;

	// TODO: Implement compiler version replacement
	if FCompilerVersion <> '' then begin
		Result := ReplaceMacro(Result, 'CompilerVersion', FCompilerVersion);
	end;

	if (not FRootDir.IsEmpty) and (not TFileUtils.IsPathAbsolute(Result)) then begin
		Result := TFileUtils.ExpandFileNameRelBaseDir(Result, FRootDir);
	end;

	if not DirectoryExists(Result) then begin
		NonExistsPaths.Add(Result);
		Result := '';
	end else begin
		Result := Result.TrimRight([PathDelim]);
	end;

end;

function TPathProcessor.ProcessProjectDefines(const _Path : string) : string;
var
	DefineList : TStringList;
	DefineValue : string;
begin
	Result := _Path;
	// Expand project preprocessor constants (DCC_Define)
	if Assigned(FProjectOptions) then begin
		DefineValue := VarToStr(FProjectOptions.Values['DCC_Define']);
		if DefineValue <> '' then begin
			DefineList := TStringList.Create;
			try
				DefineList.Delimiter := ';';
				DefineList.DelimitedText := DefineValue;
				for var i := 0 to DefineList.Count - 1 do begin
					var
					Define := DefineList[i];
					var
					EqualPos := Pos('=', Define);
					if EqualPos > 0 then begin
						// Define with value: SYMBOL=VALUE
						var
						SymbolName := Copy(Define, 1, EqualPos - 1);
						var
						SymbolValue := Copy(Define, EqualPos + 1, Length(Define));
						Result := ReplaceMacro(Result, SymbolName, SymbolValue);
					end else begin
						// Define without value: SYMBOL (assume empty string or '1')
						Result := ReplaceMacro(Result, Define, '1');
					end;
				end;
			finally
				FreeAndNil(DefineList);
			end;
		end;
	end;
end;

class function TPathProcessor.ReplaceMacro(const _str, _oldValue, _newValue : string) : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPathProcessor.ReplaceMacro', True);
	var
	replaceVal := '$(' + _oldValue + ')';
	Result := StringReplace(_str, replaceVal, _newValue, [rfReplaceAll, rfIgnoreCase]);
	if _str <> Result then begin
		dbgMsg.MsgFmt('Path: %s', [_str]);
		dbgMsg.MsgFmt('Replacing: %s with: %s = %s', [replaceVal, _newValue, Result]);
	end;
end;

class function TIdeUtils.GetIdeRootDirectory() : string;
const
	BIN_DIR_POSTFIX = PathDelim + 'Bin';
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIdeUtils.GetIdeRootDirectory');

	// There is no entry in the registry or it is invalid -> use the application's exename
	Result := ExcludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
	dbgMsg.MsgFmt('Application.ExeName: %s', [Result]);
	if SameText(RightStr(Result, Length(BIN_DIR_POSTFIX)), BIN_DIR_POSTFIX) then begin
		// Remove the '\Bin' suffix from the path
		Result := Copy(Result, 1, Length(Result) - Length(BIN_DIR_POSTFIX));
		Result := IncludeTrailingPathDelimiter(Result);
		dbgMsg.MsgFmt('Result %s', [Result]);
	end else begin
		Result := '';
		dbgMsg.Msg('Result empty');
	end;
end;

end.
