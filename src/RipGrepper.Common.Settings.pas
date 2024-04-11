unit RipGrepper.Common.Settings;

interface

uses
	System.Classes,
	System.IniFiles,
	System.Generics.Collections,
	System.Generics.Defaults,
	RipGrepper.OpenWith.SimpleTypes,
	RipGrepper.Common.Constants,
	RipGrepper.Common.Settings.Base, 
	ArrayEx;

type

	TRipGrepParameterSettings = class(TRipGrepperSettingsBase)
		const
			INI_SECTION = 'RipGrepSettings';

		private
			FRipGrepArguments : TRipGrepArguments;
			FOptions : string;
			FRipGrepPath : string;
			FSearchPath : string;
			FSearchText : string;
			FFileMasks : string;
			procedure AddArgs(const _sName : string; const _args : TArray<string>; const _bQuote : Boolean = False);
			procedure SetFileMasks(const Value : string);
			procedure SetOptions(const Value : string);
			procedure SetSearchPath(const Value : string);
			procedure SetSearchText(const Value : string);
			property FileMasks : string read FFileMasks write SetFileMasks;

		protected
			procedure Init; override;

		public
			constructor Create(const _ini : TIniFile);
			destructor Destroy; override;
			function BuildCmdLine : string;
			class function FileMasksToOptions(const _arrMasks, _arrSkipMasks: TArrayEx<string>): string;
			class function GetFileMaskParamsFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';')
				: string; overload;
			class function GetFileMaskParamsFromOptions(const _sOptions : string) : TArray<string>;
			class function GetFileMaskParamsArrFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';'):
				TArray<string>; overload;
			class function GetFileMasksDelimited(const _sOptions, _argMaskRegex : string) : string;
			class function RemoveAllParams(const _sOptions, _argMaskRegex : string; const _bSwitch : Boolean = False) : string;
			function GetIniSectionName : string; override;
			class function GetMissingFileMaskOptions(const _sOptions, _sMasks : string) : string;
			procedure InitRipGrepExePath;
			procedure Load; override;
			function ReBuildArguments : TStrings;
			procedure Store; override;
			property Options : string read FOptions write SetOptions;
			property SearchPath : string read FSearchPath write SetSearchPath;
			property SearchText : string read FSearchText write SetSearchText;
			property RipGrepArguments : TRipGrepArguments read FRipGrepArguments write FRipGrepArguments;
			property RipGrepPath : string read FRipGrepPath write FRipGrepPath;
	end;

	TRipGrepperViewSettings = class(TRipGrepperSettingsBase)
		const
			VIEW_SETTINGS : array [0 .. 4] of string = ('ShowRelativePath', 'ShowFileIcon', 'AlternateRowColors', 'IndentLines',
				'ExpandNodes');

		const
			INI_SECTION = 'RipGrepperViewSettings';

		public
			AlternateRowColors : Boolean;
			IndentLines : Boolean;
			ShowFileIcon : Boolean;
			ShowRelativePath : Boolean;
			ExpandNodes : Boolean;
			procedure StoreViewSettings(const _s : string = '');
			constructor Create(const _ini : TIniFile);
			function GetIniSectionName : string; override;
			procedure Init; override;
			procedure Load; override;
			procedure Store; override;
	end;

	TRipGrepperOpenWithSettings = class(TRipGrepperSettingsBase)
		private
			FCommandList : TStringList;
			FTestFile : TOpenWithParams;
			function GetCommand(Index : Integer) : string;
			procedure SetCommand(Index : Integer; const Value : string);

		public
			constructor Create(const _ini : TIniFile);
			destructor Destroy; override;
			function GetIniSectionName : string; override;
			procedure Load; override;
			procedure Store; override;
			property Command[index : Integer] : string read GetCommand write SetCommand;
			property TestFile : TOpenWithParams read FTestFile write FTestFile;
	end;

	TRipGrepperExtensionSettings = class(TRipGrepperSettingsBase)
		const
			INI_SECTION = 'DelphiExtensionSettings';

		private
			FDripGrepperShortCut : string;

		public
			function GetIniSectionName : string; override;
			procedure Load; override;
			procedure Store; override;
			property DripGrepperShortCut : string read FDripGrepperShortCut write FDripGrepperShortCut;
	end;

	TRipGrepperAppSettings = class(TRipGrepperSettingsBase)
		const
			INI_SECTION = 'RipGrepperSettings';

		private
			FDebugTrace : Boolean;
			FExpertMode : Boolean;

		protected
			procedure Init; override;

		public
			function GetIniSectionName : string; override;
			procedure Load; override;
			procedure Store; override;
			property DebugTrace : Boolean read FDebugTrace write FDebugTrace;
			property ExpertMode : Boolean read FExpertMode write FExpertMode;
	end;

	TRipGrepperSettings = class(TRipGrepperSettingsBase)
		private
		var
			FActualSearchPath : string;
			FRipGrepParameters : TRipGrepParameterSettings;
			FRipGrepperViewSettings : TRipGrepperViewSettings;
			FRipGrepperOpenWithSettings : TRipGrepperOpenWithSettings;
			FRipGrepParamsHistory : TSTrings;
			FSearchPathsHistory : TStrings;
			FSearchTextsHistory : TStrings;
			FRipGrepArguments : TRipGrepArguments;
			FSearchPathIsDir : Boolean;
			FExtensionSettings : TRipGrepperExtensionSettings;
			FFileMasksHistory : TStrings;
			FRipGrepperSettings : TRipGrepperAppSettings;

		class var
			function GetActualRipGrepParams : string;
			function GetActualSearchPath : string;
			function GetActualSearchText : string;
			function GetIsEmpty : Boolean;
			function GetSearchPathIsDir : Boolean;
			procedure InitSettings;
			procedure LoadHistoryEntries(var _list : TStrings; const _section : string);
			procedure SetFileMasksHistory(const Value : TStrings);
			procedure SetRipGrepParamsHistory(const Value : TSTrings);
			procedure SetSearchPathsHistory(const Value : TStrings);
			procedure SetSearchTextsHistory(const Value : TStrings);
			procedure StoreHistoryEntries(const _list : TStrings; const _section : string);

		public
			procedure Load; override;
			procedure Store; override;
			procedure StoreViewSettings(const _s : string = '');
			constructor Create;
			destructor Destroy; override;
			procedure AddIfNotContains(_to, _from : TStrings);
			function GetIniSectionName : string; override;
			function GetIsModified : Boolean; override;
			function GetRipGrepArguments : TRipGrepArguments;
			function ReBuildArguments : TStrings;
			property ActualRipGrepParams : string read GetActualRipGrepParams;
			property ActualSearchPath : string read GetActualSearchPath;
			property ActualSearchText : string read GetActualSearchText;
			property ExtensionSettings : TRipGrepperExtensionSettings read FExtensionSettings write FExtensionSettings;
			property FileMasksHistory : TStrings read FFileMasksHistory write SetFileMasksHistory;
			property IsEmpty : Boolean read GetIsEmpty;

			property RipGrepParameters : TRipGrepParameterSettings read FRipGrepParameters write FRipGrepParameters;
			property SearchPathsHistory : TStrings read FSearchPathsHistory write SetSearchPathsHistory;
			property RipGrepParamsHistory : TSTrings read FRipGrepParamsHistory write SetRipGrepParamsHistory;
			property RipGrepperOpenWithSettings : TRipGrepperOpenWithSettings read FRipGrepperOpenWithSettings;
			property RipGrepperSettings : TRipGrepperAppSettings read FRipGrepperSettings write FRipGrepperSettings;
			property RipGrepperViewSettings : TRipGrepperViewSettings read FRipGrepperViewSettings write FRipGrepperViewSettings;
			property SearchPathIsDir : Boolean read GetSearchPathIsDir;
			property SearchTextsHistory : TStrings read FSearchTextsHistory write SetSearchTextsHistory;
	end;

	TRipGrepperSettingsInstance = class
		private
			class var FInstance : TRipGrepperSettings;
			class function GetInstance : TRipGrepperSettings; static;

		public
			constructor Create;
			class destructor Destroy;
			class procedure FreeInstance; reintroduce;
			class property Instance : TRipGrepperSettings read GetInstance;
	end;

var
	GSettings : TRipGrepperSettings;

implementation

uses
	System.SysUtils,
	Vcl.Forms,
	System.StrUtils,
	RipGrepper.Helper.Types,
	RipGrepper.Tools.DebugTools,
	RipGrepper.Tools.FileUtils,
	Vcl.Dialogs,
	System.IOUtils,
	Winapi.Windows,
	System.UITypes,
	{$IFNDEF STANDALONE}
	DripExtension.IOTA.Utils,
	{$ENDIF}
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.Helper.UI,
	Vcl.Menus,
	System.RegularExpressions;

function TRipGrepperSettings.GetActualRipGrepParams : string;
begin
	RipGrepParamsHistory.TryGetDef(0, Result);
end;

function TRipGrepperSettings.GetActualSearchPath : string;
var
	s : string;
begin
	if SearchPathsHistory.TryGetDef(0, s) and (s <> FActualSearchPath) then begin
		FActualSearchPath := s;
		FSearchPathIsDir := TDirectory.Exists(FActualSearchPath);
	end;
	Result := FActualSearchPath;
end;

function TRipGrepperSettings.GetActualSearchText : string;
begin
	SearchTextsHistory.TryGetDef(0, Result);
end;

function TRipGrepperSettings.GetIsEmpty : Boolean;
begin
	Result := FRipGrepParameters.RipGrepPath.IsEmpty;
end;

function TRipGrepperSettings.GetRipGrepArguments : TRipGrepArguments;
begin
	Result := FRipGrepParameters.RipGrepArguments;
end;

procedure TRipGrepperSettings.InitSettings;
begin
	FRipGrepParameters.InitRipGrepExePath();

	if SearchPathsHistory.Count = 0 then begin
		SearchPathsHistory.Add(TDirectory.GetCurrentDirectory());
	end;

	if SearchTextsHistory.Count = 0 then begin
		SearchTextsHistory.Add('search text');
	end;

	if RipGrepParamsHistory.Count = 0 then begin
		RipGrepParamsHistory.Add('');
	end;

	if FileMasksHistory.Count = 0 then begin
		FileMasksHistory.Add('');
	end;
end;

procedure TRipGrepperSettings.LoadHistoryEntries(var _list : TStrings; const _section : string);
begin
	for var i := 0 to MAX_HISTORY_COUNT do begin
		var
		s := FIniFile.ReadString(_section, 'Item_' + i.ToString, '');
		if -1 = _list.IndexOf(s) then begin
			_list.Add(s);
		end;
	end;
end;

destructor TRipGrepperSettings.Destroy;
begin
	FRipGrepArguments.Free;
	FRipGrepParamsHistory.Free;
	FSearchTextsHistory.Free;
	FSearchPathsHistory.Free;
	FRipGrepperViewSettings.Free;
	FRipGrepperOpenWithSettings.Free;
	FRipGrepParameters.Free;
	FExtensionSettings.Free;
	FRipGrepperSettings.Free;
	FFileMasksHistory.Free;
	FIniFile.Free;
	inherited;
end;

constructor TRipGrepperSettings.Create;
begin
	{$IFDEF STANDALONE}
	FIniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
	{$ELSE}
	FIniFile := TIniFile.Create(TPath.Combine(IOTAUTils.GetSettingFilePath, EXTENSION_NAME + '.ini'));
	{$ENDIF}
	FRipGrepParameters := TRipGrepParameterSettings.Create(FIniFile);
	FRipGrepperViewSettings := TRipGrepperViewSettings.Create(FIniFile);
	FRipGrepperOpenWithSettings := TRipGrepperOpenWithSettings.Create(FIniFile);
	FExtensionSettings := TRipGrepperExtensionSettings.Create(FIniFile);
	FSearchPathsHistory := TStringList.Create(dupIgnore, False, True);
	FSearchTextsHistory := TStringList.Create(dupIgnore, False, True);
	FRipGrepParamsHistory := TStringList.Create(dupIgnore, False, True);
	FRipGrepArguments := TStringList.Create();
	FRipGrepperSettings := TRipGrepperAppSettings.Create(FIniFile);
	FFileMasksHistory := TStringList.Create(dupIgnore, False, True);
	FIsLoaded := False;
end;

procedure TRipGrepperSettings.AddIfNotContains(_to, _from : TStrings);
begin
	FIsModified := TItemInserter.AddToSringListIfNotContains(_to, _from);
end;

function TRipGrepperSettings.GetIniSectionName : string;
begin
	Result := 'DummySection';
end;

function TRipGrepperSettings.GetIsModified : Boolean;
begin
	Result := FIsModified or FRipGrepParameters.IsModified or FRipGrepperViewSettings.IsModified or FRipGrepperOpenWithSettings.IsModified;
end;

function TRipGrepperSettings.GetSearchPathIsDir : Boolean;
begin
	Result := FSearchPathIsDir;
end;

procedure TRipGrepperSettings.Load;
begin
	try
		FRipGrepParameters.Load;
		FRipGrepperViewSettings.Load;
		FRipGrepperOpenWithSettings.Load;
		FExtensionSettings.Load;
		FRipGrepperSettings.Load;

		LoadHistoryEntries(FSearchPathsHistory, 'SearchPathsHistory');
		LoadHistoryEntries(FSearchTextsHistory, 'SearchTextsHistory');
		LoadHistoryEntries(FRipGrepParamsHistory, 'RipGrepParamsHistory');
		LoadHistoryEntries(FFileMasksHistory, 'FileMasksHistory');
	except
		on E : Exception do begin
			TDebugUtils.DebugMessage(Format('Exception %s ', [E.Message]));
			MessageDlg(E.Message + CRLF + 'Settings load from ' + FIniFile.FileName + ' went wrong.', TMsgDlgType.mtError, [mbOk], 0);
		end;
	end;
	InitSettings;
	FIsLoaded := True;
end;

function TRipGrepperSettings.ReBuildArguments : TStrings;
begin
	Result := FRipGrepParameters.ReBuildArguments;
end;

procedure TRipGrepperSettings.SetFileMasksHistory(const Value : TStrings);
begin
	AddIfNotContains(FFileMasksHistory, Value);
end;

procedure TRipGrepperSettings.SetRipGrepParamsHistory(const Value : TSTrings);
begin
	AddIfNotContains(FRipGrepParamsHistory, Value);
end;

procedure TRipGrepperSettings.SetSearchPathsHistory(const Value : TStrings);
begin
	AddIfNotContains(FSearchPathsHistory, Value);
end;

procedure TRipGrepperSettings.SetSearchTextsHistory(const Value : TStrings);
begin
	AddIfNotContains(FSearchTextsHistory, Value);
end;

procedure TRipGrepperSettings.Store;
begin
	if IsLoaded and IsModified then begin
		FRipGrepperViewSettings.Store;
		FRipGrepperOpenWithSettings.Store;
		FExtensionSettings.Store;
		FRipGrepperSettings.Store;

		if (FRipGrepParameters.IsModified) then begin
			FRipGrepParameters.Store;
			StoreHistoryEntries(SearchPathsHistory, 'SearchPathsHistory');
			StoreHistoryEntries(SearchTextsHistory, 'SearchTextsHistory');
			StoreHistoryEntries(RipGrepParamsHistory, 'RipGrepParamsHistory');
			StoreHistoryEntries(FileMasksHistory, 'FileMasksHistory');
		end;
	end;
end;

procedure TRipGrepperSettings.StoreViewSettings(const _s : string = '');
begin
	FRipGrepperViewSettings.StoreViewSettings(_s);
end;

procedure TRipGrepperSettings.StoreHistoryEntries(const _list : TStrings; const _section : string);
begin
	for var i := _list.Count - 1 downto 0 do begin
		FIniFile.WriteString(_section, 'Item_' + i.ToString, _list[i]);
	end;
end;

constructor TRipGrepParameterSettings.Create(const _ini : TIniFile);
begin
	inherited Create(_ini);
	RipGrepPath := '';
	FRipGrepArguments := TStringList.Create;
end;

destructor TRipGrepParameterSettings.Destroy;
begin
	FRipGrepArguments.Free;
	inherited;
end;

procedure TRipGrepParameterSettings.AddArgs(const _sName : string; const _args : TArray<string>; const _bQuote : Boolean = False);
begin
	for var s : string in _args do begin
		if not s.IsEmpty then begin
			if _bQuote then begin
				FRipGrepArguments.AddPair(_sName, TProcessUtils.MaybeQuoteIfNotQuoted(s));
			end else begin
				FRipGrepArguments.AddPair(_sName, s);
			end;
		end;
	end;
end;

function TRipGrepParameterSettings.BuildCmdLine : string;
var
	cmdLine : TStringList;
begin
	cmdLine := TStringList.Create();
	try
		cmdLine.Add(RipGrepPath);
		cmdLine.AddStrings(RipGrepArguments.GetValues());
		cmdLine.Delimiter := ' ';
		Result := cmdLine.DelimitedText;
	finally
		cmdLine.Free;
	end;
end;

class function TRipGrepParameterSettings.FileMasksToOptions(const _arrMasks, _arrSkipMasks: TArrayEx<string>): string;
var
	newOptions: string;
begin
	for var s in _arrMasks do begin
		if (not string.IsNullOrWhiteSpace(s)) and (not _arrSkipMasks.Contains(s)) then begin
			newOptions := newOptions + ' -g ' + s;
		end;
	end;
	Result := newOptions;
end;

class function TRipGrepParameterSettings.GetFileMaskParamsFromDelimitedText(const _sFileMasksDelimited : string;
	const _sSeparator : string = ';') : string;
begin
	Result := string.Join(' ', GetFileMaskParamsArrFromDelimitedText(_sFileMasksDelimited, _sSeparator));
end;

class function TRipGrepParameterSettings.GetFileMaskParamsFromOptions(const _sOptions : string) : TArray<string>;
begin
	Result := GetFileMasksDelimited(_sOptions, RG_PARAM_REGEX_GLOB).Split([';']);
end;

class function TRipGrepParameterSettings.GetFileMaskParamsArrFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';') : TArray<string>;
var
	list : TStringList;
begin
	list := TStringList.Create(dupIgnore, False, True);
	list.Delimiter := ' ';
	try
		for var s in _sFileMasksDelimited.Split([_sSeparator]) do begin
			list.Add('-g');
			list.Add(s);
		end;
		Result := list.ToStringArray;
	finally
		list.Free;
	end;
end;

class function TRipGrepParameterSettings.GetFileMasksDelimited(const _sOptions, _argMaskRegex : string) : string;
var
	bAddNext : Boolean;
	fileMask : string;
begin
	bAddNext := False;
	for var s in _sOptions.Split([' ']) do begin
		if bAddNext then begin
			fileMask := fileMask + ';' + s;
			bAddNext := False;
		end else begin
			bAddNext := TRegEx.IsMatch(s, _argMaskRegex);
		end;
	end;
	Result := fileMask.Trim([';', ' ']);
end;

class function TRipGrepParameterSettings.RemoveAllParams(const _sOptions, _argMaskRegex : string; const _bSwitch : Boolean = False)
	: string;
var
	arrOptions : TArrayEx<string>;
	arrRemoveIdxs : TArrayEx<integer>;
begin
	arrOptions := _sOptions.Split([' ']);
	for var i := 0 to arrOptions.MaxIndex do begin
		if TRegEx.IsMatch(arrOptions[i], _argMaskRegex) then begin
			arrRemoveIdxs.Add(i);
			if not _bSwitch then begin
				arrRemoveIdxs.Add(i + 1);
			end;
		end;
	end;
	arrOptions.Delete(arrRemoveIdxs);
	Result := string.Join(' ', arrOptions.Items);
end;

function TRipGrepParameterSettings.GetIniSectionName : string;
begin
	Result := INI_SECTION;
end;

class function TRipGrepParameterSettings.GetMissingFileMaskOptions(const _sOptions, _sMasks : string) : string;
var
	existingMasks : TArrayEx<string>;
	masksEdited : TArrayEx<string>;
	newOptions : string;
begin
	existingMasks := TRipGrepParameterSettings.GetFileMaskParamsFromOptions(_sOptions);
	masksEdited := _sMasks.Split([';']);

	newOptions := FileMasksToOptions(masksEdited, existingMasks);
	Result := newOptions.Trim;
end;

procedure TRipGrepParameterSettings.Init;
begin
	inherited;
	CreateSetting(RG_INI_KEY_RGPATH, TRipGrepperSetting.New(vtString, ''));
end;

procedure TRipGrepParameterSettings.InitRipGrepExePath;
var
	rgExists : Boolean;
	rgPath : string;
	scoopInstall : string;
begin
	if RipGrepPath.IsEmpty or (not FileExists(RipGrepPath)) then begin
		rgExists := TFileUtils.FindExecutable('rg.exe', rgPath);
		if not rgExists then begin
			MessageDlg('rg.exe not found', mtError, [mbOk], 0);
			Application.Terminate();
		end;
		scoopInstall := TPath.Combine(GetEnvironmentVariable('SCOOP'), 'apps\ripgrep\current\rg.exe');
		if FileExists(scoopInstall) then begin
			rgPath := scoopInstall;
		end;

		RipGrepPath := rgPath.Trim();
	end;
end;

procedure TRipGrepParameterSettings.Load;
begin
	inherited Load();
	RipGrepPath := LoadSetting(RG_INI_KEY_RGPATH);
end;

function TRipGrepParameterSettings.ReBuildArguments : TStrings;
var
	params : string;
begin
	FRipGrepArguments.Clear();
	params := Options;
	for var s in RG_NECESSARY_PARAMS do begin
		if not params.Contains(s) then begin
			params := s + ' ' + params;
		end;
	end;

	params := params + ' ' + GetFileMaskParamsFromDelimitedText(FileMasks);

	AddArgs(RG_ARG_OPTIONS, params.Split([' ']));

	FRipGrepArguments.AddPair(RG_ARG_SEARCH_TEXT, SearchText);
	AddArgs(RG_ARG_SEARCH_PATH, searchPath.Split([',', ';']), True);
	FRipGrepArguments.Delimiter := ' '; // sArgs.QuoteChar := '"';
	Result := FRipGrepArguments;
end;

procedure TRipGrepParameterSettings.SetFileMasks(const Value : string);
begin
	if FFileMasks <> Value then begin
		FFileMasks := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepParameterSettings.SetOptions(const Value : string);
begin
	if FOptions <> Value then begin
		FOptions := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepParameterSettings.SetSearchPath(const Value : string);
begin
	if FSearchPath <> Value then begin
		FSearchPath := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepParameterSettings.SetSearchText(const Value : string);
begin
	if FSearchText <> Value then begin
		FSearchText := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepParameterSettings.Store;
begin
	StoreSetting(RG_INI_KEY_RGPATH, RipGrepPath);
end;

constructor TRipGrepperViewSettings.Create(const _ini : TIniFile);
begin
	inherited;
end;

function TRipGrepperViewSettings.GetIniSectionName : string;
begin
	Result := INI_SECTION;
end;

procedure TRipGrepperViewSettings.Init;
begin
	inherited;
	CreateSetting('ShowRelativePath', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('ShowFileIcon', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('AlternateRowColors', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('IndentLines', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('ExpandNodes', TRipGrepperSetting.New(vtBoolean, True));
end;

procedure TRipGrepperViewSettings.Load;
begin
	inherited Load();
	ShowRelativePath := LoadSetting('ShowRelativePath');
	ShowFileIcon := LoadSetting('ShowFileIcon');
	AlternateRowColors := LoadSetting('AlternateRowColors');
	IndentLines := LoadSetting('IndentLines');
	ExpandNodes := LoadSetting('ExpandNodes');

	FIsLoaded := True;
end;

procedure TRipGrepperViewSettings.Store;
begin
	if IsLoaded and IsModified then begin
		StoreViewSettings('');
		FIsModified := False;
	end;
	inherited Store();
end;

procedure TRipGrepperViewSettings.StoreViewSettings(const _s : string = '');
var
	i : integer;
begin
	i := 0;
	if _s.IsEmpty then begin
		// store all
		for i := 0 to high(VIEW_SETTINGS) do begin
			StoreViewSettings(VIEW_SETTINGS[i]);
		end;
	end else if MatchStr(_s, VIEW_SETTINGS[i]) then begin
		StoreSetting(VIEW_SETTINGS[i], ShowRelativePath);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		StoreSetting(VIEW_SETTINGS[i], ShowFileIcon);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		StoreSetting(VIEW_SETTINGS[i], AlternateRowColors);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		StoreSetting(VIEW_SETTINGS[i], IndentLines);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		StoreSetting(VIEW_SETTINGS[i], ExpandNodes);
	end else begin
		raise Exception.Create('Settings: ' + _s + ' not stored!');
	end;
	IsModified := True;
end;

constructor TRipGrepperSettingsInstance.Create();
begin
	inherited;
	FInstance := nil;
end;

class destructor TRipGrepperSettingsInstance.Destroy;
begin
	//
end;

class procedure TRipGrepperSettingsInstance.FreeInstance;
begin
	FInstance.Free;
end;

class function TRipGrepperSettingsInstance.GetInstance : TRipGrepperSettings;
begin
	if Assigned(FInstance) then begin
		FInstance := TRipGrepperSettings.Create;
	end;
	Result := FInstance;
end;

constructor TRipGrepperOpenWithSettings.Create(const _ini : TIniFile);
begin
	inherited;
	FCommandList := TStringList.Create;
end;

destructor TRipGrepperOpenWithSettings.Destroy;
begin
	FCommandList.Free;
	inherited;
end;

function TRipGrepperOpenWithSettings.GetCommand(Index : Integer) : string;
begin
	Result := '';
	if FCommandList.Count > index then begin
		Result := FCommandList[index];
	end;
end;

function TRipGrepperOpenWithSettings.GetIniSectionName : string;
begin
	Result := OPEN_WITH_SETTINGS;
end;

procedure TRipGrepperOpenWithSettings.Load;
var
	s : string;
begin
	for var i : integer := 0 to MAX_COMMAND_NUM do begin
		s := FIniFile.ReadString(OPEN_WITH_SETTINGS, OPENWITH_COMMAND_KEY + i.ToString, '');
		if (not s.IsEmpty) then begin
			Command[i] := s;
		end else begin
			break
		end;
	end;
	FIsLoaded := True;
end;

procedure TRipGrepperOpenWithSettings.SetCommand(Index : Integer; const Value : string);
begin
	if Value.IsEmpty then
		Exit;

	if FCommandList.Count > index then begin
		if (FCommandList[index] <> Value) then begin
			FCommandList[index] := Value;
			IsModified := True;
		end;
	end else begin
		FCommandList.Add(Value);
		IsModified := True;
	end;
end;

procedure TRipGrepperOpenWithSettings.Store;
var
	s : string;
begin
	if IsLoaded and IsModified then begin
		if FCommandList.Count > 0 then begin
			for var i : integer := 0 to MAX_COMMAND_NUM do begin
				s := Command[i];
				if s.IsEmpty then
					break;
				FIniFile.WriteString(OPEN_WITH_SETTINGS, OPENWITH_COMMAND_KEY + i.ToString, s);
			end;
		end;
		FIsModified := False;
	end;
end;

function TRipGrepperExtensionSettings.GetIniSectionName : string;
begin
	Result := INI_SECTION;
end;

procedure TRipGrepperExtensionSettings.Load;

begin
	{$IFNDEF STANDALONE}
	if Assigned(FIniFile) then begin
		DripGrepperShortCut := FIniFile.ReadString(INI_SECTION, 'DripGrepperShortCut', '');
		if DripGrepperShortCut = '' then begin
			DripGrepperShortCut := ShortCutToText(ShortCut(Word('R'), [ssShift, ssAlt]));
			FIsModified := True;
		end;
		FIsLoaded := True;
	end else begin
		raise Exception.Create('Settings file is nil!')
	end;
	{$ENDIF}
end;

procedure TRipGrepperExtensionSettings.Store;
begin
	{$IFNDEF STANDALONE}
	if IsLoaded and IsModified then begin
		FIniFile.WriteString(INI_SECTION, 'DripGrepperShortCut', DripGrepperShortCut);
		FIsModified := False;
	end;
	{$ENDIF}
end;

function TRipGrepperAppSettings.GetIniSectionName : string;
begin
	Result := INI_SECTION;
end;

procedure TRipGrepperAppSettings.Init;
begin
	inherited;
	CreateSetting('DebugTrace', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('ExpertMode', TRipGrepperSetting.New(vtBoolean, True));
end;

procedure TRipGrepperAppSettings.Load;
begin
	inherited Load();
	FExpertMode := LoadSetting('ExpertMode');
	FDebugTrace := LoadSetting('DebugTrace');
end;

procedure TRipGrepperAppSettings.Store;
begin
	StoreSetting('ExpertMode', FExpertMode);
	StoreSetting('DebugTrace', FDebugTrace);
	inherited Store();
end;

initialization

GSettings := TRipGrepperSettings.Create;
GSettings.Load;

finalization

GSettings.Store;
GSettings.Free;

end.
