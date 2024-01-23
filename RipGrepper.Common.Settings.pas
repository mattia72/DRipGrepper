unit RipGrepper.Common.Settings;

interface

uses
	System.Classes,
	System.IniFiles,
	System.Generics.Collections;

type
	TRipGrepParameterSettings = class
		private
			FRipGrepArguments : TStrings;
			FRipGrepParam : string;
			FRipGrepPath : string;
			FSearchPath : string;
			FSearchText : string;
			procedure AddArgs(const _args : TArray<string>; const _bQuote : Boolean = False);

		public
			constructor Create;
			destructor Destroy; override;
			function BuildCmdLine : string;
			procedure InitRipGrepExePath;
			function ReBuildArguments : TStrings;
			property RipGrepParam : string read FRipGrepParam write FRipGrepParam;
			property SearchPath : string read FSearchPath write FSearchPath;
			property SearchText : string read FSearchText write FSearchText;
			property RipGrepArguments : TStrings read FRipGrepArguments write FRipGrepArguments;
			property RipGrepPath : string read FRipGrepPath write FRipGrepPath;
	end;

	TRipGrepperViewSettings = class
		const
			VIEW_SETTINGS : array [0 .. 3] of string = ('ShowRelativePath', 'ShowFileIcon', 'AlternateRowColors', 'IndentLines');

		var

		private
			FAlternateRowColors : Boolean;

		public
			IndentLines : Boolean;
			ShowFileIcon : Boolean;
			ShowRelativePath : Boolean;
			procedure StoreViewSettings(_ini : TIniFile; const _s : string = '');
			property AlternateRowColors : Boolean read FAlternateRowColors write FAlternateRowColors;

			procedure Init;
	end;

	TRipGrepperSettingsHistory = class
		const
			MAX_HISTORY_COUNT = 20;

		var
		private
		var
			FIsLoaded : Boolean;
			FSettingsFile : TIniFile;
			FRipGrepParameters : TRipGrepParameterSettings;
			FRipGrepperViewSettings : TRipGrepperViewSettings;
			FRipGrepParamsHistory : TSTrings;
			FSearchPathsHistory : TStrings;
			FSearchTextsHistory : TStrings;
			FRipGrepArguments : TStrings;

			function GetActualRipGrepParam : string;
			function GetActualSearchPath : string;
			function GetActualSearchText : string;
			function GetIsEmpty : Boolean;
			procedure InitSettings;
			procedure LoadHistoryEntries(var _list : TStrings; const _section : string);
			procedure StoreHistoryEntries(const _list : TStrings; const _section : string);

		public
			procedure Load;
			procedure Store;
			procedure StoreViewSettings(const _s : string = '');
			destructor Destroy; override;
			constructor Create;
			function GetRipGrepArguments : TStrings;
			function ReBuildArguments : TStrings;
			property ActualRipGrepParam : string read GetActualRipGrepParam;
			property ActualSearchPath : string read GetActualSearchPath;
			property ActualSearchText : string read GetActualSearchText;
			property IsEmpty : Boolean read GetIsEmpty;

			property IsLoaded : Boolean read FIsLoaded;
			property RipGrepParameters : TRipGrepParameterSettings read FRipGrepParameters write FRipGrepParameters;
			property SearchPathsHistory : TStrings read FSearchPathsHistory;
			property RipGrepParamsHistory : TSTrings read FRipGrepParamsHistory;
			property RipGrepperViewSettings : TRipGrepperViewSettings read FRipGrepperViewSettings write FRipGrepperViewSettings;
			property SearchTextsHistory : TStrings read FSearchTextsHistory;
	end;

implementation

uses
	System.SysUtils,
	Vcl.Forms,
	System.StrUtils,
	RipGrepper.Common.Types,
	RipGrepper.Helper.Types,
	RipGrepper.Tools.DebugTools,
	RipGrepper.Tools.FileUtils,
	Vcl.Dialogs,
	System.IOUtils,
	Winapi.Windows,
	System.UITypes,
	RipGrepper.Tools.ProcessUtils;

function TRipGrepperSettingsHistory.GetActualRipGrepParam : string;
begin
	RipGrepParamsHistory.TryGetDef(0, Result);
end;

function TRipGrepperSettingsHistory.GetActualSearchPath : string;
begin
	SearchPathsHistory.TryGetDef(0, Result);
end;

function TRipGrepperSettingsHistory.GetActualSearchText : string;
begin
	SearchTextsHistory.TryGetDef(0, Result);
end;

function TRipGrepperSettingsHistory.GetIsEmpty : Boolean;
begin
	Result := FRipGrepParameters.RipGrepPath.IsEmpty;
end;

function TRipGrepperSettingsHistory.GetRipGrepArguments : TStrings;
begin
	Result := FRipGrepParameters.RipGrepArguments;
end;

procedure TRipGrepperSettingsHistory.InitSettings;
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
end;

procedure TRipGrepperSettingsHistory.LoadHistoryEntries(var _list : TStrings; const _section : string);
begin
	for var i := 0 to MAX_HISTORY_COUNT do begin
		var
		s := FSettingsFile.ReadString(_section, 'Item_' + i.ToString, '');
		if s <> '' then begin
			_list.Add(s);
		end
		else
			break;
	end;
end;

destructor TRipGrepperSettingsHistory.Destroy;
begin
	FRipGrepArguments.Free;
	FRipGrepParamsHistory.Free;
	FSearchTextsHistory.Free;
	FSearchPathsHistory.Free;
	FRipGrepperViewSettings.Free;
	FRipGrepParameters.Free;
	FSettingsFile.Free;
end;

constructor TRipGrepperSettingsHistory.Create;
begin
	FSettingsFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
	FRipGrepParameters := TRipGrepParameterSettings.Create();
	FRipGrepperViewSettings := TRipGrepperViewSettings.Create();
	FRipGrepperViewSettings.Init();
	FSearchPathsHistory := TStringList.Create;
	FSearchTextsHistory := TStringList.Create;
	FRipGrepParamsHistory := TStringList.Create;
	FRipGrepArguments := TStringList.Create();
	FIsLoaded := False;
end;

procedure TRipGrepperSettingsHistory.Load;
begin
	FRipGrepParameters.RipGrepPath := FSettingsFile.ReadString('RipGrepSettings', 'Path', '');

	FRipGrepperViewSettings.ShowRelativePath := FSettingsFile.ReadBool('RipGrepperSettings', 'ShowRelativePath', False);
	FRipGrepperViewSettings.ShowFileIcon := FSettingsFile.ReadBool('RipGrepperSettings', 'ShowFileIcon', False);
	FRipGrepperViewSettings.AlternateRowColors := FSettingsFile.ReadBool('RipGrepperSettings', 'AlternateRowColors', False);
	FRipGrepperViewSettings.IndentLines := FSettingsFile.ReadBool('RipGrepperSettings', 'IndentLines', False);

	LoadHistoryEntries(FSearchPathsHistory, 'SearchPathsHistory');
	LoadHistoryEntries(FSearchTextsHistory, 'SearchTextsHistory');
	LoadHistoryEntries(FRipGrepParamsHistory, 'RipGrepParamsHistory');

	InitSettings;
	FIsLoaded := True;
end;

function TRipGrepperSettingsHistory.ReBuildArguments : TStrings;
begin
	Result := FRipGrepParameters.ReBuildArguments;
end;

procedure TRipGrepperSettingsHistory.Store;
begin
	if IsLoaded then begin
		FSettingsFile.WriteString('RipGrepSettings', 'Path', FRipGrepParameters.RipGrepPath);
		StoreViewSettings();
		StoreHistoryEntries(SearchPathsHistory, 'SearchPathsHistory');
		StoreHistoryEntries(SearchTextsHistory, 'SearchTextsHistory');
		StoreHistoryEntries(RipGrepParamsHistory, 'RipGrepParamsHistory');
	end;
end;

procedure TRipGrepperSettingsHistory.StoreViewSettings(const _s : string = '');
begin
	FRipGrepperViewSettings.StoreViewSettings(FSettingsFile, _s);
end;

procedure TRipGrepperSettingsHistory.StoreHistoryEntries(const _list : TStrings; const _section : string);
begin
	for var i := _list.Count - 1 downto 0 do begin
		FSettingsFile.WriteString(_section, 'Item_' + i.ToString, _list[i]);
	end;
end;

constructor TRipGrepParameterSettings.Create;
begin
	inherited;
	RipGrepPath := '';
	FRipGrepArguments := TStringList.Create;
end;

destructor TRipGrepParameterSettings.Destroy;
begin
	FRipGrepArguments.Free;
	inherited;
end;

procedure TRipGrepParameterSettings.AddArgs(const _args : TArray<string>; const _bQuote : Boolean = False);
begin
	for var s : string in _args do begin
		if not s.IsEmpty then begin
			if _bQuote then begin
				FRipGrepArguments.Add(TProcessUtils.MaybeQuoteIfNotQuoted(s));
			end else begin
				FRipGrepArguments.Add(s);
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
		cmdLine.AddStrings(RipGrepArguments);
		cmdLine.Delimiter := ' ';
		Result := cmdLine.DelimitedText;
	finally
		cmdLine.Free;
	end;
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

function TRipGrepParameterSettings.ReBuildArguments : TStrings;
var
	params : string;
begin
	FRipGrepArguments.Clear();
	params := RipGrepParam;
	for var s in RG_NECESSARY_PARAMS do begin
		if not params.Contains(s) then begin
			params := s + ' ' + params;
		end;
	end;

	AddArgs(params.Split([' ']));

	FRipGrepArguments.Add(SearchText);

	AddArgs(searchPath.Split([',',';']), True);

	FRipGrepArguments.Delimiter := ' '; // sArgs.QuoteChar := '"';

	Result := FRipGrepArguments;
end;

procedure TRipGrepperViewSettings.Init;
begin
	ShowRelativePath := False;
	ShowFileIcon := False;
	AlternateRowColors := False;
	IndentLines := False;
end;

procedure TRipGrepperViewSettings.StoreViewSettings(_ini : TIniFile; const _s : string = '');
var
	i : integer;
begin
	i := 0;
	if _s.IsEmpty then begin
		// store all
		for i := 0 to high(VIEW_SETTINGS) do begin
			StoreViewSettings(_ini, VIEW_SETTINGS[i]);
		end;
	end else if MatchStr(_s, VIEW_SETTINGS[i]) then begin
		_ini.WriteBool('RipGrepperSettings', VIEW_SETTINGS[i], ShowRelativePath);
		TDebugUtils.DebugMessage(VIEW_SETTINGS[i] + ' stored');
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		_ini.WriteBool('RipGrepperSettings', VIEW_SETTINGS[i], ShowFileIcon);
		TDebugUtils.DebugMessage(VIEW_SETTINGS[i] + ' stored');
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		_ini.WriteBool('RipGrepperSettings', VIEW_SETTINGS[i], AlternateRowColors);
		TDebugUtils.DebugMessage(VIEW_SETTINGS[i] + ' stored');
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		_ini.WriteBool('RipGrepperSettings', VIEW_SETTINGS[i], IndentLines);
		TDebugUtils.DebugMessage(VIEW_SETTINGS[i] + ' stored');
	end else begin
		raise Exception.Create('Settings: ' + _s + ' not stored!');
	end;
end;

end.
