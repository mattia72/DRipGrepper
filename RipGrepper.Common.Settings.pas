unit RipGrepper.Common.Settings;

interface

uses
	System.Classes,
	System.IniFiles,
	System.Generics.Collections;

type
	TRipGrepperSettings = record
		const
			MAX_HISTORY_COUNT = 20;
			VIEW_SETTINGS : array [0 .. 3] of string = ('ShowRelativePath', 'ShowFileIcon', 'AlternateRowColors', 'IndentLines');

		var
			SettingsFile : TIniFile;
			RipGrepPath : string;
			ShowRelativePath : Boolean;
			ShowFileIcon : Boolean;
			AlternateRowColors : Boolean;
			IndentLines : Boolean;
			IsLoaded : Boolean;

		private
		var
			FRipGrepParamsHistory : TSTrings;
			FSearchPathsHistory : TStrings;
			FSearchTextsHistory : TStrings;
			FRipGrepArguments : TStrings;

			function GetActualRipGrepParam : string;
			function GetActualSearchPath : string;
			function GetActualSearchText : string;
			function GetIsEmpty : Boolean;
			function GetRipGrepArguments : TStrings;
			procedure InitSettings;
			procedure LoadHistoryEntries(var _list : TStrings; const _section : string);
			procedure StoreHistoryEntries(const _list : TStrings; const _section : string);

		public
			procedure Load;
			function ReBuildArguments : TStrings;
			procedure Store;
			procedure StoreViewSettings(const _s : string = '');
			class operator Finalize(var Dest : TRipGrepperSettings);
			class operator Initialize(out Dest : TRipGrepperSettings);
			property ActualRipGrepParam : string read GetActualRipGrepParam;
			property ActualSearchPath : string read GetActualSearchPath;
			property ActualSearchText : string read GetActualSearchText;
			property IsEmpty : Boolean read GetIsEmpty;
			property RipGrepArguments : TStrings read GetRipGrepArguments;
			property SearchPathsHistory : TStrings read FSearchPathsHistory;
			property RipGrepParamsHistory : TSTrings read FRipGrepParamsHistory;
			property SearchTextsHistory : TStrings read FSearchTextsHistory;
	end;

	PRipGrepperSettings = ^TRipGrepperSettings;

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

function TRipGrepperSettings.GetActualRipGrepParam : string;
begin
	RipGrepParamsHistory.TryGetDef(0, Result);
end;

function TRipGrepperSettings.GetActualSearchPath : string;
begin
	SearchPathsHistory.TryGetDef(0, Result);
end;

function TRipGrepperSettings.GetActualSearchText : string;
begin
	SearchTextsHistory.TryGetDef(0, Result);
end;

function TRipGrepperSettings.GetIsEmpty : Boolean;
begin
	Result := RipGrepPath.IsEmpty;
end;

function TRipGrepperSettings.GetRipGrepArguments : TStrings;
begin
	Result := FRipGrepArguments;
end;

procedure TRipGrepperSettings.InitSettings;
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

procedure TRipGrepperSettings.LoadHistoryEntries(var _list : TStrings; const _section : string);
begin
	for var i := 0 to MAX_HISTORY_COUNT do begin
		var
		s := SettingsFile.ReadString(_section, 'Item_' + i.ToString, '');
		if s <> '' then begin
			_list.Add(s);
		end
		else
			break;
	end;
end;

class operator TRipGrepperSettings.Finalize(var Dest : TRipGrepperSettings);
begin
	Dest.FSearchPathsHistory.Free;
	Dest.FSearchTextsHistory.Free;
	Dest.FRipGrepParamsHistory.Free;
	Dest.FRipGrepArguments.Free;
end;

class operator TRipGrepperSettings.Initialize(out Dest : TRipGrepperSettings);
begin
	Dest.SettingsFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
	Dest.RipGrepPath := '';
	Dest.ShowRelativePath := False;
	Dest.ShowFileIcon := False;
	Dest.AlternateRowColors := False;
	Dest.IndentLines := False;
	Dest.FSearchPathsHistory := TStringList.Create;
	Dest.FSearchTextsHistory := TStringList.Create;
	Dest.FRipGrepParamsHistory := TStringList.Create;
	Dest.FRipGrepArguments := TStringList.Create();
	Dest.IsLoaded := False;
end;

procedure TRipGrepperSettings.Load;
begin
	RipGrepPath := SettingsFile.ReadString('RipGrepSettings', 'Path', '');

	ShowRelativePath := SettingsFile.ReadBool('RipGrepperSettings', 'ShowRelativePath', False);
	ShowFileIcon := SettingsFile.ReadBool('RipGrepperSettings', 'ShowFileIcon', False);
	AlternateRowColors := SettingsFile.ReadBool('RipGrepperSettings', 'AlternateRowColors', False);
	IndentLines := SettingsFile.ReadBool('RipGrepperSettings', 'IndentLines', False);

	LoadHistoryEntries(FSearchPathsHistory, 'SearchPathsHistory');
	LoadHistoryEntries(FSearchTextsHistory, 'SearchTextsHistory');
	LoadHistoryEntries(FRipGrepParamsHistory, 'RipGrepParamsHistory');

	InitSettings;
	IsLoaded := True;
end;

function TRipGrepperSettings.ReBuildArguments : TStrings;
var
	paramsArr : TArray<string>;
	params : string;
begin
	params := ActualRipGrepParam;
	FRipGrepArguments.Clear();
	for var s in RG_NECESSARY_PARAMS do begin
		if not params.Contains(s) then begin
			params := s + ' ' + params;
		end;
	end;

	paramsArr := params.Split([' ']);
	for var s : string in paramsArr do begin
		if not s.IsEmpty then begin
			FRipGrepArguments.Add(s);
		end;
	end;

	FRipGrepArguments.Add(ActualSearchText);

	var
	searchPath := TProcessUtils.MaybeQuoteIfNotQuoted(ActualSearchPath);
	FRipGrepArguments.Add(searchPath);
	FRipGrepArguments.Delimiter := ' '; // sArgs.QuoteChar := '"';

	Result := FRipGrepArguments;
end;

procedure TRipGrepperSettings.Store;
begin
	if IsLoaded then begin
		SettingsFile.WriteString('RipGrepSettings', 'Path', RipGrepPath);
		StoreViewSettings();
		StoreHistoryEntries(SearchPathsHistory, 'SearchPathsHistory');
		StoreHistoryEntries(SearchTextsHistory, 'SearchTextsHistory');
		StoreHistoryEntries(RipGrepParamsHistory, 'RipGrepParamsHistory');
	end;
end;

procedure TRipGrepperSettings.StoreViewSettings(const _s : string = '');
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
		SettingsFile.WriteBool('RipGrepperSettings', VIEW_SETTINGS[i], ShowRelativePath);
		TDebugUtils.DebugMessage(VIEW_SETTINGS[i] + ' stored');
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		SettingsFile.WriteBool('RipGrepperSettings', VIEW_SETTINGS[i], ShowFileIcon);
		TDebugUtils.DebugMessage(VIEW_SETTINGS[i] + ' stored');
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		SettingsFile.WriteBool('RipGrepperSettings', VIEW_SETTINGS[i], AlternateRowColors);
		TDebugUtils.DebugMessage(VIEW_SETTINGS[i] + ' stored');
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		SettingsFile.WriteBool('RipGrepperSettings', VIEW_SETTINGS[i], IndentLines);
		TDebugUtils.DebugMessage(VIEW_SETTINGS[i] + ' stored');
	end else begin
		raise Exception.Create('Settings: ' + _s + ' not stored!');
	end;
end;

procedure TRipGrepperSettings.StoreHistoryEntries(const _list : TStrings; const _section : string);
begin
	for var i := _list.Count - 1 downto 0 do begin
		SettingsFile.WriteString(_section, 'Item_' + i.ToString, _list[i]);
	end;
end;

end.
