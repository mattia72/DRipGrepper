unit RipGrepper.Common.Settings.RipGrepParameterSettings;

interface

uses
	RipGrepper.Common.Settings.Base,
	System.Classes,
	System.IniFiles,
	RipGrepper.Common.Constants,
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
			class function FileMasksToOptions(const _arrMasks, _arrSkipMasks : TArrayEx<string>) : string;
			class function GetFileMaskParamsFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';')
				: string; overload;
			class function GetFileMaskParamsFromOptions(const _sOptions : string) : TArray<string>;
			class function GetFileMaskParamsArrFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';')
				: TArray<string>; overload;
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

implementation

uses
	System.SysUtils,
	Vcl.Dialogs,
	RipGrepper.Tools.FileUtils,
	System.IOUtils,
	Vcl.Forms,
	RipGrepper.Tools.ProcessUtils,
	System.RegularExpressions,
	RipGrepper.Helper.Types;

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

class function TRipGrepParameterSettings.FileMasksToOptions(const _arrMasks, _arrSkipMasks : TArrayEx<string>) : string;
var
	newOptions : string;
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

class function TRipGrepParameterSettings.GetFileMaskParamsArrFromDelimitedText(const _sFileMasksDelimited : string;
	const _sSeparator : string = ';') : TArray<string>;
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

end.
