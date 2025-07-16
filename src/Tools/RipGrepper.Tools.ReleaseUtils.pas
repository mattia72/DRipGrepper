unit RipGrepper.Tools.ReleaseUtils;

interface

uses
	System.JSON,
	REST.Types,
	REST.Client,
	Spring,
	ArrayEx;

type
	TReleaseInfo = class
		public
			Description : string;
			Version : string;
			HtmlURL : string;
			PublishedAt : TDateTime;
			LoadOk : Boolean;
			procedure FromJson(_json : TJSONValue; const _idx : Integer);
			procedure SetDescription(const _body : string);
	end;

	IReleaseInfo = IShared<TReleaseInfo>;
	TReleaseInfoArray = TArrayEx<IReleaseInfo>;

	TReleaseUtils = record
		private
			FCurrentRelease : IReleaseInfo;
			FDownloadedReleaseInfos : TReleaseInfoArray;
			FCurrentNameWithVersion : string;
			FCurrentName : string;
			FCurrentVersion : string;
			FDownloadErrorMsg : string;
			FLatestVersion : string;
			function DownloadReleaseInfoFromGithub : TReleaseInfoArray;
			function GetCurrentRelease() : IReleaseInfo;
			function GetCurrentVersion() : string;
			function GetDownloadedReleaseInfos : TReleaseInfoArray;
			function GetLatestRelease : IReleaseInfo;
			function GetLatestVersion : string;
			class function GetModuleVersion(Instance : THandle; out iMajor, iMinor, iRelease, iBuild : Integer) : Boolean; static;
			class procedure InitRestClient(_restClient : TRESTClient; _restRequest : TRESTRequest; _restResponse : TRESTResponse); static;

		public
			procedure DownloadReleaseInfos;
			class function GetAppDirectory() : string; static;
			class function GetAppNameAndVersion(const _exePath : string) : string; static;
			function GetCurrentNameWithVersion() : string;
			function GetCurrentName() : string;
			class function GetFileVersion(const _fullPath : string) : string; static;
			class function GetRunningModuleVersion() : string; static;
			class function GetModuleNameAndVersion() : string; static;
			class function GetRunningModulePath() : string; static;
			function IsCurrentTheLatest : Boolean;
			class function IsSameVersion(_ri1, _ri2 : IReleaseInfo) : Boolean; static;
			procedure ShowNewVersionMsgBox(const _bOnlyIfUpdateAvailable : Boolean = False);
			function TryGetCurrentRelInfo(var _curInfo : IReleaseInfo) : Boolean;
			property CurrentNameWithVersion : string read GetCurrentNameWithVersion;
			property CurrentName : string read GetCurrentName;
			property CurrentRelease : IReleaseInfo read GetCurrentRelease;
			property CurrentVersion : string read GetCurrentVersion;
			property DownloadedReleaseInfos : TReleaseInfoArray read GetDownloadedReleaseInfos write FDownloadedReleaseInfos;
			property DownloadErrorMsg : string read FDownloadErrorMsg write FDownloadErrorMsg;
			property LatestRelease : IReleaseInfo read GetLatestRelease;
			property LatestVersion : string read GetLatestVersion;
	end;

implementation

uses
	RipGrepper.Tools.FileUtils,
	Vcl.Forms,
	System.SysUtils,
	System.IOUtils,
	RipGrepper.Common.Constants,
	Winapi.Windows,
	System.Classes,
	RipGrepper.Tools.DebugUtils,
	System.RegularExpressions,
	RipGrepper.Helper.UI,
	System.UITypes;

procedure TReleaseInfo.FromJson(_json : TJSONValue; const _idx : Integer);
begin
	LoadOk := False;
	var
	item := _json.A[_idx];
	HtmlURL := item.GetValue<string>('html_url');
	Version := item.GetValue<string>('name');
	PublishedAt := item.GetValue<TDateTime>('published_at');
	SetDescription(item.GetValue<string>('body'));
	LoadOk := True;
end;

procedure TReleaseInfo.SetDescription(const _body : string);
var
	bIsComment : Boolean;
	lines : TArrayEx<string>;
begin
	bIsComment := False;
	for var l : string in _body.Split([CRLF]) do begin
		if bIsComment or TRegEx.IsMatch(l, '^\s*<!--') then begin
			bIsComment := True;
		end;
		if TRegEx.IsMatch(l, '-->\s*$') then begin
			bIsComment := False;
			continue
		end;
		if not bIsComment then begin
			lines.Add(l);
		end;
	end;

	Description := string.Join(CRLF, lines.Items);
end;

function TReleaseUtils.DownloadReleaseInfoFromGithub : TReleaseInfoArray;
var
	jValue : TJSONValue;
	restClient : TRESTClient;
	restRequest : TRESTRequest;
	restResponse : TRESTResponse;
	cursor : TCursorSaver;
begin
	cursor.SetHourGlassCursor();
	restClient := TRESTClient.Create(nil);
	restRequest := TRESTRequest.Create(nil);
	restResponse := TRESTResponse.Create(nil);
	try
		InitRestClient(restClient, restRequest, restResponse);
		try
			FDownloadErrorMsg := '';
			restRequest.Execute;
			jValue := restResponse.JSONValue;

			for var idx := 0 to (jValue as TJSONArray).Count - 1 do begin
				var
				ri := Shared.Make<TReleaseInfo>();
				ri.FromJson(jValue, idx);
				FDownloadedReleaseInfos.Add(ri);
			end;
		except
			on E : Exception do begin
				FDownloadErrorMsg := 'Can''t load release infos from GitHub.' + CRLF2 + 'Error: ' + E.Message;
				TMsgBox.ShowError(FDownloadErrorMsg);
			end;
		end;

	finally
		restClient.Free;
		restRequest.Free;
		restResponse.Free;
	end;
end;

procedure TReleaseUtils.DownloadReleaseInfos();
begin
	if FDownloadedReleaseInfos.Count <= 1 then begin
		DownloadReleaseInfoFromGithub();
		if FDownloadedReleaseInfos.IsEmpty then begin
			var
			cr := CurrentRelease;
			cr.LoadOk := False;
			FDownloadedReleaseInfos.Add(cr);
		end;
	end;
end;

class function TReleaseUtils.GetAppDirectory() : string;
begin
	{$IF CompilerVersion >= 360}
	Result := TPath.GetAppDirectory;
	{$ELSE}
	Result := TPath.GetDirectoryName(TReleaseUtils.GetRunningModulePath);
	{$ENDIF}
end;

class function TReleaseUtils.GetAppNameAndVersion(const _exePath : string) : string;
var
	name : string;
	sVersion : string;
begin
	name := TPath.GetFileNameWithoutExtension(_exePath);
	sVersion := GetRunningModuleVersion();
	Result := Format(FORMAT_NAME_VERSION_INFO, [name, APP_PLATFORM, sVersion]);
end;

function TReleaseUtils.GetCurrentNameWithVersion() : string;
begin
	if FCurrentNameWithVersion.IsEmpty then begin
		{$IFDEF STANDALONE}
		FCurrentNameWithVersion := TReleaseUtils.GetAppNameAndVersion(Application.ExeName);
		{$ELSE}
		FCurrentNameWithVersion := TReleaseUtils.GetModuleNameAndVersion();
		{$ENDIF}
	end;
	Result := FCurrentNameWithVersion;
end;

function TReleaseUtils.GetCurrentName() : string;
begin
	if FCurrentName.IsEmpty then begin
		{$IFDEF STANDALONE}
		FCurrentName := TPath.GetFileNameWithoutExtension(Application.ExeName);
		{$ELSE}
		var
			modulePath : string;
		modulePath := GetRunningModulePath();
		FCurrentName := TPath.GetFileNameWithoutExtension(modulePath);
		{$ENDIF}
	end;
	Result := FCurrentName;
end;

function TReleaseUtils.GetCurrentRelease() : IReleaseInfo;
begin
	if not Assigned(FCurrentRelease) then begin
		FCurrentRelease := Shared.Make<TReleaseInfo>();
		FCurrentRelease.Version := CurrentVersion;
		FCurrentRelease.LoadOk := True;
	end;
	Result := FCurrentRelease;
end;

function TReleaseUtils.GetCurrentVersion() : string;
begin
	if FCurrentVersion.IsEmpty then begin
		FCurrentVersion := GetRunningModuleVersion()
	end;
	Result := FCurrentVersion;
end;

function TReleaseUtils.GetDownloadedReleaseInfos : TReleaseInfoArray;
begin
	if FDownloadedReleaseInfos.IsEmpty then begin
		DownloadReleaseInfos();
	end;
	Result := FDownloadedReleaseInfos;
end;

function TReleaseUtils.GetLatestVersion : string;
begin
	if FLatestVersion.IsEmpty then begin
		FLatestVersion := LatestRelease.Version;
	end;
	Result := FLatestVersion;
end;

class function TReleaseUtils.GetRunningModuleVersion() : string;
var
	imajor : integer;
	iminor : integer;
	irelease : integer;
	ibuild : integer;
	h : THandle;
begin
	// TODO Exe Path to Handle
	h := 0;
	GetModuleVersion(h, imajor, iminor, irelease, ibuild);
	Result := Format(FORMAT_VERSION_INFO, [imajor, iminor, irelease, ibuild]);
end;

class function TReleaseUtils.GetModuleNameAndVersion() : string;
var
	modulePath : string;
begin
	modulePath := GetRunningModulePath();
	Result := GetAppNameAndVersion(modulePath);
end;

class function TReleaseUtils.GetModuleVersion(Instance : THandle; out iMajor, iMinor, iRelease, iBuild : Integer) : Boolean;
var
	fileInformation : PVSFIXEDFILEINFO;
	verlen : Cardinal;
	rs : TResourceStream;
	m : TMemoryStream;
	resource : HRSRC;
begin
	Result := False;
	// You said zero, but you mean "us"
	if Instance = 0 then
		Instance := HInstance;

	// UPDATE: Workaround bug in Delphi if resource doesn't exist
	resource := FindResource(Instance, PWideChar(1), RT_VERSION);
	if resource = 0 then begin
		iMajor := 0;
		iMinor := 0;
		iRelease := 0;
		iBuild := 0;
		Result := False;
		Exit;
	end;

	m := TMemoryStream.Create;
	try
		rs := TResourceStream.CreateFromID(Instance, 1, RT_VERSION);
		try
			m.CopyFrom(rs, rs.Size);
		finally
			rs.Free;
		end;

		m.Position := 0;
		if not VerQueryValue(m.Memory, '\', (* var *) Pointer(fileInformation), (* var *) verlen) then begin
			iMajor := 0;
			iMinor := 0;
			iRelease := 0;
			iBuild := 0;
			Exit;
		end;

		iMajor := fileInformation.dwFileVersionMS shr 16;
		iMinor := fileInformation.dwFileVersionMS and $FFFF;
		iRelease := fileInformation.dwFileVersionLS shr 16;
		iBuild := fileInformation.dwFileVersionLS and $FFFF;
	finally
		m.Free;
	end;

	Result := True;
end;

class function TReleaseUtils.GetFileVersion(const _fullPath : string) : string;
var
	infoSize : DWORD;
	verBuf : pointer;
	verSize : UINT;
	wnd : UINT;
	FixedFileInfo : PVSFixedFileInfo;
begin
	infoSize := GetFileVersioninfoSize(PChar(_fullPath), wnd);

	result := '';

	if infoSize <> 0 then begin
		GetMem(verBuf, infoSize);
		try
			if GetFileVersionInfo(PChar(_fullPath), wnd, infoSize, verBuf) then begin
				VerQueryValue(verBuf, '\', Pointer(FixedFileInfo), verSize);

				result := IntToStr(FixedFileInfo.dwFileVersionMS div $10000) + '.' + IntToStr(FixedFileInfo.dwFileVersionMS and $0FFFF) +
					'.' + IntToStr(FixedFileInfo.dwFileVersionLS div $10000) + '.' + IntToStr(FixedFileInfo.dwFileVersionLS and $0FFFF);
			end;
		finally
			FreeMem(verBuf);
		end;
	end;
end;

function TReleaseUtils.GetLatestRelease : IReleaseInfo;
begin
	Result := DownloadedReleaseInfos[0];
end;

class function TReleaseUtils.GetRunningModulePath() : string;
var
	szFileName : array [0 .. MAX_PATH] of Char;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TReleaseUtils.GetRunningModulePath');

	FillChar(szFileName, SizeOf(szFileName), #0);
	GetModuleFileName(hInstance, szFileName, MAX_PATH);
	Result := szFileName;
	dbgMsg.Msg(Result);
end;

class procedure TReleaseUtils.InitRestClient(_restClient : TRESTClient; _restRequest : TRESTRequest; _restResponse : TRESTResponse);
begin
	_restClient.Accept := 'application/json, text/plain; q=0.9, text/html;q=0.8,';
	_restClient.AcceptCharset := 'utf-8, *;q=0.8';
	_restClient.BaseURL := 'https://api.github.com/repos/mattia72/DripGrepper/releases';
	_restRequest.Client := _restClient;
	_restRequest.Response := _restResponse;
	_restResponse.ContentType := 'application/json';
end;

function TReleaseUtils.IsCurrentTheLatest : Boolean;
begin
	Result := TReleaseUtils.IsSameVersion(LatestRelease, CurrentRelease);
end;

class function TReleaseUtils.IsSameVersion(_ri1, _ri2 : IReleaseInfo) : Boolean;
var
	mainVersion : string;
begin
	Result := False;
	if Assigned(_ri1) and Assigned(_ri2) { and _ri1.LoadOk } then begin
		mainVersion := _ri1.Version; // v4.1.2-beta
		mainVersion := mainVersion.Substring(0, mainVersion.IndexOf('-')); // v4.1.2
		Result := _ri2.Version.StartsWith(mainVersion);
	end;
end;

procedure TReleaseUtils.ShowNewVersionMsgBox(const _bOnlyIfUpdateAvailable : Boolean = False);
begin
	if IsCurrentTheLatest then begin
		if not _bOnlyIfUpdateAvailable then begin
			if LatestRelease.LoadOk then begin
				TMsgBox.ShowInfo('You are using the latest version.');
			end; // else error msg already shown...
		end;
	end else begin
		var
		msg := Format(
			{ } 'You are using %s' + CRLF +
			{ } 'New version %s published at %s' + CRLF2 +
			{ } 'Do you wan''t to go to the release page?', [GetModuleNameAndVersion(),
			{ } LatestVersion, DateTimeToStr(LatestRelease.PublishedAt)]);
		var
		mbp := TMsgBoxParams.Create(msg, TMsgDlgType.mtConfirmation);
		mbp.ExpandedCaption := 'What''s New';
		mbp.ExpandedText := LatestRelease.Description;
		var
		res := TMsgBox.CreateMsgDialog(mbp);
		if mrYes = res then begin
			TUrlLinkHelper.OpenLink(LatestRelease.HtmlURL);
		end;
	end;
end;

function TReleaseUtils.TryGetCurrentRelInfo(var _curInfo : IReleaseInfo) : Boolean;
begin
	Result := False;
	for var relInfo in FDownloadedReleaseInfos do begin
		if IsSameVersion(relInfo, _curInfo) then begin
			_curInfo := relInfo;
			Result := True;
			break;
		end;
	end;
end;

end.
