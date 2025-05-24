unit RipGrepper.Tools.ReleaseUtils;

interface

uses
	System.JSON,
	REST.Types,
	REST.Client;

type
	TReleaseInfo = record
		private
			FCurrentNameWithVersion : string;
			class procedure InitRestClient(_restClient : TRESTClient; _restRequest : TRESTRequest; _restResponse : TRESTResponse); static;

		public
			Description : string;
			Version : string;
			HtmlURL : string;
			PublishedAt : TDateTime;
			LoadOk : Boolean;
			class function DownloadReleaseInfoFromGithub() : TArray<TReleaseInfo>; static;
			procedure FromJson(_json : TJSONValue; const _idx : Integer);
			function GetCurrentNameWithVersion() : string;
			property CurrentNameWithVersion : string read GetCurrentNameWithVersion;
	end;

	TReleaseUtils = record
		private
			class function GetModuleVersion(Instance : THandle; out iMajor, iMinor, iRelease, iBuild : Integer) : Boolean; static;

		public
			class function GetAppDirectory() : string; static;
			class function GetAppNameAndVersion(const _exePath : string) : string; static;
			class function GetFileVersion(const _fullPath: string): string; static;
			class function GetRunningModuleVersion(const _exePath : string): string; static;
			class function GetModuleNameAndVersion() : string; static;
			class function GetRunningModulePath() : string; static;
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
	RipGrepper.Tools.DebugUtils;

class function TReleaseInfo.DownloadReleaseInfoFromGithub() : TArray<TReleaseInfo>;
var
	jValue : TJSONValue;
	restClient : TRESTClient;
	restRequest : TRESTRequest;
	restResponse : TRESTResponse;
begin
	restClient := TRESTClient.Create(nil);
	restRequest := TRESTRequest.Create(nil);
	restResponse := TRESTResponse.Create(nil);
	try
		InitRestClient(restClient, restRequest, restResponse);

		restRequest.Execute;
		jValue := restResponse.JSONValue;

		for var idx := 0 to (jValue as TJSONArray).Count - 1 do begin
			var ri : TReleaseInfo;
			ri.FromJson(jValue, idx);
			Result := Result + [ri];
		end;

	finally
		restClient.Free;
		restRequest.Free;
		restResponse.Free;
	end;
end;

procedure TReleaseInfo.FromJson(_json : TJSONValue; const _idx : Integer);
begin
	LoadOk := False;
	var
	item := _json.A[_idx];
	HtmlURL := item.GetValue<string>('html_url');
	Version := item.GetValue<string>('name');
	PublishedAt := item.GetValue<TDateTime>('published_at');
	Description := item.GetValue<string>('body');
	LoadOk := True;
end;

function TReleaseInfo.GetCurrentNameWithVersion() : string;
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

class procedure TReleaseInfo.InitRestClient(_restClient : TRESTClient; _restRequest : TRESTRequest; _restResponse : TRESTResponse);
begin
	_restClient.Accept := 'application/json, text/plain; q=0.9, text/html;q=0.8,';
	_restClient.AcceptCharset := 'utf-8, *;q=0.8';
	_restClient.BaseURL := 'https://api.github.com/repos/mattia72/DripGrepper/releases';
	// SynchronizedEvents := False;
	_restRequest.Client := _restClient;
	_restRequest.Response := _restResponse;
	// SynchronizedEvents := False;
	_restResponse.ContentType := 'application/json';
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
	imajor : integer;
	iminor : integer;
	irelease : integer;
	ibuild : integer;
	name : string;
begin
	name := TPath.GetFileNameWithoutExtension(_exePath);
	GetModuleVersion(0, imajor, iminor, irelease, ibuild);
	Result := Format(FORMAT_NAME_VERSION_INFO, [name, APP_PLATFORM, imajor, iminor, irelease, ibuild]);
end;

class function TReleaseUtils.GetRunningModuleVersion(const _exePath : string):
	string;
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

class function TReleaseUtils.GetFileVersion(const _fullPath: string): string;
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

end.
