unit RipGrepper.Tools.WinHttpClient;

interface

uses
	System.SysUtils,
	System.Classes,
	Winapi.Windows,
	Winapi.WinInet;

type
	TWinHttpClient = class
		private
			FUserAgent: string;
			FAccept: string;
			FAcceptCharset: string;
			procedure CheckInternetError(const _operation: string);

		public
			constructor Create;
			destructor Destroy; override;
			function Get(const _url: string): string;
			property UserAgent: string read FUserAgent write FUserAgent;
			property Accept: string read FAccept write FAccept;
			property AcceptCharset: string read FAcceptCharset write FAcceptCharset;
	end;

implementation

constructor TWinHttpClient.Create;
begin
	inherited;
	FUserAgent := 'DripGrepper/1.0';
	FAccept := 'application/json, text/plain; q=0.9, text/html;q=0.8,';
	FAcceptCharset := 'utf-8, *;q=0.8';
end;

destructor TWinHttpClient.Destroy;
begin
	inherited;
end;

procedure TWinHttpClient.CheckInternetError(const _operation: string);
var
	errorCode: DWORD;
begin
	errorCode := GetLastError();
	if errorCode <> ERROR_SUCCESS then begin
		raise Exception.CreateFmt('%s failed with error %d', [_operation, errorCode]);
	end;
end;

function TWinHttpClient.Get(const _url: string): string;
var
	hInternet: Pointer;
	hRequest: Pointer;
	dwFlags: DWORD;
	dwBytesRead: DWORD;
	buffer: array[0..8191] of Byte;
	responseBytes: TBytes;
	totalBytes: Integer;
begin
	Result := '';
	
	// Initialize WinInet
	hInternet := InternetOpen(PChar(FUserAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
	if not Assigned(hInternet) then begin
		CheckInternetError('InternetOpen');
	end;
	
	try
		// Set flags for HTTPS if needed
		dwFlags := INTERNET_FLAG_RELOAD or INTERNET_FLAG_NO_CACHE_WRITE;
		if Pos('https://', LowerCase(_url)) = 1 then begin
			dwFlags := dwFlags or INTERNET_FLAG_SECURE;
		end;
		
		// Open URL
		hRequest := InternetOpenUrl(hInternet, PChar(_url), nil, 0, dwFlags, 0);
		if not Assigned(hRequest) then begin
			CheckInternetError('InternetOpenUrl');
		end;
		
		try
			// Read the response into byte array
			totalBytes := 0;
			SetLength(responseBytes, 0);
			
			repeat
				dwBytesRead := 0;
				if InternetReadFile(hRequest, @buffer[0], SizeOf(buffer), dwBytesRead) then begin
					if dwBytesRead > 0 then begin
						SetLength(responseBytes, totalBytes + Integer(dwBytesRead));
						Move(buffer[0], responseBytes[totalBytes], dwBytesRead);
						Inc(totalBytes, Integer(dwBytesRead));
					end;
				end else begin
					CheckInternetError('InternetReadFile');
				end;
			until dwBytesRead = 0;
			
			// Convert UTF-8 bytes to Unicode string
			if totalBytes > 0 then begin
				Result := TEncoding.UTF8.GetString(responseBytes);
			end;
			
		finally
			InternetCloseHandle(hRequest);
		end;
		
	finally
		InternetCloseHandle(hInternet);
	end;
end;

end.
