unit RipGrepper.UI.HintBuilder;

interface

uses
	System.SysUtils,
	RipGrepper.Common.NodeData;

type
	TFileHintBuilder = class
		private
			class function GetFileAttributes(const _filePath : string) : string;
			class function GetFileSize(const _filePath : string) : string;
			class function GetSourceControlInfo(const _filePath : string) : string;
			class function FindScRoot(const _filePath : string; out _scType : string) : string;
			class function GetGitFileStatus(const _repoRoot, _filePath : string) : string;
			class function GetSvnFileStatus(const _filePath : string) : string;
			class function RunCommand(const _exe, _args, _workDir : string) : string;
			class function ParseGitStatusCode(const _code : string) : string;
			class function ParseSvnStatusCode(_code : Char) : string;

		public
			// Build the hint text for file-level nodes (full path, size, attributes, SC info)
			class function BuildFileNodeHint(const _filePath : string; const _showRelativePath : Boolean) : string;
			// Build the hint text for match-level nodes (file:row:col summary + full line)
			class function BuildMatchNodeHint(const _nodeData : PVSFileNodeData) : string;
	end;

implementation

uses
	System.IOUtils,
	System.Classes,
	Winapi.Windows,
	u_dzConvertUtils;

class function TFileHintBuilder.RunCommand(const _exe, _args, _workDir : string) : string;
var
	si : TStartupInfo;
	pi : TProcessInformation;
	sa : TSecurityAttributes;
	hReadPipe, hWritePipe : THandle;
	cmdLine : string;
	bytesRead : DWORD;
	buffer : array [0 .. 4095] of AnsiChar;
	exitCode : DWORD;
begin
	Result := '';

	sa.nLength := SizeOf(TSecurityAttributes);
	sa.bInheritHandle := True;
	sa.lpSecurityDescriptor := nil;

	if not CreatePipe(hReadPipe, hWritePipe, @sa, 0) then begin
		Exit;
	end;

	try
		SetHandleInformation(hReadPipe, HANDLE_FLAG_INHERIT, 0);

		ZeroMemory(@si, SizeOf(si));
		si.cb := SizeOf(si);
		si.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
		si.wShowWindow := SW_HIDE;
		si.hStdOutput := hWritePipe;
		si.hStdError := hWritePipe;

		cmdLine := _exe + ' ' + _args;
		UniqueString(cmdLine);

		if not CreateProcess(nil, PChar(cmdLine), nil, nil, True,
			CREATE_NO_WINDOW, nil, PChar(_workDir), si, pi) then begin
			Exit;
		end;

		CloseHandle(hWritePipe);
		hWritePipe := 0;

		WaitForSingleObject(pi.hProcess, 3000);

		while ReadFile(hReadPipe, buffer, SizeOf(buffer) - 1, bytesRead, nil) and (bytesRead > 0) do begin
			buffer[bytesRead] := #0;
			Result := Result + string(AnsiString(buffer));
		end;

		GetExitCodeProcess(pi.hProcess, exitCode);
		CloseHandle(pi.hProcess);
		CloseHandle(pi.hThread);

		Result := Result.Trim;
	finally
		if hWritePipe <> 0 then begin
			CloseHandle(hWritePipe);
		end;
		CloseHandle(hReadPipe);
	end;
end;

class function TFileHintBuilder.FindScRoot(const _filePath : string; out _scType : string) : string;
var
	dir : string;
	parentDir : string;
begin
	Result := '';
	_scType := '';
	dir := TPath.GetDirectoryName(_filePath);

	while not dir.IsEmpty do begin
		if TDirectory.Exists(TPath.Combine(dir, '.git')) then begin
			_scType := 'Git';
			Result := dir;
			Exit;
		end;
		if TDirectory.Exists(TPath.Combine(dir, '.svn')) then begin
			_scType := 'SVN';
			Result := dir;
			Exit;
		end;
		parentDir := TPath.GetDirectoryName(dir);
		if parentDir = dir then begin
			Break;
		end;
		dir := parentDir;
	end;
end;

class function TFileHintBuilder.ParseGitStatusCode(const _code : string) : string;
begin
	Result := '';
	if _code.IsEmpty then begin
		Exit;
	end;
	if _code = '??' then begin
		Result := 'Untracked';
	end else if _code = '!!' then begin
		Result := 'Ignored';
	end else if _code = 'MM' then begin
		Result := 'Modified (staged + unstaged)';
	end else if _code[1] = 'M' then begin
		Result := 'Modified (staged)';
	end else if (Length(_code) > 1) and (_code[2] = 'M') then begin
		Result := 'Modified';
	end else if _code[1] = 'A' then begin
		Result := 'Added';
	end else if _code[1] = 'D' then begin
		Result := 'Deleted (staged)';
	end else if (Length(_code) > 1) and (_code[2] = 'D') then begin
		Result := 'Deleted';
	end else if _code[1] = 'R' then begin
		Result := 'Renamed';
	end else if _code[1] = 'C' then begin
		Result := 'Copied';
	end else if _code[1] = 'U' then begin
		Result := 'Unmerged';
	end else begin
		Result := 'Status: ' + _code;
	end;
end;

class function TFileHintBuilder.ParseSvnStatusCode(_code : Char) : string;
begin
	case _code of
		' ' : Result := '';
		'M' : Result := 'Modified';
		'A' : Result := 'Added';
		'D' : Result := 'Deleted';
		'R' : Result := 'Replaced';
		'C' : Result := 'Conflicted';
		'X' : Result := 'External';
		'I' : Result := 'Ignored';
		'?' : Result := 'Untracked';
		'!' : Result := 'Missing';
		'~' : Result := 'Type changed';
	else
		Result := 'Status: ' + _code;
	end;
end;

class function TFileHintBuilder.GetGitFileStatus(const _repoRoot, _filePath : string) : string;
var
	output : string;
begin
	Result := '';
	output := RunCommand('git', 'status --porcelain -- "' + _filePath + '"', _repoRoot);
	if output.IsEmpty then begin
		Result := 'Committed';
		Exit;
	end;
	if Length(output) >= 2 then begin
		Result := ParseGitStatusCode(Copy(output, 1, 2));
	end;
end;

class function TFileHintBuilder.GetSvnFileStatus(const _filePath : string) : string;
var
	output : string;
	workDir : string;
begin
	Result := '';
	workDir := TPath.GetDirectoryName(_filePath);
	output := RunCommand('svn', 'status "' + _filePath + '"', workDir);
	if output.IsEmpty then begin
		Result := 'Committed';
		Exit;
	end;
	if Length(output) >= 1 then begin
		Result := ParseSvnStatusCode(output[1]);
	end;
end;

class function TFileHintBuilder.GetSourceControlInfo(const _filePath : string) : string;
var
	scType : string;
	scRoot : string;
	status : string;
begin
	Result := '';
	scRoot := FindScRoot(_filePath, scType);

	if scType.IsEmpty then begin
		Exit;
	end;

	if scType = 'Git' then begin
		status := GetGitFileStatus(scRoot, _filePath);
	end else begin
		status := GetSvnFileStatus(_filePath);
	end;

	Result := scType;
	if not status.IsEmpty then begin
		Result := Result + ' (' + status + ')';
	end;
end;

class function TFileHintBuilder.GetFileSize(const _filePath : string) : string;
var
	fileInfo : TWin32FileAttributeData;
	size : Int64;
begin
	Result := '';
	if GetFileAttributesEx(PChar(_filePath), GetFileExInfoStandard, @fileInfo) then begin
		size := Int64(fileInfo.nFileSizeHigh) shl 32 or fileInfo.nFileSizeLow;
		Result := FileSizeToHumanReadableString(size);
	end;
end;

class function TFileHintBuilder.GetFileAttributes(const _filePath : string) : string;
var
	attrs : DWORD;
	parts : TArray<string>;
begin
	Result := '';
	attrs := Winapi.Windows.GetFileAttributes(PChar(_filePath));
	if attrs = INVALID_FILE_ATTRIBUTES then begin
		Exit;
	end;

	parts := [];
	if (attrs and FILE_ATTRIBUTE_READONLY) <> 0 then begin
		parts := parts + ['ReadOnly'];
	end;
	if (attrs and FILE_ATTRIBUTE_HIDDEN) <> 0 then begin
		parts := parts + ['Hidden'];
	end;
	if (attrs and FILE_ATTRIBUTE_SYSTEM) <> 0 then begin
		parts := parts + ['System'];
	end;
	if (attrs and FILE_ATTRIBUTE_ARCHIVE) <> 0 then begin
		parts := parts + ['Archive'];
	end;

	if Length(parts) > 0 then begin
		Result := string.Join(', ', parts);
	end;
end;

class function TFileHintBuilder.BuildFileNodeHint(const _filePath : string; const _showRelativePath : Boolean) : string;
var
	lines : TArray<string>;
	sSize : string;
	sAttrs : string;
	sSC : string;
	dtModified : TDateTime;
	dtCreated : TDateTime;
begin
	Result := '';
	if not FileExists(_filePath) then begin
		Exit;
	end;

	lines := [];

	// Show full path when relative paths are displayed in the tree
	if _showRelativePath then begin
		lines := lines + [_filePath];
	end;

	sSize := GetFileSize(_filePath);
	if not sSize.IsEmpty then begin
		lines := lines + ['Size: ' + sSize];
	end;

	// File times
	dtModified := TFile.GetLastWriteTime(_filePath);
	if dtModified > 0 then begin
		lines := lines + ['Modified: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', dtModified)];
	end;
	dtCreated := TFile.GetCreationTime(_filePath);
	if dtCreated > 0 then begin
		lines := lines + ['Created: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', dtCreated)];
	end;

	sAttrs := GetFileAttributes(_filePath);
	if not sAttrs.IsEmpty then begin
		lines := lines + ['Attr: ' + sAttrs];
	end;

	sSC := GetSourceControlInfo(_filePath);
	if not sSC.IsEmpty then begin
		lines := lines + [sSC];
	end;

	Result := string.Join(#13#10, lines);
end;

class function TFileHintBuilder.BuildMatchNodeHint(const _nodeData : PVSFileNodeData) : string;
begin
	Result := '';
	if _nodeData = nil then begin
		Exit;
	end;

	// Show full location: file:row:col and the full line text
	if (not _nodeData.FilePath.IsEmpty) and (_nodeData.MatchData.Row > 0) then begin
		Result := Format('%s:%d:%d', [_nodeData.FilePath, _nodeData.MatchData.Row, _nodeData.MatchData.ColBegin]);
		if not _nodeData.MatchData.LineText.IsEmpty then begin
			Result := Result + #13#10 + _nodeData.MatchData.LineText;
		end;
	end else if not _nodeData.MatchData.LineText.IsEmpty then begin
		Result := _nodeData.MatchData.LineText;
	end;
end;

end.
