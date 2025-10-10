unit RipGrepper.Tools.FileUtils;

interface

uses
	System.SysUtils,
	System.Classes,
	System.Generics.Collections,
	ArrayEx,
	System.JSON,
	Winapi.Windows;

type
	TCommandLineRec = record
		ExePath : string;
		Arguments : TArray<string>;

		public
			function AsString() : string;
			function ParametersAsString() : string;
			class function ParseCommand(const _sCmd : string) : TCommandLineRec; static;
	end;

	TCommandItem = record
		Caption : string;
		CommandLine : TCommandLineRec;
		Description : string;
		IsActive : Boolean;

		public
			class function New(const _caption, _cmd : string; const _descr : string = ''; const _isActive : Boolean = True) : TCommandItem;
				overload; static;
			class function New(const _arr : TArray<string>) : TCommandItem; overload; static;
			class function FromJSON(const _json : TJSONObject) : TCommandItem; static;
			function ToJSON() : TJSONObject;
			class function ArrayToJSONArrayString(const _items : TArray<TCommandItem>) : string; static;
			class function ArrayFromJSON(const _jsonString : string) : TArray<TCommandItem>; static;
			class function ArrayToStringArrayOfJsonStrings(const _items : TArray<TCommandItem>) : TArray<string>; static;
	end;

	TFileUtils = class(TObject)

		private

		public
			class procedure CreateBackup(const fileName : string);
			class procedure DeleteTempDirectory(const _dirPattern : string; const _bForce : Boolean = False);
			class procedure EmptyFile(const _filePath : string);
			/// <summary>
			/// Expands a relative FileName relative to the given BaseDir. </summary>
			class function ExpandFileNameRelBaseDir(const _Filename, _BaseDir : string) : string;
			class function FindExecutable(sFileName : string; out sOutpuPath : string) : Boolean;
			class function FindFileInSubDirs(const _dir : string; const _file : string) : string;
			class function GetVsCodeDir : string;
			class function GetVsCodeCommandItem : TCommandItem;
			// Determine if a passed in path/file is absolute or relative
			class function IsPathAbsolute(const FileName : string) : Boolean;
			class function LongToShortFilePath(const LongPath : string) : string;
			class function ShortToLongPath(const ShortPath : string) : string;
	end;

procedure GetPackageNameInfoProc(const name : string; NameType : TNameType; Flags : Byte; Param : Pointer);

implementation

uses

	Winapi.ShellAPI,
	RipGrepper.Tools.DebugUtils,
	System.IOUtils,
	RipGrepper.Common.Constants,
	System.RegularExpressions,
	System.StrUtils,
	RipGrepper.Helper.UI,
	System.UITypes,
	System.Generics.Defaults,
	System.Math,
	Vcl.Forms;

procedure GetPackageNameInfoProc(const Name : string; NameType : TNameType; Flags : Byte; Param : Pointer);
begin
	if NameType = ntDcpBpiName then begin
		PString(Param)^ := name;
	end;
end;

class procedure TFileUtils.CreateBackup(const fileName : string);
var
	backupFileName : string;
begin
	backupFileName := fileName + FormatDateTime('.yyyymmddhhnn', Now) + BAK_FILE_EXTENSION;
	CopyFile(PWideChar(fileName), PWideChar(backupFileName), true);
end;

class procedure TFileUtils.DeleteTempDirectory(const _dirPattern : string; const _bForce : Boolean = False);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TFileUtils.DeleteTempDirectory');

	{$IF CompilerVersion >= COMPILER_VERSION_DELPHI_12} // Delphi 12
	for var d in TDirectory.GetDirectoriesEnumerator(TPath.GetTempPath) do begin
	{$ELSE}
	for var d in TDirectory.GetDirectories(TPath.GetTempPath) do begin
		{$ENDIF}
		var
		dirName := TPath.GetFileName(d);
		if TPath.MatchesPattern(dirName, _dirPattern, false) then begin
			dbgMsg.Msg('Delete ' + d);
			TDirectory.Delete(d, _bForce);
		end;

	end;

end;

class procedure TFileUtils.EmptyFile(const _filePath : string);
var
	txtFile : TextFile;
begin
	AssignFile(txtFile, _filePath);
	Rewrite(txtFile);
	CloseFile(txtFile);
end;

class function TFileUtils.ExpandFileNameRelBaseDir(const _Filename, _BaseDir : string) : string;
begin
	if TPath.IsRelativePath(_Filename) then begin
		Result := IncludeTrailingPathDelimiter(_BaseDir) + _Filename;
	end else begin
		Result := _Filename;
	end;
	Result := TPath.GetFullPath(Result);
end;

class function TFileUtils.FindExecutable(sFileName : string; out sOutpuPath : string) : Boolean;
var
	Buffer : array [0 .. MAX_PATH] of Char;
	res : integer;
begin
	Result := True;
	res := Winapi.ShellAPI.FindExecutable(PChar(sFileName), PChar(nil), &Buffer);
	case res of
		0, ERROR_FILE_NOT_FOUND, ERROR_PATH_NOT_FOUND :
		Result := False;
		else begin
			SetString(sOutpuPath, PChar(@Buffer[0]), Length(Buffer));
			SetLength(sOutpuPath, Pos(#0, sOutpuPath) - 1);
		end;
	end;

	TDebugUtils.DebugMessage('TFileUtils.FindExecutable: ' + sFileName + ' path:' + sOutpuPath);
	// WriteDebugMessage(sOutpuPath);
end;

class function TFileUtils.FindFileInSubDirs(const _dir : string; const _file : string) : string;
begin
	Result := '';
	var
	filePath := TPath.Combine(_dir, _file);
	if FileExists(filePath) then begin
		Result := filePath;
	end else begin
		var
		dirs := TDirectory.GetDirectories(_dir);
		for var dir in dirs do begin
			Result := FindFileInSubDirs(dir, _file);
			if not Result.IsEmpty then begin
				break;
			end;
		end;
	end;
end;

class function TFileUtils.LongToShortFilePath(const LongPath : string) : string;
var
	ShortPath : array [0 .. MAX_PATH - 1] of Char;
begin
	if 0 < GetShortPathName(PChar(LongPath), ShortPath, MAX_PATH) then
		Result := ShortPath
	else
		Result := LongPath; // Return the original path if conversion fails
end;

class function TFileUtils.ShortToLongPath(const ShortPath : string) : string;
var
	LongPath : array [0 .. MAX_PATH - 1] of Char;
begin
	if 0 < GetLongPathName(PChar(ShortPath), LongPath, Length(LongPath)) then begin
		Result := LongPath;
	end else begin
		Result := ShortPath;
	end;
end;

class function TFileUtils.GetVsCodeDir : string;
begin
	Result := '';
	var
		sCodePath : string;
	if TFileUtils.FindExecutable('code', sCodePath) then begin
		Result := TPath.GetDirectoryName(sCodePath);
	end;
end;

class function TFileUtils.GetVsCodeCommandItem : TCommandItem;
var
	sCodePath : string;
	sCommand : string;
	sPotentialPaths : TArray<string>;
	sPath : string;
begin
	Result := default (TCommandItem);

	if TFileUtils.FindExecutable('code.exe', sCodePath) then begin
		sCommand := Format('"%s" --reuse-window "<DIR>" --goto "<FILE>:<LINE>:<COL>"', [sCodePath]);
		Result := TCommandItem.New('VSCode', sCommand, 'Open in existing VSCode instance', True);
	end else begin
		sPotentialPaths := ['C:\Program Files\Microsoft VS Code\Code.exe', 'C:\Program Files (x86)\Microsoft VS Code\Code.exe',
			'C:\Users\' + GetEnvironmentVariable('USERNAME') + '\AppData\Local\Programs\Microsoft VS Code\Code.exe'];

		for sPath in sPotentialPaths do begin
			if FileExists(sPath) then begin
				sCommand := Format('"%s" --reuse-window "<DIR>" --goto "<FILE>:<LINE>:<COL>"', [sPath]);
				Result := TCommandItem.New('VSCode', sCommand, 'Open in existing VSCode instance', True);
				Exit;
			end;
		end;

		Result := TCommandItem.New('VSCode', 'code.exe --reuse-window "<DIR>" --goto "<FILE>:<LINE>:<COL>"',
			'Open in existing VSCode instance', False);
	end;
end;

class function TFileUtils.IsPathAbsolute(const FileName : string) : Boolean;
begin
	Result := ExtractFileDrive(FileName) <> '';
end;

// TODO: PathIsRelativeAPI
// function TFileUtils.PathIsRelativeAPI(pszPath : PChar): BOOL;

function TCommandLineRec.AsString() : string;
begin
	Result := ExePath + ' ' + string.Join(' ', Arguments);;
end;

function TCommandLineRec.ParametersAsString() : string;
begin
	Result := string.Join(' ', Arguments);;
end;

class function TCommandLineRec.ParseCommand(const _sCmd : string) : TCommandLineRec;
var
	m : TMatch;
	r : TRegEx;
begin
	r := TRegex.Create('[''"]?(.*.(exe|com|bat))["'']? (.*)', [roIgnoreCase]);
	m := r.Match(_sCmd);

	if not m.Success then begin
		Exit;
	end;

	Result.ExePath := m.Groups[1].Value;
	Result.Arguments := m.Groups[3].Value.Split([' ']);
end;

class function TCommandItem.New(const _caption, _cmd : string; const _descr : string = ''; const _isActive : Boolean = True) : TCommandItem;
begin
	Result.Caption := _caption;
	Result.CommandLine := TCommandLineRec.ParseCommand(_cmd);
	Result.Description := _descr;
	Result.IsActive := _isActive;
end;

class function TCommandItem.New(const _arr : TArray<string>) : TCommandItem;
var
	arrEx : TArrayEx<string>;
begin
	arrEx := _arr;
	if arrEx.MaxIndex = 1 then begin
		Result.IsActive := (arrEx[0].ToUpper() = 'TRUE');
		Result.CommandLine := TCommandLineRec.ParseCommand(arrEx.SafeItem[1]);
		Result.Caption := TPath.GetFileNameWithoutExtension(Result.CommandLine.ExePath);
		Result.Description := '';
	end else begin
		Result.IsActive := (arrEx.SafeItem[0].ToUpper() = 'TRUE');
		Result.Caption := arrEx.SafeItem[1];
		Result.CommandLine := TCommandLineRec.ParseCommand(arrEx.SafeItem[2]);
		Result.Description := arrEx.SafeItem[3];
	end;
end;

class function TCommandItem.FromJSON(const _json : TJSONObject) : TCommandItem;
begin
	Result.Caption := _json.GetValue<string>('caption', '');
	Result.Description := _json.GetValue<string>('description', '');
	Result.IsActive := _json.GetValue<Boolean>('isActive', True);
	Result.CommandLine := TCommandLineRec.ParseCommand(_json.GetValue<string>('commandLine', ''));
end;

function TCommandItem.ToJSON() : TJSONObject;
begin
	Result := TJSONObject.Create;
	Result.AddPair('caption', Caption);
	Result.AddPair('description', Description);
	Result.AddPair('isActive', TJSONBool.Create(IsActive));
	Result.AddPair('commandLine', CommandLine.AsString());
end;

class function TCommandItem.ArrayToJSONArrayString(const _items : TArray<TCommandItem>) : string;
var
	jsonArray : TJSONArray;
	item : TCommandItem;
begin
	jsonArray := TJSONArray.Create;
	try
		for item in _items do begin
			jsonArray.AddElement(item.ToJSON());
		end;
		Result := jsonArray.Format(2);
	finally
		jsonArray.Free;
	end;
end;

class function TCommandItem.ArrayFromJSON(const _jsonString : string) : TArray<TCommandItem>;
var
	jsonArray : TJSONArray;
	jsonValue : TJSONValue;
	i : Integer;
begin
	Result := [];

	if _jsonString.Trim.IsEmpty then begin
		Exit;
	end;

	jsonValue := TJSONObject.ParseJSONValue(_jsonString);
	if not Assigned(jsonValue) then begin
		Exit;
	end;

	try
		if jsonValue is TJSONArray then begin
			jsonArray := TJSONArray(jsonValue);
			SetLength(Result, jsonArray.Count);

			for i := 0 to jsonArray.Count - 1 do begin
				if jsonArray.Items[i] is TJSONObject then begin
					Result[i] := TCommandItem.FromJSON(TJSONObject(jsonArray.Items[i]));
				end;
			end;
		end;
	finally
		jsonValue.Free;
	end;
end;

class function TCommandItem.ArrayToStringArrayOfJsonStrings(const _items : TArray<TCommandItem>) : TArray<string>;
var
	item : TCommandItem;
begin
	for item in _items do begin
		Result := Result + [item.ToJSON().ToString];
	end;
end;

end.
