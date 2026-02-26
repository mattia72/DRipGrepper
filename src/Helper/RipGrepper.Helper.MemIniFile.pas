unit RipGrepper.Helper.MemIniFile;

interface

uses
	System.IniFiles,
	System.Classes;

type
	TMemIniFileHelper = class Helper for TMemIniFile
		private
			function GetTempFileName : string;

		public
			function GetDripGrepperIniTempDir : string;
			function GetTempSectionFileName(_section : string) : string;
			function KeyExists(const _section, _key : string) : Boolean;
			procedure ReadTempSectionFiles;
			procedure ReloadIniFile;
			procedure WriteTempSectionIni(_sectionName : string; const _sectionValues : TStringList); overload;
	end;

implementation

uses
	System.IOUtils,
	System.SysUtils,
	RipGrepper.Tools.DebugUtils,
	Spring;

function TMemIniFileHelper.GetDripGrepperIniTempDir : string;
begin
	var
	dirName := TPath.GetFileNameWithoutExtension(self.FileName) + ' Temp Files';
	Result := TPath.Combine(TPath.GetTempPath, dirName);
end;

function TMemIniFileHelper.GetTempSectionFileName(_section : string) : string;
begin
	Result := TPath.Combine(GetDripGrepperIniTempDir,
		{ } TPath.GetFileNameWithoutExtension(self.FileName) + '_' + _section +
		{ } TPath.GetExtension(self.FileName));
end;

function TMemIniFileHelper.GetTempFileName : string;
begin
	Result := TPath.Combine(GetDripGrepperIniTempDir,
		{ } TPath.GetFileNameWithoutExtension(self.FileName) +
		{ } TPath.GetExtension(self.FileName));
end;

function TMemIniFileHelper.KeyExists(const _section, _key : string) : Boolean;
begin
	Result := self.ValueExists(_section, _key);
end;

procedure TMemIniFileHelper.ReadTempSectionFiles;
var
	tmpFile : string;
	sectionList : TStringList;
	valueList : TStringList;
	tmpIniFile : TMemIniFile;
begin
	sectionList := TStringList.Create();
	valueList := TStringList.Create();
	try
		self.ReadSections(sectionList);

		for var section in sectionList do begin

			tmpFile := GetTempSectionFileName(section);
			if not FileExists(tmpFile) then
				continue;

			tmpIniFile := TMemIniFile.Create(tmpFile);
			try
				tmpIniFile.ReadSectionValues(section, valueList);
				for var i := 0 to valueList.Count - 1 do begin
					var
					key := valueList.Names[i];
					var
					value := valueList.ValueFromIndex[i];
					self.WriteString(section, key, value);
				end;
			finally
				tmpIniFile.Free;
				TFile.Delete(tmpFile);
				var
				tmpDir := GetDripGrepperIniTempDir;
				if 0 = Length(TDirectory.GetFiles(tmpDir)) then begin
					TDirectory.Delete(tmpDir);
				end;
			end;
		end;
	finally
		sectionList.Free;
		valueList.Free;
	end;

end;

procedure TMemIniFileHelper.ReloadIniFile;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniFileHelper.ReloadIniFile');

	var
	origIniFile := self.FileName;
	// var
	// tmpFile := GetTempFileName;
	// dbgMsg.MsgFmt('Rename to temp file : %s', [tmpFile]);
	// self.Rename(tmpFile, false);
	self.Clear;
	dbgMsg.MsgFmt('Rename to orig file : %s', [origIniFile]);
	self.Rename(origIniFile, true);
end;

procedure TMemIniFileHelper.WriteTempSectionIni(_sectionName : string; const _sectionValues : TStringList);
var
	isModified : Boolean;
	tmpIniFile : TMemIniFile;
	newFileName : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniFileHelper.WriteTempSectionIni');

	newFileName := GetTempSectionFileName(_sectionName);

	var
	tmpDir := GetDripGrepperIniTempDir();
	if not TDirectory.Exists(tmpDir) then begin
		TDirectory.CreateDirectory(tmpDir);
	end;

	if TFile.Exists(newFileName) then begin
		dbgMsg.Msg('Delete file: ' + newFileName);
		TFile.Delete(newFileName);
	end;

	tmpIniFile := TMemIniFile.Create(newFileName);
	try
		isModified := false;
		for var i := 0 to _sectionValues.Count - 1 do begin
			var
			key := _sectionValues.Names[i];
			var
			value := _sectionValues.ValueFromIndex[i];
			tmpIniFile.WriteString(_sectionName, key, value);
			isModified := true;
		end;
		if isModified then begin
			dbgMsg.Msg('Updating file: ' + newFileName);
			tmpIniFile.UpdateFile();
		end;
	finally
		tmpIniFile.Free;
	end;
end;

end.
