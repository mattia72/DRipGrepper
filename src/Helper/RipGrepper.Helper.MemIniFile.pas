unit RipGrepper.Helper.MemIniFile;

interface

uses
	System.IniFiles;

type
	TMemIniFileHelper = class Helper for TMemIniFile
		private
			function GetTempFileName : string;
			function CreateTempDirectory: string;
			procedure WriteTempSectionIni(const _fileName, _section : string); overload;

		public
			function GetTempSectionFileName(_section : string) : string;
			procedure ReadTempSectionFiles;
			procedure WriteTempSectionIni(const _section : string); overload;
			procedure ReloadIniFile;
	end;

implementation

uses
	System.IOUtils,
	System.SysUtils,
	System.Classes,
	RipGrepper.Tools.DebugUtils;

function TMemIniFileHelper.GetTempSectionFileName(_section : string) : string;
begin
	Result := TPath.Combine(CreateTempDirectory,
		{ } TPath.GetFileNameWithoutExtension(self.FileName) + '_' + _section +
		{ } TPath.GetExtension(self.FileName));
end;

function TMemIniFileHelper.GetTempFileName : string;
begin
	Result := TPath.Combine(CreateTempDirectory,
		{ } TPath.GetFileNameWithoutExtension(self.FileName) +
		{ } TPath.GetExtension(self.FileName));
end;

function TMemIniFileHelper.CreateTempDirectory: string;
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TMemIniFileHelper.CreateTempDirectory');
	Result := TPath.Combine(TPath.GetAppPath, 'tmp');
	if not TDirectory.Exists(Result) then begin
		dbgMsg.Msg('Creating directory: ' + Result);
		TDirectory.CreateDirectory(Result);
	end;
	dbgMsg.Msg('Result: ' + Result);
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
	origIniFile := self.FileName;
	self.Rename(GetTempFileName, false);
	self.Rename(origIniFile, true);
end;

procedure TMemIniFileHelper.WriteTempSectionIni(const _fileName, _section : string);
var 
	sectionList : TStringList;
	isModified : Boolean;
	tmpIniFile : TMemIniFile; newFileName : string;
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TMemIniFileHelper.WriteTempSectionIni');
	sectionList := TStringList.Create();
	try
		self.ReadSectionValues(_section, sectionList);
		newFileName := GetTempSectionFileName(_section);

        if TFile.Exists(newFileName) then begin
			TFile.Delete(newFileName);
		end else begin
            CreateTempDirectory();
        end;

		tmpIniFile := TMemIniFile.Create(newFileName);
		try
			isModified := false;
			for var i := 0 to sectionList.Count - 1 do begin
				var
				key := sectionList.Names[i];
				var
				value := sectionList.ValueFromIndex[i];
				tmpIniFile.WriteString(_section, key, value);
				isModified := true;
			end;
			if isModified then begin
				dbgMsg.Msg('Updating file: ' + newFileName);
				tmpIniFile.UpdateFile();
			end;
		finally
			tmpIniFile.Free;
		end;
	finally
		sectionList.Free;
	end;
end;

procedure TMemIniFileHelper.WriteTempSectionIni(const _section : string);
begin
	WriteTempSectionIni(self.FileName, _section);
end;

end.
