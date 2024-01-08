unit RipGrepperSettings;

interface

uses
	System.Classes,
	System.IniFiles;

type
	TRipGrepperSettings = record
		const
			MAX_HISTORY_COUNT = 20;

		var
			SettingsFile : TIniFile;
			RipGrepPath : string;
			RipGrepParams : TStrings;
			SearchDirs : TStrings;
			SearchTexts : TStrings;

		private
			procedure LoadHistoryEntries(var _list : TStrings; const _section : string);
			procedure StoreHistoryEntries(const _list : TStrings; const _section : string);
		public
			procedure Load;
			procedure Store;
			class operator Finalize(var Dest : TRipGrepperSettings);
			class operator Initialize(out Dest : TRipGrepperSettings);
	end;

implementation

uses
	System.SysUtils,
	Vcl.Forms;

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
	Dest.SearchDirs.Free;
	Dest.SearchTexts.Free;
	Dest.RipGrepParams.Free;
end;

class operator TRipGrepperSettings.Initialize(out Dest : TRipGrepperSettings);
begin
	Dest.SettingsFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
	Dest.SearchDirs := TStringList.Create;
	Dest.SearchTexts := TStringList.Create;
	Dest.RipGrepParams := TStringList.Create;
	Dest.RipGrepPath := '';
end;

procedure TRipGrepperSettings.Load;
begin
	RipGrepPath := SettingsFile.ReadString('RipGrepSettings', 'Path', '');
	LoadHistoryEntries(SearchDirs, 'SearchDirsHistory');
	LoadHistoryEntries(SearchTexts, 'SearchTextsHistory');
	LoadHistoryEntries(RipGrepParams, 'RipGrepParamsHistory');
end;

procedure TRipGrepperSettings.StoreHistoryEntries(const _list : TStrings; const _section : string);
begin
	for var i := _list.Count -1 downto 0 do begin
		SettingsFile.WriteString(_section, 'Item_' + i.ToString, _list[i]);
	end;
end;

procedure TRipGrepperSettings.Store;
begin
	SettingsFile.WriteString('RipGrepSettings', 'Path', RipGrepPath);
	StoreHistoryEntries(SearchDirs, 'SearchDirsHistory');
	StoreHistoryEntries(SearchTexts, 'SearchTextsHistory');
	StoreHistoryEntries(RipGrepParams, 'RipGrepParamsHistory');
end;

end.
