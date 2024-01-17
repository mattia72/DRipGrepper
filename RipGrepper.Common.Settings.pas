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
			RipGrepParams : TStrings;
			SearchPaths : TStrings;
			SearchTexts : TStrings;
			ShowRelativePath : Boolean;
			ShowFileIcon : Boolean;
			AlternateRowColors : Boolean;
			IndentLines : Boolean;

		private
			procedure LoadHistoryEntries(var _list : TStrings; const _section : string);
			procedure StoreHistoryEntries(const _list : TStrings; const _section : string);

		public
			procedure Load;
			procedure Store;
			procedure StoreViewSettings(const _s : string = '');
			class operator Finalize(var Dest : TRipGrepperSettings);
			class operator Initialize(out Dest : TRipGrepperSettings);
	end;

implementation

uses
	System.SysUtils,
	Vcl.Forms,
	System.StrUtils,
	RipGrepper.Common.Types,
	RipGrepper.Helper.Types,
	RipGrepper.Tools.DebugTools;

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
	Dest.SearchPaths.Free;
	Dest.SearchTexts.Free;
	Dest.RipGrepParams.Free;
end;

class operator TRipGrepperSettings.Initialize(out Dest : TRipGrepperSettings);
begin
	Dest.SettingsFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));

	Dest.RipGrepPath := '';
	Dest.ShowRelativePath := False;
	Dest.ShowFileIcon := False;
	Dest.AlternateRowColors := False;
	Dest.IndentLines := False;
	Dest.SearchPaths := TStringList.Create;
	Dest.SearchTexts := TStringList.Create;
	Dest.RipGrepParams := TStringList.Create;

end;

procedure TRipGrepperSettings.Load;
begin
	RipGrepPath := SettingsFile.ReadString('RipGrepSettings', 'Path', '');

	ShowRelativePath := SettingsFile.ReadBool('RipGrepperSettings', 'ShowRelativePath', False);
	ShowFileIcon := SettingsFile.ReadBool('RipGrepperSettings', 'ShowFileIcon', False);
	AlternateRowColors := SettingsFile.ReadBool('RipGrepperSettings', 'AlternateRowColors', False);
	IndentLines := SettingsFile.ReadBool('RipGrepperSettings', 'IndentLines', False);

	LoadHistoryEntries(SearchPaths, 'SearchPathsHistory');
	LoadHistoryEntries(SearchTexts, 'SearchTextsHistory');
	LoadHistoryEntries(RipGrepParams, 'RipGrepParamsHistory');
end;

procedure TRipGrepperSettings.Store;
begin
	SettingsFile.WriteString('RipGrepSettings', 'Path', RipGrepPath);

	StoreViewSettings();

	StoreHistoryEntries(SearchPaths, 'SearchPathsHistory');
	StoreHistoryEntries(SearchTexts, 'SearchTextsHistory');
	StoreHistoryEntries(RipGrepParams, 'RipGrepParamsHistory');
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
