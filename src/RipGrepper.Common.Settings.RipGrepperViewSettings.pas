unit RipGrepper.Common.Settings.RipGrepperViewSettings;

interface

uses
	RipGrepper.Common.Settings.Persistable,
	System.IniFiles;

type
	TRipGrepperViewSettings = class(TPersistableSettings)
		const
			VIEW_SETTINGS : array [0 .. 4] of string = (
				{ } 'ShowRelativePath',
				{ } 'ShowFileIcon',
				{ } 'AlternateRowColors',
				{ } 'IndentLines',
				{ } 'ExpandNodes');

		const
			INI_SECTION = 'RipGrepperViewSettings';

		public
			AlternateRowColors : Boolean;
			IndentLines : Boolean;
			ShowFileIcon : Boolean;
			ShowRelativePath : Boolean;
			ExpandNodes : Boolean;
			procedure StoreViewSettings(const _s : string = '');
			constructor Create(const _ini : TMemIniFile);
			procedure Init; override;
			procedure Load; override;
			procedure Store; override;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.StrUtils,
	RipGrepper.Helper.Types,
	System.SysUtils;

constructor TRipGrepperViewSettings.Create(const _ini : TMemIniFile);
begin
	inherited;
	IniSectionName := INI_SECTION;
	TDebugUtils.DebugMessage('TRipGrepperViewSettings.Create: ' + FIniFile.FileName + '[' + GetIniSectionName + ']');
end;

procedure TRipGrepperViewSettings.Init;
begin
	inherited;
	CreateSetting('ShowRelativePath', TRipGrepperSetting.New(varBoolean, False));
	CreateSetting('ShowFileIcon', TRipGrepperSetting.New(varBoolean, False));
	CreateSetting('AlternateRowColors', TRipGrepperSetting.New(varBoolean, False));
	CreateSetting('IndentLines', TRipGrepperSetting.New(varBoolean, False));
	CreateSetting('ExpandNodes', TRipGrepperSetting.New(varBoolean, True));
end;

procedure TRipGrepperViewSettings.Load;
begin
	inherited Load();
	ShowRelativePath := LoadSetting('ShowRelativePath');
	ShowFileIcon := LoadSetting('ShowFileIcon');
	AlternateRowColors := LoadSetting('AlternateRowColors');
	IndentLines := LoadSetting('IndentLines');
	ExpandNodes := LoadSetting('ExpandNodes');

	FIsLoaded := True;
end;

procedure TRipGrepperViewSettings.Store;
begin
	if IsLoaded and IsModified then begin
		StoreViewSettings('');
		inherited Store();
		FIsModified := False;
	end;

end;

procedure TRipGrepperViewSettings.StoreViewSettings(const _s : string = '');
var
	i : integer;
begin
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.StoreViewSettings: ' + _s);

	i := 0;
	if _s.IsEmpty then begin
		// store all
		for i := 0 to high(VIEW_SETTINGS) do begin
			StoreViewSettings(VIEW_SETTINGS[i]);
		end;
	end else if MatchStr(_s, VIEW_SETTINGS[i]) then begin
		StoreSetting(VIEW_SETTINGS[i], ShowRelativePath);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		StoreSetting(VIEW_SETTINGS[i], ShowFileIcon);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		StoreSetting(VIEW_SETTINGS[i], AlternateRowColors);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		StoreSetting(VIEW_SETTINGS[i], IndentLines);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		StoreSetting(VIEW_SETTINGS[i], ExpandNodes);
	end else begin
		raise Exception.Create('Settings: ' + _s + ' not stored!');
	end;
	IsModified := True;
end;

end.
