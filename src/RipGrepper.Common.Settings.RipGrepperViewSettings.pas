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
			procedure SetViewSettingValues(const _s : string = '');
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
	CreateSetting('ShowRelativePath', TSettingVariant.New(varBoolean, False));
	CreateSetting('ShowFileIcon', TSettingVariant.New(varBoolean, False));
	CreateSetting('AlternateRowColors', TSettingVariant.New(varBoolean, False));
	CreateSetting('IndentLines', TSettingVariant.New(varBoolean, False));
	CreateSetting('ExpandNodes', TSettingVariant.New(varBoolean, True));
end;

procedure TRipGrepperViewSettings.Load;
begin
	inherited Load();
	ShowRelativePath := GetSetting('ShowRelativePath');
	ShowFileIcon := GetSetting('ShowFileIcon');
	AlternateRowColors := GetSetting('AlternateRowColors');
	IndentLines := GetSetting('IndentLines');
	ExpandNodes := GetSetting('ExpandNodes');

	FIsLoaded := True;
end;

procedure TRipGrepperViewSettings.Store;
begin
	SetViewSettingValues();
	inherited Store();
end;

procedure TRipGrepperViewSettings.SetViewSettingValues(const _s : string = '');
var
	i : integer;
begin
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.SetViewSettingValues: ' + _s);

	i := 0;
	if _s.IsEmpty then begin
		// store all
		for i := 0 to high(VIEW_SETTINGS) do begin
			SetViewSettingValues(VIEW_SETTINGS[i]);
		end;
	end else if MatchStr(_s, VIEW_SETTINGS[i]) then begin
		SetSettingValue(VIEW_SETTINGS[i], ShowRelativePath);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		SetSettingValue(VIEW_SETTINGS[i], ShowFileIcon);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		SetSettingValue(VIEW_SETTINGS[i], AlternateRowColors);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		SetSettingValue(VIEW_SETTINGS[i], IndentLines);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		SetSettingValue(VIEW_SETTINGS[i], ExpandNodes);
	end else begin
		raise Exception.Create('Settings: ' + _s + ' not stored!');
	end;
	IsModified := True;
end;

end.
