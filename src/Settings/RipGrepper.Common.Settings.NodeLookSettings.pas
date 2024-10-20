unit RipGrepper.Common.Settings.NodeLookSettings;

interface

uses
	RipGrepper.Common.Settings.Persistable,
	System.IniFiles;

type
	TNodeLookSettings = class(TPersistableSettings)
		const
			VIEW_SETTINGS : array [0 .. 4] of string = (
				{ } 'ShowRelativePath',
				{ } 'ShowFileIcon',
				{ } 'AlternateRowColors',
				{ } 'IndentLines',
				{ } 'ExpandNodes');

		const
			INI_SECTION = 'NodeLookSettings';

		public
			AlternateRowColors : Boolean;
			IndentLines : Boolean;
			ShowFileIcon : Boolean;
			ShowRelativePath : Boolean;
			ExpandNodes : Boolean;
			procedure SetViewSettingValues(const _s : string = '');
			constructor Create(const _ini : TMemIniFile);
			procedure Init; override;
			procedure LoadFromDict(); override;
			procedure LoadDefaultsFromDict; override;
			procedure StoreToDict; override;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.StrUtils,
	RipGrepper.Helper.Types,
	System.SysUtils;

constructor TNodeLookSettings.Create(const _ini : TMemIniFile);
begin
	IniSectionName := INI_SECTION;
	inherited;
	TDebugUtils.DebugMessage('TNodeLookSettings.Create: ' + FIniFile.FileName + '[' + GetIniSectionName + ']');
end;

procedure TNodeLookSettings.Init;
begin
	SettingsDict.CreateSetting('ShowRelativePath', varBoolean, False);
	SettingsDict.CreateSetting('ShowFileIcon', varBoolean, False);
	SettingsDict.CreateSetting('AlternateRowColors', varBoolean, False);
	SettingsDict.CreateSetting('IndentLines', varBoolean, False);
	SettingsDict.CreateSetting('ExpandNodes', varBoolean, True);
end;

procedure TNodeLookSettings.LoadFromDict;
begin
	ShowRelativePath := SettingsDict.GetSetting('ShowRelativePath');
	ShowFileIcon := SettingsDict.GetSetting('ShowFileIcon');
	AlternateRowColors := SettingsDict.GetSetting('AlternateRowColors');
	IndentLines := SettingsDict.GetSetting('IndentLines');
	ExpandNodes := SettingsDict.GetSetting('ExpandNodes');
end;

procedure TNodeLookSettings.LoadDefaultsFromDict;
begin
//
end;

procedure TNodeLookSettings.StoreToDict;
begin
	SetViewSettingValues();
	inherited StoreToDict();
end;

procedure TNodeLookSettings.SetViewSettingValues(const _s : string = '');
var
	i : integer;
begin
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.SetViewSettingValues: ' + _s);

	i := 0;
	if _s.IsEmpty then begin
		// StoreToDict all
		for i := 0 to high(VIEW_SETTINGS) do begin
			SetViewSettingValues(VIEW_SETTINGS[i]);
		end;
	end else if MatchStr(_s, VIEW_SETTINGS[i]) then begin
		SettingsDict.SetSettingValue(VIEW_SETTINGS[i], ShowRelativePath);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		SettingsDict.SetSettingValue(VIEW_SETTINGS[i], ShowFileIcon);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		SettingsDict.SetSettingValue(VIEW_SETTINGS[i], AlternateRowColors);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		SettingsDict.SetSettingValue(VIEW_SETTINGS[i], IndentLines);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		SettingsDict.SetSettingValue(VIEW_SETTINGS[i], ExpandNodes);
	end else begin
		raise Exception.Create('Settings: ' + _s + ' not stored!');
	end;
end;

end.
