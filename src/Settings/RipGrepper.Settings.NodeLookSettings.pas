unit RipGrepper.Settings.NodeLookSettings;

interface

uses
	RipGrepper.Settings.Persistable,
	System.IniFiles,
	RipGrepper.Settings.NodeLook.FilterSettings;

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

		private
			FFilterSettings : TFilterSettings;
			procedure SetFilterSettings(const Value : TFilterSettings);

		public
			AlternateRowColors : Boolean;
			IndentLines : Boolean;
			ShowFileIcon : Boolean;
			ShowRelativePath : Boolean;
			ExpandNodes : Boolean;
			procedure SetViewSettingValues(const _s : string = '');
			constructor Create(const _Owner : TPersistableSettings); overload;
			destructor Destroy; override;
			function GetIsModified : Boolean; override;
			procedure Init; override;
			procedure LoadFromDict(); override;
			procedure LoadDefaultsFromDict; override;
			procedure ReadIni; override;
			procedure StoreToDict; override;
			property FilterSettings : TFilterSettings read FFilterSettings write SetFilterSettings;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.StrUtils,
	RipGrepper.Helper.Types,
	System.SysUtils;

constructor TNodeLookSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := INI_SECTION;
	FFilterSettings := TFilterSettings.Create(_Owner);
	inherited;
	TDebugUtils.DebugMessage('TNodeLookSettings.Create: ' + IniFile.FileName + '[' + GetIniSectionName + ']');
end;

destructor TNodeLookSettings.Destroy;
begin
	FFilterSettings.Free;
	inherited Destroy(); // ok;
end;

function TNodeLookSettings.GetIsModified : Boolean;
begin
	Result := FIsModified or
	{ } FFilterSettings.IsModified;
end;

procedure TNodeLookSettings.Init;
begin
	SettingsDict.CreateSetting('ShowRelativePath', varBoolean, False);
	SettingsDict.CreateSetting('ShowFileIcon', varBoolean, False);
	SettingsDict.CreateSetting('AlternateRowColors', varBoolean, False);
	SettingsDict.CreateSetting('IndentLines', varBoolean, False);
	SettingsDict.CreateSetting('ExpandNodes', varBoolean, True);
	FFilterSettings.Init();
end;

procedure TNodeLookSettings.LoadFromDict;
begin
	ShowRelativePath := SettingsDict.GetSetting('ShowRelativePath');
	ShowFileIcon := SettingsDict.GetSetting('ShowFileIcon');
	AlternateRowColors := SettingsDict.GetSetting('AlternateRowColors');
	IndentLines := SettingsDict.GetSetting('IndentLines');
	ExpandNodes := SettingsDict.GetSetting('ExpandNodes');
	FilterSettings.LoadFromDict();
end;

procedure TNodeLookSettings.LoadDefaultsFromDict;
begin
	// abstract
end;

procedure TNodeLookSettings.ReadIni;
begin
	FilterSettings.ReadIni;
	inherited ReadIni();
end;

procedure TNodeLookSettings.SetFilterSettings(const Value : TFilterSettings);
begin
	FFilterSettings := Value;
end;

procedure TNodeLookSettings.StoreToDict;
begin
	SetViewSettingValues();
	FFilterSettings.StoreToDict();
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
		FilterSettings.SetViewSettingValues(_s);
	end;
end;

end.
