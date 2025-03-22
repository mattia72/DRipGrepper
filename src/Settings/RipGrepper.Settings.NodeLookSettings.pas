unit RipGrepper.Settings.NodeLookSettings;

interface

uses
	RipGrepper.Settings.Persistable,
	System.IniFiles,
	RipGrepper.Settings.NodeLook.FilterSettings,
	System.Generics.Collections,
	RipGrepper.Settings.SettingVariant;

const
	VIEW_SETTINGS_TYPES : TArray<string> = [
	{ } 'ShowRelativePath',
	{ } 'ShowFileIcon',
	{ } 'AlternateRowColors',
	{ } 'IndentLines',
	{ } 'ExpandNodes'];

type
	TNodeLookSettings = class(TPersistableSettings)

		const
			INI_SECTION = 'NodeLookSettings';

		private
			FAlternateRowColors : IBoolSetting;
			FExpandNodes : IBoolSetting;
			FFilterSettings : TFilterSettings;
			FIndentLines : IBoolSetting;
			FShowFileIcon : IBoolSetting;
			FShowRelativePath : IBoolSetting;
			function GetAlternateRowColors() : Boolean;
			function GetExpandNodes() : Boolean;
			function GetIndentLines() : Boolean;
			function GetShowFileIcon() : Boolean;
			function GetShowRelativePath() : Boolean;
			procedure SetAlternateRowColors(const Value : Boolean);
			procedure SetExpandNodes(const Value : Boolean);
			procedure SetFilterSettings(const Value : TFilterSettings);
			procedure SetIndentLines(const Value : Boolean);
			procedure SetShowFileIcon(const Value : Boolean);
			procedure SetShowRelativePath(const Value : Boolean);

		public
			constructor Create(const _Owner : TPersistableSettings); overload;
			destructor Destroy; override;
			function GetIsModified : Boolean; override;
			procedure Init; override;
			procedure LoadFromDict(); override;
			procedure ReadIni; override;
			property AlternateRowColors : Boolean read GetAlternateRowColors write SetAlternateRowColors;
			property ExpandNodes : Boolean read GetExpandNodes write SetExpandNodes;
			property FilterSettings : TFilterSettings read FFilterSettings write SetFilterSettings;
			property IndentLines : Boolean read GetIndentLines write SetIndentLines;
			property ShowFileIcon : Boolean read GetShowFileIcon write SetShowFileIcon;
			property ShowRelativePath : Boolean read GetShowRelativePath write SetShowRelativePath;
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
	TDebugUtils.DebugMessage('TNodeLookSettings.Create: ' + '[' + GetIniSectionName + ']');
end;

destructor TNodeLookSettings.Destroy;
begin
	FFilterSettings.Free;
	inherited Destroy(); // ok;
end;

function TNodeLookSettings.GetAlternateRowColors() : Boolean;
begin
	Result := FAlternateRowColors.Value;
end;

function TNodeLookSettings.GetExpandNodes() : Boolean;
begin
	Result := FExpandNodes.Value;
end;

function TNodeLookSettings.GetIndentLines() : Boolean;
begin
	Result := FIndentLines.Value;
end;

function TNodeLookSettings.GetIsModified : Boolean;
begin
	Result := FIsModified or
	{ } FFilterSettings.IsModified;
end;

function TNodeLookSettings.GetShowFileIcon() : Boolean;
begin
	Result := FShowFileIcon.Value;
end;

function TNodeLookSettings.GetShowRelativePath() : Boolean;
begin
	Result := FShowRelativePath.Value;
end;

procedure TNodeLookSettings.Init;
begin
	var
	ssb : TSettingStoreBehaviours := [ssbStoreIfModified,ssbStoreAfterChangeImmediately];
	FShowRelativePath := TBoolSetting.Create(False, ssInitialized, ssb);
	FAlternateRowColors := TBoolSetting.Create(False, ssInitialized, ssb);
	FExpandNodes := TBoolSetting.Create(False, ssInitialized, ssb);
	FIndentLines := TBoolSetting.Create(False, ssInitialized, ssb);
	FShowFileIcon := TBoolSetting.Create(False, ssInitialized, ssb);

	CreateSetting('ShowRelativePath', FShowRelativePath);
	CreateSetting('ShowFileIcon', FShowFileIcon);
	CreateSetting('AlternateRowColors', FAlternateRowColors);
	CreateSetting('IndentLines', FIndentLines);
	CreateSetting('ExpandNodes', FExpandNodes);
	FFilterSettings.Init();
end;

procedure TNodeLookSettings.LoadFromDict;
begin
	FilterSettings.LoadFromDict();
end;

procedure TNodeLookSettings.ReadIni;
begin
	FilterSettings.ReadIni;
	inherited ReadIni();
end;

procedure TNodeLookSettings.SetAlternateRowColors(const Value : Boolean);
begin
	FAlternateRowColors.Value := Value;
end;

procedure TNodeLookSettings.SetExpandNodes(const Value : Boolean);
begin
	FExpandNodes.Value := Value;
end;

procedure TNodeLookSettings.SetFilterSettings(const Value : TFilterSettings);
begin
	FFilterSettings := Value;
end;

procedure TNodeLookSettings.SetIndentLines(const Value : Boolean);
begin
	FIndentLines.Value := Value;
end;

procedure TNodeLookSettings.SetShowFileIcon(const Value : Boolean);
begin
	FShowFileIcon.Value := Value;
end;

procedure TNodeLookSettings.SetShowRelativePath(const Value : Boolean);
begin
	FShowRelativePath.Value := Value;
end;

end.
