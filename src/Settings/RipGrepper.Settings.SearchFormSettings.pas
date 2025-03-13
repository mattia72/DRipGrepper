unit RipGrepper.Settings.SearchFormSettings;

interface

uses
	System.Classes,
	System.IniFiles,
	System.Generics.Collections,
	System.Generics.Defaults,
	ArrayEx,
	RipGrepper.OpenWith.Constants,
	RipGrepper.Common.Constants,
	RipGrepper.Settings.Persistable,
	RipGrepper.Settings.RipGrepParameterSettings,
	RipGrepper.Settings.AppSettings,
	RipGrepper.Settings.ExtensionSettings,
	RipGrepper.Settings.SettingVariant;

type

	TSearchFormSettings = class(TPersistableSettings)
		const
			SEARCH_SETTINGS : array [0 .. 4] of string =
			{ } ('Pretty', 'Hidden', 'NoIgnore', 'Context', 'Encoding');

		const
			INI_SECTION = 'RipGrepperSearchSettings';

		private

			FContext : IIntegerSetting;
			FEncoding : IStringSetting;
			FExtensionSettings : TRipGrepperExtensionSettings;
			FHidden : IBoolSetting;
			FNoIgnore : IBoolSetting;
			FPretty : IBoolSetting;
			function GetContext : Integer;
			function GetEncoding : string;
			function GetExtensionSettings : TRipGrepperExtensionSettings;
			function GetHidden : Boolean;
			function GetNoIgnore : Boolean;
			function GetPretty : Boolean;
			procedure SetContext(const Value : Integer);
			procedure SetEncoding(const Value : string);
			procedure SetHidden(const Value : Boolean);
			procedure SetNoIgnore(const Value : Boolean);
			procedure SetPretty(const Value : Boolean);

		public
			constructor Create(const _Owner : TPersistableSettings); overload;
			constructor Create; overload;
			destructor Destroy; override;

			procedure Init; override;
			procedure ReadIni; override;
			procedure StoreToPersister; override;
			procedure Copy(const _other : TSearchFormSettings); reintroduce;
			procedure LoadFromDict(); override;
			procedure ReLoad; override;
			function ToLogString : string; override;

			property Context : Integer read GetContext write SetContext;
			property Encoding : string read GetEncoding write SetEncoding;
			property ExtensionSettings : TRipGrepperExtensionSettings read GetExtensionSettings write FExtensionSettings;
			property Hidden : Boolean read GetHidden write SetHidden;
			property NoIgnore : Boolean read GetNoIgnore write SetNoIgnore;
			property Pretty : Boolean read GetPretty write SetPretty;
	end;

implementation

uses
	System.SysUtils,
	Vcl.Forms,
	System.StrUtils,
	RipGrepper.Helper.Types,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Tools.FileUtils,
	Vcl.Dialogs,
	System.IOUtils,
	Winapi.Windows,
	System.UITypes,
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.Helper.UI,
	Vcl.Menus,
	System.RegularExpressions,
	RipGrepper.CommandLine.Builder;

constructor TSearchFormSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := INI_SECTION;
	inherited Create(_Owner);

	FExtensionSettings := TRipGrepperExtensionSettings.Create(self);
	AddChildSettings(FExtensionSettings);
end;

constructor TSearchFormSettings.Create;
begin
	IniSectionName := INI_SECTION;
	inherited Create;

	FExtensionSettings := TRipGrepperExtensionSettings.Create();
	AddChildSettings(FExtensionSettings);
end;

destructor TSearchFormSettings.Destroy;
begin
	inherited Destroy(); // childs will be freed - ok;
end;

procedure TSearchFormSettings.Copy(const _other : TSearchFormSettings);
begin
	if Assigned(_other) then begin
		inherited Copy(_other as TPersistableSettings);
		FExtensionSettings.Copy(_other.ExtensionSettings);
	end;
end;

function TSearchFormSettings.GetContext : Integer;
begin
	Result := FContext.Value;
end;

function TSearchFormSettings.GetEncoding : string;
begin
	Result := FEncoding.Value;
end;

function TSearchFormSettings.GetExtensionSettings : TRipGrepperExtensionSettings;
begin
	if not FExtensionSettings.IsAlreadyRead then begin
		var
		dbgMsg := TDebugMsgBeginEnd.New('TSearchFormSettings.GetExtensionSettings');

		FExtensionSettings.ReadIni;
	end;
	Result := FExtensionSettings;
end;

function TSearchFormSettings.GetHidden : Boolean;
begin
	Result := FHidden.Value;
end;

function TSearchFormSettings.GetNoIgnore : Boolean;
begin
	Result := FNoIgnore.Value;
end;

function TSearchFormSettings.GetPretty : Boolean;
begin
	Result := FPretty.Value;
end;

procedure TSearchFormSettings.Init;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSearchFormSettings.Init');

	FPretty := TBoolSetting.Create(False);
	FHidden := TBoolSetting.Create(False);
	FNoIgnore := TBoolSetting.Create(False);
	FContext := TIntegerSetting.Create(0);
	FEncoding := TStringSetting.Create('');

	CreateSetting('Pretty', FPretty);
	CreateSetting('Hidden', FHidden);
	CreateSetting('NoIgnore', FNoIgnore);
	CreateSetting('Context', FContext);
	CreateSetting('Encoding', FEncoding);
end;

procedure TSearchFormSettings.ReadIni;
begin
	FExtensionSettings.ReadIni;
	inherited ReadIni();
end;

procedure TSearchFormSettings.SetContext(const Value : Integer);
begin
	FContext.Value := Value;
end;

procedure TSearchFormSettings.SetEncoding(const Value : string);
begin
	FEncoding.Value := Value;
end;

procedure TSearchFormSettings.SetHidden(const Value : Boolean);
begin
	FHidden.Value := Value;
end;

procedure TSearchFormSettings.SetNoIgnore(const Value : Boolean);
begin
	FNoIgnore.Value := Value;
end;

procedure TSearchFormSettings.SetPretty(const Value : Boolean);
begin
	FPretty.Value := Value;
end;

procedure TSearchFormSettings.StoreToPersister; // extension switch off if TESTINSIGHT
begin
	FExtensionSettings.StoreToPersister;
	inherited StoreToPersister();
end;

procedure TSearchFormSettings.LoadFromDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSearchFormSettings.LoadFromDict');
end;

procedure TSearchFormSettings.ReLoad;
begin
	FExtensionSettings.ReLoad;
	inherited;
end;

function TSearchFormSettings.ToLogString : string;
begin
	Result := Format('Hidden=%s NoIgnore=%s Pretty=%s Context=%d Encoding=%s Extension:[%s]',
		{ } [BoolToStr(Hidden, True),
		{ } BoolToStr(NoIgnore, True),
		{ } BoolToStr(Pretty, True),
		{ } Context,
		{ } Encoding, FExtensionSettings.ToLogString]);
end;

end.
