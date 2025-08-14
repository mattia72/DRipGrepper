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
	RipGrepper.Settings.SettingVariant,
	RipGrepper.Common.Interfaces.StreamPersistable;

type

	TSearchFormSettings = class(TPersistableSettings, IStreamReaderWriterPersistable)
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
			procedure ReadFile(); override;
			procedure StoreToPersister; override;
			procedure Copy(const _other : TSearchFormSettings); reintroduce;
			procedure LoadFromStreamReader(_sr : TStreamReader);
			procedure ReLoad; override;
			procedure SaveToStreamWriter(_sw : TStreamWriter);
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
	RipGrepper.CommandLine.Builder,
	RipGrepper.Helper.StreamReaderWriter;

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
	// if not FExtensionSettings.IsAlreadyRead then begin
	// var
	// dbgMsg := TDebugMsgBeginEnd.New('TSearchFormSettings.GetExtensionSettings');
	//
	// FExtensionSettings.ReadFile;
	// end;
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

procedure TSearchFormSettings.LoadFromStreamReader(_sr : TStreamReader);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSearchFormSettings.LoadFromStreamReader');
	inherited;
	Hidden := _sr.ReadLineAsBool('Hidden');
	NoIgnore := _sr.ReadLineAsBool('NoIgnore');
	Pretty := _sr.ReadLineAsBool('Pretty');
	Context := _sr.ReadLineAsInteger('Context');
	Encoding := _sr.ReadLineAsString(true, 'Encoding'); // Encoding can potentially be empty
	ExtensionSettings.LoadFromStreamReader(_sr);
end;

procedure TSearchFormSettings.ReadFile();
begin
	FExtensionSettings.ReadFile;
	inherited ReadFile();
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

procedure TSearchFormSettings.ReLoad;
begin
	FExtensionSettings.ReLoad;
	inherited;
end;

procedure TSearchFormSettings.SaveToStreamWriter(_sw : TStreamWriter);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSearchFormSettings.SaveToStreamWriter');
	inherited;

	for var s : string in [
	{ } BoolToStr(Hidden),
	{ } BoolToStr(NoIgnore),
	{ } BoolToStr(Pretty),
	{ } Context.ToString,
	{ } Encoding]
	{ } do begin
		_sw.WriteLineAsString(s, true);
	end;

	ExtensionSettings.SaveToStreamWriter(_sw);
end;

function TSearchFormSettings.ToLogString : string;
begin
	Result := Format('Hidden=%s NoIgnore=%s Pretty=%s Context=%s Encoding=%s Extension:[%s]',
		{ } [
		{ } BoolToStr(Hidden),
		{ } BoolToStr(NoIgnore),
		{ } BoolToStr(Pretty),
		{ } Context.ToString,
		{ } Encoding,
		{ } FExtensionSettings.ToLogString]);
end;

end.
