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
			INI_SECTION = 'RipGrepperSearchSettings';
			SEARCH_SETTING_NAMES : array [0 .. 5] of string = (
			{ } 'Hidden',
			{ } 'NoIgnore',
			{ } 'Pretty',
			{ } 'Context',
			{ } 'Encoding',
			{ } 'OutputFormat');

		private

			FContext : IIntegerSetting;
			FEncoding : IStringSetting;
			FExtensionSettings : TRipGrepperExtensionSettings;
			FHidden : IBoolSetting;
			FNoIgnore : IBoolSetting;
			FOutputFormat : IStringSetting;
			FPretty : IBoolSetting;
			function GetContext : Integer;
			function GetEncoding : string;
			function GetExtensionSettings : TRipGrepperExtensionSettings;
			function GetHidden : Boolean;
			function GetNoIgnore : Boolean;
			function GetOutputFormat : string;
			function GetPretty : Boolean;
			procedure SetContext(const Value : Integer);
			procedure SetEncoding(const Value : string);
			procedure SetHidden(const Value : Boolean);
			procedure SetNoIgnore(const Value : Boolean);
			procedure SetOutputFormat(const Value : string);
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
			property OutputFormat : string read GetOutputFormat write SetOutputFormat;
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

function TSearchFormSettings.GetOutputFormat : string;
begin
	Result := FOutputFormat.Value;
end;

function TSearchFormSettings.GetPretty : Boolean;
begin
	Result := FPretty.Value;
end;

procedure TSearchFormSettings.Init;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSearchFormSettings.Init');

	FPretty := TBoolSetting.Create('Pretty', False);
	FHidden := TBoolSetting.Create('Hidden', False);
	FNoIgnore := TBoolSetting.Create('NoIgnore', False);
	FContext := TIntegerSetting.Create('Context', 0);
	FEncoding := TStringSetting.Create('Encoding', '');
	FOutputFormat := TStringSetting.Create('OutputFormat', '');

	CreateSetting(FPretty);
	CreateSetting(FHidden);
	CreateSetting(FNoIgnore);
	CreateSetting(FContext);
	CreateSetting(FEncoding);
	CreateSetting(FOutputFormat);
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
	OutputFormat := _sr.ReadLineAsString(false, 'OutputFormat'); // OutputFormat can potentially be empty
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

procedure TSearchFormSettings.SetOutputFormat(const Value : string);
begin
	FOutputFormat.Value := Value;
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
	var
	strArr := [
	{ } BoolToStr(Hidden),
	{ } BoolToStr(NoIgnore),
	{ } BoolToStr(Pretty),
	{ } Context.ToString,
	{ } Encoding,
	{ } OutputFormat];

	for var i := 0 to Length(strArr) - 1 do begin
		_sw.WriteLineAsString(strArr[i], true, SEARCH_SETTING_NAMES[i]);
	end;

	ExtensionSettings.SaveToStreamWriter(_sw);
end;

function TSearchFormSettings.ToLogString : string;
begin
	Result := Format('Hidden=%s NoIgnore=%s Pretty=%s Context=%s Encoding=%s OutputFormat=%s Extension:[%s]',
		{ } [
		{ } BoolToStr(Hidden),
		{ } BoolToStr(NoIgnore),
		{ } BoolToStr(Pretty),
		{ } Context.ToString,
		{ } Encoding,
		{ } OutputFormat,
		{ } FExtensionSettings.ToLogString]);
end;

end.
