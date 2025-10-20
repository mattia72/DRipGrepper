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
			function GetContext() : IIntegerSetting;
			function GetEncoding() : IStringSetting;
			function GetExtensionSettings : TRipGrepperExtensionSettings;
			function GetHidden() : IBoolSetting;
			function GetNoIgnore() : IBoolSetting;
			function GetOutputFormat() : IStringSetting;
			function GetPretty() : IBoolSetting;

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

			property Context : IIntegerSetting read GetContext;
			property Encoding : IStringSetting read GetEncoding;
			property ExtensionSettings : TRipGrepperExtensionSettings read GetExtensionSettings write FExtensionSettings;
			property Hidden : IBoolSetting read GetHidden;
			property NoIgnore : IBoolSetting read GetNoIgnore;
			property OutputFormat : IStringSetting read GetOutputFormat;
			property Pretty : IBoolSetting read GetPretty;
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

function TSearchFormSettings.GetContext() : IIntegerSetting;
begin
	Result := FContext;
end;

function TSearchFormSettings.GetEncoding() : IStringSetting;
begin
	Result := FEncoding;
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

function TSearchFormSettings.GetHidden() : IBoolSetting;
begin
	Result := FHidden;
end;

function TSearchFormSettings.GetNoIgnore() : IBoolSetting;
begin
	Result := FNoIgnore;
end;

function TSearchFormSettings.GetOutputFormat() : IStringSetting;
begin
	Result := FOutputFormat;
end;

function TSearchFormSettings.GetPretty() : IBoolSetting;
begin
	Result := FPretty;
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
	FOutputFormat := TStringSetting.Create('OutputFormat', 'json');
	FPretty.Enabled := False; // Pretty is disabled if json output is selected

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
	Hidden.Value := _sr.ReadLineAsBool('Hidden');
	NoIgnore.Value := _sr.ReadLineAsBool('NoIgnore');
	Pretty.Value := _sr.ReadLineAsBool('Pretty');
	Context.Value := _sr.ReadLineAsInteger('Context');
	Encoding.Value := _sr.ReadLineAsString(true, 'Encoding'); // Encoding can potentially be empty
	var
	s := _sr.ReadLineAsString(false, 'OutputFormat');
	if not s.IsEmpty then begin
		OutputFormat.Value := s;
	end;
	ExtensionSettings.LoadFromStreamReader(_sr);
end;

procedure TSearchFormSettings.ReadFile();
begin
	FExtensionSettings.ReadFile;
	inherited ReadFile();
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
	{ } BoolToStr(Hidden.Value),
	{ } BoolToStr(NoIgnore.Value),
	{ } BoolToStr(Pretty.Value),
	{ } Context.Value.ToString,
	{ } Encoding.Value,
	{ } OutputFormat.Value];

	for var i := 0 to Length(strArr) - 1 do begin
		_sw.WriteLineAsString(strArr[i], true, SEARCH_SETTING_NAMES[i]);
	end;

	ExtensionSettings.SaveToStreamWriter(_sw);
end;

function TSearchFormSettings.ToLogString : string;
begin
	Result := Format('Hidden=%s NoIgnore=%s Pretty=%s Context=%s Encoding=%s OutputFormat=%s Extension:[%s]',
		{ } [
		{ } BoolToStr(Hidden.Value),
		{ } BoolToStr(NoIgnore.Value),
		{ } BoolToStr(Pretty.Value),
		{ } Context.Value.ToString,
		{ } Encoding.Value,
		{ } OutputFormat.Value,
		{ } FExtensionSettings.ToLogString]);
end;

end.
