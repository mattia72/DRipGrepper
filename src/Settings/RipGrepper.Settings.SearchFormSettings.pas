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
	RipGrepper.Common.Interfaces.StreamPersistable,
	RipGrepper.Settings.PersistableArray;
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

			SEARCH_SETTING_NAMES_V2 : array [0 .. 3] of string = (
					{ } 'FormLeft',
					{ } 'FormTop',
					{ } 'FormWidth',
					{ } 'FormHeight');

		private
			FContext : IIntegerSetting;
			FEncoding : IStringSetting;
			FExtensionSettings : TRipGrepperExtensionSettings;
			FHidden : IBoolSetting;
			FNoIgnore : IBoolSetting;
			FOutputFormat : IStringSetting;
			FPretty : IBoolSetting;
			FFormLeft : IIntegerSetting;
			FFormTop : IIntegerSetting;
			FFormWidth : IIntegerSetting;
			FFormHeight : IIntegerSetting;
			FRegexTemplates : TPersistableArray;
			function GetContext() : IIntegerSetting;
			function GetEncoding() : IStringSetting;
			function GetExtensionSettings : TRipGrepperExtensionSettings;
			function GetHidden() : IBoolSetting;
			function GetNoIgnore() : IBoolSetting;
			function GetOutputFormat() : IStringSetting;
			function GetPretty() : IBoolSetting;
			function GetFormLeft() : IIntegerSetting;
			function GetFormTop() : IIntegerSetting;
			function GetFormWidth() : IIntegerSetting;
			function GetFormHeight() : IIntegerSetting;
			function GetRegexTemplates() : IPersistableArray;
			procedure SetRegexTemplates(const _Value : IPersistableArray);

		protected
			procedure LoadVersionDependentSettings(_sr : TStreamReader); override;
			procedure SaveVersionDependentSettings(_sw : TStreamWriter); override;

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
			property FormLeft : IIntegerSetting read GetFormLeft;
			property FormTop : IIntegerSetting read GetFormTop;
			property FormWidth : IIntegerSetting read GetFormWidth;
			property FormHeight : IIntegerSetting read GetFormHeight;
			property RegexTemplates : IPersistableArray read GetRegexTemplates write SetRegexTemplates;
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
	RipGrepper.Helper.StreamReaderWriter,
	RipGrepper.Helper.SettingStoreBehaviours;

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
		FRegexTemplates.Copy(_other.RegexTemplates);
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

function TSearchFormSettings.GetFormLeft() : IIntegerSetting;
begin
	Result := FFormLeft;
end;

function TSearchFormSettings.GetFormTop() : IIntegerSetting;
begin
	Result := FFormTop;
end;

function TSearchFormSettings.GetFormWidth() : IIntegerSetting;
begin
	Result := FFormWidth;
end;

function TSearchFormSettings.GetFormHeight() : IIntegerSetting;
begin
	Result := FFormHeight;
end;

function TSearchFormSettings.GetRegexTemplates() : IPersistableArray;
begin
	Result := FRegexTemplates;
end;

procedure TSearchFormSettings.SetRegexTemplates(const _Value : IPersistableArray);
begin
	if Assigned(FRegexTemplates) then begin
		var
			arrs : IArraySetting := FRegexTemplates.ArraySetting;
		for var s in _Value.ArraySetting.Value do begin
			arrs.AddIfNotContains(s);
		end;
		FRegexTemplates.ArraySetting := arrs;
	end;
end;

procedure TSearchFormSettings.LoadVersionDependentSettings(_sr : TStreamReader);
begin
	if StreamFormatVersion >= 2 then begin
		// FormLeft.Value := _sr.ReadLineAsInteger('FormLeft');
		// FormTop.Value := _sr.ReadLineAsInteger('FormTop');
		// FormWidth.Value := _sr.ReadLineAsInteger('FormWidth');
		// FormHeight.Value := _sr.ReadLineAsInteger('FormHeight');
	end;
end;

procedure TSearchFormSettings.SaveVersionDependentSettings(_sw : TStreamWriter);
begin
	if StreamFormatVersion >= 2 then begin
		// _sw.WriteLineAsString(FormLeft.Value.ToString, true, 'FormLeft');
		// _sw.WriteLineAsString(FormTop.Value.ToString, true, 'FormTop');
		// _sw.WriteLineAsString(FormWidth.Value.ToString, true, 'FormWidth');
		// _sw.WriteLineAsString(FormHeight.Value.ToString, true, 'FormHeight');
	end;
end;

procedure TSearchFormSettings.Init;
var
	arrSetting : IArraySetting;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSearchFormSettings.Init');

	FPretty := TBoolSetting.Create('Pretty', False);
	FHidden := TBoolSetting.Create('Hidden', False);
	FNoIgnore := TBoolSetting.Create('NoIgnore', False);
	FContext := TIntegerSetting.Create('Context', 0);
	FEncoding := TStringSetting.Create('Encoding', '');
	FOutputFormat := TStringSetting.Create('OutputFormat', OUTPUT_FORMAT_JSON);
	FPretty.Enabled := False; // Pretty is disabled if json output is selected
	FFormLeft := TIntegerSetting.Create('FormLeft', -1);
	FFormTop := TIntegerSetting.Create('FormTop', -1);
	FFormWidth := TIntegerSetting.Create('FormWidth', -1);
	FFormHeight := TIntegerSetting.Create('FormHeight', -1);

	CreateSetting(FPretty);
	CreateSetting(FHidden);
	CreateSetting(FNoIgnore);
	CreateSetting(FContext);
	CreateSetting(FEncoding);
	CreateSetting(FOutputFormat);
	CreateSetting(FFormLeft);
	CreateSetting(FFormTop);
	CreateSetting(FFormWidth);
	CreateSetting(FFormHeight);

	if not Assigned(FRegexTemplates) then begin
		arrSetting :=
		{ } TArraySetting.Create('RegexTemplates', ssInitialized, [ssbStoreOnceEvenIfNotModified]);

		// Set default regex templates
		if arrSetting.Count = 0 then begin
			arrSetting.Add(SEARCH_AS_TYPE);
			arrSetting.Add(SEARCH_AS_DECLARATION);
			arrSetting.Add(SEARCH_AS_FUNCTION);
		end;
		CreateSetting(arrSetting.Name, ITEM_KEY_PREFIX, arrSetting);

		FRegexTemplates := TPersistableArray.Create('RegexTemplates', arrSetting);
		// Add to FChildren only once; calling Init() a second time must not re-add
		AddChildSettings(FRegexTemplates);
	end;
end;

procedure TSearchFormSettings.LoadFromStreamReader(_sr : TStreamReader);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSearchFormSettings.LoadFromStreamReader');
	inherited LoadFromStreamReader(_sr);
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
	FRegexTemplates.LoadFromDict();
	ExtensionSettings.LoadFromStreamReader(_sr);
end;

procedure TSearchFormSettings.ReadFile();
begin
	FExtensionSettings.ReadFile;
	inherited ReadFile();
end;

procedure TSearchFormSettings.StoreToPersister; // extension switch off if TESTINSIGHT
begin
	FRegexTemplates.StoreToPersister;
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
	inherited SaveToStreamWriter(_sw);

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
	Result := Format
			('Hidden=%s NoIgnore=%s Pretty=%s Context=%s Encoding=%s OutputFormat=%s FormLeft=%d FormTop=%d FormWidth=%d FormHeight=%d Extension:[%s]',
			{ } [
			{ } BoolToStr(Hidden.Value),
			{ } BoolToStr(NoIgnore.Value),
			{ } BoolToStr(Pretty.Value),
			{ } Context.Value.ToString,
			{ } Encoding.Value,
			{ } OutputFormat.Value,
			{ } FormLeft.Value,
			{ } FormTop.Value,
			{ } FormWidth.Value,
			{ } FormHeight.Value,
			{ } FExtensionSettings.ToLogString]);
end;

end.
