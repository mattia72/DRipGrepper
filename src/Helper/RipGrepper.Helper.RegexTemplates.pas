unit RipGrepper.Helper.RegexTemplates;

interface

uses
	System.Classes,
	System.SysUtils,
	ArrayEx,
	RipGrepper.Settings.SettingVariant,
	RipGrepper.Settings.Persistable;

type
	TRegexTemplate = record
		const
			TEXT_PLACEHOLDER = '<text>';

		var
			Description : string;
			IsChecked : Boolean;
			Pattern : string;
			class function Parse(const _templateStr : string) : TRegexTemplate; static;
			function ApplyToText(const _text : string) : string;
	end;

	TRegexTemplateManager = class
		private
			FTemplates : TArrayEx<TRegexTemplate>;

		public
			constructor Create(const _templatesSetting : IPersistableArray);
			procedure LoadTemplates(const _templatesSetting : IPersistableArray);
			function GetTemplateCount : Integer;
			function GetTemplate(const _index : Integer) : TRegexTemplate;
			function ApplyTemplate(const _index : Integer; const _currentText : string) : string;
	end;

implementation

uses
  RipGrepper.OpenWith.Constants;

{ TRegexTemplate }

class function TRegexTemplate.Parse(const _templateStr : string) : TRegexTemplate;
var
	parts : TArray<string>;
	startIdx : Integer;
begin
	Result.IsChecked := True; // default: visible in menu

	// Handle checked prefix (TRUE/FALSE stored by TTabSeparatedConfigForm)
	parts := _templateStr.Split([SEPARATOR]);
	startIdx := 0;

	if (Length(parts) > 0) and SameText(parts[0], 'TRUE') then begin
		Result.IsChecked := True;
		startIdx := 1;
	end else if (Length(parts) > 0) and SameText(parts[0], 'FALSE') then begin
		Result.IsChecked := False;
		startIdx := 1;
	end;

	if Length(parts) >= startIdx + 2 then begin
		Result.Description := parts[startIdx];
		Result.Pattern := parts[startIdx + 1];
	end else if Length(parts) = startIdx + 1 then begin
		Result.Description := parts[startIdx];
		Result.Pattern := parts[startIdx];
	end else begin
		Result.Description := _templateStr;
		Result.Pattern := _templateStr;
	end;
end;

function TRegexTemplate.ApplyToText(const _text : string) : string;
begin
	Result := Pattern;

	// Replace <text> placeholder with provided text
	if Pos(TEXT_PLACEHOLDER, Result) > 0 then begin
		Result := StringReplace(Result, TEXT_PLACEHOLDER, _text, [rfReplaceAll]);
	end;
end;

{ TRegexTemplateManager }

constructor TRegexTemplateManager.Create(const _templatesSetting : IPersistableArray);
begin
	inherited Create;
	LoadTemplates(_templatesSetting);
end;

procedure TRegexTemplateManager.LoadTemplates(const _templatesSetting : IPersistableArray);
var
	i : Integer;
	count : Integer;
	tmpl : TRegexTemplate;
begin
	FTemplates.Clear;
	count := _templatesSetting.GetArraySetting().Count;

	for i := 0 to count - 1 do begin
		tmpl := TRegexTemplate.Parse(_templatesSetting.Item[i]);
		if tmpl.IsChecked then begin
			FTemplates.Add(tmpl);
		end;
	end;
end;

function TRegexTemplateManager.GetTemplateCount : Integer;
begin
	Result := FTemplates.Count;
end;

function TRegexTemplateManager.GetTemplate(const _index : Integer) : TRegexTemplate;
begin
	if (_index >= 0) and (_index < FTemplates.Count) then begin
		Result := FTemplates[_index];
	end else begin
		raise Exception.CreateFmt('Template index %d out of bounds', [_index]);
	end;
end;

function TRegexTemplateManager.ApplyTemplate(const _index : Integer; const _currentText : string) : string;
var
	template : TRegexTemplate;
begin
	template := GetTemplate(_index);
	Result := template.ApplyToText(_currentText);
end;

end.
