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
	separatorPos : Integer;
begin
	separatorPos := Pos(SEPARATOR, _templateStr);

	if separatorPos > 0 then begin
		Result.Description := Copy(_templateStr, 1, separatorPos - 1);
		Result.Pattern := Copy(_templateStr, separatorPos + 1, Length(_templateStr));
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
begin
	count := _templatesSetting.GetArraySetting().Count;

	for i := 0 to count - 1 do begin
		FTemplates.Add(TRegexTemplate.Parse(_templatesSetting.Item[i]));
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
