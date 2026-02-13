unit RipGrepper.Helper.RegexTemplates;

interface

uses
	System.Classes,
	System.SysUtils,
	ArrayEx,
	RipGrepper.Settings.SettingVariant;

type
	TRegexTemplate = record
		Description : string;
		Pattern : string;
		class function Parse(const _templateStr : string) : TRegexTemplate; static;
		function ApplyToText(const _text : string) : string;
	end;

	TRegexTemplateManager = class
		private
			FTemplates : TArray<TRegexTemplate>;

		public
			constructor Create(const _templatesSetting : IArraySetting);
			procedure LoadTemplates(const _templatesSetting : IArraySetting);
			function GetTemplateCount : Integer;
			function GetTemplate(const _index : Integer) : TRegexTemplate;
			function ApplyTemplate(const _index : Integer; const _currentText : string) : string;
	end;

implementation

{ TRegexTemplate }

class function TRegexTemplate.Parse(const _templateStr : string) : TRegexTemplate;
var
	separatorPos : Integer;
begin
	separatorPos := Pos('|', _templateStr);

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
	if Pos('<text>', Result) > 0 then begin
		Result := StringReplace(Result, '<text>', _text, [rfReplaceAll]);
	end;
end;

{ TRegexTemplateManager }

constructor TRegexTemplateManager.Create(const _templatesSetting : IArraySetting);
begin
	inherited Create;
	LoadTemplates(_templatesSetting);
end;

procedure TRegexTemplateManager.LoadTemplates(const _templatesSetting : IArraySetting);
var
	i : Integer;
	count : Integer;
begin
	count := _templatesSetting.Value.Count;
	SetLength(FTemplates, count);

	for i := 0 to count - 1 do begin
		FTemplates[i] := TRegexTemplate.Parse(_templatesSetting.Value[i]);
	end;
end;

function TRegexTemplateManager.GetTemplateCount : Integer;
begin
	Result := Length(FTemplates);
end;

function TRegexTemplateManager.GetTemplate(const _index : Integer) : TRegexTemplate;
begin
	if (_index >= 0) and (_index < Length(FTemplates)) then begin
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
