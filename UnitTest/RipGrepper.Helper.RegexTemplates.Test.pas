unit RipGrepper.Helper.RegexTemplates.Test;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Helper.RegexTemplates;

type

	[TestFixture]
	TRegexTemplateParseTest = class
		public
			[Test]
			procedure TestParseFullStringCheckedTrue;
			[Test]
			procedure TestParseFullStringCheckedFalse;
			[Test]
			procedure TestParsePatternWithStartAndEndAroundPlaceholder;
			[Test]
			procedure TestParsePatternWithOnlyStartPattern;
			[Test]
			procedure TestParsePatternStartingWithPlaceholder;
			[Test]
			procedure TestParseTwoPartsMakesDescriptionAndPatternSame;
			[Test]
			procedure TestParseOnePartFallsBackToRawString;
			[Test]
			procedure TestParseEmptyString;
			[Test]
			[TestCase('TRUE lowercase', 'true' + #9 + 'Desc' + #9 + 'Start<TEXT>End,True')]
			[TestCase('FALSE uppercase', 'FALSE' + #9 + 'Desc' + #9 + 'Start<TEXT>End,False')]
			procedure TestParseIsCheckedCaseInsensitive(const _input : string; const _expected : Boolean);
	end;

	[TestFixture]
	TRegexTemplateApplyToTextTest = class
		public
			[Test]
			procedure TestApplyToTextWithStartAndEnd;
			[Test]
			procedure TestApplyToTextWithStartOnly;
			[Test]
			procedure TestApplyToTextWithEndOnly;
			[Test]
			procedure TestApplyToTextWithEmptyPatterns;
			[Test]
			procedure TestApplyToTextWithEmptyInput;
			[Test]
			[TestCase('word boundary', 'hello,\bhello\b')]
			[TestCase('empty text', ',\b\b')]
			procedure TestApplyToTextWordBoundary(const _text : string; const _expected : string);
	end;

implementation

uses
	System.SysUtils;

const
	TAB = #9;

{ TRegexTemplateParseTest }

procedure TRegexTemplateParseTest.TestParseFullStringCheckedTrue;
var
	tmpl : TRegexTemplate;
begin
	tmpl := TRegexTemplate.Parse('TRUE' + TAB + 'My Pattern' + TAB + 'Start<TEXT>End');
	Assert.IsTrue(tmpl.IsChecked, 'IsChecked should be True');
	Assert.AreEqual('My Pattern', tmpl.Description, 'Description mismatch');
	Assert.AreEqual('Start', tmpl.StartPattern, 'StartPattern mismatch');
	Assert.AreEqual('End', tmpl.EndPattern, 'EndPattern mismatch');
end;

procedure TRegexTemplateParseTest.TestParseFullStringCheckedFalse;
var
	tmpl : TRegexTemplate;
begin
	tmpl := TRegexTemplate.Parse('FALSE' + TAB + 'Hidden Pattern' + TAB + 'X<TEXT>Y');
	Assert.IsFalse(tmpl.IsChecked, 'IsChecked should be False');
	Assert.AreEqual('Hidden Pattern', tmpl.Description, 'Description mismatch');
	Assert.AreEqual('X', tmpl.StartPattern, 'StartPattern mismatch');
	Assert.AreEqual('Y', tmpl.EndPattern, 'EndPattern mismatch');
end;

procedure TRegexTemplateParseTest.TestParsePatternWithStartAndEndAroundPlaceholder;
var
	tmpl : TRegexTemplate;
begin
	tmpl := TRegexTemplate.Parse('TRUE' + TAB + 'Word boundary' + TAB + '\b<TEXT>\b');
	Assert.AreEqual('\b', tmpl.StartPattern, 'StartPattern should be word boundary');
	Assert.AreEqual('\b', tmpl.EndPattern, 'EndPattern should be word boundary');
end;

procedure TRegexTemplateParseTest.TestParsePatternWithOnlyStartPattern;
var
	tmpl : TRegexTemplate;
begin
	tmpl := TRegexTemplate.Parse('TRUE' + TAB + 'Prefix only' + TAB + '^\w+');
	Assert.AreEqual('^\w+', tmpl.StartPattern, 'StartPattern should be the full pattern');
	Assert.AreEqual('', tmpl.EndPattern, 'EndPattern should be empty');
end;

procedure TRegexTemplateParseTest.TestParsePatternStartingWithPlaceholder;
var
	tmpl : TRegexTemplate;
begin
	tmpl := TRegexTemplate.Parse('TRUE' + TAB + 'Suffix only' + TAB + '<TEXT>\s*$');
	Assert.AreEqual('', tmpl.StartPattern, 'StartPattern should be empty');
	Assert.AreEqual('\s*$', tmpl.EndPattern, 'EndPattern should be suffix');
end;

procedure TRegexTemplateParseTest.TestParseTwoPartsMakesDescriptionAndPatternSame;
var
	tmpl : TRegexTemplate;
begin
	// Two parts: parts[0]=TRUE/FALSE, parts[1]=desc — both Description and patternStr get parts[1]
	tmpl := TRegexTemplate.Parse('TRUE' + TAB + '\b<TEXT>\b');
	Assert.AreEqual('\b<TEXT>\b', tmpl.Description, 'Description should equal the second part');
	Assert.AreEqual('\b', tmpl.StartPattern, 'StartPattern mismatch');
	Assert.AreEqual('\b', tmpl.EndPattern, 'EndPattern mismatch');
end;

procedure TRegexTemplateParseTest.TestParseOnePartFallsBackToRawString;
var
	tmpl : TRegexTemplate;
	raw : string;
begin
	// No tab separator: all parts are just one element, falls to else branch
	raw := '\b<TEXT>\b';
	tmpl := TRegexTemplate.Parse(raw);
	Assert.IsTrue(tmpl.IsChecked, 'Default IsChecked should be True');
	Assert.AreEqual(raw, tmpl.Description, 'Description should be the raw string');
	Assert.AreEqual('\b', tmpl.StartPattern, 'StartPattern mismatch');
	Assert.AreEqual('\b', tmpl.EndPattern, 'EndPattern mismatch');
end;

procedure TRegexTemplateParseTest.TestParseEmptyString;
var
	tmpl : TRegexTemplate;
begin
	tmpl := TRegexTemplate.Parse('');
	Assert.IsTrue(tmpl.IsChecked, 'Default IsChecked should be True');
	Assert.AreEqual('', tmpl.Description, 'Description should be empty');
	Assert.AreEqual('', tmpl.StartPattern, 'StartPattern should be empty');
	Assert.AreEqual('', tmpl.EndPattern, 'EndPattern should be empty');
end;

procedure TRegexTemplateParseTest.TestParseIsCheckedCaseInsensitive(const _input : string; const _expected : Boolean);
var
	tmpl : TRegexTemplate;
begin
	tmpl := TRegexTemplate.Parse(_input);
	Assert.AreEqual(_expected, tmpl.IsChecked, 'IsChecked value mismatch for input: ' + _input);
end;

{ TRegexTemplateApplyToTextTest }

procedure TRegexTemplateApplyToTextTest.TestApplyToTextWithStartAndEnd;
var
	tmpl : TRegexTemplate;
begin
	tmpl := TRegexTemplate.Parse('TRUE' + TAB + 'Test' + TAB + '\b<TEXT>\b');
	Assert.AreEqual('\bhello\b', tmpl.ApplyToText('hello'), 'ApplyToText result mismatch');
end;

procedure TRegexTemplateApplyToTextTest.TestApplyToTextWithStartOnly;
var
	tmpl : TRegexTemplate;
begin
	tmpl.StartPattern := '(?i)';
	tmpl.EndPattern := '';
	Assert.AreEqual('(?i)pattern', tmpl.ApplyToText('pattern'), 'ApplyToText result mismatch');
end;

procedure TRegexTemplateApplyToTextTest.TestApplyToTextWithEndOnly;
var
	tmpl : TRegexTemplate;
begin
	tmpl.StartPattern := '';
	tmpl.EndPattern := '\s*$';
	Assert.AreEqual('text\s*$', tmpl.ApplyToText('text'), 'ApplyToText result mismatch');
end;

procedure TRegexTemplateApplyToTextTest.TestApplyToTextWithEmptyPatterns;
var
	tmpl : TRegexTemplate;
begin
	tmpl.StartPattern := '';
	tmpl.EndPattern := '';
	Assert.AreEqual('search', tmpl.ApplyToText('search'), 'ApplyToText should return text unchanged');
end;

procedure TRegexTemplateApplyToTextTest.TestApplyToTextWithEmptyInput;
var
	tmpl : TRegexTemplate;
begin
	tmpl.StartPattern := '\b';
	tmpl.EndPattern := '\b';
	Assert.AreEqual('\b\b', tmpl.ApplyToText(''), 'ApplyToText with empty text should concatenate patterns only');
end;

procedure TRegexTemplateApplyToTextTest.TestApplyToTextWordBoundary(const _text : string; const _expected : string);
var
	tmpl : TRegexTemplate;
begin
	tmpl.StartPattern := '\b';
	tmpl.EndPattern := '\b';
	Assert.AreEqual(_expected, tmpl.ApplyToText(_text), 'ApplyToText word boundary mismatch');
end;

end.
