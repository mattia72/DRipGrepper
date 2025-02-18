unit RipGrepper.Tools.Replacer.Test;

interface

uses
    DUnitX.TestFramework,
    RipGrepper.Tools.Replacer,
    System.SysUtils;

type
    [TestFixture]
    TTestReplaceHelper = class
    public
        [Test]
        procedure TestReplaceString_UseRegex;
        [Test]
        procedure TestReplaceString_IgnoreCase;
		[Test]
		procedure TestReplaceStringFromCol_IgnoreCase();
        [Test]
        procedure TestReplaceString_SimpleReplace;
    end;

implementation

procedure TTestReplaceHelper.TestReplaceString_UseRegex;
var
    input, pattern, replacement, result: string;
    mode: TReplaceModes;
begin
    input :=
'Hello Delphi Delphi';
    pattern := '\w+';
    replacement := 'World';
    mode := [rmUseRegex];
    result := TReplaceHelper.ReplaceString(input, pattern, replacement, 14, mode);
    Assert.AreEqual('Hello Delphi World', result);
end;

procedure TTestReplaceHelper.TestReplaceString_IgnoreCase;
var
    input, pattern, replacement, result: string;
    mode: TReplaceModes;
begin
    input := 'Hello World';
    pattern := 'world';
    replacement := 'Delphi';
    mode := [rmIgnoreCase];
    result := TReplaceHelper.ReplaceString(input, pattern, replacement, 0, mode);
    Assert.AreEqual('Hello Delphi', result);
end;

procedure TTestReplaceHelper.TestReplaceStringFromCol_IgnoreCase();
var
	input, pattern, replacement, result: string;
	mode: TReplaceModes;
begin
	input := 'Hello Delphi Delphi';
	pattern := 'delphi';
	replacement := 'World';
	mode := [rmIgnoreCase];
	result := TReplaceHelper.ReplaceString(input, pattern, replacement, 14, mode);
	Assert.AreEqual('Hello Delphi World', result);
end;

procedure TTestReplaceHelper.TestReplaceString_SimpleReplace;
var
    input, pattern, replacement, result: string;
    mode: TReplaceModes;
begin
    input := 'Hello World';
    pattern := 'World';
    replacement := 'Delphi';
    mode := [];
    result := TReplaceHelper.ReplaceString(input, pattern, replacement, 0, mode);
    Assert.AreEqual('Hello Delphi', result);
end;

initialization
    TDUnitX.RegisterTestFixture(TTestReplaceHelper);

end.