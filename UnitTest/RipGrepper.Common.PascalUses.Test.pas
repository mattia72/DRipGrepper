unit RipGrepper.Common.PascalUses.Test;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Common.PascalUses;

type
	[TestFixture]
	TPascalUsesTest = class
		private
			procedure checkInsertion(const _source, _unitName, _expected : string; _toImplementation : Boolean);
			procedure checkUnchanged(const _source, _unitName : string; _expected : TAddUnitResult);
		public
			[Test]
			procedure AppendToInterface();
			[Test]
			procedure AppendToImplementation();
			[Test]
			procedure CreateMissingClauses();
			[Test]
			procedure FindDuplicatesInEitherSection();
			[Test]
			procedure IgnoreCommentsAndStrings();
			[Test]
			procedure HandleNamespacedUnitsAndInFiles();
			[Test]
			procedure PreserveUnicodeAndLineEndings();
			[Test]
			procedure SkipConditionalDeclarations();
			[Test]
			procedure IgnoreEscapedKeywords();
			[Test]
			procedure RefuseSelfReference();
			[Test]
			procedure RefuseConditionalHeaders();
			[Test]
			procedure RefuseIncludeHeaders();
			[Test]
			procedure RefuseMalformedSource();
			[Test]
			procedure RefuseInvalidUnitNames();
	end;

implementation

uses
	System.SysUtils;

procedure TPascalUsesTest.checkInsertion(const _source, _unitName, _expected : string; _toImplementation : Boolean);
var
	insertion : TUsesInsertion;
begin
	Assert.AreEqual<TAddUnitResult>(aurAdded, TPascalUses.PlanInsertion(_source, _unitName, _toImplementation, insertion));
	var actual := _source;
	Insert(insertion.Text, actual, insertion.Position + 1);
	Assert.AreEqual(_expected, actual);
	// The same plan must identify the original byte prefix when applied through ToolsAPI.
	var originalBytes := UTF8Encode(_source);
	var prefixBytes := UTF8Encode(Copy(_source, 1, insertion.Position));
	var actualBytes := originalBytes;
	Insert(UTF8Encode(insertion.Text), actualBytes, Length(prefixBytes) + 1);
	Assert.AreEqual(_expected, UTF8ToString(actualBytes));
end;

procedure TPascalUsesTest.checkUnchanged(const _source, _unitName : string; _expected : TAddUnitResult);
var
	insertion : TUsesInsertion;
begin
	for var toImplementation in [False, True] do begin
		Assert.AreEqual<TAddUnitResult>(_expected,
			{ } TPascalUses.PlanInsertion(_source, _unitName, toImplementation, insertion));
		Assert.AreEqual('', insertion.Text);
	end;
end;

procedure TPascalUsesTest.AppendToInterface();
begin
	checkInsertion('unit Example; interface uses System.SysUtils; implementation end.', 'NewUnit',
		'unit Example; interface uses System.SysUtils,' + sLineBreak + #9 + 'NewUnit; implementation end.', False);
end;

procedure TPascalUsesTest.AppendToImplementation();
begin
	checkInsertion('unit Example; interface uses PublicUnit; implementation uses PrivateUnit; end.', 'NewUnit',
		'unit Example; interface uses PublicUnit; implementation uses PrivateUnit,' +
		{ } sLineBreak + #9 + 'NewUnit; end.', True);
end;

procedure TPascalUsesTest.CreateMissingClauses();
begin
	checkInsertion('unit Example; interface implementation end.', 'NewUnit',
		'unit Example; interface' + sLineBreak + sLineBreak + 'uses' + sLineBreak + #9 +
		{ } 'NewUnit; implementation end.', False);
	checkInsertion('unit Example; interface implementation end.', 'NewUnit',
		'unit Example; interface implementation' + sLineBreak + sLineBreak + 'uses' +
		{ } sLineBreak + #9 + 'NewUnit; end.', True);
end;

procedure TPascalUsesTest.FindDuplicatesInEitherSection();
begin
	checkUnchanged('unit Example; interface uses My.UnitName; implementation end.', 'my.unitname', aurInInterface);
	checkUnchanged('unit Example; interface implementation uses My.UnitName; end.', 'my.unitname', aurInImplementation);
end;

procedure TPascalUsesTest.IgnoreCommentsAndStrings();
begin
	var source := 'unit Example; { implementation uses NewUnit; } interface ' +
		'uses Original { keep me }; const S = ''implementation '''' uses NewUnit;''; ' +
		'(* implementation *) // implementation' + #10 + 'implementation end.';
	var expected := source.Replace('Original { keep me };', 'Original { keep me },' + #10 + #9 + 'NewUnit;');
	checkInsertion(source, 'NewUnit', expected, False);
end;

procedure TPascalUsesTest.HandleNamespacedUnitsAndInFiles();
begin
	var source := 'unit Company.Example; interface uses Company.OldUnit in ''Old.Unit.pas''; implementation end.';
	checkInsertion(source, 'Company.NewUnit', source.Replace('''Old.Unit.pas'';',
		'''Old.Unit.pas'',' + sLineBreak + #9 + 'Company.NewUnit;'), False);
	checkUnchanged(source, 'company.oldunit', aurInInterface);
end;

procedure TPascalUsesTest.PreserveUnicodeAndLineEndings();
begin
	var source := #$FEFF + 'unit Example;' + #10 + '{ Árvíztűrő 漢字 😀 }' + #10 +
		'interface' + #10 + 'uses OldUnit;' + #10 + 'implementation end.';
	checkInsertion(source, 'Company.NewUnit',
		{ } source.Replace('OldUnit;', 'OldUnit,' + #10 + #9 + 'Company.NewUnit;'), False);
	source := 'unit Example;' + #13#10 + 'interface uses OldUnit; implementation end.';
	checkInsertion(source, 'NewUnit', source.Replace('OldUnit;', 'OldUnit,' + #13#10 + #9 + 'NewUnit;'), False);
end;

procedure TPascalUsesTest.SkipConditionalDeclarations();
begin
	var source := 'unit Example; interface uses OldUnit; ' +
		'{$IFDEF TEST} const N = 1; {$IFDEF OTHER} const M = 2; {$ENDIF} {$ENDIF} implementation end.';
	checkInsertion(source, 'NewUnit', source.Replace('OldUnit;', 'OldUnit,' + sLineBreak + #9 + 'NewUnit;'), False);
end;

procedure TPascalUsesTest.IgnoreEscapedKeywords();
begin
	var source := 'unit Example; interface uses OldUnit; const &implementation = 1; implementation end.';
	checkInsertion(source, 'NewUnit', source.Replace('OldUnit;', 'OldUnit,' + sLineBreak + #9 + 'NewUnit;'), False);
	checkUnchanged('unit Example; interface uses Company.&Unit; implementation end.', 'Company.Unit', aurInInterface);
end;

procedure TPascalUsesTest.RefuseSelfReference();
begin
	checkUnchanged('unit Company.Example; interface implementation end.', 'company.example', aurSameUnit);
end;

procedure TPascalUsesTest.RefuseConditionalHeaders();
begin
	checkUnchanged('unit Example; interface uses OldUnit {$IFDEF TEST}, OtherUnit {$ENDIF}; implementation end.',
		'NewUnit', aurUnsupportedSource);
	checkUnchanged('unit Example; interface {$IFDEF TEST} uses OldUnit; {$ENDIF} implementation end.',
		'NewUnit', aurUnsupportedSource);
	checkUnchanged('unit Example; interface {$IFDEF TEST} implementation {$ENDIF} end.', 'NewUnit', aurUnsupportedSource);
	checkUnchanged('unit Example; interface implementation uses {$IFDEF TEST} OldUnit {$ENDIF}; end.',
		'NewUnit', aurUnsupportedSource);
end;

procedure TPascalUsesTest.RefuseIncludeHeaders();
begin
	checkUnchanged('unit Example; {$I Header.inc} interface implementation end.', 'NewUnit', aurUnsupportedSource);
	checkUnchanged('unit Example; interface uses {$INCLUDE Units.inc}; implementation end.',
		{ } 'NewUnit', aurUnsupportedSource);
	checkUnchanged('unit Example; interface type {$I Types.inc} implementation end.', 'NewUnit', aurUnsupportedSource);
end;

procedure TPascalUsesTest.RefuseMalformedSource();
begin
	checkUnchanged('', 'NewUnit', aurUnsupportedSource);
	checkUnchanged('program Example; begin end.', 'NewUnit', aurUnsupportedSource);
	checkUnchanged('unit Example; interface uses MissingSemicolon implementation end.', 'NewUnit', aurUnsupportedSource);
	checkUnchanged('unit Example; interface uses OldUnit; { unterminated', 'NewUnit', aurUnsupportedSource);
	checkUnchanged('unit Example; interface const S = ''unterminated; implementation end.',
		{ } 'NewUnit', aurUnsupportedSource);
end;

procedure TPascalUsesTest.RefuseInvalidUnitNames();
begin
	for var unitName in TArray<string>.Create('', 'Name;', 'A..B', 'A B', '1Name', 'Name{comment}') do begin
		checkUnchanged('unit Example; interface implementation end.', unitName, aurUnsupportedSource);
	end;
end;

initialization
	TDUnitX.RegisterTestFixture(TPascalUsesTest);

end.
