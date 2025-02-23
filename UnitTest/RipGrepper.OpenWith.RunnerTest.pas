unit RipGrepper.OpenWith.RunnerTest;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.OpenWith.Params,
	RipGrepper.OpenWith.Runner;

type

	[TestFixture]
	TOpenWithRunnerTest = class
		public
			[Test]
			procedure TestBuildParams;
	end;

implementation

procedure TOpenWithRunnerTest.TestBuildParams;
var
	owp : TOpenWithParams;
	sParams : string;
	actual : string;
begin
	owp.RelativeBaseDirPath := 'C:\TestDir';
	owp.FilePath := 'TestFile.txt';
	owp.Row := 10;
	owp.Column := 5;

	sParams := '<DIR> <FILE> <LINE> <COL>';
	actual := TOpenWithRunner.BuildParams(owp, sParams);

	Assert.AreEqual('C:\TestDir TestFile.txt 10 5', actual, 'Die Parameter sollten korrekt ersetzt werden.');
end;

initialization

TDUnitX.RegisterTestFixture(TOpenWithRunnerTest);

end.
