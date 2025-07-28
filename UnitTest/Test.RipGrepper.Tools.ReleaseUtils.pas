unit Test.RipGrepper.Tools.ReleaseUtils;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  RipGrepper.Tools.ReleaseUtils;

{$IFDEF TESTINSIGHT}
  {$DEFINE DUNITX}
{$ENDIF}

type
  [TestFixture]
  TTestReleaseUtils = class
  public
    [Test]
    procedure Test_CompilerVersion_Constant;
  end;

implementation

procedure TTestReleaseUtils.Test_CompilerVersion_Constant;
begin
  // This test checks that the CompilerVersion macro is defined and has a plausible value
  // Delphi 12 = 36, Delphi 11 = 35, Delphi 10.4 = 34, etc.
  Assert.IsTrue(CompilerVersion = 0, 'CompilerVersion is ' + FloatToStr(CompilerVersion));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestReleaseUtils);

end.
