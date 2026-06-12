unit RipGrepper.Settings.ExtensionSettings.Test;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Settings.ExtensionSettings;

type

	[TestFixture]
	TDelphiIDEContextIsFileInProjectTest = class

		private
			FContext : TDelphiIDEContext;

		public
			[Setup]
			procedure Setup;

			[Test]
			procedure FileInProjectDirShouldReturnTrue;
			[Test]
			procedure FileInSubDirOfProjectShouldReturnTrue;
			[Test]
			procedure FileOutsideProjectShouldReturnFalse;
			[Test]
			procedure FileInLibraryPathShouldReturnTrue;
			[Test]
			procedure FileInSecondLibraryPathShouldReturnTrue;
			[Test]
			procedure EmptyProjectShouldReturnTrue;
			[Test]
			procedure CaseInsensitiveMatchShouldReturnTrue;
			[Test]
			procedure EmptyLibraryPathEntryShouldBeSkipped;
	end;

implementation

uses
	System.SysUtils;

procedure TDelphiIDEContextIsFileInProjectTest.Setup;
begin
	FContext.ActiveProject := 'C:\Projects\MyApp\MyApp.dproj';
	FContext.ProjectLibraryPath := [
		'C:\Libraries\Spring4D\Source',
		'C:\Libraries\VirtualTreeView\Source'
	];
end;

procedure TDelphiIDEContextIsFileInProjectTest.FileInProjectDirShouldReturnTrue;
begin
	var result := FContext.IsFileInProject('C:\Projects\MyApp\Unit1.pas');
	Assert.IsTrue(result);
end;

procedure TDelphiIDEContextIsFileInProjectTest.FileInSubDirOfProjectShouldReturnTrue;
begin
	var result := FContext.IsFileInProject('C:\Projects\MyApp\src\Unit1.pas');
	Assert.IsTrue(result);
end;

procedure TDelphiIDEContextIsFileInProjectTest.FileOutsideProjectShouldReturnFalse;
begin
	var result := FContext.IsFileInProject('C:\OtherFolder\SomeUnit.pas');
	Assert.IsFalse(result);
end;

procedure TDelphiIDEContextIsFileInProjectTest.FileInLibraryPathShouldReturnTrue;
begin
	var result := FContext.IsFileInProject('C:\Libraries\Spring4D\Source\Spring.Collections.pas');
	Assert.IsTrue(result);
end;

procedure TDelphiIDEContextIsFileInProjectTest.FileInSecondLibraryPathShouldReturnTrue;
begin
	var result := FContext.IsFileInProject('C:\Libraries\VirtualTreeView\Source\VirtualTrees.pas');
	Assert.IsTrue(result);
end;

procedure TDelphiIDEContextIsFileInProjectTest.EmptyProjectShouldReturnTrue;
var
	emptyContext : TDelphiIDEContext;
begin
	emptyContext.ActiveProject := '';
	emptyContext.ProjectLibraryPath := [];
	var result := emptyContext.IsFileInProject('C:\Anywhere\SomeFile.pas');
	Assert.IsTrue(result);
end;

procedure TDelphiIDEContextIsFileInProjectTest.CaseInsensitiveMatchShouldReturnTrue;
begin
	var result := FContext.IsFileInProject('c:\projects\myapp\unit1.pas');
	Assert.IsTrue(result);
end;

procedure TDelphiIDEContextIsFileInProjectTest.EmptyLibraryPathEntryShouldBeSkipped;
var
	ctx : TDelphiIDEContext;
begin
	ctx.ActiveProject := 'C:\Projects\MyApp\MyApp.dproj';
	ctx.ProjectLibraryPath := ['', 'C:\Libs\Valid'];
	var result := ctx.IsFileInProject('C:\Libs\Valid\SomeUnit.pas');
	Assert.IsTrue(result);
	// File not in project dir and not in empty path should be false
	var result2 := ctx.IsFileInProject('C:\Other\SomeUnit.pas');
	Assert.IsFalse(result2);
end;

initialization

TDUnitX.RegisterTestFixture(TDelphiIDEContextIsFileInProjectTest);

end.
