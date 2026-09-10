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
			[Test]
			procedure EmptyFilePathShouldReturnTrue;
			[Test]
			procedure RelativeFilePathShouldReturnTrue;
			[Test]
			procedure ForwardSlashPathShouldReturnTrue;
			[Test]
			procedure UnnormalizedPathShouldReturnTrue;
			[Test]
			procedure SiblingDirWithSameProjectPrefixShouldReturnFalse;
			[Test]
			procedure SiblingDirWithSameLibraryPrefixShouldReturnFalse;
			[Test]
			procedure TrailingDelimiterInLibraryPathShouldReturnTrue;
			[Test]
			procedure FileInProjectFilesButOutsideProjectDirShouldReturnTrue;
			[Test]
			procedure FileInProjectFilesDirsShouldReturnTrue;
			[Test]
			procedure RelativeLibraryPathShouldBeResolvedAgainstProjectDir;
			[Test]
			procedure UncPathInLibraryPathShouldReturnTrue;
	end;

implementation

uses
	System.SysUtils;

procedure TDelphiIDEContextIsFileInProjectTest.Setup;
begin
	FContext.ActiveProject := 'C:\Projects\MyApp\MyApp.dproj';
	FContext.ProjectFiles := [];
	FContext.ProjectFilesDirs := [];
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

procedure TDelphiIDEContextIsFileInProjectTest.EmptyFilePathShouldReturnTrue;
begin
	var result := FContext.IsFileInProject('');
	Assert.IsTrue(result, 'An empty path is not decidable, so it must not be reported as outside of the project');
end;

procedure TDelphiIDEContextIsFileInProjectTest.RelativeFilePathShouldReturnTrue;
begin
	{ ripgrep reports relative paths when it is called with a relative search path. The base
	  directory of the search isn't part of the IDE context, so such a path is not decidable. }
	var result := FContext.IsFileInProject('src\Unit1.pas');
	Assert.IsTrue(result, 'A relative path is not decidable, so it must not be reported as outside of the project');
end;

procedure TDelphiIDEContextIsFileInProjectTest.ForwardSlashPathShouldReturnTrue;
begin
	var result := FContext.IsFileInProject('C:/Projects/MyApp/src/Unit1.pas');
	Assert.IsTrue(result);
end;

procedure TDelphiIDEContextIsFileInProjectTest.UnnormalizedPathShouldReturnTrue;
begin
	var result := FContext.IsFileInProject('C:\Projects\MyApp\src\..\Unit1.pas');
	Assert.IsTrue(result);
end;

procedure TDelphiIDEContextIsFileInProjectTest.SiblingDirWithSameProjectPrefixShouldReturnFalse;
begin
	var result := FContext.IsFileInProject('C:\Projects\MyAppBackup\Unit1.pas');
	Assert.IsFalse(result, 'A sibling directory starting with the project dir name is not part of the project');
end;

procedure TDelphiIDEContextIsFileInProjectTest.SiblingDirWithSameLibraryPrefixShouldReturnFalse;
begin
	var result := FContext.IsFileInProject('C:\Libraries\Spring4D\SourceOld\Spring.Collections.pas');
	Assert.IsFalse(result, 'A sibling directory starting with a library path name is not part of the project');
end;

procedure TDelphiIDEContextIsFileInProjectTest.TrailingDelimiterInLibraryPathShouldReturnTrue;
var
	ctx : TDelphiIDEContext;
begin
	ctx.ActiveProject := 'C:\Projects\MyApp\MyApp.dproj';
	ctx.ProjectLibraryPath := ['C:\Libs\Valid\'];
	var result := ctx.IsFileInProject('C:\Libs\Valid\SomeUnit.pas');
	Assert.IsTrue(result);
end;

procedure TDelphiIDEContextIsFileInProjectTest.FileInProjectFilesButOutsideProjectDirShouldReturnTrue;
begin
	FContext.ProjectFiles := ['C:\Shared\Common\Utils.pas'];
	var result := FContext.IsFileInProject('C:\Shared\Common\Utils.pas');
	Assert.IsTrue(result, 'A unit of the project may be stored outside of the project directory');

	var result2 := FContext.IsFileInProject('C:\Shared\Common\NotInProject.pas');
	Assert.IsFalse(result2, 'Only the listed project file belongs to the project, not its whole directory');
end;

procedure TDelphiIDEContextIsFileInProjectTest.FileInProjectFilesDirsShouldReturnTrue;
begin
	FContext.ProjectFilesDirs := ['C:\Shared\Common'];
	var result := FContext.IsFileInProject('C:\Shared\Common\Utils.pas');
	Assert.IsTrue(result);
end;

procedure TDelphiIDEContextIsFileInProjectTest.RelativeLibraryPathShouldBeResolvedAgainstProjectDir;
var
	ctx : TDelphiIDEContext;
begin
	ctx.ActiveProject := 'C:\Projects\MyApp\MyApp.dproj';
	ctx.ProjectLibraryPath := ['..\Common'];
	var result := ctx.IsFileInProject('C:\Projects\Common\SomeUnit.pas');
	Assert.IsTrue(result);
end;

procedure TDelphiIDEContextIsFileInProjectTest.UncPathInLibraryPathShouldReturnTrue;
var
	ctx : TDelphiIDEContext;
begin
	ctx.ActiveProject := 'C:\Projects\MyApp\MyApp.dproj';
	ctx.ProjectLibraryPath := ['\\server\share\libs'];
	var result := ctx.IsFileInProject('\\server\share\libs\SomeUnit.pas');
	Assert.IsTrue(result);
end;

initialization

TDUnitX.RegisterTestFixture(TDelphiIDEContextIsFileInProjectTest);

end.
