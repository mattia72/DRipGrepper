unit RipGrepper.UI.SearchPathHistoryByContextTest;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Common.IDEContextValues,
	RipGrepper.UI.SearchPathHistoryByContext;

type

	[TestFixture]
	TSearchPathHistoryByContextTest = class
		private
			FHistoryByContext : TSearchPathHistoryByContext;

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			procedure GetForContextReturnsEmptyForNewContextTest;
			[Test]
			procedure SetAndGetForContextTest;
			[Test]
			procedure StorePathForContextAddsPathTest;
			[Test]
			procedure StorePathForContextInsertsUniqueTest;
			[Test]
			procedure StorePathForContextIgnoresEmptyPathTest;
			[Test]
			procedure StorePathForContextTrimmsWhitespaceTest;
			[Test]
			procedure DifferentContextsHaveSeparateHistoriesTest;
			[Test]
			procedure StorePathForContextMaintainsOrderTest;
			[Test]
			procedure GetForContextReturnsCorrectContextTest;
			[Test]
			procedure MultiplePathsInContextTest;
	end;

implementation

uses
	System.SysUtils,
	System.Generics.Collections,
	ArrayEx;

procedure TSearchPathHistoryByContextTest.Setup;
begin
	FHistoryByContext := TSearchPathHistoryByContext.Create();
end;

procedure TSearchPathHistoryByContextTest.TearDown;
begin
	FHistoryByContext.Free;
end;

procedure TSearchPathHistoryByContextTest.GetForContextReturnsEmptyForNewContextTest;
var
	result : TArrayEx<string>;
begin
	result := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicCustomLocation);
	Assert.AreEqual(0, result.Count, 'New context should have empty history');
end;

procedure TSearchPathHistoryByContextTest.SetAndGetForContextTest;
var
	paths : TArrayEx<string>;
	result : TArrayEx<string>;
begin
	paths.Add('C:\Project1');
	paths.Add('C:\Project2');

	FHistoryByContext.SetForContext(EDelphiIDESearchContext.dicCustomLocation, paths);
	result := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicCustomLocation);

	Assert.AreEqual(2, result.Count, 'Should have 2 paths');
	Assert.AreEqual('C:\Project1', result[0], 'First path should match');
	Assert.AreEqual('C:\Project2', result[1], 'Second path should match');
end;

procedure TSearchPathHistoryByContextTest.StorePathForContextAddsPathTest;
var
	result : TArrayEx<string>;
begin
	FHistoryByContext.StorePathForContext(EDelphiIDESearchContext.dicCustomLocation, 'C:\TestPath');

	result := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicCustomLocation);
	Assert.AreEqual(1, result.Count, 'Should have 1 path');
	Assert.AreEqual('C:\TestPath', result[0], 'Path should be stored');
end;

procedure TSearchPathHistoryByContextTest.StorePathForContextInsertsUniqueTest;
var
	result : TArrayEx<string>;
begin
	FHistoryByContext.StorePathForContext(EDelphiIDESearchContext.dicCustomLocation, 'C:\TestPath');
	FHistoryByContext.StorePathForContext(EDelphiIDESearchContext.dicCustomLocation, 'C:\TestPath');

	result := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicCustomLocation);
	Assert.AreEqual(1, result.Count, 'Should have only 1 unique path');
	Assert.AreEqual('C:\TestPath', result[0], 'Path should be unique');
end;

procedure TSearchPathHistoryByContextTest.StorePathForContextIgnoresEmptyPathTest;
var
	result : TArrayEx<string>;
begin
	FHistoryByContext.StorePathForContext(EDelphiIDESearchContext.dicCustomLocation, '');

	result := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicCustomLocation);
	Assert.AreEqual(0, result.Count, 'Empty path should be ignored');
end;

procedure TSearchPathHistoryByContextTest.StorePathForContextTrimmsWhitespaceTest;
var
	result : TArrayEx<string>;
begin
	FHistoryByContext.StorePathForContext(EDelphiIDESearchContext.dicCustomLocation, '  C:\TestPath  ');

	result := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicCustomLocation);
	Assert.AreEqual(1, result.Count, 'Should have 1 path');
	Assert.AreEqual('C:\TestPath', result[0], 'Path should be trimmed');
end;

procedure TSearchPathHistoryByContextTest.DifferentContextsHaveSeparateHistoriesTest;
var
	customResult : TArrayEx<string>;
	activeFileResult : TArrayEx<string>;
begin
	FHistoryByContext.StorePathForContext(EDelphiIDESearchContext.dicCustomLocation, 'C:\Custom');
	FHistoryByContext.StorePathForContext(EDelphiIDESearchContext.dicActiveFile, 'C:\Active');

	customResult := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicCustomLocation);
	activeFileResult := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicActiveFile);

	Assert.AreEqual(1, customResult.Count, 'Custom context should have 1 path');
	Assert.AreEqual('C:\Custom', customResult[0], 'Custom context path');
	Assert.AreEqual(1, activeFileResult.Count, 'ActiveFile context should have 1 path');
	Assert.AreEqual('C:\Active', activeFileResult[0], 'ActiveFile context path');
end;

procedure TSearchPathHistoryByContextTest.StorePathForContextMaintainsOrderTest;
var
	result : TArrayEx<string>;
begin
	FHistoryByContext.StorePathForContext(EDelphiIDESearchContext.dicCustomLocation, 'C:\Path1');
	FHistoryByContext.StorePathForContext(EDelphiIDESearchContext.dicCustomLocation, 'C:\Path2');
	FHistoryByContext.StorePathForContext(EDelphiIDESearchContext.dicCustomLocation, 'C:\Path3');

	result := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicCustomLocation);

	Assert.AreEqual(3, result.Count, 'Should have 3 paths');
	Assert.AreEqual('C:\Path3', result[0], 'Newest path should be first');
	Assert.AreEqual('C:\Path2', result[1], 'Second path');
	Assert.AreEqual('C:\Path1', result[2], 'Oldest path should be last');
end;

procedure TSearchPathHistoryByContextTest.GetForContextReturnsCorrectContextTest;
var
	customPaths, activeFilePaths : TArrayEx<string>;
	retrievedCustom, retrievedActiveFile : TArrayEx<string>;
begin
	customPaths.Add('C:\Custom1');
	customPaths.Add('C:\Custom2');
	activeFilePaths.Add('C:\ActiveFile1');

	FHistoryByContext.SetForContext(EDelphiIDESearchContext.dicCustomLocation, customPaths);
	FHistoryByContext.SetForContext(EDelphiIDESearchContext.dicActiveFile, activeFilePaths);

	retrievedCustom := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicCustomLocation);
	retrievedActiveFile := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicActiveFile);

	Assert.AreEqual(2, retrievedCustom.Count, 'Should have 2 custom paths');
	Assert.AreEqual('C:\Custom1', retrievedCustom[0], 'Custom path 1');
	Assert.AreEqual(1, retrievedActiveFile.Count, 'Should have 1 active file path');
	Assert.AreEqual('C:\ActiveFile1', retrievedActiveFile[0], 'Active file path');
end;

procedure TSearchPathHistoryByContextTest.MultiplePathsInContextTest;
var
	result : TArrayEx<string>;
	i : Integer;
begin
	// Store multiple paths
	for i := 1 to 5 do begin
		FHistoryByContext.StorePathForContext(EDelphiIDESearchContext.dicProjectFiles, 'C:\Project' + i.ToString);
	end;

	result := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicProjectFiles);

	Assert.AreEqual(5, result.Count, 'Should have 5 paths');
	Assert.AreEqual('C:\Project5', result[0], 'Most recent path first');
	Assert.AreEqual('C:\Project1', result[4], 'Oldest path last');
end;

initialization

TDUnitX.RegisterTestFixture(TSearchPathHistoryByContextTest);

end.
