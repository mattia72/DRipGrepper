unit RipGrepper.UI.SearchForm.StoreCmbHistoriesTest;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Common.IDEContextValues,
	RipGrepper.UI.SearchPathHistoryByContext,
	RipGrepper.UI.SearchForm.CtrlValueProxy;

type

	[TestFixture]
	TStoreCmbHistoriesTest = class
		private
			FHistoryByContext : TSearchPathHistoryByContext;
			FCtrlProxy : TSearchFormCtrlValueProxy;
			FContextSearchPath : string;

			// Helper methods to simulate form behavior
			procedure SimulateStorePath(const _path : string; const _context : EDelphiIDESearchContext; 
				const _isContextPath : Boolean);

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			procedure CustomLocationPathShouldBePersistentTest;
			[Test]
			procedure IDEContextPathShouldNotBePersistentTest;
			[Test]
			procedure ContextSwitchPreservesOldContextPathTest;
			[Test]
			procedure OnlyCustomLocationHistoryPersistedTest;
			[Test]
			procedure DisplayLabelNotStoredAsPathTest;
			[Test]
			procedure MultipleContextsKeepSeparateHistoriesTest;
			[Test]
			procedure CustomLocationHistoryMaintainsInsertionOrderTest;
			[Test]
			procedure IDEContextHistoryNotAffectCustomLocationTest;
			[Test]
			procedure EmptyPathNotStoredTest;
			[Test]
			procedure UniquePathEnforcedInContextTest;

	end;

implementation

uses
	System.SysUtils,
	System.Generics.Collections,
	ArrayEx;

procedure TStoreCmbHistoriesTest.Setup;
begin
	FHistoryByContext := TSearchPathHistoryByContext.Create();
	FCtrlProxy.SearchPathHist.Clear;
	FContextSearchPath := '';
end;

procedure TStoreCmbHistoriesTest.TearDown;
begin
	FHistoryByContext.Free;
end;

procedure TStoreCmbHistoriesTest.SimulateStorePath(const _path : string; const _context : EDelphiIDESearchContext;
	const _isContextPath : Boolean);
begin
	var
	searchPathToStore : string;
	if _isContextPath then begin
		FContextSearchPath := _path;
		searchPathToStore := FContextSearchPath;
	end else begin
		searchPathToStore := _path;
	end;

	if not searchPathToStore.IsEmpty then begin
		// Always store in context-specific dictionary
		FHistoryByContext.StorePathForContext(_context, searchPathToStore);

		// Only update persistent history for custom location context
		if _context = EDelphiIDESearchContext.dicCustomLocation then begin
			FCtrlProxy.SearchPathHist.InsertUnique(0, searchPathToStore);
		end;
	end;
end;

procedure TStoreCmbHistoriesTest.CustomLocationPathShouldBePersistentTest;
begin
	SimulateStorePath('C:\CustomPath', EDelphiIDESearchContext.dicCustomLocation, False);

	// Custom location paths should be in both in-memory and persistent
	var
	contextHist := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicCustomLocation);
	Assert.AreEqual(1, contextHist.Count, 'Context history should have path');
	Assert.AreEqual('C:\CustomPath', contextHist[0], 'Context history should contain path');
	Assert.AreEqual(1, FCtrlProxy.SearchPathHist.Count, 'Persistent history should have path');
	Assert.AreEqual('C:\CustomPath', FCtrlProxy.SearchPathHist[0], 'Persistent history should contain path');
end;

procedure TStoreCmbHistoriesTest.IDEContextPathShouldNotBePersistentTest;
begin
	SimulateStorePath('C:\ProjectLibraryPath', EDelphiIDESearchContext.dicProjectLibraryPath, True);

	// IDE context paths should be in context-specific history but NOT persistent
	var
	contextHist := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicProjectLibraryPath);
	Assert.AreEqual(1, contextHist.Count, 'IDE context history should have path');
	Assert.AreEqual('C:\ProjectLibraryPath', contextHist[0], 'IDE context should contain path');
	Assert.AreEqual(0, FCtrlProxy.SearchPathHist.Count, 'Persistent history should be empty for IDE context');
end;

procedure TStoreCmbHistoriesTest.ContextSwitchPreservesOldContextPathTest;
begin
	// Store path in Custom Location
	SimulateStorePath('C:\Custom1', EDelphiIDESearchContext.dicCustomLocation, False);

	// Store path in IDE context (simulating context switch)
	SimulateStorePath('C:\Project1', EDelphiIDESearchContext.dicActiveFile, True);

	// Both contexts should maintain their paths
	var
	customHist := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicCustomLocation);
	var
	activeFileHist := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicActiveFile);

	Assert.AreEqual(1, customHist.Count, 'Custom context should preserve path');
	Assert.AreEqual('C:\Custom1', customHist[0], 'Custom context path should be preserved');
	Assert.AreEqual(1, activeFileHist.Count, 'Active file context should have path');
	Assert.AreEqual('C:\Project1', activeFileHist[0], 'Active file path should be stored');
end;

procedure TStoreCmbHistoriesTest.OnlyCustomLocationHistoryPersistedTest;
begin
	SimulateStorePath('C:\Custom1', EDelphiIDESearchContext.dicCustomLocation, False);
	SimulateStorePath('C:\Custom2', EDelphiIDESearchContext.dicCustomLocation, False);
	SimulateStorePath('C:\ProjectFiles', EDelphiIDESearchContext.dicProjectFiles, True);
	SimulateStorePath('C:\OpenFiles', EDelphiIDESearchContext.dicOpenFiles, True);

	// Only custom location should be in persistent history
	Assert.AreEqual(2, FCtrlProxy.SearchPathHist.Count, 'Only 2 custom paths should be persistent');
	Assert.AreEqual('C:\Custom2', FCtrlProxy.SearchPathHist[0], 'Second custom path should be first (newest)');
	Assert.AreEqual('C:\Custom1', FCtrlProxy.SearchPathHist[1], 'First custom path should be second (older)');

	// IDE contexts should have their own in-memory histories
	var
	projectHist := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicProjectFiles);
	var
	openHist := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicOpenFiles);
	Assert.AreEqual(1, projectHist.Count, 'Project files context should have path');
	Assert.AreEqual(1, openHist.Count, 'Open files context should have path');
end;

procedure TStoreCmbHistoriesTest.DisplayLabelNotStoredAsPathTest;
begin
	// Simulate a long path being truncated for display
	var
	fullPath := 'C:\Path1;C:\Path2;C:\Path3;C:\Path4;C:\Path5';
	var
	displayLabel := '5 paths (e.g. C:\Path1)';

	// Only the actual path should be stored, not the display label
	SimulateStorePath(fullPath, EDelphiIDESearchContext.dicProjectLibraryPath, True);

	var
	contextHist := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicProjectLibraryPath);
	Assert.AreEqual(1, contextHist.Count, 'Should have the full path');
	Assert.AreEqual(fullPath, contextHist[0], 'Should store actual path, not display label');
end;

procedure TStoreCmbHistoriesTest.MultipleContextsKeepSeparateHistoriesTest;
begin
	// Store paths in multiple contexts
	SimulateStorePath('C:\Custom1', EDelphiIDESearchContext.dicCustomLocation, False);
	SimulateStorePath('C:\Custom2', EDelphiIDESearchContext.dicCustomLocation, False);
	SimulateStorePath('C:\ActiveFile', EDelphiIDESearchContext.dicActiveFile, True);
	SimulateStorePath('C:\ProjectLibrary', EDelphiIDESearchContext.dicProjectLibraryPath, True);

	var
	customHist := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicCustomLocation);
	var
	activeHist := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicActiveFile);
	var
	libHist := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicProjectLibraryPath);

	Assert.AreEqual(2, customHist.Count, 'Custom context should have 2 paths');
	Assert.AreEqual(1, activeHist.Count, 'Active file context should have 1 path');
	Assert.AreEqual(1, libHist.Count, 'Project library context should have 1 path');

	// Each should maintain its own content
	Assert.AreEqual('C:\Custom2', customHist[0], 'Custom most recent');
	Assert.AreEqual('C:\ActiveFile', activeHist[0], 'Active file path');
	Assert.AreEqual('C:\ProjectLibrary', libHist[0], 'Project library path');
end;

procedure TStoreCmbHistoriesTest.CustomLocationHistoryMaintainsInsertionOrderTest;
begin
	SimulateStorePath('C:\Path1', EDelphiIDESearchContext.dicCustomLocation, False);
	SimulateStorePath('C:\Path2', EDelphiIDESearchContext.dicCustomLocation, False);
	SimulateStorePath('C:\Path3', EDelphiIDESearchContext.dicCustomLocation, False);

	Assert.AreEqual(3, FCtrlProxy.SearchPathHist.Count, 'Should have 3 persistent paths');
	Assert.AreEqual('C:\Path3', FCtrlProxy.SearchPathHist[0], 'Most recent should be first');
	Assert.AreEqual('C:\Path2', FCtrlProxy.SearchPathHist[1], 'Middle should be second');
	Assert.AreEqual('C:\Path1', FCtrlProxy.SearchPathHist[2], 'Oldest should be last');
end;

procedure TStoreCmbHistoriesTest.IDEContextHistoryNotAffectCustomLocationTest;
begin
	SimulateStorePath('C:\Custom', EDelphiIDESearchContext.dicCustomLocation, False);

	// Store multiple IDE context paths
	SimulateStorePath('C:\ProjectLibrary1', EDelphiIDESearchContext.dicProjectLibraryPath, True);
	SimulateStorePath('C:\ProjectLibrary2', EDelphiIDESearchContext.dicProjectLibraryPath, True);
	SimulateStorePath('C:\ProjectFiles', EDelphiIDESearchContext.dicProjectFiles, True);

	// Custom location persistent history should only have the custom path
	Assert.AreEqual(1, FCtrlProxy.SearchPathHist.Count, 'Persistent history should have only custom path');
	Assert.AreEqual('C:\Custom', FCtrlProxy.SearchPathHist[0], 'Persistent history should be custom path');

	// IDE contexts should have their own histories
	var
	libHist := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicProjectLibraryPath);
	Assert.AreEqual(2, libHist.Count, 'Project library should have 2 paths');
end;

procedure TStoreCmbHistoriesTest.EmptyPathNotStoredTest;
begin
	// Attempt to store empty path
	FHistoryByContext.StorePathForContext(EDelphiIDESearchContext.dicCustomLocation, '');

	var
	contextHist := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicCustomLocation);
	Assert.AreEqual(0, contextHist.Count, 'Empty path should not be stored');
end;

procedure TStoreCmbHistoriesTest.UniquePathEnforcedInContextTest;
begin
	SimulateStorePath('C:\Path1', EDelphiIDESearchContext.dicCustomLocation, False);
	SimulateStorePath('C:\Path1', EDelphiIDESearchContext.dicCustomLocation, False);

	var
	contextHist := FHistoryByContext.GetForContext(EDelphiIDESearchContext.dicCustomLocation);
	Assert.AreEqual(1, contextHist.Count, 'Duplicate path should not be added');
	Assert.AreEqual('C:\Path1', contextHist[0], 'Path should exist only once');
end;

initialization

TDUnitX.RegisterTestFixture(TStoreCmbHistoriesTest);

end.
