unit RipGrepper.Common.NodeData.Test;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Common.NodeData;

type

	[TestFixture]
	TVSFileNodeDataTest = class

		public
			[Test]
			procedure TestNewDefaultFileLastWriteTime;
			[Test]
			procedure TestNewOverloadDefaultFileLastWriteTime;
			[Test]
			procedure TestFileLastWriteTimeAssignment;
			[Test]
			procedure TestFileLastWriteTimeCompareOrdering;
	end;

implementation

uses
	System.SysUtils,
	System.Math,
	System.DateUtils;

procedure TVSFileNodeDataTest.TestNewDefaultFileLastWriteTime;
var
	nodeData : TVSFileNodeData;
begin
	// The simple New overload should default FileLastWriteTime to 0
	nodeData := TVSFileNodeData.New('C:\test\file.txt');
	Assert.AreEqual(Double(0), Double(nodeData.FileLastWriteTime), 'FileLastWriteTime should default to 0');
end;

procedure TVSFileNodeDataTest.TestNewOverloadDefaultFileLastWriteTime;
var
	nodeData : TVSFileNodeData;
begin
	// The full New overload should default FileLastWriteTime to 0
	nodeData := TVSFileNodeData.New('C:\test\file.txt', 1, 1, 5, 'before', 'match', 'after');
	Assert.AreEqual(Double(0), Double(nodeData.FileLastWriteTime), 'FileLastWriteTime should default to 0');
end;

procedure TVSFileNodeDataTest.TestFileLastWriteTimeAssignment;
var
	nodeData : TVSFileNodeData;
	testDate : TDateTime;
begin
	// FileLastWriteTime should be assignable and retrievable
	testDate := EncodeDateTime(2025, 3, 10, 14, 30, 0, 0);
	nodeData := TVSFileNodeData.New('C:\test\file.txt');
	nodeData.FileLastWriteTime := testDate;
	Assert.AreEqual(Double(testDate), Double(nodeData.FileLastWriteTime), 'FileLastWriteTime should retain the assigned value');
end;

procedure TVSFileNodeDataTest.TestFileLastWriteTimeCompareOrdering;
var
	nodeData1, nodeData2 : TVSFileNodeData;
	date1, date2 : TDateTime;
begin
	// Verify that comparison of FileLastWriteTime works for sorting
	date1 := EncodeDateTime(2025, 1, 1, 0, 0, 0, 0);
	date2 := EncodeDateTime(2025, 3, 10, 0, 0, 0, 0);

	nodeData1 := TVSFileNodeData.New('file1.txt');
	nodeData1.FileLastWriteTime := date1;

	nodeData2 := TVSFileNodeData.New('file2.txt');
	nodeData2.FileLastWriteTime := date2;

	Assert.IsTrue(CompareValue(nodeData1.FileLastWriteTime, nodeData2.FileLastWriteTime) < 0,
		'Earlier date should compare less than later date');
	Assert.IsTrue(CompareValue(nodeData2.FileLastWriteTime, nodeData1.FileLastWriteTime) > 0,
		'Later date should compare greater than earlier date');
	Assert.IsTrue(CompareValue(nodeData1.FileLastWriteTime, nodeData1.FileLastWriteTime) = 0,
		'Same dates should compare equal');
end;

initialization

TDUnitX.RegisterTestFixture(TVSFileNodeDataTest);

end.
