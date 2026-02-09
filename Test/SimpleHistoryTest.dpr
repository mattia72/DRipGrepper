program SimpleHistoryTest;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls;

type
  // Simulate THistoryButtonedEdit without VCL dependencies for testing
  TSimpleHistoryEdit = class
  private
    FHistory: TStringList;
    FHistoryIndex: Integer;
    FMaxHistoryCount: Integer;
    FText: string;
    function GetHistoryCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure AddToHistory(const AText: string);
    procedure ClearHistory;
    function GetHistoryItems: TArray<string>;
    procedure NavigateHistory(Direction: Integer);
    
    property Text: string read FText write FText;
    property HistoryCount: Integer read GetHistoryCount;
    property HistoryIndex: Integer read FHistoryIndex;
  end;

constructor TSimpleHistoryEdit.Create;
begin
  inherited;
  FHistory := TStringList.Create;
  FHistoryIndex := -1;
  FMaxHistoryCount := 50;
  FText := '';
end;

destructor TSimpleHistoryEdit.Destroy;
begin
  FHistory.Free;
  inherited;
end;

function TSimpleHistoryEdit.GetHistoryCount: Integer;
begin
  Result := FHistory.Count;
end;

procedure TSimpleHistoryEdit.AddToHistory(const AText: string);
var
  trimmedText: string;
  existingIndex: Integer;
begin
  trimmedText := Trim(AText);
  if trimmedText = '' then
    Exit;
    
  existingIndex := FHistory.IndexOf(trimmedText);
  if existingIndex >= 0 then
    FHistory.Delete(existingIndex);
    
  FHistory.Insert(0, trimmedText);
  
  while FHistory.Count > FMaxHistoryCount do
    FHistory.Delete(FHistory.Count - 1);
    
  FHistoryIndex := -1;
end;

procedure TSimpleHistoryEdit.ClearHistory;
begin
  FHistory.Clear;
  FHistoryIndex := -1;
end;

function TSimpleHistoryEdit.GetHistoryItems: TArray<string>;
var
  i: Integer;
begin
  SetLength(Result, FHistory.Count);
  for i := 0 to FHistory.Count - 1 do
    Result[i] := FHistory[i];
end;

procedure TSimpleHistoryEdit.NavigateHistory(Direction: Integer);
var
  newIndex: Integer;
begin
  if FHistory.Count = 0 then
    Exit;
    
  newIndex := FHistoryIndex + Direction;
  
  if newIndex < -1 then
    newIndex := -1
  else if newIndex >= FHistory.Count then
    newIndex := FHistory.Count - 1;
    
  FHistoryIndex := newIndex;
  
  if FHistoryIndex = -1 then
    FText := ''
  else
    FText := FHistory[FHistoryIndex];
end;

// Test procedure
procedure TestHistoryEdit;
var
  edit: TSimpleHistoryEdit;
  items: TArray<string>;
  i: Integer;
begin
  WriteLn('=== THistoryButtonedEdit Functionality Test ===');
  WriteLn;
  
  edit := TSimpleHistoryEdit.Create;
  try
    // Test 1: Add items to history
    WriteLn('Test 1: Adding items to history');
    edit.AddToHistory('First entry');
    edit.AddToHistory('Second entry');
    edit.AddToHistory('Third entry');
    WriteLn('Added 3 items to history');
    WriteLn('History count: ', edit.HistoryCount);
    WriteLn;
    
    // Test 2: Show history
    WriteLn('Test 2: Current history items:');
    items := edit.GetHistoryItems;
    for i := 0 to High(items) do
      WriteLn(Format('  %d: %s', [i + 1, items[i]]));
    WriteLn;
    
    // Test 3: Navigate history
    WriteLn('Test 3: Navigate history with UP arrow (Direction: -1)');
    edit.NavigateHistory(-1); // UP arrow
    WriteLn('Current text: "', edit.Text, '"');
    WriteLn('History index: ', edit.HistoryIndex);
    
    edit.NavigateHistory(-1); // UP arrow again
    WriteLn('After second UP - Current text: "', edit.Text, '"');
    WriteLn('History index: ', edit.HistoryIndex);
    WriteLn;
    
    // Test 4: Navigate down
    WriteLn('Test 4: Navigate with DOWN arrow (Direction: +1)');
    edit.NavigateHistory(1); // DOWN arrow
    WriteLn('Current text: "', edit.Text, '"');
    WriteLn('History index: ', edit.HistoryIndex);
    WriteLn;
    
    // Test 5: Add duplicate
    WriteLn('Test 5: Add duplicate entry (should move to top)');
    edit.AddToHistory('First entry'); // Duplicate
    items := edit.GetHistoryItems;
    WriteLn('History after adding duplicate:');
    for i := 0 to High(items) do
      WriteLn(Format('  %d: %s', [i + 1, items[i]]));
    WriteLn;
    
    // Test 6: Clear history
    WriteLn('Test 6: Clear history');
    edit.ClearHistory;
    WriteLn('History count after clear: ', edit.HistoryCount);
    
  finally
    edit.Free;
  end;
  
  WriteLn;
  WriteLn('=== Test completed ===');
  WriteLn('Press ENTER to exit...');
  ReadLn;
end;

begin
  try
    TestHistoryEdit;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ReadLn;
    end;
  end;
end.
