unit RipGrepper.UI.HistoryButtonedEdit;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Classes,
	Vcl.Controls,
	Vcl.StdCtrls,
	Vcl.ExtCtrls,
	System.Generics.Collections;

type
	// Custom ButtonedEdit control with history functionality
	THistoryButtonedEdit = class(TButtonedEdit)
		private
			FHistory: TList<string>;
			FHistoryIndex: Integer;
			FMaxHistoryCount: Integer;
			FNavigating: Boolean;
			
			procedure saveToHistory();
			procedure navigateHistory(_direction: Integer);
			function getCurrentHistoryText(): string;
			function getHistoryCount(): Integer;
			
		protected
			procedure KeyDown(var Key: Word; Shift: TShiftState); override;
			procedure DoExit(); override;
			
		public
			constructor Create(AOwner: TComponent); override;
			destructor Destroy(); override;
			
			// Public methods to manage history
			procedure AddToHistory(const _text: string);
			procedure ClearHistory();
			function GetHistoryItems(): TArray<string>;
			procedure LoadFromHistory(const _items: TArray<string>);
			
			// Properties
			property MaxHistoryCount: Integer read FMaxHistoryCount write FMaxHistoryCount;
			property HistoryIndex: Integer read FHistoryIndex;
			property HistoryCount: Integer read getHistoryCount;
			property CurrentHistoryText: string read getCurrentHistoryText;
	end;

procedure Register;

implementation

uses
	Vcl.Forms;

{ THistoryButtonedEdit }

constructor THistoryButtonedEdit.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FHistory := TList<string>.Create();
	FHistoryIndex := -1;
	FMaxHistoryCount := 50; // Default maximum history items
	FNavigating := False;
end;

destructor THistoryButtonedEdit.Destroy();
begin
	FHistory.Free();
	inherited Destroy();
end;

function THistoryButtonedEdit.getHistoryCount(): Integer;
begin
	Result := FHistory.Count;
end;

procedure THistoryButtonedEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
	// Handle Up/Down arrow keys for history navigation
	if Shift = [] then begin
		case Key of
			VK_UP: begin
				navigateHistory(-1); // Go to previous item (older)
				Key := 0; // Consume the key
				Exit;
			end;
			VK_DOWN: begin
				navigateHistory(1); // Go to next item (newer)
				Key := 0; // Consume the key
				Exit;
			end;
			VK_RETURN: begin
				// Save current text to history when Enter is pressed
				saveToHistory();
			end;
		end;
	end;
	
	inherited KeyDown(Key, Shift);
end;

procedure THistoryButtonedEdit.DoExit();
begin
	// Save current text to history when losing focus
	if not FNavigating then begin
		saveToHistory();
	end;
	inherited DoExit();
end;

procedure THistoryButtonedEdit.saveToHistory();
var
	currentText: string;
	existingIndex: Integer;
begin
	currentText := Trim(Text);
	
	// Don't save empty strings
	if currentText = '' then begin
		Exit;
	end;
	
	// Check if text already exists in history
	existingIndex := FHistory.IndexOf(currentText);
	if existingIndex >= 0 then begin
		// Move existing item to top of history
		FHistory.Delete(existingIndex);
	end;
	
	// Add to beginning of history (most recent first)
	FHistory.Insert(0, currentText);
	
	// Limit history size
	while FHistory.Count > FMaxHistoryCount do begin
		FHistory.Delete(FHistory.Count - 1);
	end;
	
	// Reset history index
	FHistoryIndex := -1;
end;

procedure THistoryButtonedEdit.navigateHistory(_direction: Integer);
var
	newIndex: Integer;
begin
	if FHistory.Count = 0 then begin
		Exit;
	end;
	
	FNavigating := True;
	try
		newIndex := FHistoryIndex + _direction;
		
		// Clamp index to valid range
		if newIndex < -1 then begin
			newIndex := -1;
		end else if newIndex >= FHistory.Count then begin
			newIndex := FHistory.Count - 1;
		end;
		
		FHistoryIndex := newIndex;
		
		// Update text based on history index
		if FHistoryIndex = -1 then begin
			Text := ''; // Current/new text
		end else begin
			Text := FHistory[FHistoryIndex];
		end;
		
		// Select all text for easy replacement
		SelectAll();
	finally
		FNavigating := False;
	end;
end;

function THistoryButtonedEdit.getCurrentHistoryText(): string;
begin
	if (FHistoryIndex >= 0) and (FHistoryIndex < FHistory.Count) then begin
		Result := FHistory[FHistoryIndex];
	end else begin
		Result := '';
	end;
end;

procedure THistoryButtonedEdit.AddToHistory(const _text: string);
var
	textToAdd: string;
begin
	textToAdd := Trim(_text);
	if textToAdd <> '' then begin
		// Remove if already exists
		var existingIndex := FHistory.IndexOf(textToAdd);
		if existingIndex >= 0 then begin
			FHistory.Delete(existingIndex);
		end;
		
		// Add to beginning
		FHistory.Insert(0, textToAdd);
		
		// Limit size
		while FHistory.Count > FMaxHistoryCount do begin
			FHistory.Delete(FHistory.Count - 1);
		end;
	end;
	FHistoryIndex := -1;
end;

procedure THistoryButtonedEdit.ClearHistory();
begin
	FHistory.Clear();
	FHistoryIndex := -1;
end;

function THistoryButtonedEdit.GetHistoryItems(): TArray<string>;
begin
	Result := FHistory.ToArray();
end;

procedure THistoryButtonedEdit.LoadFromHistory(const _items: TArray<string>);
var
	i: Integer;
begin
	FHistory.Clear();
	for i := 0 to High(_items) do begin
		if Trim(_items[i]) <> '' then begin
			FHistory.Add(_items[i]);
		end;
	end;
	FHistoryIndex := -1;
end;

procedure Register;
begin
	RegisterComponents('RipGrepper', [THistoryButtonedEdit]);
end;

end.
