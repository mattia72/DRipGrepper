unit HistoryEditTest;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	Vcl.StdCtrls,
	RipGrepper.UI.HistoryButtonedEdit;

type
	THistoryEditTestForm = class(TForm)
		Button1: TButton;
		Button2: TButton;
		Label1: TLabel;
		Memo1: TMemo;
		procedure FormCreate(Sender: TObject);
		procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
		private
			FHistoryEdit: THistoryButtonedEdit;
		public
	end;

var
	HistoryEditTestForm: THistoryEditTestForm;

implementation

{$R *.dfm}

procedure THistoryEditTestForm.FormCreate(Sender: TObject);
begin
	// Create THistoryButtonedEdit programmatically
	FHistoryEdit := THistoryButtonedEdit.Create(Self);
	FHistoryEdit.Parent := Self;
	FHistoryEdit.Left := 20;
	FHistoryEdit.Top := 20;
	FHistoryEdit.Width := 300;
	FHistoryEdit.Height := 25;
	FHistoryEdit.Hint := 'Type text and press ENTER to add to history. Use UP/DOWN arrows to navigate.';
	FHistoryEdit.ShowHint := True;
	
	// Add some initial history
	FHistoryEdit.AddToHistory('Test entry 1');
	FHistoryEdit.AddToHistory('Test entry 2');
	FHistoryEdit.AddToHistory('Test entry 3');
	
	// Configure buttons
	Button1.Caption := 'Show History';
	Button2.Caption := 'Clear History';
	
	Label1.Caption := 'THistoryButtonedEdit Test:';
	Label1.Top := FHistoryEdit.Top - 20;
	
	Memo1.Lines.Add('Instructions:');
	Memo1.Lines.Add('1. Type text in the edit control');
	Memo1.Lines.Add('2. Press ENTER to add to history');
	Memo1.Lines.Add('3. Use UP/DOWN arrows to navigate history');
	Memo1.Lines.Add('4. Click "Show History" to see current items');
	Memo1.Lines.Add('5. Click "Clear History" to reset');
end;

procedure THistoryEditTestForm.Button1Click(Sender: TObject);
var
	items: TArray<string>;
	i: Integer;
begin
	// Show current history
	items := FHistoryEdit.GetHistoryItems();
	Memo1.Lines.Clear();
	Memo1.Lines.Add(Format('History Count: %d', [Length(items)]));
	Memo1.Lines.Add('History Items:');
	
	for i := 0 to High(items) do begin
		Memo1.Lines.Add(Format('%d: %s', [i + 1, items[i]]));
	end;
end;

procedure THistoryEditTestForm.Button2Click(Sender: TObject);
begin
	// Clear history
	FHistoryEdit.ClearHistory();
	FHistoryEdit.Text := '';
	Memo1.Lines.Clear();
	Memo1.Lines.Add('History cleared!');
end;

end.
