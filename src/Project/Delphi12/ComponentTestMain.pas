unit ComponentTestMain;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Variants,
	System.Classes,
	Vcl.Controls,
	Vcl.Dialogs,
	Vcl.ExtCtrls,
	Vcl.Forms,
	Vcl.Graphics,
	Vcl.Samples.Spin,
	Vcl.StdCtrls,
	RipGrepper.UI.Components.HistoryButtonedEdit,
	RipGrepper.UI.Components.NotifyingControls,
	RipGrepper.UI.Components.OptionControls, Vcl.Mask,
  RipGrepper.UI.Components.LabeledComboBox;

type
	TFormComponentTest = class(TForm, INotifyingControlOwner)
		pnlMain : TPanel;
		pnlHistoryTest : TPanel;
		lblHistoryTest : TLabel;
		btnAddToHistory : TButton;
		btnClearHistory : TButton;
		btnShowHistory : TButton;
		memoLog : TMemo;
		pnlNotifyingControls : TPanel;
		lblNotifyingTest : TLabel;
		lblSpinEdit : TLabel;
		lblComboBox : TLabel;
		lblCheckBox : TLabel;
		pnlOptionControls : TPanel;
		lblOptionControls : TLabel;
		btnTestRuntimeCreation : TButton;
		pnlRuntimeControls : TPanel;
    ntfyngspndt1: TNotifyingSpinEdit;
    optnchckbxspn1: TOptionCheckBoxSpin;
    optnchckbxcmb1: TOptionCheckBoxCombo;
    lbledt1: TLabeledEdit;
    lbldcmbx1: TLabeledComboBox;
		procedure FormCreate(Sender : TObject);
		procedure FormDestroy(Sender : TObject);
		procedure btnAddToHistoryClick(Sender : TObject);
		procedure btnClearHistoryClick(Sender : TObject);
		procedure btnShowHistoryClick(Sender : TObject);
		procedure btnTestRuntimeCreationClick(Sender : TObject);

		private
			FHistoryEdit : THistoryButtonedEdit;
			FNotifyingSpinEdit : TNotifyingSpinEdit;
			FNotifyingComboBox : TNotifyingComboBox;
			FNotifyingCheckBox : TNotifyingCheckBox;
			FOptionCheckBox : TOptionCheckBox;
			FRuntimeCheckBox : TOptionCheckBox;

			procedure log(const _msg : string);
			procedure setupHistoryEdit();
			procedure setupNotifyingControls();
			procedure setupOptionControls();
			procedure onNotifyingCheckBoxClick(Sender : TObject);
			procedure onOptionCheckBoxValueChanged(Sender : TObject);
			procedure onRuntimeCheckBoxValueChanged(Sender : TObject);

		public
			// INotifyingControlOwner implementation
			procedure OnSubItemEnabledChanged(_sender : TObject);
	end;

var
	FormComponentTest : TFormComponentTest;

implementation

{$R *.dfm}

procedure TFormComponentTest.FormCreate(Sender : TObject);
begin
	log('Component Test Application Started');
	setupHistoryEdit();
	setupNotifyingControls();
	setupOptionControls();
end;

procedure TFormComponentTest.FormDestroy(Sender : TObject);
begin
	// Components created with Owner will be freed automatically
	log('Application closing...');
end;

procedure TFormComponentTest.setupHistoryEdit();
begin
	// Create THistoryButtonedEdit at runtime
	FHistoryEdit := THistoryButtonedEdit.Create(Self);
	FHistoryEdit.Parent := pnlHistoryTest;
	FHistoryEdit.Left := 16;
	FHistoryEdit.Top := 40;
	FHistoryEdit.Width := 300;
	FHistoryEdit.Height := 21;
	FHistoryEdit.MaxHistoryCount := 10;
	FHistoryEdit.TextHint := 'Enter text and press Enter or Up/Down arrows...';

	// Add some initial history
	FHistoryEdit.AddToHistory('First entry');
	FHistoryEdit.AddToHistory('Second entry');
	FHistoryEdit.AddToHistory('Third entry');

	log('HistoryButtonedEdit created with ' + IntToStr(FHistoryEdit.HistoryCount) + ' history items');
end;

procedure TFormComponentTest.setupNotifyingControls();
begin
	// Create TNotifyingSpinEdit
	FNotifyingSpinEdit := TNotifyingSpinEdit.Create(Self);
	FNotifyingSpinEdit.Parent := pnlNotifyingControls;
	FNotifyingSpinEdit.Left := lblSpinEdit.Left + lblSpinEdit.Width + 8;
	FNotifyingSpinEdit.Top := lblSpinEdit.Top - 3;
	FNotifyingSpinEdit.Width := 121;
	FNotifyingSpinEdit.Height := 22;
	FNotifyingSpinEdit.MinValue := 0;
	FNotifyingSpinEdit.MaxValue := 100;
	FNotifyingSpinEdit.Value := 50;
	FNotifyingSpinEdit.OwnerItem := Self;

	// Create TNotifyingComboBox
	FNotifyingComboBox := TNotifyingComboBox.Create(Self);
	FNotifyingComboBox.Parent := pnlNotifyingControls;
	FNotifyingComboBox.Left := lblComboBox.Left + lblComboBox.Width + 8;
	FNotifyingComboBox.Top := lblComboBox.Top - 4;
	FNotifyingComboBox.Width := 145;
	FNotifyingComboBox.Height := 21;
	FNotifyingComboBox.Style := csDropDownList;
	FNotifyingComboBox.Items.Add('Option 1');
	FNotifyingComboBox.Items.Add('Option 2');
	FNotifyingComboBox.Items.Add('Option 3');
	FNotifyingComboBox.ItemIndex := 0;
	FNotifyingComboBox.OwnerItem := Self;

	// Create TNotifyingCheckBox
	FNotifyingCheckBox := TNotifyingCheckBox.Create(Self);
	FNotifyingCheckBox.Parent := pnlNotifyingControls;
	FNotifyingCheckBox.Left := lblCheckBox.Left + lblCheckBox.Width + 8;
	FNotifyingCheckBox.Top := lblCheckBox.Top - 2;
	FNotifyingCheckBox.Width := 97;
	FNotifyingCheckBox.Height := 17;
	FNotifyingCheckBox.Caption := 'Enable Controls';
	FNotifyingCheckBox.Checked := True;
	FNotifyingCheckBox.OwnerItem := Self;
	FNotifyingCheckBox.OnClick := onNotifyingCheckBoxClick;

	log('Notifying controls created');
end;

procedure TFormComponentTest.setupOptionControls();
begin
	// Create TOptionCheckBox
	FOptionCheckBox := TOptionCheckBox.Create(Self);
	FOptionCheckBox.Parent := pnlOptionControls;
	FOptionCheckBox.Left := 16;
	FOptionCheckBox.Top := 40;
	FOptionCheckBox.Width := 350;
	FOptionCheckBox.Height := 50;
	FOptionCheckBox.Caption := 'Enable Feature';
	FOptionCheckBox.AlignWithMargins := True;
	FOptionCheckBox.Hint := 'This is a test option checkbox';
	FOptionCheckBox.ShowHint := True;
	FOptionCheckBox.OnValueChanged := onOptionCheckBoxValueChanged;

	log('Option controls created');
end;

procedure TFormComponentTest.btnAddToHistoryClick(Sender : TObject);
var
	text : string;
begin
	text := FHistoryEdit.Text;
	if text <> '' then begin
		FHistoryEdit.AddToHistory(text);
		log('Added to history: "' + text + '" (Count: ' + IntToStr(FHistoryEdit.HistoryCount) + ')');
		FHistoryEdit.Clear;
	end
	else begin
		ShowMessage('Please enter some text first');
	end;
end;

procedure TFormComponentTest.btnClearHistoryClick(Sender : TObject);
begin
	FHistoryEdit.ClearHistory();
	FHistoryEdit.Clear;
	log('History cleared');
end;

procedure TFormComponentTest.btnShowHistoryClick(Sender : TObject);
var
	items : TArray<string>;
	historyText : string;
begin
	items := FHistoryEdit.GetHistoryItems();
	historyText := '';

	for var i : Integer := 0 to Length(items) - 1 do begin
		historyText := historyText + IntToStr(i + 1) + '. ' + items[i] + sLineBreak;
	end;

	if historyText = '' then begin
		historyText := 'No history items';
	end;

	ShowMessage(historyText);
end;

procedure TFormComponentTest.btnTestRuntimeCreationClick(Sender : TObject);
begin
	// Test creating controls at runtime
	if not Assigned(FRuntimeCheckBox) then begin
		FRuntimeCheckBox := TOptionCheckBox.Create(Self);
		FRuntimeCheckBox.Parent := pnlRuntimeControls;
		FRuntimeCheckBox.Left := 220;
		FRuntimeCheckBox.Top := 16;
		FRuntimeCheckBox.Width := 350;
		FRuntimeCheckBox.Height := 50;
		FRuntimeCheckBox.Caption := 'Runtime Created Option';
		FRuntimeCheckBox.AlignWithMargins := True;
		FRuntimeCheckBox.OnValueChanged := onRuntimeCheckBoxValueChanged;

		log('Runtime option checkbox created');
	end
	else begin
		log('Runtime checkbox already exists');
	end;
end;

procedure TFormComponentTest.onNotifyingCheckBoxClick(Sender : TObject);
begin
	FNotifyingSpinEdit.Enabled := FNotifyingCheckBox.Checked;
	FNotifyingComboBox.Enabled := FNotifyingCheckBox.Checked;
end;

procedure TFormComponentTest.onOptionCheckBoxValueChanged(Sender : TObject);
begin
	log('Option CheckBox changed: ' + VarToStr(FOptionCheckBox.Value));
end;

procedure TFormComponentTest.onRuntimeCheckBoxValueChanged(Sender : TObject);
begin
	log('Runtime CheckBox changed: ' + VarToStr(FRuntimeCheckBox.Value));
end;

procedure TFormComponentTest.OnSubItemEnabledChanged(_sender : TObject);
var
	senderName : string;
begin
	if _sender is TControl then begin
		senderName := TControl(_sender).ClassName;
	end
	else begin
		senderName := 'Unknown';
	end;

	log('OnSubItemEnabledChanged called from: ' + senderName);
end;

procedure TFormComponentTest.log(const _msg : string);
begin
	memoLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + _msg);
end;

end.
