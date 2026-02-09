unit TabSeparatedConfigTestForm;

interface

uses
	System.Actions,
	System.Classes,
	Vcl.ActnList,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.StdCtrls;

type
	TTabSeparatedConfigTestMainForm = class(TForm)
		btnOpenForm : TButton;
		btnTestAction : TButton;
		memoResults : TMemo;
		Label1 : TLabel;
		Label2 : TLabel;
		memoInput : TMemo;
		chkEnableTestAction : TCheckBox;
		ActionList1 : TActionList;
		ActionTest : TAction;
		Label3 : TLabel;
		edtHeaders : TEdit;
		Label4 : TLabel;
		procedure ActionTestExecute(Sender : TObject);
		procedure btnOpenFormClick(Sender : TObject);
		procedure FormCreate(Sender : TObject);

		private
			FInputStrings : TStringList;
			FResultStrings : TStringList;

		public
			destructor Destroy; override;
	end;

var
	TabSeparatedConfigTestMainForm : TTabSeparatedConfigTestMainForm;

implementation

uses
	System.SysUtils,
	RipGrepper.Settings.Persistable,
	RipGrepper.UI.TabSeparatedConfigForm;

{$R *.dfm}

procedure TTabSeparatedConfigTestMainForm.FormCreate(Sender : TObject);
begin
	FInputStrings := TStringList.Create;
	FResultStrings := TStringList.Create;

	// add some sample data (tab-separated)
	memoInput.Lines.Add('John Doe'#9'john@example.com'#9'Developer');
	memoInput.Lines.Add('Jane Smith'#9'jane@example.com'#9'Designer');
	memoInput.Lines.Add('Bob Johnson'#9'bob@example.com'#9'Manager');

	// default headers
	edtHeaders.Text := 'Name,Email,Position';

	memoResults.Lines.Clear;
end;

destructor TTabSeparatedConfigTestMainForm.Destroy;
begin
	FInputStrings.Free;
	FResultStrings.Free;
	inherited;
end;

procedure TTabSeparatedConfigTestMainForm.btnOpenFormClick(Sender : TObject);
var
	form : TTabSeparatedConfigForm;
	headers : TArray<string>;
	testAction : TAction;
	i : Integer;
begin
	// prepare input strings
	FInputStrings.Text := memoInput.Text;
	FResultStrings.Clear;

	// parse headers
	headers := edtHeaders.Text.Split([',']);

	// prepare test action if checkbox is checked
	if chkEnableTestAction.Checked then begin
		testAction := ActionTest;
	end else begin
		testAction := nil;
	end;

	// create and show form
	form := TTabSeparatedConfigForm.Create(Self, nil, 'dark', FInputStrings, testAction);
	try
		form.LoadColumnHeaders(headers);

		if form.ShowModal = mrOk then begin
			// display results
			memoResults.Lines.Clear;
			memoResults.Lines.Add('=== RESULTS ===');
			memoResults.Lines.Add('Total rows: ' + FInputStrings.Count.ToString);
			memoResults.Lines.Add('');

			for i := 0 to FInputStrings.Count - 1 do begin
				memoResults.Lines.Add(Format('Row %d: %s', [i + 1, FInputStrings[i]]));
			end;

			// update input memo with results
			memoInput.Lines.Text := FInputStrings.Text;
		end else begin
			memoResults.Lines.Clear;
			memoResults.Lines.Add('=== CANCELLED ===');
		end;
	finally
		form.Free;
	end;
end;

procedure TTabSeparatedConfigTestMainForm.ActionTestExecute(Sender : TObject);
begin
	ShowMessage('Test Action executed!' + sLineBreak + 'This demonstrates the optional test functionality.');
end;

end.
