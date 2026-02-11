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
	ArrayEx,
	RipGrepper.Settings.Persistable,
	RipGrepper.Settings.SettingVariant,
	RipGrepper.UI.TabSeparatedConfigForm,
	System.SysUtils,
	Vcl.Dialogs,
	RipGrepper.Common.Constants;

type
	TTestPersistableArray = class(TPersistableSettings, IPersistableArray)
		private
			FArraySetting : IArraySetting;
			FArrValues: TArray<string>;
			function GetArraySetting() : IArraySetting;

		public
			constructor Create(const _arrValues: TArray<string>);
			procedure Init; override;
	end;

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
	persistableArray : TTestPersistableArray;
	testAction : TAction;
	i : Integer;
begin
	// prepare input strings
	FInputStrings.Text := memoInput.Text;
	FResultStrings.Clear;

	// create persistable array and fill with input data
	persistableArray := TTestPersistableArray.Create(FInputStrings.ToStringArray);
	try
		// parse headers
		var
			headerText : string := edtHeaders.Text;
		headers := headerText.Split([',']);

		// prepare test action if checkbox is checked
		if chkEnableTestAction.Checked then begin
			testAction := ActionTest;
		end else begin
			testAction := nil;
		end;

		// create and show form
		form := TTabSeparatedConfigForm.Create(Self, persistableArray, 'dark', testAction);
		try
			form.LoadColumnHeaders(headers);

			if form.ShowModal = mrOk then begin
				// display results
				FResultStrings.AddStrings(form.ResultStrings.Items);
				memoResults.Lines.Clear;
				memoResults.Lines.Add('=== RESULTS ===');
				memoResults.Lines.Add('Total rows: ' + FResultStrings.Count.ToString);
				memoResults.Lines.Add('');

				for i := 0 to FResultStrings.Count - 1 do begin
					memoResults.Lines.Add(Format('Row %d: %s', [i + 1, FResultStrings[i]]));
				end;

				// update input memo with results
				memoInput.Lines.Text := FResultStrings.Text;
			end else begin
				memoResults.Lines.Clear;
				memoResults.Lines.Add('=== CANCELLED ===');
			end;
		finally
			form.Free;
		end;
	finally
		persistableArray.Free;
	end;
end;

procedure TTabSeparatedConfigTestMainForm.ActionTestExecute(Sender : TObject);
begin
	ShowMessage('Test Action executed!' + sLineBreak + 'This demonstrates the optional test functionality.');
end;

{ TTestPersistableArray }

constructor TTestPersistableArray.Create(const _arrValues: TArray<string>);
begin
	IniSectionName := 'TestData';
  FArrValues := _arrValues;
	inherited Create(nil);
end;

function TTestPersistableArray.GetArraySetting() : IArraySetting;
begin
	Result := FArraySetting;
end;

procedure TTestPersistableArray.Init;
begin
	FArraySetting := TArraySetting.Create('TestData', FArrValues );
	CreateSetting(FArraySetting.Name, ITEM_KEY_PREFIX, FArraySetting);
end;

end.
