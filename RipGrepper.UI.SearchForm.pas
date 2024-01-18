unit RipGrepper.UI.SearchForm;

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
	Vcl.ExtCtrls,
	System.ImageList,
	Vcl.ImgList,
	System.Actions,
	Vcl.ActnList,
	RipGrepper.Common.Settings;

type
	TRipGrepperSearchDialogForm = class(TForm)
		pnlSearch : TPanel;
		gbSearch : TGroupBox;
		lblParams : TLabel;
		lblPaths : TLabel;
		lblText : TLabel;
		cmbParameters : TComboBox;
		cmbSearchDir : TComboBox;
		cmbSearchText : TComboBox;
		btnConfig : TButton;
		btnSearch : TButton;
		btnCancel : TButton;
		ActionList1 : TActionList;
		ImageList1 : TImageList;
		ActionList : TActionList;
		ActionSearch : TAction;
		ActionCancel : TAction;
		ActionConfig : TAction;
		procedure ActionSearchExecute(Sender: TObject);
		procedure FormClose(Sender : TObject; var Action : TCloseAction);
		procedure FormShow(Sender : TObject);

		private
			FPSettings : PRipGrepperSettings;
			procedure LoadSettings;
			procedure StoreHistories;
			procedure StoreSearchSettings;

			{ Private-Deklarationen }
		public
			constructor Create(AOwner: TComponent; const _pSettings: PRipGrepperSettings); reintroduce; virtual;
			{ Public-Deklarationen }
	end;

var
	RipGrepperSearchDialogForm : TRipGrepperSearchDialogForm;

implementation

uses
	RipGrepper.Helper.UI;

{$R *.dfm}

constructor TRipGrepperSearchDialogForm.Create(AOwner: TComponent; const _pSettings: PRipGrepperSettings);
begin
	inherited Create(AOwner);
    FPSettings := _pSettings;
end;

procedure TRipGrepperSearchDialogForm.ActionSearchExecute(Sender: TObject);
begin
	StoreHistories();
    StoreSearchSettings();
    ModalREsult := mrOk;
end;

procedure TRipGrepperSearchDialogForm.FormClose(Sender : TObject; var Action : TCloseAction);
begin
	//
end;

procedure TRipGrepperSearchDialogForm.FormShow(Sender : TObject);
begin
	if not FPSettings^.IsLoaded then begin
		LoadSettings;
	end;
end;

procedure TRipGrepperSearchDialogForm.LoadSettings;
begin
	FPSettings^.Load;
	cmbSearchDir.Items.Assign(FPSettings^.SearchPathsHistory);
	cmbSearchDir.ItemIndex := 0;
	cmbSearchText.Items.Assign(FPSettings^.SearchTextsHistory);
	cmbSearchText.ItemIndex := 0;
	cmbParameters.Items.Assign(FPSettings^.RipGrepParamsHistory);
	cmbParameters.ItemIndex := 0;
end;

procedure TRipGrepperSearchDialogForm.StoreHistories;
begin
	TItemInserter.AddToCmbIfNotContains(cmbParameters);
	TItemInserter.AddToCmbIfNotContains(cmbSearchDir);
	TItemInserter.AddToCmbIfNotContains(cmbSearchText);
end;

procedure TRipGrepperSearchDialogForm.StoreSearchSettings;
begin
	FPSettings^.SearchPathsHistory.Assign(cmbSearchDir.Items);
	FPSettings^.SearchTextsHistory.Assign(cmbSearchText.Items);
	FPSettings^.RipGrepParamsHistory.Assign(cmbParameters.Items);
	FPSettings^.Store
end;

end.
