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
	TSearchDialogForm = class(TForm)
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
		procedure FormClose(Sender : TObject; var Action : TCloseAction);
		procedure FormShow(Sender : TObject);

		private
			FArguments : TStringList;
			FSettings : TRipGrepperSettings;
			procedure LoadSettings;

			{ Private-Deklarationen }
		public
			constructor Create(AOwner : TComponent; _settings : TRipGrepperSettings); overload; virtual;
			{ Public-Deklarationen }
	end;

var
	SearchDialogForm : TSearchDialogForm;

implementation

{$R *.dfm}

constructor TSearchDialogForm.Create(AOwner : TComponent; _settings : TRipGrepperSettings);
begin
	inherited Create(AOwner);
end;

procedure TSearchDialogForm.FormClose(Sender : TObject; var Action : TCloseAction);
begin
	//
end;

procedure TSearchDialogForm.FormShow(Sender : TObject);
begin
	if not FSettings.IsLoaded then begin
		LoadSettings;
	end;
end;

procedure TSearchDialogForm.LoadSettings;
begin
	FSettings.Load;
	cmbSearchDir.Items.Assign(FSettings.SearchPaths);
	cmbSearchDir.ItemIndex := 0;
	cmbSearchText.Items.Assign(FSettings.SearchTexts);
	cmbSearchText.ItemIndex := 0;
	cmbParameters.Items.Assign(FSettings.RipGrepParams);
	cmbParameters.ItemIndex := 0;
end;

end.
