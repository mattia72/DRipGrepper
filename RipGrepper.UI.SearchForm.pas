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
	Vcl.StdCtrls,
	Vcl.ExtCtrls,
	System.ImageList,
	Vcl.ImgList,
	System.Actions,
	Vcl.ActnList,
	RipGrepper.Common.Settings,
	Vcl.StdActns;

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
		btnSearchFolder : TButton;
		ActionSearchFolder : TAction;
		btnSearchFile : TButton;
		FileOpen1 : TFileOpen;
		procedure ActionSearchFolderExecute(Sender : TObject);
		procedure ActionConfigExecute(Sender : TObject);
		procedure ActionSearchExecute(Sender : TObject);
		procedure FileOpen1Accept(Sender : TObject);
		procedure FormClose(Sender : TObject; var Action : TCloseAction);
		procedure FormShow(Sender : TObject);

		private
			FSettings : TRipGrepperSettingsHistory;
			procedure LoadSettings;
			procedure StoreHistories;
			procedure StoreSearchSettings;

			{ Private-Deklarationen }
		public
			constructor Create(AOwner : TComponent; const _settings : TRipGrepperSettingsHistory); reintroduce; virtual;
			{ Public-Deklarationen }
	end;

var
	RipGrepperSearchDialogForm : TRipGrepperSearchDialogForm;

implementation

uses
	RipGrepper.Helper.UI,
	RipGrepper.Tools.ProcessUtils,
	Vcl.Dialogs,
	System.UITypes;

{$R *.dfm}

constructor TRipGrepperSearchDialogForm.Create(AOwner : TComponent; const _settings : TRipGrepperSettingsHistory);
begin
	inherited Create(AOwner);
	FSettings := _settings;
end;

procedure TRipGrepperSearchDialogForm.ActionSearchFolderExecute(Sender : TObject);
var
	selectedFiles : string;
	dlg : TFileOpenDialog;
begin
	dlg := TFileOpenDialog.Create(nil);
	try
		dlg.DefaultFolder := 'C:\';
		// dlg. := 'All files (*.*)|*.*';
		dlg.Options := dlg.Options + [fdoAllowMultiSelect, fdoPickfolders];
		if dlg.Execute(Handle) then begin
			selectedFiles := dlg.Files.DelimitedText;
		end;
	finally
		dlg.Free;
	end;

	if selectedFiles <> '' then begin
		cmbSearchDir.Text := selectedFiles;
	end;

end;

procedure TRipGrepperSearchDialogForm.ActionConfigExecute(Sender : TObject);
var
	sl : TStrings;
begin
	sl := TStringList.Create();
	sl.Add('-help');
	try
		TSimpleProcessOutputStringReader.RunProcess(FSettings.RipGrepParameters.RipGrepPath, sl, '.', sl);
		MessageDlg(sl.Text, TMsgDlgType.mtInformation, [mbOk], 0);
	finally
		sl.Free;
	end;
end;

procedure TRipGrepperSearchDialogForm.ActionSearchExecute(Sender : TObject);
begin
	StoreHistories();
	StoreSearchSettings();
	ModalResult := mrOk;
end;

procedure TRipGrepperSearchDialogForm.FileOpen1Accept(Sender : TObject);
begin
	cmbSearchDir.Text := FileOpen1.Dialog.Files.DelimitedText;
end;

procedure TRipGrepperSearchDialogForm.FormClose(Sender : TObject; var Action : TCloseAction);
begin
	//
end;

procedure TRipGrepperSearchDialogForm.FormShow(Sender : TObject);
begin
	LoadSettings;
end;

procedure TRipGrepperSearchDialogForm.LoadSettings;
begin
	FSettings.Load;
	cmbSearchDir.Items.Assign(FSettings.SearchPathsHistory);
	cmbSearchDir.ItemIndex := 0;
	cmbSearchText.Items.Assign(FSettings.SearchTextsHistory);
	cmbSearchText.ItemIndex := 0;
	cmbParameters.Items.Assign(FSettings.RipGrepParamsHistory);
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
	FSettings.SearchPathsHistory.Assign(cmbSearchDir.Items);
	FSettings.RipGrepParameters.SearchPath := cmbSearchDir.Text;

	FSettings.SearchTextsHistory.Assign(cmbSearchText.Items);
	FSettings.RipGrepParameters.SearchText := cmbSearchText.Text;

	FSettings.RipGrepParamsHistory.Assign(cmbParameters.Items);
	FSettings.RipGrepParameters.RipGrepParam := cmbParameters.Text;
	FSettings.ReBuildArguments;
	FSettings.Store
end;

end.
