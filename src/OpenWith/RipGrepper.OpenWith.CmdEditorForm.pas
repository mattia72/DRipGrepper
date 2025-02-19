unit RipGrepper.OpenWith.CmdEditorForm;

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
	Vcl.ExtCtrls,
	System.ImageList,
	Vcl.ImgList,
	SVGIconImageListBase,
	SVGIconImageList,
	Vcl.StdCtrls,
	System.Actions,
	Vcl.ActnList;

type

	TCommandItem = record
		Caption : string;
		CommandLine : string;
		Description : string;
	end;

	TOpenWithCommandEditor = class(TForm)
		SVGIconImageList1 : TSVGIconImageList;
		Panel1 : TPanel;
		Label1 : TLabel;
		edtCmdLine : TEdit;
		btnOpenFile : TButton;
		btn_Save : TButton;
		btn_Cancel : TButton;
		Label2 : TLabel;
		edtLabel : TEdit;
		ActionList1 : TActionList;
		ListBox1 : TListBox;
		Label3 : TLabel;
		ActionOk : TAction;
		ActionCancel : TAction;
		ActionOpenFileDialog : TAction;
		Label4 : TLabel;
		edtDescr : TEdit;
		procedure ActionCancelExecute(Sender : TObject);
		procedure ActionOkExecute(Sender : TObject);
		procedure FormShow(Sender: TObject);

		private
			FCommandItem : TCommandItem;

		public
			class function CreateAndShow(_Owner : TComponent; const ci : TCommandItem) : TCommandItem;
			property CommandItem : TCommandItem read FCommandItem write FCommandItem;

	end;

var
	OpenWithCommandEditor : TOpenWithCommandEditor;

implementation

{$R *.dfm}

procedure TOpenWithCommandEditor.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
end;

procedure TOpenWithCommandEditor.ActionOkExecute(Sender : TObject);
begin
	FCommandItem.Caption := edtLabel.Text;
	FCommandItem.CommandLine := edtLabel.Text;
	FCommandItem.Description := edtDescr.Text;
	ModalResult := mrOk;
end;

class function TOpenWithCommandEditor.CreateAndShow(_Owner : TComponent; const ci : TCommandItem) : TCommandItem;
begin
	Result := default (TCommandItem);
	var
	form := TOpenWithCommandEditor.Create(_Owner);
	try
		form.CommandItem := ci;
		if mrOk = form.ShowModal() then begin
			Result := form.CommandItem;
		end;
	finally
		form.Free;
	end;
end;

procedure TOpenWithCommandEditor.FormShow(Sender: TObject);
begin
	edtLabel.Text := FCommandItem.Caption;
	edtLabel.Text := FCommandItem.CommandLine;
	edtDescr.Text := FCommandItem.Description;

end;

end.
