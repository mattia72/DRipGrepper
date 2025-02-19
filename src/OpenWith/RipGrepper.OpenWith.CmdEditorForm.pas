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

		private
			{ Private-Deklarationen }
		public
			{ Public-Deklarationen }
	end;

var
	OpenWithCommandEditor : TOpenWithCommandEditor;

implementation

{$R *.dfm}

end.
