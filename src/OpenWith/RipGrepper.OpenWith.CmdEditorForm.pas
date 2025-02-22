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
	Vcl.ActnList,
	RipGrepper.Tools.FileUtils,
	RipGrepper.Helper.UI.DarkMode;

type

	TOpenWithCommandEditor = class(TForm)
		SVGIconImageList1 : TSVGIconImageList;
		Panel1 : TPanel;
		Label1 : TLabel;
		edtCmdPath : TEdit;
		btnOpenFile : TButton;
		btn_Save : TButton;
		btn_Cancel : TButton;
		Label2 : TLabel;
		edtLabel : TEdit;
		ActionList1 : TActionList;
		ListBox1 : TListBox;
		ActionOk : TAction;
		ActionCancel : TAction;
		ActionOpenFileDialog : TAction;
		Label4 : TLabel;
		edtDescr : TEdit;
		OpenDialog1 : TOpenDialog;
		GroupBox1 : TGroupBox;
		GroupBox2 : TGroupBox;
		Label3 : TLabel;
		edtParameters : TEdit;
		procedure ActionCancelExecute(Sender : TObject);
		procedure ActionOkExecute(Sender : TObject);
		procedure ActionOpenFileDialogExecute(Sender : TObject);
		procedure FormShow(Sender : TObject);

		private
			FCommandItem : TCommandItem;
			FThemeHandler : TThemeHandler;
			function GetThemeHandler() : TThemeHandler;
			property ThemeHandler : TThemeHandler read GetThemeHandler;

		public
			constructor Create(AOwner : TComponent; const _themeName : string); reintroduce;
			class function CheckCommand(const _sCmd : string) : Boolean;
			class function CreateAndShow(_Owner : TComponent; const ci : TCommandItem;
				const _themeName : string): TCommandItem;
			property CommandItem : TCommandItem read FCommandItem write FCommandItem;

	end;

var
	OpenWithCommandEditor : TOpenWithCommandEditor;

implementation

uses
	RipGrepper.Helper.UI,
	RipGrepper.Tools.DebugUtils;

{$R *.dfm}

constructor TOpenWithCommandEditor.Create(AOwner : TComponent; const _themeName : string);
begin
	inherited Create(AOwner);
	ThemeHandler.Init(_themeName);
end;

procedure TOpenWithCommandEditor.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
end;

procedure TOpenWithCommandEditor.ActionOkExecute(Sender : TObject);
begin
	FCommandItem.Caption := edtLabel.Text;
	FCommandItem.CommandLine := TCommandLineRec.ParseCommand(edtCmdPath.Text + ' ' + edtParameters.Text);
	FCommandItem.Description := edtDescr.Text;
	if FCommandItem.Caption.IsEmpty then begin
		TMsgBox.ShowError('Caption shouldn''t be empty!');
		Exit;
	end;
	FCommandItem.IsActive := CheckCommand(FCommandItem.CommandLine.AsString());
	ModalResult := mrOk;
end;

procedure TOpenWithCommandEditor.ActionOpenFileDialogExecute(Sender : TObject);
begin
	inherited;
	OpenDialog1.Filter := 'Executable files (*.exe)|*.exe';
	if OpenDialog1.Execute(self.Handle) then begin
		edtCmdPath.Text := OpenDialog1.FileName;
	end;
end;

class function TOpenWithCommandEditor.CheckCommand(const _sCmd : string) : Boolean;
var
	bFound : Boolean;
	sFileName : string;
	sPath : string;
begin
	Result := False;
	if _sCmd <> '' then begin
		sFileName := TCommandLineRec.ParseCommand(_sCmd).ExePath;
		if sFileName.IsEmpty then begin
			sFileName := _sCmd;
		end;
		TDebugUtils.DebugMessage(Format('TOpenWithCommandEditor.CheckCommand Exe: %s ', [sFileName]));
		if not FileExists(sFileName) then begin
			bFound := False;
			sPath := ExtractFileDir(sFileName);
			TFileUtils.FindExecutable(sFileName, sPath);
			if (not sPath.IsEmpty) then begin
				bFound := True;
			end;
			if not bFound then begin
				TMsgBox.ShowError(Format('Executable "%s" not found!', [sFileName]));
				Exit;
			end;

		end;
		Result := True;
	end;
end;

class function TOpenWithCommandEditor.CreateAndShow(_Owner : TComponent; const ci : TCommandItem; const _themeName : string) : TCommandItem;
begin
	Result := default (TCommandItem);
	var
	form := TOpenWithCommandEditor.Create(_Owner, _themeName);
	try
		form.CommandItem := ci;
		if mrOk = form.ShowModal() then begin
			Result := form.CommandItem;
		end else begin
			Result := ci;
		end;
	finally
		form.Free;
	end;
end;

procedure TOpenWithCommandEditor.FormShow(Sender : TObject);
begin
	edtLabel.Text := FCommandItem.Caption;
	edtCmdPath.Text := FCommandItem.CommandLine.ExePath;
	edtParameters.Text := FCommandItem.CommandLine.ParametersAsString;
	edtDescr.Text := FCommandItem.Description;
end;

function TOpenWithCommandEditor.GetThemeHandler() : TThemeHandler;
begin
	if not Assigned(FThemeHandler) then begin
		FThemeHandler := TThemeHandler.Create(self);
	end;
	Result := FThemeHandler;
end;

end.
