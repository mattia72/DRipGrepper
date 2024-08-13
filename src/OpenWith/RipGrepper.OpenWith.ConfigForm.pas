unit RipGrepper.OpenWith.ConfigForm;

interface

uses
	System.Classes,
	System.Actions,
	Vcl.StdCtrls,
	Vcl.Controls,
	Vcl.ExtCtrls,
	Vcl.ActnList,
	Vcl.CheckLst,
	Vcl.ImgList,
	Vcl.Dialogs,
	Vcl.Forms,
	System.ImageList,
	RipGrepper.Common.Settings.Misc,
	RipGrepper.UI.DpiScaler,
	RipGrepper.Common.Settings.RipGrepperOpenWithSettings;

type

	TOpenWithConfigForm = class(TForm)

		var
			ActionListConfig : TActionList;
			pnlBottom : TPanel;
			btnMoveUp : TButton;
			btnMoveDown : TButton;
			ActionMoveUp : TAction;
			ActionMoveDown : TAction;
			btnTest : TButton;
			ActionTest : TAction;
			edt_OpenWithCmd : TEdit;
			OpenDialog1 : TOpenDialog;
			Button1 : TButton;
			ActionOpenFileDlg : TAction;
			Label1 : TLabel;
			lbCommands : TCheckListBox;
			btnAdd : TButton;
			btnRemove : TButton;
			btnModify : TButton;
			ActionModify : TAction;
			ActionAdd : TAction;
			ActionRemove : TAction;
			ImageList1 : TImageList;
			pnlMain : TPanel;
			ActionOk : TAction;
			ActionCancel : TAction;
			procedure FormCreate(Sender : TObject);
			procedure ActionAddExecute(Sender : TObject);
			procedure ActionAddUpdate(Sender : TObject);
			procedure ActionCancelExecute(Sender : TObject);
			procedure ActionModifyExecute(Sender : TObject);
			procedure ActionModifyUpdate(Sender : TObject);
			procedure ActionMoveDownExecute(Sender : TObject);
			procedure ActionMoveDownUpdate(Sender : TObject);
			procedure ActionMoveUpExecute(Sender : TObject);
			procedure ActionMoveUpUpdate(Sender : TObject);
			procedure ActionOkExecute(Sender : TObject);
			procedure ActionRemoveExecute(Sender : TObject);
			procedure ActionRemoveUpdate(Sender : TObject);
			procedure ActionTestExecute(Sender : TObject);
			procedure ActionTestUpdate(Sender : TObject);
			procedure ActionOpenFileDlgExecute(Sender : TObject);
			procedure edt_OpenWithCmdKeyPress(Sender : TObject; var Key : Char);
			procedure lbCommandsClick(Sender : TObject);
			procedure lbCommandsDblClick(Sender : TObject);

		private
			FDpiScaler : TRipGrepperDpiScaler;
			FSettings : TRipGrepperOpenWithSettings;
			function CheckCommand(const _sCmd : string) : Boolean;
			procedure ClearOpenWithCmd;
			procedure MoveItem(const idx : Integer);
			procedure PutSelectedToEdit;

		protected
		public
			{ Public-Deklarationen }
			constructor Create(AOwner : TComponent; const ASettings : TRipGrepperOpenWithSettings); reintroduce;
			destructor Destroy; override;
			class procedure CreateAndShow(_settings : TRipGrepperOpenWithSettings);
			procedure ReadSettings;
			procedure WriteSettings;

	end;

implementation

uses
	System.SysUtils,
	Vcl.Clipbrd,
	Winapi.Windows,
	Winapi.ShellAPI,
	RipGrepper.OpenWith.Constants,
	RipGrepper.OpenWith.Runner,
	RipGrepper.OpenWith.Params,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Helper.UI,
	System.RegularExpressions,
	RipGrepper.Tools.FileUtils;

{$R *.dfm}

constructor TOpenWithConfigForm.Create(AOwner : TComponent; const ASettings : TRipGrepperOpenWithSettings);
begin
	inherited Create(AOwner); // , ImageList1);
	self.FSettings := ASettings;
	FDpiScaler := TRipGrepperDpiScaler.Create(self);
end;

destructor TOpenWithConfigForm.Destroy;
begin
	FDpiScaler.Free;
	inherited;
end;

procedure TOpenWithConfigForm.FormCreate(Sender : TObject);
begin
	ReadSettings;
end;

procedure TOpenWithConfigForm.ActionAddExecute(Sender : TObject);
begin
	inherited;
	lbCommands.Items.Add(edt_OpenWithCmd.Text);
	ClearOpenWithCmd;
end;

procedure TOpenWithConfigForm.ActionAddUpdate(Sender : TObject);
begin
	inherited;
	btnAdd.Enabled := edt_OpenWithCmd.Text <> '';
end;

procedure TOpenWithConfigForm.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
end;

procedure TOpenWithConfigForm.ActionModifyExecute(Sender : TObject);
begin
	inherited;
	lbCommands.Items[lbCommands.ItemIndex] := edt_OpenWithCmd.Text;
	ClearOpenWithCmd;
end;

procedure TOpenWithConfigForm.ActionModifyUpdate(Sender : TObject);
begin
	inherited;
	btnModify.Enabled := (lbCommands.SelCount = 1) and (edt_OpenWithCmd.Text <> '');
end;

procedure TOpenWithConfigForm.ActionMoveDownExecute(Sender : TObject);
begin
	inherited;
	var
	idx := lbCommands.ItemIndex;
	inc(idx);
	MoveItem(idx);
end;

procedure TOpenWithConfigForm.ActionMoveDownUpdate(Sender : TObject);
begin
	inherited;
	btnMoveDown.Enabled := (lbCommands.SelCount = 1) and (lbCommands.ItemIndex < lbCommands.Count - 1);
end;

procedure TOpenWithConfigForm.ActionMoveUpExecute(Sender : TObject);
begin
	inherited;
	var
	idx := lbCommands.ItemIndex;
	dec(idx);
	MoveItem(idx);
end;

procedure TOpenWithConfigForm.ActionMoveUpUpdate(Sender : TObject);
begin
	inherited;
	btnMoveUp.Enabled := (lbCommands.SelCount = 1) and (lbCommands.ItemIndex > 0);
end;

procedure TOpenWithConfigForm.ActionOkExecute(Sender : TObject);
begin
	WriteSettings;
	ModalResult := mrOk;
end;

procedure TOpenWithConfigForm.ActionRemoveExecute(Sender : TObject);
begin
	inherited;
	lbCommands.DeleteSelected;
end;

procedure TOpenWithConfigForm.ActionRemoveUpdate(Sender : TObject);
begin
	inherited;
	btnRemove.Enabled := (lbCommands.SelCount = 1);
end;

procedure TOpenWithConfigForm.ActionTestExecute(Sender : TObject);
var
	owp : TOpenWithParams;
begin
	inherited;
	owp := FSettings.TestFile; // GxOtaGetCurrentSourceFile;
	TOpenWithRunner.RunEditorCommand(lbCommands.Items[lbCommands.ItemIndex], owp);
end;

procedure TOpenWithConfigForm.ActionTestUpdate(Sender : TObject);
begin
	inherited;
	btnTest.Enabled := (lbCommands.SelCount = 1) and (not FSettings.TestFile.IsEmpty);
end;

procedure TOpenWithConfigForm.ActionOpenFileDlgExecute(Sender : TObject);
begin
	inherited;
	OpenDialog1.Filter := 'Executable files (*.exe)|*.exe';
	if OpenDialog1.Execute(self.Handle) then begin
		edt_OpenWithCmd.Text := OpenDialog1.FileName;
	end;
end;

function TOpenWithConfigForm.CheckCommand(const _sCmd : string) : Boolean;
var
	bFound : Boolean;
	sFileName : string;
	sPath : string;
begin
	Result := False;
	if _sCmd <> '' then begin
		sFileName := TFileUtils.ParseCommand(_sCmd).ExePath;
		TDebugUtils.DebugMessage(Format('TOpenWithConfigForm.CheckCommand Exe: %s ', [sFileName]));
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

procedure TOpenWithConfigForm.ClearOpenWithCmd;
begin
	edt_OpenWithCmd.Text := '';
end;

class procedure TOpenWithConfigForm.CreateAndShow(_settings : TRipGrepperOpenWithSettings);
begin
	var
	form := TOpenWithConfigForm.Create(nil, _settings);
	try
		form.ShowModal;
	finally
		form.Free
	end;
end;

procedure TOpenWithConfigForm.edt_OpenWithCmdKeyPress(Sender : TObject; var Key : Char);
begin
	inherited;
	btnModify.Default := True;
end;

procedure TOpenWithConfigForm.ReadSettings;
var
	arr : TArray<string>;
	listCmdsFromSettings : TStringList;
	i : integer;
begin
	inherited;
	lbCommands.MultiSelect := True;

	listCmdsFromSettings := TStringList.Create;
	try
		i := 0;
		repeat
			var
			sCmd := FSettings.Command[i];
			TDebugUtils.DebugMessage((Format('TOpenWithConfigForm.ReadSettings sCmd:%s ', [sCmd])));
			if sCmd = '' then
				break;
			listCmdsFromSettings.Add(sCmd);
			inc(i);
		until (i = MAX_COMMAND_NUM);

		if listCmdsFromSettings.Count <> 0 then begin
			lbCommands.Items.Clear;
		end;

		for var sCmd : string in listCmdsFromSettings do begin
			TDebugUtils.DebugMessage((Format('TOpenWithConfigForm.ReadSettings s:%s ', [sCmd])));
			arr := sCmd.Split([SEPARATOR]);
			if Length(arr) > 0 then begin
				lbCommands.Items.Add(arr[1]);
				lbCommands.Checked[lbCommands.Count - 1] := (arr[0].ToUpper() = 'TRUE');
				TDebugUtils.DebugMessage((Format('TOpenWithConfigForm.ReadSettings %s %s', [arr[0], arr[1]])));
			end;
		end;
	finally
		listCmdsFromSettings.Free;
	end;
end;

procedure TOpenWithConfigForm.WriteSettings;
var
	settings : string;
begin
	inherited;

	settings := '';
	for var i := 0 to lbCommands.Items.Count - 1 do begin
		var
		sCmd := lbCommands.Items[i].Replace(SEPARATOR, '', [rfReplaceAll]);
		TDebugUtils.DebugMessage((Format('TOpenWithConfigForm.WriteSettings %s ', [sCmd])));
		if (CheckCommand(sCmd)) then begin
			settings := Format('%s' + SEPARATOR + '%s', [BoolToStr(lbCommands.Checked[i], true), sCmd]);
		end else begin
			settings := Format('%s' + SEPARATOR + '%s', [BoolToStr(False, true), sCmd]);
		end;
		FSettings.Command[i] := settings;
		TDebugUtils.DebugMessage((Format('TOpenWithConfigForm.WriteSettings %s ', [FSettings.Command[i]])));
	end;
	FSettings.Store;
end;

procedure TOpenWithConfigForm.lbCommandsClick(Sender : TObject);
begin
	inherited;
	if edt_OpenWithCmd.Text = '' then begin
		PutSelectedToEdit;
	end;
end;

procedure TOpenWithConfigForm.lbCommandsDblClick(Sender : TObject);
begin
	inherited;
	PutSelectedToEdit;
	TDebugUtils.DebugMessage((Format('TOpenWithConfigForm.lbCommandsDblClick SelectCount %d', [lbCommands.SelCount])));
end;

procedure TOpenWithConfigForm.MoveItem(const idx : Integer);
begin
	if (idx >= 0) and (idx < lbCommands.Count) then begin
		lbCommands.Items.Move(lbCommands.ItemIndex, idx);
		lbCommands.Selected[idx] := True;
	end;
end;

procedure TOpenWithConfigForm.PutSelectedToEdit;
begin
	edt_OpenWithCmd.Text := lbCommands.Items[lbCommands.ItemIndex];
	TDebugUtils.DebugMessage((Format('TOpenWithConfigForm.lbCommandsDblClick %s ', [edt_OpenWithCmd.Text])));
end;

end.
