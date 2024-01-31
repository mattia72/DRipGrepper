unit AGOpenWithConfig_Form;

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
	RipGrepper.Common.Settings;

type

	TAGOpenWithConfigForm = class(TForm)

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
			FSettings : TRipGrepperOpenWithSettings;
			function CheckCommand(const _sCmd : string) : Boolean;
			procedure ClearOpenWithCmd;
			procedure MoveItem(const idx : Integer);
			procedure PutSelectedToEdit;

		public
			{ Public-Deklarationen }
			constructor Create(AOwner : TComponent; const ASettings : TRipGrepperOpenWithSettings); reintroduce;
			class procedure CreateAndShow(_settings : TRipGrepperOpenWithSettings);
			class function ExecutableFoundByWhere(sFileName : string; out sOutpuPath : string) : Boolean;
			procedure InitSettings;
			procedure GetSettings;

	end;

implementation

uses
	System.SysUtils,
	Vcl.Clipbrd,
	Winapi.Windows,
	Winapi.ShellAPI,
	// GX_OtaUtils,
	AGOpenWithRunner,
	RipGrepper.OpenWith.Constants;

{$R *.dfm}

constructor TAGOpenWithConfigForm.Create(AOwner : TComponent; const ASettings : TRipGrepperOpenWithSettings);
begin
	self.FSettings := ASettings;
	inherited Create(AOwner);
end;

procedure TAGOpenWithConfigForm.ActionAddExecute(Sender : TObject);
begin
	inherited;
	lbCommands.Items.Add(edt_OpenWithCmd.Text);
	ClearOpenWithCmd;
end;

procedure TAGOpenWithConfigForm.ActionAddUpdate(Sender : TObject);
begin
	inherited;
	btnAdd.Enabled := edt_OpenWithCmd.Text <> '';
end;

procedure TAGOpenWithConfigForm.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
end;

procedure TAGOpenWithConfigForm.ActionModifyExecute(Sender : TObject);
begin
	inherited;
	lbCommands.Items[lbCommands.ItemIndex] := edt_OpenWithCmd.Text;
	ClearOpenWithCmd;
end;

procedure TAGOpenWithConfigForm.ActionModifyUpdate(Sender : TObject);
begin
	inherited;
	btnModify.Enabled := (lbCommands.SelCount = 1) and (edt_OpenWithCmd.Text <> '');
end;

procedure TAGOpenWithConfigForm.ActionMoveDownExecute(Sender : TObject);
begin
	inherited;
	inherited;
	var
	idx := lbCommands.ItemIndex;
	inc(idx);
	MoveItem(idx);
end;

procedure TAGOpenWithConfigForm.ActionMoveDownUpdate(Sender : TObject);
begin
	inherited;
	btnMoveDown.Enabled := (lbCommands.SelCount = 1) and (lbCommands.ItemIndex < lbCommands.Count - 1);
end;

procedure TAGOpenWithConfigForm.ActionMoveUpExecute(Sender : TObject);
begin
	inherited;
	var
	idx := lbCommands.ItemIndex;
	dec(idx);
	MoveItem(idx);
end;

procedure TAGOpenWithConfigForm.ActionMoveUpUpdate(Sender : TObject);
begin
	inherited;
	btnMoveUp.Enabled := (lbCommands.SelCount = 1) and (lbCommands.ItemIndex > 0);
end;

procedure TAGOpenWithConfigForm.ActionOkExecute(Sender : TObject);
begin
	ModalResult := mrOk;
end;

procedure TAGOpenWithConfigForm.ActionRemoveExecute(Sender : TObject);
begin
	inherited;
	lbCommands.DeleteSelected;
end;

procedure TAGOpenWithConfigForm.ActionRemoveUpdate(Sender : TObject);
begin
	inherited;
	btnRemove.Enabled := (lbCommands.SelCount = 1);
end;

procedure TAGOpenWithConfigForm.ActionTestExecute(Sender : TObject);
begin
	inherited;
	// var
	// sFileName := GxOtaGetCurrentSourceFile;
	// TOpenWithRunner.RunEditorCommand(lbCommands.Items[lbCommands.ItemIndex], sFileName);
end;

procedure TAGOpenWithConfigForm.ActionTestUpdate(Sender : TObject);
begin
	inherited;
	// btnTest.Enabled := (lbCommands.SelCount = 1);
end;

procedure TAGOpenWithConfigForm.ActionOpenFileDlgExecute(Sender : TObject);
begin
	inherited;
	OpenDialog1.Filter := 'Executable files (*.exe)|*.exe';
	if OpenDialog1.Execute(self.Handle) then begin
		edt_OpenWithCmd.Text := OpenDialog1.FileName;
	end;
end;

function TAGOpenWithConfigForm.CheckCommand(const _sCmd : string) : Boolean;
var
	bFound : Boolean;
	sFileName : string;
	sPath : string;
begin
	Result := False;
	if _sCmd <> '' then begin
		var
		iPos := Pos('.EXE', AnsiUppercase(_sCmd));
		if iPos = 0 then begin
			MessageDlg('Die Befehlszeile enthält keinen Programmaufruf!', mtError, [mbOK], 0);
			Abort;
		end;

		sFileName := Copy(_sCmd, 1, iPos + 3);
		OutputDebugString(PChar(Format('TAGOpenWithConfigForm.GetSettings Exe: %s ', [sFileName])));
		if not FileExists(sFileName) then begin
			bFound := False;
			sPath := ExtractFileDir(sFileName);
			if (sPath = '') and ExecutableFoundByWhere(sFileName, sPath) then begin
				bFound := True;
			end;
			if not bFound then begin
				MessageDlg(Format('Das Programm "%s" wurde nicht gefunden!', [sFileName]), mtError, [mbOK], 0);
				Abort;
			end;

		end;
		Result := True;
	end;
end;

procedure TAGOpenWithConfigForm.ClearOpenWithCmd;
begin
	edt_OpenWithCmd.Text := '';
end;

class procedure TAGOpenWithConfigForm.CreateAndShow(_settings : TRipGrepperOpenWithSettings);
begin
	var
	form := TAGOpenWithConfigForm.Create(nil, _settings);
	try
		form.ShowModal;
	finally
		form.Free
	end;
end;

procedure TAGOpenWithConfigForm.edt_OpenWithCmdKeyPress(Sender : TObject; var Key : Char);
begin
	inherited;
	btnModify.Default := True;
end;

procedure TAGOpenWithConfigForm.InitSettings;
var
	arr : TArray<string>;
	list : TStringList;
	i : integer;
begin
	inherited;
	lbCommands.MultiSelect := True;
	lbCommands.Items.Clear;

	list := TStringList.Create;
	try
		i := 0;
		repeat
			var
			sCmd := FSettings.Command[i];
			OutputDebugString(PChar(Format('TAGOpenWithConfigForm.InitSettings sCmd:%s ', [sCmd])));
			if sCmd = '' then
				break;
			list.Add(sCmd);
			inc(i);
		until (i = MAX_COMMAND_NUM);

		for var sCmd : string in list do begin
			OutputDebugString(PChar(Format('TAGOpenWithConfigForm.InitSettings s:%s ', [sCmd])));
			arr := sCmd.Split([SEPARATOR]);
			if Length(arr) > 0 then begin
				lbCommands.Items.Add(arr[1]);
				lbCommands.Checked[lbCommands.Count - 1] := (arr[0].ToUpper() = 'TRUE');
				OutputDebugString(PChar(Format('TAGOpenWithConfigForm.InitSettings %s %s', [arr[0], arr[1]])));
			end;
		end;
	finally
		list.Free;
	end;
end;

class function TAGOpenWithConfigForm.ExecutableFoundByWhere(sFileName : string; out sOutpuPath : string) : Boolean;
var
	Buffer : array [0 .. MAX_PATH] of Char;
begin
	Result := FindExecutable(PChar(sFileName), PChar(nil), &Buffer) > 0;
	SetString(sOutpuPath, PChar(@Buffer[0]), Length(Buffer));
	OutputDebugString(PChar(Format('TAGOpenWithConfigForm.ExecutableFoundByWhere %s ', [sOutpuPath])));
end;

procedure TAGOpenWithConfigForm.GetSettings;
var
	settings : string;
begin
	inherited;

	settings := '';
	for var i := 0 to lbCommands.Items.Count - 1 do begin
		var
		sCmd := lbCommands.Items[i].Replace(SEPARATOR, '', [rfReplaceAll]);
		if (CheckCommand(sCmd)) then begin
			OutputDebugString(PChar(Format('TAGOpenWithConfigForm.GetSettings %s ', [sCmd])));
			settings := Format('%s' + SEPARATOR + '%s', [BoolToStr(lbCommands.Checked[i], true), sCmd]);
			FSettings.Command[i] := settings;
			OutputDebugString(PChar(Format('TAGOpenWithConfigForm.GetSettings %s ', [FSettings.Command[i]])));
		end;
	end;

end;

procedure TAGOpenWithConfigForm.lbCommandsClick(Sender : TObject);
begin
	inherited;
	if edt_OpenWithCmd.Text = '' then begin
		PutSelectedToEdit;
	end;
end;

procedure TAGOpenWithConfigForm.lbCommandsDblClick(Sender : TObject);
begin
	inherited;
	PutSelectedToEdit;
	OutputDebugString(PChar(Format('TAGOpenWithConfigForm.lbCommandsDblClick SelectCount %d', [lbCommands.SelCount])));
end;

procedure TAGOpenWithConfigForm.MoveItem(const idx : Integer);
begin
	if (idx >= 0) and (idx < lbCommands.Count) then begin
		lbCommands.Items.Move(lbCommands.ItemIndex, idx);
		lbCommands.Selected[idx] := True;
	end;
end;

procedure TAGOpenWithConfigForm.PutSelectedToEdit;
begin
	edt_OpenWithCmd.Text := lbCommands.Items[lbCommands.ItemIndex];
	OutputDebugString(PChar(Format('TAGOpenWithConfigForm.lbCommandsDblClick %s ', [edt_OpenWithCmd.Text])));
end;

end.
