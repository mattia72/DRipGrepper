unit RipGrepper.UI.Settings.AppSettingsForm;

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
	Vcl.StdCtrls,
	RipGrepper.Settings.Persistable,
	RipGrepper.Settings.AppSettings,
	RipGrepper.UI.SettingsFormBase,
	RipGrepper.UI.ColorSelectorFrame,
	RipGrepper.Settings.FontColors,
	RipGrepper.Settings.RipGrepperSettings,
	RTTI,
	Vcl.Mask,
	RipGrepper.Settings.RipGrepParameterSettings,
	System.ImageList,
	Vcl.ImgList,
	System.Actions,
	Vcl.ActnList,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	SVGIconImageListBase,
	SVGIconImageList,
	Spring,
	Vcl.Samples.Spin,
	Vcl.ControlList;

type
	EValidateCtrls = (vcRgExePath, vcIniFilePath);

	TAppSettingsForm = class(TSettingsBaseForm)
		btnedtRgExePath : TButtonedEdit;
		lblRgExePath : TLabel;
		OpenDialog1 : TOpenDialog;
		ActionList1 : TActionList;
		ActionOpenFileDialog : TAction;
		lblVersion : TLabel;
		Memo1 : TMemo;
		SVGIconImageList1 : TSVGIconImageList;
		ScrollBox1 : TScrollBox;
		grpSettings : TGroupBox;
		Label2 : TLabel;
		cmbCopyCmdShell : TComboBox;
		Label3 : TLabel;
		seCmbHistoryCount : TSpinEdit;
		cbLoadLastSearchHistories : TCheckBox;
		rgModeLoadSeraches : TRadioGroup;
		seSearchHistoryCount : TSpinEdit;
		lblSearches : TLabel;
		cbSaveResults : TCheckBox;
		grpSaveLoad : TGroupBox;
		procedure btnedtRgExePathEnter(Sender : TObject);
		procedure btnedtRgExePathExit(Sender : TObject);
		procedure btnedtRgExePathLeftButtonClick(Sender : TObject);
		procedure btnedtRgExePathRightButtonClick(Sender : TObject);
		procedure FormShow(Sender : TObject);

		private

			FRefocusing : TObject;
			FAppSettings : TAppSettings;
			FRipGrepSettings : TRipGrepParameterSettings;
			function IsRgExeValid(const filePath : string) : Boolean;
			{ User-defined message handler }
			procedure ValidateInput(var M : TMessage); message USERMESSAGE_VALIDATE_INPUT;

		protected
			procedure OnSettingsUpdated(); override;
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperSettings);
			function GetRgVersion(const _rgPath : string) : string;

		published
	end;

var
	AppSettingsForm : TAppSettingsForm;

implementation

uses

	RipGrepper.Tools.FileUtils,
	RipGrepper.Helper.UI,
	System.IOUtils,
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.OpenWith.Params,
	RipGrepper.OpenWith,
	RipGrepper.Helper.UI.DarkMode,
	System.StrUtils,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Helper.Types,
	RipGrepper.Common.LoadHistoryMode;

{$R *.dfm}

constructor TAppSettingsForm.Create(_Owner : TComponent; _settings : TRipGrepperSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := 'General';
	FAppSettings := (FSettings as TRipGrepperSettings).AppSettings;
	FRipGrepSettings := (FSettings as TRipGrepperSettings).RipGrepParameters;
end;

procedure TAppSettingsForm.btnedtRgExePathEnter(Sender : TObject);
begin
	if FRefocusing = btnedtRgExePath then
		FRefocusing := nil;
end;

procedure TAppSettingsForm.btnedtRgExePathExit(Sender : TObject);
begin
	if FRefocusing = nil then
		PostMessage(Handle, USERMESSAGE_VALIDATE_INPUT, 0, LParam(vcRgExePath));
end;

procedure TAppSettingsForm.btnedtRgExePathLeftButtonClick(Sender : TObject);
begin
	Memo1.Text := 'rg.exe --version';
	Memo1.Text := GetRgVersion(btnedtRgExePath.Text);
end;

procedure TAppSettingsForm.btnedtRgExePathRightButtonClick(Sender : TObject);
var
	origFilePath, filePath : string;
begin
	OpenDialog1.Filter := 'Executable files (*.exe)|*.exe';
	origFilePath := btnedtRgExePath.Text;
	OpenDialog1.FileName := origFilePath;
	if OpenDialog1.Execute(self.Handle) then begin
		filePath := OpenDialog1.FileName;
		btnedtRgExePath.Text := filePath;
	end;
	PostMessage(Handle, USERMESSAGE_VALIDATE_INPUT, 0, LParam(vcRgExePath));
end;

procedure TAppSettingsForm.FormShow(Sender : TObject);
begin
	ReadSettings;
end;

function TAppSettingsForm.GetRgVersion(const _rgPath : string) : string;
var
	sl : TStrings;
begin
	if _rgPath.IsEmpty then begin
		Result := RG_EXE + ' not found.';
		Exit;
	end;

	sl := TStringList.Create();
	sl.Add('--version');
	try
		TSimpleProcessOutputStringReader.RunProcess(_rgPath, sl, '.', sl);
		Result := sl.Text;
	finally
		sl.Free;
	end;
end;

function TAppSettingsForm.IsRgExeValid(const filePath : string) : Boolean;
var
	name : string;
begin
	name := TPath.GetFileName(filePath);
	Result := LowerCase(name) = LowerCase(RG_EXE);
end;

procedure TAppSettingsForm.OnSettingsUpdated();
begin
	// here you can update things depending on changed settings
	TDebugUtils.UpdateTraceActive;
end;

procedure TAppSettingsForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAppSettingsForm.ReadSettings');

	FAppSettings.LoadFromDict;
	FRipGrepSettings.LoadFromDict;

	cmbCopyCmdShell.ItemIndex := Integer(FAppSettings.CopyToClipBoardShell);
	seCmbHistoryCount.Value := FAppSettings.ComboHistoryCount;
	seSearchHistoryCount.Value := FAppSettings.SearchHistoryCount;

	FAppSettings.UpdateInternalsFromSettings();
	cbLoadLastSearchHistories.Checked := FAppSettings.LoadHistoryMode.IsSaveHistoryActive;
	rgModeLoadSeraches.ItemIndex := FAppSettings.LoadHistoryMode.ToInt;
	cbSaveResults.Checked := FAppSettings.LoadHistoryMode.IsSet(lhmSaveResults);
	var
	path := FRipGrepSettings.RipGrepPath;
	if path.IsEmpty then begin
		FRipGrepSettings.TryGetRipGrepPath(path);
	end;
	btnedtRgExePath.Text := path;
	btnedtRgExePath.LeftButton.Hint := 'Refresh version info';
	btnedtRgExePath.RightButton.Hint := 'Select rg.exe';
	Memo1.Text := GetRgVersion(path);
end;

procedure TAppSettingsForm.ValidateInput(var M : TMessage);
begin
	case EValidateCtrls(M.LParam) of
		vcRgExePath : begin
			if IsRgExeValid(btnedtRgExePath.Text) then begin
				Memo1.Text := GetRgVersion(btnedtRgExePath.Text);
			end else begin
				FRefocusing := btnedtRgExePath;
				TMsgBox.ShowError('Rg.exe path is not valid!');
				btnedtRgExePath.SetFocus;
			end;

		end;
	end;
end;

procedure TAppSettingsForm.WriteSettings;
var
	lhm : IShared<TLoadHistoryModes>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAppSettingsForm.WriteSettings');
	FAppSettings.CopyToClipBoardShell := TShellType(cmbCopyCmdShell.ItemIndex);
	FAppSettings.ComboHistoryCount := seCmbHistoryCount.Value;
	FAppSettings.SearchHistoryCount := seSearchHistoryCount.Value;
	lhm := FAppSettings.LoadHistoryMode;
	if (cbLoadLastSearchHistories.Checked) then begin
		lhm.AddModeFromInt(rgModeLoadSeraches.ItemIndex);
	end else begin
		lhm.CleanModes(False);
	end;
	if cbSaveResults.Checked then begin
		lhm.AddMode(lhmSaveResults);
	end else begin
		lhm.RemoveMode(lhmSaveResults);
	end;
	FAppSettings.UpdateSettingsFromInternals;

	var
	rgPath := btnedtRgExePath.Text;
	if IsRgExeValid(rgPath) then begin
		FRipGrepSettings.RipGrepPath := rgPath;
	end;
end;

end.
