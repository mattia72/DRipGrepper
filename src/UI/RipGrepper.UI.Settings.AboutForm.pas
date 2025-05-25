unit RipGrepper.UI.Settings.AboutForm;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Variants,
	System.Actions,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	Vcl.StdCtrls,
	Vcl.ExtCtrls,
	Vcl.ActnList,
	Spring,
	RipGrepper.UI.SettingsFormBase,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Settings.AppSettings,
	RipGrepper.Tools.ReleaseUtils,

	System.JSON,
	ArrayEx;

type

	TAboutForm = class(TSettingsBaseForm)
		btnCheckUpdate : TButton;
		pnlBottom : TPanel;
		pnlTop : TPanel;
		ScrollBox1 : TScrollBox;
		ActionList1 : TActionList;
		ActionCheckUpdate : TAction;
		Memo1 : TMemo;
		lnkLatestUrl : TLinkLabel;
		lblLatestRelease : TLabel;
		lblLatestRelease1 : TLabel;
		pnlTitle : TPanel;
		procedure ActionCheckUpdateExecute(Sender : TObject);
		procedure FormShow(Sender : TObject);
		procedure lnkLatestUrlLinkClick(Sender : TObject; const Link : string; LinkType : TSysLinkType);

		private
			FAppSettings : TAppSettings;
			FbSkipClickEvent : Boolean;
			FCurrentRelease : TReleaseUtils;
			FLatestRelease : TReleaseInfo;
			procedure DownloadReleaseInfos(var _relInfos : TArrayEx<TReleaseInfo>);
			procedure HandleNewVersionPrompt(const ReleaseInfo: TReleaseInfo);

		protected
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperSettings);

		published
	end;

var
	AboutForm : TAboutForm;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	RipGrepper.Helper.UI.DarkMode,
	RipGrepper.Helper.UI,
	Winapi.ShellAPI;

{$R *.dfm}

constructor TAboutForm.Create(_Owner : TComponent; _settings : TRipGrepperSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := ABOUT_CAPTION;

	Memo1.Lines.Clear;
	lnkLatestUrl.Visible := False;
	lblLatestRelease.Visible := False;
	FAppSettings := _settings.AppSettings;
	ReadSettings;

	pnlTitle.Caption := FCurrentRelease.CurrentNameWithVersion;
end;

procedure TAboutForm.ActionCheckUpdateExecute(Sender : TObject);
var
	mainVersion : string;
	relInfos : TArrayEx<TReleaseInfo>;
begin
	DownloadReleaseInfos(relInfos);

	FLatestRelease := relInfos[0];
	if FLatestRelease.LoadOk then begin
		lnkLatestUrl.Visible := True;
		lblLatestRelease.Visible := True;
		lnkLatestUrl.Caption := Format('<a href="%s">%s</a>',
			{ } [FLatestRelease.HtmlURL, FLatestRelease.Version]);

		Memo1.Lines.Add('Latest version' + TAB + ': ' + FLatestRelease.Version);
		Memo1.Lines.Add('Published at' + TAB + ': ' + DateTimeToStr(FLatestRelease.PublishedAt));
		Memo1.Lines.Add('Release url' + TAB + ': ' + FLatestRelease.HtmlURL);
		Memo1.Lines.Add('Description' + TAB + ': ');
		Memo1.Lines.AddStrings(FLatestRelease.Description.Split([CRLF]));

		mainVersion := FLatestRelease.Version; // v4.1.2-beta
		mainVersion := mainVersion.Substring(0, mainVersion.IndexOf('-')); // v4.1.2
		if FCurrentRelease.CurrentRelease.Version.StartsWith(mainVersion) then begin
			TMsgBox.ShowInfo('You are using the latest version.');
		end else begin
			HandleNewVersionPrompt(FLatestRelease);
		end;
	end;
end;

procedure TAboutForm.DownloadReleaseInfos(var _relInfos : TArrayEx<TReleaseInfo>);
var cursor : TCursorSaver;
begin
	cursor.SetHourGlassCursor();
	_relInfos := TReleaseUtils.DownloadReleaseInfoFromGithub();
end;

procedure TAboutForm.FormShow(Sender : TObject);

begin
	inherited;
	FbSkipClickEvent := True;
	try

	finally
		FbSkipClickEvent := False;
	end;

end;

procedure TAboutForm.lnkLatestUrlLinkClick(Sender : TObject; const Link : string; LinkType : TSysLinkType);
begin
	ShellExecute(0, 'OPEN', PChar(FLatestRelease.HtmlURL), '', '', SW_SHOWNORMAL);
end;

procedure TAboutForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAboutForm.ReadSettings');
	FAppSettings.LoadFromDict;
end;

procedure TAboutForm.WriteSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAboutForm.WriteSettings');
end;

procedure TAboutForm.HandleNewVersionPrompt(const ReleaseInfo: TReleaseInfo);
begin
	if mrYes = TMsgBox.ShowQuestion(Format('New version %s published at %s' + CRLF2 +
		{ } 'Do you wan''t to go to the release page?', [ReleaseInfo.Version,
		{ } DateTimeToStr(ReleaseInfo.PublishedAt)])) then begin
		ShellExecute(0, 'OPEN', PChar(ReleaseInfo.HtmlURL), '', '', SW_SHOWNORMAL);
	end;
end;

end.
