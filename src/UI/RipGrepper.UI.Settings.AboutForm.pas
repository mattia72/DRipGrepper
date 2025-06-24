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
	ArrayEx,
	Vcl.Imaging.jpeg,
	Vcl.Imaging.pngimage,
	Vcl.ComCtrls;

type

	TAboutForm = class(TSettingsBaseForm)
		btnCheckUpdate : TButton;
		pnlBottom : TPanel;
		pnlLeft : TPanel;
		ScrollBox1 : TScrollBox;
		ActionList1 : TActionList;
		ActionCheckUpdate : TAction;
		Memo1 : TMemo;
		lnkLatestUrl : TLinkLabel;
		lblLatestRelease1 : TLabel;
		pnlRight : TPanel;
		imgAbout : TImage;
		lblTitle : TLabel;
		pnlTopRight : TPanel;
		lnkHomeURL : TLinkLabel;
		lblVersion : TLabel;
		tbcLicenceReleaseNotes : TTabControl;
		cbCheckNewReleaseOnStartup : TCheckBox;
		procedure ActionCheckUpdateExecute(Sender : TObject);
		procedure FormResize(Sender : TObject);
		procedure FormShow(Sender : TObject);
		procedure imgAboutDblClick(Sender : TObject);
		procedure lnkHomeURLLinkClick(Sender : TObject; const Link : string; LinkType : TSysLinkType);
		procedure lnkLatestUrlLinkClick(Sender : TObject; const Link : string; LinkType : TSysLinkType);
		procedure tbcLicenceReleaseNotesChange(Sender : TObject);

		private
			FAppSettings : TAppSettings;
			FbSkipClickEvent : Boolean;
			FReleaseUtils : TReleaseUtils;

			FsLicence : string;
			procedure AddDescriptionLinesToMemo(const _relInfo : IReleaseInfo);
			procedure AddReleaseToMemo(_relInfo : IReleaseInfo; const _bCheckLoading : Boolean = True);
			procedure InitMemoWithCurrentRelInfo(_curVer : IReleaseInfo);
			function MakeLinkCaption(const _label, _href, _text : string) : string;
			procedure SetMemoText();

		protected
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperSettings);
			function GetDescriptionFromResource(const _resource : string) : string;

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

	FsLicence := GetDescriptionFromResource('LICENSE');

	var
	s := GetDescriptionFromResource('DeployDescription');
	FReleaseUtils.CurrentRelease.SetDescription(s);

	lnkLatestUrl.Visible := False;

	FAppSettings := _settings.AppSettings;
	ReadSettings;

	lblTitle.Caption := FReleaseUtils.CurrentName;
	lblVersion.Caption := FReleaseUtils.CurrentVersion;
	lnkHomeURL.Caption := MakeLinkCaption('Home: ', HOME_PAGE, 'mattia72/DripGrepper');
	SetMemoText();
end;

procedure TAboutForm.ActionCheckUpdateExecute(Sender : TObject);
var
	curInfo : IReleaseInfo;
begin
	FReleaseUtils.DownloadReleaseInfos();
	curInfo := FReleaseUtils.CurrentRelease;

	if FReleaseUtils.TryGetCurrentRelInfo(curInfo) then begin
		AddReleaseToMemo(curInfo);
	end else begin
		AddReleaseToMemo(FReleaseUtils.LatestRelease);
	end;
	tbcLicenceReleaseNotes.TabIndex := 2;

	FReleaseUtils.ShowNewVersionMsgBox();

	if FReleaseUtils.LatestRelease.LoadOk then begin
		lnkLatestUrl.Visible := True;
		lnkLatestUrl.Caption := MakeLinkCaption('', FReleaseUtils.LatestRelease.HtmlURL, FReleaseUtils.LatestVersion);
	end;
end;

procedure TAboutForm.AddDescriptionLinesToMemo(const _relInfo : IReleaseInfo);
var
	descr : TArrayEx<string>;
begin
	descr := _relInfo.Description.Split([CRLF]);
	var
	sPrev := '';
	for var s in descr do begin
		if s = sPrev then begin // skip more then two empty lines
			continue;
		end;
		Memo1.Lines.Add(s);
		sPrev := s;
	end;
	Memo1.Lines.Insert(0, '');
end;

procedure TAboutForm.AddReleaseToMemo(_relInfo : IReleaseInfo; const _bCheckLoading : Boolean = True);
begin
	Memo1.Lines.Clear;
	if _bCheckLoading and _relInfo.LoadOk then begin
		Memo1.Lines.Add('Latest version' + TAB + ': ' + _relInfo.Version);
		Memo1.Lines.Add('Published at' + TAB + ': ' + DateTimeToStr(_relInfo.PublishedAt));
		Memo1.Lines.Add('Release url' + TAB + ': ' + _relInfo.HtmlURL);
		Memo1.Lines.Add('');
		AddDescriptionLinesToMemo(_relInfo);
	end else begin
		Memo1.Lines.Add(FReleaseUtils.DownloadErrorMsg);
	end;
end;

procedure TAboutForm.FormResize(Sender : TObject);
begin
	// lnkHomeURL.Left := trunc(pnlLeft.Width / 2) - trunc(lnkHomeURL.Width / 2);
	// lnkLatestUrl.Left := trunc(pnlLeft.Width / 2) - trunc(lnkLatestUrl.Width / 2);
end;

procedure TAboutForm.FormShow(Sender : TObject);
begin
	inherited;
	FbSkipClickEvent := True;
	try
		FormResize(self);
	finally
		FbSkipClickEvent := False;
	end;
end;

function TAboutForm.GetDescriptionFromResource(const _resource : string) : string;
var
	stream : TResourceStream;
	list : TStringList;
begin
	stream := TResourceStream.Create(HInstance, _resource, RT_RCDATA);
	list := TStringList.Create;
	try
		list.DefaultEncoding := TEncoding.UTF8;
		list.LoadFromStream(stream);
		Result := list.Text;
	finally
		list.Free;
		stream.Free;
	end;
end;

procedure TAboutForm.imgAboutDblClick(Sender : TObject);
begin
	TUrlLinkHelper.OpenLink(HOME_PAGE);
end;

procedure TAboutForm.lnkLatestUrlLinkClick(Sender : TObject; const Link : string; LinkType : TSysLinkType);
begin
	TUrlLinkHelper.OpenLink(FReleaseUtils.LatestRelease.HtmlURL);
end;

procedure TAboutForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAboutForm.ReadSettings');
	FAppSettings.LoadFromDict;
	cbCheckNewReleaseOnStartup.Checked := FAppSettings.CheckNewVersionOnStartup;
end;

procedure TAboutForm.WriteSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAboutForm.WriteSettings');
	FAppSettings.CheckNewVersionOnStartup := cbCheckNewReleaseOnStartup.Checked;
end;

procedure TAboutForm.InitMemoWithCurrentRelInfo(_curVer : IReleaseInfo);
begin
	Memo1.Lines.Clear;
	Memo1.Lines.Add('Current version' + TAB + ': ' + _curVer.Version);

	if 0 <> _curVer.PublishedAt then begin
		Memo1.Lines.Add('Published at' + TAB + ': ' + DateTimeToStr(_curVer.PublishedAt));
		Memo1.Lines.Add('Release url' + TAB + ': ' + _curVer.HtmlURL);
	end;
	Memo1.Lines.Add('');
	AddDescriptionLinesToMemo(_curVer);
end;

procedure TAboutForm.lnkHomeURLLinkClick(Sender : TObject; const Link : string; LinkType : TSysLinkType);
begin
	TUrlLinkHelper.OpenLink(HOME_PAGE);
end;

function TAboutForm.MakeLinkCaption(const _label, _href, _text : string) : string;
begin
	Result := Format('%s<a href="%s">%s</a>', [_label, _href, _text]);
end;

procedure TAboutForm.SetMemoText();
begin
	case tbcLicenceReleaseNotes.TabIndex of
		0 : begin
			Memo1.Text := FsLicence;
		end;
		1 : begin
			InitMemoWithCurrentRelInfo(FReleaseUtils.CurrentRelease);
		end;
		2 : begin
			ActionCheckUpdateExecute(self);
		end;
	end;
end;

procedure TAboutForm.tbcLicenceReleaseNotesChange(Sender : TObject);
begin
	SetMemoText();
end;

end.
