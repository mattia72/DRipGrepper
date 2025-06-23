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
			FCurrentRelease : TReleaseUtils;
			FDownloadedReleaseInfos : TReleaseInfoArray;
			FsLicence : string;
			procedure AddDescriptionLinesToMemo(const _relInfo : IReleaseInfo);
			procedure AddReleaseToMemo(_relInfo : IReleaseInfo);
			procedure DownloadReleaseInfos(var _relInfos : TReleaseInfoArray);
			function GetLatestRelease() : IReleaseInfo;
			procedure ShowNewVersionMsgBox(const _relInfo : IReleaseInfo);
			procedure InitMemoWithCurrentRelInfo(_curVer : IReleaseInfo);
			function IsSameVersion(_vi1, _vi2 : TReleaseInfo) : Boolean;
			function MakeLinkCaption(const _label, _href, _text : string) : string;
			procedure OpenLink(const _link : string);
			procedure SetMemoText();
			function TryGetCurrentRelInfo(const _relInfos : TReleaseInfoArray; var _curInfo : IReleaseInfo) : Boolean;

		protected
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperSettings);
			function GetDescriptionFromResource(const _resource : string) : string;
			property LatestRelease : IReleaseInfo read GetLatestRelease;

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
	FCurrentRelease.CurrentRelease.SetDescription(s);

	lnkLatestUrl.Visible := False;

	FAppSettings := _settings.AppSettings;
	ReadSettings;

	lblTitle.Caption := FCurrentRelease.CurrentName;
	lblVersion.Caption := FCurrentRelease.CurrentVersion;
	lnkHomeURL.Caption := MakeLinkCaption('Home: ', HOME_PAGE, 'mattia72/DripGrepper');
	SetMemoText();
end;

procedure TAboutForm.ActionCheckUpdateExecute(Sender : TObject);
var
	curInfo : IReleaseInfo;
begin
	DownloadReleaseInfos(FDownloadedReleaseInfos);
	curInfo := FCurrentRelease.CurrentRelease;

	if TryGetCurrentRelInfo(FDownloadedReleaseInfos, curInfo) then begin
		AddReleaseToMemo(curInfo);
	end else begin
		AddReleaseToMemo(LatestRelease);
	end;
	tbcLicenceReleaseNotes.TabIndex := 2;

	if IsSameVersion(LatestRelease(), curInfo) then begin
		TMsgBox.ShowInfo('You are using the latest version.');
	end else begin
		ShowNewVersionMsgBox(LatestRelease);
	end;

	if LatestRelease.LoadOk then begin
		lnkLatestUrl.Visible := True;
		lnkLatestUrl.Caption := MakeLinkCaption('Latest: ', LatestRelease.HtmlURL, LatestRelease.Version);
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

procedure TAboutForm.AddReleaseToMemo(_relInfo : IReleaseInfo);
begin
	Memo1.Lines.Clear;
	Memo1.Lines.Add('Latest version' + TAB + ': ' + _relInfo.Version);
	Memo1.Lines.Add('Published at' + TAB + ': ' + DateTimeToStr(_relInfo.PublishedAt));
	Memo1.Lines.Add('Release url' + TAB + ': ' + _relInfo.HtmlURL);
	Memo1.Lines.Add('');
	AddDescriptionLinesToMemo(_relInfo);
end;

procedure TAboutForm.DownloadReleaseInfos(var _relInfos : TReleaseInfoArray);
var cursor : TCursorSaver;
begin
	cursor.SetHourGlassCursor();
	_relInfos.Clear;
	_relInfos := TReleaseUtils.DownloadReleaseInfoFromGithub();
end;

procedure TAboutForm.FormResize(Sender : TObject);
begin
	lnkHomeURL.Left := trunc(pnlLeft.Width / 2) - trunc(lnkHomeURL.Width / 2);
	lnkLatestUrl.Left := trunc(pnlLeft.Width / 2) - trunc(lnkLatestUrl.Width / 2);
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

function TAboutForm.GetLatestRelease() : IReleaseInfo;
begin
	Result := FDownloadedReleaseInfos[0];
end;

procedure TAboutForm.imgAboutDblClick(Sender : TObject);
begin
	OpenLink(HOME_PAGE);
end;

procedure TAboutForm.lnkLatestUrlLinkClick(Sender : TObject; const Link : string; LinkType : TSysLinkType);
begin
	OpenLink(LatestRelease.HtmlURL);
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

procedure TAboutForm.ShowNewVersionMsgBox(const _relInfo : IReleaseInfo);
begin
	if mrYes = TMsgBox.ShowQuestion(Format(
		{ } 'New version %s published at %s' + CRLF2 +
		{ } 'Do you wan''t to go to the release page?', [_relInfo.Version,
		{ } DateTimeToStr(_relInfo.PublishedAt)])) then begin
		OpenLink(_relInfo.HtmlURL);
	end;
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

function TAboutForm.IsSameVersion(_vi1, _vi2 : TReleaseInfo) : Boolean;
var mainVersion : string;
begin
	Result := False;
	if _vi1.LoadOk then begin
		mainVersion := _vi1.Version; // v4.1.2-beta
		mainVersion := mainVersion.Substring(0, mainVersion.IndexOf('-')); // v4.1.2
		Result := _vi2.Version.StartsWith(mainVersion);
	end;
end;

procedure TAboutForm.lnkHomeURLLinkClick(Sender : TObject; const Link : string; LinkType : TSysLinkType);
begin
	OpenLink(HOME_PAGE);
end;

function TAboutForm.MakeLinkCaption(const _label, _href, _text : string) : string;
begin
	Result := Format('%s<a href="%s">%s</a>', [_label, _href, _text]);
end;

procedure TAboutForm.OpenLink(const _link : string);
begin
	ShellExecute(0, 'OPEN', PChar(_link), '', '', SW_SHOWNORMAL);
end;

procedure TAboutForm.SetMemoText();
begin
	case tbcLicenceReleaseNotes.TabIndex of
		0 : begin
			Memo1.Text := FsLicence;
		end;
		1 : begin
			InitMemoWithCurrentRelInfo(FCurrentRelease.CurrentRelease);
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

function TAboutForm.TryGetCurrentRelInfo(const _relInfos : TReleaseInfoArray; var _curInfo : IReleaseInfo) : Boolean;
begin
	Result := False;
	for var relInfo in _relInfos do begin
		if IsSameVersion(relInfo(), FCurrentRelease.CurrentRelease()) then begin
			_curInfo := relInfo;
			Result := True;
			break;
		end;
	end;
end;

end.
