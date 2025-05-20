unit RipGrepper.UI.Settings.AboutForm;

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
	Vcl.StdCtrls,
	RipGrepper.Settings.FontColors,
	RipGrepper.UI.SettingsFormBase,
	Vcl.ExtCtrls,
	Spring,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Settings.AppSettings,
	System.Actions,
	Vcl.ActnList,
	REST.Types,
	REST.Client,
	System.JSON,
	Data.Bind.Components,
	Data.Bind.ObjectScope;

type

	TReleaseInfo = record
		private
			FCurrentVersion : string;
			function GetCurrentVersion : string;

		public
			LatestDescription : string;
			LatestVersion : string;
			LatestHtmlURL : string;
			LatestPublishedAt : TDateTime;
			LoadOk : Boolean;
			procedure FromJson(_json : TJSONValue);
			property CurrentVersion : string read GetCurrentVersion;
	end;

	TAboutForm = class(TSettingsBaseForm)
		grpFontColors : TGroupBox;
		btnCheckUpdate : TButton;
		pnlBottom : TPanel;
		pnlTop : TPanel;
		ScrollBox1 : TScrollBox;
		ActionList1 : TActionList;
		ActionCheckUpdate : TAction;
		RESTClient1 : TRESTClient;
		RESTRequest1 : TRESTRequest;
		RESTResponse1 : TRESTResponse;
		Memo1 : TMemo;
		lnkLatestUrl : TLinkLabel;
		lblLatestRelease : TLabel;
		procedure ActionCheckUpdateExecute(Sender : TObject);
		procedure FormShow(Sender : TObject);
		procedure lnkLatestUrlLinkClick(Sender : TObject; const Link : string; LinkType : TSysLinkType);

		private
			FAppSettings : TAppSettings;
			FbSkipClickEvent : Boolean;
			FReleaseInfo : TReleaseInfo;

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
	RipGrepper.UI.ColorSelectorFrame,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	System.RegularExpressions,
	RipGrepper.Helper.UI.DarkMode,
	System.StrUtils,
	RipGrepper.Helper.UI,
	Winapi.ShellAPI,
	RipGrepper.Tools.FileUtils;

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
end;

procedure TAboutForm.ActionCheckUpdateExecute(Sender : TObject);
var
	jValue : TJSONValue;
begin
	try
		RESTRequest1.Execute;
		jValue := RESTResponse1.JSONValue;
		FReleaseInfo.FromJson(jValue);
	except
		on E : Exception do
			TMsgBox.ShowError('Update check failed.' + CRLF + E.Message);
	end;
	if FReleaseInfo.LoadOk then begin
		lnkLatestUrl.Visible := True;
		lblLatestRelease.Visible := True;
		lnkLatestUrl.Caption := Format('<a href="%s">%s</a>', [FReleaseInfo.LatestHtmlURL, FReleaseInfo.LatestVersion]);

		Memo1.Lines.Add('Current version'+TAB+': ' + FReleaseInfo.CurrentVersion);
		Memo1.Lines.Add('Latest version'+TAB+': ' + FReleaseInfo.LatestVersion);
		Memo1.Lines.Add('Published at'+TAB+': ' + DateTimeToStr(FReleaseInfo.LatestPublishedAt));
		Memo1.Lines.Add('Release url'+TAB+': ' + FReleaseInfo.LatestHtmlURL);
		Memo1.Lines.Add('Description'+TAB+': ');
		Memo1.Lines.AddStrings(FReleaseInfo.LatestDescription.Split([CRLF]));
	end;
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
	ShellExecute(0, 'OPEN', PChar(FReleaseInfo.LatestHtmlURL), '', '', SW_SHOWNORMAL);
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

procedure TReleaseInfo.FromJson(_json : TJSONValue);
begin
	LoadOk := False;
	var
	latestItem := _json.A[0];
	LatestHtmlURL := latestItem.GetValue<string>('html_url');
	LatestVersion := latestItem.GetValue<string>('name');
	LatestPublishedAt := latestItem.GetValue<TDateTime>('published_at');
	LatestDescription := latestItem.GetValue<string>('body');
	LoadOk := True;
end;

function TReleaseInfo.GetCurrentVersion : string;
begin
	if FCurrentVersion.IsEmpty then begin
		{$IFDEF STANDALONE}
		FCurrentVersion := TFileUtils.GetAppNameAndVersion(Application.ExeName);
		{$ELSE}
		FCurrentVersion := TFileUtils.GetPackageNameAndVersion(HInstance);
		{$ENDIF}
	end;
	Result := FCurrentVersion;
end;

end.
