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

  System.JSON;

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
      FCurrentRelease : TReleaseInfo;
      FLatestRelease : TReleaseInfo;

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
  Winapi.ShellAPI,
  ArrayEx;

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
  ri : TReleaseInfo;
  relInfos : TArrayEx<TReleaseInfo>;
begin
  relInfos := TReleaseInfo.DownloadReleaseInfoFromGithub();
  ri := relInfos[0];
  if ri.LoadOk then begin
    FLatestRelease := ri;
    lnkLatestUrl.Visible := True;
    lblLatestRelease.Visible := True;
    lnkLatestUrl.Caption := Format('<a href="%s">%s</a>', [ri.HtmlURL, ri.Version]);

    Memo1.Lines.Add('Latest version' + TAB + ': ' + ri.Version);
    Memo1.Lines.Add('Published at' + TAB + ': ' + DateTimeToStr(ri.PublishedAt));
    Memo1.Lines.Add('Release url' + TAB + ': ' + ri.HtmlURL);
    Memo1.Lines.Add('Description' + TAB + ': ');
    Memo1.Lines.AddStrings(ri.Description.Split([CRLF]));
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

end.
