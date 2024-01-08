unit RipGrepperForm;

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
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  System.ImageList,
  Vcl.ImgList,
  System.Actions,
  Vcl.ActnList,
  ProcessTools,
  RipGrepperSettings;

type
  TRipGrepperForm = class(TForm, INewLineEventHandler)
    panelMain: TPanel;
    Label1: TLabel;
    LabelParams: TLabel;
    Label3: TLabel;
    btnConfig: TButton;
    lvResult: TListView;
    pnl_Bottom: TPanel;
    btn_Save: TButton;
    btn_Cancel: TButton;
    ImageListButtons: TImageList;
    alActions: TActionList;
    ActionSearch: TAction;
    ActionCancel: TAction;
    ActionConfig: TAction;
    cmbSearchDir: TComboBox;
    cmbSearchText: TComboBox;
    cmbParameters: TComboBox;
    procedure ActionCancelExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSettings: TRipGrepperSettings;
    procedure InitSettings;
    { Private-Deklarationen }
  public
    constructor Create(_settings: TRipGrepperSettings); reintroduce; overload;
    class function CreateAndShow(const _settings: TRipGrepperSettings): string;
    // INewLineEventHandler
    procedure OnNewResultLine(const _sLine: string);
    { Public-Deklarationen }
  end;

procedure OnNewLine(_handler: INewLineEventHandler; const _sLine: string);

var
  Form1: TRipGrepperForm;

implementation

uses
  DebugTools,
  Vcl.Dialogs,
  System.UITypes,
  System.IOUtils;

{$R *.dfm}

procedure OnNewLine(_handler: INewLineEventHandler; const _sLine: string);
begin
  _handler.OnNewResultLine(_sLine);
  TDebugUtils.DebugMessage(string(_sLine));
end;

constructor TRipGrepperForm.Create(_settings: TRipGrepperSettings);
begin
  inherited Create(nil);
  FSettings := _settings
end;

procedure TRipGrepperForm.ActionCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TRipGrepperForm.ActionSearchExecute(Sender: TObject);
var
  sArgs: TStringList;
  s: string;
begin
  sArgs := TStringList.Create();
  try
    var
      fp: string := cmbParameters.Text;
    var
      patterns: TArray<string> := fp.Split([' ']);
    for s in patterns do begin
      sArgs.Add(s);
      TDebugUtils.DebugMessage(string.Join(' ', sArgs.ToStringArray));
    end;

    sArgs.Add('"' + cmbSearchText.Text + '"');
    var workDir := cmbSearchDir.Text;
//    sArgs.Add(workDir);
    sArgs.Delimiter := ' ';
    sArgs.QuoteChar := '"';

    TDebugUtils.DebugMessage('run: ' + FSettings.RipGrepPath + ' ' + sArgs.DelimitedText);
    lvResult.items.Clear;
    var cmd := '"' + FSettings.RipGrepPath + '" ' + sArgs.DelimitedText;
    s := TProcessUtils.GetDosOutput(cmd, workDir, self as INewLineEventHandler);
    // TProcessUtils.RunProcess(FSettings.RipGrepPath, sArgs, self as INewLineEventHandler);
  finally
    sArgs.Free;
  end;
end;

class function TRipGrepperForm.CreateAndShow(const _settings: TRipGrepperSettings): string;
begin
  var
  form := TRipGrepperForm.Create(_settings);
  try
    if (mrOk = form.ShowModal()) then begin
      Result := form.lvResult.items[form.lvResult.ItemIndex].SubItems[0];
    end;

  finally
    form.Free;
  end;

end;

procedure TRipGrepperForm.FormShow(Sender: TObject);
begin
  InitSettings;

  cmbSearchDir.Text := FSettings.SearchDirs[0];
  cmbSearchText.Text := FSettings.SearchTexts[0];
  cmbParameters.Text := FSettings.RipGrepParams[0];;
end;

procedure TRipGrepperForm.InitSettings;
var
  rgPath: string;
begin
  var
  rgExists := TFileUtils.FindExecutable('rg.exe', rgPath);
  if not rgExists then begin
    MessageDlg('rg.exe not found', mtError, [mbOk], 0);
    Application.Terminate();
  end;

  if FSettings.RipGrepPath.IsEmpty or (not FileExists(FSettings.RipGrepPath)) then begin
    FSettings.RipGrepPath := rgPath.TrimLeft();
  end;

  if FSettings.SearchDirs.Count = 0 then begin
    FSettings.SearchDirs.Add(TDirectory.GetCurrentDirectory());
  end;

  if FSettings.SearchTexts.Count = 0 then begin
    FSettings.SearchTexts.Add('search text');
  end;

  if FSettings.RipGrepParams.Count = 0 then begin
    FSettings.RipGrepParams.Add('');
  end;
end;

procedure TRipGrepperForm.OnNewResultLine(const _sLine: string);
begin
  var
    item: TListItem := lvResult.items.Add();
  var
    s: string := string(_sLine);
  item.Caption := s;
//  item.SubItems.Add(s);
  TDebugUtils.DebugMessage(s);
end;

end.
