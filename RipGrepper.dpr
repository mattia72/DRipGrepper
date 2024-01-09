program RipGrepper;



uses
  Vcl.Forms,
  RipGrepper.UI.MainForm in 'RipGrepper.UI.MainForm.pas' {RipGrepperForm},
  dpipes in 'dpipes.pas',
  dprocess in 'dprocess.pas',
  RipGrepper.Tools.ProcessUtils in 'RipGrepper.Tools.ProcessUtils.pas',
  RipGrepper.Tools.DebugTools in 'RipGrepper.Tools.DebugTools.pas',
  RipGrepperSettings in 'RipGrepperSettings.pas',
  RipGrepperMatches in 'RipGrepperMatches.pas',
  RipGrepper.Helper.CursorSaver in 'RipGrepper.Helper.CursorSaver.pas',
  RipGrepper.Tools.FileUtils in 'RipGrepper.Tools.FileUtils.pas',
  Vcl.Themes,
  Vcl.Styles,
  RipGrepper.Helper.Types in 'RipGrepper.Helper.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TRipGrepperForm, Form1);
  Application.Run;
end.


