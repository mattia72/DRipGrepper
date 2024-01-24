program DRipGrepper;



uses
  Vcl.Forms,
  RipGrepper.UI.MainForm in 'RipGrepper.UI.MainForm.pas' {RipGrepperForm},
  RipGrepper.Tools.ProcessUtils in 'RipGrepper.Tools.ProcessUtils.pas',
  RipGrepper.Tools.DebugTools in 'RipGrepper.Tools.DebugTools.pas',
  RipGrepper.Common.Settings in 'RipGrepper.Common.Settings.pas',
  RipGrepper.Data.Matches in 'RipGrepper.Data.Matches.pas',
  RipGrepper.Helper.UI in 'RipGrepper.Helper.UI.pas',
  RipGrepper.Tools.FileUtils in 'RipGrepper.Tools.FileUtils.pas',
  Vcl.Themes,
  Vcl.Styles,
  RipGrepper.Common.Types in 'RipGrepper.Common.Types.pas',
  RipGrepper.Helper.Types in 'RipGrepper.Helper.Types.pas',
  RipGrepper.Common.Interfaces in 'RipGrepper.Common.Interfaces.pas',
  dpipes in 'external\dpipes.pas',
  dprocess in 'external\dprocess.pas',
  RipGrepper.UI.SearchForm in 'RipGrepper.UI.SearchForm.pas' {RipGrepperSearchDialogForm},
  ArrayHelper in 'external\ArrayHelper.pas',
  RipGrepper.Data.Parsers in 'RipGrepper.Data.Parsers.pas',
  RipGrepper.Common.Sorter in 'RipGrepper.Common.Sorter.pas',
  RipGrepper.Data.HistoryItemObject in 'RipGrepper.Data.HistoryItemObject.pas',
  RipGrepper.Helper.ListBox in 'RipGrepper.Helper.ListBox.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TRipGrepperForm, RipGrepperForm);
  Application.Run;
end.

