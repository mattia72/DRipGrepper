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
  RipGrepper.Helper.ListBox in 'RipGrepper.Helper.ListBox.pas',
  RipGrepper.UI.RipGrepOptionsForm in 'RipGrepper.UI.RipGrepOptionsForm.pas' {RipGrepOptionsForm},
  u_dzDpiScaleUtils in 'external\dzlib\u_dzDpiScaleUtils.pas',
  u_dzMiscUtils in 'external\dzlib\u_dzMiscUtils.pas',
  u_dzTypesUtils in 'external\dzlib\u_dzTypesUtils.pas',
  u_dzTypInfo in 'external\dzlib\u_dzTypInfo.pas',
  GX_IdeDock in 'external\GExpert\GX_IdeDock.pas',
  u_dzVersionInfo in 'external\dzlib\u_dzVersionInfo.pas',
  u_dzVclUtils in 'external\dzlib\u_dzVclUtils.pas',
  u_dzVariantUtils in 'external\dzlib\u_dzVariantUtils.pas',
  u_dzTypes in 'external\dzlib\u_dzTypes.pas',
  u_dzTranslator in 'external\dzlib\u_dzTranslator.pas',
  u_dzStringUtils in 'external\dzlib\u_dzStringUtils.pas',
  u_dzStringArrayUtils in 'external\dzlib\u_dzStringArrayUtils.pas',
  u_dzSortUtils in 'external\dzlib\u_dzSortUtils.pas',
  u_dzSortProvider in 'external\dzlib\u_dzSortProvider.pas',
  u_dzQuicksort in 'external\dzlib\u_dzQuicksort.pas',
  u_dzOsUtils in 'external\dzlib\u_dzOsUtils.pas',
  u_dzNamedThread in 'external\dzlib\u_dzNamedThread.pas',
  u_dzLineBuilder in 'external\dzlib\u_dzLineBuilder.pas',
  u_dzFileUtils in 'external\dzlib\u_dzFileUtils.pas',
  u_dzDateUtils in 'external\dzlib\u_dzDateUtils.pas',
  u_dzConvertUtils in 'external\dzlib\u_dzConvertUtils.pas',
  u_dzClassUtils in 'external\dzlib\u_dzClassUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TRipGrepperForm, RipGrepperForm);
  Application.Run;
end.


