program DRipGrepper;

uses
  Vcl.Forms,
  RipGrepper.UI.MainForm in 'src\RipGrepper.UI.MainForm.pas' {RipGrepperForm},
  RipGrepper.Tools.ProcessUtils in 'src\RipGrepper.Tools.ProcessUtils.pas',
  RipGrepper.Tools.DebugTools in 'src\RipGrepper.Tools.DebugTools.pas',
  RipGrepper.Common.Settings in 'src\RipGrepper.Common.Settings.pas',
  RipGrepper.Data.Matches in 'src\RipGrepper.Data.Matches.pas',
  RipGrepper.Helper.UI in 'src\RipGrepper.Helper.UI.pas',
  RipGrepper.Tools.FileUtils in 'src\RipGrepper.Tools.FileUtils.pas',
  Vcl.Themes,
  Vcl.Styles,
  RipGrepper.Common.Types in 'src\RipGrepper.Common.Types.pas',
  RipGrepper.Helper.Types in 'src\RipGrepper.Helper.Types.pas',
  RipGrepper.Common.Interfaces in 'src\RipGrepper.Common.Interfaces.pas',
  dpipes in 'external\dpipes.pas',
  dprocess in 'external\dprocess.pas',
  RipGrepper.UI.SearchForm in 'src\RipGrepper.UI.SearchForm.pas' {RipGrepperSearchDialogForm},
  ArrayEx in 'external\ArrayEx.pas',
  RipGrepper.Data.Parsers in 'src\RipGrepper.Data.Parsers.pas',
  RipGrepper.Common.Sorter in 'src\RipGrepper.Common.Sorter.pas',
  RipGrepper.Data.HistoryItemObject in 'src\RipGrepper.Data.HistoryItemObject.pas',
  RipGrepper.Helper.ListBox in 'src\RipGrepper.Helper.ListBox.pas',
  RipGrepper.UI.RipGrepOptionsForm in 'src\RipGrepper.UI.RipGrepOptionsForm.pas' {RipGrepOptionsForm},
  u_dzDpiScaleUtils in 'external\dzlib\u_dzDpiScaleUtils.pas',
  u_dzMiscUtils in 'external\dzlib\u_dzMiscUtils.pas',
  u_dzTypesUtils in 'external\dzlib\u_dzTypesUtils.pas',
  u_dzTypInfo in 'external\dzlib\u_dzTypInfo.pas',
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
  u_dzClassUtils in 'external\dzlib\u_dzClassUtils.pas',
  RipGrepper.Parsers.VimGrepMatchLine in 'src\RipGrepper.Parsers.VimGrepMatchLine.pas',
  RipGrepper.Common.ParsedObject in 'src\RipGrepper.Common.ParsedObject.pas',
  GX_OtaUtils in 'external\GExpert\GX_OtaUtils.pas',
  GX_GxUtils in 'external\GExpert\GX_GxUtils.pas',
  GX_IdeUtils in 'external\GExpert\GX_IdeUtils.pas',
  RipGrepper.OpenWith.CmdListForm in 'src\OpenWith\RipGrepper.OpenWith.CmdListForm.pas' {OpenWithCmdList},
  RipGrepper.OpenWith.ConfigForm in 'src\OpenWith\RipGrepper.OpenWith.ConfigForm.pas' {OpenWithConfigForm},
  RipGrepper.OpenWith in 'src\OpenWith\RipGrepper.OpenWith.pas',
  RipGrepper.OpenWith.Runner in 'src\OpenWith\RipGrepper.OpenWith.Runner.pas',
  RipGrepper.OpenWith.SimpleTypes in 'src\OpenWith\RipGrepper.OpenWith.SimpleTypes.pas',
  RipGrepper.UI.ScaleableBaseForm in 'src\RipGrepper.UI.ScaleableBaseForm.pas',
  RipGrepper.UI.MainFrame in 'src\RipGrepper.UI.MainFrame.pas' {RipGrepperMainFrame: TFrame},
  RipGrepper.UI.BottomFrame in 'src\RipGrepper.UI.BottomFrame.pas' {RipGrepperBottomFrame: TFrame},
  RipGrepper.UI.TopFrame in 'src\RipGrepper.UI.TopFrame.pas' {RipGrepperTopFrame: TFrame},
  RipGrepper.UI.ParentFrame in 'src\RipGrepper.UI.ParentFrame.pas' {ParentFrame: TFrame},
  RipGrepper.UI.DpiScaler in 'src\RipGrepper.UI.DpiScaler.pas';

{$R *.res}

begin
	{$IFDEF DEBUG}
	ReportMemoryLeaksOnShutdown := True;
	{$ENDIF}
	Application.Initialize;
	Application.MainFormOnTaskbar := True;
	Application.CreateForm(TRipGrepperForm, RipGrepperForm);
  Application.Run;

end.
