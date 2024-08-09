program DRipGrepper;

uses
  {$IFDEF FASTMM4_OPT}
  FastMM4,
  {$ENDIF }
  Vcl.Forms,
  dpipes in 'external\dpipes.pas',
  dprocess in 'external\dprocess.pas',
  ArrayEx in 'external\ArrayEx.pas',
  u_dzDpiScaleUtils in 'external\dzlib\u_dzDpiScaleUtils.pas',
  u_dzMiscUtils in 'external\dzlib\u_dzMiscUtils.pas',
  u_dzTypesUtils in 'external\dzlib\u_dzTypesUtils.pas',
  u_dzTypInfo in 'external\dzlib\u_dzTypInfo.pas',
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
  RipGrepper.Common.Constants in 'src\RipGrepper.Common.Constants.pas',
  RipGrepper.Common.Interfaces in 'src\RipGrepper.Common.Interfaces.pas',
  RipGrepper.Common.ParsedObject in 'src\RipGrepper.Common.ParsedObject.pas',
  RipGrepper.Common.Settings.RipGrepperSearchFormSettings in 'src\RipGrepper.Common.Settings.RipGrepperSearchFormSettings.pas',
  RipGrepper.Common.Settings.Base in 'src\RipGrepper.Common.Settings.Base.pas',
  RipGrepper.Common.Settings.RipGrepParameterSettings in 'src\RipGrepper.Common.Settings.RipGrepParameterSettings.pas',
  RipGrepper.Common.Sorter in 'src\RipGrepper.Common.Sorter.pas',
  RipGrepper.Data.HistoryItemObject in 'src\RipGrepper.Data.HistoryItemObject.pas',
  RipGrepper.Data.Matches in 'src\RipGrepper.Data.Matches.pas',
  RipGrepper.Data.Parsers in 'src\RipGrepper.Data.Parsers.pas',
  RipGrepper.Helper.ListBox in 'src\RipGrepper.Helper.ListBox.pas',
  RipGrepper.Helper.Types in 'src\RipGrepper.Helper.Types.pas',
  RipGrepper.Helper.UI in 'src\RipGrepper.Helper.UI.pas',
  RipGrepper.OpenWith in 'src\OpenWith\RipGrepper.OpenWith.pas',
  RipGrepper.OpenWith.CmdListForm in 'src\OpenWith\RipGrepper.OpenWith.CmdListForm.pas' {OpenWithCmdList},
  RipGrepper.OpenWith.ConfigForm in 'src\OpenWith\RipGrepper.OpenWith.ConfigForm.pas' {OpenWithConfigForm},
  RipGrepper.OpenWith.Runner in 'src\OpenWith\RipGrepper.OpenWith.Runner.pas',
  RipGrepper.OpenWith.Constants in 'src\OpenWith\RipGrepper.OpenWith.Constants.pas',
  RipGrepper.Parsers.Factory in 'src\RipGrepper.Parsers.Factory.pas',
  RipGrepper.Parsers.ParallelParser in 'src\RipGrepper.Parsers.ParallelParser.pas',
  RipGrepper.Parsers.VimGrepMatchLine in 'src\RipGrepper.Parsers.VimGrepMatchLine.pas',
  RipGrepper.Tools.DebugUtils in 'src\RipGrepper.Tools.DebugUtils.pas',
  RipGrepper.Tools.FileUtils in 'src\RipGrepper.Tools.FileUtils.pas',
  RipGrepper.Tools.ProcessUtils in 'src\RipGrepper.Tools.ProcessUtils.pas',
  RipGrepper.UI.DpiScaler in 'src\RipGrepper.UI.DpiScaler.pas',
  RipGrepper.UI.MainForm in 'src\RipGrepper.UI.MainForm.pas' {RipGrepperForm},
  RipGrepper.UI.TopFrame in 'src\RipGrepper.UI.TopFrame.pas' {RipGrepperTopFrame: TFrame},
  RipGrepper.UI.MiddleFrame in 'src\RipGrepper.UI.MiddleFrame.pas' {RipGrepperMiddleFrame: TFrame},
  RipGrepper.UI.BottomFrame in 'src\RipGrepper.UI.BottomFrame.pas' {RipGrepperBottomFrame: TFrame},
  RipGrepper.UI.ParentFrame in 'src\RipGrepper.UI.ParentFrame.pas' {ParentFrame: TFrame},
  RipGrepper.UI.RipGrepOptionsForm in 'src\RipGrepper.UI.RipGrepOptionsForm.pas' {RipGrepOptionsForm},
  RipGrepper.UI.SearchForm in 'src\RipGrepper.UI.SearchForm.pas' {RipGrepperSearchDialogForm},
  RipGrepper.CommandLine.Builder in 'src\RipGrepper.CommandLine.Builder.pas',
  RipGrepper.CommandLine.OptionHelper in 'src\RipGrepper.CommandLine.OptionHelper.pas',
  RipGrepper.Common.GuiSearchParams in 'src\RipGrepper.Common.GuiSearchParams.pas',
  RipGrepper.Common.IOTAUtils in 'src\RipGrepper.Common.IOTAUtils.pas',
  RipGrepper.Common.SearchParams in 'src\RipGrepper.Common.SearchParams.pas',
  RipGrepper.Common.Settings in 'src\RipGrepper.Common.Settings.pas',
  RipGrepper.Common.Settings.RipGrepperSettings in 'src\RipGrepper.Common.Settings.RipGrepperSettings.pas',
  RipGrepper.Common.Settings.RipGrepperViewSettings in 'src\RipGrepper.Common.Settings.RipGrepperViewSettings.pas',
  RipGrepper.Common.Settings.RipGrepperOpenWithSettings in 'src\RipGrepper.Common.Settings.RipGrepperOpenWithSettings.pas',
  RipGrepper.OpenWith.Params in 'src\OpenWith\RipGrepper.OpenWith.Params.pas';

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
