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
  u_DelphiVersions in 'external\dzlib\u_DelphiVersions.pas',
  RipGrepper.Data.HistoryItemObject in 'src\RipGrepper.Data.HistoryItemObject.pas',
  RipGrepper.Data.Matches in 'src\RipGrepper.Data.Matches.pas',
  RipGrepper.Data.Parsers in 'src\RipGrepper.Data.Parsers.pas',
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
  RipGrepper.CommandLine.Builder in 'src\RipGrepper.CommandLine.Builder.pas',
  RipGrepper.CommandLine.OptionHelper in 'src\RipGrepper.CommandLine.OptionHelper.pas',
  RipGrepper.OpenWith.Params in 'src\OpenWith\RipGrepper.OpenWith.Params.pas',
  RipGrepper.CommandLine.OptionStrings in 'src\RipGrepper.CommandLine.OptionStrings.pas',
  RipGrepper.Common.Constants in 'src\Common\RipGrepper.Common.Constants.pas',
  RipGrepper.Common.GuiSearchParams in 'src\Common\RipGrepper.Common.GuiSearchParams.pas',
  RipGrepper.Common.Interfaces in 'src\Common\RipGrepper.Common.Interfaces.pas',
  RipGrepper.Common.IOTAUtils in 'src\Common\RipGrepper.Common.IOTAUtils.pas',
  RipGrepper.Common.NodeData in 'src\Common\RipGrepper.Common.NodeData.pas',
  RipGrepper.Common.ParsedObject in 'src\Common\RipGrepper.Common.ParsedObject.pas',
  RipGrepper.Common.SearchParams in 'src\Common\RipGrepper.Common.SearchParams.pas',
  RipGrepper.Common.SimpleTypes in 'src\Common\RipGrepper.Common.SimpleTypes.pas',
  RipGrepper.Common.Sorter in 'src\Common\RipGrepper.Common.Sorter.pas',
  RipGrepper.UI.MainForm in 'src\UI\RipGrepper.UI.MainForm.pas' {RipGrepperForm},
  RipGrepper.UI.TopFrame in 'src\UI\RipGrepper.UI.TopFrame.pas' {RipGrepperTopFrame: TFrame},
  RipGrepper.UI.BottomFrame in 'src\UI\RipGrepper.UI.BottomFrame.pas' {RipGrepperBottomFrame: TFrame},
  RipGrepper.UI.MiddleFrame in 'src\UI\RipGrepper.UI.MiddleFrame.pas' {RipGrepperMiddleFrame: TFrame},
  RipGrepper.UI.ConfigForm in 'src\UI\RipGrepper.UI.ConfigForm.pas' {ConfigForm},
  RipGrepper.UI.DpiScaler in 'src\UI\RipGrepper.UI.DpiScaler.pas',
  RipGrepper.UI.MiddleLeftFrame in 'src\UI\RipGrepper.UI.MiddleLeftFrame.pas' {MiddleLeftFrame: TFrame},
  RipGrepper.UI.ParentFrame in 'src\UI\RipGrepper.UI.ParentFrame.pas' {ParentFrame: TFrame},
  RipGrepper.UI.RipGrepOptionsForm in 'src\UI\RipGrepper.UI.RipGrepOptionsForm.pas' {RipGrepOptionsForm},
  RipGrepper.UI.SearchForm in 'src\UI\RipGrepper.UI.SearchForm.pas' {RipGrepperSearchDialogForm},
  RipGrepper.Tools.DebugUtils in 'src\Tools\RipGrepper.Tools.DebugUtils.pas',
  RipGrepper.Tools.FileUtils in 'src\Tools\RipGrepper.Tools.FileUtils.pas',
  RipGrepper.Tools.PackageInstall in 'src\Tools\RipGrepper.Tools.PackageInstall.pas',
  RipGrepper.Tools.ProcessUtils in 'src\Tools\RipGrepper.Tools.ProcessUtils.pas',
  RipGrepper.Settings.AppSettings in 'src\Settings\RipGrepper.Settings.AppSettings.pas',
  RipGrepper.Settings.ExtensionSettings in 'src\Settings\RipGrepper.Settings.ExtensionSettings.pas',
  RipGrepper.Settings.Instance in 'src\Settings\RipGrepper.Settings.Instance.pas',
  RipGrepper.Settings.NodeLookSettings in 'src\Settings\RipGrepper.Settings.NodeLookSettings.pas',
  RipGrepper.Settings.OpenWithSettings in 'src\Settings\RipGrepper.Settings.OpenWithSettings.pas',
  RipGrepper.Settings.Persistable in 'src\Settings\RipGrepper.Settings.Persistable.pas',
  RipGrepper.Settings.RipGrepParameterSettings in 'src\Settings\RipGrepper.Settings.RipGrepParameterSettings.pas',
  RipGrepper.Settings.RipGrepperSettings in 'src\Settings\RipGrepper.Settings.RipGrepperSettings.pas',
  RipGrepper.Settings.SearchFormSettings in 'src\Settings\RipGrepper.Settings.SearchFormSettings.pas',
  RipGrepper.Settings.SettingsDictionary in 'src\Settings\RipGrepper.Settings.SettingsDictionary.pas',
  RipGrepper.Settings.SettingVariant in 'src\Settings\RipGrepper.Settings.SettingVariant.pas';

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
