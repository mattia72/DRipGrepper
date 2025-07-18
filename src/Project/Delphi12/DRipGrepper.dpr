program DRipGrepper;

{$R *.dres}

uses
	{$IFDEF FASTMM4_OPT}
	FastMM4,
	{$ENDIF }
	Vcl.Forms,
	Vcl.Themes,
	Vcl.Styles,
	dpipes in '..\..\..\external\dpipes.pas',
	dprocess in '..\..\..\external\dprocess.pas',
	ArrayEx in '..\..\..\external\ArrayEx.pas',
	u_dzDpiScaleUtils in '..\..\..\external\dzlib\u_dzDpiScaleUtils.pas',
	u_dzMiscUtils in '..\..\..\external\dzlib\u_dzMiscUtils.pas',
	u_dzTypesUtils in '..\..\..\external\dzlib\u_dzTypesUtils.pas',
	u_dzTypInfo in '..\..\..\external\dzlib\u_dzTypInfo.pas',
	u_dzVclUtils in '..\..\..\external\dzlib\u_dzVclUtils.pas',
	u_dzVariantUtils in '..\..\..\external\dzlib\u_dzVariantUtils.pas',
	u_dzTypes in '..\..\..\external\dzlib\u_dzTypes.pas',
	u_dzTranslator in '..\..\..\external\dzlib\u_dzTranslator.pas',
	u_dzStringUtils in '..\..\..\external\dzlib\u_dzStringUtils.pas',
	u_dzStringArrayUtils in '..\..\..\external\dzlib\u_dzStringArrayUtils.pas',
	u_dzSortUtils in '..\..\..\external\dzlib\u_dzSortUtils.pas',
	u_dzSortProvider in '..\..\..\external\dzlib\u_dzSortProvider.pas',
	u_dzQuicksort in '..\..\..\external\dzlib\u_dzQuicksort.pas',
	u_dzOsUtils in '..\..\..\external\dzlib\u_dzOsUtils.pas',
	u_dzNamedThread in '..\..\..\external\dzlib\u_dzNamedThread.pas',
	u_dzLineBuilder in '..\..\..\external\dzlib\u_dzLineBuilder.pas',
	u_dzDateUtils in '..\..\..\external\dzlib\u_dzDateUtils.pas',
	u_dzConvertUtils in '..\..\..\external\dzlib\u_dzConvertUtils.pas',
	u_dzClassUtils in '..\..\..\external\dzlib\u_dzClassUtils.pas',
	RipGrepper.Tools.DelphiVersions in '..\..\Tools\RipGrepper.Tools.DelphiVersions.pas',
	{$IFNDEF STANDALONE}
	RipGrepper.Common.IOTAUtils in '..\..\Common\RipGrepper.Common.IOTAUtils.pas',
	{$ENDIF }
	RipGrepper.Data.HistoryItemObject in '..\..\RipGrepper.Data.HistoryItemObject.pas',
	RipGrepper.Data.Matches in '..\..\RipGrepper.Data.Matches.pas',
	RipGrepper.Data.Parsers in '..\..\RipGrepper.Data.Parsers.pas',
	RipGrepper.Helper.Types in '..\..\RipGrepper.Helper.Types.pas',
	RipGrepper.Helper.UI in '..\..\RipGrepper.Helper.UI.pas',
	RipGrepper.OpenWith in '..\..\OpenWith\RipGrepper.OpenWith.pas',
	RipGrepper.OpenWith.CmdListForm in '..\..\OpenWith\RipGrepper.OpenWith.CmdListForm.pas' {OpenWithCmdList} ,
	RipGrepper.OpenWith.ConfigForm in '..\..\OpenWith\RipGrepper.OpenWith.ConfigForm.pas' {OpenWithConfigForm} ,
	RipGrepper.OpenWith.Runner in '..\..\OpenWith\RipGrepper.OpenWith.Runner.pas',
	RipGrepper.OpenWith.Constants in '..\..\OpenWith\RipGrepper.OpenWith.Constants.pas',
	RipGrepper.Parsers.Factory in '..\..\RipGrepper.Parsers.Factory.pas',
	RipGrepper.Parsers.ParallelParser in '..\..\RipGrepper.Parsers.ParallelParser.pas',
	RipGrepper.Parsers.VimGrepMatchLine in '..\..\RipGrepper.Parsers.VimGrepMatchLine.pas',
	RipGrepper.CommandLine.Builder in '..\..\RipGrepper.CommandLine.Builder.pas',
	RipGrepper.CommandLine.OptionHelper in '..\..\RipGrepper.CommandLine.OptionHelper.pas',
	RipGrepper.OpenWith.Params in '..\..\OpenWith\RipGrepper.OpenWith.Params.pas',
	RipGrepper.CommandLine.OptionStrings in '..\..\RipGrepper.CommandLine.OptionStrings.pas',
	RipGrepper.Common.Constants in '..\..\Common\RipGrepper.Common.Constants.pas',
	RipGrepper.Common.GuiSearchParams in '..\..\Common\RipGrepper.Common.GuiSearchParams.pas',
	RipGrepper.Common.Interfaces in '..\..\Common\RipGrepper.Common.Interfaces.pas',
	RipGrepper.Common.NodeData in '..\..\Common\RipGrepper.Common.NodeData.pas',
	RipGrepper.Common.ParsedObject in '..\..\Common\RipGrepper.Common.ParsedObject.pas',
	RipGrepper.Common.SearchParams in '..\..\Common\RipGrepper.Common.SearchParams.pas',
	RipGrepper.Common.SimpleTypes in '..\..\Common\RipGrepper.Common.SimpleTypes.pas',
	RipGrepper.Common.Sorter in '..\..\Common\RipGrepper.Common.Sorter.pas',
	RipGrepper.UI.MainForm in '..\..\UI\RipGrepper.UI.MainForm.pas' {RipGrepperForm} ,
	RipGrepper.UI.TopFrame in '..\..\UI\RipGrepper.UI.TopFrame.pas' {RipGrepperTopFrame: TFrame} ,
	RipGrepper.UI.BottomFrame in '..\..\UI\RipGrepper.UI.BottomFrame.pas' {RipGrepperBottomFrame: TFrame} ,
	RipGrepper.UI.MiddleFrame in '..\..\UI\RipGrepper.UI.MiddleFrame.pas' {RipGrepperMiddleFrame: TFrame} ,
	RipGrepper.UI.Settings.ConfigForm in '..\..\UI\RipGrepper.UI.Settings.ConfigForm.pas' {ConfigForm} ,
	RipGrepper.UI.DpiScaler in '..\..\UI\RipGrepper.UI.DpiScaler.pas',
	RipGrepper.UI.MiddleLeftFrame in '..\..\UI\RipGrepper.UI.MiddleLeftFrame.pas' {MiddleLeftFrame: TFrame} ,
	RipGrepper.UI.ParentFrame in '..\..\UI\RipGrepper.UI.ParentFrame.pas' {ParentFrame: TFrame} ,
	RipGrepper.UI.RipGrepOptionsForm in '..\..\UI\RipGrepper.UI.RipGrepOptionsForm.pas' {RipGrepOptionsForm} ,
	RipGrepper.UI.SearchForm in '..\..\UI\RipGrepper.UI.SearchForm.pas' {RipGrepperSearchDialogForm} ,
	RipGrepper.Tools.DebugUtils in '..\..\Tools\RipGrepper.Tools.DebugUtils.pas',
	RipGrepper.Tools.FileUtils in '..\..\Tools\RipGrepper.Tools.FileUtils.pas',
	RipGrepper.Tools.PackageInstall in '..\..\Tools\RipGrepper.Tools.PackageInstall.pas',
	RipGrepper.Tools.ProcessUtils in '..\..\Tools\RipGrepper.Tools.ProcessUtils.pas',
	RipGrepper.Settings.AppSettings in '..\..\Settings\RipGrepper.Settings.AppSettings.pas',
	RipGrepper.Settings.ExtensionSettings in '..\..\Settings\RipGrepper.Settings.ExtensionSettings.pas',
	RipGrepper.Settings.NodeLookSettings in '..\..\Settings\RipGrepper.Settings.NodeLookSettings.pas',
	RipGrepper.Settings.OpenWithSettings in '..\..\Settings\RipGrepper.Settings.OpenWithSettings.pas',
	RipGrepper.Settings.Persistable in '..\..\Settings\RipGrepper.Settings.Persistable.pas',
	RipGrepper.Settings.RipGrepParameterSettings in '..\..\Settings\RipGrepper.Settings.RipGrepParameterSettings.pas',
	RipGrepper.Settings.RipGrepperSettings in '..\..\Settings\RipGrepper.Settings.RipGrepperSettings.pas',
	RipGrepper.Settings.SearchFormSettings in '..\..\Settings\RipGrepper.Settings.SearchFormSettings.pas',
	RipGrepper.Settings.SettingsDictionary in '..\..\Settings\RipGrepper.Settings.SettingsDictionary.pas',
	RipGrepper.Settings.SettingVariant in '..\..\Settings\RipGrepper.Settings.SettingVariant.pas',
	RipGrepper.Settings.NodeLook.FilterSettings in '..\..\Settings\RipGrepper.Settings.NodeLook.FilterSettings.pas',
	RipGrepper.Tools.Replacer in '..\..\Tools\RipGrepper.Tools.Replacer.pas',
	RipGrepper.Tools.Replacer.StandaloneContext in '..\..\Tools\RipGrepper.Tools.Replacer.StandaloneContext.pas',
	RipGrepper.Common.EncodedStringList in '..\..\Common\RipGrepper.Common.EncodedStringList.pas',
	RipGrepper.UI.IFrameEvents in '..\..\UI\RipGrepper.UI.IFrameEvents.pas',
	RipGrepper.UI.Settings.AppSettingsForm in '..\..\UI\RipGrepper.UI.Settings.AppSettingsForm.pas' {AppSettingsForm} ,
	RipGrepper.UI.SettingsFormBase in '..\..\UI\RipGrepper.UI.SettingsFormBase.pas',
	RipGrepper.Settings.FontColors in '..\..\Settings\RipGrepper.Settings.FontColors.pas',
	RipGrepper.UI.ColorSelectorFrame in '..\..\UI\RipGrepper.UI.ColorSelectorFrame.pas' {ColorSelectorFrame: TFrame} ,
	RipGrepper.Tools.LockGuard in '..\..\Tools\RipGrepper.Tools.LockGuard.pas',
	RipGrepper.UI.Settings.ColorSettingsForm in '..\..\UI\RipGrepper.UI.Settings.ColorSettingsForm.pas',
	RipGrepper.UI.Settings.ExtensionSettingsForm in '..\..\UI\RipGrepper.UI.Settings.ExtensionSettingsForm.pas' {ExtensionSettingsForm} ,
	RipGrepper.Helper.MemIniFile in '..\..\Helper\RipGrepper.Helper.MemIniFile.pas',
	RipGrepper.Helper.UI.DarkMode in '..\..\RipGrepper.Helper.UI.DarkMode.pas',
	RipGrepper.Common.SearchTextWithOptions in '..\..\Common\RipGrepper.Common.SearchTextWithOptions.pas',
	RipGrepper.OpenWith.CmdEditorForm in '..\..\OpenWith\RipGrepper.OpenWith.CmdEditorForm.pas' {OpenWithCommandEditor} ,
	RipGrepper.Settings.FilePersister in '..\..\Settings\RipGrepper.Settings.FilePersister.pas',
	RipGrepper.Common.Interfaces.StreamPersistable in '..\..\Common\RipGrepper.Common.Interfaces.StreamPersistable.pas',
	RipGrepper.Settings.StreamPersister in '..\..\Settings\RipGrepper.Settings.StreamPersister.pas',
	RipGrepper.Settings.Persister.Interfaces in '..\..\Settings\RipGrepper.Settings.Persister.Interfaces.pas',
	RipGrepper.Helper.SettingStoreBehaviours in '..\..\Helper\RipGrepper.Helper.SettingStoreBehaviours.pas',
	RipGrepper.Settings.RipGrepArguments in '..\..\Settings\RipGrepper.Settings.RipGrepArguments.pas',
	RipGrepper.Helper.StreamReaderWriter in '..\..\Helper\RipGrepper.Helper.StreamReaderWriter.pas',
	RipGrepper.UI.Settings.AboutForm in '..\..\UI\RipGrepper.UI.Settings.AboutForm.pas',
	RipGrepper.Tools.ReleaseUtils in '..\..\Tools\RipGrepper.Tools.ReleaseUtils.pas',
	RipGrepper.Common.LoadHistoryMode in '..\..\Common\RipGrepper.Common.LoadHistoryMode.pas',
	RipGrepper.UI.Settings.AdvancedForm in '..\..\UI\RipGrepper.UI.Settings.AdvancedForm.pas';

{$R *.res}

begin
	{$IFDEF DEBUG}
	ReportMemoryLeaksOnShutdown := True;
	{$ENDIF}
	Application.Initialize;
	Application.MainFormOnTaskbar := True;
	TStyleManager.TrySetStyle('Windows10');
	Application.CreateForm(TRipGrepperForm, RipGrepperForm);
	Application.CreateForm(TOpenWithCommandEditor, OpenWithCommandEditor);
	Application.Run;

end.
