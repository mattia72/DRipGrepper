library DRipExtensions;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters.

  Important note about VCL usage: when this DLL will be implicitly
  loaded and this DLL uses TWicImage / TImageCollection created in
  any unit initialization section, then Vcl.WicImageInit must be
  included into your library's USES clause. }

{$R *.dres}

uses
	ShareMem,
	System.SysUtils,
	System.Classes,
	ToolsAPI,
	RipGrepper.Tools.DebugUtils in '..\..\..\..\src\Tools\RipGrepper.Tools.DebugUtils.pas',
	RipGrepper.UI.SettingsFormBase in '..\..\..\..\src\UI\RipGrepper.UI.SettingsFormBase.pas',
	RipGrepper.CommandLine.Builder in '..\..\..\..\src\RipGrepper.CommandLine.Builder.pas',
	RipGrepper.CommandLine.OptionHelper in '..\..\..\..\src\RipGrepper.CommandLine.OptionHelper.pas',
	RipGrepper.Data.HistoryItemObject in '..\..\..\..\src\RipGrepper.Data.HistoryItemObject.pas',
	RipGrepper.Data.Matches in '..\..\..\..\src\RipGrepper.Data.Matches.pas',
	RipGrepper.Data.Parsers in '..\..\..\..\src\RipGrepper.Data.Parsers.pas',
	RipGrepper.Helper.Types in '..\..\..\..\src\RipGrepper.Helper.Types.pas',
	RipGrepper.Helper.UI in '..\..\..\..\src\RipGrepper.Helper.UI.pas',
	RipGrepper.Parsers.Factory in '..\..\..\..\src\RipGrepper.Parsers.Factory.pas',
	RipGrepper.Parsers.ParallelParser in '..\..\..\..\src\RipGrepper.Parsers.ParallelParser.pas',
	RipGrepper.Parsers.VimGrepMatchLine in '..\..\..\..\src\RipGrepper.Parsers.VimGrepMatchLine.pas',
	RipGrepper.OpenWith.CmdListForm in '..\..\..\..\src\OpenWith\RipGrepper.OpenWith.CmdListForm.pas' {OpenWithCmdList} ,
	RipGrepper.OpenWith.ConfigForm in '..\..\..\..\src\OpenWith\RipGrepper.OpenWith.ConfigForm.pas' {OpenWithConfigForm} ,
	RipGrepper.OpenWith in '..\..\..\..\src\OpenWith\RipGrepper.OpenWith.pas',
	RipGrepper.OpenWith.Runner in '..\..\..\..\src\OpenWith\RipGrepper.OpenWith.Runner.pas',
	RipGrepper.OpenWith.Constants in '..\..\..\..\src\OpenWith\RipGrepper.OpenWith.Constants.pas',
	RipGrepper.OpenWith.Params in '..\..\..\..\src\OpenWith\RipGrepper.OpenWith.Params.pas',
	ArrayEx in '..\..\..\..\external\ArrayEx.pas',
	dpipes in '..\..\..\..\external\dpipes.pas',
	dprocess in '..\..\..\..\external\dprocess.pas',
	RipGrepper.CommandLine.OptionStrings in '..\..\..\..\src\RipGrepper.CommandLine.OptionStrings.pas',
	RipGrepper.UI.BottomFrame in '..\..\..\..\src\UI\RipGrepper.UI.BottomFrame.pas' {RipGrepperBottomFrame: TFrame} ,
	RipGrepper.UI.Settings.ConfigForm in '..\..\..\..\src\UI\RipGrepper.UI.Settings.ConfigForm.pas' {ConfigForm} ,
	RipGrepper.UI.DpiScaler in '..\..\..\..\src\UI\RipGrepper.UI.DpiScaler.pas',
	RipGrepper.UI.MainForm in '..\..\..\..\src\UI\RipGrepper.UI.MainForm.pas' {RipGrepperForm} ,
	RipGrepper.UI.MiddleFrame in '..\..\..\..\src\UI\RipGrepper.UI.MiddleFrame.pas' {RipGrepperMiddleFrame: TFrame} ,
	RipGrepper.UI.MiddleLeftFrame in '..\..\..\..\src\UI\RipGrepper.UI.MiddleLeftFrame.pas' {MiddleLeftFrame: TFrame} ,
	RipGrepper.UI.ParentFrame in '..\..\..\..\src\UI\RipGrepper.UI.ParentFrame.pas' {ParentFrame: TFrame} ,
	RipGrepper.UI.RipGrepOptionsForm in '..\..\..\..\src\UI\RipGrepper.UI.RipGrepOptionsForm.pas' {RipGrepOptionsForm} ,
	RipGrepper.UI.SearchForm in '..\..\..\..\src\UI\RipGrepper.UI.SearchForm.pas' {RipGrepperSearchDialogForm} ,
	RipGrepper.UI.TopFrame in '..\..\..\..\src\UI\RipGrepper.UI.TopFrame.pas' {RipGrepperTopFrame: TFrame} ,
	RipGrepper.Common.Constants in '..\..\..\..\src\Common\RipGrepper.Common.Constants.pas',
	RipGrepper.Common.GuiSearchParams in '..\..\..\..\src\Common\RipGrepper.Common.GuiSearchParams.pas',
	RipGrepper.Common.Interfaces in '..\..\..\..\src\Common\RipGrepper.Common.Interfaces.pas',
	RipGrepper.Common.IOTAUtils in '..\..\..\..\src\Common\RipGrepper.Common.IOTAUtils.pas',
	RipGrepper.Common.NodeData in '..\..\..\..\src\Common\RipGrepper.Common.NodeData.pas',
	RipGrepper.Common.ParsedObject in '..\..\..\..\src\Common\RipGrepper.Common.ParsedObject.pas',
	RipGrepper.Common.SearchParams in '..\..\..\..\src\Common\RipGrepper.Common.SearchParams.pas',
	RipGrepper.Common.SimpleTypes in '..\..\..\..\src\Common\RipGrepper.Common.SimpleTypes.pas',
	RipGrepper.Common.Sorter in '..\..\..\..\src\Common\RipGrepper.Common.Sorter.pas',
	RipGrepper.Common.SyncObjLock in '..\..\..\..\src\Common\RipGrepper.Common.SyncObjLock.pas',
	RipGrepper.Tools.FileUtils in '..\..\..\..\src\Tools\RipGrepper.Tools.FileUtils.pas',
	RipGrepper.Tools.PackageInstall in '..\..\..\..\src\Tools\RipGrepper.Tools.PackageInstall.pas',
	RipGrepper.Tools.ProcessUtils in '..\..\..\..\src\Tools\RipGrepper.Tools.ProcessUtils.pas',
	RipGrepper.Settings.AppSettings in '..\..\..\..\src\Settings\RipGrepper.Settings.AppSettings.pas',
	RipGrepper.Settings.ExtensionSettings in '..\..\..\..\src\Settings\RipGrepper.Settings.ExtensionSettings.pas',
	RipGrepper.Settings.NodeLookSettings in '..\..\..\..\src\Settings\RipGrepper.Settings.NodeLookSettings.pas',
	RipGrepper.Settings.OpenWithSettings in '..\..\..\..\src\Settings\RipGrepper.Settings.OpenWithSettings.pas',
	RipGrepper.Settings.Persistable in '..\..\..\..\src\Settings\RipGrepper.Settings.Persistable.pas',
	RipGrepper.Settings.RipGrepParameterSettings in '..\..\..\..\src\Settings\RipGrepper.Settings.RipGrepParameterSettings.pas',
	RipGrepper.Settings.RipGrepperSettings in '..\..\..\..\src\Settings\RipGrepper.Settings.RipGrepperSettings.pas',
	RipGrepper.Settings.SearchFormSettings in '..\..\..\..\src\Settings\RipGrepper.Settings.SearchFormSettings.pas',
	RipGrepper.Settings.SettingsDictionary in '..\..\..\..\src\Settings\RipGrepper.Settings.SettingsDictionary.pas',
	RipGrepper.Settings.SettingVariant in '..\..\..\..\src\Settings\RipGrepper.Settings.SettingVariant.pas',
	RipGrepper.Tools.Replacer in '..\..\..\..\src\Tools\RipGrepper.Tools.Replacer.pas',
	RipGrepper.Common.EncodedStringList in '..\..\..\..\src\Common\RipGrepper.Common.EncodedStringList.pas',
	RipGrepper.Settings.NodeLook.FilterSettings in '..\..\..\..\src\Settings\RipGrepper.Settings.NodeLook.FilterSettings.pas',
	RipGrepper.Tools.Replacer.ExtensionContext in '..\..\..\..\src\Tools\RipGrepper.Tools.Replacer.ExtensionContext.pas',
	RipGrepper.UI.IFrameEvents in '..\..\..\..\src\UI\RipGrepper.UI.IFrameEvents.pas',
	RipGrepper.Settings.FontColors in '..\..\..\..\src\Settings\RipGrepper.Settings.FontColors.pas',
	RipGrepper.UI.Settings.AppSettingsForm in '..\..\..\..\src\UI\RipGrepper.UI.Settings.AppSettingsForm.pas',
	RipGrepper.UI.ColorSelectorFrame in '..\..\..\..\src\UI\RipGrepper.UI.ColorSelectorFrame.pas',
	RipGrepper.Tools.LockGuard in '..\..\..\..\src\Tools\RipGrepper.Tools.LockGuard.pas',
	RipGrepper.UI.Settings.ColorSettingsForm in '..\..\..\..\src\UI\RipGrepper.UI.Settings.ColorSettingsForm.pas',
	RipGrepper.UI.Settings.ExtensionSettingsForm in '..\..\..\..\src\UI\RipGrepper.UI.Settings.ExtensionSettingsForm.pas',
	RipGrepper.Tools.DelphiVersions in '..\..\..\..\src\Tools\RipGrepper.Tools.DelphiVersions.pas',
	DRipExtension.Main in '..\..\DRipExtension.Main.pas',
	DripExtension.UI.DockableForm in '..\..\DripExtension.UI.DockableForm.pas',
	DRipExtension.Menu in '..\..\DRipExtension.Menu.pas';

{$R *.res}

exports
	InitWizard name WizardEntryPoint;

begin
	GSettings := TRipGrepperSettings.Create;

	GSettings.AppSettings.ReadIni;
	GSettings.AppSettings.LoadFromDict();
	TDebugUtils.UpdateTraceActive;
	// GSettings.Free in TDripExtensionMenu.Destroy
end.
