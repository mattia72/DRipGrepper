program TabSeparatedConfigTestProject;

uses
  Vcl.Forms,
  TabSeparatedConfigTestForm in 'TabSeparatedConfigTestForm.pas' {TabSeparatedConfigTestMainForm},
  RipGrepper.UI.TabSeparatedConfigForm in '..\src\UI\RipGrepper.UI.TabSeparatedConfigForm.pas' {TabSeparatedConfigForm},
  RipGrepper.UI.SettingsFormBase in '..\src\UI\RipGrepper.UI.SettingsFormBase.pas' {SettingsBaseForm},
  RipGrepper.Settings.Persistable in '..\src\Settings\RipGrepper.Settings.Persistable.pas',
  RipGrepper.UI.DpiScaler in '..\src\UI\RipGrepper.UI.DpiScaler.pas',
  RipGrepper.Helper.UI.DarkMode in '..\src\RipGrepper.Helper.UI.DarkMode.pas',
  RipGrepper.Common.Constants in '..\src\Common\RipGrepper.Common.Constants.pas',
  RipGrepper.Helper.UI in '..\src\RipGrepper.Helper.UI.pas',
  RipGrepper.Tools.DebugUtils in '..\src\Tools\RipGrepper.Tools.DebugUtils.pas',
  RipGrepper.Tools.FileUtils in '..\src\Tools\RipGrepper.Tools.FileUtils.pas',
  ArrayEx in '..\external\ArrayEx.pas',
  RipGrepper.Settings.PersistableArray in '..\src\Settings\RipGrepper.Settings.PersistableArray.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TTabSeparatedConfigTestMainForm, TabSeparatedConfigTestMainForm);
  Application.Run;
end.
