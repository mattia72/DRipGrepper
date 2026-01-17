program ComponentTestApp;

uses
  Vcl.Forms,
  ComponentTestMain in 'ComponentTestMain.pas' {FormComponentTest},
  RipGrepper.UI.Components.LabeledComboBox in '..\..\UI\Components\RipGrepper.UI.Components.LabeledComboBox.pas';

{$R *.res}

begin
	Application.Initialize;
	Application.MainFormOnTaskbar := True;
	Application.CreateForm(TFormComponentTest, FormComponentTest);
  Application.Run;
end.
