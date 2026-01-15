program ComponentTestApp;

uses
  Vcl.Forms,
  ComponentTestMain in 'ComponentTestMain.pas' {FormComponentTest};

{$R *.res}

begin
	Application.Initialize;
	Application.MainFormOnTaskbar := True;
	Application.CreateForm(TFormComponentTest, FormComponentTest);
  Application.Run;
end.
