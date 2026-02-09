program HistoryEditTestProject;

uses
  Vcl.Forms,
  HistoryEditTest in 'HistoryEditTest.pas' {HistoryEditTestForm},
  RipGrepper.UI.HistoryButtonedEdit in 'src\UI\RipGrepper.UI.HistoryButtonedEdit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(THistoryEditTestForm, HistoryEditTestForm);
  Application.Run;
end.
