program RipGrepper;

uses
  Vcl.Forms,
  RipGrepperForm in 'RipGrepperForm.pas' {RipGrepperForm} ,
  dpipes in 'dpipes.pas',
  dprocess in 'dprocess.pas',
  ProcessTools in 'ProcessTools.pas',
  DebugTools in 'DebugTools.pas',
  RipGrepperSettings in 'RipGrepperSettings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TRipGrepperForm, RipGrepperForm.Form1);
  Application.Run;

end.
