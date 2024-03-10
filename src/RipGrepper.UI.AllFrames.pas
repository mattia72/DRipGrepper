unit RipGrepper.UI.AllFrames;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RipGrepper.UI.TopFrame, RipGrepper.UI.MainFrame,
  RipGrepper.UI.BottomFrame;

type
  TFrames = class(TFrame)
    BottomFrame: TRipGrepperBottomFrame;
    MainFrame: TRipGrepperMainFrame;
    TopFrame: TRipGrepperTopFrame;
  private
    { Private-Deklarationen }
  public
	constructor Create(AOwner: TComponent);
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

constructor TFrames.Create(AOwner: TComponent);
begin
	inherited;

end;

end.
