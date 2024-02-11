// https://www.gexperts.org/open-tools-api-faq/#sample
unit DRipExtension;

interface

uses ToolsAPI;

type
  // TNotifierObject has stub implementations for the necessary but
  // unused IOTANotifer methods
  TDRipExtension = class(TNotifierObject, IOTAMenuWizard, IOTAWizard)
  public
    // IOTAWizard interafce methods(required for all wizards/experts)
	function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTAMenuWizard (creates a simple menu item on the help menu)
    function GetMenuText: string;
  end;

procedure Register;

implementation

uses Dialogs,
RipGrepper.UI.MainForm;

procedure Register;
begin
  RegisterPackageWizard(TDRipExtension.Create);
end;

procedure TDRipExtension.Execute;
begin
   TRipGrepperForm.CreateAndShow(nil);
end;

function TDRipExtension.GetIDString: string;
begin
  Result := 'EB.DRipExtension';
end;

function TDRipExtension.GetMenuText: string;
begin
  Result := '&' + GetName();
end;

function TDRipExtension.GetName: string;
begin
  Result := 'DRip Extensions';
end;

function TDRipExtension.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

end.
