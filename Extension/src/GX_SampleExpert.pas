{***************************************************************
 * Unit Name: GX_SampleExpert
 * Purpose  : Template for creation of new GExperts experts
 *          : Customize this when creating new experts
 *          : Add this unit to the GExperts .dpr and compile
 * Authors  : Stefan Hoffmeister, Scott Mattes, Erik Berry
 ****************************************************************}

unit GX_SampleExpert;

{$I GX_CondDefine.inc}

interface

uses
  GX_Experts, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, GX_BaseForm;

type
  TfmGxSampleExpertForm = class(TfmBaseForm)
    btnOK: TButton;
    lblNote: TLabel;
    lblData: TLabel;
    edtData: TEdit;
    btnCancel: TButton;
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Registry, Menus, GX_GExperts, GX_ConfigurationInfo;

type
  ///<summary>
  /// Use this sample expert as a starting point for your own expert.
  /// Do not forget to rename the unit and the class(es).
  /// Many of the methods are optional and you can omit them if the default
  /// behaviour is suitable for your expert. </summary>
  TGxSampleExpert = class(TGX_Expert)
  private
    FSomeData: string;
  protected
    procedure SetActive(New: Boolean); override;
  public
    // optional, defauls to true
    function CanHaveShortCut: boolean; override;
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    // optional, defaults to no shortcut
    function GetDefaultShortCut: TShortCut; override;
    // optional, but recommended
    function GetHelpString: string; override;
    // optional, defaults to true
    function HasConfigOptions: Boolean; override;
    // optional, defaults to true
    function HasMenuItem: Boolean; override;
    // optional if HasConfigOptions returns false
    procedure Configure; override;
    // Override to load any configuration settings
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    // Override to save any configuration settings
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
    procedure Execute(Sender: TObject); override;
  end;

{ TGxSampleExpert }

//*********************************************************
//    Name: TGxSampleExpert.Execute
// Purpose: The action taken when the menu item is clicked
//*********************************************************
procedure TGxSampleExpert.Execute(Sender: TObject);
begin
  with TfmGxSampleExpertForm.Create(nil) do
  try
    edtData.Text := FSomeData;
    if ShowModal = mrOk then
    begin
      FSomeData := edtData.Text;
      SaveSettings;
    end;
  finally
    Free;
  end;
end;

//*********************************************************
//    Name: TGxSampleExpert.CanHaveShortCut
// Purpose: Determines whether this expert can have a
//          hotkey assigned to it
//    Note: If it returns false, no hotkey configuration
//          control is shown in the configuratoin dialog.
//          If your expert can have a hotkey, you can
//          simply delete this function, since the
//          inherited funtion already returns true.
//*********************************************************
function TGxSampleExpert.CanHaveShortCut: boolean;
begin
  Result := True;
end;

//*********************************************************
//    Name: TGxSampleExpert.Configure
// Purpose: Action taken when user clicks the Configure
//          button on the Experts tab of menu item GExperts/
//          GExperts Configuration...
//*********************************************************
procedure TGxSampleExpert.Configure;
resourcestring
  SYouClickedConfigure = 'You clicked the Configuration button!';
begin
  MessageDlg(SYouClickedConfigure, mtInformation, [mbOK], 0);
end;

constructor TGxSampleExpert.Create;
begin
  inherited Create;

  // Assign a default value to your data
  FSomeData := 'Sample Data';

  // Saved settings are loaded automatically for you by the ancestor
  // via the virtual LoadSettings method
end;

destructor TGxSampleExpert.Destroy;
begin
  // Free any created objects here
  inherited Destroy;
end;

//*********************************************************
//    Name: TGxSampleExpert.GetActionCaption
// Purpose: Returns the string displayed on the GExperts
//          menu item
//*********************************************************
function TGxSampleExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Sample E&xpert...';
begin
  Result := SMenuCaption;
end;

//*********************************************************
//    Name: TGxSampleExpert.GetDefaultShortCut
// Purpose: The default shortcut to call your expert.
//   Notes: It is perfectly fine not to assign a default
//          shortcut and let the expert be called via
//          the menu only. The user can always assign
//          a shortcut to it in the configuration dialog.
//          Available shortcuts have become a very rare
//          resource in the Delphi IDE.
//          The value of ShortCut is touchy, use the ShortCut
//          button on the Editor Experts tab of menu item
//          GExperts/GExperts Configuration... on an existing
//          editor expert to see if you can use a specific
//          combination for your expert.
//*********************************************************
function TGxSampleExpert.GetDefaultShortCut: TShortCut;
begin
  // If desired, assign a default menu item shortcut
  Result := Menus.ShortCut(Word('Z'), [ssCtrl, ssShift, ssAlt]);
end;

//*********************************************************
//    Name: TGxSampleExpert.GetHelpString
// Purpose: To provide your text on what this expert
//          does to the expert description hint that is shown
//          when the user puts the mouse over the expert's icon
//          in the configuration dialog.
//*********************************************************
function TGxSampleExpert.GetHelpString: string;
resourcestring
  SSampleExpertHelp =
    'This is the text that will appear in the explanation hint on the ' +
    'Editor tab of menu item GExperts/GExperts Configuration...' + sLineBreak +
    sLineBreak +
    'You can allow the text to wrap automatically, or you can force your ' +
    'own line breaks, or both.';
begin
  Result := SSampleExpertHelp;
end;

//*********************************************************
//    Name: TGxSampleExpert.HasConfigOptions
// Purpose: This expert should have a configure button in
//          the configuration dialog
//*********************************************************
function TGxSampleExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

//*********************************************************
//    Name: TGxSampleExpert.HasMenuItem
// Purpose: This expert has a visible menu item
//          in the GExperts top level menu
//*********************************************************
function TGxSampleExpert.HasMenuItem: Boolean;
begin
  Result := True;
end;

//*********************************************************
//    Name: TGxSampleExpert.LoadSettings
// Purpose: Gets the expert settings from the registry
//*********************************************************
procedure TGxSampleExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited;
  FSomeData := _Settings.ReadString('TestSetting', FSomeData);
end;

//*********************************************************
//    Name: TGxSampleExpert.SaveSettings
// Purpose: Saves the expert settings to the registry
// You must call this manually in the destructor
//*********************************************************
procedure TGxSampleExpert.InternalSaveSettings(_Settings: IExpertSettings); 
begin
  inherited;
  _Settings.WriteString('TestSetting', FSomeData);
end;

//********************************************************************
//    Name: TGxSampleExpert.SetActive
// Purpose: Called to clean up the expert when it is disabled at runtime
// or destroyed on shutdown.  Free any forms and objects here.
// The ancestor removes the menu item for you.
//********************************************************************
procedure TGxSampleExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Initialize anything necessary here (generally empty for modal experts)
    else
    begin
      // If we had a non-modal form, it would be destroyed here
      //FreeAndNil(FSampleForm);
    end;
  end;
end;

//*********************************************************
// Purpose: Lets GExperts know about this expert
//*********************************************************
initialization
  RegisterGX_Expert(TGxSampleExpert);
end.
