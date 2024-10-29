unit GX_FeedbackWizard;

// Original Authors:
//   Stig Jørgensen <stig.joergensen@clearsky.dk> and
//   Erik Berry <eberry@gexperts.org>

interface

{$I GX_CondDefine.inc}
{$WARN SYMBOL_PLATFORM OFF}

uses
  Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, ActnList,
  GX_BaseForm, Actions, ShellAPI;

type
  TFeedbackType = (fbBug, fbFeature);

  TfmFeedbackWizard = class(TfmBaseForm)
    pnlTop: TPanel;
    pnlHeader: TPanel;
    lblFeeedback: TLabel;
    pgeMain: TPageControl;
    lblDescription: TLabel;
    tshType: TTabSheet;
    pnlContent: TPanel;
    pnlButtons: TPanel;
    btnCancel: TButton;
    btnPrev: TButton;
    btnNext: TButton;
    pgeInfo: TPageControl;
    tshInfoHelp: TTabSheet;
    tshInfoExample: TTabSheet;
    Splitter: TSplitter;
    tshDescription: TTabSheet;
    mmoDescription: TMemo;
    mmoInfoHelp: TMemo;
    mmoInfoExample: TMemo;
    pnlButtonsCenter: TPanel;
    tshBugDetails: TTabSheet;
    tshBugSteps: TTabSheet;
    mmoBugSteps: TMemo;
    tshReport: TTabSheet;
    mmoReport: TMemo;
    pnlReportButtons: TPanel;
    btnCopy: TButton;
    btnEmail: TButton;
    btnSave: TButton;
    tshConfiguration: TTabSheet;
    gbxConfigurationData: TGroupBox;
    chkOS: TCheckBox;
    chkExperts: TCheckBox;
    chkIdeVer: TCheckBox;
    chkPackages: TCheckBox;
    chkLocaleKeyboard: TCheckBox;
    chkCpu: TCheckBox;
    chkGExpertsVer: TCheckBox;
    gbxBugDetails: TGroupBox;
    chkProjectSpecific: TCheckBox;
    chkMultipleMachines: TCheckBox;
    lblPercent: TLabel;
    edtPercent: TEdit;
    chkReproducible: TCheckBox;
    Actions: TActionList;
    actPrev: TAction;
    actNext: TAction;
    dlgSaveReport: TSaveDialog;
    pnlDescription: TPanel;
    pnlMain: TPanel;
    chkGExpertsSettings: TCheckBox;
    l_BugReportUrl: TLabel;
    rb_BugReport: TRadioButton;
    rb_FeatureRequest: TRadioButton;
    l_FeatureUrl: TLabel;
    l_Verify: TLabel;
    l_URL: TLabel;
    procedure actPrevUpdate(Sender: TObject);
    procedure actNextUpdate(Sender: TObject);
    procedure actPrevExecute(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure chkReproducibleClick(Sender: TObject);
    procedure rgpFeedbackTypeClick(Sender: TObject);
    procedure btnEmailClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure pgeMainResize(Sender: TObject);
    procedure pnlButtonsResize(Sender: TObject);
    procedure edtPercentKeyPress(Sender: TObject; var Key: Char);
    procedure InfoMemoEnter(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FActiveTab: TTabSheet;
    FActiveTabNumber: Integer;
    TabsNotForBugs: TList;
    TabsNotForFeatures: TList;
    FIDEName: string;
    FBugEmail: string;
    FFeatureEmail: string;
    procedure SetDescriptionCaption;
    function GetFeedbackType: TFeedbackType;
    procedure SetDescriptionInfo;
    procedure SetExampleInfo;
    procedure UpdateInfo;
    property FeedbackType: TFeedbackType read GetFeedbackType;
    procedure UpdateForNewTab;
    procedure SetTabCaptions;
    procedure SetDefaultConfigurationData;
    function GetNextTab: TTabSheet;
    function GetPrevTab: TTabSheet;
    function OnLastPage: Boolean;
    function OnFirstPage: Boolean;
    function CanProceed: Boolean;
    procedure SetTabFocusControl;
    function IsValidTab(const Tab: TTabSheet): Boolean;
    procedure CenterTabContents;
    procedure CenterInParent(Control: TControl);
    procedure InitializeTabControls;
    procedure GenerateReport;
    function GetDestinationEmail: string;
    function GetFeedbackTypeString: string;
    function GetTabReportText(Tab: TTabSheet): string;
    function GetBugDetailsString: string;
    function GetSystemConfigurationString: string;
    procedure Init(const ABugEmail, AFeatureEmail: string);
  public
    class procedure Execute(AOwner: TComponent; const ABugEmail, AFeatureEmail: string);
    constructor Create(_Owner: TComponent); override;
  end;

implementation

uses Windows, SysUtils, TypInfo, Clipbrd, Registry,
  GX_GenericUtils, GX_ConfigurationInfo, GX_OtaUtils, GX_GetIdeVersion, GX_IdeUtils,
  Menus, Math, Graphics, u_dzVclUtils;

{$R *.dfm}

const
  SkipLine = sLineBreak + sLineBreak;
  CRLF = sLineBreak;

const
  SItemOfItem = ' (%d of %d)';

function GetLocaleChar(Locale, LocaleType: Integer; Default: Char): Char;
begin
  Result := SysUtils.GetLocaleChar(Locale, LocaleType, Default);
end;

function GetLocaleStr(Locale, LocaleType: Integer; const Default: string): string;
begin
  Result := SysUtils.GetLocaleStr(Locale, LocaleType, Default);
end;

{ TfmFeedbackWizard }

class procedure TfmFeedbackWizard.Execute(AOwner: TComponent; const ABugEmail, AFeatureEmail: string);
var
  Form: TfmFeedbackWizard;
begin
  Form := TfmFeedbackWizard.Create(AOwner);
  Form.Init(ABugEmail, AFeatureEmail);
  Form.Show; // Non-modal, this frees itself using caFree Action
end;

procedure TfmFeedbackWizard.Init(const ABugEmail, AFeatureEmail: string);
begin
  FBugEmail := ABugEmail;
  FFeatureEmail := AFeatureEmail;
  TLabel_MakeUrlLabel(l_BugReportUrl, FBugEmail, True);
  TLabel_MakeUrlLabel(l_FeatureUrl, FFeatureEmail, True)
end;

procedure TfmFeedbackWizard.SetDescriptionCaption;
begin
  if FActiveTab = nil then
    lblDescription.Caption := ''
  else if FActiveTab = tshType then
    lblDescription.Caption := FActiveTab.Caption
  else if FeedbackType = fbFeature then
    lblDescription.Caption := FActiveTab.Caption +
      Format(SItemOfItem, [FActiveTabNumber, pgeMain.PageCount - TabsNotForFeatures.Count])
  else
    lblDescription.Caption := FActiveTab.Caption +
      Format(SItemOfItem, [FActiveTabNumber, pgeMain.PageCount - TabsNotForBugs.Count]);
end;

function TfmFeedbackWizard.GetFeedbackType: TFeedbackType;
begin
  if rb_BugReport.Checked then
    Result := fbBug
  else
    Result := fbFeature;
end;

procedure TfmFeedbackWizard.SetDescriptionInfo;
const
  TypeDescription =
    '  Bug reports include access violations, crashes, ' +
    'non-working features, and other failures.  ' + sLineBreak + sLineBreak +
    '  Feature requests are suggestions for future development ' +
    'of GExperts.' + sLineBreak + sLineBreak +
    '  Please verify you are using the latest GExperts release before ' +
    'sending in feedback.  See: https://gexperts.dummzeuch.de/';
  BugDescriptionDescription =
    '  Please enter a detailed description of the bug.  Include any unique ' +
    'data about your system setup that you think might assist us in ' +
    'duplicating the bug.' + sLineBreak + sLineBreak +
    '  The exact steps necessary to duplicate the bug and general data such ' +
    'as the version of your IDE and operating system will be collected later.';
  FeatureDescriptionDescription =
    '  Please enter a detailed description of the feature you are proposing.  ' +
    'Be sure to state why/when the feature would be useful to you.';
  DetailsDescription =
    '  It is important to try to find repeatable steps that can reproduce ' +
    'a bug before reporting it.  Please indicate if you have been able to ' +
    'find reproducible steps and how regularly the bug is seen using those ' +
    'steps.  Also indicate if you have verified that the problem happens ' +
    'on other machines or only happens with specific projects.';
  StepsDescription =
    '  It is critical to enter a detailed list of steps to help us ' +
    'duplicate the problem.  Your steps should begin with a fresh startup ' +
    'of the IDE and be as specific as possible.  Include mouse clicks, ' +
    'keypresses, windows currently visible/active, and exact error messages ' +
    'where appropriate.' + sLineBreak + sLineBreak +
    '  If possible, try to duplicate problems with the default (empty) project, ' +
    'a simple project, or one of the IDE''s included demo projects.';
  ConfigurationDescription1 =
    '  All the items you check here will be sent with your report to help us ';
  ConfigurationDescription2 =
    '  To ensure an effective report, we strongly recommend you leave the ' +
    'default items checked.  You may edit or remove any of the configuration ' +
    'information in a moment (before reporting anything to us).';
  BugConfigurationDescription = ConfigurationDescription1 +
    'duplicate the bug and determine the cause.' + ConfigurationDescription2;
  FeatureConfigurationDescription = ConfigurationDescription1 +
    'determine when the requested feature would be helpful.' + ConfigurationDescription2;
  ReportDescription =
    '  Click Finish to copy the generated report to the clipboard and ' +
    'open your web browser where you can then ' +
    'paste the report into a bug report or feature request ticket.' + sLineBreak + sLineBreak +
    '  You may also save the generated report to a file and send it at a ' +
    'later date to %s.' + sLineBreak + sLineBreak +
    '  This wizard does not directly transmit any information to us.';
var
  Lines: TStrings;
begin
  Lines := mmoInfoHelp.Lines;
  Lines.Text := '';

  if FActiveTab = tshType then
    Lines.Text := TypeDescription
  else if FActiveTab = tshDescription then
  begin
    if FeedbackType = fbBug then
      Lines.Text := BugDescriptionDescription
    else
      Lines.Text := FeatureDescriptionDescription;
  end
  else if FActiveTab = tshBugDetails then
    Lines.Text := DetailsDescription
  else if FActiveTab = tshBugSteps then
    Lines.Text := StepsDescription
  else if FActiveTab = tshConfiguration then
  begin
    if FeedbackType = fbBug then
      Lines.Text := BugConfigurationDescription
    else
      Lines.Text := FeatureConfigurationDescription;
  end
  else if FActiveTab = tshReport then
    Lines.Text := Format(ReportDescription, [GetDestinationEmail]);
end;

procedure TfmFeedbackWizard.SetExampleInfo;
const
  TypeExample =
    '  Some examples of features we do NOT have any plans to ' +
    'implement include method folding, custom painting inside the editor, support for ' +
    'Visual C++ and Java, etc.';
  BugDescriptionExample =
    'The editor toolbar is shown without the separators when the toolbar ' +
    'is aligned to the left or right of the editor window.  The ' +
    'separators appear fine when the toolbar is aligned to the ' +
    'top or bottom. I have about 15 toolbar buttons and 4 separators ' +
    'in my editor toolbar.';
  FeatureDescriptionExample =
    'I would love to see a new editor expert that translates my code from %s ' +
    'to Visual Basic so I can write Outlook email viruses.  I would '+
    'also like to offer you a job that includes a huge salary and lots of ' +
    'paid vacation time.';
  StepsExample =
    '1. Open %s from the start menu' + sLineBreak +
    '2. Turn on the GExperts editor toolbar from the GExperts Configuration dialog' + sLineBreak +
    '3. Align the toolbar to the left' + sLineBreak +
    '4. Click the Customize Toolbar button' + sLineBreak +
    '5. Add 20 toolbar actions of any type with separators every 5 actions' + sLineBreak +
    '6. Save the GExperts settings and look at the editor window' + sLineBreak +
    '7. The separators are missing from the toolbar' + sLineBreak +
    '8. Re-align the toolbar to the Top, and the separators appear again';
var
  Lines: TStrings;
begin
  Lines := mmoInfoExample.Lines;
  Lines.Text := '';

  if FActiveTab = tshType then
    Lines.Text := TypeExample
  else if FActiveTab = tshDescription then
  begin
    if FeedbackType = fbBug then
      Lines.Text := BugDescriptionExample
    else
      Lines.Text := Format(FeatureDescriptionExample, [FIDEName]);
  end
  else if FActiveTab = tshBugSteps then
    Lines.Text := Format(StepsExample, [FIDEName]);
end;

procedure TfmFeedbackWizard.actPrevUpdate(Sender: TObject);
begin
  actPrev.Enabled := not OnFirstPage;
end;

procedure TfmFeedbackWizard.actNextUpdate(Sender: TObject);
const
  SFinish = '&Finish';
  SNext = '&Next  >';
begin
  actNext.Enabled := CanProceed;
  if OnLastPage then begin
    actNext.Caption := SFinish;
  end else
    actNext.Caption := SNext;
end;

procedure TfmFeedbackWizard.actPrevExecute(Sender: TObject);
begin
  FActiveTab := GetPrevTab;
  UpdateForNewTab;
  pgeMain.ActivePage := FActiveTab;
  SetTabFocusControl;
end;

procedure TfmFeedbackWizard.actNextExecute(Sender: TObject);
begin
  if OnLastPage then begin
    btnEmail.Click;
  end else begin
    FActiveTab := GetNextTab;
    UpdateForNewTab;
    pgeMain.ActivePage := FActiveTab;
    SetTabFocusControl;
  end;
end;

constructor TfmFeedbackWizard.Create(_Owner: TComponent);
var
  i: Integer;
begin
  inherited;

  TLabel_MakeUrlLabel(l_URL);

  for i := 0 to pgeMain.PageCount - 1 do
    pgeMain.Pages[i].TabVisible := False;
  pgeMain.ActivePage := pgeMain.Pages[0];
  FActiveTab := pgeMain.Pages[0];
  TabsNotForBugs := TList.Create;
  TabsNotForFeatures := TList.Create;
  TabsNotForFeatures.Add(tshBugDetails);
  TabsNotForFeatures.Add(tshBugSteps);
  SetTabFocusControl;
  FActiveTabNumber := 1;
  UpdateInfo;
  pgeMain.Style := tsButtons;
  FIDEName := GxOtaGetIDEProductIdentifier;
  SetParentBackgroundValue(pnlHeader, False);

  InitDpiScaler;
end;

procedure TfmFeedbackWizard.chkReproducibleClick(Sender: TObject);
begin
  edtPercent.Enabled := chkReproducible.Checked;
  lblPercent.Enabled := chkReproducible.Checked;
end;

procedure TfmFeedbackWizard.UpdateForNewTab;
begin
  UpdateInfo;
  SetDescriptionCaption;
  CenterTabContents;
  InitializeTabControls;
end;

procedure TfmFeedbackWizard.rgpFeedbackTypeClick(Sender: TObject);
begin
  SetTabCaptions;
  SetDefaultConfigurationData;
end;

procedure TfmFeedbackWizard.SetTabCaptions;
const
  SFeedbackDescription = '%s Description';
  SFeedbackCompleted = 'Completed %s';
begin
  tshDescription.Caption := Format(SFeedbackDescription, [GetFeedbackTypeString]);
  tshReport.Caption := Format(SFeedbackCompleted, [GetFeedbackTypeString]);
end;

procedure TfmFeedbackWizard.SetDefaultConfigurationData;
var
  i: Integer;
  CheckBox: TCheckBox;
begin
{$IFNDEF GX_VER310_up}
  CheckBox := nil;
{$ENDIF}
  for i := 0 to gbxConfigurationData.ControlCount - 1 do
  begin
    if gbxConfigurationData.Controls[i] is TCheckBox then
      CheckBox := gbxConfigurationData.Controls[i] as TCheckBox
    else
      Continue;
    if FeedbackType = fbBug then
      CheckBox.Checked := True
    else
      CheckBox.Checked := not CheckBox.Enabled;
  end;
end;

function TfmFeedbackWizard.GetNextTab: TTabSheet;
var
  CurrentIndex: Integer;
begin
  Assert(not OnLastPage);
  CurrentIndex := pgeMain.ActivePageIndex;
  Inc(CurrentIndex);
  Result := pgeMain.Pages[CurrentIndex];
  while not IsValidTab(Result) do
  begin
    Inc(CurrentIndex);
    Result := pgeMain.Pages[CurrentIndex];
  end;
  Inc(FActiveTabNumber);
end;

function TfmFeedbackWizard.GetPrevTab: TTabSheet;
var
  CurrentIndex: Integer;
begin
  Assert(not OnFirstPage);
  CurrentIndex := pgeMain.ActivePageIndex;
  Dec(CurrentIndex);
  Result := pgeMain.Pages[CurrentIndex];
  if FeedbackType = fbFeature then
    if (Result = tshBugDetails) or (Result = tshBugSteps) then
      Result := tshDescription;
  Dec(FActiveTabNumber);
end;

function TfmFeedbackWizard.CanProceed: Boolean;
begin
  Result := True;
  if FActiveTab = tshDescription then
    Result := Length(Trim(mmoDescription.Text)) > 0
  else if FActiveTab = tshBugSteps then
    Result := mmoBugSteps.Modified;
end;

function TfmFeedbackWizard.OnFirstPage: Boolean;
begin
  Result := pgeMain.ActivePage = pgeMain.Pages[0];
end;

function TfmFeedbackWizard.OnLastPage: Boolean;
begin
  Result := pgeMain.ActivePage = pgeMain.Pages[pgeMain.PageCount - 1];
end;

type
  TTabSheetCracker = class(TTabSheet);

procedure TfmFeedbackWizard.SetTabFocusControl;
begin
  TTabSheetCracker(FActiveTab).SelectFirst;
end;

procedure TfmFeedbackWizard.btnEmailClick(Sender: TObject);
var
  ExecFilename: string;
begin
  // Do not localize the lines below.
  ExecFilename := GetDestinationEmail;
  GXShellOpen(ExecFilename, False);
end;

procedure TfmFeedbackWizard.btnCopyClick(Sender: TObject);
begin
  Clipboard.AsText := mmoReport.Lines.Text;
end;

procedure TfmFeedbackWizard.btnSaveClick(Sender: TObject);
begin
  if GetOpenSaveDialogExecute(dlgSaveReport) then
    mmoReport.Lines.SaveToFile(dlgSaveReport.FileName);
end;

procedure TfmFeedbackWizard.btnCancelClick(Sender: TObject);
begin
  Close;
end;

function TfmFeedbackWizard.IsValidTab(const Tab: TTabSheet): Boolean;
begin
  Result := False;
  case FeedbackType of
    fbBug:
      Result := TabsNotForBugs.IndexOf(Tab) = -1;
    fbFeature:
      Result := TabsNotForFeatures.IndexOf(Tab) = -1;
  end;
end;

procedure TfmFeedbackWizard.CenterTabContents;
begin
  if not Assigned(FActiveTab) then
    Exit;
  if (FActiveTab.ControlCount = 1) then
    CenterInParent(FActiveTab.Controls[0]);
end;

procedure TfmFeedbackWizard.pgeMainResize(Sender: TObject);
begin
  CenterTabContents;
end;

procedure TfmFeedbackWizard.CenterInParent(Control: TControl);
var
  Parent: TControl;
begin
  Parent := Control.Parent;
  Assert(Assigned(Parent));
  Control.Top := (Parent.ClientHeight - Control.Height) div 2;
  Control.Left := (Parent.ClientWidth - Control.Width) div 2;
end;

procedure TfmFeedbackWizard.pnlButtonsResize(Sender: TObject);
begin
  CenterInParent(pnlButtonsCenter);
end;

procedure TfmFeedbackWizard.InitializeTabControls;
const
  Steps = '1. Start %s from the start menu' + CRLF +
      '2. The default (empty) project is open' + CRLF +
      '3. On the main menu, select ...' + CRLF +
      '4.' + CRLF +
      '5.';
begin
  if FActiveTab = tshReport then
    GenerateReport;

  // Do not localize the lines below.
  if (FActiveTab = tshBugSteps) and (not mmoBugSteps.Modified) then
    mmoBugSteps.Lines.Text := Format(Steps, [FIDEName]);
end;

procedure TfmFeedbackWizard.GenerateReport;
var
  Report: string;
  i: Integer;
begin
  Report := '';
  for i := 0 to pgeMain.PageCount - 1 do
    if IsValidTab(pgeMain.Pages[i]) then
      Report := Report + GetTabReportText(pgeMain.Pages[i]);
  mmoReport.Lines.Text := Report;
end;

procedure TfmFeedbackWizard.edtPercentKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, ['0'..'9', #8]) then
    Key := #0;
end;

procedure TfmFeedbackWizard.UpdateInfo;
var
  IsVisible: Boolean;
begin
  SetDescriptionInfo;
  SetExampleInfo;
  IsVisible := mmoInfoHelp.Lines.Text <> '';
  tshInfoHelp.TabVisible := IsVisible;
  IsVisible := mmoInfoExample.Lines.Text <> '';
  tshInfoExample.TabVisible := IsVisible;
  IsVisible := tshInfoHelp.TabVisible or tshInfoExample.TabVisible;
  pgeInfo.Visible := IsVisible;
  Splitter.Visible := IsVisible;
end;

procedure TfmFeedbackWizard.InfoMemoEnter(Sender: TObject);
begin
  pgeMain.SetFocus;
end;

function TfmFeedbackWizard.GetDestinationEmail: string;
begin
  if FeedbackType = fbBug then
    Result := FBugEmail
  else
    Result := FFeatureEmail;
end;

function TfmFeedbackWizard.GetTabReportText(Tab: TTabSheet): string;
const
  SDescription = 'Description:';
  SSteps = 'Steps:';
begin
  Result := '';
  if Tab = tshType then
    Result := 'GExperts ' + GetFeedbackTypeString + SkipLine // Do not localize.
  else if Tab = tshDescription then
    Result := SDescription + CRLF + mmoDescription.Lines.Text + SkipLine
  else if Tab = tshBugDetails then
    Result := GetBugDetailsString
  else if Tab = tshBugSteps then
    Result := SSteps + CRLF + mmoBugSteps.Lines.Text + SkipLine
  else if Tab = tshConfiguration then
    Result := GetSystemConfigurationString;
end;

function TfmFeedbackWizard.GetFeedbackTypeString: string;
const
  SBugReport = 'Bug Report';
  SFeatureRequest = 'Feature Request';
begin
  if FeedbackType = fbBug then
    Result := SBugReport
  else
    Result := SFeatureRequest;
end;

function TfmFeedbackWizard.GetBugDetailsString: string;
const
  SBugDetails = 'Bug Details:';
  SBugIsReproducible = '  The bug is reproducible %s%% of the time';
  SBugIsNotReproducible = '  The bug is not reproducible';
begin
  Result := SBugDetails + CRLF;
  if chkReproducible.Checked then
    Result := Result + Format(SBugIsReproducible + CRLF, [edtPercent.Text])
  else
    Result := Result + SBugIsNotReproducible + CRLF;
  if chkMultipleMachines.Checked then
    Result := Result + '  ' + StripHotkey(chkMultipleMachines.Caption) + CRLF;
  if chkProjectSpecific.Checked then
    Result := Result + '  ' + StripHotkey(chkProjectSpecific.Caption) + CRLF;
  Result := Result + CRLF;
end;

function GetGExpertsVersionString: string;
begin
  Result := ExtractFileName(ThisDllName);
  Result := Result + ' ' + GetFileVersionString(ThisDllName, True, False);
  Result := Result + ' from ' + DateTimeToStr(GetFileDate(ThisDllName));
  Result := Result + CRLF;
end;

// Do not localize any of the strings below.
const
  cKeyNamePath = '\SYSTEM\CurrentControlSet\Control\Keyboard Layouts\';
  cKeyCodePath = '\SYSTEM\CurrentControlSet\Control\Keyboard Layout\DosKeybCodes';
  cKeySubCodes: array[0..7] of string = ('Unknown',
    'IBM PC/XT or compatible (83-key) keyboard',
    'Olivetti "ICO" (102-key) keyboard',
    'IBM PC/AT (84-key) or similar keyboard',
    'IBM enhanced (101 or 102-key) keyboard',
    'Nokia 1050 and similar keyboards',
    'Nokia 9140 and similar keyboards',
    'Japanese keyboard');
  cKeyNumFKeys: array[0..7] of string = ('Unknown', '10', '12 (18)', '10', '12', '10', '24', 'OEM');
  cSysInfoProcessorText: array[0..4] of string = ('Intel', 'MIPS', 'ALPHA', 'PPC', 'UNKNOWN');

function GetKeyboardLayoutNameFromReg: string;
var
  Reg: TRegistry;
  Name, Code, Dll: string;
  Layout: string;
begin
  Result := '<unknown>';
  SetLength(Layout, KL_NAMELENGTH);
  if not GetKeyboardLayoutName(PChar(Layout)) then
    Exit;

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey(cKeyNamePath + Layout, False) then
    begin
      Name := Reg.ReadString('Layout Text');
      Dll := Reg.ReadString('Layout File');
    end;
    Reg.CloseKey;

    if Reg.OpenKey(cKeyCodePath, False) then
      Code := Reg.ReadString(Layout);
    Reg.CloseKey;

    Result := Format('%s (%s) in %s', [Name, Code, Dll]);
  finally
    FreeAndNil(Reg);
  end;
end;

function GetLocaleKeyboardString: string;
var
  Data: TStrings;
begin
  Data := TStringList.Create;
  try
    Data.Add('Keyboard:');
    Data.Add(Format('  Type %d: %s with %s FKeys', [GetKeyboardType(1),
      cKeySubCodes[Min(GetKeyboardType(0), 7)],
        cKeyNumFKeys[Min(GetKeyboardType(2), 7)]]));
    Data.Add(Format('  Layout: %s', [GetKeyboardLayoutNameFromReg]));
    Data.Add('');

    Data.Add('Locale Info:');
    Data.Add(Format('  Number of Digits: ''%s''', [GetLocaleChar(0, LOCALE_IDIGITS, '.')]));
    Data.Add(Format('  Leading Zero: ''%s''', [GetLocaleStr(0, LOCALE_ITLZERO, '0')]));
    Data.Add(Format('  List Separators: ''%s''', [GetLocaleStr(0, LOCALE_SLIST, '0')]));
    Data.Add(Format('  Grouping: ''%s''', [GetLocaleStr(0, LOCALE_SGROUPING, '0')]));
    Data.Add(Format('  Decimal Separator: ''%s''', [GetLocaleStr(0, LOCALE_SDECIMAL, '.')]));
    Data.Add(Format('  Group Separator: ''%s''', [GetLocaleStr(0, LOCALE_STHOUSAND, ',')]));
    Data.Add(Format('  Monetary Grouping: ''%s''', [GetLocaleStr(0, LOCALE_SMONGROUPING, '0')]));
    Data.Add(Format('  Monetary Decimal Separator: ''%s''', [GetLocaleStr(0, LOCALE_SMONDECIMALSEP, '0')]));
    Data.Add(Format('  Monetary Group Separator: ''%s''', [GetLocaleStr(0, LOCALE_SMONTHOUSANDSEP, '0')]));
  finally
    Result := Data.Text + CRLF;
    FreeAndNil(Data);
  end;
end;

function GetCPUSpeed: Extended;
const
  DelayTime = 250;
var
  TimerHi, TimerLo: DWORD;
  PriorityClass, Priority: Integer;
begin
  try
    PriorityClass := GetPriorityClass(GetCurrentProcess());
    Priority := GetThreadPriority(GetCurrentThread());

    SetPriorityClass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS);
    SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_TIME_CRITICAL);

    Sleep(10);
    asm
      dw 310Fh // rdtsc
      mov TimerLo, eax
      mov TimerHi, edx
    end;
    Sleep(DelayTime);
    asm
      dw 310Fh // rdtsc
      sub eax, TimerLo
      sbb edx, TimerHi
      mov TimerLo, eax
      mov TimerHi, edx
    end;

    SetThreadPriority(GetCurrentThread(), Priority);
    SetPriorityClass(GetCurrentProcess(), PriorityClass);

    Result := (TimerLo / DelayTime) * 999.1; // Inaccuracy in sleep
  except
    Result := 0;
  end;
end;

function GetCpuInfoString: string;
var
  Data: TStringList;
  SysInfo: TSystemInfo;
begin
  GetSystemInfo(SysInfo);
  Data := TStringList.Create;
  try
    Data.Add('CPU:');
    Data.Add(Format('  # Processors: %d', [SysInfo.dwNumberOfProcessors]));
    Data.Add(Format('  Type: %s %d model %d Stepping %d',
      [cSysInfoProcessorText[SysInfo.wProcessorArchitecture], SysInfo.dwProcessorType,
      Integer(SysInfo.wProcessorRevision shr 8), Integer(SysInfo.wProcessorRevision and $00FF)]));
    Data.Add(Format('  Speed: %.2f MHz', [GetCPUSpeed / 1000000]));
  finally
    Result := Data.Text + CRLF;
    FreeAndNil(Data);
  end;
end;

function ReportItemsInRegistryKey(const Header, Key: string): string;
var
  Reg: TRegistry;
  Names: TStringList;
  i: Integer;
  Value: string;
begin
  Reg := TRegistry.Create(KEY_READ);
  try
    if Reg.OpenKey(Key, False) then
    begin
      Names := TStringList.Create;
      try
        Reg.GetValueNames(Names);
        Result := Result + Header + CRLF;
        for i := 0 to Names.Count - 1 do
        begin
          Value := Reg.ReadString(Names[i]);
          Result := Result + Format('  %s = %s', [Value, Names[i]]) + CRLF;
        end;
      finally
        FreeAndNil(Names);
      end;
    end
    else
      Result := 'No data for ' + Key + SkipLine;
  finally
    FreeAndNil(Reg);
  end;
end;

function GetInstalledExpertsString: string;
begin
  Result := ReportItemsInRegistryKey('Installed Experts:',
    GxOtaGetIdeBaseRegistryKey + '\Experts') + CRLF;
end;

function GetInstalledPackagesString: string;
begin
  Result := ReportItemsInRegistryKey('Installed Packages:',
    GxOtaGetIdeBaseRegistryKey + '\Known Packages') + CRLF;
  Result := Result + ReportItemsInRegistryKey('Installed IDE Packages:',
    GxOtaGetIdeBaseRegistryKey + '\Known IDE Packages') + CRLF;
end;

function GetGExpertsSettingsString: string;
begin
  Result := ReportItemsInRegistryKey('GExperts Experts:',
    ConfigInfo.GExpertsIdeRootRegistryKey + '\EnabledExperts') + CRLF;
end;

function TfmFeedbackWizard.GetSystemConfigurationString: string;
begin
  Result := 'Configuration:' + CRLF;
  Result := Result + '  OS: ' + GetOSString + CRLF;
  Result := Result + '  GExperts: ' + GetGExpertsVersionString;
  Result := Result + '  IDE: ' + TypInfo.GetEnumName(TypeInfo(TBorlandIdeVersion),
    Ord(GetBorlandIdeVersion)) + ' ' + GetIdeEdition + CRLF;
  Result := Result + '  ComCtl32: ' + GetFileVersionString('comctl32.dll', False) + SkipLine;

  if chkExperts.Checked then
    Result := Result + GetInstalledExpertsString;
  if chkPackages.Checked then
    Result := Result + GetInstalledPackagesString;
  if chkGExpertsSettings.Checked then
    Result := Result + GetGExpertsSettingsString;
  if chkCpu.Checked then
    Result := Result + GetCpuInfoString;
  if chkLocaleKeyboard.Checked then
    Result := Result + GetLocaleKeyboardString;
end;

procedure TfmFeedbackWizard.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
