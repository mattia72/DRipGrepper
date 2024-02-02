unit GX_IdeUtils;

{ A number of utility functions that work directly on the
  IDE using mechanism provided by the VCL.

  None of the functions in this unit use the "official"
  way through the Open Tools API, as the Open Tools API
  does not deliver whatever is needed here.

  See GX_OtaUtils.pas for routines that make use of the
  officially exposed functionality. }

interface

{$I GX_CondDefine.inc}

uses
  Graphics, Forms, Menus, ComCtrls, Controls, Classes, StdCtrls, ActnList, Actions;

const
  EditorFormClassName = 'TEditWindow';
  EditorControlName = 'Editor';
  EditorControlClassName = 'TEditControl';

// Returns a reference to the main form of the IDE (TAppBuilder)
function GetIdeMainForm: TCustomForm;
// Returns the height of a IDE main menu item
function GetMainMenuItemHeight: Integer;
// Returns the height of a standard OS menu item
function GetStandardMenuItemHeight: Integer;
// Returns a reference to the IDE's component palette tab control.
// May return nil.
function GetComponentPaletteTabControl: TTabControl;
// Returns a reference to the Object Inspector form.
// May return nil.
function GetObjectInspectorForm: TCustomForm;
function GetComponentPalettePopupMenu: TPopupMenu;
// Returns True of AForm is an editor form in the IDE,
// False otherwise. AForm may be nil.
function IsIdeEditorForm(AForm: TCustomForm): Boolean;
function IsEditControl(Control: TControl): Boolean;
// Get the actual TEditControl embedded in the given IDE editor form
function GetIDEEditControl(Form: TCustomForm): TWinControl;
function GetIDEFormNamed(const FormName: string): TCustomForm;
// We can cause internal editor kernel AVs if we change the editor text
// while the replace confirmation dialog is up, so we detect that case here
function IsReplaceConfirmDialogOnScreen: Boolean;

///<summary>
/// Searches through Screen.Forms for a form of the given class
/// @param ClassName is the classname of the form we are looking for
/// @param Form contains the first form of the given class, only valid if Result = True
/// @returns True, if a form could be found, False otherwise </summary>
function TScreen_TryFindClassForm(const _ClassName: string; out _Form: TForm): Boolean;

///<summary>
/// Searches Screen.Forms for the Message View form
/// @param Form contains the message view form, only valid if Result = True
/// @returns True, if the form could be found, False otherwise </summary>
function TryFindMessageView(out _Form: TForm): Boolean;

///<summary>
/// Searches Screen.Forms for the Message View form and closes it if found
/// @returns True, if the form could be found and was closed, False otherwise </summary>
function TryCloseMessageView: Boolean;

// Return the IDE's root directory (the installation directory).
// Returns an empty string if the information could not be retrieved.
function GetIdeRootDirectory: string;

///<summary>
/// Reads the current Desktop from the registry </summary>
function GetIdeDesktopName: string;

///<summary>
/// Sets the IDE desktop by changing it in the Destkop toolbar </summary>
procedure SetIdeDesktop(const _Desktop: string);

///<summary>
/// Returns a reference to the IDE desktop selection combobox's Items.
/// Use with care!!!! </summary>
function TryGetIdeDesktops(out _Items: TStrings): Boolean;

function TryGetDesktopCombo(out _cmb: TCustomCombobox): Boolean;

// Return the IDE's version identifier, such as ENT, CSS, PRO, STD,
// or the empty string if unknown
function GetIdeEdition: string;

procedure HijackIDEAction(const ActionName: string; var OriginalIDEAction: TNotifyEvent; NewIDEAction: TNotifyEvent);
procedure ResetIDEAction(const ActionName: string; IDEHandler: TNotifyEvent);

// Get the IDE's editor background color
function GetIdeEditorForegroundColor: TColor;
function GetIdeEditorBackgroundColor: TColor;
function GetIdeEditorLineHighlightColor: TColor;
function GetIdeEnvironmentOverrides(Overrides: TStrings): Boolean;

// Set the PopupMode property in Delphi 8+ to get the z-order/layering right
procedure SetNonModalFormPopupMode(Form: TCustomForm);
procedure SetModalFormPopupMode(Form: TCustomForm);

function GetIDEVersionID: string;

function RunningWindows: Boolean;

function RunningDelphi8: Boolean;
function RunningDelphi8OrGreater: Boolean;
function RunningDelphi7OrLess: Boolean;
function RunningDelphi7OrGreater: Boolean;
function RunningDelphi2005: Boolean;
function RunningBDS2006OrLess: Boolean;
function RunningBDS2006: Boolean;
function RunningBDS2006OrGreater: Boolean;
function RunningDelphi2007OrLess: Boolean;
function RunningDelphi2007: Boolean;
function RunningDelphi2007OrGreater: Boolean;
function RunningRS2009: Boolean;
function RunningRS2009OrGreater: Boolean;
function RunningRS2010OrGreater: Boolean;
function RunningRSXE: Boolean;
function RunningRSXEOrGreater: Boolean;

function RunningCPPBuilder: Boolean;
function IDEHasWelcomePage: Boolean;
function FileIsWelcomePage(const FileName: string): Boolean;
function IDEEditorEncodingIsUTF8: Boolean;

function IsThemingEnabled: Boolean;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Windows, Registry, StrUtils,
  u_dzCompilerAndRtlVersions, u_dzClassUtils, u_dzStringUtils,
  GX_GenericUtils, GX_OtaUtils, GX_GxUtils;

function GetIdeMainForm: TCustomForm;
begin
  Assert(Assigned(Application));
  Result := Application.FindComponent('AppBuilder') as TCustomForm;

  {$IFOPT D+}
    if Result = nil then
      SendDebugError('Unable to find AppBuilder!');
  {$ENDIF}
end;

function GetMainMenuItemHeight: Integer;
var
  MainForm: TCustomForm;
  Component: TComponent;
begin
  // start with what Windows says, but add 2 pixels (that's correct for at least my computer),
  Result := GetSystemMetrics(SM_CYMENU) + 2;
  // but it doesn't really matter, because:
  // Since at least Delphi 6 the IDE no longer uses a regular main menu but a TActionMenuBar which
  // is higher than a regular main menu. It has the name 'MenuBar' and is a component of the main form.
  MainForm := GetIdeMainForm;
  Component := nil;
  if MainForm <> nil then
    Component := MainForm.FindComponent('MenuBar');
  if (Component <> nil) and (Component is TControl) then
    Result := TControl(Component).ClientHeight; // This is approximate?
end;

function GetStandardMenuItemHeight: Integer;
begin
  Result := GetSystemMetrics(SM_CYMENU);
  if Result < 1 then
    Result := 20; // Guess instead of raising an error?  This should never happen.
end;

function GetComponentPaletteTabControl: TTabControl;
var
  MainForm: TCustomForm;
begin
  Result := nil;

  MainForm := GetIdeMainForm;
  if MainForm <> nil then
    Result := MainForm.FindComponent('TabControl') as TTabControl;

  {$IFOPT D+}
    if (Result = nil) and (ComponentPaletteAvailable) then
      SendDebugError('Unable to find component palette TTabControl!');
  {$ENDIF}
end;

function GetObjectInspectorForm: TCustomForm;
var
  MainForm: TCustomForm;
begin
  Result := nil;
  MainForm := GetIdeMainForm;
  if MainForm <> nil then
    Result := (MainForm.FindComponent('PropertyInspector') as TCustomForm);
  if Result = nil then
    Result := GetIDEFormNamed('PropertyInspector');

  {$IFOPT D+}
    if Result = nil then
      SendDebugError('Unable to find object inspector!');
  {$ENDIF}
end;

function GetComponentPalettePopupMenu: TPopupMenu;
var
  MainForm: TCustomForm;
begin
  Result := nil;
  MainForm := GetIdeMainForm;
  if MainForm <> nil then
    Result := (MainForm.FindComponent('PaletteMenu') as TPopupMenu);

  {$IFOPT D+}
    if Result = nil then
      SendDebugError('Unable to find PaletteMenu!');
  {$ENDIF}
end;

function IsIdeEditorForm(AForm: TCustomForm): Boolean;
begin
  Result := (AForm <> nil) and
            (StartsStr('EditWindow_', AForm.Name)) and
            (AForm.ClassName = EditorFormClassName) and
            (not (csDesigning in AForm.ComponentState));
end;

function IsEditControl(Control: TControl): Boolean;
begin
  Result := False;
  if Assigned(Control) then
    Result := Control.ClassName = EditorControlClassName;
end;

function GetIDEEditControl(Form: TCustomForm): TWinControl;
var
  Component: TComponent;
begin
  Assert(Assigned(Form));
  Result :=  nil;
  Component := (Form.FindComponent(EditorControlName) as TWinControl);
  if Assigned(Component) then
    if Component is TWinControl then
      Result := Component as TWinControl;
end;

function GetIDEFormNamed(const FormName: string): TCustomForm;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Screen.CustomFormCount - 1 do
  begin
    if Screen.CustomForms[i].Name = FormName then
    begin
      Result := Screen.CustomForms[i];
      Break;
    end;
  end;
end;

// We can cause internal editor kernel AVs if we change the editor text
// while the replace confirmation dialog is up, so we detect that case here
function IsReplaceConfirmDialogOnScreen: Boolean;
const
  ConfirmDialogClassName = 'TMessageForm';
var
  AForm: TForm;
  i: Integer;
begin
  Result := False;

  Assert(Assigned(Screen));
  // We search in reverse order, since what we're looking for
  // should usually be last
  for i := Screen.FormCount - 1 downto 0 do
  begin
    AForm := Screen.Forms[i];

    Assert(Assigned(AForm));
    if SameText(AForm.ClassName, ConfirmDialogClassName) then
    begin
      // Make sure it lives in the main VCL module (package or not)
      if FindClassHInstance(AForm.ClassType) = VclInstance then
      begin
        // Now some weak heuristics (don't localize the component names).
        if Assigned(AForm.FindComponent('Yes')) and
           Assigned(AForm.FindComponent('No')) and
           Assigned(AForm.FindComponent('Cancel')) and
           Assigned(AForm.FindComponent('All')) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

function GetIdeDesktopName: string;
var
  reg: TRegistry;
begin
  Result := '';

  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKeyReadOnly(GxOtaGetIdeBaseRegistryKey+'\Session') then
      begin
        Result := Reg.ReadString('DesktopName');
        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;
  except
    on E: Exception do
      GxLogException(E, 'Error in GetIdeDesktopName');
  end;
end;

type
  TComboBoxHack = class(TCustomComboBox)
  end;

function TryGetDesktopCombo(out _cmb: TCustomComboBox): Boolean;
var
  AppBuilder: TForm;
begin
  Result := False;
  AppBuilder := TForm(Application.FindComponent('AppBuilder'));
  if not Assigned(AppBuilder) then
    Exit;
  _cmb := TCustomComboBox(AppBuilder.FindComponent('cbDesktop'));
  Result := Assigned(_cmb);
end;

procedure SetIdeDesktop(const _Desktop: string);
var
  cbDesktop: TCustomComboBox;
begin
  if not TryGetDesktopCombo(cbDesktop) then
    Exit; //==>

  TComboBoxHack(cbDesktop).Text := _Desktop;
  try
    TComboBoxHack(cbDesktop).Click;
  except
    // no idea why, but sometimes this causes an access violation
    on E: EAccessViolation do
      GxLogException(E, 'Error in SetIdeDesktop');
  end;
end;

function TryGetIdeDesktops(out _Items: TStrings): Boolean;
var
  cbDesktop: TCustomComboBox;
begin
  Result := TryGetDesktopCombo(cbDesktop);
  if Result then
    _Items := cbDesktop.Items;
end;

function GetIdeRootDirectory: string;
const
  BinDirPostfix = PathDelim + 'Bin';
begin
  if TRegistry_TryReadString(GxOtaGetIdeBaseRegistryKey, 'RootDir', Result, HKEY_LOCAL_MACHINE) then begin
    if DirectoryExists(Result) then begin
      Result := AddSlash(Result);
      Exit; //==>
    end;
  end;

  // There is no entry in the registry or it is invalid -> use the application's exename
  Result := RemoveSlash(ExtractFilePath(Application.ExeName));
  if SameText(RightStr(Result, Length(BinDirPostfix)), BinDirPostfix) then begin
    Result := DeleteRight(Result, Length(BinDirPostfix));
    Result := AddSlash(Result);
  end else
    Result := '';
end;

function GetIdeEdition: string;
begin
  Result := TRegistry_ReadString(GxOtaGetIdeBaseRegistryKey, 'Version', '', HKEY_LOCAL_MACHINE);
end;


function TryReadRegString(_reg: TRegistry; const _Name: string; out _Value: string): Boolean;
begin
  if _reg.ValueExists(_Name) then begin
    if _reg.GetDataType(_Name) = rdString then begin
      _Value := _reg.ReadString(_Name);
      Result := True;
      Exit //==>
    end;
  end;
  Result := False;
end;

{$IFDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
function GetIdeBackgroundColor(const _RegKey: string): TColor;
var
  Reg: TRegistry;
  Value: string;
begin
  Result := clWindow;
  Reg := TRegistry.Create;
  try
    if Reg.OpenKey(GxOtaGetIdeBaseRegistryKey + _RegKey, False) then
    begin
      if TryReadRegString(reg, 'Default Background', Value) then begin
        if SameText(Value, 'False') then begin
          if TryReadRegString(reg, 'Background Color New', Value) then
            Result := StringToColor(Value);
        end;
      end;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

function GetIdeForegroundColor(const _RegKey: string): TColor;
var
  Reg: TRegistry;
  Value: string;
begin
  Result := clWindowText;
  Reg := TRegistry.Create;
  try
    if Reg.OpenKey(GxOtaGetIdeBaseRegistryKey + _RegKey, False) then
    begin
      if TryReadRegString(reg, 'Default Foreground', Value) then begin
        if SameText(Value, 'False') then begin
          if TryReadRegString(reg, 'Foreground Color New', Value) then
            Result := StringToColor(Value);
        end;
      end;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;
{$ELSE} // Delphi 6 / 7
function GetIdeBackgroundColor(const _RegKey: string): TColor;
var
  Reg: TRegistry;
  Value: Integer;
  s: string;
const
  IdePalette: array [0..15] of TColor = (clBlack, clMaroon, clGreen,
    clOlive, clNavy, clPurple, clTeal, clLtGray, clDkGray, clRed,
    clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);
begin
  Result := clWindow;
  Reg := TRegistry.Create;
  try
    if Reg.OpenKey(GxOtaGetIdeBaseRegistryKey + _RegKey, False) then
    begin
      if TryReadRegString(reg, 'Default Background', s) then begin
        if SameText(s, 'False') then begin
          if Reg.ValueExists('Background Color') then
          begin
            if Reg.GetDataType('Background Color') = rdInteger then
            begin
              Value := Reg.ReadInteger('Background Color');
              if (Value > 15) or (Value < 0) then
                Value := 15;
              Result := IdePalette[Value];
            end;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

function GetIdeForegroundColor(const _RegKey: string): TColor;
var
  Reg: TRegistry;
  Value: Integer;
  s: string;
const
  IdePalette: array [0..15] of TColor = (clBlack, clMaroon, clGreen,
    clOlive, clNavy, clPurple, clTeal, clLtGray, clDkGray, clRed,
    clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);
begin
  Result := clWindowText;
  Reg := TRegistry.Create;
  try
    if Reg.OpenKey(GxOtaGetIdeBaseRegistryKey + _RegKey, False) then
    begin
      if TryReadRegString(reg, 'Default Foreground', s) then begin
        if SameText(s, 'False') then begin
          if Reg.ValueExists('Foreground Color') then
          begin
            if Reg.GetDataType('Foreground Color') = rdInteger then
            begin
              Value := Reg.ReadInteger('Foreground Color');
              if (Value > 15) or (Value < 0) then
                Value := 15;
              Result := IdePalette[Value];
            end;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;
{$ENDIF}

function GetIdeEditorBackgroundColor: TColor;
const
  RegAddKey = '\Editor\Highlight\Whitespace'; // do not localize
begin
  Result := GetIdeBackgroundColor(RegAddKey);
  {$IFOPT D+}SendDebug('IDE Background Color is: ' + ColorToString(Result) + '  '); {$ENDIF}
end;

function GetIdeEditorForegroundColor: TColor;
const
  RegAddKey = '\Editor\Highlight\Whitespace'; // do not localize
begin
  Result := GetIdeForegroundColor(RegAddKey);
  {$IFOPT D+}SendDebug('IDE Foreground Color is: ' + ColorToString(Result) + '  '); {$ENDIF}
end;

function GetIdeEditorLineHighlightColor: TColor;
const
  RegAddKey = '\Editor\Highlight\Line Highlight'; // do not localize
begin
  Result := GetIdeBackgroundColor(RegAddKey);
  {$IFOPT D+}SendDebug('IDE Line Highlight Color is: ' + ColorToString(Result) + '  '); {$ENDIF}
end;

function GetIdeEnvironmentOverrides(Overrides: TStrings): Boolean;
var
  Reg: TRegistry;
  Names: TStringList;
  i: Integer;
  Value: string;
begin
  Assert(Assigned(Overrides));
  Overrides.Clear;
  Result := True;
  Names := nil;
  Reg := TRegistry.Create;
  try
    Names := TStringList.Create;
    if Reg.OpenKey(GxOtaGetIdeBaseRegistryKey + '\Environment Variables', False) then
    begin
      Reg.GetValueNames(Names);
      for i := 0 to Names.Count - 1 do begin
        Value := Reg.ReadString(Names[i]);
        Overrides.Add(Names[i] + '=' + Value);
      end;
    end;
  finally
    FreeAndNil(Reg);
    FreeAndNil(Names);
  end;
end;

procedure HijackIDEAction(const ActionName: string; var OriginalIDEAction: TNotifyEvent; NewIDEAction: TNotifyEvent);
var
  Action: TContainedAction;
begin
  if GxOtaTryGetIdeActionByName(ActionName, Action)
    and (Action is TCustomAction) then begin
    OriginalIDEAction := (Action as TCustomAction).OnExecute;
    (Action as TCustomAction).OnExecute := NewIDEAction;
  end;
end;

procedure ResetIDEAction(const ActionName: string; IDEHandler: TNotifyEvent);
var
  Action: TContainedAction;
begin
  // todo: Is this correct? Can we always be sure that the IDEHandler
  //       was assigned? If not, our own handler might still be active
  //       and cause problems.
  if not Assigned(IDEHandler) then
    Exit;
  if GxOtaTryGetIdeActionByName(ActionName, Action) then
    Action.OnExecute := IDEHandler;
end;

procedure SetNonModalFormPopupMode(Form: TCustomForm);
begin {$IFNDEF GX_VER160_up} {FI:W519} {$ENDIF}
  {$IFDEF GX_VER160_up}
  if Assigned(Form) then
    Form.PopupMode := pmExplicit;
  {$ENDIF GX_VER160_up}
end;

procedure SetModalFormPopupMode(Form: TCustomForm);
begin {$IFNDEF GX_VER160_up} {FI:W519} {$ENDIF}
  {$IFDEF GX_VER160_up}
  if Assigned(Form) then
    Form.PopupMode := pmAuto;
  {$ENDIF GX_VER160_up}
end;

function GetIDEVersionID: string;
var
  RegKey: string;
  LastSlashPos: Integer;
  Version: string;
begin
  RegKey := GxOtaGetIdeBaseRegistryKey;
  LastSlashPos := LastCharPos(RegKey, '\');
  Version := Copy(RegKey, LastSlashPos + 1);
  Result := Version;
  if RunningDelphi8OrGreater then
    Result := 'BDS' + Version;
end;

function RunningWindows: Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningDelphi8: Boolean;
begin
  {$IFDEF VER160}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningDelphi8OrGreater: Boolean;
begin
  {$IFDEF GX_VER160_up}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningDelphi7OrLess: Boolean;
begin
  Result := not RunningDelphi8OrGreater;
end;

function RunningDelphi7OrGreater: Boolean;
begin
  {$IFDEF GX_VER150_up}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningDelphi2005: Boolean;
begin
  {$IFDEF VER170}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningDelphi2007: Boolean;
begin
  {$IFDEF VER185}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningDelphi2007OrLess: Boolean;
begin
  {$IFDEF GX_VER200_up}
  Result := False;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

function RunningDelphi2007OrGreater: Boolean;
begin
  {$IFDEF GX_VER185_up}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningRS2009: Boolean;
begin
  {$IFDEF VER200}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningRSXE: Boolean;
begin
  {$IFDEF VER220}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningRSXEOrGreater: Boolean;
begin
  {$IFDEF GX_VER220_up}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningRS2009OrGreater: Boolean;
begin
  {$IFDEF GX_VER200_up}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningRS2010OrGreater: Boolean;
begin
  {$IFDEF GX_VER210_up}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningBDS2006OrGreater: Boolean;
begin
  {$IFDEF GX_VER180_up}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningBDS2006OrLess: Boolean;
begin
  Result := not RunningDelphi2007OrGreater;
end;

function RunningBDS2006: Boolean;
begin
  {$IFDEF VER180}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

// This is whether we are running C++Builder Version 6, not if the IDE has C++ support (like BDS2006+)
function RunningCPPBuilder: Boolean;
begin
  {$IFDEF BCB}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function IDEHasWelcomePage: Boolean;
begin
  Result := RunningDelphi8OrGreater;
end;

function FileIsWelcomePage(const FileName: string): Boolean;
begin
  Result := IDEHasWelcomePage and StringInArray(FileName, ['default.htm', 'bds:/default.htm']);
end;

function IDEEditorEncodingIsUTF8: Boolean;
begin
  Result := RunningDelphi8OrGreater;
end;

function IsThemingEnabled: Boolean;
var
  reg: TRegistry;
begin
  Result := False;
  if CompilerVersion < CompilerVersionDelphiX103 then
    Exit; //==>

  // if yes, check if theming is enabled
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly(GxOtaGetIdeBaseRegistryKey + '\Theme') then begin
      try
        if reg.ValueExists('Enabled') and (reg.GetDataType('Enabled') = rdInteger) then
          Result := (reg.ReadInteger('Enabled') <> 0);
      finally
        reg.CloseKey;
      end;
    end;
  finally
    FreeAndNil(reg);
  end;
end;

function TScreen_TryFindClassForm(const _ClassName: string; out _Form: TForm): Boolean;
var
  i: Integer;
  frm: TForm;
begin
  Result := True;
  for i := 0 to Screen.FormCount - 1 do begin
    frm := Screen.Forms[i];
    if frm.ClassNameIs(_ClassName) then begin
      _Form := frm;
      Exit; //==>
    end;
  end;
  Result := False;
end;

function TryFindMessageView(out _Form: TForm): Boolean;
begin
  Result := TScreen_TryFindClassForm('TMsgWindow', _Form);
  if not Result then begin
    // otherwise TMessageViewForm is used
    Result := TScreen_TryFindClassForm('TMessageViewForm', _Form);
  end;
end;

function TryCloseMessageView: Boolean;
var
  MessageViewForm: TForm;
begin
  Result := TryFindMessageView(MessageViewForm);
  if Result then
    MessageViewForm.Hide;
end;

end.
