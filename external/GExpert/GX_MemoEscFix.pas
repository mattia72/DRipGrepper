unit GX_MemoEscFix;

// This is a fix for an annoying bug in the VCL that swallows Esc as well as Enter if the focus
// is on a TMemo control, so cancelling a dialog with Esc is not possible if a TMemo has the focus.

// Source: David Heffernan on StackOverflow
// https://stackoverflow.com/a/22563307/49925

// to use this fix add this unit to the form's uses list and also add
// type
//   TMemo = class(TMemoEscFix)
//   end;
// to it.
// I didn't make this automatic, by exporting the fix directly as TMemo, because I want a strong
// hint that the class is being interposed when I am debugging the code.

interface

uses
  StdCtrls,
  Controls,
  Messages;

type
  TMemoEscFix = class(StdCtrls.TMemo)
  protected
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
  end;

implementation

uses
  Windows;

{ TMemoEscFix }

procedure TMemoEscFix.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  case Msg.CharCode of
    VK_ESCAPE:
      Msg.Result := 0;
    VK_RETURN, VK_EXECUTE, VK_CANCEL:
      Msg.Result := 1;
  else
    inherited;
  end;
end;

procedure TMemoEscFix.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := Msg.Result and not DLGC_WANTALLKEYS;
end;

end.
