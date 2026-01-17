unit RipGrepper.UI.Components.BoundCheckBox;

interface

uses
	System.Classes,
	System.SysUtils,
	System.Types,
	Vcl.Controls,
	Vcl.ExtCtrls,
	Vcl.Graphics,
	Vcl.StdCtrls,
	Winapi.Messages;

type
	TCheckBoxPosition = (cpAbove, cpBelow, cpLeft, cpRight);

	{ TBoundCheckBox }

	TBoundCheckBoxClass = class of TBoundCheckBox;

	TBoundCheckBox = class(TCheckBox)
		strict private
			procedure CMTextChanged(var _message : TMessage); message CM_TEXTCHANGED;

		protected
			FIsCheckBoxModified : Boolean;

		public
			constructor Create(_owner : TComponent); override;
			property IsCheckBoxModified : Boolean read FIsCheckBoxModified write FIsCheckBoxModified;
	end;

implementation

{ TBoundCheckBox }

constructor TBoundCheckBox.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	name := 'SubCheckBox';
	SetSubComponent(True);
	if Assigned(_owner) then begin
		Caption := _owner.Name;
	end;
	IsCheckBoxModified := False;
end;

procedure TBoundCheckBox.CMTextChanged(var _message : TMessage);
begin
	inherited;
	IsCheckBoxModified := True;
end;

end.
