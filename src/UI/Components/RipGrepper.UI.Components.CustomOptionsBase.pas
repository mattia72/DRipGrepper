unit RipGrepper.UI.Components.CustomOptionsBase;

interface

uses
	System.Classes,
	Vcl.Controls,
	Vcl.ExtCtrls;

type
	// Base class for custom options panels (checkbox/radio groups)
	TCustomOptionsBase = class(TCustomPanel)
		const
			CTRL_SPACE = 8;
			ITEM_HEIGHT = 22;
			GROUPBOX_PADDING = 8;

		strict private
			FExpertHeightDiff : Integer;
			FUseFlowLayout : Boolean;
			FColumns : Integer;
			procedure setColumns(const _value : Integer);

		protected
			procedure Resize; override;

		public
			constructor Create(_owner : TComponent); override;
			procedure AlignControlItems(); virtual; abstract;
			procedure Clear; virtual; abstract;
			function GetFontSize() : integer;
			function GetTextHeight(const _sText : string) : Integer;
			property ExpertHeightDiff : Integer read FExpertHeightDiff write FExpertHeightDiff;
			property UseFlowLayout : Boolean read FUseFlowLayout write FUseFlowLayout;

		published
			property Columns : Integer read FColumns write setColumns default 1;
	end;

implementation

{ TCustomOptionsBase }

constructor TCustomOptionsBase.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	FColumns := 1;
	Caption := '';
	BevelOuter := bvNone;
	Width := 200;
	Height := 100;
	FUseFlowLayout := False;
end;

function TCustomOptionsBase.GetFontSize() : integer;
begin
	Result := Font.Size;
end;

function TCustomOptionsBase.GetTextHeight(const _sText : string) : Integer;
begin
	Result := Canvas.TextHeight(_sText);
end;

procedure TCustomOptionsBase.setColumns(const _value : Integer);
begin
	if (_value > 0) and (FColumns <> _value) then begin
		FColumns := _value;
		AlignControlItems();
	end;
end;

procedure TCustomOptionsBase.Resize;
begin
	inherited Resize;
	// AlignControlItems should be called explicitly when needed
	// Automatic alignment during resize causes performance issues during initialization
end;

end.
