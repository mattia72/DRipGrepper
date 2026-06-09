unit RipGrepper.UI.Components.IconLabel;

interface

uses
	System.Classes,
	Vcl.Controls,
	Vcl.Graphics,
	Vcl.StdCtrls,
	Winapi.Windows;

type
	TIconLabelType = (iltNone, iltWarning, iltError, iltInfo, iltQuestion);

	TIconLabel = class(TLabel)
	strict private
		FIconType : TIconLabelType;
		FIconText : string;
		FOrigCaption : string;
		FIconColor : TColor;
		procedure SetIconType(const _value : TIconLabelType);
		function GetIconChar(const _iconType : TIconLabelType) : string;
		function GetIconColor(const _iconType : TIconLabelType) : TColor;
	protected
		procedure Paint; override;
		procedure Loaded; override;
	public
		property IconType : TIconLabelType read FIconType write SetIconType;
		property IconText : string read FIconText write FIconText;
	end;

implementation

uses
	System.SysUtils,
	System.StrUtils;

{ TIconLabel }

procedure TIconLabel.SetIconType(const _value : TIconLabelType);
begin
	FIconType := _value;
	FIconColor := GetIconColor(_value);
	Invalidate();
end;

function TIconLabel.GetIconChar(const _iconType : TIconLabelType) : string;
begin
	case _iconType of
		iltWarning : Result := #$26A0;
		iltError : Result := #$2716;
		iltInfo : Result := #$2139;
		iltQuestion : Result := #$2753;
	else
		Result := '';
	end;
end;

function TIconLabel.GetIconColor(const _iconType : TIconLabelType) : TColor;
begin
	case _iconType of
		iltWarning : Result := $0080FF; // orange
		iltError : Result := clRed;
		iltInfo : Result := clBlue;
		iltQuestion : Result := clNavy;
	else
		Result := clWindowText;
	end;
end;

procedure TIconLabel.Loaded;
begin
	inherited;
	FOrigCaption := Caption;
end;

procedure TIconLabel.Paint;
var
	iconStr : string;
	origWidth : Integer;
	R : TRect;
begin
	if FIconType <> iltNone then begin

		Canvas.Font.Assign(Font);
		Canvas.Brush.Style := bsClear;

		// Draw original caption in normal style (DrawText handles '&' as accelerator prefix)
		Canvas.Font.Color := Font.Color;
		Canvas.Font.Style := [];
		R := Rect(0, 0, MaxInt, Height);
		DrawText(Canvas.Handle, PChar(FOrigCaption + ' '), -1, R, DT_LEFT or DT_CALCRECT);
		origWidth := R.Right;
		R := Rect(0, 0, origWidth, Height);
		DrawText(Canvas.Handle, PChar(FOrigCaption + ' '), -1, R, DT_LEFT);

		// Draw icon + text in colored bold style
		Canvas.Font.Color := FIconColor;
		Canvas.Font.Style := [fsBold];
		R := Rect(origWidth, 0, Width, Height);
		iconStr := GetIconChar(FIconType);
		DrawText(Canvas.Handle, PChar(IfThen(not FIconText.IsEmpty, ' ' + FIconText) + iconStr), -1, R, DT_LEFT);
	end else begin
		inherited;
	end;
end;

// initialization
// 	System.Classes.RegisterClass(TIconLabel);

end.
