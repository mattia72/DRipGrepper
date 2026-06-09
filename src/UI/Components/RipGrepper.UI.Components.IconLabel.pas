unit RipGrepper.UI.Components.IconLabel;

interface

uses
	System.Classes,
	Vcl.Controls,
	Vcl.Graphics,
	Vcl.StdCtrls;

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
	textToDraw : string;
	iconStr : string;
	origWidth : Integer;
begin
	if FIconType <> iltNone then begin
		iconStr := GetIconChar(FIconType);
		textToDraw := FOrigCaption + ' ' + iconStr;
		if not FIconText.IsEmpty then begin
			textToDraw := textToDraw + ' ' + FIconText;
		end;

		Canvas.Font.Assign(Font);
		Canvas.Brush.Style := bsClear;

		// Draw original caption in normal style
		Canvas.Font.Color := Font.Color;
		Canvas.Font.Style := [];
		origWidth := Canvas.TextWidth(FOrigCaption + ' ');
		Canvas.TextOut(0, 0, FOrigCaption + ' ');

		// Draw icon + text in colored bold style
		Canvas.Font.Color := FIconColor;
		Canvas.Font.Style := [fsBold];
		Canvas.TextOut(origWidth, 0, iconStr + IfThen(not FIconText.IsEmpty, ' ' + FIconText));
	end else begin
		inherited;
	end;
end;

// initialization
// 	System.Classes.RegisterClass(TIconLabel);

end.
