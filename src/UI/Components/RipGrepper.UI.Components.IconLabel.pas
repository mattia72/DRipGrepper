unit RipGrepper.UI.Components.IconLabel;

interface

uses
	System.Classes,
	Vcl.Controls,
	Vcl.Graphics,
	Vcl.ImgList,
	Vcl.StdCtrls,
	System.UITypes,
	Winapi.Windows,
	RipGrepper.Helper.UI.DarkMode;

type
	TIconLabelType = (iltNone, iltWarning, iltError, iltInfo, iltQuestion);

	TIconLabel = class(TLabel)
		strict private
			FIconType : TIconLabelType;
			FIconHint : string;
			FOrigCaption : string;
			FIconColor : TColor;
			FImages : TCustomImageList;
			FImageIndexWarning : TImageIndex;
			FImageIndexError : TImageIndex;
			FImageIndexInfo : TImageIndex;
			FImageIndexQuestion : TImageIndex;
			procedure SetIconType(const _value : TIconLabelType);
			procedure SetIconHint(const _value : string);
			procedure SetImages(const _value : TCustomImageList);
			function GetIconChar(const _iconType : TIconLabelType) : string;
			function GetIconColor(const _iconType : TIconLabelType) : TColor;
			function GetImageIndex(const _iconType : TIconLabelType) : TImageIndex;
			function DrawTextMeasured(const _text : string; _left : Integer) : Integer;

		protected
			procedure Notification(_component : TComponent; _operation : TOperation); override;
			procedure Paint; override;
			procedure Loaded; override;

		public
			constructor Create(_owner : TComponent); override;
			property IconType : TIconLabelType read FIconType write SetIconType;
			property IconHint : string read FIconHint write SetIconHint;
			property Images : TCustomImageList read FImages write SetImages;
			// Image indexes for different icon types (if Images is assigned)
			// Image names should have 'icon-' prefix followed by the icon type (e.g. 'icon-warning', 'icon-error', etc.)
			property ImageIndexWarning : TImageIndex read FImageIndexWarning write FImageIndexWarning;
			property ImageIndexError : TImageIndex read FImageIndexError write FImageIndexError;
			property ImageIndexInfo : TImageIndex read FImageIndexInfo write FImageIndexInfo;
			property ImageIndexQuestion : TImageIndex read FImageIndexQuestion write FImageIndexQuestion;
	end;

procedure Register;

implementation

uses
	System.SysUtils,
	RipGrepper.UI.Components.Constants;

{ TIconLabel }

constructor TIconLabel.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	FImageIndexWarning := -1;
	FImageIndexError := -1;
	FImageIndexInfo := -1;
	FImageIndexQuestion := -1;
end;

procedure TIconLabel.SetIconType(const _value : TIconLabelType);
begin
	FIconType := _value;
	FIconColor := GetIconColor(_value);
	Invalidate();
end;

procedure TIconLabel.SetIconHint(const _value : string);
begin
	FIconHint := _value;
	Hint := _value;
	ShowHint := not _value.IsEmpty;
end;

function TIconLabel.GetIconChar(const _iconType : TIconLabelType) : string;
begin
	case _iconType of
		iltWarning :
		Result := #$26A0;
		iltError :
		Result := #$2716;
		iltInfo :
		Result := #$2139;
		iltQuestion :
		Result := #$2753;
		else
		Result := '';
	end;
end;

function TIconLabel.GetIconColor(const _iconType : TIconLabelType) : TColor;
const
	FNAME = 'TIconLabel.GetIconColor';
var
	isDark : Boolean;
begin
	isDark := TDarkModeHelper.GetActualThemeMode = tmDark;
	// Keep logging lightweight: this unit is part of GUI components, so avoid DebugUtils dependency.
	OutputDebugString(PChar(Format('%s: IsDarkMode: %s iconType: %d', [FNAME, BoolToStr(isDark, True), Ord(_iconType)])));

	case _iconType of
		iltWarning :
		if isDark then
			Result := $00AEFF // light orange
		else
			Result := $0080FF; // orange
		iltError :
		if isDark then
			Result := $7070FF // light red
		else
			Result := clRed;
		iltInfo :
		if isDark then
			Result := $FFCC66 // light blue
		else
			Result := clBlue;
		iltQuestion :
		if isDark then
			Result := $FF9966 // light navy
		else
			Result := clNavy;
		else
		Result := TDarkModeHelper.GetThemedTextColor;
	end;
	OutputDebugString(PChar(Format('%s: Resulting color: %d', [FNAME, Result])));
end;

function TIconLabel.GetImageIndex(const _iconType : TIconLabelType) : TImageIndex;
begin
	case _iconType of
		iltWarning :
		Result := FImageIndexWarning;
		iltError :
		Result := FImageIndexError;
		iltInfo :
		Result := FImageIndexInfo;
		iltQuestion :
		Result := FImageIndexQuestion;
		else
		Result := -1;
	end;
end;

procedure TIconLabel.Notification(_component : TComponent; _operation : TOperation);
begin
	inherited;
	if (_operation = opRemove) and (_component = FImages) then begin
		FImages := nil;
		Invalidate();
	end;
end;

procedure TIconLabel.SetImages(const _value : TCustomImageList);
begin
	if FImages <> _value then begin
		if Assigned(FImages) then
			FImages.RemoveFreeNotification(Self);
		FImages := _value;
		if Assigned(FImages) then
			FImages.FreeNotification(Self);
		Invalidate();
	end;
end;

procedure TIconLabel.Loaded;
begin
	inherited;
	FOrigCaption := Caption;
end;

function TIconLabel.DrawTextMeasured(const _text : string; _left : Integer) : Integer;
var
	R : TRect;
begin
	R := Rect(_left, 0, MaxInt, Height);
	DrawText(Canvas.Handle, PChar(_text), -1, R, DT_LEFT or DT_CALCRECT);
	Result := R.Right;
	DrawText(Canvas.Handle, PChar(_text), -1, R, DT_LEFT);
end;

procedure TIconLabel.Paint;
var
	origWidth : Integer;
begin
	if FIconType <> iltNone then begin

		Canvas.Font.Assign(Font);
		Canvas.Brush.Style := bsClear;

		// Draw original caption in themed style (DrawText handles '&' as accelerator prefix)
		Canvas.Font.Color := TDarkModeHelper.GetThemedTextColor;
		Canvas.Font.Style := [];
		origWidth := DrawTextMeasured(FOrigCaption + ' ', 0);

		// Draw icon + text in colored bold style
		Canvas.Font.Color := FIconColor;
		Canvas.Font.Style := [fsBold];

		if Assigned(FImages) and (GetImageIndex(FIconType) >= 0) then begin
			var
			imgY := (Height - FImages.Height) div 2;
			FImages.Draw(Canvas, origWidth, imgY, GetImageIndex(FIconType));
		end else begin
			// Fallback to unicode icon character
			DrawTextMeasured(GetIconChar(FIconType), origWidth);
		end;
	end else begin
		inherited;
	end;
end;

procedure Register;
begin
	RegisterComponents(SECTION_NAME, [TIconLabel]);
end;

end.
