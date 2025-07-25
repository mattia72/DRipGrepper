unit RipGrepper.UI.ColorSelectorFrame;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	Vcl.ActnMan,
	Vcl.ActnColorMaps,
	Vcl.StdCtrls,
	Vcl.ExtCtrls,
	RipGrepper.Settings.FontColors;

type
	TControlCanvasHelper = class
		private
			FCtrlCanvas : TControlCanvas;
			function GetCanvas(AControl : TControl) : TCanvas;
			procedure SetCanvas(const Value : TControlCanvas);

		public
			constructor Create;
			destructor Destroy; override;
			property Canvas : TControlCanvas read FCtrlCanvas write SetCanvas;
	end;

	TStaticText = class(Vcl.StdCtrls.TStaticText)
		strict private
		private
			FCtrlCanvas : TControlCanvas;
			function GetCtrlCanvas : TControlCanvas;
			property CtrlCanvas : TControlCanvas read GetCtrlCanvas;

		public
			destructor Destroy; override;
			procedure WMNCPaint(var Message : TWMNCPaint); message WM_NCPAINT;
	end;

	TColorSelectorFrame = class(TFrame)
		cbBackground : TColorBox;
		cbForeground : TColorBox;
		ExampleText : TStaticText;
		LabelText : TLabel;
		cbBold : TCheckBox;
		cbItalic : TCheckBox;
		cbUnderline : TCheckBox;
		FontDialog1 : TFontDialog;
		cbStrikeOut : TCheckBox;
		procedure cbBackgroundChange(Sender : TObject);
		procedure cbBoldClick(Sender : TObject);
		procedure cbForegroundChange(Sender : TObject);
		procedure cbItalicClick(Sender : TObject);
		procedure cbStrikeOutClick(Sender : TObject);
		procedure cbUnderlineClick(Sender : TObject);
		procedure ExampleTextDblClick(Sender : TObject);

		private
			FBSkipChangeEvent : Boolean;
			FSelectedFont : TFont;
			FSelectedFontAttributes : TFontAttributes;
			procedure Adjust(_chResize, _chMove : TCheckBox);
			procedure AdjustCheckBoxWidthToText(CheckBox : TCheckBox);
			function GetSelectedFont : TFont;
			function GetSelectedFontAttributes : TFontAttributes;
			procedure SetFontStylesByCheckBox;
			procedure UpdateFontStyle(_cb : TCheckBox; const _fs : TFontStyle);

		protected
			procedure Loaded; override;

		public
			constructor Create(AOwner : TComponent); reintroduce;
			destructor Destroy; override;
			class function AddSelectionFrames(const _fontColors : TFontColors; _parentForm : TForm; _parentCtrl : TWinControl) : integer;
			procedure AssignFontAttributes(const _fa : TFontAttributes);
			procedure Refresh;
			function GetSystemBgColor() : TColor;
			class procedure WriteColorSettings(var _fontColors : TFontColors; _parentForm : TForm);
			property SelectedFontAttributes : TFontAttributes read FSelectedFontAttributes write FSelectedFontAttributes;
			property SelectedFont : TFont read GetSelectedFont write FSelectedFont;
	end;

implementation

uses
	System.UITypes,
	System.Rtti,
	System.Math,
	RipGrepper.Common.Constants,
	System.TypInfo,
	System.RegularExpressions,
	RipGrepper.Tools.DebugUtils,
	{$IFNDEF STANDALONE}
	ToolsAPI,
	{$ENDIF}
	Vcl.Themes,
	RipGrepper.Helper.UI.DarkMode;

{$R *.dfm}

constructor TColorSelectorFrame.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	FSelectedFontAttributes.Style := [];
end;

destructor TColorSelectorFrame.Destroy;
begin
	FSelectedFont.Free;
	inherited;
end;

class function TColorSelectorFrame.AddSelectionFrames(const _fontColors : TFontColors; _parentForm : TForm; _parentCtrl : TWinControl)
	: integer;
var
	Context : TRttiContext;
	TypeFontColors : TRttiType;
	prop : TRttiField;
	NewFrame : TColorSelectorFrame;
	fa : TFontAttributes;
	colors : ^TFontColors;
	allHeight : integer;
begin
	allHeight := 0;
	Context := TRttiContext.Create;
	// _parentForm.Autosize := False;
	try
		TypeFontColors := Context.GetType(TypeInfo(TFontColors));
		for prop in TypeFontColors.GetFields do begin
			if prop.Visibility = mvPublic then begin
				colors := @_fontColors;
				fa := prop.GetValue(colors).AsType<TFontAttributes>;

				NewFrame := TColorSelectorFrame.Create(_parentForm);
				NewFrame.Name := prop.Name + COMPONENT_NAME_COLORSELECTOR;
				_parentForm.InsertComponent(NewFrame); // !!!
				NewFrame.Parent := _parentCtrl;
				NewFrame.Align := alTop;
				NewFrame.LabelText.Caption := TRegex.Replace(prop.Name, '[A-Z]', ' $0') + ':';
				NewFrame.AssignFontAttributes(fa);

				var
				heightInc := NewFrame.Height +
				{ } IfThen(NewFrame.AlignWithMargins,
					{ } NewFrame.Margins.Top + NewFrame.Margins.Bottom, 2);

				Inc(allHeight, heightInc);

				// _parentForm.Height := _parentForm.Height + heightInc;
				// _parentCtrl.Height := _parentCtrl.Height + heightInc;
			end;
		end;
		Result := allHeight;
	finally
		Context.Free;
		_parentForm.Autosize := True;
	end;
end;

procedure TColorSelectorFrame.Adjust(_chResize, _chMove : TCheckBox);
begin
	AdjustCheckBoxWidthToText(_chResize);
	_chMove.Left := _chResize.Left + _chResize.Width + _chResize.Margins.Left;
end;

procedure TColorSelectorFrame.AssignFontAttributes(const _fa : TFontAttributes);
begin
	FSelectedFontAttributes := _fa;
	Refresh;
end;

procedure TColorSelectorFrame.cbBackgroundChange(Sender : TObject);
begin
	if FBSkipChangeEvent then
		Exit;
	FSelectedFontAttributes.BgColor := cbBackground.Selected;
	Refresh;
end;

procedure TColorSelectorFrame.cbBoldClick(Sender : TObject);
begin
	SetFontStylesByCheckBox();
end;

procedure TColorSelectorFrame.cbForegroundChange(Sender : TObject);
begin
	if FBSkipChangeEvent then
		Exit;
	FSelectedFontAttributes.Color := cbForeground.Selected;
	Refresh;
end;

procedure TColorSelectorFrame.cbItalicClick(Sender : TObject);
begin
	SetFontStylesByCheckBox();
end;

procedure TColorSelectorFrame.cbStrikeOutClick(Sender : TObject);
begin
	SetFontStylesByCheckBox();
end;

procedure TColorSelectorFrame.cbUnderlineClick(Sender : TObject);
begin
	SetFontStylesByCheckBox();
end;

procedure TColorSelectorFrame.ExampleTextDblClick(Sender : TObject);
begin
	FontDialog1.Font.Assign(SelectedFont);
	if FontDialog1.Execute(self.Handle) then begin
		SelectedFont.Assign(FontDialog1.Font);
		FSelectedFontAttributes.FromFont(SelectedFont);
		Refresh;
	end;
end;

function TColorSelectorFrame.GetSelectedFont : TFont;
begin
	if not Assigned(FSelectedFont) then begin
		FSelectedFont := TFont.Create();
		FSelectedFont.Style := [];
	end;
	Result := FSelectedFont;
end;

function TColorSelectorFrame.GetSelectedFontAttributes : TFontAttributes;
begin
	FSelectedFontAttributes.FromFont(ExampleText.Font);
	Result.FromString(FSelectedFontAttributes.ToString);
end;

procedure TColorSelectorFrame.Loaded;
begin
	inherited;
	ExampleText.Constraints.MaxHeight := ExampleText.Height;
	ExampleText.Constraints.MaxWidth := ExampleText.Width;
	ExampleText.Constraints.MinHeight := ExampleText.Height;
	ExampleText.Constraints.MinWidth := ExampleText.Width;
	ExampleText.Top := cbForeground.Top + Trunc(cbForeground.Height / 2) - Trunc(ExampleText.Height / 2);
	cbBackground.AutoDropDownWidth := True;
	cbForeground.AutoDropDownWidth := True;

	Adjust(cbBold, cbItalic);
	Adjust(cbItalic, cbUnderline);
	Adjust(cbUnderline, cbStrikeOut);
	AdjustCheckBoxWidthToText(cbStrikeOut);
end;

procedure TColorSelectorFrame.AdjustCheckBoxWidthToText(CheckBox : TCheckBox);
var
	TextWidth : Integer;
begin
	var
	ccch := TControlCanvasHelper.Create();
	try
		var
		canvas := ccch.GetCanvas(CheckBox);
		canvas.Font := CheckBox.Font;
		TextWidth := Canvas.TextWidth(CheckBox.Caption);
		CheckBox.Width := TextWidth + 20;
	finally
		ccch.Free;
	end;
end;

procedure TColorSelectorFrame.Refresh;
begin
	GetSelectedFont; // Create FSelectedFont if not exists
	SelectedFontAttributes.ToFont(FSelectedFont);
	ExampleText.Font.Assign(FSelectedFont);
	if SelectedFontAttributes.BgColor = clNone then begin
		ExampleText.Color := GetSystemBgColor;
	end else begin
		ExampleText.Color := FSelectedFontAttributes.BgColor;
	end;

	FBSkipChangeEvent := True;
	try
		cbForeground.Selected := ExampleText.Font.Color;
		cbForeground.Hint := 'Foreground: ' + ColorToString(cbForeground.Selected);

		if SelectedFontAttributes.BgColor = clNone then begin
			cbBackground.Selected := GetSystemBgColor();
		end else begin
			cbBackground.Selected := FSelectedFontAttributes.BgColor;
		end;
		cbBackground.Hint := 'Background: ' + ColorToString(cbBackground.Selected);

		cbItalic.Checked := fsItalic in ExampleText.Font.Style;
		cbBold.Checked := fsBold in ExampleText.Font.Style;
		cbUnderline.Checked := fsUnderline in ExampleText.Font.Style;
		cbStrikeOut.Checked := fsStrikeOut in ExampleText.Font.Style;
	finally
		FBSkipChangeEvent := False;
	end;
end;

procedure TColorSelectorFrame.SetFontStylesByCheckBox;
begin
	if FBSkipChangeEvent then
		Exit;

	UpdateFontStyle(cbItalic, fsItalic);
	UpdateFontStyle(cbBold, fsBold);
	UpdateFontStyle(cbUnderline, fsUnderline);
	UpdateFontStyle(cbStrikeOut, fsStrikeOut);
	Refresh;
end;

function TColorSelectorFrame.GetSystemBgColor() : TColor;
begin
	{$IFDEF STANDALONE}
	Result := TStyleManager.ActiveStyle.GetSystemColor(clWindow);
	{$ELSE}
	Result := TDarkModeHelper.GetIdeSystemColor(clWindow);
	{$ENDIF};
end;

procedure TColorSelectorFrame.UpdateFontStyle(_cb : TCheckBox; const _fs : TFontStyle);
var
	styles : TFontStyles;
begin
	styles := FSelectedFontAttributes.Style;
	if _cb.Checked then begin
		styles := styles + [_fs];
	end else begin
		styles := styles - [_fs]
	end;
	FSelectedFontAttributes.Style := styles;
end;

class procedure TColorSelectorFrame.WriteColorSettings(var _fontColors : TFontColors; _parentForm : TForm);
var
	compName : string;
	comp : TComponent;
	csf : TColorSelectorFrame;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TColorSelectorFrame.WriteColorSettings');
	var
	fc := _fontColors;
	for var i := 0 to _parentForm.ComponentCount - 1 do begin
		comp := _parentForm.Components[i];
		if not Assigned(comp) then begin
			dbgMsg.ErrorMsgFmt('%i Component not exists?', [comp]);
			continue;
		end;
		compName := comp.Name;
		if compName.EndsWith(COMPONENT_NAME_COLORSELECTOR) then begin
			csf := comp as TColorSelectorFrame;
			if Assigned(csf) then begin
				fc.SetByName(compName.Replace(COMPONENT_NAME_COLORSELECTOR, ''),
					{ } csf.GetSelectedFontAttributes);
			end else begin
				dbgMsg.ErrorMsgFmt('%s settings not saved.', [compName]);
			end;
		end;
	end;
	_fontColors := fc;
end;

destructor TStaticText.Destroy;
begin
	FCtrlCanvas.Free;
	inherited;
end;

function TStaticText.GetCtrlCanvas : TControlCanvas;
begin
	if not Assigned(FCtrlCanvas) then begin
		FCtrlCanvas := TControlCanvas.Create;
		FCtrlCanvas.Control := self;
	end;
	Result := FCtrlCanvas;
end;

procedure TStaticText.WMNCPaint(var Message : TWMNCPaint);
begin
	inherited;
	var
	frame := Parent as TColorSelectorFrame;
	CtrlCanvas.Font := frame.SelectedFont;
	CtrlCanvas.Font.Color := frame.SelectedFontAttributes.Color;
	CtrlCanvas.Brush.Color := frame.SelectedFontAttributes.BgColor;
	SetBkMode(CtrlCanvas.Handle, Ord(OPAQUE)); // TRANSPARENT or OPAQUE
	CtrlCanvas.FillRect(Rect(0, 0, Width, Height));
	CtrlCanvas.TextOut(0, 0, Caption);
end;

constructor TControlCanvasHelper.Create;
begin
	inherited Create();
	FCtrlCanvas := TControlCanvas.Create;
end;

destructor TControlCanvasHelper.Destroy;
begin
	FCtrlCanvas.Free;
	inherited;
end;

function TControlCanvasHelper.GetCanvas(AControl : TControl) : TCanvas;
begin
	FCtrlCanvas.Control := AControl;
	Result := FCtrlCanvas;
end;

procedure TControlCanvasHelper.SetCanvas(const Value : TControlCanvas);
begin
	FCtrlCanvas := Value;
end;

end.
