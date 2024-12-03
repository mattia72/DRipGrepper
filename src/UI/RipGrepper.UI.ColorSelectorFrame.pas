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
			FSelectedFontAttributes : TFontAttributes;
			FSelectedFont : TFont;
			function GetSelectedFont : TFont;
			function GetSelectedFontAttributes : TFontAttributes;
			procedure SetFontStylesByCheckBox;
			procedure UpdateFontStyle(_chb : TCheckBox; const _fs : TFontStyle);

		protected
			procedure Loaded; override;

		public
			constructor Create(AOwner : TComponent); reintroduce;
			destructor Destroy; override;
			procedure AssignFontAttributes(const _fa : TFontAttributes);
			procedure Refresh;
			property SelectedFontAttributes : TFontAttributes read GetSelectedFontAttributes write FSelectedFontAttributes;
			property SelectedFont : TFont read GetSelectedFont write FSelectedFont;

	end;

implementation

uses
	System.UITypes;

{$R *.dfm}

constructor TColorSelectorFrame.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
end;

destructor TColorSelectorFrame.Destroy;
begin
	FSelectedFont.Free;
	inherited;
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
	FSelectedFontAttributes.Color := SelectedFont.Color;
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
	end;
	Result := FSelectedFont;
end;

function TColorSelectorFrame.GetSelectedFontAttributes : TFontAttributes;
begin
	FSelectedFontAttributes.FromFont(SelectedFont);
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
end;

procedure TColorSelectorFrame.Refresh;
begin
	GetSelectedFont; // Create FSelectedFont if not exists
	FSelectedFontAttributes.ToFont(FSelectedFont);
	ExampleText.Font.Assign(FSelectedFont);
	ExampleText.Color := FSelectedFontAttributes.BgColor;

	FBSkipChangeEvent := True;
	try
		cbForeground.Selected := ExampleText.Font.Color;
		cbBackground.Selected := ExampleText.Color;

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

procedure TColorSelectorFrame.UpdateFontStyle(_chb : TCheckBox; const _fs : TFontStyle);
var
	styles : TFontStyles;
begin
	styles := FSelectedFontAttributes.Style;
	if _chb.Checked then begin
		styles := styles + [_fs];
	end else begin
		styles := styles - [_fs]
	end;
	FSelectedFontAttributes.Style := styles;
end;

end.
