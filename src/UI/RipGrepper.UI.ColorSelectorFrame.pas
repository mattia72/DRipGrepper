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
	Vcl.ExtCtrls;

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
		procedure cbForegroundChange(Sender : TObject);
		procedure ExampleTextDblClick(Sender : TObject);

		private
			FBSkipChangeEvent : Boolean;
			FSelectedBackground: TColor;
			FSelectedFont : TFont;
		protected
			procedure Loaded; override;

		public
			constructor Create(AOwner : TComponent); reintroduce;
		procedure Assign(const _font: TFont; const _colorBackground: TColor);
			procedure Refresh;
			property SelectedBackground: TColor read FSelectedBackground write FSelectedBackground;
			property SelectedFont : TFont read FSelectedFont write FSelectedFont;

	end;

implementation

uses
	System.UITypes;

{$R *.dfm}

constructor TColorSelectorFrame.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);

end;

procedure TColorSelectorFrame.Assign(const _font: TFont; const _colorBackground: TColor);
begin
	SelectedBackground := _colorBackground;
    SelectedFont := _font;
    Refresh;
end;

procedure TColorSelectorFrame.cbBackgroundChange(Sender : TObject);
begin
	if FBSkipChangeEvent then
		Exit;
	ExampleText.Color := cbBackground.Selected;
	ExampleText.Refresh;
end;

procedure TColorSelectorFrame.cbForegroundChange(Sender : TObject);
begin
	if FBSkipChangeEvent then
		Exit;

	ExampleText.Font.Color := cbForeground.Selected;
	ExampleText.Refresh;
end;

procedure TColorSelectorFrame.ExampleTextDblClick(Sender : TObject);
begin
	FontDialog1.Font.Assign(FSelectedFont);
	if FontDialog1.Execute(self.Handle) then begin
		FSelectedFont.Assign(FontDialog1.Font);
		Refresh;
	end;
end;

procedure TColorSelectorFrame.Loaded;
begin
	inherited;
	ExampleText.Constraints.MaxHeight := ExampleText.Height;
	ExampleText.Constraints.MaxWidth := ExampleText.Width;
	ExampleText.Constraints.MinHeight := ExampleText.Height;
	ExampleText.Constraints.MinWidth := ExampleText.Width;
	ExampleText.Top := cbForeground.Top + Trunc(cbForeground.Height / 2) - Trunc(ExampleText.Height / 2);
end;

procedure TColorSelectorFrame.Refresh;
begin
	ExampleText.Font.Assign(FSelectedFont);
    ExampleText.Color := FSelectedBackground;

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

end.
