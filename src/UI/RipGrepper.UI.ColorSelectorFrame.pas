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
			function GetFGColor : TColor;
			function GetBGColor : TColor;
			procedure SetFGColor(const Value : TColor);
			procedure SetBGColor(const Value : TColor);

		protected
			procedure Loaded; override;

		public
			constructor Create(AOwner : TComponent); reintroduce;
			procedure Refresh;
			property FGColor : TColor read GetFGColor write SetFGColor;
			property BGColor : TColor read GetBGColor write SetBGColor;
			{ Public-Deklarationen }
	end;

implementation

uses
	System.UITypes;

{$R *.dfm}

constructor TColorSelectorFrame.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);

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
	FontDialog1.Font.Assign(ExampleText.Font);
	if FontDialog1.Execute(self.Handle) then begin
		ExampleText.Font.Assign(FontDialog1.Font);
		Refresh;
	end;
end;

function TColorSelectorFrame.GetFGColor : TColor;
begin
	Result := cbForeground.Selected;
end;

procedure TColorSelectorFrame.Loaded;
begin
	inherited;
	cbBackground.Selected := ExampleText.Color;
	cbForeground.Selected := ExampleText.Font.Color;
	ExampleText.Constraints.MaxHeight := ExampleText.Height;
	ExampleText.Constraints.MaxWidth := ExampleText.Width;
	ExampleText.Constraints.MinHeight := ExampleText.Height;
	ExampleText.Constraints.MinWidth := ExampleText.Width;
	ExampleText.Top := cbForeground.Top + Trunc(cbForeground.Height / 2) - Trunc(ExampleText.Height / 2);
end;

function TColorSelectorFrame.GetBGColor : TColor;
begin
	Result := cbBackground.Selected;
end;

procedure TColorSelectorFrame.Refresh;
begin
	ExampleText.Refresh;
	FBSkipChangeEvent := True;
	try
		FGColor := ExampleText.Font.Color;
		BGColor := ExampleText.Color;

		cbItalic.Checked := fsItalic in ExampleText.Font.Style;
		cbBold.Checked := fsBold in ExampleText.Font.Style;
		cbUnderline.Checked := fsUnderline in ExampleText.Font.Style;
		cbStrikeOut.Checked := fsStrikeOut in ExampleText.Font.Style;
	finally
		FBSkipChangeEvent := False;
	end;
end;

procedure TColorSelectorFrame.SetFGColor(const Value : TColor);
begin
	cbForeground.Selected := Value;
	cbForegroundChange(self);
end;

procedure TColorSelectorFrame.SetBGColor(const Value : TColor);
begin
	cbBackground.Selected := Value;
	cbBackgroundChange(self);
end;

end.
