unit RipGrepper.UI.BottomFrame;

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
	Vcl.ComCtrls,
	Vcl.ExtCtrls,
	System.Actions,
	Vcl.ActnList,
	RipGrepper.UI.IFrameEvents,
	System.Math,
	System.UITypes,
	System.ImageList,
	Vcl.ImgList;

type
	TRipGrepperBottomFrame = class(TFrame, IFrameEvents)
		pnlBottom : TPanel;
		StatusBar1 : TStatusBar;
		Timer1 : TTimer;
		ActionList : TActionList;
		ActionStatusBar : TAction;
		ImageList1 : TImageList;
		procedure ActionStatusBarUpdate(Sender : TObject);
		procedure FrameResize(Sender : TObject);
		procedure StatusBar1Click(Sender : TObject);
		procedure StatusBar1DrawPanel(StatusBar : TStatusBar; Panel : TStatusPanel; const Rect : TRect);
		procedure Timer1Timer(Sender : TObject);

		private
			FIsInitialized : Boolean;
			FSpinnerFrame : Integer;
			FStatusBarMessage : string;
			FStatusBarStatistic : string;
			FStatusBarStatus : string;
			function GetIsInitialized() : Boolean;
			function IsPanelInfoClicked : Boolean;
			procedure DrawStatusPanel(ACanvas : TCanvas; const ARect : TRect);
			procedure ShowAboutDialog;
			procedure SetRunningStatus;
			procedure SetStatusBarMsgElapsedTime;

		public
			constructor Create(AOwner : TComponent); override;
			procedure AfterHistObjChange;
			procedure Initialize();
			procedure AfterSearch;
			procedure BeforeSearch(var _bAbort : Boolean);
			procedure SetReadyStatus;
			procedure UpdateUIStyle(_sNewStyle : string = '');
			property IsInitialized : Boolean read GetIsInitialized;
			property StatusBarMessage : string read FStatusBarMessage write FStatusBarMessage;
			property StatusBarStatistic : string read FStatusBarStatistic write FStatusBarStatistic;
			property StatusBarStatus : string read FStatusBarStatus write FStatusBarStatus;
	end;

var
	BottomFrame : TRipGrepperBottomFrame;

implementation

uses
	RipGrepper.Common.Constants,
	RipGrepper.UI.MiddleFrame,
	System.StrUtils,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Helper.UI.DarkMode;

{$R *.dfm}

constructor TRipGrepperBottomFrame.Create(AOwner : TComponent);
begin
	inherited;
	BottomFrame := self;
	FIsInitialized := False;
end;

procedure TRipGrepperBottomFrame.ActionStatusBarUpdate(Sender : TObject);
begin
	StatusBar1.Panels[PNL_MESSAGE_IDX].Text := FStatusBarMessage;
	StatusBar1.Panels[PNL_STATUS_IDX].Text := FStatusBarStatus;
	StatusBar1.Panels[PNL_STATISTIC_IDX].Text := FStatusBarStatistic;
end;

procedure TRipGrepperBottomFrame.AfterHistObjChange;
begin
	SetStatusBarMsgElapsedTime();
	SetReadyStatus();
end;

procedure TRipGrepperBottomFrame.AfterSearch;
begin
	SetStatusBarMsgElapsedTime();
	SetReadyStatus();
end;

procedure TRipGrepperBottomFrame.BeforeSearch(var _bAbort : Boolean);
begin
	if _bAbort then begin
		// StatusBarStatistic := 'ERROR';
	end else begin;
		SetRunningStatus();
	end;
end;

procedure TRipGrepperBottomFrame.FrameResize(Sender : TObject);
begin
	var
	hisWidth := MainFrame.PanelHistory.Width;
	StatusBar1.Panels[PNL_STATISTIC_IDX].Width := hisWidth;
	StatusBar1.Panels[PNL_MESSAGE_IDX].Width := StatusBar1.Width - hisWidth - StatusBar1.Panels[PNL_STATUS_IDX].Width -
		StatusBar1.Panels[PNL_INFO_IDX].Width;
end;

function TRipGrepperBottomFrame.GetIsInitialized() : Boolean;
begin
	Result := FIsInitialized;
end;

procedure TRipGrepperBottomFrame.Initialize();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperBottomFrame.Initialize');

	if IsInitialized then begin
		dbgMsg.Msg('Already initialized');
		Exit;
	end;
	if IsInitialized then begin
		Exit;
	end;

	{$IFDEF STANDALONE}
	var
	bStandalone := True;
	{$ELSE}
	var
	bStandalone := False;
	{$ENDIF}
	if not bStandalone then begin
		Height := Height - 5;
	end;
	StatusBarMessage := Format(FORMAT_VERSION_INFO_IN_STATUSBAR, [MainFrame.ModuleNameAndVersion]);
	SetReadyStatus();
	FIsInitialized := True;
end;

function TRipGrepperBottomFrame.IsPanelInfoClicked : Boolean;
var
	pt : TPoint;
	i : Integer;
	xSum : Integer;
	panelIdx : Integer;
begin
	Result := True;
	pt := StatusBar1.ScreenToClient(Mouse.CursorPos);
	xSum := 0;
	panelIdx := -1;
	for i := 0 to StatusBar1.Panels.Count - 1 do begin
		xSum := xSum + StatusBar1.Panels[i].Width;
		if pt.X < xSum then begin
			panelIdx := i;
			Break;
		end;
	end;
	if panelIdx <> PNL_INFO_IDX then begin
		Result := False;
	end;
end;

procedure TRipGrepperBottomFrame.SetRunningStatus;
begin
	FSpinnerFrame := 0;
	Timer1.Enabled := True;
	FStatusBarStatus := 'RUNNING';
end;

procedure TRipGrepperBottomFrame.SetReadyStatus;
begin
	Timer1.Enabled := False;
	FStatusBarStatus := 'READY';
	StatusBar1.Invalidate;
end;

procedure TRipGrepperBottomFrame.Timer1Timer(Sender : TObject);
begin
	FSpinnerFrame := (FSpinnerFrame + 1) mod 12;
	StatusBar1.Invalidate;
end;

procedure TRipGrepperBottomFrame.SetStatusBarMsgElapsedTime;
var
	msg : string;
begin
	if (MainFrame.HistItemObject = nil) or (MainFrame.HistItemObject.ElapsedTimeText = '') then begin
	  	msg := Format(FORMAT_VERSION_INFO_IN_STATUSBAR, [MainFrame.ModuleNameAndVersion]);
	end else begin
		msg := Format('Search took %s seconds', 
		[MainFrame.HistItemObject.ElapsedTimeText]); 
	end;
	StatusBarMessage := msg;
end;

procedure TRipGrepperBottomFrame.ShowAboutDialog;
var
	dlg : TTaskDialog;
begin
	dlg := TTaskDialog.Create(self);
	try
		dlg.Caption := 'About DRipGrepper';
		dlg.Title := MainFrame.ModuleNameAndVersion;
	{$IFDEF STANDALONE}
			dlg.Text := 'GUI for Windows';
	{$ELSE}
			dlg.Text := 'Delphi IDE plugin';
	{$ENDIF}
		dlg.Text := dlg.Text + ' to parametrize ripgrep for superfast search.' + CRLF2 +
		{ } '<A HREF="https://github.com/mattia72/DripGrepper">https://github.com/mattia72/DripGrepper</A>';
		dlg.MainIcon := tdiInformation;
		dlg.Flags := [tfEnableHyperlinks]; // Enable hyperlink support
		dlg.CommonButtons := [tcbOk];
		dlg.Execute();
	finally
		dlg.Free;
	end;
end;

procedure TRipGrepperBottomFrame.StatusBar1Click(Sender : TObject);
begin
	if IsPanelInfoClicked then
		ShowAboutDialog;
end;

procedure TRipGrepperBottomFrame.DrawStatusPanel(ACanvas : TCanvas; const ARect : TRect);
const
	SEGMENT_COUNT = 12;
	TRAIL_LEN = 5;
var
	i, activeIdx, distFromActive : Integer;
	angleRad : Double;
	x1, y1, x2, y2, cx, cy, spinR : Integer;
	ratio : Double;
	fgR, fgG, fgB, bgR, bgG, bgB, segR, segG, segB : Byte;
	fgColor, bgColor : TColorRef;
	textLeft, textH, textY : Integer;
begin
	spinR := ARect.Height div 2 - 2;
	if spinR < 3 then
		spinR := 3;
	cx := ARect.Left + spinR + 3;
	cy := ARect.Top + ARect.Height div 2;

	// Capture panel colors before changing brush
	bgColor := ColorToRGB(ACanvas.Brush.Color);
	fgColor := ColorToRGB(ACanvas.Font.Color);
	fgR := GetRValue(fgColor);
	fgG := GetGValue(fgColor);
	fgB := GetBValue(fgColor);
	bgR := GetRValue(bgColor);
	bgG := GetGValue(bgColor);
	bgB := GetBValue(bgColor);

	// Fill background
	ACanvas.Brush.Style := bsSolid;
	ACanvas.FillRect(ARect);
	ACanvas.Brush.Style := bsClear;

	if Timer1.Enabled then begin
		// Draw spinner segments
		activeIdx := FSpinnerFrame;
		for i := 0 to SEGMENT_COUNT - 1 do begin
			angleRad := i * (2 * Pi / SEGMENT_COUNT);
			x1 := cx + Round(spinR * 0.45 * Sin(angleRad));
			y1 := cy - Round(spinR * 0.45 * Cos(angleRad));
			x2 := cx + Round(spinR * Sin(angleRad));
			y2 := cy - Round(spinR * Cos(angleRad));

			distFromActive := (activeIdx - i + SEGMENT_COUNT) mod SEGMENT_COUNT;
			if distFromActive < TRAIL_LEN then
				ratio := 1.0 - (distFromActive / TRAIL_LEN)
			else
				ratio := 0.05;

			segR := EnsureRange(Round(bgR + (fgR - bgR) * ratio), 0, 255);
			segG := EnsureRange(Round(bgG + (fgG - bgG) * ratio), 0, 255);
			segB := EnsureRange(Round(bgB + (fgB - bgB) * ratio), 0, 255);

			ACanvas.Pen.Color := RGB(segR, segG, segB);
			ACanvas.Pen.Width := 2;
			ACanvas.MoveTo(x1, y1);
			ACanvas.LineTo(x2, y2);
		end;
		textLeft := cx + spinR + 5;
	end else begin
		textLeft := ARect.Left + 4;
	end;

	// Draw status text
	ACanvas.Pen.Width := 1;
	ACanvas.Font.Color := fgColor;
	textH := ACanvas.TextHeight('Wg');
	textY := ARect.Top + (ARect.Height - textH) div 2;
	ACanvas.TextOut(textLeft, textY, FStatusBarStatus);
end;

procedure TRipGrepperBottomFrame.StatusBar1DrawPanel(StatusBar : TStatusBar; Panel : TStatusPanel; const Rect : TRect);
begin
	if Panel = StatusBar1.Panels[PNL_STATUS_IDX] then begin
		DrawStatusPanel(StatusBar.Canvas, Rect);
	end else if Panel = StatusBar1.Panels[PNL_INFO_IDX] then begin
		// Draw the info icon centered in the panel rect
		var
		x := Rect.Left + (Rect.Width - ImageList1.Width) div 2;
		var
		y := Rect.Top + (Rect.Height - ImageList1.Height) div 2;
		ImageList1.Draw(StatusBar.Canvas, x, y, 1);
	end;
end;

procedure TRipGrepperBottomFrame.UpdateUIStyle(_sNewStyle : string = '');
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperBottomFrame.UpdateUIStyle');

	if _sNewStyle.IsEmpty then begin
		StyleName := TDarkModeHelper.GetActualThemeName();
	end else begin
		StyleName := _sNewStyle;
	end;
	dbgMsg.Msg('Theme: ' + StyleName);
end;

end.
