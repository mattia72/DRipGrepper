unit RipGrepper.UI.DpiScaler;

interface

uses
	System.Classes,
	u_dzDpiScaleUtils,
	Vcl.Controls,
	System.Types,
	Winapi.Messages,
	Winapi.Windows;

type
	TRipGrepperDpiScaler = class

		private
			FOwner : TWinControl;
			// FImageScaler : TImageListScaler;
			FWinDpiScaler : TFormDpiScaler;
			FMsgHandlerHWND : HWND;
			FScaleImageList : TImageList;

			function GetActualDPI : Integer;
			// procedure ApplyDpiForImages(_NewDpi : Integer);
			procedure ResizeImageListImagesforHighDPI(imgList : TImageList);
			procedure WndMethod(var Msg : TMessage);
			procedure SetButtonImages(_ctrl : TControl; _imgList : TImageList);
			procedure ApplyDpi(_NewDpi : Integer; _NewBounds : PRect);
			procedure ApplyDpiForWindow(_NewDpi : Integer; _NewBounds : PRect);
			procedure ArrangeControls;
			procedure FindImageListForDpiScaler(_parent : TComponent);
			procedure InitDpiScaler;
			procedure InitImageListScaler(_imgList : TImageList);

		public
			constructor Create(AOwner : TWinControl);
			destructor Destroy; override;
			property ActualDPI : Integer read GetActualDPI;
	end;

implementation

uses
	System.SysUtils,
	RipGrepper.Tools.DebugUtils,
	Vcl.StdCtrls,
	Vcl.ComCtrls,
	Vcl.Forms,
	Vcl.ExtCtrls,
	Vcl.Graphics,
	Winapi.CommCtrl;

constructor TRipGrepperDpiScaler.Create(AOwner : TWinControl);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperDpiScaler.Create', True);
	dbgMsg.Msg('Owner: ' + AOwner.Name);
	FOwner := AOwner;
	FMsgHandlerHWND := AllocateHWnd(WndMethod);
	// set FScaleImageList
	// FindImageListForDpiScaler(FOwner);
	InitDpiScaler();
	// if not Assigned(FScaleImageList) then begin
	//
	// end;
end;

destructor TRipGrepperDpiScaler.Destroy;
begin
	DeallocateHWnd(FMsgHandlerHWND);
	FWinDpiScaler.Free;
	// FImageScaler.Free;
	inherited;
end;

procedure TRipGrepperDpiScaler.ApplyDpi(_NewDpi : Integer; _NewBounds : PRect);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperDpiScaler.ApplyDpi', True);
	dbgMsg.Msg('NewDpi: ' + _NewDpi.ToString);
	// ApplyDpiForWindow(_NewDpi, _NewBounds); // TODO: check it, so doesn't work on highdpi monitor
	for var i := 0 to -1 + FOwner.ComponentCount do begin
		if FOwner.Components[i] is TImageList then begin
			ResizeImageListImagesforHighDPI(TImageList(FOwner.Components[i]));
		end;
	end;
	// ApplyDpiForImages(_NewDpi);
end;

// procedure TRipGrepperDpiScaler.ApplyDpiForImages(_NewDpi : Integer);
// var
// li : TImageList;
// begin
// if Assigned(FScaleImageList) then begin
// if not Assigned(FImageScaler) then begin
// FImageScaler := TImageListScaler.Create(FOwner, FScaleImageList);
// TDebugUtils.DebugMessage('TRipGrepperDpiScaler.ApplyDpi ImageScaler Created - ' + FScaleImageList.Name);
// end;
// li := FImageScaler.GetScaledList(_NewDpi);
// SetButtonImages(FOwner, li);
// end else begin
// TDebugUtils.DebugMessage('TRipGrepperDpiScaler.ApplyDpi No FScaleImageList to scale');
// end;
// end;

procedure TRipGrepperDpiScaler.ApplyDpiForWindow(_NewDpi : Integer; _NewBounds : PRect);
begin
	TDebugUtils.DebugMessage('TRipGrepperDpiScaler.ApplyDpiForWindow');
	if Assigned(FWinDpiScaler) then begin
		FWinDpiScaler.ApplyDpi(ActualDPI, _NewBounds);
	end;
	ArrangeControls;
end;

procedure TRipGrepperDpiScaler.ArrangeControls;
begin
	//
end;

procedure TRipGrepperDpiScaler.FindImageListForDpiScaler(_parent : TComponent);
begin
	TDebugUtils.DebugMessage('TRipGrepperDpiScaler.FindImageListForDpiScaler for ' + _parent.Name);

	if Assigned(FScaleImageList) then begin
		TDebugUtils.DebugMessage('TRipGrepperDpiScaler.FindImageListForDpiScaler Exit - ' + FScaleImageList.Name);
		Exit;
	end;
	for var i := 0 to _parent.ComponentCount - 1 do begin
		var
		cmp := _parent.Components[i];
		if cmp is TImageList then begin
			TDebugUtils.DebugMessage('TRipGrepperDpiScaler.FindImageListForDpiScaler ' + cmp.Name);
			var
				imgList : TImageList := cmp as TImageList;
			if (imgList.Count > 0) then begin
				InitImageListScaler(imgList);
				break;
			end;
		end else if cmp.ComponentCount > 0 then begin
			TDebugUtils.DebugMessage('TRipGrepperDpiScaler.FindImageListForDpiScaler recursive call ' + cmp.Name);
			FindImageListForDpiScaler(cmp);
		end;
	end;
end;

function TRipGrepperDpiScaler.GetActualDPI : Integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperDpiScaler.GetActualDPI', True);
	var
	monitor := Screen.MonitorFromWindow(GetForegroundWindow());
	// GetCursorPos(cp);
	// monitor := Screen.MonitorFromPoint(cp) ;
	if (Assigned(monitor)) then begin
		dbgMsg.Msg('Monitor.ppi: ' + monitor.PixelsPerInch.ToString);
		Result := monitor.PixelsPerInch;
	end else begin
		dbgMsg.Msg('Screen.ppi: ' + monitor.PixelsPerInch.ToString);
		Result := Screen.PixelsPerInch;
	end;
end;

procedure TRipGrepperDpiScaler.InitDpiScaler;
begin
	FWinDpiScaler := TFormDpiScaler.Create(TForm(FOwner));
	TDebugUtils.DebugMessage(Format('TRipGrepperDpiScaler.InitDpiScaler - %s', [FOwner.Name]));
	ApplyDpi(ActualDPI, nil);
end;

procedure TRipGrepperDpiScaler.InitImageListScaler(_imgList : TImageList);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperDpiScaler.InitImageListScaler', True);
	if not Assigned(FScaleImageList) then begin
		self.FScaleImageList := _imgList;
		dbgMsg.Msg('TRipGrepperDpiScaler.InitImageListScaler ' + FScaleImageList.Name);
		// InitDpiScaler();
	end;
end;

procedure TRipGrepperDpiScaler.ResizeImageListImagesforHighDPI(imgList : TImageList);
const
	DevImgSIZE = 16;
var
	ii : integer;
	mb, ib, sib, smb : TBitmap;
	dpi : Integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperDpiScaler.ResizeImageListImagesforHighDPI');

	dpi := ActualDPI;
	if dpi = 96 then begin
		dbgMsg.Msg('Skip');
		Exit;
	end;

	FScaleImageList := TImageList.Create(nil);
	try

		// add from source image list
		for ii := 0 to -1 + imgList.Count do begin
			FScaleImageList.AddImage(imgList, ii);
		end;

		// set size to match DPI size (like 250% of 16px = 40px)
		imgList.SetSize(MulDiv(DevImgSIZE, dpi, 96), MulDiv(DevImgSIZE, dpi, 96));

		// add images back to original ImageList stretched (if DPI scaling > 150%) or centered (if DPI scaling <= 150%)
		for ii := 0 to -1 + FScaleImageList.Count do begin
			sib := TBitmap.Create; // stretched (or centered) image
			smb := TBitmap.Create; // stretched (or centered) mask
			try
				sib.Width := imgList.Width;
				sib.Height := imgList.Height;
				sib.Canvas.FillRect(sib.Canvas.ClipRect);
				smb.Width := imgList.Width;
				smb.Height := imgList.Height;
				smb.Canvas.FillRect(smb.Canvas.ClipRect);

				ib := TBitmap.Create;
				mb := TBitmap.Create;
				try
					ib.Width := DevImgSIZE;
					ib.Height := DevImgSIZE;
					ib.Canvas.FillRect(ib.Canvas.ClipRect);

					mb.Width := DevImgSIZE;
					mb.Height := DevImgSIZE;
					mb.Canvas.FillRect(mb.Canvas.ClipRect);

					ImageList_DrawEx(
						{ } FScaleImageList.Handle, ii, ib.Canvas.Handle, 0, 0, ib.Width, ib.Height, CLR_NONE, CLR_NONE, ILD_NORMAL);
					ImageList_DrawEx(
						{ } FScaleImageList.Handle, ii, mb.Canvas.Handle, 0, 0, mb.Width, mb.Height, CLR_NONE, CLR_NONE, ILD_MASK);

					if dpi * 100 / 96 <= 150 then // center if <= 150%
					begin
						dbgMsg.Msg('Centered');
						sib.Canvas.Draw((sib.Width - ib.Width) div 2, (sib.Height - ib.Height) div 2, ib);
						smb.Canvas.Draw((smb.Width - mb.Width) div 2, (smb.Height - mb.Height) div 2, mb);
					end else begin // stretch if > 150%
						dbgMsg.Msg('Stretched');
						sib.Canvas.StretchDraw(Rect(0, 0, sib.Width, sib.Width), ib);
						smb.Canvas.StretchDraw(Rect(0, 0, smb.Width, smb.Width), mb);
					end;
				finally
					ib.Free;
					mb.Free;
				end;

				imgList.Add(sib, smb);
			finally
				sib.Free;
				smb.Free;
			end;
		end;
	finally
		FScaleImageList.Free;
	end;
end;

procedure TRipGrepperDpiScaler.SetButtonImages(_ctrl : TControl; _imgList : TImageList);
var
	i : integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperDpiScaler.SetButtonImages', True);

	if _ctrl is TButton then begin
		dbgMsg.Msg('TButton' + _ctrl.Name);
		(_ctrl as TButton).Images := _imgList;
	end else if _ctrl is TButtonedEdit then begin
		dbgMsg.Msg('TButtonedEdit' + _ctrl.Name);
		(_ctrl as TButtonedEdit).Images := _imgList;
	end else if _ctrl is TToolBar then begin
		dbgMsg.Msg('TToolbar ' + _ctrl.Name);
		(_ctrl as TToolBar).Images := _imgList;
	end;

	if _ctrl is TWinControl then begin
		for i := 0 to (_ctrl as TWinControl).ControlCount - 1 do
			SetButtonImages((_ctrl as TWinControl).Controls[i], _imgList);
	end else begin
		// TDebugUtils.DebugMessage('TRipGrepperDpiScaler.SetButtonImages - Not a WinCtrl:' + _ctrl.Name);
	end;
end;

procedure TRipGrepperDpiScaler.WndMethod(var Msg : TMessage);
begin
	case Msg.Msg of
		WM_DPICHANGED : begin
			TDebugUtils.DebugMessage('TRipGrepperDpiScaler.WndMethod - WM_DPICHANGED');
			ApplyDpi(Msg.WParamHi, PRect(Msg.LParam));
			Msg.Result := 0;
		end;
		else begin
			Msg.Result := DefWindowProc(FMsgHandlerHWND, Msg.Msg, Msg.wParam, Msg.lParam);
		end;
	end;
end;

end.
