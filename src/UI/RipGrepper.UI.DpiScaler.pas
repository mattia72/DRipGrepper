unit RipGrepper.UI.DpiScaler;

interface

uses
	System.Classes,
	Vcl.Controls,
	System.Types,
	Winapi.Messages,
	Winapi.Windows;

type
	TRipGrepperDpiScaler = class

		private
			FOwner : TWinControl;
			FMsgHandlerHWND : HWND;
			FScaledImageList : TImageList;

			procedure WndMethod(var Msg : TMessage);
			procedure ApplyDpiForImageLists(_NewDpi : Integer; _NewBounds : PRect);

		public
			constructor Create(AOwner : TWinControl; _bScaleImgLists : Boolean = True);
			destructor Destroy; override;
			class function GetActualDPI : Integer;
			function ResizeImageListImagesforHighDPI(imgList : TImageList) : TImageList;
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

constructor TRipGrepperDpiScaler.Create(AOwner : TWinControl; _bScaleImgLists : Boolean = True);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperDpiScaler.Create', True);
	dbgMsg.Msg('Owner: ' + AOwner.Name);
	FOwner := AOwner;
	FScaledImageList := TImageList.Create(nil);
	FMsgHandlerHWND := AllocateHWnd(WndMethod);
	if _bScaleImgLists then begin
		ApplyDpiForImageLists(GetActualDPI, nil);
	end;
end;

destructor TRipGrepperDpiScaler.Destroy;
begin
	DeallocateHWnd(FMsgHandlerHWND);
	FScaledImageList.Free;
	inherited;
end;

procedure TRipGrepperDpiScaler.ApplyDpiForImageLists(_NewDpi : Integer; _NewBounds : PRect);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperDpiScaler.ApplyDpiForImageLists', True);
	dbgMsg.Msg('NewDpi: ' + _NewDpi.ToString);

	for var i := 0 to -1 + FOwner.ComponentCount do begin
		if FOwner.Components[i] is TImageList then begin
			ResizeImageListImagesforHighDPI(TImageList(FOwner.Components[i]));
		end;
	end;
end;

class function TRipGrepperDpiScaler.GetActualDPI : Integer;
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

function TRipGrepperDpiScaler.ResizeImageListImagesforHighDPI(imgList : TImageList) : TImageList;
const
	DevImgSIZE = 16;
var
	ii : integer;
	mb, ib, sib, smb : TBitmap;
	dpi : Integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperDpiScaler.ResizeImageListImagesforHighDPI');

	dpi := GetActualDPI;
	if dpi = 96 then begin
		dbgMsg.Msg('Skip');
		Exit;
	end;

	// add from source image list
	for ii := 0 to -1 + imgList.Count do begin
		FScaledImageList.AddImage(imgList, ii);
	end;

	// set size to match DPI size (like 250% of 16px = 40px)
	imgList.SetSize(MulDiv(DevImgSIZE, dpi, 96), MulDiv(DevImgSIZE, dpi, 96));

	// add images back to original ImageList stretched (if DPI scaling > 150%) or centered (if DPI scaling <= 150%)
	for ii := 0 to -1 + FScaledImageList.Count do begin
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
					{ } FScaledImageList.Handle, ii, ib.Canvas.Handle, 0, 0, ib.Width, ib.Height, CLR_NONE, CLR_NONE, ILD_NORMAL);
				ImageList_DrawEx(
					{ } FScaledImageList.Handle, ii, mb.Canvas.Handle, 0, 0, mb.Width, mb.Height, CLR_NONE, CLR_NONE, ILD_MASK);

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
end;

procedure TRipGrepperDpiScaler.WndMethod(var Msg : TMessage);
begin
	case Msg.Msg of
		WM_DPICHANGED : begin
			TDebugUtils.DebugMessage('TRipGrepperDpiScaler.WndMethod - WM_DPICHANGED');
			ApplyDpiForImageLists(Msg.WParamHi, PRect(Msg.LParam));
			Msg.Result := 0;
		end;
		else begin
			Msg.Result := DefWindowProc(FMsgHandlerHWND, Msg.Msg, Msg.wParam, Msg.lParam);
		end;
	end;
end;

end.
