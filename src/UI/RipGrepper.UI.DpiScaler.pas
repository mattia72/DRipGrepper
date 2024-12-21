unit RipGrepper.UI.DpiScaler;

interface

uses
	System.Classes,
	Vcl.Controls,
	System.Types,
	Winapi.Messages,
	Winapi.Windows,
	Vcl.ImgList;

type
	TRipGrepperDpiScaler = class
		const
			DevImgSIZE = 16;

		private
			FOwner : TWinControl;
			FMsgHandlerHWND : HWND;
			FScaledImageList : TImageList;

			procedure WndMethod(var Msg : TMessage);
			procedure ApplyDpiForImageListComponents(_NewDpi : Integer; _NewBounds : PRect);
			procedure CopyImages(_src : TCustomImageList; _dst : TImageList); overload;
			procedure CopyImages(_src : TImageList; _dst : TCustomImageList); overload;
			procedure ScaleImageList(_imgList : TImageList; const _dpi : integer);

		public
			constructor Create(AOwner : TWinControl; _bScaleImgLists : Boolean = True);
			destructor Destroy; override;
			class function GetActualDPI : Integer;
			procedure ScaleImageListToActDpi(_imgList : TImageList); overload;
			procedure ScaleImageListToActDpi(_imgList : TCustomImageList); overload;
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
		ApplyDpiForImageListComponents(GetActualDPI, nil);
	end;
end;

destructor TRipGrepperDpiScaler.Destroy;
begin
	DeallocateHWnd(FMsgHandlerHWND);
	FScaledImageList.Free;
	inherited;
end;

procedure TRipGrepperDpiScaler.ApplyDpiForImageListComponents(_NewDpi : Integer; _NewBounds : PRect);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperDpiScaler.ApplyDpiForImageListComponents', True);
	dbgMsg.Msg('NewDpi: ' + _NewDpi.ToString);

	for var i := 0 to -1 + FOwner.ComponentCount do begin
		if FOwner.Components[i] is TImageList then begin
			ScaleImageListToActDpi(TImageList(FOwner.Components[i]));
		end;
	end;
end;

procedure TRipGrepperDpiScaler.CopyImages(_src : TCustomImageList; _dst : TImageList);
begin
	// add from source image list
	for var ii := 0 to _src.Count - 1 do begin
		_dst.AddImage(_src, ii);
	end;
end;

procedure TRipGrepperDpiScaler.CopyImages(_src : TImageList; _dst : TCustomImageList);
begin
	// add from source image list
	for var ii := 0 to _dst.Count - 1 do begin
		_src.AddImage(_dst, ii);
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

procedure TRipGrepperDpiScaler.ScaleImageList(_imgList : TImageList; const _dpi : integer);
var
	mb : TBitmap;
	ib : TBitmap;
	sib : TBitmap;
	smb : TBitmap;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperDpiScaler.ScaleImageList', True);

	FScaledImageList.Clear;
	// add from source image list
	for var ii := 0 to _imgList.Count - 1 do begin
		FScaledImageList.AddImage(_imgList, ii);
	end;
	// set size to match _dpi size (like 250% of 16px = 40px)
	_imgList.SetSize(MulDiv(DevImgSIZE, _dpi, 96), MulDiv(DevImgSIZE, _dpi, 96));

	// add images back to original ImageList stretched (if _dpi scaling > 150%) or centered (if _dpi scaling <= 150%)
	for var ii := 0 to -1 + FScaledImageList.Count do begin
		sib := TBitmap.Create; // stretched (or centered) image
		smb := TBitmap.Create; // stretched (or centered) mask
		try
			sib.Width := _imgList.Width;
			sib.Height := _imgList.Height;
			sib.Canvas.FillRect(sib.Canvas.ClipRect);
			smb.Width := _imgList.Width;
			smb.Height := _imgList.Height;
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

				if _dpi * 100 / 96 <= 150 then // center if <= 150%
				begin
					sib.Canvas.Draw((sib.Width - ib.Width) div 2, (sib.Height - ib.Height) div 2, ib);
					smb.Canvas.Draw((smb.Width - mb.Width) div 2, (smb.Height - mb.Height) div 2, mb);
					dbgMsg.MsgIf(ii = 0, 'Images redrawed Centered');
				end else begin // stretch if > 150%
					sib.Canvas.StretchDraw(Rect(0, 0, sib.Width, sib.Width), ib);
					smb.Canvas.StretchDraw(Rect(0, 0, smb.Width, smb.Width), mb);
					dbgMsg.MsgIf(ii = 0,'Images redrawed Stretched');
				end;
			finally
				ib.Free;
				mb.Free;
			end;

			_imgList.Add(sib, smb);
		finally
			sib.Free;
			smb.Free;
		end;
	end;
end;

procedure TRipGrepperDpiScaler.ScaleImageListToActDpi(_imgList : TImageList);
var
	dpi : Integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperDpiScaler.ScaleImageListToActDpi');

	dpi := GetActualDPI;
	if dpi = 96 then begin
		dbgMsg.Msg('Skip');
		Exit;
	end;

	ScaleImageList(_imgList, dpi);
end;

procedure TRipGrepperDpiScaler.ScaleImageListToActDpi(_imgList : TCustomImageList);
var
	dpi : Integer;
	il : TImageList;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperDpiScaler.ScaleImageListToActDpi');

	dpi := GetActualDPI;
	if dpi = 96 then begin
		dbgMsg.Msg('Skip');
		Exit;
	end;

	il := TImageList.Create(nil);
	try
		CopyImages(_imgList, il); // copy to custom to normal image list

		ScaleImageList(il, dpi);

		_imgList.SetSize(MulDiv(DevImgSIZE, dpi, 96), MulDiv(DevImgSIZE, dpi, 96));
		CopyImages(il, _imgList); // copy back

	finally
		il.Free;
	end;
end;

procedure TRipGrepperDpiScaler.WndMethod(var Msg : TMessage);
begin
	case Msg.Msg of
		WM_DPICHANGED : begin
			TDebugUtils.DebugMessage('TRipGrepperDpiScaler.WndMethod - WM_DPICHANGED');
			ApplyDpiForImageListComponents(Msg.WParamHi, PRect(Msg.LParam));
			Msg.Result := 0;
		end;
		else begin
			Msg.Result := DefWindowProc(FMsgHandlerHWND, Msg.Msg, Msg.wParam, Msg.lParam);
		end;
	end;
end;

end.
