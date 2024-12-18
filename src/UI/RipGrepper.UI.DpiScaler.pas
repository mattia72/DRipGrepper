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
			FActualDPI : Integer;
			FOwner : TWinControl;

			FImageScaler : TImageListScaler;
			FWinDpiScaler : TFormDpiScaler;
			FMsgHandlerHWND : HWND;
			procedure ApplyDpiForImages(_NewDpi: Integer);
			procedure WndMethod(var Msg : TMessage);
			procedure SetButtonImages(_ctrl : TControl; _imgList : TImageList);

		protected
			FScaleImageList : TImageList;
			procedure ApplyDpi(_NewDpi : Integer; _NewBounds : PRect);
			procedure ApplyDpiForWindow(_NewDpi : Integer; _NewBounds : PRect);
			procedure ArrangeControls;
			procedure FindImageListForDpiScaler(_parent : TComponent);
			procedure InitDpiScaler;
			procedure InitImageListScaler(_imgList : TImageList);

		public
			constructor Create(AOwner : TWinControl);
			destructor Destroy; override;
			property ActualDPI : Integer read FActualDPI write FActualDPI;
	end;

implementation

uses
    System.SysUtils,
	RipGrepper.Tools.DebugUtils,
	Vcl.StdCtrls,
	Vcl.ComCtrls,
	Vcl.Forms;

constructor TRipGrepperDpiScaler.Create(AOwner : TWinControl);
begin
	TDebugUtils.DebugMessage('TRipGrepperDpiScaler.Create');
	FOwner := AOwner;
	FMsgHandlerHWND := AllocateHWnd(WndMethod);
	// set FScaleImageList
	FindImageListForDpiScaler(FOwner);
	InitDpiScaler();
	// if not Assigned(FScaleImageList) then begin
	//
	// end;
end;

destructor TRipGrepperDpiScaler.Destroy;
begin
	DeallocateHWnd(FMsgHandlerHWND);
	FWinDpiScaler.Free;
	FImageScaler.Free;
	inherited;
end;

procedure TRipGrepperDpiScaler.ApplyDpi(_NewDpi : Integer; _NewBounds : PRect);
begin
	TDebugUtils.DebugMessage('TRipGrepperDpiScaler.ApplyDpi - ' + _NewDpi.ToString);
	ApplyDpiForWindow(_NewDpi, _NewBounds);
	ApplyDpiForImages(_NewDpi);
end;

procedure TRipGrepperDpiScaler.ApplyDpiForImages(_NewDpi: Integer);
var
	li: TImageList;
begin
	if Assigned(FScaleImageList) then begin
		if not Assigned(FImageScaler) then begin
			FImageScaler := TImageListScaler.Create(FOwner, FScaleImageList);
			TDebugUtils.DebugMessage('TRipGrepperDpiScaler.ApplyDpi ImageScaler Created - ' + FScaleImageList.Name);
		end;
		li := FImageScaler.GetScaledList(_NewDpi);
		SetButtonImages(FOwner, li);
	end;
end;

procedure TRipGrepperDpiScaler.ApplyDpiForWindow(_NewDpi : Integer; _NewBounds : PRect);
begin
	TDebugUtils.DebugMessage('TRipGrepperDpiScaler.ApplyDpiForWindow');
	if Assigned(FWinDpiScaler) then begin
		FWinDpiScaler.ApplyDpi(_NewDpi, _NewBounds);
		FActualDPI := _NewDpi;
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

procedure TRipGrepperDpiScaler.InitDpiScaler;
begin
	FWinDpiScaler := TFormDpiScaler.Create(TForm(FOwner));

	FActualDPI := FOwner.PixelsPerInch;
	ApplyDpi(FActualDPI, nil);
end;

procedure TRipGrepperDpiScaler.InitImageListScaler(_imgList : TImageList);
begin
	TDebugUtils.DebugMessage('TRipGrepperDpiScaler.InitImageListScaler');
	if not Assigned(FScaleImageList) then begin
		self.FScaleImageList := _imgList;
		TDebugUtils.DebugMessage('TRipGrepperDpiScaler.InitImageListScaler ' + FScaleImageList.Name);
		// InitDpiScaler();
	end;
end;

procedure TRipGrepperDpiScaler.SetButtonImages(_ctrl : TControl; _imgList : TImageList);
var
	i : integer;
begin
	if _ctrl is TButton then begin
		TDebugUtils.DebugMessage('TRipGrepperDpiScaler.SetButtonImages - Button' + _ctrl.Name);
		(_ctrl as TButton).Images := _imgList;
	end else if _ctrl is TToolBar then begin
		TDebugUtils.DebugMessage('TRipGrepperDpiScaler.SetButtonImages - Toolbar ' + _ctrl.Name);
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
