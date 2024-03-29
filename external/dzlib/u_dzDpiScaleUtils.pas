unit u_dzDpiScaleUtils;

{$INCLUDE dzlib.inc}
{ .$DEFINE DPI_SCALER_LOGGING }

{$IFOPT d-}
{$UNDEF DPI_SCALER_LOGGING}
{$UNDEF SUPPORTS_INLINE}
{$ENDIF}

interface

uses
	Windows,
	SysUtils,
	Classes,
	Graphics,
	Forms,
	Controls,
	Types,
	ExtCtrls;

type
	PScaledImagesRec = ^TScaledImagesRec;

	TScaledImagesRec = record
		FDpi : Integer;
		FImages : TImageList;
	end;

type
	TImageListScaler = class(TComponent)
		private
			FScaledImages : array of TScaledImagesRec;
			FOriginal : TImageList;
			function ResizeImagesforHighDPI(_DPI : Integer) : TImageList;

		public
			constructor Create(_Owner : TComponent; _Original : TImageList); reintroduce;
			destructor Destroy; override;
			function GetScaledList(_DPI : Integer) : TImageList;
	end;

type
	TDpiScaler = record
		private
			FDesignDpi : Integer;
			FCurrentDpi : Integer;

		public
			procedure Init(_ctrl : TWinControl); overload; inline;
			procedure Init(_Dpi : Integer); overload; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
			procedure Init(_DesignDpi, _CurrentDpi : Integer); overload; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
			procedure SetCurrentDpi(_frm : TCustomForm); overload; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
			procedure SetCurrentDpi(_Dpi : Integer); overload; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
			function Calc(_Value : Integer) : Integer; overload; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
			function Calc(const _Value : TRect) : TRect; overload; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
			function ScaleFactorPercent : Integer;
	end;

type
	TCtrlDpiScaler = record
		Ctrl : TControl;
		BoundsRect : TRect;
		FontSize : Integer;
		ItemHeight : Integer;
		procedure Assign(_Ctrl : TControl);
		procedure ApplyScale(const _Scaler : TDpiScaler);
		procedure ResizeFont(const _Scaler : TDpiScaler);
	end;

	TFormDpiScaler = class
		private
			FDesignDpi : Integer;
			FClientWidth, FClientHeight : Integer;
			FMinWidth, FMinHeight : Integer;
			FMaxWidth, FMaxHeight : Integer;
			FFontSize : Integer;
			FFont : TFont;
			FFrm : TWinControl;
			FCtrlParams : TArray<TCtrlDpiScaler>;
			FPnlMaster : TPanel;
			procedure AddControls(_Ctrl : TWinControl);

		public
			constructor Create(_frm : TWinControl);
			destructor Destroy; override;
			procedure ApplyScale(const _Scaler : TDpiScaler);
			procedure ApplyDpi(_NewDpi : Integer; _NewBounds : PRect);
			function Calc(_Value : Integer) : Integer;
			property DesignDPI : Integer read FDesignDpi;
	end;

implementation

uses
	StdCtrls,
	CommCtrl,
	u_dzTypInfo,
	u_dzTypesUtils,
	u_dzMiscUtils;

function TWinControl_GetDesignDPI(_ctrl : TWinControl) : UINT;
begin
	{$IFDEF HAS_TFORM_GETDESIGNDPI}
	Result := _ctrl.PixelsPerInch;
	{$ELSE}
	Result := 96;
	{$ENDIF}
end;

type
	TGetDpiForWindow = function(_HWnd : HWND) : UINT; stdcall;

var
	GetDpiForWindow : TGetDpiForWindow = nil;

procedure InitApiCalls;
var
	Handle : Cardinal;
begin
	Handle := LoadLibrary('user32.dll');
	if Handle <> 0 then begin
		GetDpiForWindow := GetProcAddress(Handle, 'GetDpiForWindow');
	end;
end;

function TScreen_GetDpiForForm(_Frm : TCustomForm) : UINT;
begin
	Result := Screen.PixelsPerInch;
	if _Frm is TForm then begin
		if Assigned(GetDpiForWindow) then
			Result := GetDpiForWindow(_Frm.Handle);
	end;
end;

{$IFDEF DPI_SCALER_LOGGING}

var
	LogFile : Textfile;
	{$ENDIF}

procedure LogStr(const _s : string);
begin
	{$IFDEF DPI_SCALER_LOGGING}
	WriteLn(LogFile, _s);
	Flush(LogFile);
	{$ENDIF}
end;

procedure LogFmt(const _s : string; _Params : array of const);
begin
	LogStr(Format(_s, _Params));
end;

procedure LogRect(const _Prefix : string; const _Rect : TRect);
begin
	LogFmt('%s: (Left: %d, Top: %d, Right: %d, Bottom: %d, (Width: %d, Height: %d))', [_Prefix, _Rect.Left, _Rect.Top, _Rect.Right,
		_Rect.Bottom, TRect_Width(_Rect), TRect_Height(_Rect)]);
end;

procedure LogConstraints(const _Prefix : string; _Cstr : TSizeConstraints);
begin
	LogFmt('%s: (MinWidth, %d, MinHeight: %d, MaxWidth: %d, MaxHeight: %d)', [_Prefix, _Cstr.MinWidth, _Cstr.MinHeight, _Cstr.MaxWidth,
		_Cstr.MaxHeight]);
end;

procedure LogForm(const _Prefix : string; _frm : TWinControl);
begin
	LogFmt('%s: ClientWidth: %d, ClientHeight: %d', [_Prefix, _frm.ClientWidth, _frm.ClientHeight]);
end;

{ TDpiScaler }

function TDpiScaler.Calc(_Value : Integer) : Integer;
begin
	Result := MulDiv(_Value, FCurrentDpi, FDesignDpi);
end;

function TDpiScaler.Calc(const _Value : TRect) : TRect;
begin
	Result.Left := Calc(_Value.Left);
	Result.Top := Calc(_Value.Top);
	Result.Right := Calc(_Value.Right);
	Result.Bottom := Calc(_Value.Bottom);
end;

procedure TDpiScaler.Init(_Dpi : Integer);
begin
	FDesignDpi := _Dpi;
	FCurrentDpi := _Dpi;
end;

procedure TDpiScaler.Init(_DesignDpi, _CurrentDpi : Integer);
begin
	FDesignDpi := _DesignDpi;
	FCurrentDpi := _CurrentDpi;
end;

function GetFontSize(_fnt : TFont) : Integer;
begin
	// Result := _fnt.Size;
	Result := _fnt.Height;
end;

procedure SetFontSize(_fnt : TFont; _Size : Integer);
begin
	// _fnt.Size := _Size;
	_fnt.Height := _Size;
end;

procedure TDpiScaler.Init(_ctrl : TWinControl);
begin
	if not Assigned(_ctrl) then begin
		FDesignDpi := 96;
		FCurrentDpi := 96;
	end else begin
		// todo: adjust as needed
		{$IFDEF DELPHIX_TOKYO_UP}
		// FDesignDpi := TWinControl_GetDesignDPI(TForm(_ctrl));
		// I don't remember why I didn't use TForm(_ctrl).PixelsPerInch here
		// there possibly was a bug in some Delphi versions.
		// FCurrentDpi := TScreen_GetDpiForForm(_ctrl);
		// {$ELSE ~DELPHIX_TOKYO_UP}
		FDesignDpi := _ctrl.PixelsPerInch;
		FCurrentDpi := _ctrl.PixelsPerInch;
		{$ENDIF DELPHIX_TOKYO_UP}
	end;
end;

function TDpiScaler.ScaleFactorPercent : Integer;
begin
	Result := MulDiv(100, FCurrentDpi, FDesignDpi);
end;

procedure TDpiScaler.SetCurrentDpi(_Dpi : Integer);
begin
	FCurrentDpi := _Dpi;
end;

procedure TDpiScaler.SetCurrentDpi(_frm : TCustomForm);
begin
	if not Assigned(_frm) then begin
		FCurrentDpi := 96;
	end else begin
		// todo: adjust as needed
		{$IFDEF DELPHIX_TOKYO_UP}
		// FCurrentDpi := TScreen_GetDpiForForm(_frm)
		// {$ELSE ~DELPHIX_TOKYO_UP}
		FCurrentDpi := TForm(_frm).PixelsPerInch;
		{$ENDIF DELPHIX_TOKYO_UP}
	end;
end;

{ TCtrlDpiScaler }

procedure TCtrlDpiScaler.ApplyScale(const _Scaler : TDpiScaler);
var
	br : TRect;
	LabelWasAutoSize : Boolean;
begin
	LogFmt('TCtrlDpiScaler.ApplyScale(%s, %d %%)', [Ctrl.Name, _Scaler.ScaleFactorPercent]);

	ResizeFont(_Scaler);

	br := _Scaler.Calc(BoundsRect);
	LogRect('  br', br);

	LabelWasAutoSize := False;
	if (Ctrl is TLabel) then begin
		LogStr('  Ctrl is TLabel');
		LabelWasAutoSize := TLabel(Ctrl).AutoSize;
		if LabelWasAutoSize then begin
			LogStr('  AutoSize: True');
			TLabel(Ctrl).AutoSize := False;
		end;
		case TLabel(Ctrl).Alignment of
			taRightJustify : begin
				LogStr('  Alignment: taRightJustify');
				br.Left := _Scaler.Calc(BoundsRect.Left + TRect_Width(BoundsRect) - TRect_Width(br));
				LogFmt('  br: (Left: %d, Top: %d, Width: %d, Height: %d)', [br.Left, br.Top, TRect_Width(br), TRect_Height(br)]);
			end;
			taCenter : begin
				LogStr('  Alignment: taCenter');
				br.Left := _Scaler.Calc(BoundsRect.Left + TRect_Width(BoundsRect) div 2) - TRect_Width(br) div 2;
				LogRect('  br', br);
			end;
		end;
	end else if (Ctrl is TEdit) and TEdit(Ctrl).AutoSize then begin
		LogStr('  Ctrl is TEdit Autosize: True');
		TRect_SetHeight(br, Ctrl.Height);
		LogRect('  br', br);
	end;

	Ctrl.BoundsRect := br;

	if ItemHeight <> 0 then begin
		TrySetIntProperty(Ctrl, 'ItemHeight', _Scaler.Calc(ItemHeight));
	end;

	// if we don't do this, the text is truncated on the left
	if LabelWasAutoSize then begin
		TLabel(Ctrl).AutoSize := False;
		TLabel(Ctrl).AutoSize := True;
	end;
end;

procedure TCtrlDpiScaler.Assign(_Ctrl : TControl);
var
	fnt : TFont;
begin
	Ctrl := _Ctrl;
	BoundsRect := Ctrl.BoundsRect;
	if not TryGetObjectProperty(_Ctrl, 'Font', TObject(fnt)) then begin
		FontSize := 0;
	end else begin
		FontSize := GetFontSize(fnt);
	end;

	if not TryGetIntProperty(_Ctrl, 'ItemHeight', ItemHeight) then begin
		ItemHeight := 0;
	end;

	LogFmt('TCtrlDpiScaler.Assign(%s):', [_Ctrl.Name]);
	LogRect('  BoundsRect', BoundsRect);
end;

procedure TCtrlDpiScaler.ResizeFont(const _Scaler : TDpiScaler);
var
	fnt : TFont;
	ParentFontValue : Boolean;
	OldFontSize : Integer;
begin
	if TryGetObjectProperty(Ctrl, 'Font', TObject(fnt)) then begin
		if not TryGetBoolProperty(Ctrl, 'ParentFont', ParentFontValue) or not ParentFontValue then begin
			Assert(FontSize <> 0);

			LogFmt('TCtrlDpiScaler.ResizeFont(%d %%)', [_Scaler.ScaleFactorPercent]);
			OldFontSize := GetFontSize(fnt);
			SetFontSize(fnt, _Scaler.Calc(FontSize));

			LogFmt('  OldFontSize: %d, NewFontSize: %d', [OldFontSize, GetFontSize(fnt)]);
		end;
	end;
end;

{ TFormDpiScaler }

procedure TFormDpiScaler.AddControls(_Ctrl : TWinControl);
var
	i : Integer;
	cnt : Integer;
	ctrl : TControl;
	cds : TCtrlDpiScaler;
begin
	cnt := _Ctrl.ControlCount;
	for i := 0 to cnt - 1 do begin
		ctrl := _Ctrl.Controls[i];
		cds.Assign(ctrl);
		FCtrlParams := FCtrlParams + [cds];
		if ctrl is TWinControl then
			AddControls(TWinControl(ctrl));
	end;
end;

procedure TFormDpiScaler.ApplyDpi(_NewDpi : Integer; _NewBounds : PRect);
var
	Scaler : TDpiScaler;
	// RedrawLock: IInterface;
	ClientRect : TRect;
	NewWidth : Integer;
	NewHeight : Integer;
begin
	if not Assigned(FFrm) then
		Exit; // ==>

	if Assigned(_NewBounds) then
		LogFmt('TFormDpiScaler.ApplyDpi(%s, NewDpi: %d, NewBounds: (Left: %d, Top: %d, Width: %d, Height: %d))',
			[FFrm.Name, _NewDpi, _NewBounds.Left, _NewBounds.Top, TRect_Width(_NewBounds^), TRect_Height(_NewBounds^)])
	else
		LogFmt('TFormDpiScaler.ApplyDpi(%s, NewDpi: %d, NewBounds: (nil))', [FFrm.Name, _NewDpi]);

	// locking redraws seemed like a good idea but had undesireable side effects:
	// https://en.delphipraxis.net/topic/5516-the-state-of-gexperts-support-for-delphi-11/?do=findComment&comment=49633
	// https://en.delphipraxis.net/topic/5516-the-state-of-gexperts-support-for-delphi-11/?do=findComment&comment=49626
	// both effects were gone after I removed this call:
	// RedrawLock := TWinControl_Lock(FPnlMaster);
	FFrm.DisableAlign;
	try
		FPnlMaster.Visible := False;
		Scaler.Init(FDesignDpi, _NewDpi);
		// Disable constraints to assure the new size can be set
		FFrm.Constraints.MinWidth := 0;
		FFrm.Constraints.MinHeight := 0;
		FFrm.Constraints.MaxWidth := 0;
		FFrm.Constraints.MaxHeight := 0;

		if Assigned(_NewBounds) then begin
			FFrm.BoundsRect := _NewBounds^;
		end else begin
			ClientRect := FFrm.ClientRect;
			NewWidth := Scaler.Calc(FClientWidth);
			NewHeight := Scaler.Calc(FClientHeight);
			FFrm.ClientWidth := NewWidth;
			FFrm.ClientHeight := NewHeight;
		end;
		LogForm('  FFrm', FFrm);

		ApplyScale(Scaler);
	finally
		FPnlMaster.Visible := True;
		FFrm.EnableAlign;
		// RedrawLock := nil;
	end;
end;

procedure TFormDpiScaler.ApplyScale(const _Scaler : TDpiScaler);
var
	cnt : Integer;
	i : Integer;
	OldFontSize : Integer;
begin
	LogFmt('TFormDpiScaler.ApplyScale(%s, %d %%)', [FFrm.Name, _Scaler.ScaleFactorPercent]);

	OldFontSize := GetFontSize(FFont);
	SetFontSize(FFont, _Scaler.Calc(FFontSize));
	LogFmt('  OldFontSize: %d, NewFontSize: %d', [OldFontSize, GetFontSize(FFont)]);

	cnt := Length(FCtrlParams);
	for i := 0 to cnt - 1 do begin
		FCtrlParams[i].ApplyScale(_Scaler);
	end;

	FFrm.Constraints.MinWidth := _Scaler.Calc(FMinWidth);
	FFrm.Constraints.MinHeight := _Scaler.Calc(FMinHeight);
	FFrm.Constraints.MaxWidth := _Scaler.Calc(FMaxWidth);
	FFrm.Constraints.MaxHeight := _Scaler.Calc(FMaxHeight);

	LogConstraints('  FFrm.Constraints', FFrm.Constraints);
end;

function TFormDpiScaler.Calc(_Value : Integer) : Integer;
var
	Scaler : TDpiScaler;
begin
	Scaler.Init(FFrm);
	Result := Scaler.Calc(_Value);
end;

constructor TFormDpiScaler.Create(_frm : TWinControl);
var
	Scaler : TDpiScaler;
	cnstr : TSizeConstraints;
	CurrFontSize : Integer;
	Ctrl : TControl;
begin
	inherited Create;
	FFrm := _frm;
	FClientWidth := _frm.ClientWidth;
	FClientHeight := _frm.ClientHeight;
	cnstr := _frm.Constraints;
	FMinWidth := cnstr.MinWidth;
	FMinHeight := cnstr.MinHeight;
	FMaxWidth := cnstr.MaxWidth;
	FMaxHeight := cnstr.MaxHeight;
	if _frm is TForm then begin
		FFont := (_frm as TForm).Font;
	end else if _frm is TFrame then begin
		FFont := (_frm as TFrame).Font;
	end else begin
		raise Exception.Create('TFormDpiScaler.Create ony TForm and TFrame is supported');
	end;
	CurrFontSize := GetFontSize(FFont);
	FDesignDpi := _frm.PixelsPerInch;

	FPnlMaster := TPanel.Create(FFrm);
	FPnlMaster.Parent := FFrm;
	FPnlMaster.Width := FClientWidth;
	FPnlMaster.Height := FClientHeight;
	FPnlMaster.Caption := '';
	FPnlMaster.Name := '';
	while FFrm.ControlCount > 1 do begin
		Ctrl := FFrm.Controls[0];
		Assert(Ctrl <> FPnlMaster);
		Ctrl.Parent := FPnlMaster;
	end;
	FPnlMaster.Align := alClient;

	LogFmt('TFormDpiScaler.Create(%s)', [FFrm.Name]);
	LogFmt('  ClientWidth: %d, ClientHeight: %d, MinWidth: %d, MinHeight: %d, MaxWdidth: %d MaxHeight: %d CurrFontSize: %d DesignDpi: %d',
		[FClientWidth, FClientHeight, FMinWidth, FMinHeight, FMaxWidth, FMaxHeight, CurrFontSize, FDesignDpi]);

	// FontSize has already been changed by the VCL, but we need the design font size
	Scaler.Init(FFont.PixelsPerInch, FDesignDpi);
	FFontSize := Scaler.Calc(CurrFontSize);
	LogFmt('  (FontPixelsPerInc: %d, FontSize: %d', [FFont.PixelsPerInch, FFontSize]);

	AddControls(FFrm);
end;

destructor TFormDpiScaler.Destroy;
begin
	FPnlMaster.Free;
	inherited;
end;

{ TImageListScaler }

constructor TImageListScaler.Create(_Owner : TComponent; _Original : TImageList);
begin
	inherited Create(_Owner);
	FOriginal := _Original;
end;

destructor TImageListScaler.Destroy;
var
	i : Integer;
begin
	// do not FOriginal.Free
	for i := 0 to high(FScaledImages) do
		FreeAndNil(FScaledImages[i].FImages);
	inherited;
end;

function TImageListScaler.GetScaledList(_DPI : Integer) : TImageList;
var
	i : Integer;
	NewRec : PScaledImagesRec;
begin
	if _DPI = 96 then begin
		Result := FOriginal;
		Exit; // ==>
	end;
	for i := low(FScaledImages) to high(FScaledImages) do begin
		if _DPI = FScaledImages[i].FDpi then begin
			Result := FScaledImages[i].FImages;
			Exit; // ==>
		end;
	end;
	i := Length(FScaledImages);
	SetLength(FScaledImages, i + 1);
	NewRec := @(FScaledImages[i]);
	NewRec.FDpi := _DPI;
	Result := ResizeImagesforHighDPI(_DPI);
	NewRec.FImages := Result;
end;

procedure ClearBmp(_cnv : TCanvas); inline;
begin
	_cnv.FillRect(_cnv.ClipRect);
end;

// taken from
// http://zarko-gajic.iz.hr/resizing-delphis-timagelist-bitmaps-to-fit-high-dpi-scaling-size-for-menus-toolbars-trees-etc/
// but heavily modified for readability and performance
function TImageListScaler.ResizeImagesforHighDPI(_DPI : Integer) : TImageList;
var
	i : Integer;
	OrigBmp, OrigMask : TBitmap;
	ScaledBmp, ScaledMask : TBitmap;
	OrigWidth : Integer;
	OrigHeight : Integer;
	ScaledWidth : Integer;
	ScaledHeight : Integer;
begin
	Result := TImageList.Create(Self);

	// set size to match DPI size (like 250% of 16px = 40px)
	OrigWidth := FOriginal.Width;
	OrigHeight := FOriginal.Height;
	ScaledWidth := MulDiv(OrigWidth, _DPI, 96);
	ScaledHeight := MulDiv(OrigHeight, _DPI, 96);

	Result.SetSize(ScaledWidth, ScaledHeight);

	InitializeNil(OrigMask, OrigBmp, ScaledBmp, ScaledMask);
	try
		OrigBmp := TBitmap.Create;
		OrigMask := TBitmap.Create;
		OrigBmp.SetSize(OrigWidth, OrigHeight);
		OrigMask.SetSize(OrigWidth, OrigHeight);

		ScaledBmp := TBitmap.Create;
		ScaledMask := TBitmap.Create;
		ScaledBmp.SetSize(ScaledWidth, ScaledHeight);
		ScaledMask.SetSize(ScaledWidth, ScaledHeight);

		// add images stretched
		for i := 0 to FOriginal.Count - 1 do begin
			ClearBmp(ScaledBmp.Canvas);
			ClearBmp(ScaledMask.Canvas);
			ClearBmp(OrigBmp.Canvas);
			ClearBmp(OrigMask.Canvas);

			ImageList_DrawEx(FOriginal.Handle, i, OrigBmp.Canvas.Handle, 0, 0, OrigBmp.Width, OrigBmp.Height, CLR_NONE, CLR_NONE,
				ILD_NORMAL);
			ImageList_DrawEx(FOriginal.Handle, i, OrigMask.Canvas.Handle, 0, 0, OrigMask.Width, OrigMask.Height, CLR_NONE, CLR_NONE,
				ILD_MASK);

			ScaledBmp.Canvas.StretchDraw(Rect(0, 0, ScaledBmp.Width, ScaledBmp.Width), OrigBmp);
			ScaledMask.Canvas.StretchDraw(Rect(0, 0, ScaledMask.Width, ScaledMask.Width), OrigMask);
			Result.Add(ScaledBmp, ScaledMask);
		end;
	finally
		FreeAndNil(OrigMask, OrigBmp, ScaledBmp, ScaledMask);
	end;
end;

initialization

// Uwe Raabe suggested this might help to improve performance on rescaling:
// TStyleManager.UseParentPaintBuffers := True;
// It didn't make any noticable difference though, probably because GExperts doens't use VCL styles.
{$IFDEF DPI_SCALER_LOGGING}
	Assignfile(LogFile, 'd:\DpiScaling.log');
Rewrite(LogFile);

finalization

CloseFile(LogFile);
{$ENDIF}

end.
