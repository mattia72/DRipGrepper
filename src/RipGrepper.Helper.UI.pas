unit RipGrepper.Helper.UI;

interface

uses
	System.UITypes,
	Vcl.ComCtrls,
	Vcl.Graphics,
	Vcl.StdCtrls,
	System.Types,
	System.Classes,
	RipGrepper.Common.ParsedObject,
	Vcl.ExtCtrls,
	Vcl.Controls,
	System.Generics.Collections,
	Winapi.Windows,
	Vcl.Forms,
	VirtualTrees,
	System.SysUtils;
// Winapi.Messages;

type

	EDlgException = class(Exception);

	TMsgBoxBase = class
		private
		protected
			class function GetButtonsByType(const _type : TMsgDlgType) : TMsgDlgButtons;
			class procedure SetCaption(_msgDlg : TForm);
	end;

	// TODO : It Doesn't work :(
	TMsgBoxThread = class(TMsgBoxBase)

		private
			class var FMsgDlg : TForm;

			class procedure SetModalResultAndClose(const _mr : Integer);
			class procedure ThreadShowDlg;

		public
			constructor Create;
			procedure CreateMsgDialog(const _msg : string; const _type : TMsgDlgType; const _bModal : Boolean = True;
				_parent : TWinControl = nil);

			class procedure OnNoClick(Sender : TObject);
			class procedure OnOkClick(Sender : TObject);
			class procedure OnYesClick(Sender : TObject);
	end;

	TMsgBox = class(TMsgBoxBase)
		private
			class function CreateMsgDialog(const _msg : string; const _type : TMsgDlgType) : Integer;

		public
			class procedure ShowError(const _msg : string);
			class procedure ShowWarning(const _msg : string; const _bModal : Boolean = True; _parent : TWinControl = nil);
			class procedure ShowInfo(const _msg : string);
			class function ShowQuestion(const _msg : string) : Integer;
	end;

	TCursorSaver = record
		strict private
			FOldCursor : TCursor;

		public
			procedure ChangeTo(NewCursor : TCursor);
			constructor Create(NewCursor : TCursor);
			procedure SetHourGlassCursor;
			class operator Finalize(var Dest : TCursorSaver);
	end;

	TBeginEndUpdater = record
		private
			VirtualStrTree : TVirtualStringTree;

		public
			class function New(_lb : TVirtualStringTree) : TBeginEndUpdater; static;
			class operator Finalize(var Dest : TBeginEndUpdater);
	end;

	TFontSizeHelper = class
		class function TrueFontSize(fnt : TFont; const text : string) : Winapi.Windows.TSize;
	end;

	TStatusBarAdjuster = class
		class procedure AutoSizeStatusbarPanel(_sb : TStatusBar; const _idx : Integer);
	end;

	TCanvasHelper = class Helper for Vcl.Graphics.TCanvas
		procedure SetAlteringColors(_idx : Integer);
		procedure SetSelectedColors(State : TOwnerDrawState);
	end;

	TItemInserter = class
		public
			class procedure AddTextToItemsIfNotContains(_cmb : TComboBox);
			class function AddToListBoxIfNotContains(_lb : TListBox; const _s : string; _val : TObject) : Integer;
			class function AddToSringListIfNotContains(_to, _from : TStrings) : Boolean;
	end;

	TItemDrawer = class
		public
			class function ShrinkRect(const r : TRect; const X0, X1, Y0, Y1 : integer) : TRect; inline;
			class function DrawFileIcon(Canvas : TCanvas; Rect : TRect; Item : TListItem; _img : TImage) : Vcl.Graphics.TBitmap;
			class function DrawCheckBox(_Canvas : TCanvas; _Rect : TRect; _Item : TListItem; _img : TImage) : TRect;
			class procedure DrawItemOnBitmap(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);
			class function GetIconBitmap(const sFileName : string; _img : TImage) : Vcl.Graphics.TBitmap;
			class procedure SetTextColorMatch(TargetCanvas : TCanvas);
			class procedure SetTextColorErrorStaticText(TargetCanvas : TCanvas; const _bError : Boolean);
			class procedure SetTextColorHistorySearchText(TargetCanvas : TCanvas);
			class procedure SetTextColorHistoryReplaceText(TargetCanvas : TCanvas);
			class procedure SetTextColorHistoryReplacedText(TargetCanvas : TCanvas);
			class procedure SetTextColorReplacedText(TargetCanvas : TCanvas);
			class procedure SetTextColorReplaceText(TargetCanvas : TCanvas);
	end;

	TIconImageList = class
		private
			FImageList : TImageList;
			FExtIndexDict : TDictionary<string, integer>;
			FHandleForm : HWND;
			FImage : TImage;

		public
			constructor Create(_handleForm : HWND; _imgList : TImageList);
			destructor Destroy; override;
			function GetImgIndex(_sFilePath : string) : integer;
			property ImageList : TImageList read FImageList write FImageList;
	end;

	TDrawParams = record
		FgColor : TColor;
		BgColor : TColor;
		FontSize : TColor;
		FontStyle : TFontStyles;
		class function Save(const _canvas : TCanvas) : TDrawParams; static;
		procedure Load(const _canvas : TCanvas);
	end;

implementation

uses

	Winapi.CommCtrl,
	RipGrepper.Helper.Types,
	Winapi.ShellAPI,
	System.IOUtils,
	RipGrepper.Common.Constants,

	Vcl.Dialogs,
	System.StrUtils;

procedure TCursorSaver.SetHourGlassCursor;
begin
	ChangeTo(crHourGlass);
end;

procedure TCursorSaver.ChangeTo(NewCursor : TCursor);
begin
	FOldCursor := Screen.Cursor;
	Screen.Cursor := NewCursor;
end;

constructor TCursorSaver.Create(NewCursor : TCursor);
begin
	FOldCursor := Screen.Cursor;
	Screen.Cursor := NewCursor;
end;

class operator TCursorSaver.Finalize(var Dest : TCursorSaver);
begin
	Screen.Cursor := Dest.FOldCursor;
end;

class procedure TStatusBarAdjuster.AutoSizeStatusbarPanel(_sb : TStatusBar; const _idx : Integer);
const
	MARGIN = 10;
var
	s : string;
	borders : array [0 .. 2] of Integer;
begin
	// don't deal with simple panels
	// don't resize the last panel
	if _sb.SimplePanel or (_idx >= _sb.Panels.Count - 1) then
		Exit;

	// get the borders of the statusbar
	// border[0] = width of the horizontal border
	// border[1] = width of the vertical border
	// border[2] = width of the border between rectangles
	SendMessage(_sb.Handle, SB_GETBORDERS, 0, Integer(@borders));

	s := _sb.Panels[_idx].Text;

	// calculate the width of the Panel
	_sb.Panels[_idx].Width := TFontSizeHelper.TrueFontSize(_sb.Font, s).cx + borders[2] * 2 + MARGIN;
	// vertical border * 2 + 2 extra Pixels
end;

procedure TCanvasHelper.SetAlteringColors(_idx : Integer);
begin
	if Odd(_idx) then begin
		self.Font.Color := clBlack;
		self.Brush.Color := clWhite;
	end else begin
		self.Font.Color := clBlack;
		self.Brush.Color := cl3DLight; // clGrayText; // clLtGray;
	end;
end;

procedure TCanvasHelper.SetSelectedColors(State : TOwnerDrawState);
begin
	if odSelected in State then begin
		self.Font.Color := clWhite;
		self.Brush.Color := clMenuHighlight;
	end;
end;

class function TItemInserter.AddToSringListIfNotContains(_to, _from : TStrings) : Boolean;
var
	bIsModified : Boolean;
begin
	bIsModified := False;
	for var i : integer := 0 to _from.Count - 1 do begin
		var
			s : string := _from[i];
		var
			idx : integer := _to.IndexOf(s);
		if i <> idx then begin
			if idx = -1 then begin
				_to.Insert(0, s);
			end else begin
				_to.Delete(idx);
				_to.Insert(i, s);
			end;
			bIsModified := True;
		end;
	end;
	Result := bIsModified
end;

class procedure TItemInserter.AddTextToItemsIfNotContains(_cmb : TComboBox);
var
	idxval : Integer;
	val : string;
begin
	val := _cmb.Text;
	if not _cmb.Items.Contains(val) then begin
		_cmb.Items.Insert(0, val);
	end else begin
		idxval := _cmb.Items.IndexOf(val);
		_cmb.Items.Delete(idxval);
		_cmb.Items.Insert(0, val);
		_cmb.ItemIndex := 0;
	end;
end;

class function TItemInserter.AddToListBoxIfNotContains(_lb : TListBox; const _s : string; _val : TObject) : Integer;
var
	idxval : Integer;
	val : string;
begin
	val := _s;
	if not _lb.Items.Contains(val) then begin
		// _lb.Items.InsertObject(0, val, _val);
		idxval := _lb.Items.Count - 1;
	end else begin
		idxval := _lb.Items.IndexOf(val);
		_lb.Items.Delete(idxval);
		// _lb.Items.InsertObject(0, val, _val);
		_lb.ItemIndex := 0;
	end;
	Result := idxval;
end;

class function TItemDrawer.DrawFileIcon(Canvas : TCanvas; Rect : TRect; Item : TListItem; _img : TImage) : Vcl.Graphics.TBitmap;
var
	bm : Vcl.Graphics.TBitmap; // ImageFileIcon
	sFileName : string;
begin
	sFileName := item.Caption;
	bm := TItemDrawer.GetIconBitmap(sFileName, _img);
	Canvas.Draw(Rect.Left + 3, Rect.Top + (Rect.Bottom - Rect.Top - bm.Height) div 2, bm);
	Result := bm;
end;

class function TItemDrawer.DrawCheckBox(_Canvas : TCanvas; _Rect : TRect; _Item : TListItem; _img : TImage) : TRect;
var
	checkboxRect : TRect;
begin
	checkboxRect := _Rect;
	checkboxRect.Offset(3, 3);
	checkboxRect.Height := _Rect.Height - 6;
	checkboxRect.Width := _Rect.Height - 6;
	// _Canvas.Brush.Color :=
	_Canvas.FrameRect(checkboxRect);
	Result := checkboxRect;
end;

class procedure TItemDrawer.DrawItemOnBitmap(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);
var
	noFlickerBm : Vcl.Graphics.TBitmap;
begin
	noFlickerBm := Vcl.Graphics.TBitmap.Create();
	try
		noFlickerBm.Width := Rect.Right - Rect.Left;
		noFlickerBm.Height := Rect.Bottom - Rect.Top;
		// DrawItemOnCanvas(noFlickerBm.Canvas, Rect, Item, State);
		Sender.Canvas.Draw(Rect.Left, Rect.Top, noFlickerBm);
	finally
		noFlickerBm.Free;
	end;
end;

class function TItemDrawer.GetIconBitmap(const sFileName : string; _img : TImage) : Vcl.Graphics.TBitmap;
var
	sfi : TSHFileInfo;
	icon : TIcon;
begin
	icon := TIcon.Create;
	try
		SHGetFileInfo(PChar(sFileName), 0, sfi, SizeOf(TSHFileInfo), SHGFI_SMALLICON or SHGFI_ICON);
		icon.Handle := sfi.hIcon;
		_img.Picture.Bitmap.Assign(icon);
		Result := _img.Picture.Bitmap;
	finally
		icon.Free;
	end;
end;

class procedure TItemDrawer.SetTextColorMatch(TargetCanvas : TCanvas);
begin
	TargetCanvas.Font.Color := TREEVIEW_MATCH_TEXT_COLOR;
	TargetCanvas.Brush.Color := TREEVIEW_MATCH_TEXT_BGCOLOR;
	TargetCanvas.Font.style := TREEVIEW_MATCH_TEXT_STYLE;
end;

class procedure TItemDrawer.SetTextColorErrorStaticText(TargetCanvas : TCanvas; const _bError : Boolean);
begin
	if _bError then begin
		TargetCanvas.Font.Color := TREEVIEW_ERROR_COLOR;
		TargetCanvas.Font.style := [fsBold];
	end else begin
		TargetCanvas.Font.Color := TREEVIEW_STAT_COLOR;
		TargetCanvas.Font.style := [];
	end;
end;

class procedure TItemDrawer.SetTextColorHistorySearchText(TargetCanvas : TCanvas);
begin
	TargetCanvas.Font.Color := HIST_TREEVIEW_SEARCH_TEXT_COLOR;
	// TargetCanvas.Brush.Color := HIST_TREEVIEW_SEARCH_TEXT_BGCOLOR;
	TargetCanvas.Font.style := HIST_TREEVIEW_SEARCH_TEXT_STYLE;
end;

class procedure TItemDrawer.SetTextColorHistoryReplaceText(TargetCanvas : TCanvas);
begin
	TargetCanvas.Font.Color := HIST_TREEVIEW_REPLACE_TEXT_COLOR;
	// TargetCanvas.Brush.Color := HIST_TREEVIEW_REPLACE_TEXT_BGCOLOR;
	TargetCanvas.Font.style := TREEVIEW_REPLACE_TEXT_STYLE;
end;

class procedure TItemDrawer.SetTextColorHistoryReplacedText(TargetCanvas : TCanvas);
begin
	TargetCanvas.Font.Color := HIST_TREEVIEW_REPLACED_TEXT_COLOR;
	// TargetCanvas.Brush.Color := HIST_TREEVIEW_REPLACED_TEXT_BGCOLOR;
	TargetCanvas.Font.style := TREEVIEW_REPLACED_TEXT_STYLE;
end;

class procedure TItemDrawer.SetTextColorReplacedText(TargetCanvas : TCanvas);
begin
	TargetCanvas.Font.Color := TREEVIEW_REPLACED_TEXT_COLOR;
	TargetCanvas.Brush.Color := TREEVIEW_REPLACED_TEXT_BGCOLOR;
	TargetCanvas.Font.style := TREEVIEW_REPLACED_TEXT_STYLE;
end;

class procedure TItemDrawer.SetTextColorReplaceText(TargetCanvas : TCanvas);
begin
	TargetCanvas.Font.Color := TREEVIEW_REPLACE_TEXT_COLOR;
	TargetCanvas.Font.style := TREEVIEW_REPLACE_TEXT_STYLE;
	TargetCanvas.Brush.Color := TREEVIEW_REPLACE_TEXT_BGCOLOR;
end;

class function TItemDrawer.ShrinkRect(const r : TRect; const X0, X1, Y0, Y1 : integer) : TRect;
begin
	result := r;
	inc(result.Left, X0);
	inc(result.Top, Y0);
	dec(result.Right, X1);
	dec(result.Bottom, Y1);
end;

class function TFontSizeHelper.TrueFontSize(fnt : TFont; const text : string) : Winapi.Windows.TSize;
var
	dc : hdc;
begin
	dc := GetDC(0);
	SelectObject(DC, fnt.Handle);
	GetTextExtentPoint32(dc, PChar(text), Length(text), Result);
	ReleaseDC(0, DC);
end;

class function TBeginEndUpdater.New(_lb : TVirtualStringTree) : TBeginEndUpdater;
begin
	Result.VirtualStrTree := _lb;
	Result.VirtualStrTree.BeginUpdate
end;

class operator TBeginEndUpdater.Finalize(var Dest : TBeginEndUpdater);
begin
	Dest.VirtualStrTree.EndUpdate;
end;

constructor TIconImageList.Create(_handleForm : HWND; _imgList : TImageList);
begin
	inherited Create();
	FImageList := _imgList;
	FHandleForm := _handleForm;
	FExtIndexDict := TDictionary<string, integer>.Create;
	FImage := TImage.Create(nil);
end;

destructor TIconImageList.Destroy;
begin
	FExtIndexDict.Free;
	FImage.Free;
	inherited;
end;

function TIconImageList.GetImgIndex(_sFilePath : string) : integer;
var
	sExtension : string;
	bmp : Vcl.Graphics.TBitmap;
	bErrorLine : boolean;
	iconIdx : Integer;
begin
	bErrorLine := MatchStr(_sFilePath, [RG_ERROR_MSG_PREFIX, RG_PARSE_ERROR]);
	if bErrorLine or (not TPath.HasValidPathChars(_sFilePath, False)) then begin
		sExtension := _sFilePath;
	end else begin
		sExtension := TPath.GetExtension(_sFilePath);
	end;

	if not FExtIndexDict.TryGetValue(sExtension, Result) then begin
		if bErrorLine then begin
			if sExtension = RG_ERROR_MSG_PREFIX then begin
				iconIdx := ICON_IDX_ERROR;
			end else begin
				iconIdx := ICON_IDX_PARSE_ERROR;
			end;
			Result := ImageList_AddIcon(FImageList.Handle,
				ExtractIcon(FHandleForm, PWideChar(TPath.Combine(GetEnvironmentVariable('Windir'), ICON_RESOURCE_DLL)), iconIdx));

		end else begin
			bmp := TItemDrawer.GetIconBitmap(_sFilePath, FImage);
			Result := FImageList.AddMasked(bmp, bmp.TransparentColor);
		end;
		FExtIndexDict.Add(sExtension, Result);
	end;
end;

class function TMsgBox.CreateMsgDialog(const _msg : string; const _type : TMsgDlgType) : Integer;
var
	btns : TMsgDlgButtons;
begin
	btns := TMsgBoxBase.GetButtonsByType(_type);
	var
	FMsgDlg := CreateMessageDialog(_msg, _type, btns);
	try
		TMsgBoxBase.SetCaption(FMsgDlg);
		Result := FMsgDlg.ShowModal;
	finally
		FreeAndNil(FMsgDlg);
	end;
end;

class procedure TMsgBox.ShowError(const _msg : string);
begin
	CreateMsgDialog(_msg, TMsgDlgType.mtError);
end;

class procedure TMsgBox.ShowWarning(const _msg : string; const _bModal : Boolean = True; _parent : TWinControl = nil);
begin
	CreateMsgDialog(_msg, TMsgDlgType.mtWarning);
end;

class procedure TMsgBox.ShowInfo(const _msg : string);
begin
	CreateMsgDialog(_msg, TMsgDlgType.mtInformation);
end;

class function TMsgBox.ShowQuestion(const _msg : string) : Integer;
begin
	Result := CreateMsgDialog(_msg, TMsgDlgType.mtConfirmation);
end;

class function TDrawParams.Save(const _canvas : TCanvas) : TDrawParams;
begin
	Result.FgColor := _canvas.Font.Color;
	Result.BgColor := _canvas.Brush.Color;
	Result.FontSize := _canvas.Font.Size;
	Result.FontStyle := _canvas.Font.style;
end;

procedure TDrawParams.Load(const _canvas : TCanvas);
begin
	_canvas.Font.Color := FgColor;
	_canvas.Brush.Color := BgColor;
	_canvas.Font.Size := FontSize;
	_canvas.Font.style := FontStyle;
end;

constructor TMsgBoxThread.Create;
begin
	FMsgDlg := nil;
end;

procedure TMsgBoxThread.CreateMsgDialog(const _msg : string; const _type : TMsgDlgType; const _bModal : Boolean = True;
	_parent : TWinControl = nil);
var
	btns : TMsgDlgButtons;
begin
	btns := GetButtonsByType(_type);
	FMsgDlg := CreateMessageDialog(_msg, _type, btns);
	try
		FMsgDlg.Parent := _parent;
		SetCaption(FMsgDlg);
		if _bModal then begin
			FMsgDlg.ShowModal;
		end else begin
			ThreadShowDlg;
		end;
	finally
		if _bModal then begin
			FreeAndNil(FMsgDlg);
		end;
	end;
end;

class procedure TMsgBoxThread.OnNoClick(Sender : TObject);
begin
	SetModalResultAndClose(mrNo);
end;

class procedure TMsgBoxThread.OnOkClick(Sender : TObject);
begin
	SetModalResultAndClose(mrOk);
end;

class procedure TMsgBoxThread.OnYesClick(Sender : TObject);
begin
	SetModalResultAndClose(mrYes);
end;

class procedure TMsgBoxThread.SetModalResultAndClose(const _mr : Integer);
begin
	FMsgDlg.ModalResult := _mr;
	FMsgDlg.Close;
	// FreeAndNil(FMsgDlg);
end;

class procedure TMsgBoxThread.ThreadShowDlg;
begin
	TThread.Queue(nil,
		procedure()
		begin
			var
			btnOk := (FMsgDlg.FindComponent('OK') as TButton);
			if Assigned(btnOk) then begin
				btnOk.OnClick := TMsgBoxThread.OnOkClick;
			end else begin
				var
				btnYes := (FMsgDlg.FindComponent('Yes') as TButton);
				if Assigned(btnYes) then begin
					btnYes.OnClick := TMsgBoxThread.OnYesClick;
				end;
				var
				btnNo := (FMsgDlg.FindComponent('No') as TButton);
				if Assigned(btnNo) then begin
					btnNo.OnClick := TMsgBoxThread.OnNoClick;
				end;
			end;
			FMsgDlg.Visible := True;
		end);
end;

class function TMsgBoxBase.GetButtonsByType(const _type : TMsgDlgType) : TMsgDlgButtons;
begin
	Result := [];
	case _type of
		{ } TMsgDlgType.mtWarning,
		{ } TMsgDlgType.mtError,
		{ } TMsgDlgType.mtInformation :
		Result := [mbOk];
		TMsgDlgType.mtConfirmation :
		Result := mbYesNo;
		TMsgDlgType.mtCustom :
		{ } raise EDlgException.Create('mtCustom dlg type not supported');
	end;
end;

class procedure TMsgBoxBase.SetCaption(_msgDlg : TForm);
begin
	_msgDlg.Caption := APPNAME;
end;

end.
