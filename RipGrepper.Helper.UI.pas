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
	Vcl.ExtCtrls;
// Winapi.Messages;

type
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
			ListBox : TListBox;

		public
			class function New(_lb : TListBox) : TBeginEndUpdater; static;
			class operator Finalize(var Dest : TBeginEndUpdater);
	end;

	TWidthHelper = class
		class function TrueFontWidth(fnt : TFont; const text : string) : Integer;
	end;

	TStatusBarAdjuster = class
		class procedure AutoSizeStatusbarPanel(_sb : TStatusBar; const _idx : Integer);
	end;

	TListViewColumnAdjuster = class
		private
			class function GetMaxWidth(_lv : TListView; const _width, _colIndex : Integer; var maxWidths : TArray<integer>) : integer;

		public
			class procedure AdjustColumnWidths(_lv : TListView; _item : TListItem; var maxWidths : TArray<integer>);
	end;

	TListViewHelper = class Helper for TCustomListView
		// procedure WMNotify(var AMessage : TWMNotify); message WM_NOTIFY;
		procedure AdjustColumnWidths(var _MaxWidths : TArray<Integer>);
		function TryGetSelected(out _Idx : Integer) : Boolean;
		function GetSelectedOrFirst() : TListItem;
		procedure InitMaxWidths(var _arrMaxWidths : TArray<Integer>);
		procedure SetAlteringColors(Item : TListItem);
		procedure SetSelectedColors(State : TOwnerDrawState);

		private
	end;

	TCanvasHelper = class Helper for Vcl.Graphics.TCanvas
		procedure SetAlteringColors(Item : TListItem);
		procedure SetSelectedColors(State : TOwnerDrawState);
	end;

	TItemInserter = class
		class procedure AddToCmbIfNotContains(_cmb : TComboBox);
		class function AddToListBoxIfNotContains(_lb : TListBox; const _s : string; _val : TObject) : Integer;
	end;

	TItemDrawer = class
		private
		public
			class function DrawFileIcon(Canvas : TCanvas; Rect : TRect; Item : TListItem; _img : TImage) : Vcl.Graphics.TBitmap;
			class procedure DrawItemOnBitmap(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);
			class function GetIconBitmap(const sFileName : string; _img : TImage) : Vcl.Graphics.TBitmap;
	end;

	TListViewGrouper = class

		private
			Grouping : Boolean;
			ItemGroups : TStrings;
			Matches : TParsedObjectGroupedRowCollection;

		public
			procedure PutIntoGroup(const _idx : Integer; _lv : TListView; _item : TListItem);
	end;

implementation

uses
	Vcl.Forms,
	Winapi.Windows,
	Winapi.CommCtrl,
	RipGrepper.Helper.Types,
	Winapi.ShellAPI;

procedure TListViewHelper.AdjustColumnWidths(var _MaxWidths : TArray<Integer>);
begin
	if self.Items.Count > 0 then begin
		TListViewColumnAdjuster.AdjustColumnWidths(self as TListView, self.GetSelectedOrFirst(), _MaxWidths);
	end;
end;

function TListViewHelper.GetSelectedOrFirst : TListItem;
var
	idx : Integer;
begin
	if TryGetSelected(idx) then begin
		Result := Items[idx];
	end else begin
		Result := Items[0];
	end;
end;

procedure TListViewHelper.InitMaxWidths(var _arrMaxWidths : TArray<Integer>);
begin
	if Length(_arrMaxWidths) = 0 then begin
		for var i := 0 to Columns.Count - 1 do begin
			_arrMaxWidths := _arrMaxWidths + [0];
		end;
	end else begin
		for var i := 0 to Columns.Count - 1 do begin
			_arrMaxWidths[i] := 0;
		end;
	end;
end;

procedure TListViewHelper.SetAlteringColors(Item : TListItem);
begin
	Self.Canvas.SetAlteringColors(Item);
end;

procedure TListViewHelper.SetSelectedColors(State : TOwnerDrawState);
begin
	Self.Canvas.SetSelectedColors(State);
end;

{ TCursorSaver }

function TListViewHelper.TryGetSelected(out _Idx : Integer) : Boolean;
begin
	_Idx := self.ItemIndex;
	Result := (_Idx <> -1);
end;

// procedure TListViewHelper.WMNotify(var AMessage : TWMNotify);
// begin
/// /	if (AMessage.NMHdr.hwndFrom = self.Handle) and ((AMessage.NMHdr.code = HDN_ENDTRACK) or (AMessage.NMHdr.code = HDN_TRACK)) then begin
// if (AMessage.NMHdr.hwndFrom = self.Handle) and ((AMessage.NMHdr.code = HDN_ENDTRACK) or (AMessage.NMHdr.code = HDN_ITEMCHANGING )) then begin
// TMessage(AMessage).Result := 0;
// InvalidateRect(self.Handle, nil, true);
/// /		CodeSite.Send('TListView.WMNotify: HDN_ENDTRACK');
// end
// else
// inherited;
// end;

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
	_sb.Panels[_idx].Width := TWidthHelper.TrueFontWidth(_sb.Font, s) + borders[2] * 2 + MARGIN; // vertical border * 2 + 2 extra Pixels
end;

class procedure TListViewColumnAdjuster.AdjustColumnWidths(_lv : TListView; _item : TListItem; var maxWidths : TArray<integer>);
const
	SPACE_TITLE = 50;
	SPACE = 20;
begin
	if (_lv.Items.Count > 0) and (_item.SubItems.Count > 0) then begin
        _lv.Columns[0].Width := SPACE_TITLE +
		// in an early state of drawing, it doesn't work well:
		// { } GetMaxWidth(_lv, ListView_GetStringWidth(_lv.Handle, PChar(_item.Caption)), 0, maxWidths);
		{ } GetMaxWidth(_lv, TWidthHelper.TrueFontWidth(_lv.Font, _item.Caption), 0, maxWidths);
		for var i := 1 to _lv.Columns.Count - 1 do begin
			_lv.Columns[i].Width := SPACE +
			{ } GetMaxWidth(_lv, ListView_GetStringWidth(_lv.Handle, PChar(_item.SubItems[i - 1])), i, maxWidths);
		end;
	end;
end;

class function TListViewColumnAdjuster.GetMaxWidth(_lv : TListView; const _width, _colIndex : Integer; var maxWidths : TArray<integer>)
	: integer;
begin
	if Length(maxWidths) = 0 then begin
		Result := 0
	end else begin
		if _width > maxWidths[_colIndex] then begin
			_lv.Columns[_colIndex].Width := _width;
			maxWidths[_colIndex] := _width;
		end;
		Result := maxWidths[_colIndex];
	end;
end;

procedure TCanvasHelper.SetAlteringColors(Item : TListItem);
begin
	if Odd(Item.Index) then begin
		self.Font.Color := clBlack;
		self.Brush.Color := cl3DLight; // clGrayText; // clLtGray;
	end else begin
		self.Font.Color := clBlack;
		self.Brush.Color := clWhite;
	end;
end;

procedure TCanvasHelper.SetSelectedColors(State : TOwnerDrawState);
begin
	if odSelected in State then begin
		self.Font.Color := clWhite;
		self.Brush.Color := clMenuHighlight;
	end;
end;

class procedure TItemInserter.AddToCmbIfNotContains(_cmb : TComboBox);
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
		_img.Picture.Bitmap.Assign(Icon);
		Result := _img.Picture.Bitmap;
	finally
		icon.Free;
	end;
end;

procedure TListViewGrouper.PutIntoGroup(const _idx : Integer; _lv : TListView; _item : TListItem);
begin
	if not Grouping then
		Exit;

//	if ItemGroups.Contains(Matches.Items[_idx].Columns[Integer(ciFile)].Text) then begin
//		_item.GroupID := Matches[_idx].GroupID;
//	end else begin
//		var
//		Group := _lv.Groups.Add;
//		Group.State := [lgsNormal, lgsCollapsible];
//		Group.Header := Matches[_idx].FileName;
//		var
//		match := Matches[_idx];
//		match.GroupID := Group.GroupID;
//		Matches[_idx] := match;
//		ItemGroups.Add(Matches[_idx].FileName);
//	end;
end;

class function TWidthHelper.TrueFontWidth(fnt : TFont; const text : string) : Integer;
var
	dc : hdc;
	size : Winapi.Windows.TSize;
begin
	dc := GetDC(0);
	SelectObject(DC, fnt.Handle);
	GetTextExtentPoint32(dc, PChar(text), Length(text), size);
	ReleaseDC(0, DC);
	Result := size.cx;
end;

class function TBeginEndUpdater.New(_lb : TListBox) : TBeginEndUpdater;
begin
	Result.ListBox := _lb;
	Result.ListBox.Items.BeginUpdate;
end;

class operator TBeginEndUpdater.Finalize(var Dest : TBeginEndUpdater);
begin
	Dest.ListBox.Items.EndUpdate;
end;

end.
