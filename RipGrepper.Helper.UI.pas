unit RipGrepper.Helper.UI;

interface

uses
	System.UITypes,
	Vcl.ComCtrls,
	Vcl.Graphics;
// Winapi.Messages;

type
	TCursorSaver = record
		strict private
			FOldCursor : TCursor;

		private
		public
			procedure ChangeTo(NewCursor : TCursor);
			constructor Create(NewCursor : TCursor); // Verwendung ist nicht so eindeutig
			procedure SetHourGlassCursor;
			class operator Finalize(var Dest : TCursorSaver);
	end;

	TStatusBarAdjuster = class

		private
			class function TrueFontWidth(fnt : TFont; const text : string) : Integer;

		public
			class procedure AutoSizeStatusbarPanel(_sb : TStatusBar; const _idx : Integer);
	end;

	TListViewColumnAdjuster = class(TObject)
		private
			class function GetMaxWidth(_lv : TListView; const _width, _colIndex : Integer; var maxWidths : TArray<integer>) : integer;

		public
			class procedure AdjustColumnWidths(_lv : TListView; _item : TListItem; var maxWidths : TArray<integer>);
	end;

	TListViewHelper = class Helper for TCustomListView
		private
		protected
			// procedure WMNotify(var AMessage : TWMNotify); message WM_NOTIFY;
		public
			procedure AdjustColumnWidths(var _MaxWidths: TArray<Integer>);
			function TryGetSelected(out _Idx : Integer) : Boolean;
			function GetSelectedOrFirst() : TListItem;
			procedure SetAlteringColors(Item : TListItem);
			procedure SetSelectedColors(State : TOwnerDrawState);
	end;

	TCanvasHelper = class Helper for Vcl.Graphics.TCanvas
		public
			procedure SetAlteringColors(Item : TListItem);
			procedure SetSelectedColors(State : TOwnerDrawState);
	end;

implementation

uses
	Vcl.Forms,
	Winapi.Windows,
	Winapi.CommCtrl,
	System.Classes;

procedure TListViewHelper.AdjustColumnWidths(var _MaxWidths: TArray<Integer>);
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
	_sb.Panels[_idx].Width := TrueFontWidth(_sb.Font, s) + borders[2] * 2 + MARGIN; // vertical border * 2 + 2 extra Pixels
end;

class function TStatusBarAdjuster.TrueFontWidth(fnt : TFont; const text : string) : Integer;
var
	dc : hdc;
	tsize : Winapi.Windows.TSize;
begin
	dc := GetDC(0);
	SelectObject(DC, fnt.Handle);
	GetTextExtentPoint32(dc, PChar(text), Length(text), tsize);
	ReleaseDC(0, DC);
	Result := tsize.cx;
end;

class procedure TListViewColumnAdjuster.AdjustColumnWidths(_lv : TListView; _item : TListItem; var maxWidths : TArray<integer>);
const
	SPACE_TITLE = 50;
	SPACE = 20;
begin
	_lv.Columns[0].Width := SPACE_TITLE +
	{ } GetMaxWidth(_lv, ListView_GetStringWidth(_lv.Handle, PChar(_item.Caption)), 0, maxWidths);
	for var i := 1 to _lv.Columns.Count - 1 do begin
		_lv.Columns[i].Width := SPACE +
		{ } GetMaxWidth(_lv, ListView_GetStringWidth(_lv.Handle, PChar(_item.SubItems[i - 1])), i, maxWidths);
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

end.
