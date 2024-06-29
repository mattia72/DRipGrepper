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
  Vcl.Forms;
// Winapi.Messages;

type
  TMsgBox = class

    private
      class procedure SetCaption(_msgDlg : TForm);

    public
      class procedure ShowError(const _msg : string);
      class procedure ShowWarning(const _msg : string);
      class procedure ShowInfo(const _msg : string);
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

  TListViewHelper = class Helper for TCustomListView
    function TryGetSelected(out _Idx : Integer) : Boolean;
    function GetSelectedOrFirst() : TListItem;
    procedure InitMaxWidths(var _arrMaxWidths : TArray<Integer>);
    procedure SetAlteringColors(Item : TListItem);
    procedure SetSelectedColors(State : TOwnerDrawState);
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
  end;

  TIconImageList = class
    FImageList : TImageList;
    FExtIndexDict : TDictionary<string, integer>;
    function GetImgIndex(_sFilePath : string) : integer;

    private
      FHandleForm : HWND;
      FImage : TImage;

    public
      constructor Create(_handleForm : HWND; _imgList : TImageList);
      destructor Destroy; override;
  end;

implementation

uses

  Winapi.CommCtrl,
  RipGrepper.Helper.Types,
  Winapi.ShellAPI,
  System.IOUtils,
  RipGrepper.Common.Constants,
  System.SysUtils,
  Vcl.Dialogs;

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
  Self.Canvas.SetAlteringColors(Item.Index);
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

class function TItemDrawer.ShrinkRect(const r : TRect; const X0, X1, Y0, Y1 : integer) : TRect;
begin
  result := r;
  inc(result.Left, X0);
  inc(result.Top, Y0);
  dec(result.Right, X1);
  dec(result.Bottom, Y1);
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
begin
  if _sFilePath = RG_ERROR_MSG_PREFIX then begin
    sExtension := RG_ERROR_MSG_PREFIX;
  end else begin
    sExtension := TPath.GetExtension(_sFilePath);
  end;

  if not FExtIndexDict.TryGetValue(sExtension, Result) then begin
    if _sFilePath = RG_ERROR_MSG_PREFIX then begin
      Result := ImageList_AddIcon(FImageList.Handle, ExtractIcon(FHandleForm, PWideChar(TPath.Combine(GetEnvironmentVariable('Windir'),
          'System32\imageres.dll')), IMAGERES_DLL_ICON_IDX_ERROR));
    end else begin
      bmp := TItemDrawer.GetIconBitmap(_sFilePath, FImage);
      Result := FImageList.AddMasked(bmp, clWhite);
    end;
    FExtIndexDict.Add(sExtension, Result);
  end;
end;

class procedure TMsgBox.SetCaption(_msgDlg : TForm);
begin
  _msgDlg.Caption := APPNAME;
end;

class procedure TMsgBox.ShowError(const _msg : string);
begin
  var MsgDlg : TForm := CreateMessageDialog(_msg, TMsgDlgType.mtError, [mbOK]);
  try
    SetCaption(MsgDlg);
    MsgDlg.ShowModal;
  finally
    MsgDlg.Free;
  end;
end;

class procedure TMsgBox.ShowWarning(const _msg : string);
begin
  var MsgDlg : TForm := CreateMessageDialog(_msg, TMsgDlgType.mtWarning, [mbOK]);
  try
    SetCaption(MsgDlg);
    MsgDlg.ShowModal;
  finally
    MsgDlg.Free;
  end;
end;

class procedure TMsgBox.ShowInfo(const _msg : string);
begin
  var MsgDlg : TForm := CreateMessageDialog(_msg, TMsgDlgType.mtInformation, [mbOK]);
  try
    SetCaption(MsgDlg);
    MsgDlg.ShowModal;
  finally
    MsgDlg.Free;
  end;
end;

end.
