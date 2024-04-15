unit RipGrepper.UI.RipGrepOptionsForm;

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
	Vcl.ExtCtrls,
	Vcl.StdCtrls,
	RipGrepper.Common.Settings,
	System.Actions,
	Vcl.ActnList,
	ArrayEx,
	Vcl.ComCtrls,
	System.RegularExpressions,
	RipGrepper.UI.DpiScaler,
	RipGrepper.Common.Settings.RipGrepParameterSettings;

type

	THelpOptions = record
		Short : string;
		Long : string;
		Value : string;
		Description : string;
		Group : string;
		class function New(const _short : string; const _long : string = ''; const _value : string = ''; const _description : string = '';
			const _group : string = '') : THelpOptions; static;
	end;

	TRipGrepOptionsForm = class(TForm)
		PanelMain : TPanel;
		btnOk : TButton;
		btnCancel : TButton;
		ActionList1 : TActionList;
		ActionOk : TAction;
		ActionCancel : TAction;
		ActionAdd : TAction;
		PanelBottom : TPanel;
		PanelTop : TPanel;
		ListView1 : TListView;
		Label1 : TLabel;
		Label2 : TLabel;
		procedure ActionCancelExecute(Sender : TObject);
		procedure ActionOkExecute(Sender : TObject);
		procedure ListView1Click(Sender : TObject);
		procedure ListView1DrawItem(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);
		procedure ListView1SelectItem(Sender : TObject; Item : TListItem; Selected : Boolean);
		procedure LoadRipGrepHelp;

		private
			FDpiScaler : TRipGrepperDpiScaler;
			FGroup : TListGroup;
			FGroupIngLineRegex : TRegex;
			FOptionList : TStringList;
			FRGLongParamHelpRegex : TRegex;
			FRGParamHelpRegex : TRegex;
			FRipGrepParameters : TRipGrepParameterSettings;
			procedure AddColumn(const _caption : string);
			procedure AddColumns;
			procedure DrawItemOnCanvas(_Canvas : TCanvas; const _Rect : TRect; _Item : TListItem; const _State : TOwnerDrawState);
			function IsParameterHelpLine(_Item : TListItem) : Boolean;
			function ParseLine(const _s : string) : THelpOptions;
			procedure SetGroup(const _groupHeader : string);
			procedure SetItem(const _ho : THelpOptions; const _groupId : Integer; const _bEnabled : Boolean = True);

			{ Private-Deklarationen }
		public
			constructor Create(AOwner : TComponent; const _settings : TRipGrepParameterSettings); reintroduce; virtual;
			destructor Destroy; override;
			procedure CreateItems(_sl : TStrings);
			{ Public-Deklarationen }
	end;

var
	RipGrepOptionsForm : TRipGrepOptionsForm;

implementation

uses
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.Common.Constants,
	Winapi.CommCtrl,
	RipGrepper.Helper.UI,
	RipGrepper.Tools.DebugTools,
	Winapi.UxTheme,
	System.Math;

{$R *.dfm}

constructor TRipGrepOptionsForm.Create(AOwner : TComponent; const _settings : TRipGrepParameterSettings);
begin
	inherited Create(AOwner);
	FRipGrepParameters := _settings;
	FOptionList := TStringList.Create(TDuplicates.dupIgnore, False, False);
	FOptionList.Delimiter := ' ';
	FOptionList.AddStrings(FRipGrepParameters.RgExeOptions.Split([' ']));

	FRGParamHelpRegex := TRegex.Create(RG_HELP_LINE_REGEX);
	FRGLongParamHelpRegex := TRegex.Create(RG_HELP_LONG_PARAM_REGEX);
	FGroupIngLineRegex := TRegex.Create('^([A-Z][ A-Z]+):');
	FDpiScaler := TRipGrepperDpiScaler.Create(self);
	LoadRipGrepHelp;
end;

destructor TRipGrepOptionsForm.Destroy;
begin
	FOptionList.Free;
	FDpiScaler.Free;
	inherited;
end;

procedure TRipGrepOptionsForm.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
end;

procedure TRipGrepOptionsForm.ActionOkExecute(Sender : TObject);
begin
	FRipGrepParameters.RgExeOptions := FOptionList.DelimitedText;
	ModalResult := mrOk;
end;

procedure TRipGrepOptionsForm.AddColumn(const _caption : string);
var
	col : TListColumn;
begin
	col := ListView1.Columns.Add();
	col.AutoSize := True;
	col.Caption := _caption;
end;

procedure TRipGrepOptionsForm.CreateItems(_sl : TStrings);
begin

	ListView1.ViewStyle := vsReport;
	ListView1.GroupView := True;
	// ListView1.Checkboxes := True;
	AddColumns;
	SetGroup('INFO');
	for var s : string in _sl do begin
		ParseLine(s);
	end;
	ListView_SetColumnWidth(ListView1.Handle, 0, ColumnTextWidth);
	ListView_SetColumnWidth(ListView1.Handle, 1, ColumnTextWidth);
	ListView_SetColumnWidth(ListView1.Handle, 2, ColumnTextWidth);
	ListView_SetColumnWidth(ListView1.Handle, 3, ColumnTextWidth);
end;

procedure TRipGrepOptionsForm.LoadRipGrepHelp;
var
	sl : TStrings;
begin
	sl := TStringList.Create();
	sl.Add('-help');
	try
		TSimpleProcessOutputStringReader.RunProcess(FRipGrepParameters.RipGrepPath, sl, '.', sl);
		CreateItems(sl);
	finally
		sl.Free;
	end;
end;

function TRipGrepOptionsForm.ParseLine(const _s : string) : THelpOptions;
var
	m, groupingMatch : TMatch;
	ho : THelpOptions;
	group : string;
begin
	groupingMatch := FGroupIngLineRegex.Match(_s);
	if groupingMatch.Success then begin
		group := groupingMatch.Groups[1].Value;
		SetGroup(group);
		exit
	end;
	m := FRGParamHelpRegex.Match(_s);
	if m.Success then begin
		ho := THelpOptions.New(
			{ } m.Groups['short'].Value,
			{ } m.Groups['long'].Value,
			{ } m.Groups['value'].Value,
			{ } m.Groups['desc'].Value,
			{ } FGroup.Header);
		SetItem(ho, FGroup.GroupID);
	end else begin
		ho := THelpOptions.New('', '', '', _s);
		SetItem(ho, FGroup.GroupID, False);
	end;

end;

procedure TRipGrepOptionsForm.SetGroup(const _groupHeader : string);
begin
	FGroup := ListView1.Groups.Add();
	FGroup.Header := _groupHeader;
	FGroup.State := [lgsNormal, lgsCollapsible];
	FGroup.HeaderAlign := taLeftJustify;
end;

procedure TRipGrepOptionsForm.AddColumns;
begin
	AddColumn('Description');
	AddColumn('Short');
	AddColumn('Long');
	AddColumn('Value');
end;

procedure TRipGrepOptionsForm.DrawItemOnCanvas(_Canvas : TCanvas; const _Rect : TRect; _Item : TListItem; const _State : TOwnerDrawState);
var
	i : Integer;
	x1 : integer;
	x2 : integer;
	r : TRect;
	s : string;
begin
	if _item.SubItems.Count = 0 then
		Exit;

	_Canvas.SetSelectedColors(_State);

	if not IsParameterHelpLine(_Item) then begin
		// _Canvas.Brush.Color := cl3DLight;
		_Canvas.Font.Color := clGrayText;
	end;

	_Canvas.Brush.Style := bsSolid;
	_Canvas.FillRect(_Rect);
	_Canvas.Brush.Style := bsClear;

	x1 := _Rect.Left;
	x2 := _Rect.Left;
	r := _Rect;

	for i := 0 to ListView1.Columns.Count - 1 do begin
		inc(x2, ListView_GetColumnWidth(ListView1.Handle, ListView1.Columns[i].Index));
		r.Left := x1;
		r.Right := x2;

		case i of
			0 : begin
				s := _Item.Caption;
			end;
			1, 2, 3 : begin
				s := _Item.SubItems[i - 1];
			end;
			else
			TDebugUtils.DebugMessage('Invalid index: ' + i.ToString);
		end;
		_Canvas.TextRect(r, s, [tfSingleLine, DT_ALIGN[ListView1.Columns[i].Alignment], tfVerticalCenter, tfEndEllipsis]);
		x1 := x2;
	end;
end;

function TRipGrepOptionsForm.IsParameterHelpLine(_Item : TListItem) : Boolean;
var
	s : string;
	m : TMatch;
begin
	s := _Item.SubItems[1];
	m := FRGLongParamHelpRegex.Match(s);
	Result := m.Success;
end;

procedure TRipGrepOptionsForm.ListView1Click(Sender : TObject);
var
	option : string;
	selected : TListItem;
begin
	selected := ListView1.Selected;
	while selected <> nil do begin
		// Add long option and value
		option := selected.SubItems[1] + selected.SubItems[2];
		if (FOptionList.IndexOf(option) < 0) then begin
			FOptionList.Add(option);
		end;
		selected := ListView1.GetNextItem(selected, sdAll, [isSelected]);
	end;
end;

procedure TRipGrepOptionsForm.ListView1DrawItem(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);
begin
	DrawItemOnCanvas(Sender.Canvas, Rect, Item, State);
end;

procedure TRipGrepOptionsForm.ListView1SelectItem(Sender : TObject; Item : TListItem; Selected : Boolean);
begin
	if Selected and not IsParameterHelpLine(Item) then begin
		Item.Selected := false;
	end;
end;

procedure TRipGrepOptionsForm.SetItem(const _ho : THelpOptions; const _groupId : Integer; const _bEnabled : Boolean = True);
var
	i : TListItem;
begin
	i := ListView1.Items.Add();
	// i.Checked := _bEnabled;
	i.Caption := _ho.Description;
	i.SubItems.Add(_ho.short); // 0
	i.SubItems.Add(_ho.long); // 1
	i.SubItems.Add(_ho.value); // 2
	i.GroupId := _groupId;
end;

class function THelpOptions.New(const _short : string; const _long : string = ''; const _value : string = '';
	const _description : string = ''; const _group : string = '') : THelpOptions;
begin
	Result.Short := _short;
	Result.Long := _long;
	Result.Value := _value;
	Result.Description := _description;
	Result.Group := _group;
end;

end.
