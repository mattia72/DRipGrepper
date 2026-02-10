unit RipGrepper.UI.TabSeparatedConfigForm;

interface

uses
	System.Actions,
	System.Classes,
	System.ImageList,
	RipGrepper.Helper.UI.DarkMode,
	RipGrepper.Settings.Persistable,
	RipGrepper.UI.DpiScaler,
	RipGrepper.UI.SettingsFormBase,
	SVGIconImageList,
	SVGIconImageListBase,
	Vcl.ActnList,
	Vcl.ComCtrls,
	Vcl.Controls,
	Vcl.ExtCtrls,
	Vcl.Forms,
	Vcl.StdCtrls,
	Vcl.ToolWin,
	VirtualTrees,
	VirtualTrees.Types,
	Vcl.ImgList;

type
	TTabSeparatedData = record
		Cells : TArray<string>;
		class function New(const _cells : TArray<string>) : TTabSeparatedData; static;
	end;

	PTabSeparatedData = ^TTabSeparatedData;

	TTabSeparatedConfigForm = class(TSettingsBaseForm, ISettingsForm)
		var
			ActionListConfig : TActionList;
			ActionAdd : TAction;
			ActionCancel : TAction;
			ActionMoveDown : TAction;
			ActionMoveUp : TAction;
			ActionOk : TAction;
			ActionRemove : TAction;
			ActionTest : TAction;
			btn_Cancel : TButton;
			btn_Save : TButton;
			pnlBottom : TPanel;
			pnlMain : TPanel;
			SVGIconImageList1 : TSVGIconImageList;
			tbDown : TToolButton;
			tbMinus : TToolButton;
			tbPlus : TToolButton;
			tbTestRun : TToolButton;
			tbUp : TToolButton;
			ToolBar1 : TToolBar;
			VstData : TVirtualStringTree;
			procedure ActionAddExecute(Sender : TObject);
			procedure ActionCancelExecute(Sender : TObject);
			procedure ActionMoveDownExecute(Sender : TObject);
			procedure ActionMoveDownUpdate(Sender : TObject);
			procedure ActionMoveUpExecute(Sender : TObject);
			procedure ActionMoveUpUpdate(Sender : TObject);
			procedure ActionOkExecute(Sender : TObject);
			procedure ActionRemoveExecute(Sender : TObject);
			procedure ActionRemoveUpdate(Sender : TObject);
			procedure ActionTestExecute(Sender : TObject);
			procedure ActionTestUpdate(Sender : TObject);
			procedure VstDataDblClick(Sender : TObject);
			procedure VstDataFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
			procedure VstDataGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; TextType : TVSTTextType;
					var CellText : string);

		private
			FColorTheme : string;
			FDpiScaler : TRipGrepperDpiScaler;
			FResultStrings : TStrings;
			FTestAction : TAction;
			FThemeHandler : TThemeHandler;
			procedure AddOrSetDataRow(const _data : TTabSeparatedData; _node : PVirtualNode = nil);
			procedure ExchangeNodes(const i, j : Integer);
			function GetNodeByIndex(Tree : TVirtualStringTree; Index : Integer) : PVirtualNode;
			function GetThemeHandler : TThemeHandler;
			procedure SetSelectedNode(const _idx : integer);
			property ThemeHandler : TThemeHandler read GetThemeHandler;

		protected
		public
			constructor Create(AOwner : TComponent; _settings : IPersistable; const _colorTheme : string; _resultStrings : TStrings;
					_testAction : TAction = nil); reintroduce;
			destructor Destroy; override;
			procedure LoadColumnHeaders(const _headers : TArray<string>);
			procedure ReadSettings; override;
			procedure WriteSettings(); override;

	end;

implementation

uses
	ArrayEx,
	RipGrepper.Common.Constants,
	RipGrepper.Helper.UI,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Tools.FileUtils,
	System.SysUtils,
	Vcl.Clipbrd,
	Winapi.ShellAPI,
	Winapi.Windows,
	RipGrepper.OpenWith.Constants,
	VirtualTrees.Header;

{$R *.dfm}

class function TTabSeparatedData.New(const _cells : TArray<string>) : TTabSeparatedData;
begin
	Result.Cells := _cells;
end;

constructor TTabSeparatedConfigForm.Create(AOwner : TComponent; _settings : IPersistable; const _colorTheme : string;
		_resultStrings : TStrings; _testAction : TAction = nil);
begin
	inherited Create(AOwner, _settings, _colorTheme);
	FDpiScaler := TRipGrepperDpiScaler.Create(self);
	FResultStrings := _resultStrings;
	FTestAction := _testAction;
	FColorTheme := _colorTheme;

	// show test button only if test action is assigned
	tbTestRun.Visible := Assigned(FTestAction);

	// setup virtual string tree
	VstData.NodeDataSize := SizeOf(TTabSeparatedData);
	VstData.TreeOptions.SelectionOptions := VstData.TreeOptions.SelectionOptions + [toFullRowSelect];
	VstData.TreeOptions.MiscOptions := VstData.TreeOptions.MiscOptions + [toEditable];

	ReadSettings;
	ThemeHandler.Init(_colorTheme);
end;

destructor TTabSeparatedConfigForm.Destroy;
begin
	FDpiScaler.Free;
	inherited;
end;

procedure TTabSeparatedConfigForm.ActionAddExecute(Sender : TObject);
var
	data : TTabSeparatedData;
	node : PVirtualNode;
	i : Integer;
begin
	inherited;
	node := VstData.GetFirstSelected;

	// create empty data with same column count
	if VstData.Header.Columns.Count > 0 then begin
		SetLength(data.Cells, VstData.Header.Columns.Count);
		for i := 0 to high(data.Cells) do begin
			data.Cells[i] := '';
		end;
		AddOrSetDataRow(data, node);
	end;
end;

procedure TTabSeparatedConfigForm.ActionCancelExecute(Sender : TObject);
begin
	OnCancel();
end;

procedure TTabSeparatedConfigForm.ActionMoveDownExecute(Sender : TObject);
var
	node : PVirtualNode;
	idx : Cardinal;
begin
	inherited;
	node := VstData.GetFirstSelected;
	if Assigned(node) then begin
		idx := node.Index;
		ExchangeNodes(idx, idx + 1);
		SetSelectedNode(idx + 1);
	end;
end;

procedure TTabSeparatedConfigForm.ActionMoveDownUpdate(Sender : TObject);
var
	node : PVirtualNode;
begin
	inherited;
	node := VstData.GetFirstSelected;
	ActionMoveDown.Enabled := Assigned(node) and (node.Index < VstData.RootNodeCount - 1);
end;

procedure TTabSeparatedConfigForm.ActionMoveUpExecute(Sender : TObject);
var
	node : PVirtualNode;
	idx : Cardinal;
begin
	inherited;
	node := VstData.GetFirstSelected;
	if Assigned(node) then begin
		idx := node.Index;
		ExchangeNodes(idx, idx - 1);
		SetSelectedNode(idx - 1);
	end;
end;

procedure TTabSeparatedConfigForm.ActionMoveUpUpdate(Sender : TObject);
var
	node : PVirtualNode;
begin
	inherited;
	node := VstData.GetFirstSelected;
	ActionMoveUp.Enabled := Assigned(node) and (node.Index > 0);
end;

procedure TTabSeparatedConfigForm.ActionOkExecute(Sender : TObject);
begin
	OnOk();
end;

procedure TTabSeparatedConfigForm.ActionRemoveExecute(Sender : TObject);
var
	node : PVirtualNode;
begin
	inherited;
	node := VstData.GetFirstSelected;
	if Assigned(node) then begin
		VstData.DeleteNode(node);
	end;
end;

procedure TTabSeparatedConfigForm.ActionRemoveUpdate(Sender : TObject);
begin
	inherited;
	var
	node := VstData.GetFirstSelected;
	ActionRemove.Enabled := Assigned(node);
end;

procedure TTabSeparatedConfigForm.ActionTestExecute(Sender : TObject);
begin
	inherited;
	if Assigned(FTestAction) then begin
		FTestAction.Execute;
	end;
end;

procedure TTabSeparatedConfigForm.ActionTestUpdate(Sender : TObject);
begin
	inherited;
	var
	node := VstData.GetFirstSelected;
	ActionTest.Enabled := Assigned(FTestAction) and
	{ } Assigned(node);
end;

procedure TTabSeparatedConfigForm.AddOrSetDataRow(const _data : TTabSeparatedData; _node : PVirtualNode = nil);
var
	node : PVirtualNode;
	nodeData : PTabSeparatedData;
begin
	VstData.BeginUpdate;
	try
		if Assigned(_node) then begin
			node := _node;
		end else begin
			node := VstData.AddChild(nil);
		end;

		nodeData := VstData.GetNodeData(node);
		nodeData^.Cells := _data.Cells;
		VstData.InvalidateNode(node);
	finally
		VstData.EndUpdate;
	end;
end;

function TTabSeparatedConfigForm.GetThemeHandler : TThemeHandler;
begin
	if not Assigned(FThemeHandler) then begin
		FThemeHandler := TThemeHandler.Create(self);
	end;
	Result := FThemeHandler;
end;

procedure TTabSeparatedConfigForm.ReadSettings;
var
	arr : TArray<string>;
	data : TTabSeparatedData;
	i : Integer;
	s : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TTabSeparatedConfigForm.ReadSettings');

	VstData.BeginUpdate;
	try
		VstData.Clear;

		for i := 0 to FResultStrings.Count - 1 do begin
			s := FResultStrings[i];
			dbgMsg.MsgFmt('Row %d: %s', [i, s]);

			arr := s.Split([SEPARATOR]); // TAB
			if Length(arr) > 0 then begin
				data := TTabSeparatedData.New(arr);
				AddOrSetDataRow(data);
			end;
		end;
	finally
		VstData.EndUpdate;
	end;
end;

procedure TTabSeparatedConfigForm.WriteSettings();
var
	node : PVirtualNode;
	nodeData : PTabSeparatedData;
	row : string;
	i : Integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TTabSeparatedConfigForm.WriteSettings');

	FResultStrings.Clear;

	node := VstData.GetFirst;
	while Assigned(node) do begin
		nodeData := VstData.GetNodeData(node);
		row := '';

		if Length(nodeData^.Cells) > 0 then begin
			for i := 0 to high(nodeData^.Cells) do begin
				if i > 0 then begin
					row := row + SEPARATOR;
				end;
				row := row + nodeData^.Cells[i];
			end;

			dbgMsg.MsgFmt('Row: %s', [row]);
			FResultStrings.Add(row);
		end;

		node := VstData.GetNext(node);
	end;
end;

procedure TTabSeparatedConfigForm.VstDataDblClick(Sender : TObject);
begin
	inherited;
	// TODO: implement edit dialog
	TDebugUtils.DebugMessage('TTabSeparatedConfigForm.VstDataDblClick - Edit functionality not implemented yet');
end;

procedure TTabSeparatedConfigForm.VstDataFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
var
	nodeData : PTabSeparatedData;
begin
	nodeData := Sender.GetNodeData(Node);
	if Assigned(nodeData) then begin
		Finalize(nodeData^);
	end;
end;

procedure TTabSeparatedConfigForm.VstDataGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex;
		TextType : TVSTTextType; var CellText : string);
var
	nodeData : PTabSeparatedData;
begin
	nodeData := Sender.GetNodeData(Node);
	if Assigned(nodeData) and (Column >= 0) and (Column < Length(nodeData^.Cells)) then begin
		CellText := nodeData^.Cells[Column];
	end else begin
		CellText := '';
	end;
end;

procedure TTabSeparatedConfigForm.ExchangeNodes(const i, j : Integer);
var
	nodeI, nodeJ : PVirtualNode;
	dataI, dataJ : PTabSeparatedData;
	tempCells : TArray<string>;
begin
	nodeI := GetNodeByIndex(VstData, i);
	nodeJ := GetNodeByIndex(VstData, j);

	if Assigned(nodeI) and Assigned(nodeJ) then begin
		VstData.BeginUpdate;
		try
			dataI := VstData.GetNodeData(nodeI);
			dataJ := VstData.GetNodeData(nodeJ);

			// swap data
			tempCells := dataI^.Cells;
			dataI^.Cells := dataJ^.Cells;
			dataJ^.Cells := tempCells;

			VstData.InvalidateNode(nodeI);
			VstData.InvalidateNode(nodeJ);
		finally
			VstData.EndUpdate;
		end;
	end;
end;

function TTabSeparatedConfigForm.GetNodeByIndex(Tree : TVirtualStringTree; Index : Integer) : PVirtualNode;
var
	node : PVirtualNode;
begin
	Result := nil;
	node := Tree.GetFirstChildNoInit(nil);
	while Assigned(node) do begin
		if Integer(node.Index) = index then begin
			Result := node;
			Exit;
		end;
		node := Tree.GetNextNoInit(node);
	end;
end;

procedure TTabSeparatedConfigForm.SetSelectedNode(const _idx : integer);
var
	node : PVirtualNode;
begin
	node := GetNodeByIndex(VstData, _idx);
	if Assigned(node) then begin
		VstData.ClearSelection;
		VstData.Selected[node] := True;
		VstData.FocusedNode := node;
	end;
end;

procedure TTabSeparatedConfigForm.LoadColumnHeaders(const _headers : TArray<string>);
var
	col : TVirtualTreeColumn;
	i : Integer;
begin
	VstData.Header.Columns.Clear;

	for i := 0 to high(_headers) do begin
		col := VstData.Header.Columns.Add;
		col.Text := _headers[i];
		col.Width := 150;
		col.Options := col.Options + [coVisible];
	end;

	VstData.Header.Options := VstData.Header.Options + [hoVisible];
end;

end.
