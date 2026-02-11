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
	Vcl.ImgList,
	RipGrepper.Settings.SettingVariant,
	ArrayEx;

type
	TTabSeparatedData = record
		Checked : Boolean;
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
			procedure VstDataChecked(Sender : TBaseVirtualTree; Node : PVirtualNode);
			procedure VstDataFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
			procedure VstDataGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; TextType : TVSTTextType;
					var CellText : string);
			procedure VstDataNewText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; NewText : string);

		private
			FColorTheme : string;
			FDpiScaler : TRipGrepperDpiScaler;
			FResultStrings : TArrayEx<string>;
			FArraySettings : IPersistableArray;
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
			constructor Create(AOwner : TComponent; _settings : IPersistable; const _colorTheme : string; _testAction : TAction = nil);
					reintroduce;
			destructor Destroy; override;
			procedure LoadColumnHeaders(const _headers : TArray<string>);
			/// ReadSettings: here you can transform FArraySettings to your needs
			procedure ReadSettings(); override;
			/// WriteSettings: here you can transform controls to FArraySettings
			procedure WriteSettings(); override;
			property ResultStrings : TArrayEx<string> read FResultStrings;

	end;

implementation

uses

	RipGrepper.Common.Constants,
	RipGrepper.Helper.UI,
	RipGrepper.OpenWith.Constants,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Tools.FileUtils,
	System.SysUtils,
	Vcl.Clipbrd,
	VirtualTrees.Header,
	Winapi.ShellAPI,
	Winapi.Windows;

{$R *.dfm}

class function TTabSeparatedData.New(const _cells : TArray<string>) : TTabSeparatedData;
begin
	Result.Checked := False;
	Result.Cells := _cells;
end;

constructor TTabSeparatedConfigForm.Create(AOwner : TComponent; _settings : IPersistable; const _colorTheme : string;
		_testAction : TAction = nil);
begin
	inherited Create(AOwner, _settings, _colorTheme);
	FDpiScaler := TRipGrepperDpiScaler.Create(self);
	FArraySettings := _settings as IPersistableArray;
	FTestAction := _testAction;
	FColorTheme := _colorTheme;

	// show test button only if test action is assigned
	tbTestRun.Visible := Assigned(FTestAction);

	// setup virtual string tree
	VstData.NodeDataSize := SizeOf(TTabSeparatedData);
	VstData.TreeOptions.SelectionOptions := VstData.TreeOptions.SelectionOptions + [toFullRowSelect];
	VstData.TreeOptions.MiscOptions := VstData.TreeOptions.MiscOptions + [toEditable, toCheckSupport];
	VstData.CheckImageKind := ckSystemDefault;
	VstData.OnChecked := VstDataChecked;
	VstData.OnNewText := VstDataNewText;

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

	// create empty data with same column count
	if VstData.Header.Columns.Count > 0 then begin
		SetLength(data.Cells, VstData.Header.Columns.Count - 1);
		for i := 0 to high(data.Cells) do begin
			data.Cells[i] := '';
		end;
		data.Checked := False;
		AddOrSetDataRow(data);
		node := VstData.GetLast;
		if Assigned(node) then begin
			SetSelectedNode(node.Index);
			VstData.FocusedColumn := 1;
			VstData.EditNode(node, 1);
		end;
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
		node^.CheckType := ctCheckBox;
		nodeData^.Checked := _data.Checked;
		nodeData^.Cells := _data.Cells;
		if nodeData^.Checked then begin
			VstData.CheckState[node] := csCheckedNormal;
		end else begin
			VstData.CheckState[node] := csUncheckedNormal;
		end;
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
	arrSetting : IArraySetting;
	data : TTabSeparatedData;
	i : Integer;
	s : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TTabSeparatedConfigForm.ReadSettings');

	VstData.BeginUpdate;
	try
		VstData.Clear;
		arrSetting := FArraySettings.GetArraySetting();

		for i := 0 to arrSetting.Count - 1 do begin
			s := arrSetting[i];
			dbgMsg.MsgFmt('Row %d: %s', [i, s]);

			arr := s.Split([SEPARATOR]); // TAB
			if Length(arr) > 0 then begin
				var
				startIndex := 0;
				data.Checked := False;
				if SameText(arr[0], 'TRUE') or SameText(arr[0], 'FALSE') then begin
					data.Checked := SameText(arr[0], 'TRUE');
					startIndex := 1;
				end;

				SetLength(data.Cells, Length(arr) - startIndex);
				for var j := startIndex to high(arr) do begin
					data.Cells[j - startIndex] := arr[j];
				end;
				AddOrSetDataRow(data);
			end;
		end;
	finally
		VstData.EndUpdate;
	end;
end;

procedure TTabSeparatedConfigForm.WriteSettings();
var
	arrSetting : IArraySetting;
	node : PVirtualNode;
	nodeData : PTabSeparatedData;
	row : string;
	i : Integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TTabSeparatedConfigForm.WriteSettings');

	FResultStrings.Clear;
	arrSetting := FArraySettings.GetArraySetting();
	arrSetting.Clear();
	arrSetting.State := ssModified; // State must be modified for StoreToPersister to work

	node := VstData.GetFirst;
	while Assigned(node) do begin
		nodeData := VstData.GetNodeData(node);
		var
		allEmpty := True;
		row := BoolToStr(nodeData^.Checked, True);
		for i := 0 to high(nodeData^.Cells) do begin
			row := row + SEPARATOR + nodeData^.Cells[i];
			allEmpty := allEmpty and (Trim(nodeData^.Cells[i]) = '');
		end;

		if (not allEmpty) or nodeData^.Checked then begin
			dbgMsg.MsgFmt('Row: %s', [row]);
			FResultStrings.Add(row);
			arrSetting.Add(row);
		end;

		node := VstData.GetNext(node);
	end;

	FArraySettings.StoreToPersister();
	if FArraySettings is TPersistableSettings then begin
		(FArraySettings as TPersistableSettings).UpdateFile();
	end;
end;

procedure TTabSeparatedConfigForm.VstDataDblClick(Sender : TObject);
begin
	inherited;
	var
	pt := VstData.ScreenToClient(Mouse.CursorPos);
	var hit : THitInfo;
	var col : TColumnIndex;
	VstData.GetHitTestInfoAt(pt.X, pt.Y, True, hit);
	if Assigned(hit.HitNode) then begin
		col := hit.HitColumn;
		if col < 1 then begin
			col := 1;
		end;
		VstData.FocusedNode := hit.HitNode;
		VstData.FocusedColumn := col;
		VstData.EditNode(hit.HitNode, col);
	end;
end;

procedure TTabSeparatedConfigForm.VstDataChecked(Sender : TBaseVirtualTree; Node : PVirtualNode);
var
	nodeData : PTabSeparatedData;
begin
	nodeData := Sender.GetNodeData(Node);
	if Assigned(nodeData) then begin
		nodeData^.Checked := Sender.CheckState[Node] in [csCheckedNormal, csCheckedPressed];
	end;
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
	cellIndex : Integer;
begin
	nodeData := Sender.GetNodeData(Node);
	if not Assigned(nodeData) then begin
		CellText := '';
		Exit;
	end;

	if Column = 0 then begin
		CellText := '';
		Exit;
	end;

	cellIndex := Column - 1;
	if (cellIndex >= 0) and (cellIndex < Length(nodeData^.Cells)) then begin
		CellText := nodeData^.Cells[cellIndex];
	end else begin
		CellText := '';
	end;
end;

procedure TTabSeparatedConfigForm.VstDataNewText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; NewText : string);
var
	nodeData : PTabSeparatedData;
	cellIndex : Integer;
begin
	if Column < 1 then begin
		Exit;
	end;

	nodeData := Sender.GetNodeData(Node);
	if not Assigned(nodeData) then begin
		Exit;
	end;

	cellIndex := Column - 1;
	if cellIndex >= Length(nodeData^.Cells) then begin
		SetLength(nodeData^.Cells, cellIndex + 1);
	end;

	nodeData^.Cells[cellIndex] := NewText;
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

	col := VstData.Header.Columns.Add;
	col.Text := '';
	col.Width := 28;
	col.Options := col.Options + [coVisible];

	for i := 0 to high(_headers) do begin
		col := VstData.Header.Columns.Add;
		col.Text := _headers[i];
		col.Width := 150;
		col.Options := col.Options + [coVisible];
	end;

	VstData.Header.Options := VstData.Header.Options + [hoVisible];
end;

end.
