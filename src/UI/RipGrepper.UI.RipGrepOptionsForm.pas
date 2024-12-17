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
	RipGrepper.Settings.AppSettings,
	System.Actions,
	Vcl.ActnList,
	ArrayEx,
	Vcl.ComCtrls,
	System.RegularExpressions,
	RipGrepper.UI.DpiScaler,
	RipGrepper.Settings.RipGrepParameterSettings,
	VirtualTrees,
	VirtualTrees.Types,
	Vcl.WinXCtrls;

type

	THelpOptions = record
		Short : string;
		Long : string;
		Value : string;
		Description : string;

		class function New(const _short : string; const _long : string = ''; const _value : string = ''; const _description : string = '')
			: THelpOptions; static;
		function ToString : string;
	end;

	PHelpOptions = ^THelpOptions;

	THelpOptionsGroup = record
		Group : string;
		Option : THelpOptions;
		class function New(const _short : string; const _long : string = ''; const _value : string = ''; const _description : string = '';
			const _group : string = '') : THelpOptionsGroup; overload; static;
		class function New(const _group : PVirtualNode; const _short : string = ''; const _long : string = ''; const _value : string = '';
			const _description : string = '') : THelpOptionsGroup; overload; static;
	end;

	PHelpOptionsGroup = ^THelpOptionsGroup;

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
		Label2 : TLabel;
		llblHelp : TLinkLabel;
		VstResult : TVirtualStringTree;
		SearchBox1 : TSearchBox;
		procedure ActionCancelExecute(Sender : TObject);
		procedure ActionOkExecute(Sender : TObject);
		procedure FormShow(Sender : TObject);
		procedure llblHelpLinkClick(Sender : TObject; const Link : string; LinkType : TSysLinkType);
		procedure LoadRipGrepHelp();
		procedure SearchBox1Change(Sender : TObject);
		procedure VstResultChecked(Sender : TBaseVirtualTree; Node : PVirtualNode);
		procedure VstResultFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
		procedure VstResultGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; TextType : TVSTTextType;
			var CellText : string);
		procedure VstResultPaintText(Sender : TBaseVirtualTree; const TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
			TextType : TVSTTextType);

		private
			FDpiScaler : TRipGrepperDpiScaler;
			FGroupNode : PVirtualNode;
			FGroupIngLineRegex : TRegex;
			FOptionList : TStringList;
			FRGLongParamHelpRegex : TRegex;
			FRGParamHelpRegex : TRegex;
			FRipGrepParameters : TRipGrepParameterSettings;
			procedure AddColumn(const _caption : string);
			procedure AddColumns;
			function AddVSTStructure(AVST : TCustomVirtualStringTree; ANode : PVirtualNode; ARecord : THelpOptionsGroup) : PVirtualNode;
			procedure InitVirtualTree;
			function ParseLine(const _s : string) : THelpOptions;
			procedure SetGroup(const _groupHeader : string);

			{ Private-Deklarationen }
		public
			constructor Create(AOwner : TComponent; const _settings : TRipGrepParameterSettings); reintroduce; virtual;
			destructor Destroy; override;
			procedure CreateItems(_sl : TStrings);
			procedure SearchForText(Sender : TBaseVirtualTree; Node : PVirtualNode; Data : Pointer; var Abort : Boolean);
			{ Public-Deklarationen }
	end;

var
	RipGrepOptionsForm : TRipGrepOptionsForm;

implementation

uses
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.Common.Constants,
	RipGrepper.Helper.UI,
	RipGrepper.Tools.DebugUtils,
	System.Math,
	Winapi.ShellAPI,
	VirtualTrees.Header,
	System.StrUtils,
	RipGrepper.CommandLine.OptionStrings,
	System.UITypes,
	RipGrepper.Settings.FontColors;

{$R *.dfm}

constructor TRipGrepOptionsForm.Create(AOwner : TComponent; const _settings : TRipGrepParameterSettings);
begin
	inherited Create(AOwner);
	FRipGrepParameters := _settings;
	FOptionList := TStringList.Create(TDuplicates.dupIgnore, False, False);
	FOptionList.Delimiter := ' ';
	FOptionList.AddStrings(FRipGrepParameters.RgExeOptions.AsArray.Items);

	FRGParamHelpRegex := TRegex.Create(RG_HELP_LINE_REGEX);
	FRGLongParamHelpRegex := TRegex.Create(RG_HELP_LONG_PARAM_REGEX);
	FGroupIngLineRegex := TRegex.Create('^([A-Z][ A-Z]+):');
	FDpiScaler := TRipGrepperDpiScaler.Create(self);
	llblHelp.Caption := '<a href="' + WWW_LINK_RG_MAN_PAGE + '">' + WWW_LINK_RG_MAN_PAGE + '</a>';
end;

destructor TRipGrepOptionsForm.Destroy;
begin
	FOptionList.Free;
	FDpiScaler.Free;
	inherited Destroy;
end;

procedure TRipGrepOptionsForm.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
end;

procedure TRipGrepOptionsForm.ActionOkExecute(Sender : TObject);
begin
	FRipGrepParameters.RgExeOptions := TOptionStrings.New(FOptionList.DelimitedText); // from option help form
	ModalResult := mrOk;
end;

procedure TRipGrepOptionsForm.AddColumn(const _caption : string);
var
	vtc : TVirtualTreeColumn;
begin
	vtc := VstResult.Header.Columns.Add;
	vtc.Text := _caption;
end;

procedure TRipGrepOptionsForm.CreateItems(_sl : TStrings);
begin

	InitVirtualTree;

	AddColumns;
	SetGroup('INFO');

	VstResult.BeginUpdate;
	try
		for var s : string in _sl do begin
			ParseLine(s);
		end;
	finally
		VstResult.EndUpdate;
		VstResult.FullExpand;
		VstResult.Realign;
	end;
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
	ho : THelpOptionsGroup;
	group : string;
	node : PVirtualNode;
begin
	groupingMatch := FGroupIngLineRegex.Match(_s);
	if groupingMatch.Success then begin
		group := groupingMatch.Groups[1].Value;
		SetGroup(group);
		exit
	end;
	m := FRGParamHelpRegex.Match(_s);
	if m.Success then begin
		ho := THelpOptionsGroup.New(
			{ } FGroupNode,
			{ } m.Groups['short'].Value,
			{ } m.Groups['long'].Value,
			{ } m.Groups['value'].Value,
			{ } m.Groups['desc'].Value);

		node := AddVSTStructure(VstResult, FGroupNode, ho);
		node.CheckType := ctCheckBox;

	end else begin
		ho := THelpOptionsGroup.New('', '', '', _s);
		AddVSTStructure(VstResult, FGroupNode, ho);
	end;

end;

procedure TRipGrepOptionsForm.SetGroup(const _groupHeader : string);
var
	ho : THelpOptionsGroup;
begin
	ho.Group := _groupHeader;
	FGroupNode := AddVSTStructure(VstResult, nil, ho);
end;

procedure TRipGrepOptionsForm.AddColumns;
begin
	AddColumn('Description');
	AddColumn('Short');
	AddColumn('Long');
	AddColumn('Value');
	VstResult.Header.AutoSizeIndex := VstResult.Header.Columns.GetLastVisibleColumn();
	VstResult.Header.Options := VstResult.Header.Options + [hoAutoResize, hoColumnResize, hoDblClickResize];
end;

function TRipGrepOptionsForm.AddVSTStructure(AVST : TCustomVirtualStringTree; ANode : PVirtualNode; ARecord : THelpOptionsGroup)
	: PVirtualNode;
var
	Data : PHelpOptionsGroup;
begin
	Result := AVST.AddChild(ANode);
	Data := AVST.GetNodeData(Result);
	Avst.ValidateNode(Result, False);
	Data^.Option := ARecord.Option;
	Data^.Group := ARecord.Group;
end;

procedure TRipGrepOptionsForm.FormShow(Sender : TObject);
begin
	LoadRipGrepHelp;
end;

procedure TRipGrepOptionsForm.InitVirtualTree;
begin
	VstResult.TreeOptions.StringOptions := VstResult.TreeOptions.StringOptions + [toShowStaticText];
	VstResult.TreeOptions.PaintOptions := VstResult.TreeOptions.PaintOptions + [toUseExplorerTheme];
	VstResult.Header.Options := VstResult.Header.Options + [hoVisible, hoDblClickResize, hoAutoResize, hoAutoResizeInclCaption];
	VstResult.TreeOptions.AutoOptions := VstResult.TreeOptions.AutoOptions + [toAutoSpanColumns];
	VstResult.TreeOptions.SelectionOptions := VstResult.TreeOptions.SelectionOptions + [toFullRowSelect];

	VstResult.NodeDataSize := SizeOf(THelpOptionsGroup);
end;

procedure TRipGrepOptionsForm.llblHelpLinkClick(Sender : TObject; const Link : string; LinkType : TSysLinkType);
begin
	ShellExecute(0, 'OPEN', PChar(Link), '', '', SW_SHOWNORMAL);
end;

procedure TRipGrepOptionsForm.SearchBox1Change(Sender : TObject);
var
	foundNode : PVirtualNode;
begin
	inherited; // SearchBox1Change(Sender)
	// first param is your starting point. nil starts at top of tree. if you want to implement findnext
	// functionality you will need to supply the previous found node to continue from that point.
	// be sure to set the IncrementalSearchTimeout to allow users to type a few characters before starting a search.
	foundNode := VstResult.IterateSubtree(nil, SearchForText, pointer(SearchBox1.text));

	if Assigned(foundNode) then begin
		VstResult.FocusedNode := foundNode;
		VstResult.Selected[foundNode] := True;
	end;
end;

procedure TRipGrepOptionsForm.SearchForText(Sender : TBaseVirtualTree; Node : PVirtualNode; Data : Pointer; var Abort : Boolean);
var
	dataStr : string;
	NodeData : PHelpOptionsGroup; // replace by your record structure
begin
	if not string(data).IsEmpty then begin
		NodeData := Sender.GetNodeData(Node);
		dataStr := NodeData.Option.ToString;
		Abort := ContainsText(dataStr, string(data)); // abort the search if a node with the text is found.
		TDebugUtils.DebugMessage(Format('%s in %s', [string(data), dataStr]));
	end;
end;

procedure TRipGrepOptionsForm.VstResultChecked(Sender : TBaseVirtualTree; Node : PVirtualNode);
var
	NodeData : PHelpOptionsGroup;
	option : string;
begin
	NodeData := Sender.GetNodeData(Node);
	option := NodeData.Option.Long + NodeData.Option.Value;
	if (FOptionList.IndexOf(option) < 0) then begin
		FOptionList.Add(option);
	end;
end;

procedure TRipGrepOptionsForm.VstResultFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
var
	NodeData : PHelpOptionsGroup;
begin
	NodeData := Sender.GetNodeData(Node);
	NodeData.Group := '';
	NodeData.Option := THelpOptions.New('');
end;

procedure TRipGrepOptionsForm.VstResultGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex;
	TextType : TVSTTextType; var CellText : string);
var
	NodeData : PHelpOptionsGroup;
begin
	NodeData := Sender.GetNodeData(Node);
	CellText := '';
	// return the the identifier of the node
	if (TextType = ttNormal) and (NodeData^.Option.description <> '') then begin
		case Column of
			- 1, 0 : // main column, -1 if columns are hidden, 0 if they are shown
			CellText := NodeData^.Option.description;
			1 :
			CellText := NodeData^.Option.short;
			2 :
			CellText := NodeData^.Option.long;
			3 :
			CellText := NodeData^.Option.value;
		end;
	end else begin
		if TextType = ttNormal then begin
			case Column of
				- 1, 0 :
				CellText := NodeData^.Group;
			end;
		end;
	end;
end;

procedure TRipGrepOptionsForm.VstResultPaintText(Sender : TBaseVirtualTree; const TargetCanvas : TCanvas; Node : PVirtualNode;
	Column : TColumnIndex; TextType : TVSTTextType);
begin
	if TextType = ttNormal then begin
		case Column of
			0 : begin
				if Node.ChildCount > 0 then begin
					TItemDrawer.SetTextColor(TargetCanvas, TDefaultFontColors.RG_OPTIONS_TREEVIEW_SECTION_TITLE_TEXT, false);
				end;
			end;
		end;
	end else begin // ttStatic
		// TargetCanvas.Font.Color := TREEVIEW_STAT.Color;
	end;
end;

class function THelpOptions.New(const _short : string; const _long : string = ''; const _value : string = '';
	const _description : string = '') : THelpOptions;
begin
	Result.Short := _short;
	Result.Long := _long;
	Result.Value := _value;
	Result.Description := _description.Trim;
end;

function THelpOptions.ToString : string;
begin
	Result := Description + ' ' + Short + ' ' + Long + ' ' + Value;
end;

class function THelpOptionsGroup.New(const _short : string; const _long : string = ''; const _value : string = '';
	const _description : string = ''; const _group : string = '') : THelpOptionsGroup;
begin
	Result.Option := THelpOptions.New(_short, _long, _value, _description);
	Result.Group := _group;
end;

class function THelpOptionsGroup.New(const _group : PVirtualNode; const _short : string = ''; const _long : string = '';
	const _value : string = ''; const _description : string = '') : THelpOptionsGroup;
var
	ho : THelpOptionsGroup;
begin
	ho := _group.GetData<THelpOptionsGroup>();
	Result := THelpOptionsGroup.New(_short, _long, _value, _description, ho.Group);
end;

end.
