unit RipGrepper.UI.CustomCheckOptions;

interface

uses
	System.Classes,
	System.Generics.Collections,
	System.Generics.Defaults,
	System.SysUtils,
	Vcl.Controls,
	Vcl.StdCtrls,
	Vcl.ExtCtrls,
	Vcl.Samples.Spin,
	Winapi.Windows,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Settings.SettingVariant;

type

	TAutoSetReset = record
		private
			FBoolPtr : PBoolean;

		public
			class function New(var _bValue : Boolean; const _bInitValue : Boolean = True) : TAutoSetReset; static;
			class operator Finalize(var Dest : TAutoSetReset);
	end;

	// Forward declarations
	TCustomCheckOptions = class;

	// Item control types
	ECustomItemType = (citCheckBox, citCheckBoxWithCombo, citCheckBoxWithSpin, citLabelWithCombo);

	// Sub-item index constants
	ESubItemIndex = (siFirst = 0, siSecond = 1);

	// Custom collection item for checkbox items
	TCustomCheckItem = class(TCollectionItem)
		strict private
			FHintHelper : TLabel;
			FParentPanel : TPanel;
			function GetHintHelper() : TLabel;
			function GetParentPanel() : TPanel;

		private
			FCaption : string;
			FTagObject : IInterface;
			FSubItems : array of TControl;
			FComboBoxItems : TStringList;
			FItemType : ECustomItemType;
			FMinValue : Integer;
			FMaxValue : Integer;
			FSpinValue : Integer;
			FSetting : ISetting;
			FEnabled : Boolean;
			FDisabledHint : string;
			procedure enableCtrls(const _bEnable : Boolean);
			function getChecked() : Boolean;
			function getComboText() : string;
			function getSpinValue() : Integer;
			function getSetting() : ISetting;
			function getEnabled() : Boolean;
			function getCheckBox() : TCheckBox;
			function getLabel() : TLabel;
			function getComboBox() : TComboBox;
			function getSpinEdit() : TSpinEdit;
			function getSubItem(_index : ESubItemIndex; _controlClass : TClass) : TControl;
			function GetSubItemEnabled(const _idx : ESubItemIndex) : Boolean;
			function HasAnyDisabledControl() : Boolean;
			procedure setCaption(const _value : string);
			procedure setTagObject(const Value : IInterface);
			procedure setChecked(const _value : Boolean);
			procedure setComboBoxItems(const _value : TStringList);
			procedure setComboText(const _value : string);
			procedure setSpinValue(const _value : Integer);
			procedure setMinValue(const _value : Integer);
			procedure setMaxValue(const _value : Integer);
			procedure setSetting(const _value : ISetting);
			procedure setEnabled(const _value : Boolean);
			procedure showHintHelper();
			procedure hideHintHelper();
			procedure updateHintHelperVisibility();
			procedure setSubItemEnabled(const _idx: ESubItemIndex; const _bEnable: Boolean);

		public
			constructor Create(Collection : TCollection); override;
			destructor Destroy; override;

			procedure SetControlEnabled(_index : ESubItemIndex; _enabled : Boolean);

			property CheckBox : TCheckBox read getCheckBox;
			property LabelControl : TLabel read getLabel;
			property ComboBox : TComboBox read getComboBox;
			property SpinEdit : TSpinEdit read getSpinEdit;
			property HintHelper : TLabel read GetHintHelper;
			property ParentPanel : TPanel read GetParentPanel;
			property ComboBoxItems : TStringList read FComboBoxItems write setComboBoxItems;
			property Caption : string read FCaption write setCaption;
			property TagObject : IInterface read FTagObject write setTagObject;
			property Checked : Boolean read getChecked write setChecked;
			property ItemType : ECustomItemType read FItemType write FItemType;
			property ComboText : string read getComboText write setComboText;
			property SpinValue : Integer read getSpinValue write setSpinValue;
			property MinValue : Integer read FMinValue write setMinValue;
			property MaxValue : Integer read FMaxValue write setMaxValue;
			property Setting : ISetting read getSetting write setSetting;
			property Enabled : Boolean read getEnabled write setEnabled;
			property DisabledHint : string read FDisabledHint write FDisabledHint;
	end;

	// Collection for checkbox items
	TCustomCheckItems = class(TCollection)
		strict private
			FOwner : TControl;
			function getItem(_index : Integer) : TCustomCheckItem;
			procedure setItem(_index : Integer; const _value : TCustomCheckItem);

		protected
			function GetOwner : TPersistent; override;

		public
			constructor Create(_owner : TControl);
			function Add : TCustomCheckItem;
			function AddItem(_cb : TCheckBox; const _caption : string; _setting : ISetting) : TCustomCheckItem; overload;
			function AddItem(_cb : TCheckBox; _combo : TComboBox; const _caption : string; _setting : ISetting) : TCustomCheckItem;
				overload;
			function AddItem(_cb : TCheckBox; _spin : TSpinEdit; const _caption : string; _setting : ISetting) : TCustomCheckItem; overload;
			function AddItem(_lbl : TLabel; _combo : TComboBox; const _caption : string; _setting : ISetting) : TCustomCheckItem; overload;
			property Items[index : Integer] : TCustomCheckItem read getItem write setItem; default;
	end;

	// Event type for item selection
	TItemChangedEvent = procedure(Sender : TObject; Item : TCustomCheckItem) of object;

	// Base class for custom option controls
	TCustomOptionsBase = class(TCustomPanel)
		strict private
			FColumns : Integer;
			procedure setColumns(const _value : Integer);

		protected
			procedure Resize; override;
			procedure AlignControlItems; virtual; abstract;
			function BuildControlNameFromCaption(const _caption : string) : string;

		public
			constructor Create(_owner : TComponent); override;
			procedure Clear; virtual; abstract;

		published
			property Columns : Integer read FColumns write setColumns default 1;
	end;

	// Custom checkbox options control
	TCustomCheckOptions = class(TCustomOptionsBase)
		strict private
			FItems : TCustomCheckItems;
			FOnItemChange : TItemChangedEvent;
			procedure onItemChangeEventHandler(_sender : TObject);
			function getSelectedItems : TArray<TCustomCheckItem>;

		protected
			FEventsEnabled : Boolean;
			procedure AlignControlItems; override;
			function HasTwoControlItems : Boolean;
			function CalculateItemWidth(const _hasTwoControls : Boolean) : Integer;
			function GetItemWidth(const _item : TCustomCheckItem; const _baseWidth : Integer) : Integer;
			procedure CalculateActualWidths(const _itemWidth : Integer; out _actualFirstWidth, _actualSecondWidth : Integer);
			procedure PositionItemPanel(const _item : TCustomCheckItem; const _itemIndex, _itemWidth, _itemHeight : Integer);
			procedure PositionItemControls(const _item : TCustomCheckItem; const _itemIndex, _actualFirstWidth, 
				_actualSecondWidth, _itemHeight : Integer);
			function CreateItemWithPanel(const _caption : string; _itemType : ECustomItemType; _setting : ISetting) : TCustomCheckItem;
			function CreateCheckBox(const _parent : TWinControl; const _caption, _hint, _settingName : string) : TCheckBox;

		public
			constructor Create(_owner : TComponent); override;
			destructor Destroy; override;
			procedure Clear; override;
			function AddCheckboxItem(const _caption, _hint : string; _setting : ISetting) : TCustomCheckItem; overload;
			function AddCheckboxComboItem(const _caption, _hint : string; _comboItems : TArray<string>; _setting : ISetting)
				: TCustomCheckItem; overload;
			function AddCheckboxSpinItem(const _caption, _hint : string; _minValue, _maxValue, _defaultValue : Integer; _setting : ISetting)
				: TCustomCheckItem;
			function AddLabelComboItem(const _caption, _hint : string; _comboItems : TArray<string>; _setting : ISetting)
				: TCustomCheckItem;
			// Getter functions for specific items by order index
			function GetItemChecked(_idx : Integer) : Boolean;
			function GetItemText(_idx : Integer) : string;
			function GetItemSpinValue(_idx : Integer) : Integer;
			function GetItemCaption(_idx : Integer) : string;
			function GetItemByCaption(const _caption : string) : TCustomCheckItem;
			procedure SetItemControlEnabled(_itemIdx : Integer; _controlIdx : ESubItemIndex; _enabled : Boolean);
			function GetMinimumWidth() : Integer;
			property EventsEnabled : Boolean read FEventsEnabled write FEventsEnabled;
			property SelectedItems : TArray<TCustomCheckItem> read getSelectedItems;
			property Items : TCustomCheckItems read FItems write FItems;
			property OnItemChange : TItemChangedEvent read FOnItemChange write FOnItemChange;
	end;

procedure Register;

implementation

uses
	Math,
	RipGrepper.Common.IDEContextValues,
	RipGrepper.Common.SimpleTypes,
	Spring;

{ TCustomOptionsBase }

constructor TCustomOptionsBase.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	FColumns := 1;
	Caption := '';
	BevelOuter := bvNone;
	Width := 200;
	Height := 100;
end;

function TCustomOptionsBase.BuildControlNameFromCaption(const _caption : string) : string;
begin
	Result := _caption.Trim(['-', '=', ':']).Replace('-', '_').Replace(' ', '_');
end;

procedure TCustomOptionsBase.setColumns(const _value : Integer);
begin
	if (_value > 0) and (FColumns <> _value) then begin
		FColumns := _value;
		AlignControlItems;
	end;
end;

procedure TCustomOptionsBase.Resize;
begin
	inherited Resize;
	AlignControlItems;
end;

{ TCustomCheckItem }

constructor TCustomCheckItem.Create(Collection : TCollection);
begin
	inherited Create(Collection);
	SetLength(FSubItems, 2);
	FSubItems[Ord(siFirst)] := nil;
	FSubItems[Ord(siSecond)] := nil;
	FHintHelper := nil;
	FParentPanel := nil;
	FComboBoxItems := TStringList.Create;
	FTagObject := nil;
	FItemType := citCheckBox;
	FMinValue := 0;
	FMaxValue := 100;
	FSpinValue := 0;
	FEnabled := True;
	FDisabledHint := '';
end;

destructor TCustomCheckItem.Destroy;
var
	i : Integer;
begin
	if Assigned(FHintHelper) then begin
		FHintHelper.Free;
		FHintHelper := nil;
	end;
	for i := low(FSubItems) to high(FSubItems) do begin
		if Assigned(FSubItems[i]) then begin
			FSubItems[i].Free;
			FSubItems[i] := nil;
		end;
	end;
	SetLength(FSubItems, 0);
	
	if Assigned(FParentPanel) then begin
		FParentPanel.Free;
		FParentPanel := nil;
	end;
	
	FComboBoxItems.Free;
	inherited Destroy;
end;

procedure TCustomCheckItem.enableCtrls(const _bEnable : Boolean);
begin
	setSubItemEnabled(siFirst, _bEnable);
	setSubItemEnabled(siSecond, _bEnable);
end;

function TCustomCheckItem.getSubItem(_index : ESubItemIndex; _controlClass : TClass) : TControl;
begin
	Result := nil;
	if (Ord(_index) >= low(FSubItems)) and (Ord(_index) <= high(FSubItems)) then begin
		Result := FSubItems[Ord(_index)];
		if Assigned(Result) and not Result.InheritsFrom(_controlClass) then begin
			Result := nil;
		end;
	end;
end;

function TCustomCheckItem.getCheckBox() : TCheckBox;
begin
	Result := getSubItem(siFirst, TCheckBox) as TCheckBox;
end;

function TCustomCheckItem.getLabel() : TLabel;
begin
	Result := getSubItem(siFirst, TLabel) as TLabel;
end;

function TCustomCheckItem.getComboBox() : TComboBox;
begin
	Result := getSubItem(siSecond, TComboBox) as TComboBox;
end;

function TCustomCheckItem.getSpinEdit() : TSpinEdit;
begin
	Result := getSubItem(siSecond, TSpinEdit) as TSpinEdit;
end;

function TCustomCheckItem.getChecked() : Boolean;
begin
	Result := Assigned(CheckBox) and CheckBox.Checked;
end;

function TCustomCheckItem.getComboText() : string;
begin
	Result := '';
	if Assigned(ComboBox) then begin
		Result := ComboBox.Text;
	end;
end;

function TCustomCheckItem.getSpinValue() : Integer;
begin
	Result := FSpinValue;
	if Assigned(SpinEdit) then begin
		Result := SpinEdit.Value;
	end;
end;

procedure TCustomCheckItem.setCaption(const _value : string);
begin
	if FCaption <> _value then begin
		FCaption := _value;
		if Assigned(CheckBox) then begin
			CheckBox.Caption := _value;
		end;
		if Assigned(LabelControl) then begin
			LabelControl.Caption := _value;
		end;
	end;
end;

procedure TCustomCheckItem.setChecked(const _value : Boolean);
begin
	if Assigned(CheckBox) and (CheckBox.Checked <> _value) then begin
		CheckBox.Checked := _value;
	end;
end;

procedure TCustomCheckItem.setTagObject(const Value : IInterface);
begin
	FTagObject := Value;

	{$IFDEF DEBUG}
	var
		icv : IIDEContextValues;

	if Supports(FTagObject, IIDEContextValues, icv) then begin
		Assert(icv.GetContextType <= high(EDelphiIDESearchContext), 'Context type is greater than the max');
	end;
	{$ENDIF}
end;

procedure TCustomCheckItem.setComboText(const _value : string);
begin
	if Assigned(ComboBox) then begin
		ComboBox.Text := _value;
	end;
end;

procedure TCustomCheckItem.setSpinValue(const _value : Integer);
begin
	FSpinValue := _value;
	if Assigned(SpinEdit) then begin
		SpinEdit.Value := _value;
	end;
end;

procedure TCustomCheckItem.setMinValue(const _value : Integer);
begin
	if FMinValue <> _value then begin
		FMinValue := _value;
		if Assigned(SpinEdit) then begin
			SpinEdit.MinValue := _value;
		end;
	end;
end;

procedure TCustomCheckItem.setMaxValue(const _value : Integer);
begin
	if FMaxValue <> _value then begin
		FMaxValue := _value;
		if Assigned(SpinEdit) then begin
			SpinEdit.MaxValue := _value;
		end;
	end;
end;

function TCustomCheckItem.getSetting() : ISetting;
begin
	Result := FSetting;
end;

procedure TCustomCheckItem.setSetting(const _value : ISetting);
begin
	FSetting := _value;
end;

function TCustomCheckItem.getEnabled() : Boolean;
begin
	Result := FEnabled;
end;

function TCustomCheckItem.GetHintHelper() : TLabel;
begin
	// Create hint helper if not exists
	if not Assigned(FHintHelper) then begin
		var
		items := TCustomCheckItems(Collection);
		var
		owner := items.GetOwner as TControl;
		FHintHelper := TLabel.Create(owner);
		FHintHelper.Name := Setting.Name + '_lblHintHelper';
		FHintHelper.Caption := '';
		FHintHelper.Parent := ParentPanel;
		FHintHelper.AutoSize := False;
		FHintHelper.Transparent := True;
	end;
	Result := FHintHelper;
end;

function TCustomCheckItem.GetParentPanel() : TPanel;
begin
	// Create parent panel if not exists
	if not Assigned(FParentPanel) then begin
		var
		items := TCustomCheckItems(Collection);
		var
		owner := items.GetOwner as TControl;
		FParentPanel := TPanel.Create(owner);
		FParentPanel.Name := 'pnl' + Setting.Name;
		FParentPanel.Parent := owner as TWinControl;
		FParentPanel.BevelOuter := bvNone;
		FParentPanel.Caption := '';
		FParentPanel.ParentBackground := False;
		FParentPanel.ParentColor := True;
	end;
	Result := FParentPanel;
end;

function TCustomCheckItem.GetSubItemEnabled(const _idx : ESubItemIndex) : Boolean;
begin
	Result := False;
	var
	ctrl := FSubItems[Ord(_idx)];
	if Assigned(ctrl) and (ctrl is TWinControl) then begin
		Result := TWinControl(ctrl).Enabled;
	end;
end;

function TCustomCheckItem.HasAnyDisabledControl() : Boolean;
begin
	// Check if the whole item is disabled OR any individual control is disabled
	Result := (not FEnabled) or 
	          (not GetSubItemEnabled(siFirst)) or 
	          (not GetSubItemEnabled(siSecond));
end;

procedure TCustomCheckItem.updateHintHelperVisibility();
begin
	// Show hint helper only if there are disabled controls
	if HasAnyDisabledControl then begin
		showHintHelper();
	end else begin
		hideHintHelper();
	end;
end;

procedure TCustomCheckItem.SetControlEnabled(_index : ESubItemIndex; _enabled : Boolean);
var
	ctrl : TControl;
begin
	ctrl := FSubItems[Ord(_index)];

	// Check if already in desired state
	if not Assigned(ctrl) or ((ctrl is TWinControl) and (TWinControl(ctrl).Enabled = _enabled)) then begin
		Exit;
	end;

	setSubItemEnabled(_index, _enabled);

	// Update hint helper based on overall state
	updateHintHelperVisibility();
end;

procedure TCustomCheckItem.setEnabled(const _value : Boolean);
begin
	if FEnabled = _value then begin
		Exit;
	end;

	FEnabled := _value;
	enableCtrls(FEnabled);

	// Update hint helper based on overall state
	updateHintHelperVisibility();
end;

procedure TCustomCheckItem.showHintHelper();
var
	firstCtrl, secondCtrl : TControl;
	leftPos, topPos, rightPos, bottomPos : Integer;
	hintText : string;
	firstEnabled, secondEnabled : Boolean;
begin
	// Ensure controls are assigned
	firstCtrl := FSubItems[Ord(siFirst)];
	if not Assigned(firstCtrl) then begin
		Exit;
	end;
	
	secondCtrl := FSubItems[Ord(siSecond)];

	// Get enabled states from controls
	firstEnabled := GetSubItemEnabled(siFirst);
	secondEnabled := GetSubItemEnabled(siSecond);

	// Determine which controls to cover based on enabled states
	if not FEnabled then begin
		// Whole item disabled - cover both controls
		// Coordinates are relative to ParentPanel since HintHelper.Parent = ParentPanel
		leftPos := firstCtrl.Left;
		topPos := firstCtrl.Top;
		rightPos := leftPos + firstCtrl.Width;
		bottomPos := topPos + firstCtrl.Height;

		if Assigned(secondCtrl) then begin
			rightPos := Max(rightPos, secondCtrl.Left + secondCtrl.Width);
			bottomPos := Max(bottomPos, secondCtrl.Top + secondCtrl.Height);
		end;

		hintText := FDisabledHint;
	end else begin
		// Individual controls disabled - cover only disabled ones
		if not firstEnabled then begin
			leftPos := firstCtrl.Left;
			topPos := firstCtrl.Top;
			rightPos := leftPos + firstCtrl.Width;
			bottomPos := topPos + firstCtrl.Height;
			hintText := firstCtrl.Hint;
		end else if not secondEnabled and Assigned(secondCtrl) then begin
			leftPos := secondCtrl.Left;
			topPos := secondCtrl.Top;
			rightPos := leftPos + secondCtrl.Width;
			bottomPos := topPos + secondCtrl.Height;
			hintText := secondCtrl.Hint;
		end else begin
			// Nothing to cover - all controls are enabled
			hideHintHelper();
			Exit;
		end;
	end;

	// Ensure HintHelper is created and properly configured
	if not Assigned(HintHelper) then begin
		Exit;
	end;

	// Set position and size using SetBounds for atomic update
	// Coordinates are relative to ParentPanel
	HintHelper.SetBounds(leftPos, topPos, rightPos - leftPos, bottomPos - topPos);

	HintHelper.Hint := hintText;
	HintHelper.ShowHint := True;
	HintHelper.Visible := True;
	HintHelper.BringToFront();
end;

procedure TCustomCheckItem.hideHintHelper();
begin
	if Assigned(FHintHelper) then begin
		FHintHelper.Visible := False;
		FHintHelper.Hint := '';
	end;
end;

procedure TCustomCheckItem.setComboBoxItems(const _value : TStringList);
begin
	if Assigned(_value) then begin
		FComboBoxItems.Assign(_value);
		if Assigned(ComboBox) then begin
			ComboBox.Items.Assign(_value);
		end;
	end;
end;

procedure TCustomCheckItem.setSubItemEnabled(const _idx: ESubItemIndex; const _bEnable: Boolean);
begin
	var ctrl := FSubItems[Ord(_idx)];
	if Assigned(ctrl) and (ctrl is TWinControl) then begin
		TWinControl(ctrl).Enabled := _bEnable;
	end;
end;

{ TCustomCheckItems }

constructor TCustomCheckItems.Create(_owner : TControl);
begin
	inherited Create(TCustomCheckItem);
	FOwner := _owner;
end;

function TCustomCheckItems.Add : TCustomCheckItem;
begin
	Result := TCustomCheckItem(inherited Add);
end;

function TCustomCheckItems.AddItem(_cb : TCheckBox; const _caption : string; _setting : ISetting) : TCustomCheckItem;
begin
	Result := Add;
	Result.FCaption := _caption;
	Result.TagObject := _setting;
	Result.FSubItems[Ord(siFirst)] := _cb;
	Result.FItemType := citCheckBox;
	Result.FSetting := _setting;
end;

function TCustomCheckItems.AddItem(_cb : TCheckBox; _combo : TComboBox; const _caption : string; _setting : ISetting) : TCustomCheckItem;
begin
	Result := Add;
	Result.FCaption := _caption;
	Result.TagObject := _setting;
	Result.FSubItems[Ord(siFirst)] := _cb;
	Result.FSubItems[Ord(siSecond)] := _combo;
	Result.FItemType := citCheckBoxWithCombo;
	Result.FSetting := _setting;
end;

function TCustomCheckItems.AddItem(_cb : TCheckBox; _spin : TSpinEdit; const _caption : string; _setting : ISetting) : TCustomCheckItem;
begin
	Result := Add;
	Result.FCaption := _caption;
	Result.TagObject := _setting;
	Result.FSubItems[Ord(siFirst)] := _cb;
	Result.FSubItems[Ord(siSecond)] := _spin;
	Result.FItemType := citCheckBoxWithSpin;
	Result.FSetting := _setting;
end;

function TCustomCheckItems.AddItem(_lbl : TLabel; _combo : TComboBox; const _caption : string; _setting : ISetting) : TCustomCheckItem;
begin
	Result := Add;
	Result.FCaption := _caption;
	Result.TagObject := _setting;
	Result.FSubItems[Ord(siFirst)] := _lbl;
	Result.FSubItems[Ord(siSecond)] := _combo;
	Result.FItemType := citLabelWithCombo;
	Result.FSetting := _setting;
end;

function TCustomCheckItems.getItem(_index : Integer) : TCustomCheckItem;
begin
	Result := TCustomCheckItem(inherited GetItem(_index));
end;

function TCustomCheckItems.GetOwner : TPersistent;
begin
	Result := FOwner;
end;

procedure TCustomCheckItems.setItem(_index : Integer; const _value : TCustomCheckItem);
begin
	inherited SetItem(_index, _value);
end;

{ TCustomCheckOptions }

constructor TCustomCheckOptions.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	FItems := TCustomCheckItems.Create(Self);
	FEventsEnabled := True;
end;

destructor TCustomCheckOptions.Destroy;
begin
	Clear;
	FItems.Free;
	inherited Destroy;
end;

procedure TCustomCheckOptions.Clear;
var
	i : Integer;
	item : TCustomCheckItem;
begin
	// Free all controls
	for i := FItems.Count - 1 downto 0 do begin
		item := FItems[i];
		item.Free;
	end;
	FItems.Clear;
end;

function TCustomCheckOptions.CreateItemWithPanel(const _caption : string; _itemType : ECustomItemType; _setting : ISetting) : TCustomCheckItem;
begin
	var
	ar := TAutoSetReset.New(FEventsEnabled, False);

	// Create the item to get the parent panel
	Result := FItems.Add;
	Result.FCaption := _caption;
	Result.TagObject := _setting;
	Result.FItemType := _itemType;
	Result.FSetting := _setting;
end;

function TCustomCheckOptions.CreateCheckBox(const _parent : TWinControl; const _caption, _hint, _settingName : string) : TCheckBox;
begin
	Result := TCheckBox.Create(Self);
	Result.Name := 'cb' + _settingName;
	Result.Parent := _parent;
	Result.Caption := _caption;
	Result.Hint := _hint;
	Result.ShowHint := True;
	Result.OnClick := onItemChangeEventHandler;
end;

function TCustomCheckOptions.AddCheckboxItem(const _caption, _hint : string; _setting : ISetting) : TCustomCheckItem;
var
	checkBox : TCheckBox;
begin
	Result := CreateItemWithPanel(_caption, citCheckBox, _setting);

	// Create the checkbox
	checkBox := CreateCheckBox(Result.ParentPanel, _caption, _hint, _setting.Name);
	
	Result.FSubItems[Ord(siFirst)] := checkBox;
end;

function TCustomCheckOptions.AddCheckboxComboItem(const _caption, _hint : string; _comboItems : TArray<string>; _setting : ISetting)
	: TCustomCheckItem;
var
	checkBox : TCheckBox;
	comboBox : TComboBox;
begin
	Result := CreateItemWithPanel(_caption, citCheckBoxWithCombo, _setting);

	// Create the checkbox
	checkBox := CreateCheckBox(Result.ParentPanel, _caption, _hint, _setting.Name);

	// Create the combo box
	comboBox := TComboBox.Create(Self);
	comboBox.Name := 'cmb' + _setting.Name;
	comboBox.Parent := Result.ParentPanel;
	comboBox.Hint := _hint;
	comboBox.ShowHint := True;
	comboBox.Style := csDropDown;
	comboBox.AutoDropDownWidth := True;

	var
		strList : IShared<TStringList> := Shared.Make<TStringList>();
	strList.AddStrings(_comboItems);
	if Assigned(strList) then begin
		comboBox.Items.Assign(strList());
	end;
	
	Result.FSubItems[Ord(siFirst)] := checkBox;
	Result.FSubItems[Ord(siSecond)] := comboBox;
	Result.ComboBoxItems := strList();
end;

function TCustomCheckOptions.AddCheckboxSpinItem(const _caption, _hint : string; _minValue, _maxValue, _defaultValue : Integer;
	_setting : ISetting) : TCustomCheckItem;
var
	checkBox : TCheckBox;
	spinEdit : TSpinEdit;
begin
	Result := CreateItemWithPanel(_caption, citCheckBoxWithSpin, _setting);
	Result.MinValue := _minValue;
	Result.MaxValue := _maxValue;
	Result.SpinValue := _defaultValue;

	// Create the checkbox
	checkBox := CreateCheckBox(Result.ParentPanel, _caption, _hint, _setting.Name);

	// Create the spin edit
	spinEdit := TSpinEdit.Create(Self);
	spinEdit.Name := 'spin' + _setting.Name;
	spinEdit.Parent := Result.ParentPanel;
	spinEdit.MinValue := _minValue;
	spinEdit.MaxValue := _maxValue;
	spinEdit.Value := _defaultValue;
	spinEdit.Hint := _hint;
	spinEdit.ShowHint := True;
	spinEdit.OnChange := onItemChangeEventHandler;

	Result.FSubItems[Ord(siFirst)] := checkBox;
	Result.FSubItems[Ord(siSecond)] := spinEdit;
end;

function TCustomCheckOptions.AddLabelComboItem(const _caption, _hint : string; _comboItems : TArray<string>; _setting : ISetting)
	: TCustomCheckItem;
var
	labelControl : TLabel;
	comboBox : TComboBox;
	comboItems : IShared<TStringList>;
begin
	Result := CreateItemWithPanel(_caption, citLabelWithCombo, _setting);

	// Create the label
	labelControl := TLabel.Create(Self);
	var
	cleanCaption := BuildControlNameFromCaption(_caption);
	labelControl.Name := 'cb' + _setting.Name;
	labelControl.Parent := Result.ParentPanel;
	labelControl.Caption := _caption;
	labelControl.Hint := _hint;
	labelControl.ShowHint := True;

	// Create the combo box
	comboBox := TComboBox.Create(Self);
	comboBox.Name := 'cmb' + _setting.Name;
	comboBox.Parent := Result.ParentPanel;
	comboBox.Hint := _hint;
	comboBox.ShowHint := True;
	comboBox.Style := csDropDown;
	comboBox.AutoDropDownWidth := True;
	comboBox.OnChange := onItemChangeEventHandler;

	comboItems := Shared.Make<TStringList>();
	comboItems.AddStrings(_comboItems);
	if Assigned(_comboItems) then begin
		comboBox.Items.Assign(comboItems);
	end;

	comboBox.ItemIndex := 0;
	
	Result.FSubItems[Ord(siFirst)] := labelControl;
	Result.FSubItems[Ord(siSecond)] := comboBox;
	Result.ComboBoxItems := comboItems;
end;

function TCustomCheckOptions.HasTwoControlItems : Boolean;
var
	i : Integer;
begin
	Result := False;
	for i := 0 to FItems.Count - 1 do begin
		if FItems[i].ItemType in [citCheckBoxWithCombo, citCheckBoxWithSpin, citLabelWithCombo] then begin
			Result := True;
			Break;
		end;
	end;
end;

function TCustomCheckOptions.CalculateItemWidth(const _hasTwoControls : Boolean) : Integer;
const
	SPACE = 8;
	FIRST_CONTROL_WIDTH = 100;
	SECOND_CONTROL_COMBO_WIDTH = 80;
begin
	// This method calculates the base width for layout purposes
	// Individual items will use this differently based on their type
	
	// If ANY two-control items exist, use their width as the base unit
	if _hasTwoControls then begin
		// Two-control items: itemWidth = first + space + second + margins
		Result := FIRST_CONTROL_WIDTH + SPACE + SECOND_CONTROL_COMBO_WIDTH + (2 * SPACE);
	end else if FItems.Count = 1 then begin
		// Single one-control item: use FIRST_CONTROL_WIDTH exactly
		Result := FIRST_CONTROL_WIDTH + (2 * SPACE);
	end else begin
		// Multiple one-control items only: each is half the width of a two-control item
		// So two one-control items together equal one two-control item
		Result := (FIRST_CONTROL_WIDTH + SPACE + SECOND_CONTROL_COMBO_WIDTH + (2 * SPACE)) div 2;
	end;
end;

function TCustomCheckOptions.GetItemWidth(const _item : TCustomCheckItem; const _baseWidth : Integer) : Integer;
const
	SPACE = 8;
	FIRST_CONTROL_WIDTH = 100;
	SECOND_CONTROL_COMBO_WIDTH = 80;
begin
	// Calculate width for individual item based on its type
	case _item.ItemType of
		citCheckBox : begin
			// One-control item
			if HasTwoControlItems then begin
				// When mixed with two-control items, one-control items are half width
				Result := _baseWidth div 2;
			end else begin
				// Only one-control items: use base width
				Result := _baseWidth;
			end;
		end;
		
		citCheckBoxWithCombo, citCheckBoxWithSpin, citLabelWithCombo : begin
			// Two-control items always use full base width
			Result := _baseWidth;
		end;
		
		else
			Result := _baseWidth;
	end;
end;

procedure TCustomCheckOptions.CalculateActualWidths(const _itemWidth : Integer; out _actualFirstWidth, 
	_actualSecondWidth : Integer);
const
	SPACE = 8;
	FIRST_CONTROL_WIDTH = 100;
	SECOND_CONTROL_COMBO_WIDTH = 80;
var
	availableForControls : Integer;
begin
	// Calculate space available for controls (excluding margins)
	availableForControls := _itemWidth - (3 * SPACE);
	
	// Use fixed widths as specified in requirements
	// Only adjust if there's not enough space
	if availableForControls >= (FIRST_CONTROL_WIDTH + SECOND_CONTROL_COMBO_WIDTH) then begin
		// Enough space - use standard widths
		_actualFirstWidth := FIRST_CONTROL_WIDTH;
		_actualSecondWidth := SECOND_CONTROL_COMBO_WIDTH;
	end else if availableForControls >= 100 then begin
		// Less space - use minimum for first, rest for second
		_actualFirstWidth := FIRST_CONTROL_WIDTH;
		_actualSecondWidth := availableForControls - FIRST_CONTROL_WIDTH;
	end else begin
		// Very limited space - split evenly but ensure minimums
		_actualFirstWidth := Max(50, availableForControls div 2);
		_actualSecondWidth := Max(50, availableForControls - _actualFirstWidth);
	end;
end;

procedure TCustomCheckOptions.PositionItemPanel(const _item : TCustomCheckItem; const _itemIndex, _itemWidth, 
	_itemHeight : Integer);
const
	SPACE = 8;
var
	col, row : Integer;
	baseLeft : Integer;
	panelHeight : Integer;
begin
	col := _itemIndex mod Columns;
	row := _itemIndex div Columns;
	baseLeft := col * _itemWidth;

	// Calculate panel height to accommodate controls
	// Controls are positioned at SPACE div 2 from top and have height of _itemHeight - 2
	// Add SPACE div 2 for bottom margin for symmetry
	panelHeight := (SPACE div 2) + (_itemHeight - 2) + (SPACE div 2);

	// Position the parent panel
	if Assigned(_item.ParentPanel) then begin
		_item.ParentPanel.Left := baseLeft;
		_item.ParentPanel.Top := row * _itemHeight;
		_item.ParentPanel.Width := _itemWidth;
		_item.ParentPanel.Height := panelHeight; // Height to fully contain controls with margins
	end;
end;

procedure TCustomCheckOptions.PositionItemControls(const _item : TCustomCheckItem; const _itemIndex, _actualFirstWidth, 
	_actualSecondWidth, _itemHeight : Integer);
const
	SPACE = 8;
	FIRST_CONTROL_WIDTH = 100;
	SECOND_CONTROL_SPIN_WIDTH = 60;
var
	spinWidth : Integer;
begin
	// Position controls within the panel (relative to panel)
	case _item.ItemType of
		citCheckBox : begin
			// Single checkbox - if only one item total, use FIRST_CONTROL_WIDTH, otherwise use full item width
			if Assigned(_item.CheckBox) then begin
				_item.CheckBox.Left := SPACE;
				_item.CheckBox.Top := SPACE div 2;
				if (FItems.Count = 1) then begin
					_item.CheckBox.Width := FIRST_CONTROL_WIDTH;
				end else begin
					_item.CheckBox.Width := _item.ParentPanel.Width - (2 * SPACE);
				end;
				_item.CheckBox.Height := _itemHeight - 2;
				_item.CheckBox.Tag := _itemIndex;
			end;
		end;

		citCheckBoxWithCombo : begin
			// Checkbox + ComboBox - both left aligned
			if Assigned(_item.CheckBox) then begin
				_item.CheckBox.Left := SPACE;
				_item.CheckBox.Top := SPACE div 2;
				_item.CheckBox.Width := _actualFirstWidth;
				_item.CheckBox.Height := _itemHeight - 2;
				_item.CheckBox.Tag := _itemIndex;
			end;
			if Assigned(_item.ComboBox) then begin
				_item.ComboBox.Left := SPACE + _actualFirstWidth + SPACE;
				_item.ComboBox.Top := SPACE div 2;
				_item.ComboBox.Width := _actualSecondWidth;
				_item.ComboBox.Height := _itemHeight - 2;
				_item.ComboBox.Tag := _itemIndex;
			end;
		end;

		citCheckBoxWithSpin : begin
			// Checkbox + SpinEdit - spin is narrower than combo, but itemWidth stays the same
			if Assigned(_item.CheckBox) then begin
				_item.CheckBox.Left := SPACE;
				_item.CheckBox.Top := SPACE div 2;
				_item.CheckBox.Width := _actualFirstWidth;
				_item.CheckBox.Height := _itemHeight - 2;
				_item.CheckBox.Tag := _itemIndex;
			end;
			if Assigned(_item.SpinEdit) then begin
				// Spin control gets smaller width than combo
				spinWidth := Min(SECOND_CONTROL_SPIN_WIDTH, _actualSecondWidth);
				_item.SpinEdit.Left := SPACE + _actualFirstWidth + SPACE;
				_item.SpinEdit.Top := SPACE div 2;
				_item.SpinEdit.Width := spinWidth;
				_item.SpinEdit.Height := _itemHeight - 2;
				_item.SpinEdit.Tag := _itemIndex;
			end;
		end;

		citLabelWithCombo : begin
			// Label + ComboBox - both left aligned
			if Assigned(_item.LabelControl) then begin
				_item.LabelControl.Left := SPACE;
				_item.LabelControl.Top := SPACE div 2 + 3; // Slight vertical offset for better alignment
				_item.LabelControl.Width := _actualFirstWidth;
				_item.LabelControl.Height := _itemHeight - 2;
			end;
			if Assigned(_item.ComboBox) then begin
				_item.ComboBox.Left := SPACE + _actualFirstWidth + SPACE;
				_item.ComboBox.Top := SPACE div 2;
				_item.ComboBox.Width := _actualSecondWidth;
				_item.ComboBox.Height := _itemHeight - 2;
				_item.ComboBox.Tag := _itemIndex;
			end;
		end;
	end;
end;

procedure TCustomCheckOptions.AlignControlItems;
const
	ITEM_HEIGHT = 22;
var
	i : Integer;
	itemHeight, baseWidth, currentItemWidth : Integer;
	item : TCustomCheckItem;
	hasTwoControls : Boolean;
	actualFirstWidth, actualSecondWidth : Integer;
	currentLeft, currentTop, currentRow : Integer;
	maxWidth : Integer;
begin
	if FItems.Count = 0 then begin
		Exit;
	end;

	// Calculate layout
	itemHeight := ITEM_HEIGHT;

	// Determine if we have any two-control items
	hasTwoControls := HasTwoControlItems;

	// Calculate base width for layout
	baseWidth := CalculateItemWidth(hasTwoControls);

	// Position panels and their controls
	currentLeft := 0;
	currentTop := 0;
	currentRow := 0;
	maxWidth := 0;
	
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		
		// Get width for this specific item
		currentItemWidth := GetItemWidth(item, baseWidth);
		
		// Check if we need to wrap to next row based on columns
		if (i > 0) and (i mod Columns = 0) then begin
			currentLeft := 0;
			Inc(currentRow);
			currentTop := currentRow * itemHeight;
		end;
		
		// Position the parent panel at current position
		if Assigned(item.ParentPanel) then begin
			item.ParentPanel.Left := currentLeft;
			item.ParentPanel.Top := currentTop;
			item.ParentPanel.Width := currentItemWidth;
			item.ParentPanel.Height := itemHeight + 8; // Height to fully contain controls with margins
		end;
		
		// Calculate actual widths for this item's controls
		CalculateActualWidths(currentItemWidth, actualFirstWidth, actualSecondWidth);
		
		// Position controls within the panel
		PositionItemControls(item, i, actualFirstWidth, actualSecondWidth, itemHeight);
		
		// Move to next position
		currentLeft := currentLeft + currentItemWidth;
		maxWidth := Max(maxWidth, currentLeft);
	end;

	// Adjust control height if needed
	Height := (currentRow + 1) * itemHeight + 16; // Padding for clean layout

	// Update hint helper visibility for all items (handles both disabled items and individual disabled controls)
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		item.updateHintHelperVisibility();
	end;
end;

procedure TCustomCheckOptions.onItemChangeEventHandler(_sender : TObject);
var
	checkBox : TCheckBox;
	combo : TComboBox;
	spin : TSpinEdit;
	itemIndex : Integer;
	item : TCustomCheckItem;
begin
	if not EventsEnabled then begin
		Exit;
	end;

	itemIndex := -1;

	if _sender is TCheckBox then begin
		checkBox := _sender as TCheckBox;
		itemIndex := checkBox.Tag;
	end else if _sender is TComboBox then begin
		combo := _sender as TComboBox;
		itemIndex := combo.Tag;
	end else if _sender is TSpinEdit then begin
		spin := _sender as TSpinEdit;
		itemIndex := spin.Tag;
	end;

	if itemIndex < 0 then begin
		raise Exception.CreateFmt('CustomCheckOptions type %s not supported', [_sender.ClassName]);
	end;

	// Update the checked state in the item
	if (itemIndex >= 0) and (itemIndex < FItems.Count) then begin
		item := FItems[itemIndex];
		if Assigned(item.CheckBox) and (not item.Checked) then begin
			// Clear associated controls when checkbox is unchecked
			if Assigned(item.ComboBox) then begin
				item.ComboBox.Text := '';
			end;
			if Assigned(item.SpinEdit) then begin
				item.SpinEdit.Value := item.MinValue;
			end;
		end;

		// Fire event
		if Assigned(FOnItemChange) then begin
			FOnItemChange(Self, item);
		end;
	end;
end;

function TCustomCheckOptions.getSelectedItems : TArray<TCustomCheckItem>;
var
	i : Integer;
	selectedCount : Integer;
	item : TCustomCheckItem;
begin
	// Count selected items first
	selectedCount := 0;
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		if item.Checked then begin
			Inc(selectedCount);
		end;
	end;

	// Create result array
	SetLength(Result, selectedCount);
	selectedCount := 0;
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		if item.Checked then begin
			Result[selectedCount] := item;
			Inc(selectedCount);
		end;
	end;
end;

function TCustomCheckOptions.GetItemChecked(_idx : Integer) : Boolean;
begin
	Result := FItems[_idx].Checked;
end;

function TCustomCheckOptions.GetItemText(_idx : Integer) : string;
var
	item : TCustomCheckItem;
begin
	Result := '';

	item := FItems[_idx];
	case item.ItemType of
		citCheckBoxWithCombo, citLabelWithCombo : begin
			if Assigned(item.ComboBox) then begin
				Result := item.ComboBox.Text;
				Exit;
			end;
		end;
		citCheckBoxWithSpin : begin
			if Assigned(item.SpinEdit) then begin
				Result := item.SpinEdit.Value.ToString;
				Exit;
			end;
		end;
	end;
end;

function TCustomCheckOptions.GetItemSpinValue(_idx : Integer) : Integer;
var
	item : TCustomCheckItem;
begin
	Result := 0;
	item := FItems[_idx];
	if (item.ItemType = citCheckBoxWithSpin) and Assigned(item.SpinEdit) then begin
		Result := item.SpinEdit.Value;
		Exit;
	end;

end;

function TCustomCheckOptions.GetItemCaption(_idx : Integer) : string;
var
	item : TCustomCheckItem;
begin
	Result := '';

	item := FItems[_idx];

	case item.ItemType of
		citLabelWithCombo : begin
			if Assigned(item.LabelControl) then begin
				Result := item.LabelControl.Caption;
				Exit;
			end;
		end;
		citCheckBoxWithCombo, citCheckBoxWithSpin : begin
			if Assigned(item.CheckBox) then begin
				Result := item.CheckBox.Caption;
				Exit;
			end;
		end;
	end;

end;

function TCustomCheckOptions.GetItemByCaption(const _caption : string) : TCustomCheckItem;
var
	i : Integer;
	item : TCustomCheckItem;
begin
	Result := nil;
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		if item.Caption = _caption then begin
			Result := item;
		end;
	end;
end;

procedure TCustomCheckOptions.SetItemControlEnabled(_itemIdx : Integer; _controlIdx : ESubItemIndex; _enabled : Boolean);
begin
	if (_itemIdx < 0) or (_itemIdx >= FItems.Count) then begin
		raise Exception.CreateFmt('Item index %d out of range (0..%d)', [_itemIdx, FItems.Count - 1]);
	end;

	FItems[_itemIdx].SetControlEnabled(_controlIdx, _enabled);
end;

function TCustomCheckOptions.GetMinimumWidth() : Integer;
const
	SPACE = 8;
	FIRST_CONTROL_WIDTH = 100;
	SECOND_CONTROL_COMBO_WIDTH = 80;
var
	i : Integer;
	item : TCustomCheckItem;
	baseWidth : Integer;
	totalWidth : Integer;
	currentRowWidth : Integer;
	hasTwoControls : Boolean;
	itemWidth : Integer;
begin
	Result := 0;
	
	if FItems.Count = 0 then begin
		Exit;
	end;

	// Determine if we have any two-control items
	hasTwoControls := HasTwoControlItems;

	// Calculate base width for layout
	baseWidth := CalculateItemWidth(hasTwoControls);
	
	// Calculate the width needed for the widest row
	currentRowWidth := 0;
	totalWidth := 0;
	
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		
		// Get width for this specific item
		itemWidth := GetItemWidth(item, baseWidth);
		
		// Check if we need to wrap to next row based on columns
		if (i > 0) and (i mod Columns = 0) then begin
			// Row complete - check if it's the widest
			totalWidth := Max(totalWidth, currentRowWidth);
			currentRowWidth := 0;
		end;
		
		// Add item width to current row
		currentRowWidth := currentRowWidth + itemWidth;
	end;
	
	// Check last row
	totalWidth := Max(totalWidth, currentRowWidth);
	
	// Add some padding for borders and margins
	Result := totalWidth + 16; // 16px for panel margins and borders
end;

procedure Register;
begin
	RegisterComponents('Custom', [TCustomCheckOptions]);
end;

class function TAutoSetReset.New(var _bValue : Boolean; const _bInitValue : Boolean = True) : TAutoSetReset;
begin
	_bValue := _bInitValue;
	Result.FBoolPtr := @_bValue;
end;

class operator TAutoSetReset.Finalize(var Dest : TAutoSetReset);
begin
	if Assigned(Dest.FBoolPtr) then begin
		Dest.FBoolPtr^ := not Dest.FBoolPtr^;
	end;
end;

end.
