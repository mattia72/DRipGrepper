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
	Winapi.Messages,
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
	TCustomCheckItem = class;

	// Custom SpinEdit that notifies parent TCustomCheckItem about enabled state changes
	TNotifyingSpinEdit = class(TSpinEdit)
		private
			FOwnerItem : TCustomCheckItem;

		protected
			procedure CMEnabledChanged(var Message : TMessage); message CM_ENABLEDCHANGED;

		public
			property OwnerItem : TCustomCheckItem read FOwnerItem write FOwnerItem;
	end;

	// Custom ComboBox that notifies parent TCustomCheckItem about enabled state changes
	TNotifyingComboBox = class(TComboBox)
		private
			FOwnerItem : TCustomCheckItem;

		protected
			procedure CMEnabledChanged(var Message : TMessage); message CM_ENABLEDCHANGED;

		public
			property OwnerItem : TCustomCheckItem read FOwnerItem write FOwnerItem;
	end;

	// Custom CheckBox that notifies parent TCustomCheckItem about enabled state changes
	TNotifyingCheckBox = class(TCheckBox)
		private
			FOwnerItem : TCustomCheckItem;

		protected
			procedure CMEnabledChanged(var Message : TMessage); message CM_ENABLEDCHANGED;

		public
			property OwnerItem : TCustomCheckItem read FOwnerItem write FOwnerItem;

		published
			property AutoSize;
	end;

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
			FIndex : Integer;
			FStartNewRow : Boolean;
			FShowInExpertModeOnly : Boolean;
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
			function getSubItemEnabled(const _idx : ESubItemIndex) : Boolean;
			function hasAnyDisabledControl() : Boolean;
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
			procedure setSubItemEnabled(const _idx : ESubItemIndex; const _bEnable : Boolean);
			procedure showSubItem(const _idx : ESubItemIndex; const _bShow : Boolean = True);

		public
			constructor Create(Collection : TCollection); override;
			destructor Destroy; override;

			// Called by notifying controls when their enabled state changes externally
			procedure OnSubItemEnabledChanged(_sender : TObject);
			procedure SetControlEnabled(_index : ESubItemIndex; _enabled : Boolean);
			procedure ShowControl(_index : ESubItemIndex; const _bShow : Boolean);

			property CheckBox : TCheckBox read getCheckBox;
			property LabelControl : TLabel read getLabel;
			property ComboBox : TComboBox read getComboBox;
			property SpinEdit : TSpinEdit read getSpinEdit;
			property HintHelper : TLabel read GetHintHelper;
			property ParentPanel : TPanel read GetParentPanel;
			property ComboBoxItems : TStringList read FComboBoxItems write setComboBoxItems;
			property Caption : string read FCaption write setCaption;
			property TagObject : IInterface read FTagObject write setTagObject;
			property index : Integer read FIndex write FIndex;
			property Checked : Boolean read getChecked write setChecked;
			property ItemType : ECustomItemType read FItemType write FItemType;
			property ComboText : string read getComboText write setComboText;
			property SpinValue : Integer read getSpinValue write setSpinValue;
			property MinValue : Integer read FMinValue write setMinValue;
			property MaxValue : Integer read FMaxValue write setMaxValue;
			property Setting : ISetting read getSetting write setSetting;
			property Enabled : Boolean read getEnabled write setEnabled;
			property DisabledHint : string read FDisabledHint write FDisabledHint;
			// When True, this item will be shown only when expert mode is enabled
			property ShowInExpertModeOnly : Boolean read FShowInExpertModeOnly write FShowInExpertModeOnly;
			// When True, this item will start on a new row regardless of the current column position
			property StartNewRow : Boolean read FStartNewRow write FStartNewRow;
	end;

	TCustomCheckItemsEnumerator = class;

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
			function GetEnumerator : TCustomCheckItemsEnumerator;
			property Items[index : Integer] : TCustomCheckItem read getItem write setItem; default;
	end;

	// Enumerator for TCustomCheckItems
	TCustomCheckItemsEnumerator = class
		private
			FCollection : TCustomCheckItems;
			FIndex : Integer;

		public
			constructor Create(_collection : TCustomCheckItems);
			function GetCurrent : TCustomCheckItem;
			function MoveNext : Boolean;
			property Current : TCustomCheckItem read GetCurrent;
	end;

	// Event type for item selection
	TItemChangedEvent = procedure(Sender : TObject; Item : TCustomCheckItem) of object;

	// Base class for custom option controls
	TCustomOptionsBase = class(TCustomPanel)
		strict private
			FUseFlowLayout : Boolean;
			FColumns : Integer;
			procedure setColumns(const _value : Integer);

		protected
			procedure Resize; override;

		public
			constructor Create(_owner : TComponent); override;
			procedure AlignControlItems(); virtual; abstract;
			procedure Clear; virtual; abstract;
			property UseFlowLayout : Boolean read FUseFlowLayout write FUseFlowLayout;

		published
			property Columns : Integer read FColumns write setColumns default 1;
	end;

	// Custom checkbox options control
	TCustomCheckOptions = class(TCustomOptionsBase)
		const
			ITEM_HEIGHT = 22;
			PANEL_HEIGHT = 28;
			BOTTOM_PADDING = 16;
			SPACE = 8;
			SECOND_CONTROL_SPIN_WIDTH = 60;
			FIRST_CONTROL_WIDTH = 100;
			SECOND_CONTROL_COMBO_WIDTH = 80;
			CHECKBOX_MARGIN = 20;

		strict private
			FItems : TCustomCheckItems;
			FOnItemChange : TItemChangedEvent;
			FSettings : TRipGrepperSettings;
			procedure onItemChangeEventHandler(_sender : TObject);
			function getSelectedItems : TArray<TCustomCheckItem>;
			procedure SetSettings(const Value : TRipGrepperSettings);

		private
			function CreateComboBox(const _name, _hint : string; _parent : TPanel) : TComboBox;

		protected
			FEventsEnabled : Boolean;
			function HasTwoControlItems : Boolean;
			function CalculateItemWidth(const _hasTwoControls : Boolean) : Integer;
			function GetItemWidth(const _item : TCustomCheckItem; const _baseWidth : Integer) : Integer;
			procedure CalculateActualWidths(const _itemWidth : Integer; out _actualFirstWidth, _actualSecondWidth : Integer);
			procedure PositionItemPanel(const _item : TCustomCheckItem; const _itemIndex, _itemWidth, _itemHeight : Integer);
			procedure PositionItemControls(const _item : TCustomCheckItem; const _itemIndex, _actualFirstWidth, _actualSecondWidth,
				_itemHeight : Integer);
			// Helper methods for positioning different control types
			procedure PositionCheckBoxOnly(const _item : TCustomCheckItem; const _itemIndex, _actualFirstWidth, _itemHeight : Integer);
			procedure PositionCheckBoxWithCombo(const _item : TCustomCheckItem; const _itemIndex, _actualFirstWidth, _actualSecondWidth,
				_itemHeight : Integer);
			procedure PositionCheckBoxWithSpin(const _item : TCustomCheckItem; const _itemIndex, _actualFirstWidth, _actualSecondWidth,
				_itemHeight : Integer);
			procedure PositionLabelWithCombo(const _item : TCustomCheckItem; const _itemIndex, _actualFirstWidth, _actualSecondWidth,
				_itemHeight : Integer);
			function CreateItemWithPanel(const _caption : string; _itemType : ECustomItemType; _setting : ISetting) : TCustomCheckItem;
			function CreateCheckBox(const _parent : TWinControl; const _caption, _hint, _settingName : string) : TCheckBox;
			procedure AdjustParentHeights;

		public
			constructor Create(_owner : TComponent); override;
			destructor Destroy; override;
			procedure Clear; override;
			function AddCheckboxItem(const _caption, _hint : string; _setting : ISetting; _startNewRow : Boolean = False;
				_showInExpertModeOnly : Boolean = False) : TCustomCheckItem; overload;
			function AddCheckboxComboItem(const _caption, _hint : string; _comboItems : TArray<string>; _setting : ISetting;
				_startNewRow : Boolean = False; _showInExpertModeOnly : Boolean = False) : TCustomCheckItem; overload;
			function AddCheckboxSpinItem(const _caption, _hint : string; _minValue, _maxValue, _defaultValue : Integer; _setting : ISetting;
				_startNewRow : Boolean = False; _showInExpertModeOnly : Boolean = False) : TCustomCheckItem;
			function AddLabelComboItem(const _caption, _hint : string; _comboItems : TArray<string>; _setting : ISetting;
				_startNewRow : Boolean = False; _showInExpertModeOnly : Boolean = False) : TCustomCheckItem;
			procedure AlignControlItems(); override;
			// Getter functions for specific items by order index
			function GetItemChecked(_idx : Integer) : Boolean;
			function GetItemText(_idx : Integer) : string;
			function GetItemSpinValue(_idx : Integer) : Integer;
			function GetItemCaption(_idx : Integer) : string;
			function GetItemByCaption(const _caption : string) : TCustomCheckItem;
			procedure SetItemControlEnabled(_itemIdx : Integer; _controlIdx : ESubItemIndex; _enabled : Boolean);
			function GetMinimumWidth() : Integer;
			procedure ShowExpertItems(const _bShow : Boolean = True);
			procedure SetDefaultValues();
			property EventsEnabled : Boolean read FEventsEnabled write FEventsEnabled;
			property SelectedItems : TArray<TCustomCheckItem> read getSelectedItems;
			property Items : TCustomCheckItems read FItems write FItems;
			property Settings : TRipGrepperSettings read FSettings write SetSettings;
			property OnItemChange : TItemChangedEvent read FOnItemChange write FOnItemChange;
	end;

procedure Register;

implementation

uses
	Math,
	RipGrepper.Common.IDEContextValues,
	RipGrepper.Common.SimpleTypes,
	Spring,
	RipGrepper.Tools.DebugUtils;

{ TCustomCheckItemsEnumerator }

constructor TCustomCheckItemsEnumerator.Create(_collection : TCustomCheckItems);
begin
	inherited Create;
	FCollection := _collection;
	FIndex := -1;
end;

function TCustomCheckItemsEnumerator.GetCurrent : TCustomCheckItem;
begin
	Result := FCollection.Items[FIndex];
end;

function TCustomCheckItemsEnumerator.MoveNext : Boolean;
begin
	Inc(FIndex);
	Result := FIndex < FCollection.Count;
end;

{ TCustomOptionsBase }

constructor TCustomOptionsBase.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	FColumns := 1;
	Caption := '';
	BevelOuter := bvNone;
	Width := 200;
	Height := 100;
	FUseFlowLayout := False;
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
	// AlignControlItems should be called explicitly when needed
	// Automatic alignment during resize causes performance issues during initialization
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
	FStartNewRow := False;
	FShowInExpertModeOnly := False;
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

function TCustomCheckItem.getSubItemEnabled(const _idx : ESubItemIndex) : Boolean;
begin
	Result := False;
	var
	ctrl := FSubItems[Ord(_idx)];
	if Assigned(ctrl) and (ctrl is TWinControl) then begin
		Result := TWinControl(ctrl).Enabled;
	end;
end;

function TCustomCheckItem.hasAnyDisabledControl() : Boolean;
begin
	// Check if the whole item is disabled OR any individual control is disabled
	Result := (not FEnabled) or (not getSubItemEnabled(siFirst)) or (not getSubItemEnabled(siSecond));
end;

procedure TCustomCheckItem.updateHintHelperVisibility();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TCustomCheckItem.updateHintHelperVisibility');

	// Show hint helper only if there are disabled controls
	if hasAnyDisabledControl then begin
		dbgMsg.MsgFmt('Show hint helper of item %s', [Setting.Name]);
		showHintHelper();
	end else begin
		dbgMsg.MsgFmt('Hide hint helper of item %s', [Setting.Name]);
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
end;

procedure TCustomCheckItem.setEnabled(const _value : Boolean);
begin
	if FEnabled = _value then begin
		Exit;
	end;

	FEnabled := _value;
	enableCtrls(FEnabled);
end;

procedure TCustomCheckItem.showHintHelper();
var
	firstCtrl, secondCtrl : TControl;
	leftPos, topPos, rightPos, bottomPos : Integer;
	hintText : string;
	firstEnabled, secondEnabled : Boolean;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TCustomCheckItem.showHintHelper');

	// Ensure controls are assigned
	firstCtrl := FSubItems[Ord(siFirst)];
	if not Assigned(firstCtrl) then begin
		Exit;
	end;

	secondCtrl := FSubItems[Ord(siSecond)];

	// Get enabled states from controls
	firstEnabled := getSubItemEnabled(siFirst);
	secondEnabled := getSubItemEnabled(siSecond);

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

procedure TCustomCheckItem.setSubItemEnabled(const _idx : ESubItemIndex; const _bEnable : Boolean);
begin
	var
	ctrl := FSubItems[Ord(_idx)];
	if Assigned(ctrl) and (ctrl is TWinControl) then begin
		TWinControl(ctrl).Enabled := _bEnable;
	end;
end;

procedure TCustomCheckItem.OnSubItemEnabledChanged(_sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TCustomCheckItem.OnSubItemEnabledChanged');
	dbgMsg.MsgFmt('Control %s enabled state changed to: %s for item %s',
		{ } [_sender.ClassName, BoolToStr(TWinControl(_sender).Enabled, True), Setting.Name]);

	// Update hint helper when control enabled state changes externally
	updateHintHelperVisibility();
end;

procedure TCustomCheckItem.showSubItem(const _idx : ESubItemIndex; const _bShow : Boolean = True);
begin
	var
	ctrl := FSubItems[Ord(_idx)];
	if Assigned(ctrl) and (ctrl is TWinControl) then begin
		TWinControl(ctrl).Visible := _bShow;
	end;
end;

procedure TCustomCheckItem.ShowControl(_index : ESubItemIndex; const _bShow : Boolean);
var
	ctrl : TControl;
begin
	ctrl := FSubItems[Ord(_index)];

	// Check if already in desired state
	if not Assigned(ctrl) or
	{ } ((ctrl is TWinControl) and (TWinControl(ctrl).Visible = _bShow)) then begin
		Exit;
	end;

	showSubItem(_index, _bShow);
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

function TCustomCheckItems.GetEnumerator : TCustomCheckItemsEnumerator;
begin
	Result := TCustomCheckItemsEnumerator.Create(Self);
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

function TCustomCheckOptions.CreateItemWithPanel(const _caption : string; _itemType : ECustomItemType; _setting : ISetting)
	: TCustomCheckItem;
begin
	var
	ar := TAutoSetReset.New(FEventsEnabled, False);

	// Create the item to get the parent panel
	Result := FItems.Add;
	Result.FCaption := _caption;
	Result.TagObject := _setting;
	Result.FItemType := _itemType;
	Result.FSetting := _setting;
	Result.Index := Items.Count - 1;
end;

function TCustomCheckOptions.CreateCheckBox(const _parent : TWinControl; const _caption, _hint, _settingName : string) : TCheckBox;
begin
	Result := TNotifyingCheckBox.Create(Self);
	Result.Name := 'cb' + _settingName;
	Result.Parent := _parent;
	Result.Caption := _caption;
	Result.Hint := _hint;
	Result.ShowHint := True;
	Result.OnClick := onItemChangeEventHandler;
	// Note: AutoSize for TCheckBox doesn't work as expected in all Delphi versions
	// Will calculate width based on caption manually in AlignControlItems
end;

function TCustomCheckOptions.AddCheckboxItem(const _caption, _hint : string; _setting : ISetting; _startNewRow : Boolean = False;
	_showInExpertModeOnly : Boolean = False) : TCustomCheckItem;
var
	checkBox : TNotifyingCheckBox;
	caption : string;
	expertChoice : Boolean;
begin
	Result := CreateItemWithPanel(_caption, citCheckBox, _setting);
	Result.StartNewRow := _startNewRow;
	Result.ShowInExpertModeOnly := _showInExpertModeOnly;

	// Check if item should be shown based on expert mode
	if not(Assigned(FSettings) and Assigned(FSettings.AppSettings)) then begin
		// No settings available, add item normally
	end else begin
		expertChoice := FSettings.AppSettings.IsExpertMode and _showInExpertModeOnly;
		if not expertChoice and _showInExpertModeOnly then begin
			// Expert mode only item, but expert mode is off - hide the control
			Result.ParentPanel.Visible := False;
		end;
	end;

	// Adjust caption if expert mode item
	if _showInExpertModeOnly and Assigned(FSettings) and FSettings.AppSettings.IsExpertMode then begin
		caption := _caption + ' *';
	end else begin
		caption := _caption;
	end;

	// Create the checkbox
	checkBox := CreateCheckBox(Result.ParentPanel, caption, _hint, _setting.Name) as TNotifyingCheckBox;
	checkBox.OwnerItem := Result;

	Result.FSubItems[Ord(siFirst)] := checkBox;
end;

function TCustomCheckOptions.AddCheckboxComboItem(const _caption, _hint : string; _comboItems : TArray<string>; _setting : ISetting;
	_startNewRow : Boolean = False; _showInExpertModeOnly : Boolean = False) : TCustomCheckItem;
var
	checkBox : TNotifyingCheckBox;
	comboBox : TNotifyingComboBox;
	caption : string;
	expertChoice : Boolean;
begin
	Result := CreateItemWithPanel(_caption, citCheckBoxWithCombo, _setting);
	Result.StartNewRow := _startNewRow;
	Result.ShowInExpertModeOnly := _showInExpertModeOnly;

	// Check if item should be shown based on expert mode
	if not(Assigned(FSettings) and Assigned(FSettings.AppSettings)) then begin
		// No settings available, add item normally
	end else begin
		expertChoice := FSettings.AppSettings.IsExpertMode and _showInExpertModeOnly;
		if not expertChoice and _showInExpertModeOnly then begin
			// Expert mode only item, but expert mode is off - hide the control
			Result.ParentPanel.Visible := False;
		end;
	end;

	// Adjust caption if expert mode item
	if _showInExpertModeOnly and Assigned(FSettings) and FSettings.AppSettings.IsExpertMode then begin
		caption := _caption + ' *';
	end else begin
		caption := _caption;
	end;

	checkBox := CreateCheckBox(Result.ParentPanel, caption, _hint, _setting.Name) as TNotifyingCheckBox;
	checkBox.OwnerItem := Result;

	comboBox := CreateComboBox(_setting.Name, _hint, Result.ParentPanel) as TNotifyingComboBox;
	comboBox.OwnerItem := Result;

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
	_setting : ISetting; _startNewRow : Boolean = False; _showInExpertModeOnly : Boolean = False) : TCustomCheckItem;
var
	checkBox : TNotifyingCheckBox;
	spinEdit : TNotifyingSpinEdit;
	caption : string;
	expertChoice : Boolean;
begin
	Result := CreateItemWithPanel(_caption, citCheckBoxWithSpin, _setting);
	Result.StartNewRow := _startNewRow;
	Result.ShowInExpertModeOnly := _showInExpertModeOnly;
	Result.MinValue := _minValue;
	Result.MaxValue := _maxValue;
	Result.SpinValue := _defaultValue;

	// Check if item should be shown based on expert mode
	if not(Assigned(FSettings) and Assigned(FSettings.AppSettings)) then begin
		// No settings available, add item normally
	end else begin
		expertChoice := FSettings.AppSettings.IsExpertMode and _showInExpertModeOnly;
		if not expertChoice and _showInExpertModeOnly then begin
			// Expert mode only item, but expert mode is off - hide the control
			Result.ParentPanel.Visible := False;
		end;
	end;

	// Adjust caption if expert mode item
	if _showInExpertModeOnly and Assigned(FSettings) and FSettings.AppSettings.IsExpertMode then begin
		caption := _caption + ' *';
	end else begin
		caption := _caption;
	end;

	// Create the checkbox
	checkBox := CreateCheckBox(Result.ParentPanel, caption, _hint, _setting.Name) as TNotifyingCheckBox;
	checkBox.OwnerItem := Result;

	// Create the spin edit
	spinEdit := TNotifyingSpinEdit.Create(Self);
	spinEdit.Name := 'spin' + _setting.Name;
	spinEdit.Parent := Result.ParentPanel;
	spinEdit.MinValue := _minValue;
	spinEdit.MaxValue := _maxValue;
	spinEdit.Value := _defaultValue;
	spinEdit.Hint := _hint;
	spinEdit.ShowHint := True;
	spinEdit.OnChange := onItemChangeEventHandler;
	spinEdit.Height := checkBox.Height;
	spinEdit.OwnerItem := Result;

	Result.FSubItems[Ord(siFirst)] := checkBox;
	Result.FSubItems[Ord(siSecond)] := spinEdit;
end;

function TCustomCheckOptions.AddLabelComboItem(const _caption, _hint : string; _comboItems : TArray<string>; _setting : ISetting;
	_startNewRow : Boolean = False; _showInExpertModeOnly : Boolean = False) : TCustomCheckItem;
var
	labelControl : TLabel;
	comboBox : TNotifyingComboBox;
	comboItems : IShared<TStringList>;
	caption : string;
	expertChoice : Boolean;
begin
	Result := CreateItemWithPanel(_caption, citLabelWithCombo, _setting);
	Result.StartNewRow := _startNewRow;
	Result.ShowInExpertModeOnly := _showInExpertModeOnly;

	// Check if item should be shown based on expert mode
	if not(Assigned(FSettings) and Assigned(FSettings.AppSettings)) then begin
		// No settings available, add item normally
	end else begin
		expertChoice := FSettings.AppSettings.IsExpertMode and _showInExpertModeOnly;
		if not expertChoice and _showInExpertModeOnly then begin
			// Expert mode only item, but expert mode is off - hide the control
			Result.ParentPanel.Visible := False;
		end;
	end;

	// Adjust caption if expert mode item
	if _showInExpertModeOnly and Assigned(FSettings) and FSettings.AppSettings.IsExpertMode then begin
		caption := _caption + ' *';
	end else begin
		caption := _caption;
	end;

	// Create the label
	labelControl := TLabel.Create(Self);
	labelControl.Name := 'cb' + _setting.Name;
	labelControl.Parent := Result.ParentPanel;
	labelControl.Caption := caption;
	labelControl.Hint := _hint;
	labelControl.ShowHint := True;
	labelControl.AutoSize := True;

	// Create the combo box
	comboBox := CreateComboBox(_setting.Name, _hint, Result.ParentPanel) as TNotifyingComboBox;
	comboBox.OwnerItem := Result;

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

procedure TCustomCheckOptions.CalculateActualWidths(const _itemWidth : Integer; out _actualFirstWidth, _actualSecondWidth : Integer);
var
	availableForControls : Integer;
begin
	// If only one item exists, don't calculate with second control width
	if FItems.Count = 1 then begin
		_actualFirstWidth := _itemWidth - (2 * SPACE);
		_actualSecondWidth := 0;
		Exit;
	end;

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

procedure TCustomCheckOptions.PositionItemPanel(const _item : TCustomCheckItem; const _itemIndex, _itemWidth, _itemHeight : Integer);
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

procedure TCustomCheckOptions.PositionCheckBoxOnly(const _item : TCustomCheckItem;
	const _itemIndex, _actualFirstWidth, _itemHeight : Integer);
var
	iTextWidth : integer;
begin
	if not Assigned(_item.CheckBox) then begin
		Exit;
	end;

	_item.CheckBox.Left := SPACE;
	_item.CheckBox.Top := SPACE div 2;
	iTextWidth := Canvas.TextWidth(_item.CheckBox.Caption) + CHECKBOX_MARGIN;
	if UseFlowLayout then begin
		// Flow layout: calculate width based on caption text width using parent's canvas
		// var checkboxWidth : integer := Canvas.TextWidth(_item.CheckBox.Caption) + CHECKBOX_MARGIN;
		// it causes not used variable warning !
		if (FItems.Count = 1) then begin
			_item.CheckBox.Width := Max(FIRST_CONTROL_WIDTH, iTextWidth);
		end else begin
			_item.CheckBox.Width := iTextWidth;
		end;
	end else begin
		_item.CheckBox.Width := Max(_item.ParentPanel.Width - (2 * SPACE), iTextWidth);
	end;
	_item.CheckBox.Height := _itemHeight - 2;
	_item.CheckBox.Tag := _itemIndex;
end;

procedure TCustomCheckOptions.PositionCheckBoxWithCombo(const _item : TCustomCheckItem;
	const _itemIndex, _actualFirstWidth, _actualSecondWidth, _itemHeight : Integer);
begin
	if Assigned(_item.CheckBox) then begin
		_item.CheckBox.Left := SPACE;
		_item.CheckBox.Top := SPACE div 2;

		if UseFlowLayout then begin
			// Flow layout: calculate width based on caption text width using parent's canvas
			_item.CheckBox.Width := Canvas.TextWidth(_item.CheckBox.Caption) + CHECKBOX_MARGIN;
		end else begin
			// Table layout: use fixed width
			_item.CheckBox.Width := _actualFirstWidth;
		end;

		_item.CheckBox.Height := _itemHeight - 2;
		_item.CheckBox.Tag := _itemIndex;
	end;

	// Position combobox
	if Assigned(_item.ComboBox) then begin
		if UseFlowLayout then begin
			// Flow layout: position combobox after the checkbox's actual width
			_item.ComboBox.Left := _item.CheckBox.Left + _item.CheckBox.Width + SPACE;
		end else begin
			// Table layout: use fixed position
			_item.ComboBox.Left := SPACE + _actualFirstWidth + SPACE;
		end;

		_item.ComboBox.Top := SPACE div 2;
		_item.ComboBox.Width := _actualSecondWidth;
		_item.ComboBox.Height := _itemHeight - 2;
		_item.ComboBox.Tag := _itemIndex;
	end;
end;

procedure TCustomCheckOptions.PositionCheckBoxWithSpin(const _item : TCustomCheckItem;
	const _itemIndex, _actualFirstWidth, _actualSecondWidth, _itemHeight : Integer);
var
	spinWidth : Integer;
begin
	if Assigned(_item.CheckBox) then begin
		_item.CheckBox.Left := SPACE;
		_item.CheckBox.Top := SPACE div 2;

		if UseFlowLayout then begin
			// Flow layout: calculate width based on caption text width using parent's canvas
			_item.CheckBox.Width := Canvas.TextWidth(_item.CheckBox.Caption) + CHECKBOX_MARGIN;
		end else begin
			// Table layout: use fixed width
			_item.CheckBox.Width := _actualFirstWidth;
		end;

		_item.CheckBox.Height := _itemHeight - 2;
		_item.CheckBox.Tag := _itemIndex;
	end;

	// Position spin edit
	if Assigned(_item.SpinEdit) then begin
		// Spin control gets smaller width than combo
		spinWidth := Min(SECOND_CONTROL_SPIN_WIDTH, _actualSecondWidth);

		if UseFlowLayout then begin
			// Flow layout: position spin after the checkbox's actual width
			_item.SpinEdit.Left := _item.CheckBox.Left + _item.CheckBox.Width + SPACE;
		end else begin
			// Table layout: use fixed position
			_item.SpinEdit.Left := SPACE + _actualFirstWidth + SPACE;
		end;

		_item.SpinEdit.Top := SPACE div 2;
		_item.SpinEdit.Width := spinWidth;
		_item.SpinEdit.Height := _itemHeight - 2;
		_item.SpinEdit.Tag := _itemIndex;
	end;
end;

procedure TCustomCheckOptions.PositionLabelWithCombo(const _item : TCustomCheckItem;
	const _itemIndex, _actualFirstWidth, _actualSecondWidth, _itemHeight : Integer);
const
	SPACE = 8;
begin
	// Position label
	if Assigned(_item.LabelControl) then begin
		_item.LabelControl.Left := SPACE;
		_item.LabelControl.Top := SPACE div 2 + 3; // Slight vertical offset for better alignment

		if not UseFlowLayout then begin
			// Table layout: set fixed width (override AutoSize if needed)
			_item.LabelControl.Width := _actualFirstWidth;
		end;
		// Flow layout: AutoSize handles width automatically (set in AddLabelComboItem)

		_item.LabelControl.Height := _itemHeight - 2;
	end;

	// Position combobox
	if Assigned(_item.ComboBox) then begin
		if UseFlowLayout then begin
			// Flow layout: position combobox after the label's actual width (respecting AutoSize)
			if Assigned(_item.LabelControl) and _item.LabelControl.AutoSize then begin
				_item.ComboBox.Left := _item.LabelControl.Left + _item.LabelControl.Width + SPACE;
			end else begin
				_item.ComboBox.Left := SPACE + _actualFirstWidth + SPACE;
			end;
		end else begin
			// Table layout: use fixed position
			_item.ComboBox.Left := SPACE + _actualFirstWidth + SPACE;
		end;

		_item.ComboBox.Top := SPACE div 2;
		_item.ComboBox.Width := _actualSecondWidth;
		_item.ComboBox.Height := _itemHeight - 2;
		_item.ComboBox.Tag := _itemIndex;
	end;
end;

procedure TCustomCheckOptions.PositionItemControls(const _item : TCustomCheckItem; const _itemIndex, _actualFirstWidth, _actualSecondWidth,
	_itemHeight : Integer);
begin
	// Position controls within the panel (relative to panel) - delegate to specific helper methods
	case _item.ItemType of
		citCheckBox :
		PositionCheckBoxOnly(_item, _itemIndex, _actualFirstWidth, _itemHeight);
		citCheckBoxWithCombo :
		PositionCheckBoxWithCombo(_item, _itemIndex, _actualFirstWidth, _actualSecondWidth, _itemHeight);
		citCheckBoxWithSpin :
		PositionCheckBoxWithSpin(_item, _itemIndex, _actualFirstWidth, _actualSecondWidth, _itemHeight);
		citLabelWithCombo :
		PositionLabelWithCombo(_item, _itemIndex, _actualFirstWidth, _actualSecondWidth, _itemHeight);
	end;
end;

procedure TCustomCheckOptions.AdjustParentHeights;
var
	p : TWinControl;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TCustomCheckOptions.AdjustParentHeights');

	p := Parent;

	while Assigned(p) do begin
		// Only adjust panels and groupboxes
		if (p is TPanel) or (p is TGroupBox) then begin
			// Set parent height to match content height
			// Add extra padding for groupboxes to account for border and caption
			if p is TGroupBox then begin
				if p.Height < Height + SPACE then begin
					p.Height := Height + SPACE;
					dbgMsg.MsgFmt('GroupBox %s height: %d', [p.Name, p.Height]);
				end;
				break;
			end else begin
				if (p.Height < Height) then begin
					p.Height := Height;
					dbgMsg.MsgFmt('Not GroupBox %s height: %d', [p.Name, p.Height]);
				end;
			end;
		end;
		p := p.Parent;
	end;
end;

procedure TCustomCheckOptions.AlignControlItems();
var
	i : Integer;
	baseWidth, currentItemWidth : Integer;
	item : TCustomCheckItem;
	actualFirstWidth, actualSecondWidth : Integer;
	currentLeft, currentTop, currentRow : Integer;
	maxWidth : Integer;
	colCount : Integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TCustomCheckOptions.AlignControlItems');

	if FItems.Count = 0 then begin
		Exit;
	end;

	// Calculate base width for layout
	baseWidth := CalculateItemWidth(HasTwoControlItems);

	// Position panels and their controls
	currentLeft := 0;
	currentTop := 0;
	currentRow := 0;
	maxWidth := 0;
	colCount := 0;

	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];

		// Skip hidden items
		if Assigned(item.ParentPanel) and not item.ParentPanel.Visible then begin
			Continue;
		end;

		// Check if this item should start a new row
		if item.StartNewRow and (i > 0) then begin
			// Force new row
			currentLeft := 0;
			Inc(currentRow);
			currentTop := currentRow * PANEL_HEIGHT;
			colCount := 0;
		end else begin
			// Check if we need to wrap to next row based on columns
			if (colCount > 0) and (colCount mod Columns = 0) then begin
				currentLeft := 0;
				Inc(currentRow);
				currentTop := currentRow * PANEL_HEIGHT;
				colCount := 0;
			end;
		end;

		// Get width for this specific item
		currentItemWidth := GetItemWidth(item, baseWidth);

		// Position the parent panel at current position
		if Assigned(item.ParentPanel) then begin
			item.ParentPanel.Left := currentLeft;
			item.ParentPanel.Top := currentTop;
			item.ParentPanel.Width := currentItemWidth;
			item.ParentPanel.Height := PANEL_HEIGHT;
			dbgMsg.MsgFmt('Item %s height: %d', [item.Setting.Name, item.ParentPanel.Height]);
		end;

		// Calculate actual widths for this item's controls
		CalculateActualWidths(currentItemWidth, actualFirstWidth, actualSecondWidth);

		// Position controls within the panel
		PositionItemControls(item, i, actualFirstWidth, actualSecondWidth, ITEM_HEIGHT);

		// Move to next position
		currentLeft := currentLeft + currentItemWidth;
		maxWidth := Max(maxWidth, currentLeft);
		Inc(colCount);
	end;

	// Adjust parent heights to accommodate content
	AdjustParentHeights;

	// Update hint helper visibility for all items (handles both disabled items and individual disabled controls)
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		// Skip hidden items
		if Assigned(item.ParentPanel) and not item.ParentPanel.Visible then begin
			Continue;
		end;
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

function TCustomCheckOptions.CreateComboBox(const _name, _hint : string; _parent : TPanel) : TComboBox;
begin
	Result := TNotifyingComboBox.Create(Self);
	Result.Name := 'cmb' + _name;
	Result.Parent := _parent;
	Result.Hint := _hint;
	Result.ShowHint := True;
	Result.Style := csDropDown;
	Result.AutoDropDownWidth := True;
	Result.OnChange := onItemChangeEventHandler;
end;

procedure TCustomCheckOptions.SetSettings(const Value : TRipGrepperSettings);
begin
	FSettings := Value;
end;

procedure TCustomCheckOptions.ShowExpertItems(const _bShow : Boolean = True);
begin
	for var item in Items do begin
		if item.ShowInExpertModeOnly then begin
			item.ParentPanel.Visible := _bShow;
		end;
	end;
end;

procedure TCustomCheckOptions.SetDefaultValues();
begin
	for var item in Items do begin
		if item.ShowInExpertModeOnly then begin
			item.Setting.SetDefaultValue();
		end;
	end;
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

{ TNotifyingSpinEdit }

procedure TNotifyingSpinEdit.CMEnabledChanged(var Message : TMessage);
begin
	inherited;
	if Assigned(FOwnerItem) then begin
		FOwnerItem.OnSubItemEnabledChanged(Self);
	end;
end;

{ TNotifyingComboBox }

procedure TNotifyingComboBox.CMEnabledChanged(var Message : TMessage);
begin
	inherited;
	if Assigned(FOwnerItem) then begin
		FOwnerItem.OnSubItemEnabledChanged(Self);
	end;
end;

{ TNotifyingCheckBox }

procedure TNotifyingCheckBox.CMEnabledChanged(var Message : TMessage);
begin
	inherited;
	if Assigned(FOwnerItem) then begin
		FOwnerItem.OnSubItemEnabledChanged(Self);
	end;
end;

end.
