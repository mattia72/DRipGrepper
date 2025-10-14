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
	RipGrepper.Settings.RipGrepperSettings;

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
	TCustomItemType = (citCheckBox, citCheckBoxWithCombo, citCheckBoxWithSpin, citLabelWithCombo);

	// Custom collection item for checkbox items
	TCustomCheckItem = class(TCollectionItem)
		private
			FCaption : string;
			FOrderIndex : Integer;
			FTagObject : IInterface;
			FCheckBox : TCheckBox;
			FLabel : TLabel;
			FComboBox : TComboBox;
			FSpinEdit : TSpinEdit;
			FComboBoxItems : TStringList;
			FItemType : TCustomItemType;
			FMinValue : Integer;
			FMaxValue : Integer;
			FSpinValue : Integer;
			function getChecked() : Boolean;
			function getComboText() : string;
			function getSpinValue() : Integer;
			procedure setCaption(const _value : string);
			procedure setOrderIndex(const _value : Integer);
			procedure setTagObject(const Value : IInterface);
			procedure setChecked(const _value : Boolean);
			procedure setComboBoxItems(const _value : TStringList);
			procedure setComboText(const _value : string);
			procedure setSpinValue(const _value : Integer);
			procedure setMinValue(const _value : Integer);
			procedure setMaxValue(const _value : Integer);

		public
			constructor Create(Collection : TCollection); override;
			destructor Destroy; override;

			property CheckBox : TCheckBox read FCheckBox write FCheckBox;
			property LabelControl : TLabel read FLabel write FLabel;
			property ComboBox : TComboBox read FComboBox write FComboBox;
			property SpinEdit : TSpinEdit read FSpinEdit write FSpinEdit;
			property ComboBoxItems : TStringList read FComboBoxItems write setComboBoxItems;
			property Caption : string read FCaption write setCaption;
			property OrderIndex : Integer read FOrderIndex write setOrderIndex;
			property TagObject : IInterface read FTagObject write setTagObject;
			property Checked : Boolean read getChecked write setChecked;
			property ItemType : TCustomItemType read FItemType write FItemType;
			property ComboText : string read getComboText write setComboText;
			property SpinValue : Integer read getSpinValue write setSpinValue;
			property MinValue : Integer read FMinValue write setMinValue;
			property MaxValue : Integer read FMaxValue write setMaxValue;
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
			function AddItem(_cb : TCheckBox; const _caption : string; const _orderIndex : Integer; _obj : IInterface = nil)
				: TCustomCheckItem; overload;
			function AddItem(_cb : TCheckBox; _combo : TComboBox; const _caption : string; const _orderIndex : Integer;
				_obj : IInterface = nil) : TCustomCheckItem; overload;
			function AddItem(_cb : TCheckBox; _spin : TSpinEdit; const _caption : string; const _orderIndex : Integer;
				_obj : IInterface = nil) : TCustomCheckItem; overload;
			function AddItem(_lbl : TLabel; _combo : TComboBox; const _caption : string; const _orderIndex : Integer;
				_obj : IInterface = nil) : TCustomCheckItem; overload;
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

		public
			constructor Create(_owner : TComponent); override;
			destructor Destroy; override;
			procedure Clear; override;
			function AddCheckboxItem(const _caption, _hint : string; _orderIndex : Integer; _obj : IInterface = nil)
				: TCustomCheckItem; overload;
			function AddCheckboxComboItem(const _caption, _hint : string; _orderIndex : Integer; _comboItems : TArray<string>;
				_obj : IInterface = nil) : TCustomCheckItem; overload;
			function AddCheckboxSpinItem(const _caption, _hint : string; _orderIndex : Integer;
				_minValue, _maxValue, _defaultValue : Integer; _obj : IInterface = nil) : TCustomCheckItem;
			function AddLabelComboItem(const _caption, _hint : string; _orderIndex : Integer; _comboItems : TArray<string>;
				_obj : IInterface = nil) : TCustomCheckItem;
			// Getter functions for specific items by order index
			function GetItemChecked(orderIndex : Integer) : Boolean;
			function GetItemText(orderIndex : Integer) : string;
			function GetItemSpinValue(orderIndex : Integer) : Integer;
			property EventsEnabled : Boolean read FEventsEnabled write FEventsEnabled;

		published
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
	FOrderIndex := 0; // Default order index
	FCheckBox := nil;
	FLabel := nil;
	FComboBox := nil;
	FSpinEdit := nil;
	FComboBoxItems := TStringList.Create;
	FTagObject := nil;
	FItemType := citCheckBox;
	FMinValue := 0;
	FMaxValue := 100;
	FSpinValue := 0;
end;

destructor TCustomCheckItem.Destroy;
begin
	if Assigned(FSpinEdit) then begin
		FSpinEdit.Free;
		FSpinEdit := nil;
	end;
	if Assigned(FComboBox) then begin
		FComboBox.Free;
		FComboBox := nil;
	end;
	if Assigned(FLabel) then begin
		FLabel.Free;
		FLabel := nil;
	end;
	if Assigned(FCheckBox) then begin
		FCheckBox.Free;
		FCheckBox := nil;
	end;
	FComboBoxItems.Free;
	inherited Destroy;
end;

function TCustomCheckItem.getChecked() : Boolean;
begin
	Result := Assigned(FCheckbox) and FCheckBox.Checked;
end;

function TCustomCheckItem.getComboText() : string;
begin
	Result := '';
	if Assigned(FComboBox) then begin
		Result := FComboBox.Text;
	end;
end;

function TCustomCheckItem.getSpinValue() : Integer;
begin
	Result := FSpinValue;
	if Assigned(FSpinEdit) then begin
		Result := FSpinEdit.Value;
	end;
end;

procedure TCustomCheckItem.setCaption(const _value : string);
begin
	if FCaption <> _value then begin
		FCaption := _value;
		if Assigned(FCheckBox) then begin
			FCheckBox.Caption := _value;
		end;
		if Assigned(FLabel) then begin
			FLabel.Caption := _value;
		end;
	end;
end;

procedure TCustomCheckItem.setChecked(const _value : Boolean);
begin
	if Assigned(FCheckBox) and (FCheckbox.Checked <> _value) then begin
		FCheckBox.Checked := _value;
	end;
end;

procedure TCustomCheckItem.setOrderIndex(const _value : Integer);
begin
	if FOrderIndex <> _value then begin
		FOrderIndex := _value;
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
	if Assigned(FComboBox) then begin
		FComboBox.Text := _value;
	end;
end;

procedure TCustomCheckItem.setSpinValue(const _value : Integer);
begin
	FSpinValue := _value;
	if Assigned(FSpinEdit) then begin
		FSpinEdit.Value := _value;
	end;
end;

procedure TCustomCheckItem.setMinValue(const _value : Integer);
begin
	if FMinValue <> _value then begin
		FMinValue := _value;
		if Assigned(FSpinEdit) then begin
			FSpinEdit.MinValue := _value;
		end;
	end;
end;

procedure TCustomCheckItem.setMaxValue(const _value : Integer);
begin
	if FMaxValue <> _value then begin
		FMaxValue := _value;
		if Assigned(FSpinEdit) then begin
			FSpinEdit.MaxValue := _value;
		end;
	end;
end;

procedure TCustomCheckItem.setComboBoxItems(const _value : TStringList);
begin
	if Assigned(_value) then begin
		FComboBoxItems.Assign(_value);
		if Assigned(FComboBox) then begin
			FComboBox.Items.Assign(_value);
		end;
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

function TCustomCheckItems.AddItem(_cb : TCheckBox; const _caption : string; const _orderIndex : Integer; _obj : IInterface = nil)
	: TCustomCheckItem;
begin
	Result := Add;
	Result.FCaption := _caption;
	Result.FOrderIndex := _orderIndex;
	Result.TagObject := _obj;
	Result.FCheckBox := _cb;
	Result.FItemType := citCheckBox;
end;

function TCustomCheckItems.AddItem(_cb : TCheckBox; _combo : TComboBox; const _caption : string; const _orderIndex : Integer;
	_obj : IInterface = nil) : TCustomCheckItem;
begin
	Result := Add;
	Result.FCaption := _caption;
	Result.FOrderIndex := _orderIndex;
	Result.TagObject := _obj;
	Result.FCheckBox := _cb;
	Result.FComboBox := _combo;
	Result.FItemType := citCheckBoxWithCombo;
end;

function TCustomCheckItems.AddItem(_cb : TCheckBox; _spin : TSpinEdit; const _caption : string; const _orderIndex : Integer;
	_obj : IInterface = nil) : TCustomCheckItem;
begin
	Result := Add;
	Result.FCaption := _caption;
	Result.FOrderIndex := _orderIndex;
	Result.TagObject := _obj;
	Result.FCheckBox := _cb;
	Result.FSpinEdit := _spin;
	Result.FItemType := citCheckBoxWithSpin;
end;

function TCustomCheckItems.AddItem(_lbl : TLabel; _combo : TComboBox; const _caption : string; const _orderIndex : Integer;
	_obj : IInterface = nil) : TCustomCheckItem;
begin
	Result := Add;
	Result.FCaption := _caption;
	Result.FOrderIndex := _orderIndex;
	Result.TagObject := _obj;
	Result.FLabel := _lbl;
	Result.FComboBox := _combo;
	Result.FItemType := citLabelWithCombo;
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
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		if Assigned(item.SpinEdit) then begin
			item.SpinEdit.Free;
			item.SpinEdit := nil;
		end;
		if Assigned(item.ComboBox) then begin
			item.ComboBox.Free;
			item.ComboBox := nil;
		end;
		if Assigned(item.LabelControl) then begin
			item.LabelControl.Free;
			item.LabelControl := nil;
		end;
		if Assigned(item.CheckBox) then begin
			item.CheckBox.Free;
			item.CheckBox := nil;
		end;
	end;
	FItems.Clear;
end;

function TCustomCheckOptions.AddCheckboxItem(const _caption, _hint : string; _orderIndex : Integer; _obj : IInterface = nil)
	: TCustomCheckItem;
var
	checkBox : TCheckBox;
begin
	var
	ar := TAutoSetReset.New(FEventsEnabled, False);

	checkBox := TCheckBox.Create(Self);
	var
	cleanCaption := BuildControlNameFromCaption(_caption);
	checkbox.Name := 'cmb' + UpCase(cleanCaption[1]) + cleanCaption.Substring(1);
	checkBox.Parent := Self;
	checkBox.Caption := _caption;
	checkBox.Hint := _hint;
	checkBox.ShowHint := True;
	checkBox.OnClick := onItemChangeEventHandler;
	Result := FItems.AddItem(checkBox, _caption, _orderIndex, _obj);
end;

function TCustomCheckOptions.AddCheckboxComboItem(const _caption, _hint : string; _orderIndex : Integer; _comboItems : TArray<string>;
	_obj : IInterface = nil) : TCustomCheckItem;
var
	checkBox : TCheckBox;
	comboBox : TComboBox;
begin
	var
	ar := TAutoSetReset.New(FEventsEnabled, False);

	checkBox := TCheckBox.Create(Self);
	checkBox.Parent := Self;
	checkBox.Caption := BuildControlNameFromCaption(_caption);
	checkBox.Hint := _hint;
	checkBox.ShowHint := True;
	checkBox.OnClick := onItemChangeEventHandler;

	comboBox := TComboBox.Create(Self);
	comboBox.Parent := Self;
	comboBox.Style := csDropDown;
	comboBox.AutoDropDownWidth := True;

	var strList : IShared<TStringList> := Shared.Make<TStringList>();
	strList.AddStrings(_comboItems);
	if Assigned(strList) then begin
		comboBox.Items.Assign(strList());
	end;
	// We'll set OnChange in the SearchForm after creation

	Result := FItems.AddItem(checkBox, comboBox, _caption, _orderIndex, _obj);
	Result.ComboBoxItems := strList();
end;

function TCustomCheckOptions.AddCheckboxSpinItem(const _caption, _hint : string; _orderIndex : Integer;
	_minValue, _maxValue, _defaultValue : Integer; _obj : IInterface = nil) : TCustomCheckItem;
var
	checkBox : TCheckBox;
	spinEdit : TSpinEdit;
begin
	var
	ar := TAutoSetReset.New(FEventsEnabled, False);

	checkBox := TCheckBox.Create(Self);
	var
	cleanCaption := BuildControlNameFromCaption(_caption);
	checkBox.Name := 'cb' + UpCase(cleanCaption[1]) + cleanCaption.Substring(1);
	checkBox.Parent := Self;
	checkBox.Caption := _caption;
	checkBox.Hint := _hint;
	checkBox.ShowHint := True;
	checkBox.OnClick := onItemChangeEventHandler;

	spinEdit := TSpinEdit.Create(Self);
	spinEdit.Name := 'sp' + UpCase(cleanCaption[1]) + cleanCaption.Substring(1);
	spinEdit.Parent := Self;
	spinEdit.MinValue := _minValue;
	spinEdit.MaxValue := _maxValue;
	spinEdit.Value := _defaultValue;
    spinEdit.OnChange := onItemChangeEventHandler;

	Result := FItems.AddItem(checkBox, spinEdit, _caption, _orderIndex, _obj);
	Result.MinValue := _minValue;
	Result.MaxValue := _maxValue;
	Result.SpinValue := _defaultValue;
end;

function TCustomCheckOptions.AddLabelComboItem(const _caption, _hint : string; _orderIndex : Integer; _comboItems : TArray<string>;
	_obj : IInterface = nil) : TCustomCheckItem;
var
	labelControl : TLabel;
	comboBox : TComboBox;
	comboItems : IShared<TStringList>;
begin
	var
	ar := TAutoSetReset.New(FEventsEnabled, False);

	labelControl := TLabel.Create(Self);
	var
	cleanCaption := BuildControlNameFromCaption(_caption);
	labelControl.Name := 'lbl' + UpCase(cleanCaption[1]) + cleanCaption.Substring(1);
	labelControl.Parent := Self;
	labelControl.Caption := _caption;
	labelControl.Hint := _hint;
	labelControl.ShowHint := True;

	comboBox := TComboBox.Create(Self);
	comboBox.Name := 'cmb' + UpCase(cleanCaption[1]) + cleanCaption.Substring(1);
	comboBox.Parent := Self;
	comboBox.Style := csDropDown;
	comboBox.AutoDropDownWidth := True;
	comboBox.OnChange := onItemChangeEventHandler;

	comboItems := Shared.Make<TStringList>();
	comboItems.AddStrings(_comboItems);
	if Assigned(_comboItems) then begin
		comboBox.Items.Assign(comboItems);
	end;

	comboBox.ItemIndex := 0;
	Result := FItems.AddItem(labelControl, comboBox, _caption, _orderIndex, _obj);
	Result.ComboBoxItems := comboItems;
end;

procedure TCustomCheckOptions.AlignControlItems;
const
	SPACE = 8;
	FIRST_CONTROL_WIDTH = 80; // Fixed width for first control (checkbox/label)
	SECOND_CONTROL_WIDTH = 80; // Fixed width for second control (combo/spin)
var
	i, col, row : Integer;
	itemHeight, itemWidth : Integer;
	maxRows : Integer;
	item : TCustomCheckItem;
	baseLeft : Integer;
begin
	if FItems.Count = 0 then begin
		Exit;
	end;

	// Calculate layout
	itemHeight := 22; // Standard height
	itemWidth := Width div Columns;
	maxRows := Ceil(FItems.Count / Columns);

	// Position controls based on item type
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		col := i mod Columns;
		row := i div Columns;
		baseLeft := col * itemWidth + SPACE;

		case item.ItemType of
			citCheckBox : begin
				// Single checkbox with full width
				if Assigned(item.CheckBox) then begin
					item.CheckBox.Left := baseLeft;
					item.CheckBox.Top := row * itemHeight + SPACE;
					item.CheckBox.Width := itemWidth - (2 * SPACE);
					item.CheckBox.Height := itemHeight - 2;
					item.CheckBox.Tag := i;
				end;
			end;

			citCheckBoxWithCombo : begin
				// Checkbox + ComboBox - both left aligned
				if Assigned(item.CheckBox) then begin
					item.CheckBox.Left := baseLeft;
					item.CheckBox.Top := row * itemHeight + SPACE;
					item.CheckBox.Width := FIRST_CONTROL_WIDTH;
					item.CheckBox.Height := itemHeight - 2;
					item.CheckBox.Tag := i;
				end;
				if Assigned(item.ComboBox) then begin
					item.ComboBox.Left := baseLeft + FIRST_CONTROL_WIDTH + SPACE;
					item.ComboBox.Top := row * itemHeight + SPACE;
					item.ComboBox.Width := SECOND_CONTROL_WIDTH;
					item.ComboBox.Height := itemHeight - 2;
					item.ComboBox.Tag := i;
				end;
			end;

			citCheckBoxWithSpin : begin
				// Checkbox + SpinEdit - both left aligned
				if Assigned(item.CheckBox) then begin
					item.CheckBox.Left := baseLeft;
					item.CheckBox.Top := row * itemHeight + SPACE;
					item.CheckBox.Width := FIRST_CONTROL_WIDTH;
					item.CheckBox.Height := itemHeight - 2;
					item.CheckBox.Tag := i;
				end;
				if Assigned(item.SpinEdit) then begin
					item.SpinEdit.Left := baseLeft + FIRST_CONTROL_WIDTH + SPACE;
					item.SpinEdit.Top := row * itemHeight + SPACE;
					item.SpinEdit.Width := SECOND_CONTROL_WIDTH;
					item.SpinEdit.Height := itemHeight - 2;
					item.SpinEdit.Tag := i;
				end;
			end;

			citLabelWithCombo : begin
				// Label + ComboBox - both left aligned
				if Assigned(item.LabelControl) then begin
					item.LabelControl.Left := baseLeft;
					item.LabelControl.Top := row * itemHeight + SPACE + 3; // Slight vertical offset for better alignment
					item.LabelControl.Width := FIRST_CONTROL_WIDTH;
					item.LabelControl.Height := itemHeight - 2;
				end;
				if Assigned(item.ComboBox) then begin
					item.ComboBox.Left := baseLeft + FIRST_CONTROL_WIDTH + SPACE;
					item.ComboBox.Top := row * itemHeight + SPACE;
					item.ComboBox.Width := SECOND_CONTROL_WIDTH;
					item.ComboBox.Height := itemHeight - 2;
					item.ComboBox.Tag := i;
				end;
			end;
		end;
	end;

	// Adjust control height if needed
	if maxRows > 0 then begin
		Height := maxRows * itemHeight + 16; // Padding for clean layout
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

function TCustomCheckOptions.GetItemChecked(orderIndex : Integer) : Boolean;
var
	i : Integer;
	item : TCustomCheckItem;
begin
	Result := False;
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		if item.OrderIndex = orderIndex then begin
			Result := item.Checked;
			Exit;
		end;
	end;
end;

function TCustomCheckOptions.GetItemText(orderIndex : Integer) : string;
var
	i : Integer;
	item : TCustomCheckItem;
begin
	Result := '';
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		if item.OrderIndex = orderIndex then begin
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
	end;
end;

function TCustomCheckOptions.GetItemSpinValue(orderIndex : Integer) : Integer;
var
	i : Integer;
	item : TCustomCheckItem;
begin
	Result := 0;
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		if (item.OrderIndex = orderIndex) and (item.ItemType = citCheckBoxWithSpin) and Assigned(item.SpinEdit) then begin
			Result := item.SpinEdit.Value;
			Exit;
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

end.
