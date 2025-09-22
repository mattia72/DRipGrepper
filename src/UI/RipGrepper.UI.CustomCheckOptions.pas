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
	Winapi.Windows;

type
	// Forward declarations
	TCustomCheckOptions = class;

	// Custom collection item for checkbox items
	TCustomCheckItem = class(TCollectionItem)
		private
			FCaption : string;
			FOrderIndex : Integer;
			FTagObject : IInterface;
			FCheckBox : TCheckBox;
			FComboBox : TComboBox;
			FComboBoxItems : TStringList;
			FChecked : Boolean;
			FHasComboBox : Boolean;
			procedure setCaption(const _value : string);
			procedure setOrderIndex(const _value : Integer);
			procedure SetTagObject(const Value : IInterface);
			procedure setChecked(const _value : Boolean);
			procedure setHasComboBox(const _value : Boolean);
			procedure setComboBoxItems(const _value : TStringList);

		public
			constructor Create(Collection : TCollection); override;
			destructor Destroy; override;

			property CheckBox : TCheckBox read FCheckBox write FCheckBox;
			property ComboBox : TComboBox read FComboBox write FComboBox;
			property ComboBoxItems : TStringList read FComboBoxItems write setComboBoxItems;
			property Caption : string read FCaption write setCaption;
			property OrderIndex : Integer read FOrderIndex write setOrderIndex;
			property TagObject : IInterface read FTagObject write SetTagObject;
			property Checked : Boolean read FChecked write setChecked;
			property HasComboBox : Boolean read FHasComboBox write setHasComboBox;
	end;

	// Collection for checkbox items
	TCustomCheckItems = class(TCollection)
		private
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
			function AddItem(_cb : TCheckBox; _combo : TComboBox; const _caption : string; const _orderIndex : Integer; _obj : IInterface = nil)
				: TCustomCheckItem; overload;
			property Items[index : Integer] : TCustomCheckItem read getItem write setItem; default;
	end;

	// Event type for item selection
	TCheckItemSelectEvent = procedure(Sender : TObject; Item : TCustomCheckItem) of object;

	// Base class for custom option controls
	TCustomOptionsBase = class(TCustomPanel)
		private
			FColumns : Integer;
			procedure setColumns(const _value : Integer);

		protected
			procedure Resize; override;
			procedure ArrangeItems; virtual; abstract;

		public
			constructor Create(_owner : TComponent); override;
			procedure Clear; virtual; abstract;

		published
			property Columns : Integer read FColumns write setColumns default 1;
	end;

	// Custom checkbox options control
	TCustomCheckOptions = class(TCustomOptionsBase)
		private
			FItems : TCustomCheckItems;
			FOnItemSelect : TCheckItemSelectEvent;
			procedure onCheckBoxClick(_sender : TObject);
			function getSelectedItems : TArray<TCustomCheckItem>;

		protected
			procedure ArrangeItems; override;

		public
			constructor Create(_owner : TComponent); override;
			destructor Destroy; override;
			procedure Clear; override;
			function AddItem(const _caption, _hint : string; _orderIndex : Integer; _obj : IInterface = nil) : TCustomCheckItem; overload;
			function AddItem(const _caption, _hint : string; _orderIndex : Integer; _comboItems : TStringList; _obj : IInterface = nil) : TCustomCheckItem; overload;

		published
			property SelectedItems : TArray<TCustomCheckItem> read getSelectedItems;
			property Items : TCustomCheckItems read FItems write FItems;
			property OnItemSelect : TCheckItemSelectEvent read FOnItemSelect write FOnItemSelect;
	end;

procedure Register;

implementation

uses
	Math,
	RipGrepper.Common.IDEContextValues,
	RipGrepper.Common.SimpleTypes;

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

procedure TCustomOptionsBase.setColumns(const _value : Integer);
begin
	if (_value > 0) and (FColumns <> _value) then begin
		FColumns := _value;
		ArrangeItems;
	end;
end;

procedure TCustomOptionsBase.Resize;
begin
	inherited Resize;
	ArrangeItems;
end;

{ TCustomCheckItem }

constructor TCustomCheckItem.Create(Collection : TCollection);
begin
	inherited Create(Collection);
	FOrderIndex := 0; // Default order index
	FCheckBox := nil;
	FComboBox := nil;
	FComboBoxItems := TStringList.Create;
	FTagObject := nil;
	FChecked := False;
	FHasComboBox := False;
end;

destructor TCustomCheckItem.Destroy;
begin
	if Assigned(FComboBox) then begin
		FComboBox.Free;
		FComboBox := nil;
	end;
	if Assigned(FCheckBox) then begin
		FCheckBox.Free;
		FCheckBox := nil;
	end;
	FComboBoxItems.Free;
	inherited Destroy;
end;

procedure TCustomCheckItem.setCaption(const _value : string);
begin
	if FCaption <> _value then begin
		FCaption := _value;
		if Assigned(FCheckBox) then begin
			FCheckBox.Caption := _value;
		end;
	end;
end;

procedure TCustomCheckItem.setChecked(const _value : Boolean);
begin
	if FChecked <> _value then begin
		FChecked := _value;
		if Assigned(FCheckBox) then begin
			FCheckBox.Checked := _value;
		end;
	end;
end;

procedure TCustomCheckItem.setOrderIndex(const _value : Integer);
begin
	if FOrderIndex <> _value then begin
		FOrderIndex := _value;
	end;
end;

procedure TCustomCheckItem.SetTagObject(const Value : IInterface);
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

procedure TCustomCheckItem.setHasComboBox(const _value : Boolean);
begin
	if FHasComboBox <> _value then begin
		FHasComboBox := _value;
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
	Result.FHasComboBox := False;
end;

function TCustomCheckItems.AddItem(_cb : TCheckBox; _combo : TComboBox; const _caption : string; const _orderIndex : Integer; _obj : IInterface = nil)
	: TCustomCheckItem;
begin
	Result := Add;
	Result.FCaption := _caption;
	Result.FOrderIndex := _orderIndex;
	Result.TagObject := _obj;
	Result.FCheckBox := _cb;
	Result.FComboBox := _combo;
	Result.FHasComboBox := True;
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
	// Free all checkboxes and comboboxes
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		if Assigned(item.ComboBox) then begin
			item.ComboBox.Free;
			item.ComboBox := nil;
		end;
		if Assigned(item.CheckBox) then begin
			item.CheckBox.Free;
			item.CheckBox := nil;
		end;
	end;
	FItems.Clear;
end;

function TCustomCheckOptions.AddItem(const _caption, _hint : string; _orderIndex : Integer; _obj : IInterface = nil) : TCustomCheckItem;
var
	checkBox : TCheckBox;
begin
	checkBox := TCheckBox.Create(Self);
	checkBox.Parent := Self;
	checkBox.Caption := _caption;
	checkBox.OnClick := onCheckBoxClick;
	checkBox.Hint := _hint;
	checkBox.ShowHint := True;
	Result := FItems.AddItem(checkBox, _caption, _orderIndex, _obj);
end;

function TCustomCheckOptions.AddItem(const _caption, _hint : string; _orderIndex : Integer; _comboItems : TStringList; _obj : IInterface = nil) : TCustomCheckItem;
var
	checkBox : TCheckBox;
	comboBox : TComboBox;
begin
	checkBox := TCheckBox.Create(Self);
	checkBox.Parent := Self;
	checkBox.Caption := _caption;
	checkBox.OnClick := onCheckBoxClick;
	checkBox.Hint := _hint;
	checkBox.ShowHint := True;

	comboBox := TComboBox.Create(Self);
	comboBox.Parent := Self;
	comboBox.Style := csDropDownList;
	if Assigned(_comboItems) then begin
		comboBox.Items.Assign(_comboItems);
	end;
	
	Result := FItems.AddItem(checkBox, comboBox, _caption, _orderIndex, _obj);
	Result.ComboBoxItems := _comboItems;
end;

procedure TCustomCheckOptions.ArrangeItems;
var
	i, col, row : Integer;
	itemHeight, itemWidth, checkBoxWidth, comboBoxWidth : Integer;
	maxRows : Integer;
	item : TCustomCheckItem;
begin
	if FItems.Count = 0 then begin
		Exit;
	end;

	// Calculate layout
	itemHeight := 22; // Standard height
	itemWidth := Width div FColumns;
	checkBoxWidth := itemWidth div 2; // Half width for checkbox when combo is present
	comboBoxWidth := itemWidth - checkBoxWidth - 16; // Remaining width for combo
	maxRows := Ceil(FItems.Count / FColumns);

	// Position checkboxes and comboboxes
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		if Assigned(item.CheckBox) then begin
			col := i mod FColumns;
			row := i div FColumns;

			if item.HasComboBox then begin
				// Position checkbox with reduced width
				item.CheckBox.Left := col * itemWidth + 8;
				item.CheckBox.Top := row * itemHeight + 8;
				item.CheckBox.Width := checkBoxWidth - 8;
				item.CheckBox.Height := itemHeight - 2;
				item.CheckBox.Tag := i;

				// Position combobox next to checkbox
				if Assigned(item.ComboBox) then begin
					item.ComboBox.Left := col * itemWidth + checkBoxWidth + 8;
					item.ComboBox.Top := row * itemHeight + 8;
					item.ComboBox.Width := comboBoxWidth;
					item.ComboBox.Height := itemHeight - 2;
					item.ComboBox.Tag := i;
				end;
			end else begin
				// Position checkbox with full width
				item.CheckBox.Left := col * itemWidth + 8;
				item.CheckBox.Top := row * itemHeight + 8;
				item.CheckBox.Width := itemWidth - 16;
				item.CheckBox.Height := itemHeight - 2;
				item.CheckBox.Tag := i;
			end;
		end;
	end;

	// Adjust control height if needed
	if maxRows > 0 then begin
		Height := maxRows * itemHeight + 16; // Padding for clean layout
	end;
end;

procedure TCustomCheckOptions.onCheckBoxClick(_sender : TObject);
var
	checkBox : TCheckBox;
	itemIndex : Integer;
	item : TCustomCheckItem;
begin
	checkBox := _sender as TCheckBox;
	itemIndex := checkBox.Tag;

	// Update the checked state in the item
	if (itemIndex >= 0) and (itemIndex < FItems.Count) then begin
		item := FItems[itemIndex];
		item.Checked := checkBox.Checked;

		// Fire event
		if Assigned(FOnItemSelect) then begin
			FOnItemSelect(Self, item);
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

procedure Register;
begin
	RegisterComponents('Custom', [TCustomCheckOptions]);
end;

end.