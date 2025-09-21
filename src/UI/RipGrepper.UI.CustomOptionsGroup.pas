unit RipGrepper.UI.CustomOptionsGroup;

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
	// Enumeration for the control type
	TCustomOptionsGroupType = (cogtRadioButtons, cogtCheckBoxes);

	// Custom collection item for radio items
	TCustomRadioItem = class(TCollectionItem)
		private
			FCaption : string;
			FOrderIndex : Integer;
			FTagObject : IInterface;
			FRadioButton : TRadioButton;
			procedure setCaption(const _value : string);
			procedure setOrderIndex(const _value : Integer);
			procedure SetTagObject(const Value : IInterface);

		public
			constructor Create(Collection : TCollection); override;
			destructor Destroy; override;

			property RadioButton : TRadioButton read FRadioButton write FRadioButton;
			property Caption : string read FCaption write setCaption;
			property OrderIndex : Integer read FOrderIndex write setOrderIndex;
			property TagObject : IInterface read FTagObject write SetTagObject;
	end;

	// Custom collection item for checkbox items
	TCustomCheckItem = class(TCollectionItem)
		private
			FCaption : string;
			FOrderIndex : Integer;
			FTagObject : IInterface;
			FCheckBox : TCheckBox;
			FChecked : Boolean;
			procedure setCaption(const _value : string);
			procedure setOrderIndex(const _value : Integer);
			procedure SetTagObject(const Value : IInterface);
			procedure setChecked(const _value : Boolean);

		public
			constructor Create(Collection : TCollection); override;
			destructor Destroy; override;

			property CheckBox : TCheckBox read FCheckBox write FCheckBox;
			property Caption : string read FCaption write setCaption;
			property OrderIndex : Integer read FOrderIndex write setOrderIndex;
			property TagObject : IInterface read FTagObject write SetTagObject;
			property Checked : Boolean read FChecked write setChecked;
	end;

	// Collection for radio items
	TCustomRadioItems = class(TCollection)
		private
			FOwner : TControl;
			function getItem(_index : Integer) : TCustomRadioItem;
			procedure setItem(_index : Integer; const _value : TCustomRadioItem);

		protected
			function GetOwner : TPersistent; override;

		public
			constructor Create(_owner : TControl);
			function Add : TCustomRadioItem;
			function AddItem(_rb : TRadioButton; const _caption : string; const _orderIndex : Integer; _obj : IInterface = nil)
				: TCustomRadioItem;
			property Items[index : Integer] : TCustomRadioItem read getItem write setItem; default;
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
				: TCustomCheckItem;
			property Items[index : Integer] : TCustomCheckItem read getItem write setItem; default;
	end;

	// Forward declarations
	TCustomOptionsBase = class;
	TCustomRadioOptions = class;
	TCustomCheckOptions = class;

	// Event type for item selection
	TRadioItemSelectEvent = procedure(Sender : TObject; Item : TCustomRadioItem) of object;
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

	// Custom radio options control
	TCustomRadioOptions = class(TCustomOptionsBase)
		private
			FItems : TCustomRadioItems;
			FItemIndex : Integer;
			FOnItemSelect : TRadioItemSelectEvent;
			procedure onRadioButtonClick(_sender : TObject);
			procedure setItemIndex(const _value : Integer);
			function getSelectedItem : TCustomRadioItem;

		protected
			procedure ArrangeItems; override;

		public
			constructor Create(_owner : TComponent); override;
			destructor Destroy; override;
			procedure Clear; override;
			function AddItem(const _caption, _hint : string; _orderIndex : Integer; _obj : IInterface = nil) : TCustomRadioItem;

		published
			property SelectedItem : TCustomRadioItem read getSelectedItem;
			property Items : TCustomRadioItems read FItems write FItems;
			property ItemIndex : Integer read FItemIndex write setItemIndex default -1;
			property OnItemSelect : TRadioItemSelectEvent read FOnItemSelect write FOnItemSelect;
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
			function AddItem(const _caption, _hint : string; _orderIndex : Integer; _obj : IInterface = nil) : TCustomCheckItem;

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
	FTagObject := nil;
	FChecked := False;
end;

destructor TCustomCheckItem.Destroy;
begin
	if Assigned(FCheckBox) then begin
		FCheckBox.Free;
		FCheckBox := nil;
	end;
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

{ TCustomRadioItem }

constructor TCustomRadioItem.Create(Collection : TCollection);
begin
	inherited Create(Collection);
	FOrderIndex := 0; // Default order index
	FRadioButton := nil;
	FTagObject := nil;
end;

destructor TCustomRadioItem.Destroy;
begin
	if Assigned(FRadioButton) then begin
		FRadioButton.Free;
		FRadioButton := nil;
	end;
	inherited Destroy;
end;

procedure TCustomRadioItem.setCaption(const _value : string);
begin
	if FCaption <> _value then begin
		FCaption := _value;
		if Assigned(FRadioButton) then begin
			FRadioButton.Caption := _value;
		end;
	end;
end;

procedure TCustomRadioItem.setOrderIndex(const _value : Integer);
begin
	if FOrderIndex <> _value then begin
		FOrderIndex := _value;
	end;
end;

procedure TCustomRadioItem.SetTagObject(const Value : IInterface);
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

{ TCustomRadioItems }

constructor TCustomRadioItems.Create(_owner : TControl);
begin
	inherited Create(TCustomRadioItem);
	FOwner := _owner;
end;

function TCustomRadioItems.Add : TCustomRadioItem;
begin
	Result := TCustomRadioItem(inherited Add);
end;

function TCustomRadioItems.AddItem(_rb : TRadioButton; const _caption : string; const _orderIndex : Integer; _obj : IInterface = nil)
	: TCustomRadioItem;
begin
	Result := Add;
	Result.FCaption := _caption;
	Result.FOrderIndex := _orderIndex;
	Result.TagObject := _obj;
	Result.FRadioButton := _rb;
end;

function TCustomRadioItems.getItem(_index : Integer) : TCustomRadioItem;
begin
	Result := TCustomRadioItem(inherited GetItem(_index));
end;

function TCustomRadioItems.GetOwner : TPersistent;
begin
	Result := FOwner;
end;

procedure TCustomRadioItems.setItem(_index : Integer; const _value : TCustomRadioItem);
begin
	inherited SetItem(_index, _value);
end;

{ TCustomRadioOptions }

constructor TCustomRadioOptions.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	FItems := TCustomRadioItems.Create(Self);
	FItemIndex := -1;
end;

destructor TCustomRadioOptions.Destroy;
begin
	Clear;
	FItems.Free;
	inherited Destroy;
end;

procedure TCustomRadioOptions.Clear;
var
	i : Integer;
	item : TCustomRadioItem;
begin
	// Free all radio buttons
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		if Assigned(item.RadioButton) then begin
			item.RadioButton.Free;
			item.RadioButton := nil;
		end;
	end;
	FItems.Clear;
	FItemIndex := -1;
end;

function TCustomRadioOptions.AddItem(const _caption, _hint : string; _orderIndex : Integer; _obj : IInterface = nil) : TCustomRadioItem;
var
	radioButton : TRadioButton;
begin
	radioButton := TRadioButton.Create(Self);
	radioButton.Parent := Self;
	radioButton.Caption := _caption;
	radioButton.OnClick := onRadioButtonClick;
	radioButton.Hint := _hint;
	radioButton.ShowHint := True;
	Result := FItems.AddItem(radioButton, _caption, _orderIndex, _obj);
end;

procedure TCustomRadioOptions.ArrangeItems;
var
	i, col, row : Integer;
	itemHeight, itemWidth : Integer;
	maxRows : Integer;
	item : TCustomRadioItem;
begin
	if FItems.Count = 0 then begin
		Exit;
	end;

	// Calculate layout
	itemHeight := 22; // Standard height
	itemWidth := Width div FColumns;
	maxRows := Ceil(FItems.Count / FColumns);

	// Position radio buttons
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		if Assigned(item.RadioButton) then begin
			col := i mod FColumns;
			row := i div FColumns;

			item.RadioButton.Left := col * itemWidth + 8;
			item.RadioButton.Top := row * itemHeight + 8;
			item.RadioButton.Width := itemWidth - 16;
			item.RadioButton.Height := itemHeight - 2;
			item.RadioButton.Tag := i;
		end;
	end;

	// Adjust control height if needed
	if maxRows > 0 then begin
		Height := maxRows * itemHeight + 16; // Padding for clean layout
	end;
end;

procedure TCustomRadioOptions.onRadioButtonClick(_sender : TObject);
var
	radioButton : TRadioButton;
	itemIndex : Integer;
	i : Integer;
	item : TCustomRadioItem;
begin
	radioButton := _sender as TRadioButton;
	itemIndex := radioButton.Tag;

	// Uncheck all other radio buttons
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		if Assigned(item.RadioButton) and (item.RadioButton <> radioButton) then begin
			item.RadioButton.Checked := False;
		end;
	end;

	// Set the selected item
	FItemIndex := itemIndex;

	// Fire event
	if Assigned(FOnItemSelect) and (itemIndex >= 0) and (itemIndex < FItems.Count) then begin
		FOnItemSelect(Self, FItems[itemIndex]);
	end;
end;

procedure TCustomRadioOptions.setItemIndex(const _value : Integer);
var
	i : Integer;
	item : TCustomRadioItem;
	targetRadioButton : TRadioButton;
begin
	if (FItemIndex <> _value) and (_value >= -1) and (_value < FItems.Count) then begin
		FItemIndex := _value;

		// Find the radio button that corresponds to the selected item
		targetRadioButton := nil;
		if _value >= 0 then begin
			item := FItems[_value];
			targetRadioButton := item.RadioButton;
		end;

		// Update radio button states
		for i := 0 to FItems.Count - 1 do begin
			item := FItems[i];
			if Assigned(item.RadioButton) then begin
				item.RadioButton.Checked := (item.RadioButton = targetRadioButton);
			end;
		end;

		// Fire event
		if Assigned(FOnItemSelect) and (_value >= 0) then begin
			FOnItemSelect(Self, FItems[_value]);
		end;
	end;
end;

function TCustomRadioOptions.getSelectedItem : TCustomRadioItem;
begin
	if (FItemIndex >= 0) and (FItemIndex < FItems.Count) then begin
		Result := FItems[FItemIndex];
	end else begin
		Result := nil;
	end;
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
	// Free all checkboxes
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
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

procedure TCustomCheckOptions.ArrangeItems;
var
	i, col, row : Integer;
	itemHeight, itemWidth : Integer;
	maxRows : Integer;
	item : TCustomCheckItem;
begin
	if FItems.Count = 0 then begin
		Exit;
	end;

	// Calculate layout
	itemHeight := 22; // Standard height
	itemWidth := Width div FColumns;
	maxRows := Ceil(FItems.Count / FColumns);

	// Position checkboxes
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		if Assigned(item.CheckBox) then begin
			col := i mod FColumns;
			row := i div FColumns;

			item.CheckBox.Left := col * itemWidth + 8;
			item.CheckBox.Top := row * itemHeight + 8;
			item.CheckBox.Width := itemWidth - 16;
			item.CheckBox.Height := itemHeight - 2;
			item.CheckBox.Tag := i;
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
	RegisterComponents('Custom', [TCustomRadioOptions, TCustomCheckOptions]);
end;

end.
