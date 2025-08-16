unit RipGrepper.UI.CustomRadioGroup;

interface

uses
	System.Classes,
	System.Generics.Collections,
	System.Generics.Defaults,
	System.SysUtils,
	Vcl.Controls,
	Vcl.StdCtrls,
	Vcl.ExtCtrls;

type
	// Record to hold radio button item data
	TRadioItem = record
		Caption: string;
		OrderIndex: Integer;
		Obj: TObject;
		class function New(const _caption: string; _orderIndex: Integer; _obj: TObject = nil): TRadioItem; static;
	end;

	// Custom collection item for radio items
	TCustomRadioItem = class(TCollectionItem)
		private
			FCaption: string;
			FOrderIndex: Integer;
			FObj: TObject;
			FRadioButton: TRadioButton;
			procedure setCaption(const _value: string);
			procedure setOrderIndex(const _value: Integer);
		public
			constructor Create(Collection: TCollection); override;
			destructor Destroy; override;
			property RadioButton: TRadioButton read FRadioButton write FRadioButton;
		published
			property Caption: string read FCaption write setCaption;
			property OrderIndex: Integer read FOrderIndex write setOrderIndex;
			property Obj: TObject read FObj write FObj;
	end;

	// Collection for radio items
	TCustomRadioItems = class(TCollection)
		private
			FOwner: TControl;
			function getItem(_index: Integer): TCustomRadioItem;
			procedure setItem(_index: Integer; const _value: TCustomRadioItem);
		protected
			function GetOwner: TPersistent; override;
		public
			constructor Create(_owner: TControl);
			function Add: TCustomRadioItem;
			function AddItem(const _caption: string; _orderIndex: Integer; _obj: TObject = nil): TCustomRadioItem;
			property Items[Index: Integer]: TCustomRadioItem read getItem write setItem; default;
	end;

	// Forward declaration
	TCustomRadioGroup = class;

	// Event type for item selection
	TRadioItemSelectEvent = procedure(Sender: TObject; Item: TCustomRadioItem) of object;

	// Main custom radio group control
	TCustomRadioGroup = class(TCustomPanel)
		private
			FItems: TCustomRadioItems;
			FItemIndex: Integer;
			FColumns: Integer;
			FOnItemSelect: TRadioItemSelectEvent;
			FRadioButtons: TList<TRadioButton>;
			procedure createRadioButtons;
			procedure arrangeRadioButtons;
			procedure onRadioButtonClick(_sender: TObject);
			procedure setColumns(const _value: Integer);
			procedure setItemIndex(const _value: Integer);
			function getSelectedItem: TCustomRadioItem;
		protected
			procedure Resize; override;
		public
			constructor Create(_owner: TComponent); override;
			destructor Destroy; override;
			procedure Clear;
			function AddItem(const _caption: string; _orderIndex: Integer; _obj: TObject = nil): TCustomRadioItem;
			property SelectedItem: TCustomRadioItem read getSelectedItem;
		published
			property Items: TCustomRadioItems read FItems write FItems;
			property ItemIndex: Integer read FItemIndex write setItemIndex default -1;
			property Columns: Integer read FColumns write setColumns default 1;
			property OnItemSelect: TRadioItemSelectEvent read FOnItemSelect write FOnItemSelect;
	end;

procedure Register;

implementation

uses
	Math;

{ TRadioItem }

class function TRadioItem.New(const _caption: string; _orderIndex: Integer; _obj: TObject): TRadioItem;
begin
	Result.Caption := _caption;
	Result.OrderIndex := _orderIndex;
	Result.Obj := _obj;
end;

{ TCustomRadioItem }

constructor TCustomRadioItem.Create(Collection: TCollection);
begin
	inherited Create(Collection);
	FOrderIndex := Collection.Count - 1;
	FRadioButton := nil;
end;

destructor TCustomRadioItem.Destroy;
begin
	if Assigned(FRadioButton) then begin
		FRadioButton.Free;
	end;
	inherited Destroy;
end;

procedure TCustomRadioItem.setCaption(const _value: string);
begin
	if FCaption <> _value then begin
		FCaption := _value;
		if Assigned(FRadioButton) then begin
			FRadioButton.Caption := _value;
		end;
		if Assigned(Collection) and Assigned(TCustomRadioItems(Collection).FOwner) then begin
			TCustomRadioGroup(TCustomRadioItems(Collection).FOwner).createRadioButtons;
		end;
	end;
end;

procedure TCustomRadioItem.setOrderIndex(const _value: Integer);
begin
	if FOrderIndex <> _value then begin
		FOrderIndex := _value;
		if Assigned(Collection) and Assigned(TCustomRadioItems(Collection).FOwner) then begin
			TCustomRadioGroup(TCustomRadioItems(Collection).FOwner).arrangeRadioButtons;
		end;
	end;
end;

{ TCustomRadioItems }

constructor TCustomRadioItems.Create(_owner: TControl);
begin
	inherited Create(TCustomRadioItem);
	FOwner := _owner;
end;

function TCustomRadioItems.Add: TCustomRadioItem;
begin
	Result := TCustomRadioItem(inherited Add);
end;

function TCustomRadioItems.AddItem(const _caption: string; _orderIndex: Integer; _obj: TObject): TCustomRadioItem;
begin
	Result := Add;
	Result.Caption := _caption;
	Result.OrderIndex := _orderIndex;
	Result.Obj := _obj;
end;

function TCustomRadioItems.getItem(_index: Integer): TCustomRadioItem;
begin
	Result := TCustomRadioItem(inherited GetItem(_index));
end;

function TCustomRadioItems.GetOwner: TPersistent;
begin
	Result := FOwner;
end;

procedure TCustomRadioItems.setItem(_index: Integer; const _value: TCustomRadioItem);
begin
	inherited SetItem(_index, _value);
end;

{ TCustomRadioGroup }

constructor TCustomRadioGroup.Create(_owner: TComponent);
begin
	inherited Create(_owner);
	FItems := TCustomRadioItems.Create(Self);
	FRadioButtons := TList<TRadioButton>.Create;
	FItemIndex := -1;
	FColumns := 1;
	Caption := '';
	BevelOuter := bvNone;
	Width := 200;
	Height := 100;
end;

destructor TCustomRadioGroup.Destroy;
begin
	Clear;
	FRadioButtons.Free;
	FItems.Free;
	inherited Destroy;
end;

procedure TCustomRadioGroup.Clear;
var
	radioButton: TRadioButton;
begin
	for radioButton in FRadioButtons do begin
		radioButton.Free;
	end;
	FRadioButtons.Clear;
	FItems.Clear;
	FItemIndex := -1;
end;

function TCustomRadioGroup.AddItem(const _caption: string; _orderIndex: Integer; _obj: TObject): TCustomRadioItem;
begin
	Result := FItems.AddItem(_caption, _orderIndex, _obj);
	createRadioButtons;
end;

procedure TCustomRadioGroup.createRadioButtons;
var
	i: Integer;
	radioButton: TRadioButton;
	item: TCustomRadioItem;
begin
	// Clear existing radio buttons
	for radioButton in FRadioButtons do begin
		radioButton.Free;
	end;
	FRadioButtons.Clear;

	// Create new radio buttons for each item
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		radioButton := TRadioButton.Create(Self);
		radioButton.Parent := Self;
		radioButton.Caption := item.Caption;
		radioButton.OnClick := onRadioButtonClick;
		radioButton.Tag := i; // Store item index in Tag
		item.RadioButton := radioButton;
		FRadioButtons.Add(radioButton);
	end;

	arrangeRadioButtons;
end;

procedure TCustomRadioGroup.arrangeRadioButtons;
var
	sortedItems: TList<TCustomRadioItem>;
	i, col, row: Integer;
	itemHeight, itemWidth: Integer;
	maxRows: Integer;
	item: TCustomRadioItem;
begin
	if FRadioButtons.Count = 0 then begin
		Exit;
	end;

	// Create a sorted list based on OrderIndex
	sortedItems := TList<TCustomRadioItem>.Create;
	try
		// Add all items to the list
		for i := 0 to FItems.Count - 1 do begin
			sortedItems.Add(FItems[i]);
		end;

		// Sort by OrderIndex
		sortedItems.Sort(
			TComparer<TCustomRadioItem>.Construct(
				function(const Left, Right: TCustomRadioItem): Integer
				begin
					Result := Integer(Left.OrderIndex) - Integer(Right.OrderIndex);
				end));

		// Calculate layout
		itemHeight := 20;
		itemWidth := Width div FColumns;
		maxRows := Ceil(sortedItems.Count / FColumns);

		// Position radio buttons according to sorted order
		for i := 0 to sortedItems.Count - 1 do begin
			item := sortedItems[i];
			if Assigned(item.RadioButton) then begin
				col := i mod FColumns;
				row := i div FColumns;

				item.RadioButton.Left := col * itemWidth + 8;
				item.RadioButton.Top := row * itemHeight + 8;
				item.RadioButton.Width := itemWidth - 16;
				item.RadioButton.Height := itemHeight - 2;
			end;
		end;

		// Adjust control height if needed
		if maxRows > 0 then begin
			Height := Max(Height, maxRows * itemHeight + 16);
		end;

	finally
		sortedItems.Free;
	end;
end;

procedure TCustomRadioGroup.onRadioButtonClick(_sender: TObject);
var
	radioButton: TRadioButton;
	itemIndex: Integer;
	i: Integer;
begin
	radioButton := _sender as TRadioButton;
	itemIndex := radioButton.Tag;

	// Uncheck all other radio buttons
	for i := 0 to FRadioButtons.Count - 1 do begin
		if FRadioButtons[i] <> radioButton then begin
			FRadioButtons[i].Checked := False;
		end;
	end;

	// Set the selected item
	FItemIndex := itemIndex;

	// Fire event
	if Assigned(FOnItemSelect) and (itemIndex >= 0) and (itemIndex < FItems.Count) then begin
		FOnItemSelect(Self, FItems[itemIndex]);
	end;
end;

procedure TCustomRadioGroup.Resize;
begin
	inherited Resize;
	arrangeRadioButtons;
end;

procedure TCustomRadioGroup.setColumns(const _value: Integer);
begin
	if (_value > 0) and (FColumns <> _value) then begin
		FColumns := _value;
		arrangeRadioButtons;
	end;
end;

procedure TCustomRadioGroup.setItemIndex(const _value: Integer);
var
	i: Integer;
begin
	if (FItemIndex <> _value) and (_value >= -1) and (_value < FItems.Count) then begin
		FItemIndex := _value;

		// Update radio button states
		for i := 0 to FRadioButtons.Count - 1 do begin
			FRadioButtons[i].Checked := (i = _value);
		end;

		// Fire event
		if Assigned(FOnItemSelect) and (_value >= 0) then begin
			FOnItemSelect(Self, FItems[_value]);
		end;
	end;
end;

function TCustomRadioGroup.getSelectedItem: TCustomRadioItem;
begin
	if (FItemIndex >= 0) and (FItemIndex < FItems.Count) then begin
		Result := FItems[FItemIndex];
	end else begin
		Result := nil;
	end;
end;

procedure Register;
begin
	RegisterComponents('Custom', [TCustomRadioGroup]);
end;

end.
