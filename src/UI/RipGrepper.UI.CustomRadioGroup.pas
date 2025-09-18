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

	// Forward declaration
	TCustomRadioGroup = class;

	// Event type for item selection
	TRadioItemSelectEvent = procedure(Sender : TObject; Item : TCustomRadioItem) of object;

	// Main custom radio group control
	TCustomRadioGroup = class(TCustomPanel)
		private
			FItems : TCustomRadioItems;
			FItemIndex : Integer;
			FColumns : Integer;
			FOnItemSelect : TRadioItemSelectEvent;
			procedure onRadioButtonClick(_sender : TObject);
			procedure setColumns(const _value : Integer);
			procedure setItemIndex(const _value : Integer);
			function getSelectedItem : TCustomRadioItem;

		protected
			procedure Resize; override;

		public
			constructor Create(_owner : TComponent); override;
			destructor Destroy; override;
			procedure Clear;
			function AddItem(const _caption, _hint : string; _orderIndex : Integer; _obj : IInterface = nil) : TCustomRadioItem;
			procedure Arrange();

		published
			property SelectedItem : TCustomRadioItem read getSelectedItem;
			property Items : TCustomRadioItems read FItems write FItems;
			property ItemIndex : Integer read FItemIndex write setItemIndex default -1;
			property Columns : Integer read FColumns write setColumns default 1;
			property OnItemSelect : TRadioItemSelectEvent read FOnItemSelect write FOnItemSelect;
	end;

procedure Register;

implementation

uses
	Math,
	Spring,
	RipGrepper.Common.IDEContextValues,
	RipGrepper.Common.SimpleTypes;

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

{ TCustomRadioGroup }

constructor TCustomRadioGroup.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	FItems := TCustomRadioItems.Create(Self);
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
	FItems.Free;
	inherited Destroy;
end;

procedure TCustomRadioGroup.Clear;
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

function TCustomRadioGroup.AddItem(const _caption, _hint : string; _orderIndex : Integer; _obj : IInterface = nil) : TCustomRadioItem;
var
	radioButton : TRadioButton;
begin
	radioButton := TRadioButton.Create(Self);
	radioButton.Parent := Self;
	radioButton.Caption := _caption;
	radioButton.OnClick := onRadioButtonClick;
	radioButton.Hint := _hint;
	Result := FItems.AddItem(radioButton, _caption, _orderIndex, _obj);
end;

procedure TCustomRadioGroup.Arrange();
var
	sortedItems : IShared<TList<TCustomRadioItem>>;
	i, j, col, row : Integer;
	itemHeight, itemWidth : Integer;
	maxRows : Integer;
	item : TCustomRadioItem;
	originalIndex : Integer;
begin
	if FItems.Count = 0 then begin
		Exit;
	end;

	// Create a sorted list based on OrderIndex
	sortedItems := Shared.Make < TList < TCustomRadioItem >> ();
	for i := 0 to FItems.Count - 1 do begin
		sortedItems.Add(FItems[i]);
	end;

	// Sort by OrderIndex
	sortedItems.Sort(TComparer<TCustomRadioItem>.Construct(
		function(const Left, Right : TCustomRadioItem) : Integer
		begin
			Result := TComparer<Integer>.Default.Compare(Left.OrderIndex, Right.OrderIndex);
		end));

	// Calculate layout
	itemHeight := 22;
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

			// Find the original index in the collection
			originalIndex := -1;
			for j := 0 to FItems.Count - 1 do begin
				if FItems[j] = item then begin
					originalIndex := j;
					Break;
				end;
			end;
			item.RadioButton.Tag := originalIndex;
		end;
	end;

	// Adjust control height if needed
	if maxRows > 0 then begin
		Height := maxRows * itemHeight + 8; // Minimal padding for clean layout
	end;

end;

procedure TCustomRadioGroup.onRadioButtonClick(_sender : TObject);
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

procedure TCustomRadioGroup.Resize;
begin
	inherited Resize;
	Arrange;
end;

procedure TCustomRadioGroup.setColumns(const _value : Integer);
begin
	if (_value > 0) and (FColumns <> _value) then begin
		FColumns := _value;
		Arrange;
	end;
end;

procedure TCustomRadioGroup.setItemIndex(const _value : Integer);
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

function TCustomRadioGroup.getSelectedItem : TCustomRadioItem;
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
