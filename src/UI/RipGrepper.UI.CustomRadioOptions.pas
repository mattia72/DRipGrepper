unit RipGrepper.UI.CustomRadioOptions;

interface

uses
	System.Classes,
	System.Generics.Collections,
	System.Generics.Defaults,
	System.SysUtils,
	Vcl.Controls,
	Vcl.StdCtrls,
	Vcl.ExtCtrls,
	Winapi.Windows,
	RipGrepper.UI.CustomCheckOptions;

type
	// Forward declarations
	TCustomRadioOptions = class;

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

	// Event type for item selection
	TRadioItemSelectEvent = procedure(Sender : TObject; Item : TCustomRadioItem) of object;

	// Custom radio options control
	TCustomRadioOptions = class(TCustomOptionsBase)
		private
			FItems : TCustomRadioItems;
			FItemIndex : Integer;
			FOnRadioItemSelect : TRadioItemSelectEvent;
			procedure onRadioButtonClick(_sender : TObject);
			procedure setItemIndex(const _value : Integer);
			function getSelectedItem : TCustomRadioItem;

		protected
		public
			constructor Create(_owner : TComponent); override;
			destructor Destroy; override;
			procedure Clear; override;
			function AddItem(const _caption, _hint : string; _orderIndex : Integer; _obj : IInterface = nil) : TCustomRadioItem;
			procedure AlignControlItems(); override;

		published
			property SelectedItem : TCustomRadioItem read getSelectedItem;
			property Items : TCustomRadioItems read FItems write FItems;
			property ItemIndex : Integer read FItemIndex write setItemIndex default -1;
			property OnRadioItemSelect : TRadioItemSelectEvent read FOnRadioItemSelect write FOnRadioItemSelect;
	end;

procedure Register;

implementation

uses
	Math,
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

procedure TCustomRadioOptions.AlignControlItems();
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
	itemWidth := Width div Columns;
	maxRows := Ceil(FItems.Count / Columns);

	// Position radio buttons
	for i := 0 to FItems.Count - 1 do begin
		item := FItems[i];
		if Assigned(item.RadioButton) then begin
			col := i mod Columns;
			row := i div Columns;

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
	if Assigned(FOnRadioItemSelect) and (itemIndex >= 0) and (itemIndex < FItems.Count) then begin
		FOnRadioItemSelect(Self, FItems[itemIndex]);
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
		if Assigned(FOnRadioItemSelect) and (_value >= 0) then begin
			FOnRadioItemSelect(Self, FItems[_value]);
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

procedure Register;
begin
	RegisterComponents('Custom', [TCustomRadioOptions]);
end;

end.
