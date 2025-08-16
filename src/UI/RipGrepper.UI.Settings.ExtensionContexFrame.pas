unit RipGrepper.UI.Settings.ExtensionContexFrame;

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
	Vcl.StdCtrls,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.UI.CustomRadioGroup;

type
	// Event type for extension context change
	TExtensionContextChangeEvent = procedure(Sender: TObject; IDEContext: EDelphiIDESearchContext) of object;

	TExtensionContextFrame = class(TFrame)
		private
			FCustomRadioGroup: TCustomRadioGroup;
			FOnContextChange: TExtensionContextChangeEvent;
			procedure onRadioItemSelect(_sender: TObject; _item: TCustomRadioItem);
		public
			constructor Create(_owner: TComponent); override;
			function GetSelectedIDEContext: EDelphiIDESearchContext;
			procedure SetSelectedIDEContext(_ideContext: EDelphiIDESearchContext);
			property CustomRadioGroup: TCustomRadioGroup read FCustomRadioGroup;
		published
			property OnContextChange: TExtensionContextChangeEvent read FOnContextChange write FOnContextChange;
	end;

implementation

{$R *.dfm}

constructor TExtensionContextFrame.Create(_owner: TComponent);
begin
	inherited Create(_owner);
	
	// Create the custom radio group control
	FCustomRadioGroup := TCustomRadioGroup.Create(Self);
	FCustomRadioGroup.Parent := Self;
	FCustomRadioGroup.Align := alClient;
	FCustomRadioGroup.Columns := 1;
	FCustomRadioGroup.OnItemSelect := onRadioItemSelect;
	
	// Add extension options matching rbExtensionOptions order and values
	// Store EDelphiIDESearchContext enum values in the Obj property
	FCustomRadioGroup.AddItem('Current File',Integer(EDelphiIDESearchContext.dicActiveFile), TObject(Pointer(Integer(EDelphiIDESearchContext.dicActiveFile))));
	FCustomRadioGroup.AddItem('All Open Files', Integer(EDelphiIDESearchContext.dicOpenFiles), TObject(Pointer(Integer(EDelphiIDESearchContext.dicOpenFiles))));
	FCustomRadioGroup.AddItem('Project Files', Integer(EDelphiIDESearchContext.dicProjectFiles), TObject(Pointer(Integer(EDelphiIDESearchContext.dicProjectFiles))));
	FCustomRadioGroup.AddItem('Project Source Paths', Integer(EDelphiIDESearchContext.dicProjectSourcePath), TObject(Pointer(Integer(EDelphiIDESearchContext.dicProjectSourcePath))));
	FCustomRadioGroup.AddItem('Custom Locations:', Integer(EDelphiIDESearchContext.dicPath), TObject(Pointer(Integer(EDelphiIDESearchContext.dicPath))));
	
	// Select first option by default
	FCustomRadioGroup.ItemIndex := 0;
end;

procedure TExtensionContextFrame.onRadioItemSelect(_sender: TObject; _item: TCustomRadioItem);
var
	ideContext: EDelphiIDESearchContext;
begin
	if Assigned(_item) and Assigned(_item.Obj) then begin
		ideContext := EDelphiIDESearchContext(Integer(Pointer(_item.Obj)));
		
		// Fire change event
		if Assigned(FOnContextChange) then begin
			FOnContextChange(Self, ideContext);
		end;
	end;
end;

function TExtensionContextFrame.GetSelectedIDEContext: EDelphiIDESearchContext;
var
	selectedItem: TCustomRadioItem;
begin
	selectedItem := FCustomRadioGroup.SelectedItem;
	if Assigned(selectedItem) and Assigned(selectedItem.Obj) then begin
		Result := EDelphiIDESearchContext(Integer(Pointer(selectedItem.Obj)));
	end else begin
		Result := EDelphiIDESearchContext.dicNotSet;
	end;
end;

procedure TExtensionContextFrame.SetSelectedIDEContext(_ideContext: EDelphiIDESearchContext);
var
	i: Integer;
	item: TCustomRadioItem;
begin
	for i := 0 to FCustomRadioGroup.Items.Count - 1 do begin
		item := FCustomRadioGroup.Items[i];
		if Assigned(item.Obj) and (EDelphiIDESearchContext(Integer(Pointer(item.Obj))) = _ideContext) then begin
			FCustomRadioGroup.ItemIndex := i;
			Break;
		end;
	end;
end;

end.
