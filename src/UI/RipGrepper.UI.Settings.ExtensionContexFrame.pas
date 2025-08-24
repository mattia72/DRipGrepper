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
	RipGrepper.Common.IDEContextValues,
	RipGrepper.UI.CustomRadioGroup;

type
	TExtensionContextFrame = class(TFrame)
		private
			FContextRadioGroup : TCustomRadioGroup;
			FOnContextChange : TExtensionContextChangeEvent;
			function GetContextValues() : IIDEContextValues;
			function getSelectedItem() : TCustomRadioItem;
			procedure onRadioItemSelect(_sender : TObject; _item : TCustomRadioItem);

		public
			constructor Create(_owner : TComponent); override;
			class function GetAsHint(const _paths : string): string; overload;
			class function GetAsHint(var _paths: TArray<string>): string; overload;
			function GetSelectedIDEContext : EDelphiIDESearchContext;
			procedure SetSelectedIDEContext(_ideContext : EDelphiIDESearchContext);
			property ContextRadioGroup : TCustomRadioGroup read FContextRadioGroup;
			property ContextValues : IIDEContextValues read GetContextValues;

		published
			property SelectedItem : TCustomRadioItem read getSelectedItem;
			property OnContextChange : TExtensionContextChangeEvent read FOnContextChange write FOnContextChange;
	end;

implementation

uses
	Spring,
	RipGrepper.Settings.ExtensionSettings,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants;

{$R *.dfm}

constructor TExtensionContextFrame.Create(_owner : TComponent);
var
	icv : IIDEContextValues;
	dic : TDelphiIDEContext;
begin
	inherited Create(_owner);

	FContextRadioGroup := TCustomRadioGroup.Create(Self);
	FContextRadioGroup.Parent := Self;
	FContextRadioGroup.Align := alClient;
	FContextRadioGroup.Columns := 1;
	FContextRadioGroup.OnItemSelect := onRadioItemSelect;

	dic.LoadFromIOTA();

	icv := TIDEContextValues.Create(EDelphiIDESearchContext.dicActiveFile, dic.ActiveFile);
	FContextRadioGroup.AddItem('Current File', dic.ActiveFile, 0, icv);

	var
	value := string.Join(';', dic.OpenFiles);
	icv := TIDEContextValues.Create(EDelphiIDESearchContext.dicOpenFiles, value);
	FContextRadioGroup.AddItem('All Open Files', GetAsHint(dic.OpenFiles), 1, icv);

	value := string.Join(';', dic.ProjectFiles);
	icv := TIDEContextValues.Create(EDelphiIDESearchContext.dicProjectFiles, value);
	FContextRadioGroup.AddItem('Project Files', GetAsHint(dic.ProjectFiles), 2, icv);

	value := string.Join(';', dic.ProjectSourcePath);
	icv := TIDEContextValues.Create(EDelphiIDESearchContext.dicProjectSourcePath, value);
	FContextRadioGroup.AddItem('Project Source Paths', GetAsHint(dic.ProjectSourcePath), 3, icv);

	icv := TIDEContextValues.Create(EDelphiIDESearchContext.dicCustomLocation, '');
	FContextRadioGroup.AddItem('Custom Locations:', '', 4, icv);

	// Select first option by default
	FContextRadioGroup.ItemIndex := 0;
end;

class function TExtensionContextFrame.GetAsHint(const _paths : string) : string;
begin
	var
	pathArray := _paths.Split([';', ',']);
	Result := GetAsHint(pathArray);
end;

class function TExtensionContextFrame.GetAsHint(var _paths: TArray<string>):
	string;
begin
	TArray.Sort<string>(_paths);
	Result := string.Join(CRLF, _paths);
end;

function TExtensionContextFrame.GetContextValues() : IIDEContextValues;
var
	icv : IIDEContextValues;
begin
	if Assigned(SelectedItem) and Assigned(SelectedItem.TagObject) then begin
		if Supports(SelectedItem.TagObject, IIDEContextValues, icv) then begin
			Result := icv;
			Exit;
		end;
	end;
	Result := TIDEContextValues.Create(EDelphiIDESearchContext.dicNotSet, '');
end;

procedure TExtensionContextFrame.onRadioItemSelect(_sender : TObject; _item : TCustomRadioItem);
var
	icv : IIDEContextValues;
begin
	if Assigned(_item) and Assigned(_item.TagObject) then begin
		if Supports(_item.TagObject, IIDEContextValues, icv) then begin
			// Fire change event
			if Assigned(FOnContextChange) then begin
				FOnContextChange(Self, icv);
			end;
		end;
	end;
end;

function TExtensionContextFrame.GetSelectedIDEContext : EDelphiIDESearchContext;
var
	selectedItem : TCustomRadioItem;
begin
	selectedItem := FContextRadioGroup.SelectedItem;
	if Assigned(selectedItem) and Assigned(selectedItem.TagObject) then begin
		Result := (selectedItem.TagObject as IIDEContextValues).GetContextType;
	end else begin
		Result := EDelphiIDESearchContext.dicNotSet;
	end;
end;

function TExtensionContextFrame.getSelectedItem() : TCustomRadioItem;
begin
	Result := ContextRadioGroup.SelectedItem;
end;

procedure TExtensionContextFrame.SetSelectedIDEContext(_ideContext : EDelphiIDESearchContext);
var
	i : Integer;
	item : TCustomRadioItem;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TExtensionContextFrame.SetSelectedIDEContext');
	for i := 0 to FContextRadioGroup.Items.Count - 1 do begin
		item := FContextRadioGroup.Items[i];
		if Assigned(item.TagObject) and
		{ } ((item.TagObject as IIDEContextValues).GetContextType = _ideContext) then begin
			dbgMsg.MsgFmt('Setting IDE context to %s', [item.Caption]);
			FContextRadioGroup.ItemIndex := i;
			Break;
		end;
	end;
end;

end.
