unit RipGrepper.UI.Settings.ExtensionContexPanel;

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
	Vcl.ExtCtrls,
	RipGrepper.Common.IDEContextValues,
	RipGrepper.Settings.ExtensionSettings,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.UI.CustomRadioOptions,
	RipGrepper.UI.RgOptionsPanel;

type
	TExtensionContexPanel = class(TOptionPanel)
		const
			PARENT_MARGIN = 16;
			RADIO_GROUP_MARGIN = 8;
			PANEL_PADDING = 4;

		strict private
		private
			FContextRadioGroup : TCustomRadioOptions;
			FOnContextChange : TExtensionContextChangeEvent;
			FRadioItemIndex : Integer;
			procedure AddItem(const _caption : string; const _dic : TDelphiIDEContext; const _bInExpertModeOnly : Boolean = False);
			procedure AddItemIntern(const caption : string; const _dic : TDelphiIDEContext);
			function GetContextValues() : IIDEContextValues;
			function getSelectedItem() : TCustomRadioItem;
			procedure onRadioItemSelect(_sender : TObject; _item : TCustomRadioItem);

		public
			constructor Create(_owner : TComponent); override;
			procedure AddItems();
			procedure AdjustHeight();
			class function GetAsHint(const _paths : string) : string; overload;
			class function GetAsHint(var _paths : TArray<string>) : string; overload;
			function GetSelectedIDEContext : EDelphiIDESearchContext;
			procedure SetSelectedIDEContext(_ideContext : EDelphiIDESearchContext);
			property ContextRadioGroup : TCustomRadioOptions read FContextRadioGroup;
			property ContextValues : IIDEContextValues read GetContextValues;

		published
			property SelectedItem : TCustomRadioItem read getSelectedItem;
			property OnContextChange : TExtensionContextChangeEvent read FOnContextChange write FOnContextChange;
	end;

implementation

uses
	Spring,
	System.IOUtils,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	System.StrUtils;

constructor TExtensionContexPanel.Create(_owner : TComponent);
begin
	inherited Create(_owner);
//  BevelOuter := bvNone;

	// pnlMain := TPanel.Create(Self);
	// pnlMain.Parent := Self;
	pnlMain.Align := alTop;
	// pnlMain.BevelOuter := bvNone;
	// pnlMain.Caption := '';
	// pnlMain.Padding.Left := PANEL_PADDING;
	// pnlMain.Padding.Top := 0;
	// pnlMain.Padding.Right := PANEL_PADDING;
	// pnlMain.Padding.Bottom := 0;

	FContextRadioGroup := TCustomRadioOptions.Create(Self);
	FContextRadioGroup.Parent := pnlMain;
	FContextRadioGroup.Align := alClient;
	FContextRadioGroup.Columns := 3;
	FContextRadioGroup.OnRadioItemSelect := onRadioItemSelect;
	FRadioItemIndex := 0;
end;

procedure TExtensionContexPanel.AddItem(const _caption : string; const _dic : TDelphiIDEContext;
	const _bInExpertModeOnly : Boolean = False);
var
	caption : string;
	expertChoice : Boolean;
begin
	if not(Assigned(Settings) and Assigned(Settings.AppSettings)) then begin
		AddItemIntern(_caption, _dic);
	end else begin
		expertChoice := Settings.AppSettings.ExpertMode and _bInExpertModeOnly;
		caption := IfThen(expertChoice, _caption + ' *', _caption);
		if expertChoice then begin
			AddItemIntern(caption, _dic);
		end else if not _bInExpertModeOnly then begin
			AddItemIntern(caption, _dic);
		end;
	end;
end;

procedure TExtensionContexPanel.AddItemIntern(const caption : string; const _dic : TDelphiIDEContext);
var
	icv : IIDEContextValues;
	values : string;
	hint : string;
begin
	values := _dic.GetValueByContext();
	icv := TIDEContextValues.Create(_dic.IDESearchContext, values);
	hint := GetAsHint(values);
	if hint.IsEmpty then begin
		hint := values;
	end;
	FContextRadioGroup.AddItem(caption, hint, FRadioItemIndex, icv);
	Inc(FRadioItemIndex);
end;

procedure TExtensionContexPanel.AddItems();
var
	dic : TDelphiIDEContext;
begin
	dic.LoadFromIOTA();

	dic.IDESearchContext := EDelphiIDESearchContext.dicActiveFile;
	AddItem('Current File', dic);

	dic.IDESearchContext := EDelphiIDESearchContext.dicOpenFiles;
	AddItem('All Open Files', dic);

	dic.IDESearchContext := EDelphiIDESearchContext.dicProjectFiles;
	AddItem('Project Files', dic);

	dic.IDESearchContext := EDelphiIDESearchContext.dicProjectFilesDirs;
	AddItem('Project Files Directories', dic);

	dic.IDESearchContext := EDelphiIDESearchContext.dicProjectRootDirectory;
	AddItem('Project Root Directory', dic, True);

	dic.IDESearchContext := EDelphiIDESearchContext.dicProjectLibraryPath;
	AddItem('Project Library Paths', dic, True);

	dic.IDESearchContext := EDelphiIDESearchContext.dicCustomLocation;
	AddItem('Custom Locations:', dic);

	// Select first option by default
	// FContextRadioGroup.ItemIndex := 0;
end;

procedure TExtensionContexPanel.AdjustHeight();
begin
	// Ensure width fits within parent with margins
	if Assigned(Parent) then begin
		Width := Parent.ClientWidth - PARENT_MARGIN;
	end;
	// Make sure the radio group uses full available width
	FContextRadioGroup.Width := Width - RADIO_GROUP_MARGIN;
	// Align radio button items
	FContextRadioGroup.AlignControlItems();
	// Height should match the radio group height exactly
	// pnlMain has no top/bottom padding and uses alClient,
	// so the outer panel height equals the radio group height
	Height := FContextRadioGroup.Height;
end;

class function TExtensionContexPanel.GetAsHint(const _paths : string) : string;
begin
	var
	pathArray := _paths.Split([';', ',']);
	Result := GetAsHint(pathArray);
end;

class function TExtensionContexPanel.GetAsHint(var _paths : TArray<string>) : string;
begin
	// TArray.Sort<string>(_paths);
	Result := string.Join(CRLF, _paths);
end;

function TExtensionContexPanel.GetContextValues() : IIDEContextValues;
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

procedure TExtensionContexPanel.onRadioItemSelect(_sender : TObject; _item : TCustomRadioItem);
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

function TExtensionContexPanel.GetSelectedIDEContext : EDelphiIDESearchContext;
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

function TExtensionContexPanel.getSelectedItem() : TCustomRadioItem;
begin
	Result := ContextRadioGroup.SelectedItem;
end;

procedure TExtensionContexPanel.SetSelectedIDEContext(_ideContext : EDelphiIDESearchContext);
var
	i : Integer;
	item : TCustomRadioItem;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TExtensionContexPanel.SetSelectedIDEContext');
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
