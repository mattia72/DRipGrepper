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
	RipGrepper.UI.CustomRadioGroup,
	RipGrepper.Settings.ExtensionSettings,
	RipGrepper.Settings.RipGrepperSettings;

type
	TExtensionContextFrame = class(TFrame)
		strict private
			FSettings : TRipGrepperSettings;
			procedure SetSettings(const Value : TRipGrepperSettings);

		private
			FContextRadioGroup : TCustomRadioGroup;
			FOnContextChange : TExtensionContextChangeEvent;
			procedure AddItem(const _caption : string; const _idx : Integer; const _dic : TDelphiIDEContext;
				const _bInExpertModeOnly : Boolean = False);
			procedure AddItemIntern(const caption : string; const _idx : Integer; const _dic : TDelphiIDEContext);
			function GetContextValues() : IIDEContextValues;
			function getSelectedItem() : TCustomRadioItem;
			procedure onRadioItemSelect(_sender : TObject; _item : TCustomRadioItem);

		public
			constructor Create(_owner : TComponent); override;
			class function GetAsHint(const _paths : string) : string; overload;
			class function GetAsHint(var _paths : TArray<string>) : string; overload;
			function GetSelectedIDEContext : EDelphiIDESearchContext;
			procedure SetSelectedIDEContext(_ideContext : EDelphiIDESearchContext);
			property ContextRadioGroup : TCustomRadioGroup read FContextRadioGroup;
			property ContextValues : IIDEContextValues read GetContextValues;

		published
			property SelectedItem : TCustomRadioItem read getSelectedItem;
			property Settings : TRipGrepperSettings read FSettings write SetSettings;
			property OnContextChange : TExtensionContextChangeEvent read FOnContextChange write FOnContextChange;
	end;

implementation

uses
	Spring,
	System.IOUtils,

	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	System.StrUtils;

{$R *.dfm}

constructor TExtensionContextFrame.Create(_owner : TComponent);
var
	dic : TDelphiIDEContext;
begin
	inherited Create(_owner);

	FContextRadioGroup := TCustomRadioGroup.Create(Self);
	FContextRadioGroup.Parent := Self;
	FContextRadioGroup.Align := alClient;
	FContextRadioGroup.Columns := 2;
	FContextRadioGroup.OnItemSelect := onRadioItemSelect;

	dic.LoadFromIOTA();

	dic.IDESearchContext := EDelphiIDESearchContext.dicActiveFile;
	AddItem('Current File', 0, dic);

	dic.IDESearchContext := EDelphiIDESearchContext.dicOpenFiles;
	AddItem('All Open Files', 1, dic);

	dic.IDESearchContext := EDelphiIDESearchContext.dicProjectFiles;
	AddItem('Project Files', 2, dic);

	dic.IDESearchContext := EDelphiIDESearchContext.dicProjectRootDirectory;
	AddItem('Project Root Directory', 3, dic);

	dic.IDESearchContext := EDelphiIDESearchContext.dicProjectSourcePath;
	AddItem('Project Source Paths', 4, dic, True);

	dic.IDESearchContext := EDelphiIDESearchContext.dicCustomLocation;
	AddItem('Custom Locations:', 5, dic);

	// Select first option by default
	FContextRadioGroup.ItemIndex := 0;
end;

procedure TExtensionContextFrame.AddItem(const _caption : string; const _idx : Integer; const _dic : TDelphiIDEContext;
	const _bInExpertModeOnly : Boolean = False);
var
	icv : IIDEContextValues;
	values, hint, caption : string;
begin
	caption := IfThen(Settings.AppSettings.ExpertMode, _caption + ' *', _caption);
	if Settings.AppSettings.ExpertMode then begin
		if _bInExpertModeOnly then begin
			AddItemIntern(caption, _idx, _dic);
		end;
	end else begin
		AddItemIntern(caption, _idx, _dic);
	end;
end;

procedure TExtensionContextFrame.AddItemIntern(const caption : string; const _idx : Integer; const _dic : TDelphiIDEContext);
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
	FContextRadioGroup.AddItem(caption, hint, _idx, icv);
end;

class function TExtensionContextFrame.GetAsHint(const _paths : string) : string;
begin
	var
	pathArray := _paths.Split([';', ',']);
	Result := GetAsHint(pathArray);
end;

class function TExtensionContextFrame.GetAsHint(var _paths : TArray<string>) : string;
begin
	// TArray.Sort<string>(_paths);
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

procedure TExtensionContextFrame.SetSettings(const Value : TRipGrepperSettings);
begin
	FSettings := Value;
end;

end.
