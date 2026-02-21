unit RipGrepper.Settings.PersistableArray;

interface

uses
	RipGrepper.Settings.Persistable,
	RipGrepper.Settings.SettingVariant,
	RipGrepper.Common.Constants,
	RipGrepper.Tools.DebugUtils,
	ArrayEx;

type
	TPersistableArray = class(TPersistableSettings, IPersistableArray)
		private
			FArraySetting : IArraySetting;

			function GetArraySetting() : IArraySetting;
			function GetItem(index : Integer) : string;
			procedure SetArraySetting(const Value : IArraySetting);
			procedure SetItem(index : Integer; const Value : string);

		protected
			procedure UpdateSettingsFromInternals(); override;

		public
			constructor Create(_section : string; const _arr : IArraySetting);
			procedure Copy(const _other : IPersistableArray); reintroduce;
			procedure ReadFile(); override;
			procedure Init; override;
			property ArraySetting : IArraySetting read GetArraySetting write SetArraySetting;
			property Item[index : Integer] : string read GetItem write SetItem;
	end;

implementation

	{ TPersistableArray }

constructor TPersistableArray.Create(_section : string; const _arr : IArraySetting);
begin
	IniSectionName := _section; // must be set before inherited Create, so SettingsDict.FSectionName is initialized correctly
	inherited Create(nil);
	FArraySetting := _arr;
	FManagedByInterface := True; // Indicate this object is managed by an interface
end;

procedure TPersistableArray.Copy(const _other : IPersistableArray);
begin
	FArraySetting.Copy(_other.GetArraySetting);
end;

function TPersistableArray.GetArraySetting() : IArraySetting;
begin
	Result := FArraySetting;
end;

function TPersistableArray.GetItem(index : Integer) : string;
begin
	Result := '';
	if TArraySetting(FArraySetting).Count > index then begin
		Result := TArraySetting(FArraySetting)[index];
	end;
end;

procedure TPersistableArray.Init;
begin
	// FArraySetting := TArraySetting.Create('TestData', FArrValues);
	// CreateSetting(FArraySetting.Name, ITEM_KEY_PREFIX, FArraySetting);
end;

procedure TPersistableArray.SetArraySetting(const Value : IArraySetting);
begin
	FArraySetting := Value;
end;

procedure TPersistableArray.SetItem(index : Integer; const Value : string);
var
	arrCmds : TArrayEx<string>;
begin
	if FArraySetting.Value.IsEmpty then
		Exit;

	if FArraySetting.Value.Count > index then begin
		if (FArraySetting.Value[index] <> Value) then begin
			FArraySetting.Value[index] := Value;
			FIsModified := True;
		end;
	end else begin
		arrCmds := FArraySetting.Value;
		arrCmds.Add(Value);
		FArraySetting.Value := arrCmds;
		FIsModified := True;
	end;
end;

procedure TPersistableArray.ReadFile();
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TPersistableArray.ReadFile');
	if not Assigned(FArraySetting) then begin
		dbgMsg.ErrorMsg('FArraySetting not assigned - cannot load from persister');
		Exit;
	end;
	if not Assigned(FArraySetting.Persister) then begin
		dbgMsg.ErrorMsg('FArraySetting.Persister not assigned for section: ' + IniSectionName);
		Exit;
	end;
	FArraySetting.LoadFromPersister();
	dbgMsg.MsgFmt('Loaded %d items from section [%s]', [FArraySetting.Count, IniSectionName]);
end;

procedure TPersistableArray.UpdateSettingsFromInternals();
begin
	inherited;
	// Re-create the setting in SettingsDict to update individual string entries (Item_0, Item_1, etc.)
	CreateSetting(FArraySetting.Name, ITEM_KEY_PREFIX, FArraySetting);
end;

end.
