unit RipGrepper.Common.Settings.SettingsDictionary;

interface

uses
	RipGrepper.Common.Settings.SettingVariant,
	System.Generics.Collections;

type
	TSettingsDictionary = class(TDictionary<string, ISettingVariant>)

		public
			constructor Create;
			destructor Destroy; override;
			procedure AddOrChange(const Key : string; const Value : ISettingVariant);
	end;

implementation

constructor TSettingsDictionary.Create;
begin
	inherited Create;
end;

destructor TSettingsDictionary.Destroy;
begin
	for var key in Keys do begin
		Self[key] := nil;
	end;
	inherited Destroy;
end;

procedure TSettingsDictionary.AddOrChange(const Key : string; const Value : ISettingVariant);
var
	setting : ISettingVariant;
begin
	if self.TryGetValue(Key, setting) and Assigned(setting) and (not setting.Equals(Value)) then begin
		self[Key] := nil;
	end;
	AddOrSetValue(Key, Value);
end;

end.
