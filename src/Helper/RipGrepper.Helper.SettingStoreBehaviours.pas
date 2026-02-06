unit RipGrepper.Helper.SettingStoreBehaviours;

interface

uses
	System.SysUtils;

type
	TSettingStoreBehaviour = (ssbNotSet,
		{ } ssbStoreIfModified,
		{ } ssbStoreAfterChangeImmediately,
		{ } ssbStoreOnceEvenIfNotModified);

	TSettingStoreBehaviours = set of TSettingStoreBehaviour;

	TSettingStoreBehavioursHelper = class
		const
			SettingStoreBehaviourNames : array [TSettingStoreBehaviour] of string = (
				{ } 'ssbNotSet',
				{ } 'ssbStoreIfModified',
				{ } 'ssbStoreAfterChangeImmediately',
				{ } 'ssbStoreOnceEvenIfNotModified');

		private
			FBehaviours : TSettingStoreBehaviours;

		public
			constructor Create(const _behaviours : TSettingStoreBehaviours);
			class function ToString(const _behaviours : TSettingStoreBehaviours) : string; reintroduce;
			class function FromString(const _value : string) : TSettingStoreBehaviours;

			property Behaviours : TSettingStoreBehaviours read FBehaviours write FBehaviours;
	end;

implementation

constructor TSettingStoreBehavioursHelper.Create(const _behaviours : TSettingStoreBehaviours);
begin
	FBehaviours := _behaviours;
end;

class function TSettingStoreBehavioursHelper.ToString(const _behaviours : TSettingStoreBehaviours) : string;
var
	behaviourNames : TArray<string>;
begin
	behaviourNames := [];
	for var behaviour : TSettingStoreBehaviour in _behaviours do begin
		behaviourNames := behaviourNames + [SettingStoreBehaviourNames[behaviour]];
	end;
	Result := string.Join(',', behaviourNames);
end;

class function TSettingStoreBehavioursHelper.FromString(const _value : string) : TSettingStoreBehaviours;
var
	behaviourNames : TArray<string>;
begin
	Result := [];
	behaviourNames := _value.Split([',']);
	for var behaviourName : string in behaviourNames do begin
		for var behaviour : TSettingStoreBehaviour := low(TSettingStoreBehaviour) to high(TSettingStoreBehaviour) do begin
			if SettingStoreBehaviourNames[behaviour] = behaviourName then begin
				Include(Result, behaviour);
			end;
		end;
	end;
end;

end.
