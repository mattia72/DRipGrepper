unit RipGrepper.Settings.Instance;

interface

uses
	RipGrepper.Settings.RipGrepperSettings,
	System.SysUtils;

type
	TRipGrepperSettingsInstance = class
		strict private
			constructor Create;
			class destructor Destroy;

		private
			class var FInstance : TRipGrepperSettingsInstance;
			class var FLock : TObject;
			class var FSettings : TRipGrepperSettings;
		public
			class constructor Create;
			destructor Destroy; override;
			class function GetInstance: TRipGrepperSettings; static;
			class property Instance : TRipGrepperSettings read GetInstance;
	end;

implementation

constructor TRipGrepperSettingsInstance.Create;
begin
	inherited;
	FSettings := TRipGrepperSettings.Create;
end;

class constructor TRipGrepperSettingsInstance.Create;
begin
	Flock := TObject.Create;
end;

destructor TRipGrepperSettingsInstance.Destroy;
begin
	FSettings.Free;
	inherited;
end;

class destructor TRipGrepperSettingsInstance.Destroy;
begin
	FInstance.Free;
	FLock.Free;
end;

class function TRipGrepperSettingsInstance.GetInstance: TRipGrepperSettings;
begin
	if not Assigned(FInstance) then begin
		System.TMonitor.Enter(FLock);
		try
			if not Assigned(FInstance) then begin
				FInstance := TRipGrepperSettingsInstance.Create;
			end;
		finally
			System.TMonitor.Exit(FLock);
		end;
	end;
	Result := FInstance.FSettings;
end;

//initialization

//TRipGrepperSettingsInstance.FLock := TObject.Create;

//finalization

//FreeAndNil(TRipGrepperSettingsInstance.FInstance);
//FreeAndNil(TRipGrepperSettingsInstance.FLock);

end.
