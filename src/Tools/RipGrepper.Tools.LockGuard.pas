unit RipGrepper.Tools.LockGuard;

interface

uses
	System.SyncObjs;

type
	TLockGuard = record
		private
			FLockObj: TObject;

		public
			class function NewLock(const _lockObj: TObject): TLockGuard; static;
			class operator Finalize(var Dest : TLockGuard);
			class operator Initialize(out Dest : TLockGuard);
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils;

class function TLockGuard.NewLock(const _lockObj: TObject): TLockGuard;
begin
	TDebugUtils.Msg('TLockGuard.NewLock');
	Result.FLockObj := _lockObj;
	TMonitor.Enter(Result.FLockObj);
end;

class operator TLockGuard.Finalize(var Dest : TLockGuard);
begin
	TDebugUtils.Msg('TLockGuard.Release');
	TMonitor.Exit(Dest.FLockObj);
end;

class operator TLockGuard.Initialize(out Dest : TLockGuard);
begin
	TDebugUtils.Msg('TLockGuard.Initialize');
	Dest.FLockObj := nil;
end;

end.
