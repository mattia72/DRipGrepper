unit RipGrepper.Tools.LockGuard;

interface

uses
	System.SyncObjs;

type
	TLockGuard = record
		private
			FbOwnCriticalSection : Boolean;
			FCriticalSection : TCriticalSection;

		public
			class function NewLock(const _critSec : TCriticalSection = nil) : TLockGuard; static;
			class operator Finalize(var Dest : TLockGuard);
			class operator Initialize(out Dest : TLockGuard);
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils;

class function TLockGuard.NewLock(const _critSec : TCriticalSection = nil) : TLockGuard;
begin
	TDebugUtils.Msg('TLockGuard.NewLock');
	Result.FbOwnCriticalSection := (_critSec = nil);
	if Result.FbOwnCriticalSection then begin
		Result.FCriticalSection := TCriticalSection.Create();
	end else begin
		Result.FCriticalSection := _critSec;
	end;
end;

class operator TLockGuard.Finalize(var Dest : TLockGuard);
begin
	TDebugUtils.Msg('TLockGuard.Release');
	Dest.FCriticalSection.Release;
	if Dest.FbOwnCriticalSection then begin
		Dest.FCriticalSection.Free;
	end;
end;

class operator TLockGuard.Initialize(out Dest : TLockGuard);
begin
	TDebugUtils.Msg('TLockGuard.Initialize');
	Dest.FCriticalSection := nil;
	Dest.FbOwnCriticalSection := True;
end;

end.
