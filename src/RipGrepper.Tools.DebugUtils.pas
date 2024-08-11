unit RipGrepper.Tools.DebugUtils;

interface

type
	TDebugUtils = class(TObject)
		private
		public
			class procedure DebugMessage(const _s : string);
			class procedure Msg(const _s : string);
			class procedure DebugMessageFormat(const _s : string; const _args : array of const);
			class procedure MsgFmt(const _s : string; const _args : array of const);
	end;

implementation

uses
	Winapi.Windows,
	System.SysUtils;

class procedure TDebugUtils.DebugMessage(const _s : string);
begin
	OutputDebugString(PChar(_s));
end;

class procedure TDebugUtils.Msg(const _s : string);
begin
	DebugMessage(_s);
end;

class procedure TDebugUtils.DebugMessageFormat(const _s : string; const _args : array of const);
begin
	OutputDebugString(PChar(Format(_s, _args)));
end;

class procedure TDebugUtils.MsgFmt(const _s : string; const _args : array of const);
begin
	DebugMessageFormat(_s, _args);
end;

end.
