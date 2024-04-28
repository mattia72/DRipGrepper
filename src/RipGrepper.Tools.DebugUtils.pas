unit RipGrepper.Tools.DebugUtils;

interface

type
  TDebugUtils = class(TObject)
  private
  public
    class procedure DebugMessage(const _s: string);
  end;

implementation

uses
  Winapi.Windows;

class procedure TDebugUtils.DebugMessage(const _s: string);
begin
  OutputDebugString(PChar(_s));
end;

end.
