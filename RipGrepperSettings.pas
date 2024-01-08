unit RipGrepperSettings;

interface

uses
  System.Classes;

type
  TRipGrepperSettings = record
    RipGrepPath: string;
    RipGrepParams: TStrings;
    SearchDirs: TStrings;
    SearchTexts: TStrings;
  public
    class operator Finalize(var Dest: TRipGrepperSettings);
    class operator Initialize(out Dest: TRipGrepperSettings);
  end;

implementation

class operator TRipGrepperSettings.Finalize(var Dest: TRipGrepperSettings);
begin
  Dest.SearchDirs.Free;
  Dest.SearchTexts.Free;
  Dest.RipGrepParams.Free;
end;

class operator TRipGrepperSettings.Initialize(out Dest: TRipGrepperSettings);
begin
  Dest.SearchDirs := TStringList.Create;
  Dest.SearchTexts := TStringList.Create;
  Dest.RipGrepParams := TStringList.Create;
end;

end.
