unit GX_IdeUtils;

interface

uses
  Windows,
  SysUtils;

function GetIdeRootDirectory: string;

///<summary>
/// Always returns true </summary>
function RunningDelphi8OrGreater: Boolean;

///<summary>
/// Always returns False </summary>
function FileIsWelcomePage(const FileName: string): Boolean;

function GetIdeEdition: string;

implementation

uses
  StrUtils,
  Forms,
  u_dzClassUtils,
  GX_OtaUtils,
  GX_GenericUtils;

function GetIdeRootDirectory: string;
const
  BinDirPostfix = PathDelim + 'Bin';
begin
  if TRegistry_TryReadString(GxOtaGetIdeBaseRegistryKey, 'RootDir', Result, HKEY_LOCAL_MACHINE) then begin
    if DirectoryExists(Result) then begin
      Result := AddSlash(Result);
      Exit; //==>
    end;
  end;

  // There is no entry in the registry or it is invalid -> use the application's exename
  Result := RemoveSlash(ExtractFilePath(Application.ExeName));
  if SameText(RightStr(Result, Length(BinDirPostfix)), BinDirPostfix) then begin
    Result := DeleteRight(Result, Length(BinDirPostfix));
    Result := AddSlash(Result);
  end else
    Result := '';
end;

function RunningDelphi8OrGreater: Boolean;
begin
  Result := True;
end;

function FileIsWelcomePage(const FileName: string): Boolean;
begin
  Result := False;
end;

function GetIdeEdition: string;
begin
  Result := '';
end;

end.
