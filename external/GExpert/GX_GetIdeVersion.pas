unit GX_GetIdeVersion;

// Original Authors: Stefan Hoffmeister and Erik Berry

{$I GX_CondDefine.inc}

interface

uses
  UITypes; // for inlining

type
  TBorlandIdeVersion =
    (ideUndetected, ideUnknown,
     // Delphi
     ideD600, ideD601R, ideD601F, ideD602,
     ideD700, ideD71,
     ideD800, ideD801, ideD802,
     ideD900, ideD901, ideD902, ideD903,
     // BDS
     ideBDS2006,
     // RAD Studio
     ideDelphi2007,
     ideRS2009, ideRS2009U1, ideRS2009U2, ideRS2009U3, ideRS2009U4,
     ideRS2010, ideRS2010U1, ideRS2010U4, ideRS2010U5, // Updates 2/3 were recalled
     ideRSXE1, ideRSXE1U1,
     ideRSXE2,
     ideRSXE3,
     ideRSXE4,
     ideRSXE5,
     ideRSXE6,
     ideRSXE7,
     ideRSXE8,
     ideRS10,
     ideRS101, // Rad Studio 10.1 Berlin
     ideRS101U1, // Rad Studio 10.1 Berlin Update 1
     ideRS101U2, // Rad Studio 10.1 Berlin Update 2
     ideRS102, // Rad Studio 10.2 Tokyo
     ideRS103, // Rad Studio 10.3 Rio
     ideRS103U1, // Rad Studio 10.3 Rio Update 1
     ideRS103U2, // Rad Studio 10.3 Rio Update 2
     ideRS103U3, // Rad Studio 10.3 Rio Update 3
     ideRS104,   // Rad Studio 10.4 Sydney
     ideRS104P2, // Rad Studio 10.4 Sydney with Patch2 installed
     ideRS104U1, // Rad Studio 10.4 Sydney Update 1
     ideRS104U2, // Rad Studio 10.4 Sydney Update 2
     ideRS11,   // Rad Studio 11 Alexandria
     ideRS11U1, // Rad Studio 11 Alexandria Update 1 (aka version 11.1)
     ideRS11U2, // Rad Studio 11 Alexandria Update 2 (aka version 11.2)
     ideRS11U3, // Rad Studio 11 Alexandria Update 3 (aka version 11.3)
     ideRS12,   // Rad Studio 12 Athens
     ideRS12P1, // Rad Studio 12 Athens Patch 1
     ideRS12U1, // Rad Studio 12 Athens Update 1
     // C# Builder
     ideCSB100,
     // C++Builder
     ideBCB600, ideBCB601, ideBCB602, ideBCB604,
     // Kylix
     ideKylix100,
     ideKylix200,
     ideKylix300
     );

// Returns the *exact* version of the product;
//
// Note that the IDE executable and hence the IDE's reported
// version number in the about box may have not been changed.
//
// We err on the safe side, i.e. until we do not
// detect a feature of a higher version, we do
// not increment the version number to something
// higher.

function GetBorlandIdeVersion: TBorlandIdeVersion;
function GetBorlandIdeVersionStr: string;

implementation

uses
  Windows, SysUtils, Dialogs,
  u_dzClassUtils,
  GX_GenericUtils, GX_IdeUtils;

var
  DetectedVersion: TBorlandIdeVersion;

// Result < 0 if V1 < V2
// Result = 0 if V1 = V2
// Result > 0 if V1 > V2
function CompareVersionNumber(const V1, V2: TVersionNumber): Integer;
begin
  Result := V1.Major - V2.Major;
  if Result <> 0 then
    Exit;

  Result := V1.Minor - V2.Minor;
  if Result <> 0 then
    Exit;

  Result := V1.Release - V2.Release;
  if Result <> 0 then
    Exit;

  Result := V1.Build - V2.Build;
end;

{
  Delphi 6.00:

  File            File Version  Size      Modified Time
  delphide60.bpl  6.0.6.142     409,600   Thursday, May 17, 2001, 2:46:06 PM (?)
  vclide60.bpl    6.0.6.142     697,344   Thursday, May 17, 2001, 2:46:22 PM (?)
  designide60.bpl 6.0.6.142     702,976   Thursday, May 17, 2001, 2:46:06 PM (?)
  coreide60.bpl   6.0.6.142     3,074,560 Thursday, May 17, 2001, 2:45:56 PM (?)
  DCC60.DLL       6.0.6.142     867,840   Thursday, May 17, 2001, 2:45:58 PM (?)
  delphi32.exe    6.0.6.142     473,088   Thursday, May 17, 2001, 2:46:06 PM (?)

  Delphi 6.01 (recalled release):

  File            File Version  Size      Modified Time
  delphide60.bpl  6.0.6.189     3,065,344 Thursday, September 06, 2001, 6:01:00 AM
  vclide60.bpl    6.0.6.163     696,832   Tuesday, May 22, 2001, 1:00:00 AM (?)
  designide60.bpl 6.0.6.163     701,440   Tuesday, May 22, 2001, 1:00:00 AM (?)
  coreide60.bpl   6.0.6.189     3,065,344 Thursday, September 06, 2001, 6:01:00 AM
  DCC60.DLL       6.0.6.189     792,064   Thursday, September 06, 2001, 6:01:00 AM
  delphi32.exe    6.0.6.189     472,064   Thursday, September 06, 2001, 6:01:00 AM

  Delphi 6.01 (final release):

  File            File Version  Size      Modified Time
  delphide60.bpl  6.0.6.163(!)  409,088   Tuesday, May 22, 2001, 12:00:00 AM
  vclide60.bpl    6.0.6.163     696,832   Tuesday, May 22, 2001, 12:00:00 AM
  designide60.bpl 6.0.6.163     701,440   Tuesday, May 22, 2001, 12:00:00 AM
  coreide60.bpl   6.0.6.190     3,065,344 Thursday, September 27, 2001, 5:01:00 AM
  DCC60.DLL       6.0.6.190     792,064   Thursday, September 27, 2001, 5:01:00 AM
  delphi32.exe    6.0.6.190     472,064   Thursday, September 27, 2001, 5:01:00 AM

  Delphi 6.02:
  File            File Version  Size      Modified Time
  delphide60.bpl  6.0.6.240     409,600   Friday, February 15, 2002, 2:02:00 PM
  vclide60.bpl    6.0.6.163     696,832   Tuesday, May 22, 2001, 12:00:00 AM
  designide60.bpl 6.0.6.163     701,440   Tuesday, May 22, 2001, 12:00:00 AM
  coreide60.bpl   6.0.6.240     3,065,344 Friday, February 15, 2002, 2:02:00 PM
  DCC60.DLL       6.0.6.240     707,584   Friday, February 15, 2002, 2:02:00 PM
  delphi32.exe    6.0.6.240     472,064   Friday, February 15, 2002, 2:02:00 PM
}

function GetDelphi6IdeVersion: TBorlandIdeVersion;
const
  CoreIdeD600: TVersionNumber =
    (Minor: 0; Major: 6; Build: 142; Release: 6);
  CoreIdeD601R: TVersionNumber =
    (Minor: 0; Major: 6; Build: 189; Release: 6);
  CoreIdeD601F: TVersionNumber =
    (Minor: 0; Major: 6; Build: 190; Release: 6);
var
  ReadFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideD600;
  ReadFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide60.bpl');
  VersionNumber := CompareVersionNumber(ReadFileVersion, CoreIdeD600);
  if VersionNumber > 0 then
    Result := ideD601R;
  VersionNumber := CompareVersionNumber(ReadFileVersion, CoreIdeD601R);
  if VersionNumber > 0 then
    Result := ideD601F;
  VersionNumber := CompareVersionNumber(ReadFileVersion, CoreIdeD601F);
  if VersionNumber > 0 then
    Result := ideD602;
end;

{
  C++Builder 6.00:

  File            File Version  Size      Modified Time
  designide60.bpl 6.0.10.157    706,560   Saturday, February 02, 2002, 1:00:00 AM
  coreide60.bpl   6.0.10.157    3,158,016 Saturday, February 02, 2002, 1:00:00 AM
  bcb.exe         6.0.10.157    732,672   Friday, February 01, 2002, 2:00:00 PM
  bcc32.exe       5.6.0.0       1,394,688 Saturday, February 02, 2002, 1:00:00 AM
  dcc60.dll       6.0.10.157    797,696   Saturday, February 02, 2002, 1:00:00 AM
  bcbide60.bpl    6.0.10.157    974,848   Saturday, February 02, 2002, 12:00:00 AM

  C++Builder 6.01:

  File            File Version  Size      Modified Time
  designide60.bpl 6.0.10.157   706,560 Saturday, February 02, 2002, 1:00:00 AM
  dcc60.dll       6.0.10.157   797,696 Saturday, February 02, 2002, 1:00:00 AM
  bcb.exe         6.0.10.161   732,672 Friday, February 01, 2002, 2:00:00 PM
  coreide60.bpl   6.0.10.157 3,158,016 Saturday, February 02, 2002, 1:00:00 AM
  bcc32.exe       5.6.0.0    1,394,688 Saturday, February 02, 2002, 1:00:00 AM
  bcbide60.bpl    6.0.10.161   974,848 Friday, February 01, 2002, 2:00:00 PM

  C++Builder 6.02:

  File            File Version  Size      Modified Time
  designide60.bpl 6.0.10.157   706,560 Saturday, February 02, 2002, 1:00:00 AM
  dcc60.dll       6.0.10.157   797,696 Saturday, February 02, 2002, 1:00:00 AM
  bcb.exe         6.0.10.165   732,672 Thursday, July 11, 2002, 6:02:00 AM
  coreide60.bpl   6.0.10.165 3,158,016 Thursday, July 11, 2002, 6:02:00 AM
  bcc32.exe       5.6.1.0    1,397,760 Thursday, July 11, 2002, 6:02:00 AM
  bcbide60.bpl    6.0.10.165   974,848 Thursday, July 11, 2002, 6:02:00 AM

  (C++Builder 6.03 was a semi-private beta release)

  C++Builder 6.04:

  File            File Version  Size      Modified Time
  designide60.bpl 6.0.10.166   706,560 Thursday, January 30, 2003, 6:04:00 AM
  dcc60.dll       6.0.10.155   797,696 Wednesday, January 30, 2002, 5:38:44 PM
  bcb.exe         6.0.10.166   732,672 Thursday, January 30, 2003, 6:04:00 AM
  coreide60.bpl   6.0.10.166 3,158,016 Thursday, January 30, 2003, 6:04:00 AM
  bcc32.exe       5.6.4.0    1,398,272 Thursday, January 30, 2003, 6:04:00 AM
  bcbide60.bpl    6.0.10.166   975,872 Thursday, January 30, 2003, 6:04:00 AM
}

function GetCppBuilder6IdeVersion: TBorlandIdeVersion;
const
  BcbExe600: TVersionNumber =
    (Minor: 0; Major: 6; Build: 157; Release: 10);
  BcbExe601: TVersionNumber =
    (Minor: 0; Major: 6; Build: 161; Release: 10);
  BcbExe602: TVersionNumber =
    (Minor: 0; Major: 6; Build: 165; Release: 10);
var
  ReadFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideBCB600;
  ReadFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\bcb.exe');
  VersionNumber := CompareVersionNumber(ReadFileVersion, BcbExe600);
  if VersionNumber > 0 then
    Result := ideBCB601;
  VersionNumber := CompareVersionNumber(ReadFileVersion, BcbExe601);
  if VersionNumber > 0 then
    Result := ideBCB602;
  VersionNumber := CompareVersionNumber(ReadFileVersion, BcbExe602);
  if VersionNumber > 0 then
    Result := ideBCB604;
end;

{
  Delphi 7.00:

  File            File Version  Size      Modified Time
  delphide70.bpl  7.0.4.453    473,088    Friday, August 09, 2002, 8:00:00 AM
  vclide70.bpl    7.0.4.453    708,608    Friday, August 09, 2002, 8:00:00 AM
  designide70.bpl 7.0.4.453    625,152    Friday, August 09, 2002, 8:00:00 AM
  coreide70.bpl   7.0.4.453  3,180,544    Friday, August 09, 2002, 8:00:00 AM
  DCC70.DLL       7.0.4.453    841,216    Friday, August 09, 2002, 8:00:00 AM
  delphi32.exe    7.0.4.453    545,792    Friday, August 09, 2002, 1:00:00 PM

  Delphi 7.1:

  File            File Version  Size      Modified Time
  delphide70.bpl  7.0.4.453    473,088    Friday, August 09, 2002, 9:00:00 AM
  vclide70.bpl    7.0.4.453    708,608    Friday, August 09, 2002, 9:00:00 AM
  designide70.bpl 7.0.8.1      625,664    Friday, April 23, 2004, 9:01:00 AM
  coreide70.bpl   7.0.8.1    3,186,688    Friday, April 23, 2004, 9:01:00 AM
  DCC70.DLL       7.0.8.1      843,264    Friday, April 23, 2004, 9:01:00 AM
  delphi32.exe    7.0.8.1      545,792    Friday, April 23, 2004, 9:01:00 AM
}
function GetDelphi7IdeVersion: TBorlandIdeVersion;
const
  CoreIDE700: TVersionNumber =
    (Minor: 0; Major: 7; Build: 453; Release: 4);
var
  ReadFileVersion: TVersionNumber;
  VersionNumber: Integer;
  CoreIdeFile: string;
begin
  Result := ideD700;
  CoreIdeFile := GetIdeRootDirectory + 'Bin\coreide70.bpl';
  if FileExists(CoreIdeFile) then begin
    ReadFileVersion := GetFileVersionNumber(CoreIdeFile);
    VersionNumber := CompareVersionNumber(ReadFileVersion, CoreIDE700);
    if VersionNumber > 0 then
      Result := ideD71;
  end;
end;

function GetCSharpBuilder1Version: TBorlandIdeVersion;
begin
  Result := ideCSB100;
end;

{
  Delphi 8.00:

  File                 File Version   Size       Modified Time
  delphicoreide71.bpl 7.1.1446.610   1,228,800  Wednesday, December 17, 2003, 10:00:00 AM
  vclide71.bpl        7.1.1446.610     921,088  Wednesday, December 17, 2003, 10:00:00 AM
  designide71.bpl     7.1.1446.610     677,376  Wednesday, December 17, 2003, 10:00:00 AM
  coreide71.bpl       7.1.1446.610   2,793,984  Wednesday, December 17, 2003, 10:00:00 AM
  DCC71.DLL           7.1.1446.610     922,112  Wednesday, December 17, 2003, 10:00:00 AM
  bds.exe             7.1.1446.610   1,092,608  Wednesday, December 17, 2003, 10:00:00 AM
  Studio.Host.dll     7.1.1446.610     684,032  Wednesday, December 17, 2003, 10:00:00 AM
  bordbk71.dll        50.4.228.1       718,848  Wednesday, December 17, 2003, 10:00:00 AM

  Delphi 8.01:
  delphicoreide71.bpl
  vclide71.bpl
  designide71.bpl
  coreide71.bpl
  DCC71.DLL           7.1.1490.25464
  bds.exe
  Studio.Host.dll
  bordbk71.dll        50.4.228.2

  Delphi 8.02:
  delphicoreide71.bpl 7.1.1523.17956 1,231,360  Friday, February 27, 2004, 10:02:00 AM
  vclide71.bpl        7.1.1523.17956   921,600  Friday, February 27, 2004, 10:02:00 AM
  designide71.bpl     7.1.1523.17956   678,912  Friday, February 27, 2004, 10:02:00 AM
  coreide71.bpl       7.1.1523.17956 2,797,568  Friday, February 27, 2004, 10:02:00 AM
  DCC71.DLL           7.1.1523.17956   923,136  Friday, February 27, 2004, 10:02:00 AM
  bds.exe             7.1.1523.17956 1,092,608  Friday, February 27, 2004, 10:02:00 AM
  B.Studio.Host.dll   7.1.1523.17956   688,128  Friday, February 27, 2004, 10:02:00 AM
}
function GetDelphi8Version: TBorlandIdeVersion;
const
  Dcc800: TVersionNumber =
    (Minor: 1; Major: 7; Build: 610; Release: 1446);
  BdsExe800: TVersionNumber =
    (Minor: 1; Major: 7; Build: 610; Release: 1446);
var
  DccFileVersion: TVersionNumber;
  BdsFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideD800;
  DccFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\DCC71.DLL');
  VersionNumber := CompareVersionNumber(DccFileVersion, Dcc800);
  if VersionNumber > 0 then begin
    Result := ideD801;
    BdsFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\bds.exe');
    VersionNumber := CompareVersionNumber(BdsFileVersion, BdsExe800);
    if VersionNumber > 0 then
      Result := ideD802;
  end;
end;

{
  Delphi 2005:

  File                 File Version   Size       Modified Time
  delphicoreide9.bpl   9.0.1761.24408 2,891,264  Friday, October 22, 2004, 10:00:00 AM
  vclide9.bpl          9.0.1761.24408 1,255,936  Friday, October 22, 2004, 10:00:00 AM
  designide9.bpl       9.0.1761.24408 749,056    Friday, October 22, 2004, 10:00:00 AM
  coreide9.bpl         9.0.1761.24408 3,367,936  Friday, October 22, 2004, 10:00:00 AM
  DCC90.DLL            9.0.1761.24408 987,136    Friday, October 22, 2004, 10:00:00 AM
  bds.exe              9.0.1761.24408 916,992    Friday, October 22, 2004, 10:00:00 AM
  B.Studio.Host.dll    9.0.1761.24408 688,128    Friday, October 22, 2004, 10:00:00 AM
  bordbk9.dll          90.1.1.1       779,264    Friday, October 22, 2004, 10:00:00 AM

  Delphi 2005 Update 1:

  File                 File Version   Size       Modified Time
  delphicoreide9.bpl   9.0.1810.11291 2,891,776  Thursday, December 09, 2004, 9:01:00 AM
  vclide9.bpl          9.0.1761.24408 1,255,936  Friday, October 22, 2004, 10:00:00 AM
  designide9.bpl       9.0.1810.11291 749,568    Thursday, December 09, 2004, 9:01:00 AM
  coreide9.bpl         9.0.1810.11291 3,369,472  Thursday, December 09, 2004, 9:01:00 AM
  DCC90.DLL            9.0.1810.11291 988,672    Thursday, December 09, 2004, 9:01:00 AM
  bds.exe              9.0.1761.24408 916,992    Thursday, December 09, 2004, 9:01:00 AM
  B.Studio.Host.dll    9.0.1810.11291 688,128    Thursday, December 09, 2004, 9:01:00 AM
  bordbk9.dll          90.1.2.1       782,336    Thursday, December 09, 2004, 9:01:00 AM

  We do not have the version numbers for Delphi 2005 Update 2, so we assume that
  the version numbers are > those of Update 1 and < those of Update 3.

  Delphi 2005 Update 3:

  File                 File Version   Size       Modified Time
  delphicoreide9.bpl   9.0.1882.30496 2,899,968  Friday, March 04, 2005, 12:02:00 PM
  vclide9.bpl          9.0.1882.30496 1,255,936  Friday, March 04, 2005, 12:02:00 PM
  designide9.bpl       9.0.1935.22056 750,080    Tuesday, April 19, 2005, 11:03:00 AM
  coreide9.bpl         9.0.1935.22056 3,372,544  Tuesday, April 19, 2005, 11:03:00 AM
  DCC90.DLL            9.0.1882.30496 989,184    Friday, March 04, 2005, 12:02:00 PM
  bds.exe              9.0.1935.22056 917,504    Tuesday, April 19, 2005, 11:03:00 AM
  B.Studio.Host.dll    9.0.1882.30496 688,128    Friday, March 04, 2005, 12:02:00 PM
  bordbk9.dll          90.1.3.1       784,896    Friday, March 04, 2005, 12:02:00 PM
}
function GetDelphi9Version: TBorlandIdeVersion;
const
  CoreIde900: TVersionNumber =
    (Minor: 0; Major: 9; Build: 24408; Release: 1761);
  CoreIde901: TVersionNumber =
    (Minor: 0; Major: 9; Build: 11291; Release: 1810);
  CoreIde903: TVersionNumber =
    (Minor: 0; Major: 9; Build: 22056; Release: 1935);
var
  CoreIdeFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideD900;
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide90.bpl');
  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde900);
  if VersionNumber > 0 then
  begin
    Result := ideD901;
    VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde901);
    if VersionNumber > 0 then
    begin
      Result := ideD902;
      VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde903);
      if VersionNumber >= 0 then
        Result := ideD903;
    end;
  end;
end;

{
  BDS 2006:

  todo: get at least the version numbers of Delphi 2006 from my installation

  File                 File Version   Size       Modified Time
  delphicoreide100.bpl
  vclide100.bpl
  designide100.bpl
  coreide100.bpl
  DCC100.DLL
  bds.exe
  B.Studio.Host.dll
  bordbk10.dll
}
function GetBDS2006Version: TBorlandIdeVersion;
const
  CoreIde1000: TVersionNumber =
    (Minor: 0; Major: 0; Build: 0; Release: 0);
var
  CoreIdeFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideBDS2006;
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide100.bpl');
  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde1000);
  if VersionNumber > 0 then begin
    //Result := ideD1001;
  end;
end;

{
  Delphi 2007:

  File                 File Version   Size       Modified Time
  delphicoreide110.bpl
  vclide110.bpl
  designide110.bpl
  coreide110.bpl
  DCC110.DLL
  bds.exe
  B.Studio.Host.dll
  bordbk11.dll
}
function GetDelphi2007Version: TBorlandIdeVersion;
const
  CoreIde1000: TVersionNumber =
    (Minor: 0; Major: 0; Build: 0; Release: 0);
var
  CoreIdeFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideDelphi2007;
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide100.bpl');
  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde1000);
  if VersionNumber > 0 then begin
    //Result := ideD1001;
  end;
end;

{
  Delphi 2009:
  File                 File Version    Size       Modified Time
  delphicoreide120.bpl 12.0.3170.16989 3,099,136  Friday, August 29, 2008, 2:00:00 PM
  coreide120.bpl       12.0.3170.16989 4,571,136  Friday, August 29, 2008, 2:00:00 PM
  bds.exe              12.0.3170.16989 983,552    Friday, August 29, 2008, 2:00:00 PM
  dcldb120.bpl         12.0.3170.16989 286,720    Friday, August 29, 2008, 2:00:00 PM

  Delphi 2009 Update 1:
  File                 File Version    Size       Modified Time
  delphicoreide120.bpl 12.0.3210.17555 3,099,136  Wednesday, October 01, 2008, 2:01:00 PM
  coreide120.bpl       12.0.3210.17555 4,571,136  Wednesday, October 01, 2008, 2:01:00 PM
  bds.exe              12.0.3210.17555 983,552    Wednesday, October 01, 2008, 2:01:00 PM
  dcldb120.bpl         12.0.3210.17555 286,720    Wednesday, October 01, 2008, 2:01:00 PM

  Delphi 2009 Update 2 (database only):
  File                 File Version    Size       Modified Time
  delphicoreide120.bpl 12.0.3210.17555 3,099,136  Wednesday, October 01, 2008, 2:01:00 PM
  coreide120.bpl       12.0.3210.17555 4,571,136  Wednesday, October 01, 2008, 2:01:00 PM
  bds.exe              12.0.3210.17555 983,552    Wednesday, October 01, 2008, 2:01:00 PM
  dcldb120.bpl         12.0.3210.17555 286,720    Wednesday, October 01, 2008, 2:01:00 PM
  dcldbx120.bpl        12.0.3250.18309 154,624    Wednesday, November 12, 2008, 3:02:00 PM
  DataExplorer120.bpl  12.0.3250.18309 181,248    Wednesday, November 12, 2008, 3:02:00 PM

  Delphi 2009 Update 3:
  File                 File Version    Size       Modified Time
  delphicoreide120.bpl 12.0.3420.21218 3,102,720  Wednesday, January 14, 2009, 2:03:00 PM
  coreide120.bpl       12.0.3420.21218 4,727,048  Wednesday, January 14, 2009, 2:03:00 PM
  bds.exe              12.0.3420.21218 3,704,128  Thursday, May 14, 2009, 2:06:42 AM
  dcldb120.bpl         12.0.3420.21218 286,720    Wednesday, January 14, 2009, 2:03:00 PM
  dcldbx120.bpl        12.0.3250.18309 154,624    Wednesday, November 12, 2008, 3:02:00 PM
  DataExplorer120.bpl  12.0.3250.18309 181,248    Wednesday, November 12, 2008, 3:02:00 PM

  Delphi 2009 Update 4 (database only):
  File                 File Version    Size       Modified Time
  delphicoreide120.bpl 12.0.3420.21218 3,102,720  Wednesday, January 14, 2009, 2:03:00 PM
  coreide120.bpl       12.0.3420.21218 4,727,048  Wednesday, January 14, 2009, 2:03:00 PM
  bds.exe              12.0.3420.21218 3,704,128  Thursday, May 14, 2009, 2:06:42 AM
  dcldb120.bpl         12.0.3420.21218 286,720    Wednesday, January 14, 2009, 2:03:00 PM
  dcldbx120.bpl        12.0.3420.21218 154,624    Wednesday, January 14, 2009, 3:04:00 PM
  DataExplorer120.bpl  12.0.3420.21218 181,248    Wednesday, January 14, 2009, 3:04:00 PM
}
function GetRS2009Version: TBorlandIdeVersion;
const
  CoreIde1200: TVersionNumber =
    (Minor: 0; Major: 12; Build: 16989; Release: 3170);
  CoreIde1201: TVersionNumber =
    (Minor: 0; Major: 12; Build: 17555; Release: 3210);
  DclDbx1200: TVersionNumber =
    (Minor: 0; Major: 12; Build: 16989; Release: 3170);
  DclDbx1202: TVersionNumber =
    (Minor: 0; Major: 12; Build: 18309; Release: 3250);
var
  CoreIdeFileVersion: TVersionNumber;
  DclDbxFileVersion: TVersionNumber;
  VersionNumber: Integer;
  DclDbxFile: string;
begin
  Result := ideRS2009;
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide120.bpl');
  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde1200);
  if VersionNumber > 0 then begin
    Result := ideRS2009U1;
    DclDbxFile := GetIdeRootDirectory + 'Bin\dcldbx120.bpl';
    if FileExists(DclDbxFile) then
    begin
      DclDbxFileVersion := GetFileVersionNumber(DclDbxFile);
      VersionNumber := CompareVersionNumber(DclDbxFileVersion, DclDbx1200);
      if VersionNumber > 0 then
        Result := ideRS2009U2;
    end;
    VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde1201);
    if VersionNumber > 0 then
      Result := ideRS2009U3;
    if FileExists(DclDbxFile) then
    begin
      VersionNumber := CompareVersionNumber(DclDbxFileVersion, DclDbx1202);
      if VersionNumber > 0 then
        Result := ideRS2009U4;
    end;
  end;
end;


{
  Delphi 2010:
  File                 File Version    Size       Modified Time
  delphicoreide140.bpl 14.0.3513.24210 3,162,112  Wednesday, August 19, 2009, 3:00:00 PM
  coreide140.bpl       14.0.3513.24210 6,801,936  Wednesday, August 19, 2009, 3:00:00 PM
  bds.exe              14.0.3513.24210 4,267,312  Wednesday, August 19, 2009, 3:00:00 PM
  dcldb140.bpl         14.0.3513.24210   312,320  Wednesday, August 19, 2009, 3:00:00 PM
  sanctuarylib.dll     8.1.10.0        2,034,096  Wednesday, August 19, 2009, 3:00:00 PM
  DCC140.dll           14.0.3513.24210 1,456,128  Wednesday, August 19, 2009, 3:00:00 PM

  Delphi 2010 U1 (updated files only, licensing changes only):
  bds.exe              14.0.3539.24502 5,051,184  Wednesday, September 09, 2009, 3:01:00 PM
  coreide140.bpl       14.0.3539.24502 6,801,936  Wednesday, September 09, 2009, 3:01:00 PM
  sanctuarylib.dll     8.1.16.1        2,040,592  Wednesday, September 09, 2009, 3:01:00 PM

  Delphi 2010 U2 and U3 were recalled soon after release

  Delphi 2010 Update 4:
  delphicoreide140.bpl 14.0.3593.25826 3,165,696  Monday, November 02, 2009, 4:02:00 PM
  coreide140.bpl       14.0.3615.26342 6,888,544  Monday, November 23, 2009, 4:04:00 PM
  bds.exe              14.0.3615.26342 6,019,888  Monday, November 23, 2009, 2:04:00 PM
  dcldb140.bpl         14.0.3593.25826   312,320  Monday, November 02, 2009, 4:02:00 PM
  sanctuarylib.dll     8.2.15.0        2,347,480  Monday, November 23, 2009, 2:04:00 PM
  DCC140.dll           14.0.3593.25826 1,437,696  Monday, November 02, 2009, 4:02:00 PM
  dbx140.bpl           14.0.3593.25826   742,400  Monday, November 02, 2009, 4:02:00 PM
  dcldbx140.bpl        14.0.3513.24210   201,728  Saturday, August 01, 2009, 3:00:00 PM

  Delphi 2010 Update 5 (updated files only):
  dcldbx140.bpl        14.0.3615.26342   201,728  Wednesday, November 18, 2009, 4:05:00 PM
}
function GetRS2010Version: TBorlandIdeVersion;
const
  CoreIde1400: TVersionNumber = (Minor: 0; Major: 14; Build: 24210; Release: 3513);
  CoreIde1401: TVersionNumber = (Minor: 0; Major: 14; Build: 24502; Release: 3539);
  CoreIde1404: TVersionNumber = (Minor: 0; Major: 14; Build: 26342; Release: 3615);
  CoreIde1405: TVersionNumber = (Minor: 0; Major: 14; Build: 26342; Release: 3615);
  DclDbx1404 : TVersionNumber = (Minor: 0; Major: 14; Build: 24210; Release: 3513);
var
  CoreIdeFileVersion: TVersionNumber;
  DclDbxFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideRS2010;
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide140.bpl');
  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde1400);
  if VersionNumber > 0 then begin
    Result := ideRS2010U1;
    VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde1401);
    if VersionNumber > 0 then begin
      Result := ideRS2010U4;
      if FileExists(GetIdeRootDirectory + 'Bin\dcldbx140.bpl') then begin
        DclDbxFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\dcldbx140.bpl');
        VersionNumber := CompareVersionNumber(DclDbxFileVersion, DclDbx1404);
        if VersionNumber > 0 then begin
          Result := ideRS2010U5;
        end;
      end;
    end;
  end;
end;

{
  Delphi XE 1:
  File                 File Version    Size       Modified Time
  delphicoreide150.bpl 15.0.3890.34076 3,312,640  Friday, August 27, 2010, 2:55:00 PM
  coreide150.bpl       15.0.3890.34076 9,258,888  Friday, August 27, 2010, 2:55:00 PM
  bds.exe              15.0.3890.34076 7,089,048  Friday, August 27, 2010, 2:55:00 PM
  dcldb150.bpl         15.0.3890.34076   311,808  Friday, August 27, 2010, 2:55:00 PM

  Delphi XE 1 Update 1:
  File                 File Version      Size       Modified Time
  delphicoreide150.bpl 15.0.3953.35171  3,304,960  Wednesday, November 03, 2010, 4:55:00 PM
  coreide150.bpl       15.0.3953.35171  9,265,152  Wednesday, November 03, 2010, 4:55:00 PM
  bds.exe              15.0.3953.35171 10,436,504  Wednesday, November 03, 2010, 4:55:00 PM
  dcldb150.bpl         15.0.3953.35171    311,808  Wednesday, November 03, 2010, 4:55:00 PM
}
function GetRSXE1Version: TBorlandIdeVersion;
const
  CoreIde1500: TVersionNumber = (Minor: 0; Major: 15; Build: 34076; Release: 3890);
  CoreIde1501: TVersionNumber = (Minor: 0; Major: 15; Build: 35171; Release: 3953);
var
  CoreIdeFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideRSXE1;
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide150.bpl');
  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde1500);
  if VersionNumber > 0 then begin
    Result := ideRSXE1U1;
  end;
end;

{
  Delphi XE 2:
  File                 File Version    Size       Modified Time
  delphicoreide160.bpl
  coreide160.bpl
  bds.exe
  dcldb160.bpl
}
function GetRSXE2Version: TBorlandIdeVersion;
const
  CoreIde1600: TVersionNumber = (Minor: 0; Major: 16; Build: 0; Release: 0);
  CoreIde1601: TVersionNumber = (Minor: 0; Major: 16; Build: 0; Release: 0);
var
  CoreIdeFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideRSXE2;
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide160.bpl');
  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde1600);
  if VersionNumber > 0 then begin
    //Result := ideRSXE2U1;
  end;
end;

{
  Delphi XE 3:
  File                 File Version    Size       Modified Time
  delphicoreide170.bpl
  coreide170.bpl
  bds.exe
  dcldb170.bpl
}
function GetRSXE3Version: TBorlandIdeVersion;
const
  CoreIde1700: TVersionNumber = (Minor: 0; Major: 17; Build: 0; Release: 0);
var
  CoreIdeFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideRSXE3;
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide170.bpl');
  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde1700);
  if VersionNumber > 0 then begin
    //Result := ideRSXE3U1;
  end;
end;

{
  Delphi XE 4:
  File                 File Version    Size       Modified Time
  delphicoreide180.bpl
  coreide180.bpl
  bds.exe
  dcldb180.bpl
}
function GetRSXE4Version: TBorlandIdeVersion;
const
  CoreIde1800: TVersionNumber = (Minor: 0; Major: 18; Build: 0; Release: 0);
var
  CoreIdeFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideRSXE4;
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide180.bpl');
  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde1800);
  if VersionNumber > 0 then begin
    //Result := ideRSXE4U1;
  end;
end;

{
  Delphi XE 5 Update 2:
  File                 File Version    Size       Modified Time
  delphicoreide190.bpl
  coreide190.bpl       19.0.14356.6604 13,070,712 Saturday, December 07, 2013, 5:55:00 PM
  bds.exe
  dcldb190.bpl
}
function GetRSXE5Version: TBorlandIdeVersion;
const
  CoreIde1900: TVersionNumber = (Minor: 0; Major: 19; Build: 0; Release: 0);
var
  CoreIdeFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideRSXE5;
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide190.bpl');
  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde1900);
  if VersionNumber > 0 then begin
    //Result := ideRSXE5U1;
  end;
end;

{
  Delphi XE 6:
  File                 File Version    Size       Modified Time
  delphicoreide200.bpl 20.0.15596.9843 3,947,384  Friday, April 11, ?2014, 5:55:00 PM
  coreide200.bpl       20.0.15596.9843 13,121,912 Friday, April 11, ?2014, 5:55:00 PM
  bds.exe              20.0.15596.9843 12,455,800 Friday, April 11, ?2014, 5:55:00 PM
  dcldb200.bpl         20.0.15596.9843 445,304    Friday, April 11, ?2014, 5:55:00 PM
}
function GetRSXE6Version: TBorlandIdeVersion;
const
  CoreIde2000: TVersionNumber = (Minor: 0; Major: 20; Build: 9843; Release: 15596);
var
  CoreIdeFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideRSXE6;
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide200.bpl');
  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde2000);
  if VersionNumber > 0 then begin
    //Result := ideRSXE6U1;
  end;
end;

{
  Delphi XE 7:
  File                 File Version    Size       Modified Time
  delphicoreide210.bpl
  coreide210.bpl
  bds.exe
  dcldb210.bpl
}
function GetRSXE7Version: TBorlandIdeVersion;
const
  CoreIde2100: TVersionNumber = (Minor: 0; Major: 0; Build: 0; Release: 0);
var
  CoreIdeFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideRSXE7;
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide210.bpl');
  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde2100);
  if VersionNumber > 0 then begin
    //Result := ideRSXE7U1;
  end;
end;

{
  Delphi XE 8:
  File                 File Version    Size       Modified Time
  delphicoreide220.bpl
  coreide220.bpl
  bds.exe
  dcldb220.bpl
}
function GetRSXE8Version: TBorlandIdeVersion;
const
  CoreIde2200: TVersionNumber = (Minor: 0; Major: 0; Build: 0; Release: 0);
var
  CoreIdeFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideRSXE8;
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide220.bpl');
  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde2200);
  if VersionNumber > 0 then begin
    //Result := ideRSXE8U1;
  end;
end;

{
  Delphi 10 Seattle
  File                 File Version    Size       Modified Time
  delphicoreide230.bpl
  coreide230.bpl
  bds.exe
  dcldb230.bpl
}
function GetRS10Version: TBorlandIdeVersion;
const
  CoreIde2300: TVersionNumber = (Minor: 0; Major: 0; Build: 0; Release: 0);
var
  CoreIdeFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideRS10;
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide230.bpl');
  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde2300);
  if VersionNumber > 0 then begin
    //Result := ideRS10U1;
  end;
end;

{
  Delphi 10.1 Berlin
  File                 File Version    Size       Modified Time
  delphicoreide240.bpl
  coreide240.bpl       24.0.22858.6822
  bds.exe
  dcldb240.bpl

  Delphi 10.1 Berlin Update 1
  File                 File Version    Size       Modified Time
  delphicoreide240.bpl
  coreide240.bpl       24.0.24468.8770
  bds.exe
  dcldb240.bpl

  Delphi 10.1 Berlin Update 2
  File                 File Version    Size       Modified Time
  delphicoreide240.bpl 24.0.25048.9432
  coreide240.bpl       24.0.25048.9432
  bds.exe              24.0.25048.9432
  dcldb240.bpl         24.0.25048.9432
}
function GetRS101Version: TBorlandIdeVersion;
const
  CoreIde2400: TVersionNumber = (Minor: 0; Major: 24; Build: 6822; Release: 22858);
  CoreIde2400Upd1: TVersionNumber = (Minor: 0; Major: 24; Build: 8770; Release: 24468);
  CoreIde2400Upd2: TVersionNumber = (Minor: 0; Major: 24; Build: 9432; Release: 25048);
var
  CoreIdeFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide240.bpl');
  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde2400Upd2);
  if VersionNumber >= 0 then begin
    Result := ideRS101U2;
  end else begin
    VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde2400Upd1);
    if VersionNumber >= 0 then begin
      Result := ideRS101U1;
    end else
      Result := ideRS101;
  end;
end;

{
  Delphi 10.2 Tokyo
  File                 File Version    Size       Modified Time
  delphicoreide250.bpl 25.0.26309.314
  coreide250.bpl       25.0.26309.314
  bds.exe              25.0.26309.314
  dcldb250.bpl         25.0.26309.314
}
function GetRS102Version: TBorlandIdeVersion;
const
  CoreIde2500: TVersionNumber = (Minor: 0; Major: 25; Build: 314; Release: 26309);
begin
  Result := ideRS102;
end;

{
  Delphi 10.3 Rio
  File                 File Version    Size       Modified Time
  delphicoreide260.bpl 26.0.32429.4364
  coreide260.bpl       26.0.32429.4364
  bds.exe              26.0.32429.4364
  dcldb260.bpl         26.0.32429.4364

  Delphi 10.3.1 Rio Update 1
  File                 File Version    Size       Modified Time
  delphicoreide260.bpl 26.0.33219.4899
  coreide260.bpl       26.0.33219.4899
  bds.exe              26.0.33219.4899
  dcldb260.bpl         26.0.33219.4899

  Delphi 10.3.2 Rio Update 2
  File                 File Version    Size       Modified Time
  delphicoreide260.bpl 26.0.34749.6593
  coreide260.bpl       26.0.34749.6593
  bds.exe              26.0.34749.6593
  dcldb260.bpl         26.0.34749.6593

  Delphi 10.3.3 Rio Update 3
  File                 File Version    Size       Modified Time
  delphicoreide260.bpl 26.0.36039.7899
  coreide260.bpl       26.0.36039.7899
  bds.exe              26.0.36039.7899
  dcldb260.bpl         26.0.36039.7899
}
function GetRS103Version: TBorlandIdeVersion;
const
  CoreIde2600: TVersionNumber = (Minor: 0; Major: 26; Build: 4364; Release: 32429);
  CoreIde2600Upd1: TVersionNumber = (Minor: 0; Major: 26; Build: 4899; Release: 33219);
  CoreIde2600Upd2: TVersionNumber = (Minor: 0; Major: 26; Build: 6593; Release: 34749);
  CoreIde2600Upd3: TVersionNumber = (Minor: 0; Major: 26; Build: 7899; Release: 36039);
var
  CoreIdeFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide260.bpl');

  VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde2600Upd3);
  if VersionNumber >= 0 then begin
    Result := ideRS103U3;
  end else begin
    VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde2600Upd2);
    if VersionNumber >= 0 then begin
      Result := ideRS103U2;
    end else begin
      VersionNumber := CompareVersionNumber(CoreIdeFileVersion, CoreIde2600Upd1);
      if VersionNumber >= 0 then begin
        Result := ideRS103U1;
      end else
        Result := ideRS103;
    end;
  end;
end;

{
  Delphi 10.4 Sydney
  File                 File Version
  delphicoreide270.bpl 27.0.37889.9797
  coreide270.bpl       27.0.37889.9797
  bds.exe              27.0.37889.9797
  dcldb270.bpl         27.0.37889.9797

  Patch1 and Patch2 apparently did not change any of these versions and even those files
  the patches replaced still have the same version as the original files. WTF?

  Delphi 10.4.1 Sydney Update 1
  File                 File Version
  delphicoreide270.bpl 27.0.38860.1461
  coreide270.bpl       27.0.38860.1461
  bds.exe              27.0.38860.1461
  dcldb270.bpl         27.0.38860.1461

  Delphi 10.4.2 Sydney Update 2
  File                 File Version
  delphicoreide270.bpl 27.0.40680.4203
  coreide270.bpl       27.0.40680.4203
  bds.exe              27.0.40680.4203
  dcldb270.bpl         27.0.40680.4203

  Delphi 10.4.2 Sydney Update 2 + Patches released on 2021-05-01 (we currently don't use this this)
  File                 File Version
  delphicoreide270.bpl 27.0.41310.5003 <-- Only this one was changed
  coreide270.bpl       27.0.40680.4203
  bds.exe              27.0.40680.4203
  dcldb270.bpl         27.0.40680.4203

}

function GetRS104Version: TBorlandIdeVersion;
const
  RegKey = 'Software\Embarcadero\BDS\21.0\CatalogRepository\Elements\10.4Patch2pro-10';
  CoreIde2700:     TVersionNumber = (Minor: 0; Major: 27; Build: 9797; Release:  37829);
  CoreIde2700UPd1: TVersionNumber = (Minor: 0; Major: 27; Build: 1461; Release:  38860);
  CoreIde2700UPd2: TVersionNumber = (Minor: 0; Major: 27; Build: 4203; Release:  40680);
var
  RegValue: Integer;
  CoreIdeFileVersion: TVersionNumber;
begin
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide270.bpl');

  if CompareVersionNumber(CoreIdeFileVersion, CoreIde2700UPd2) >= 0 then begin
    Result := ideRS104U2;
  end else if CompareVersionNumber(CoreIdeFileVersion, CoreIde2700UPd1) >= 0 then begin
    Result := ideRS104U1;
  end else begin
    // There was patch 2 for 10.4.0 which broke StringGrids in a new way
    // so we have to detect it.
    RegValue := TRegistry_ReadInteger(RegKey, 'Installed', 0, HKEY_CURRENT_USER);
    if RegValue <> 0 then
      Result := ideRS104P2
    else
      Result := ideRS104;
  end;
end;

{
  Delphi 11 Alexandria
  File                 File Version
  delphicoreide280.bpl 28.0.42600.6491
  coreide280.bpl       28.0.42600.6491
  bds.exe              28.0.42600.6491
  dcldb280.bpl         28.0.42600.6491

  Delphi 11.1 (Update 1) (with and without patch 1 2022-04)
  File                 File Version
  delphicoreide280.bpl 28.0.44500.8973
  coreide280.bpl       28.0.44500.8973
  bds.exe              28.0.44500.8973
  dcldb280.bpl         28.0.44500.8973

  Delphi 11.2 (Update 2)
  File                 File Version
  delphicoreide280.bpl 28.0.46141.0937
  coreide280.bpl       28.0.46141.0937
  bds.exe              28.0.46141.0937
  dcldb280.bpl         28.0.46141.0937

  Delphi 11.3 (Update 3)
  File                 File Version
  delphicoreide280.bpl 28.0.47991.2819
  coreide280.bpl       28.0.47991.2819
  bds.exe              28.0.47991.2819
  dcldb280.bpl         28.0.47991.2819

}
function GetRS11Version: TBorlandIdeVersion;
const
  CoreIde2800: TVersionNumber =     (Minor: 0; Major: 28; Build: 6491; Release: 42600);
  CoreIde2800Upd1: TVersionNumber = (Minor: 0; Major: 28; Build: 8973; Release: 44500);
  CoreIde2800Upd2: TVersionNumber = (Minor: 0; Major: 28; Build: 0937; Release: 46141);
  CoreIde2800Upd3: TVersionNumber = (Minor: 0; Major: 28; Build: 2819; Release: 47991);
var
  CoreIdeFileVersion: TVersionNumber;
begin
  CoreIdeFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\coreide280.bpl');
  if CompareVersionNumber(CoreIdeFileVersion, CoreIde2800Upd3) >= 0 then begin
    Result := ideRS11U3
  end else if CompareVersionNumber(CoreIdeFileVersion, CoreIde2800Upd2) >= 0 then begin
    Result := ideRS11U2
  end else if CompareVersionNumber(CoreIdeFileVersion, CoreIde2800Upd1) >= 0 then begin
    Result := ideRS11U1
  end else begin
    Result := ideRS11;
  end;
end;

{
  Delphi 12 Athens
  delphicoreide290.bpl 29.0.50491.5718
  coreide290.bpl       29.0.50491.5718
  bds.exe              29.0.50491.5718
  dcldb290.bpl         29.0.50491.5718
  rtl290.bpl           29.0.51211.6493

  Delphi 12 (Patch 1)
  rtl290.bpl           29.0.51211.6493

  Delphi 12 (Update 1)
  delphicoreide290.bpl 29.0.51961.7529
  coreide290.bpl       29.0.51961.7529
  bds.exe              29.0.51961.7529
  dcldb290.bpl         29.0.51961.7529
  rtl290.bpl           29.0.51961.7529

}

function GetRS12Version: TBorlandIdeVersion;
const
  RtlBpl2900: TVersionNumber =     (Minor: 0; Major: 29; Build: 5718; Release: 50491);
  RtlBpl2900Patch1: TVersionNumber = (Minor: 0; Major: 29; Build: 6493; Release: 51211);
  RtlBpl2900Upd1:TVersionNumber = (Minor: 0; Major: 29; Build: 7529; Release: 51961);
var
  RtlBplFileVersion: TVersionNumber;
begin
  RtlBplFileVersion := GetFileVersionNumber(GetIdeRootDirectory + 'Bin\rtl290.bpl');
  if CompareVersionNumber(RtlBplFileVersion, RtlBpl2900Upd1) >= 0 then begin
    Result := ideRS12U1;
  end else if CompareVersionNumber(RtlBplFileVersion, RtlBpl2900Patch1) >= 0 then begin
    Result := ideRS12P1
  end else if CompareVersionNumber(RtlBplFileVersion, RtlBpl2900) >= 0 then begin
    Result := ideRS12
  end else begin
    Result := ideUnknown;
  end;
end;

function GetBorlandIdeVersion: TBorlandIdeVersion;
begin
  // We only actually detect the version once per session.
  // The previous result is cached in DetectedVersion.
  if DetectedVersion <> ideUndetected then
  begin
    Result := DetectedVersion;
    Exit;
  end;

  {$IFDEF VER140}  // Delphi 6.0 and C++Builder 6.0
    if RunningCppBuilder then
    begin
      Result := GetCppBuilder6IdeVersion;
      Assert(Result in [ideBCB600, ideBCB601, ideBCB602, ideBCB604]);
    end
    else begin
      Result := GetDelphi6IdeVersion;
      Assert(Result in [ideD600, ideD601R, ideD601F, ideD602]);
    end;
  {$ENDIF VER140}

  {$IFDEF VER150}  // Delphi 7.0 and C++Builder 7.0
    Result := GetDelphi7IdeVersion;
    Assert(Result in [ideD700, ideD71]);
  {$ENDIF VER150}

  {$IFDEF VER160}  // Delphi 8 and C#Builder 1.0
    Result := GetDelphi8Version;
    Assert(Result in [ideD800, ideD801, ideD802, ideCSB100]);
  {$ENDIF VER160}

  {$IFDEF VER170}  // Delphi 9 (2005)
    Result := GetDelphi9Version;
    Assert(Result in [ideD900, ideD901, ideD902, ideD903]);
    case Result of
      ideD900:
        raise Exception.Create('Your IDE version (Delphi 2005 without updades) is no longer supported, please install at least Update 2!');
      ideD901:
        raise Exception.Create('Your IDE version (Delphi 2005 Update 1) is no longer supported, please install at least Update 2!');
    end;
  {$ENDIF VER170}

  {$IFDEF VER180}
    {$IFNDEF VER185} // BDS 2006
      Result := GetBDS2006Version;
      Assert(Result in [ideBDS2006]);
    {$ELSE} // Delphi 2007
      Result := GetDelphi2007Version;
      Assert(Result in [ideDelphi2007]);
    {$ENDIF}
  {$ENDIF VER180}

  // VER190 is Delphi.NET 2007

  {$IFDEF VER200}
    Result := GetRS2009Version;
    Assert(Result in [ideRS2009, ideRS2009U1, ideRS2009U2, ideRS2009U3, ideRS2009U4]);
  {$ENDIF VER200}

  {$IFDEF VER210}
    Result := GetRS2010Version;
    Assert(Result in [ideRS2010, ideRS2010U1, ideRS2010U4, ideRS2010U5]);
  {$ENDIF VER210}
  
  {$IFDEF VER220}
    Result := GetRSXE1Version;
    Assert(Result in [ideRSXE1, ideRSXE1U1]);
  {$ENDIF VER220}

  {$IFDEF VER230}
    Result := GetRSXE2Version;
    Assert(Result in [ideRSXE2]);
  {$ENDIF VER230}

  {$IFDEF VER240}
    Result := GetRSXE3Version;
    Assert(Result in [ideRSXE3]);
  {$ENDIF VER240}

  {$IFDEF VER250}
    Result := GetRSXE4Version;
    Assert(Result in [ideRSXE4]);
  {$ENDIF VER250}

  {$IFDEF VER260}
    Result := GetRSXE5Version;
    Assert(Result in [ideRSXE5]);
  {$ENDIF VER260}

  {$IFDEF VER270}
    Result := GetRSXE6Version;
    Assert(Result in [ideRSXE6]);
  {$ENDIF VER270}

  {$IFDEF VER280}
    Result := GetRSXE7Version;
    Assert(Result in [ideRSXE7]);
  {$ENDIF VER280}

  {$IFDEF VER290}
    Result := GetRSXE8Version;
    Assert(Result in [ideRSXE8]);
  {$ENDIF VER290}

  {$IFDEF VER300}
    Result := GetRS10Version;
    Assert(Result in [ideRS10]);
  {$ENDIF VER300}

  {$IFDEF VER310}
    Result := GetRS101Version;
    Assert(Result in [ideRS101, ideRS101U1, ideRS101U2]);
  {$ENDIF VER310}

  {$IFDEF VER320}
    Result := GetRS102Version;
    Assert(Result in [ideRS102]);
  {$ENDIF VER320}

  {$IFDEF VER330}
    Result := GetRS103Version;
    Assert(Result in [ideRS103, ideRS103U1, ideRS103U2, ideRS103U3]);
  {$ENDIF VER330}

  {$IFDEF VER340}
    Result := GetRS104Version;
    Assert(Result in [ideRS104, ideRS104P2, ideRS104U1, ideRs104U2]);
  {$ENDIF VER340}

  {$IFDEF VER350}
    Result := GetRS11Version;
    Assert(Result in [ideRS11, ideRS11U1, ideRS11U2, ideRS11U3]);
  {$ENDIF VER350}

  {$IFDEF VER360}
    Result := GetRS12Version;
    Assert(Result in [ideRS12, ideRS12P1, ideRS12U1]);
  {$ENDIF VER360}

  if Result = ideUnknown then
    MessageDlg('Unknown IDE major version detected.  Please update GX_GetIdeVersion.pas.', mtError, [mbOK], 0);

  DetectedVersion := Result;
end;

{$IF CompilerVersion > 36} // new Delphi version
  'Add the information for the new Delphi version above and increase the CompilerVersion in this conditional'
{$IFEND}

function GetBorlandIdeVersionStr: string;
begin
  case GetBorlandIdeVersion of
     ideD600: Result := 'Delphi 6.00';
     ideD601R: Result := 'Delphi 6.01 (recalled)';
     ideD601F: Result := 'Delphi 6.01 (final)';
     ideD602: Result := 'Delphi 6.02';

     ideD700: Result := 'Delphi 7.0';
     ideD71: Result := 'Delphi 7.1';

     ideD800: Result := 'Delphi 8.00';
     ideD801: Result := 'Delphi 8.01';
     ideD802: Result := 'Delphi 8.02';

     ideD900: Result := 'Delphi 2005';
     ideD901: Result := 'Delphi 2005 upd 1';
     ideD902: Result := 'Delphi 2005 upd 2';
     ideD903: Result := 'Delphi 2005 upd 3';

     ideBDS2006: Result := 'BDS 2006';

     ideDelphi2007: Result := 'RAD Studio 2007';

     ideRS2009: Result := 'RAD Studio 2009';
     ideRS2009U1: Result := 'RAD Studio 2009 upd 1';
     ideRS2009U2: Result := 'RAD Studio 2009 upd 2';
     ideRS2009U3: Result := 'RAD Studio 2009 upd 3';
     ideRS2009U4: Result := 'RAD Studio 2009 upd 4';

     ideRS2010: Result := 'RAD Studio 2009';
     ideRS2010U1: Result := 'RAD Studio 2009 upd 1';
     ideRS2010U4: Result := 'RAD Studio 2009 upd 4';
     ideRS2010U5: Result := 'RAD Studio 2009 upd 5';

     ideRSXE1: Result := 'RAD Studio XE';
     ideRSXE1U1: Result := 'RAD Studio XE upd 1';
     ideRSXE2: Result := 'RAD Studio XE2';
     ideRSXE3: Result := 'RAD Studio XE3';
     ideRSXE4: Result := 'RAD Studio XE4';
     ideRSXE5: Result := 'RAD Studio XE5';
     ideRSXE6: Result := 'RAD Studio XE6';
     ideRSXE7: Result := 'RAD Studio XE7';
     ideRSXE8: Result := 'RAD Studio XE8';

     ideRS10: Result := 'RAD Studio 10 (Seattle)';

     ideRS101: Result := 'RAD Studio 10.1 (Berlin)';
     ideRS101U1: Result := 'RAD Studio 10.1 (Berlin) upd 1';
     ideRS101U2: Result := 'RAD Studio 10.1 (Berlin) upd 2';

     ideRS102: Result := 'RAD Studio 10.2 (Tokyo)';

     ideRS103: Result := 'RAD Studio 10.3 (Rio)';
     ideRS103U1: Result := 'RAD Studio 10.3 (Rio) upd 1';
     ideRS103U2: Result := 'RAD Studio 10.3 (Rio) upd 2';
     ideRS103U3: Result := 'RAD Studio 10.3 (Rio) upd 3';

     ideRS104: Result := 'RAD Studio 10.4 (Sydney)';
     ideRS104P2: Result := 'RAD Studio 10.4 (Sydney) patch 2';
     ideRS104U1: Result := 'RAD Studio 10.4 (Sydney) upd 1';
     ideRS104U2: Result := 'RAD Studio 10.4 (Sydney) upd 2';

     ideRS11: Result := 'RAD Studio 11 (Alexandria)';
     ideRS11U1: Result := 'RAD Studio 11.1 (Alexandria upd 1)';
     ideRS11U2: Result := 'RAD Studio 11.2 (Alexandria upd 2)';
     ideRS11U3: Result := 'RAD Studio 11.3 (Alexandria upd 3)';

     ideRS12: Result := 'RAD Studio 12 (Athens)';
     ideRS12P1: Result := 'RAD Studio 12 (Athens) patch 1';
     ideRS12U1: Result := 'RAD Studio 12.1 (Athens upd 1)';

     ideCSB100: Result := 'C# Builder 1.00';

     ideBCB600: Result := 'C++ Builder 6.00';
     ideBCB601: Result := 'C++ Builder 6.01';
     ideBCB602: Result := 'C++ Builder 6.02';
     ideBCB604: Result := 'C++ Builder 6.04';

     ideKylix100: Result := 'Kylix 1.00';
     ideKylix200: Result := 'Kylix 2.00';
     ideKylix300: Result := 'Kylix 3.00';
  else
    Result := 'Unknown IDE version';
  end;
end;

initialization
  DetectedVersion := ideUndetected;

end.
