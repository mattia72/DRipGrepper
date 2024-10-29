unit GX_SharedImages;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  ImgList,
{$IFDEF GX_IDE_IS_HIDPI_AWARE}
  u_dzDpiScaleUtils,
{$ENDIF}
  ImageList;

type
  TdmSharedImages = class(TDataModule)
    Images: TImageList;
    DisabledImages: TImageList;
  private
{$IFDEF GX_IDE_IS_HIDPI_AWARE}
    FImagesScaler: TImageListScaler;
    FDisabledScaler: TImageListScaler;
{$ENDIF}
  public
    function GetScaledImages(_DPI: Integer): TImageList;
    function GetScaledDisabledImages(_DPI: Integer): TImageList;
  end;

const
  ImageIndexNew = 10;
  ImageIndexExpand = 12;
  ImageIndexContract = 13;
  ImageIndexInfo = 16;
  ImageIndexTrash = 30;
  ImageIndexFunction = 29;
  ImageIndexGear = 28;
  ImageIndexClosedFolder = 21;
  ImageIndexOpenFolder = 22;
  ImageIndexDocument = 23;
  ImageIndexArrow = 43;
  ImageIndexCheck = 46;
  ImageIndexBlank = 47;
  ImageIndexVisibility = 53;
  ImageIndexMemberType = 48;
  ImageIndexWindow = 57;
  ImageIndexWindows = 58;
  ImageIndexUnit = 59;
  ImageIndexToDoPriority = 71;

  ImageIndexVirtual = 82;
  ImageIndexDynamic = 83;
  ImageIndexOverride = 84;
  ImageIndexMessage = 85;

  ImageIndexInterface = 86;

  ImageIndexMoveDown  = 92;
  ImageIndexMoveUp    = 93;

function GetSharedImageList: TImageList;

implementation

{$R *.dfm}

uses
  Graphics,
  CommCtrl,
  u_dzMiscUtils,
  GX_GExperts;

function GetSharedImageList: TImageList;
begin
  Result := GExpertsInst(True).GetSharedImages;
end;

{ TdmSharedImages }

function TdmSharedImages.GetScaledDisabledImages(_DPI: Integer): TImageList;
begin
{$IFDEF GX_IDE_IS_HIDPI_AWARE}
  if not Assigned(FDisabledScaler) then
    FDisabledScaler := TImageListScaler.Create(Self, DisabledImages);
  Result := FDisabledScaler.GetScaledList(_DPI);
{$ELSE}
  Result := DisabledImages;
{$ENDIF}
end;

function TdmSharedImages.GetScaledImages(_DPI: Integer): TImageList;
begin
{$IFDEF GX_IDE_IS_HIDPI_AWARE}
  if not Assigned(FImagesScaler) then
    FImagesScaler := TImageListScaler.Create(Self, Images);
  Result := FImagesScaler.GetScaledList(_DPI);
{$ELSE}
  Result := Images;
{$ENDIF}
end;

end.

