unit GX_GExperts;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  GX_Experts;

type
  IGExpertsInst = interface
    function GetSharedImages: TImageList;
    function GetScaledSharedDisabledImages(_Dpi: Integer): TImageList;
    function GetScaledSharedImages(_Dpi: Integer): TImageList;
  end;

function GExpertsInst(_AssertValid: Boolean): IGExpertsInst;
procedure ShowGXAboutForm;
procedure InitSharedResources;
procedure FreeSharedResources;

implementation

uses
  GX_SharedImages,
  GX_About;

type
  TGExpertsInst = class(TInterfacedObject, IGExpertsInst)
  private
    function GetSharedImages: TImageList;
    function GetScaledSharedDisabledImages(_Dpi: Integer): TImageList;
    function GetScaledSharedImages(_Dpi: Integer): TImageList;
  end;

var
  gblGExpertsInst: IGExpertsInst;
  SharedImages: TdmSharedImages;

function GExpertsInst(_AssertValid: Boolean): IGExpertsInst;
begin
  if not Assigned(gblGExpertsInst) then
    gblGExpertsInst := TGExpertsInst.Create;
  Result := gblGExpertsInst;
end;

{ TGExpertsInst }

function TGExpertsInst.GetScaledSharedDisabledImages(_Dpi: Integer): TImageList;
begin
  Result := nil;
  if Assigned(SharedImages) then
    Result := SharedImages.GetScaledDisabledImages(_Dpi);
end;

function TGExpertsInst.GetScaledSharedImages(_Dpi: Integer): TImageList;
begin
  Result := nil;
  if Assigned(SharedImages) then
    Result := SharedImages.GetScaledImages(_Dpi);
end;

function TGExpertsInst.GetSharedImages: TImageList;
begin
  Result := nil;
  if Assigned(SharedImages) then
    Result := SharedImages.Images;
end;

procedure ShowGXAboutForm;
begin
  gblAboutFormClass.Execute(nil);
end;

procedure InitSharedResources;
begin
  if not Assigned(SharedImages) then
    SharedImages := TdmSharedImages.Create(nil);
end;

procedure FreeSharedResources;
begin
  FreeAndNil(SharedImages);
end;

end.

