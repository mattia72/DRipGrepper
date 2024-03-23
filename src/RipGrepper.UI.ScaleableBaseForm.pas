unit RipGrepper.UI.ScaleableBaseForm;

interface

uses
	GX_BaseForm,
	u_dzDpiScaleUtils,
	Vcl.Controls,
	System.Types,
	System.Classes;

type
	TScaleableBaseForm = class(TfmBaseForm)

		private
			FImageScaler : TImageListScaler;
			procedure SetButtonImages(_ctrl : TControl; _imgList : TImageList); virtual;

		protected

			FScaleImageList : TImageList;
			procedure ApplyDpi(_NewDpi : Integer; _NewBounds : PRect); override;
			procedure ArrangeControls; override;
			procedure FindImageListForDpiScaler(_parent : TComponent);
			procedure InitImageListScaler(_imgList : TImageList);

		public
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;
			procedure Loaded; override;
	end;

implementation

uses
	Vcl.StdCtrls,
	RipGrepper.Tools.DebugTools;

constructor TScaleableBaseForm.Create(AOwner : TComponent);
begin
	TDebugUtils.DebugMessage('TScaleableBaseForm.Create');
	inherited;

end;

destructor TScaleableBaseForm.Destroy;
begin
	FImageScaler.Free;
	inherited;
end;

procedure TScaleableBaseForm.ApplyDpi(_NewDpi : Integer; _NewBounds : PRect);
var
	li : TImageList;
begin
	inherited ApplyDpi(_NewDpi, _NewBounds);
	if not Assigned(FImageScaler) then begin
		FImageScaler := TImageListScaler.Create(Self, FScaleImageList);
		TDebugUtils.DebugMessage('TScaleableBaseForm.ApplyDpi ImageScaler Created - ' + FScaleImageList.Name);
	end;
	li := FImageScaler.GetScaledList(_NewDpi);
	SetButtonImages(self, li);
end;

procedure TScaleableBaseForm.ArrangeControls;
begin
	inherited;
end;

procedure TScaleableBaseForm.FindImageListForDpiScaler(_parent : TComponent);
begin
	TDebugUtils.DebugMessage('TScaleableBaseForm.FindImageListForDpiScaler');

	if Assigned(FScaleImageList) then begin
		TDebugUtils.DebugMessage('TScaleableBaseForm.FindImageListForDpiScaler Exit - ' + FScaleImageList.Name);
		Exit;
	end;
	for var i := 0 to _parent.ComponentCount - 1 do begin
		var
		cmp := _parent.Components[i];
		if cmp is TImageList then begin
			TDebugUtils.DebugMessage('TScaleableBaseForm.FindImageListForDpiScaler ' + cmp.Name);
			var
				imgList : TImageList := cmp as TImageList;
			if (imgList.Count > 0) then begin
				InitImageListScaler(imgList);
				break
			end;
		end else if cmp.ComponentCount > 0 then begin
			TDebugUtils.DebugMessage('TScaleableBaseForm.FindImageListForDpiScaler recursive call ' + cmp.Name);
			FindImageListForDpiScaler(cmp);
		end;
	end;
end;

procedure TScaleableBaseForm.InitImageListScaler(_imgList : TImageList);
begin
	TDebugUtils.DebugMessage('TScaleableBaseForm.InitImageListScaler');
	if not Assigned(FScaleImageList) then begin
		self.FScaleImageList := _imgList;
		TDebugUtils.DebugMessage('TScaleableBaseForm.InitImageListScaler ' + FScaleImageList.Name);
		InitDpiScaler();
	end;
end;

procedure TScaleableBaseForm.Loaded;
begin
	TDebugUtils.DebugMessage('TScaleableBaseForm.Loaded');
	inherited Loaded;

	// FindImageListForDpiScaler(self);
end;

procedure TScaleableBaseForm.SetButtonImages(_ctrl : TControl; _imgList : TImageList);
var
	i : integer;
begin
	if _ctrl is TButton then begin
		TDebugUtils.DebugMessage('TScaleableBaseForm.SetButtonImages - ' + _ctrl.Name);
		(_ctrl as TButton).Images := _imgList;
	end;

	if _ctrl is TWinControl then begin
		for i := 0 to (_ctrl as TWinControl).ControlCount - 1 do
			SetButtonImages((_ctrl as TWinControl).Controls[i], _imgList);
	end;
end;

end.
