unit RipGrepper.UI.ScaleableBaseForm;

interface

uses
	GX_BaseForm,
	u_dzDpiScaleUtils,
	Vcl.Controls,
	System.Types,
	System.Classes;

type
	TScaleableBaseForm = class(tfmBaseForm)

		private
			FImageScaler : TImageListScaler;
			procedure FindImageListForDpiScaler(_parent : TComponent);
			procedure ProcessControl(_ctrl : TControl; _imgList : TImageList); virtual;

		protected

			FScaleImageList : TImageList;
			procedure ApplyDpi(_NewDpi : Integer; _NewBounds : PRect); override;
			procedure ArrangeControls; override;
			procedure InitImageListScaler(_imgList : TImageList);

		public
			// constructor Create(AOwner: TComponent; var _imgList: TImageList); overload;
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;
			procedure Loaded; override;
	end;

implementation

uses
	Vcl.StdCtrls,
	RipGrepper.Tools.DebugTools;

// constructor TScaleableBaseForm.Create(AOwner: TComponent; var _imgList: TImageList);
// begin
// inherited Create(AOwner);
// InitImageListScaler(_imgList);
// end;

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
	if not Assigned(FImageScaler) then
		FImageScaler := TImageListScaler.Create(Self, FScaleImageList);
	li := FImageScaler.GetScaledList(_NewDpi);
	ProcessControl(self, li);
end;

procedure TScaleableBaseForm.ArrangeControls;
begin
	inherited;
end;

procedure TScaleableBaseForm.FindImageListForDpiScaler(_parent : TComponent);
begin
	if Assigned(FScaleImageList) then begin
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
			FindImageListForDpiScaler(cmp);
		end;
	end;
end;

procedure TScaleableBaseForm.InitImageListScaler(_imgList : TImageList);
begin
	if not Assigned(FScaleImageList) then begin
		self.FScaleImageList := _imgList;
		InitDpiScaler();
	end;
end;

procedure TScaleableBaseForm.Loaded;
begin
	TDebugUtils.DebugMessage('TScaleableBaseForm.Loaded');
	inherited Loaded;

	FindImageListForDpiScaler(self);
end;

procedure TScaleableBaseForm.ProcessControl(_ctrl : TControl; _imgList : TImageList);
var
	i : integer;
begin
	if _ctrl is TButton then begin
		(_ctrl as TButton).Images := _imgList;
	end;

	if _ctrl is TWinControl then begin
		for i := 0 to (_ctrl as TWinControl).ControlCount - 1 do
			ProcessControl((_ctrl as TWinControl).Controls[i], _imgList);
	end;
end;

end.
