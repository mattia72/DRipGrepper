unit RipGrepper.UI.ScaleableBaseForm;

interface

uses
	GX_BaseForm,
	u_dzDpiScaleUtils,
	Vcl.Controls,
	System.Types, System.Classes;

type
	TScaleableBaseForm = class(tfmBaseForm)

		private
			FImageScaler : TImageListScaler;
			procedure ProcessControl(_ctrl : TControl; _imgList : TImageList); virtual;

		protected

			FScaleImageList : TImageList;
			procedure ApplyDpi(_NewDpi : Integer; _NewBounds : PRect); override;
			procedure ArrangeControls; override;
			procedure InitImageListScaler(_imgList: TImageList);

		public
			constructor Create(AOwner: TComponent; var _imgList: TImageList); reintroduce;
			destructor Destroy; override;
	end;

implementation

uses
	Vcl.StdCtrls;

constructor TScaleableBaseForm.Create(AOwner: TComponent; var _imgList: TImageList);
begin
	inherited Create(AOwner);
	InitImageListScaler(_imgList);
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

procedure TScaleableBaseForm.InitImageListScaler(_imgList: TImageList);
begin
	self.FScaleImageList := _imgList;
	InitDpiScaler();
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
