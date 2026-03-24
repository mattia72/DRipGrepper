unit RipGrepper.UI.BaseForm;

interface

uses
	System.Classes,
	Vcl.ExtCtrls,
	Vcl.Forms,
	Vcl.StdCtrls,
	System.ImageList,
	Vcl.ImgList,
	SVGIconImageListBase,
	SVGIconImageList,
	RipGrepper.UI.DpiScaler,
	RipGrepper.Helper.UI.DarkMode;

type
	TBaseForm = class(TForm)
		PanelBottom : TPanel;
		btnOk : TButton;
		btnCancel : TButton;
		SVGImageListBottomPanel : TSVGIconImageList;

	private
		FThemeHandler : TThemeHandler;
		function GetThemeHandler : TThemeHandler;

	protected
		FDpiScaler : TRipGrepperDpiScaler;
		property ThemeHandler : TThemeHandler read GetThemeHandler;
		// Override to customize Ok button caption, image and action
		procedure UpdateOkButton; virtual;
		// Override to customize Cancel button caption, image and action
		procedure UpdateCancelButton; virtual;

	public
		constructor Create(_Owner : TComponent; _themeName : string = ''); reintroduce; virtual;
		destructor Destroy; override;
		procedure AfterConstruction; override;
	end;

var
	BaseForm : TBaseForm;

implementation

{$R *.dfm}

constructor TBaseForm.Create(_Owner : TComponent; _themeName : string = '');
begin
	inherited Create(_Owner);
	FDpiScaler := TRipGrepperDpiScaler.Create(self);
	if _themeName <> '' then
		ThemeHandler.Init(_themeName);
end;

destructor TBaseForm.Destroy;
begin
	FDpiScaler.Free;
	inherited;
end;

function TBaseForm.GetThemeHandler : TThemeHandler;
begin
	if not Assigned(FThemeHandler) then begin
		FThemeHandler := TThemeHandler.Create(self);
	end;
	Result := FThemeHandler;
end;

procedure TBaseForm.AfterConstruction;
begin
	inherited;
	UpdateOkButton;
	UpdateCancelButton;
end;

procedure TBaseForm.UpdateOkButton;
begin
	btnOk.Caption := 'OK';
	btnOk.ImageIndex := 0;
	btnOk.ImageName := 'ok';
	btnOk.Images := SVGImageListBottomPanel;
end;

procedure TBaseForm.UpdateCancelButton;
begin
	btnCancel.Caption := 'Cancel';
	btnCancel.ImageIndex := 1;
	btnCancel.ImageName := 'close';
	btnCancel.Images := SVGImageListBottomPanel;
end;

end.
