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
	SVGIconImageList;

type
	TBaseForm = class(TForm)
		PanelBottom : TPanel;
		btnOk : TButton;
		btnCancel : TButton;
		SVGImageListBottomPanel : TSVGIconImageList;
	protected
		// Override to customize Ok button caption, image and action
		procedure UpdateOkButton; virtual;
		// Override to customize Cancel button caption, image and action
		procedure UpdateCancelButton; virtual;
	public
		procedure AfterConstruction; override;
	end;

var
	BaseForm : TBaseForm;

implementation

{$R *.dfm}

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
