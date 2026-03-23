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
	end;

var
	BaseForm : TBaseForm;

implementation

{$R *.dfm}

end.
