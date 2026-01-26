unit RipGrepper.UI.SVGIconDataModule;

interface

uses
	System.SysUtils,
	System.Classes,
	System.ImageList,
	Vcl.ImgList,
	SVGIconImageCollection, Vcl.BaseImageCollection;

type
	TSVGIconDataModule = class(TDataModule)
		SVGIconImageCollection1 : TSVGIconImageCollection;
	end;

var
	SVGIconDataModule : TSVGIconDataModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
