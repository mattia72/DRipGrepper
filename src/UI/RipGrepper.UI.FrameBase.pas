unit RipGrepper.UI.FrameBase;

interface

uses
  Vcl.Forms;

type
	TFrameBase = class(TFrame)

		public
			procedure Init; virtual; abstract;
			procedure BeforeSearch; virtual; abstract;
			procedure AfterSearch; virtual; abstract;
            procedure AfterHistObjChange; virtual; abstract;

	end;

implementation

end.
