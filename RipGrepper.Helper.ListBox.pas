unit RipGrepper.Helper.ListBox;

interface

uses
  Vcl.StdCtrls;

type
	TListBoxHelper = class

		public
			class procedure DiposeItemObjects(_lb: TListBox);
	end;

implementation

uses
  RipGrepper.Data.HistoryItemObject;

class procedure TListBoxHelper.DiposeItemObjects(_lb: TListBox);
begin
	for var i := 0 to _lb.Items.Count - 1 do
		Dispose(PHistoryItemObject(_lb.Items.Objects[i]));
end;

end.
