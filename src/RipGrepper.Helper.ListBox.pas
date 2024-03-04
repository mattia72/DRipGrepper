unit RipGrepper.Helper.ListBox;

interface

uses
	Vcl.StdCtrls;

type
	TListBoxHelper = class

		public
			class procedure FreeItemObjects(_lb : TListBox);
	end;

implementation

uses
	RipGrepper.Data.HistoryItemObject;

class procedure TListBoxHelper.FreeItemObjects(_lb : TListBox);
begin
	if Assigned(_lb) then begin
		for var i := 0 to _lb.Items.Count - 1 do begin
			_lb.Items.Objects[i].Free;
			_lb.Items.Objects[i] := nil;
		end;
    end;
end;

end.
