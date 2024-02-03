unit RipGrepper.OpenWith.SimpleTypes;

interface

const
	OPEN_WITH_SETTINGS = 'OPEN_WITH_SETTINGS';
	OPENWITH_COMMAND_KEY = 'COMMAND';

	MAX_COMMAND_NUM = 10;
	// Tabulator
	SEPARATOR = #9;

type
	TOpenWithParams = record
		DirPath : string;
		FileName : string;
		Row : Integer;
		Column : Integer;
		IsEmpty : Boolean;

		public
			class operator Initialize(out Dest : TOpenWithParams);
	end;

implementation

class operator TOpenWithParams.Initialize(out Dest : TOpenWithParams);
begin
	Dest.DirPath := '';
	Dest.FileName := '';
	Dest.Row := -1;
	Dest.Column := -1;
	Dest.IsEmpty := True;
end;

end.
