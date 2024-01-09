unit RipGrepper.Helper.CursorSaver;

interface

uses
	System.UITypes;

type
	TCursorSaver = record
		strict private
			FOldCursor : TCursor;

		private
		public
			procedure ChangeTo(NewCursor : TCursor);
			constructor Create(NewCursor : TCursor); // Verwendung ist nicht so eindeutig
			procedure SetHourGlassCursor;
			class operator Finalize(var Dest : TCursorSaver);
	end;

implementation

uses
	Vcl.Forms;

{ TCursorSaver }

procedure TCursorSaver.SetHourGlassCursor;
begin
	ChangeTo(crHourGlass);
end;

procedure TCursorSaver.ChangeTo(NewCursor : TCursor);
begin
	FOldCursor := Screen.Cursor;
	Screen.Cursor := NewCursor;
end;

constructor TCursorSaver.Create(NewCursor : TCursor);
begin
	FOldCursor := Screen.Cursor;
	Screen.Cursor := NewCursor;
end;

class operator TCursorSaver.Finalize(var Dest : TCursorSaver);
begin
	Screen.Cursor := Dest.FOldCursor;
end;

end.
