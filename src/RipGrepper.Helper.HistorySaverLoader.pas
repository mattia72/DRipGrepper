unit RipGrepper.Helper.HistorySaverLoader;

interface

uses
	System.Classes,
	RipGrepper.Common.Interfaces;

type
	THistorySaverLoader = class(TObject)
		private
			FHistObj : IHistoryItemObject;

		public
			constructor Create(_histObj : IHistoryItemObject);
			procedure SaveToStream(_stream : TStream);
			procedure LoadFromStream(_stream : TStream);
	end;

implementation

constructor THistorySaverLoader.Create(_histObj : IHistoryItemObject);
begin
	FHistObj := _histObj;
end;

procedure THistorySaverLoader.SaveToStream(_stream : TStream);
begin
	FHistObj.RipGrepArguments.SaveToStream(_stream);
end;

procedure THistorySaverLoader.LoadFromStream(_stream : TStream);
begin
	FHistObj.RipGrepArguments.LoadFromStream(_stream);
end;

end.
