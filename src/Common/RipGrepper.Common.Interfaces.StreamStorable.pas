unit RipGrepper.Common.Interfaces.StreamStorable;

interface

uses
	System.Classes;

type
	IStreamStorable = interface
		['{2C7AF195-2AF4-4DA7-A609-C403977EA649}']
		procedure LoadFromStream(_stream : TStream);
		procedure SaveToStream(_stream : TStream);
	end;

implementation

end.
