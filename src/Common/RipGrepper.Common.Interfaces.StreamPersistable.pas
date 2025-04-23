unit RipGrepper.Common.Interfaces.StreamPersistable;

interface

uses
	System.Classes;

type

	IStreamPersistable = interface
		['{2C7AF195-2AF4-4DA7-A609-C403977EA649}']
		procedure LoadFromStream(_stream : TStream);
		procedure SaveToStream(_stream : TStream);
	end;

	IStreamReaderWriterPersistable = interface(IInterface)
		['{B915458F-4ADD-46DB-BF5B-ACD02E378036}']
		procedure LoadFromStreamReader(_sr : TStreamReader);
		procedure SaveToStreamWriter(_sw : TStreamWriter);
	end;

implementation

end.
