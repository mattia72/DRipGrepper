unit RipGrepper.UI.IFrameEvents;

interface

type
	IFrameEvents = interface                       // TODO: rename IFrameEventHandler
		['{4E501DFF-A887-4EF3-A376-9A177EE499FE}']
		procedure Initialize;
		function GetIsInitialized() : Boolean;
		procedure BeforeSearch(var _bAbort : Boolean);
		procedure AfterSearch;
		procedure AfterHistObjChange;
		procedure UpdateUIStyle(_sNewStyle : string = '');   // TODO: it should be cleaned
		property IsInitialized: Boolean read GetIsInitialized;
	end;

implementation

end.
