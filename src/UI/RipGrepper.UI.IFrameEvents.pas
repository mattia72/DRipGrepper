unit RipGrepper.UI.IFrameEvents;

interface

type
	IFrameEvents = interface
	['{4E501DFF-A887-4EF3-A376-9A177EE499FE}']
			procedure Init;
			procedure BeforeSearch;
			procedure AfterSearch;
            procedure AfterHistObjChange;

	end;

implementation

end.
