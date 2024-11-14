unit RipGrepper.Tools.Replacer.StandaloneContext;

interface

uses
	System.Classes,
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.EncodedStringList;

type
	TReplaceContext = class(TInterfacedObject, IReplaceContext)
		public
			procedure GetFileLines(_file : string; _list : TEncodedStringList);
			procedure WriteFileLines(_file : string; _list : TEncodedStringList);
	end;

implementation

{ TReplaceContext }

procedure TReplaceContext.GetFileLines(_file : string; _list : TEncodedStringList);
begin
	_list.LoadFromFile(_file);
end;

procedure TReplaceContext.WriteFileLines(_file : string; _list : TEncodedStringList);
begin
	_list.SaveToFile(_file);
end;

end.
