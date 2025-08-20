unit RipGrepper.Common.IDEContextValues;

interface

uses
	RipGrepper.Common.SimpleTypes;

type
	IIDEContextValues = interface
		['{75CC020F-921D-4722-BE07-7105092FFA41}']
		function GetContextType() : EDelphiIDESearchContext;
		function GetValue() : string;
		procedure SetContextType(const Value : EDelphiIDESearchContext);
		procedure SetValue(const Value : string);

	end;

	TIDEContextValues = class(TInterfacedObject, IIDEContextValues)
		private
			FContextType : EDelphiIDESearchContext;
			FValue : string;
			function GetContextType() : EDelphiIDESearchContext;
			function GetValue() : string;
			procedure SetContextType(const Value : EDelphiIDESearchContext);
			procedure SetValue(const Value : string);

		public
			constructor Create(const _sc : EDelphiIDESearchContext; const _val : string);
			property Value : string read GetValue write SetValue;
			property ContextType : EDelphiIDESearchContext read GetContextType write SetContextType;
	end;

    // Event type for extension context change
	TExtensionContextChangeEvent = procedure(Sender : TObject; _icv : IIDEContextValues) of object;

implementation

constructor TIDEContextValues.Create(const _sc : EDelphiIDESearchContext; const _val : string);
begin
	inherited Create;
	FContextType := _sc;
	FValue := _val;
end;

function TIDEContextValues.GetContextType() : EDelphiIDESearchContext;
begin
	Result := FContextType;
end;

function TIDEContextValues.GetValue() : string;
begin
	Result := FValue;
end;

procedure TIDEContextValues.SetContextType(const Value : EDelphiIDESearchContext);
begin
	FContextType := Value;
end;

procedure TIDEContextValues.SetValue(const Value : string);
begin
	FValue := Value;
end;

end.
