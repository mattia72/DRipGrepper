unit RootUnit;

interface

uses
	Pkg.Json.DTO,
	System.Generics.Collections,
	REST.Json.Types;

{$M+}

type
	TAlternateRowColors = class;
	TContext = class;
	TDebugTrace = class;
	TEncoding = class;
	TExpandNodes = class;
	TExpertMode = class;
	TFileMasks = class;
	THidden = class;
	TIndentLines = class;
	TNoIgnore = class;
	TPretty = class;
	TSearchParams = class;
	TSearchPath = class;
	TShowFileIcon = class;
	TShowRelativePath = class;

	TExpertMode = class
		private
			[JSONName('Value')]
			FValue : Boolean;

		published
			property Value : Boolean read FValue write FValue;
	end;

	TDebugTrace = class
		private
			[JSONName('Value')]
			FValue : Boolean;

		published
			property Value : Boolean read FValue write FValue;
	end;

	TRipGrepperSettings = class
		private
			[JSONName('DebugTrace')]
			FDebugTrace : TDebugTrace;
			[JSONName('ExpertMode')]
			FExpertMode : TExpertMode;

		published
			property DebugTrace : TDebugTrace read FDebugTrace;
			property ExpertMode : TExpertMode read FExpertMode;

		public
			constructor Create;
			destructor Destroy; override;
	end;

	TAlternateRowColors = class
		private
			[JSONName('Value')]
			FValue : Boolean;

		published
			property Value : Boolean read FValue write FValue;
	end;

	TExpandNodes = class
		private
			[JSONName('Value')]
			FValue : Boolean;

		published
			property Value : Boolean read FValue write FValue;
	end;

	TIndentLines = class
		private
			[JSONName('Value')]
			FValue: Boolean;

		published
			property Value: Boolean read FValue write FValue;
	end;

	TShowFileIcon = class
		private
			[JSONName('Value')]
			FValue : Boolean;

		published
			property Value : Boolean read FValue write FValue;
	end;

	TShowRelativePath = class
		private
			[JSONName('Value')]
			FValue : Boolean;

		published
			property Value : Boolean read FValue write FValue;
	end;

	TNodeLookSettings = class
		private
			[JSONName('AlternateRowColors')]
			FAlternateRowColors : TAlternateRowColors;
			[JSONName('ExpandNodes')]
			FExpandNodes : TExpandNodes;
			[JSONName('IndentLines')]
			FIndentLines : TIndentLines;
			[JSONName('ShowFileIcon')]
			FShowFileIcon : TShowFileIcon;
			[JSONName('ShowRelativePath')]
			FShowRelativePath : TShowRelativePath;

		published
			property AlternateRowColors : TAlternateRowColors read FAlternateRowColors;
			property ExpandNodes : TExpandNodes read FExpandNodes;
			property IndentLines : TIndentLines read FIndentLines;
			property ShowFileIcon : TShowFileIcon read FShowFileIcon;
			property ShowRelativePath : TShowRelativePath read FShowRelativePath;

		public
			constructor Create;
			destructor Destroy; override;
	end;

	TContext = class
		private
			[JSONName('Value')]
			FValue : Integer;

		published
			property Value : Integer read FValue write FValue;
	end;

	TNoIgnore = class
		private
			[JSONName('Value')]
			FValue : Boolean;

		published
			property Value : Boolean read FValue write FValue;
	end;

	TEncoding = class
		private
			[JSONName('Value')]
			FValue : string;

		published
			property Value : string read FValue write FValue;
	end;

	TPretty = class
		private
			[JSONName('Value')]
			FValue : Boolean;

		published
			property Value : Boolean read FValue write FValue;
	end;

	THidden = class
		private
			[JSONName('Value')]
			FValue : Boolean;

		published
			property Value : Boolean read FValue write FValue;
	end;

	TRipGrepperSearchSettings = class
		private
			[JSONName('Context')]
			FContext : TContext;
			[JSONName('Encoding')]
			FEncoding : TEncoding;
			[JSONName('Hidden')]
			FHidden : THidden;
			[JSONName('NoIgnore')]
			FNoIgnore : TNoIgnore;
			[JSONName('Pretty')]
			FPretty : TPretty;

		published
			property Context : TContext read FContext;
			property Encoding : TEncoding read FEncoding;
			property Hidden : THidden read FHidden;
			property NoIgnore : TNoIgnore read FNoIgnore;
			property Pretty : TPretty read FPretty;

		public
			constructor Create;
			destructor Destroy; override;
	end;

	TFileMasks = class
		private
			[JSONName('Value')]
			FValue : string;

		published
			property Value : string read FValue write FValue;
	end;

	TSearchPath = class
		private
			[JSONName('Value')]
			FValue : string;

		published
			property Value : string read FValue write FValue;
	end;

	TSearchParams = class
		private
			[JSONName('Value')]
			FValue : string;

		published
			property Value : string read FValue write FValue;
	end;

	TRipGrepSettings = class
		private
			[JSONName('FileMasks')]
			FFileMasks : TFileMasks;
			[JSONName('SearchParams')]
			FSearchParams : TSearchParams;
			[JSONName('SearchPath')]
			FSearchPath : TSearchPath;

		published
			property FileMasks : TFileMasks read FFileMasks;
			property SearchParams : TSearchParams read FSearchParams;
			property SearchPath : TSearchPath read FSearchPath;

		public
			constructor Create;
			destructor Destroy; override;
	end;

	TRoot = class(TJsonDTO)
		private
			[JSONName('RipGrepSettings')]
			FRipGrepSettings : TRipGrepSettings;
			[JSONName('RipGrepperSearchSettings')]
			FRipGrepperSearchSettings : TRipGrepperSearchSettings;
			[JSONName('RipGrepperSettings')]
			FRipGrepperSettings : TRipGrepperSettings;
			[JSONName('NodeLookSettings')]
			FNodeLookSettings : TNodeLookSettings;

		published
			property RipGrepSettings : TRipGrepSettings read FRipGrepSettings;
			property RipGrepperSearchSettings : TRipGrepperSearchSettings read FRipGrepperSearchSettings;
			property RipGrepperSettings : TRipGrepperSettings read FRipGrepperSettings;
			property NodeLookSettings : TNodeLookSettings read FNodeLookSettings;

		public
			constructor Create; override;
			destructor Destroy; override;
	end;

implementation

{ TRipGrepperSettings }

constructor TRipGrepperSettings.Create;
begin
	inherited;
	FDebugTrace := TDebugTrace.Create;
	FExpertMode := TExpertMode.Create;
end;

destructor TRipGrepperSettings.Destroy;
begin
	FDebugTrace.Free;
	FExpertMode.Free;
	inherited;
end;

{ TNodeLookSettings }

constructor TNodeLookSettings.Create;
begin
	inherited;
	FShowRelativePath := TShowRelativePath.Create;
	FShowFileIcon := TShowFileIcon.Create;
	FIndentLines := TIndentLines.Create;
	FExpandNodes := TExpandNodes.Create;
	FAlternateRowColors := TAlternateRowColors.Create;
end;

destructor TNodeLookSettings.Destroy;
begin
	FShowRelativePath.Free;
	FShowFileIcon.Free;
	FIndentLines.Free;
	FExpandNodes.Free;
	FAlternateRowColors.Free;
	inherited;
end;

{ TRipGrepperSearchSettings }

constructor TRipGrepperSearchSettings.Create;
begin
	inherited;
	FHidden := THidden.Create;
	FPretty := TPretty.Create;
	FEncoding := TEncoding.Create;
	FNoIgnore := TNoIgnore.Create;
	FContext := TContext.Create;
end;

destructor TRipGrepperSearchSettings.Destroy;
begin
	FHidden.Free;
	FPretty.Free;
	FEncoding.Free;
	FNoIgnore.Free;
	FContext.Free;
	inherited;
end;

{ TRipGrepSettings }

constructor TRipGrepSettings.Create;
begin
	inherited;
	FSearchParams := TSearchParams.Create;
	FSearchPath := TSearchPath.Create;
	FFileMasks := TFileMasks.Create;
end;

destructor TRipGrepSettings.Destroy;
begin
	FSearchParams.Free;
	FSearchPath.Free;
	FFileMasks.Free;
	inherited;
end;

{ TRoot }

constructor TRoot.Create;
begin
	inherited;
	FRipGrepSettings := TRipGrepSettings.Create;
	FRipGrepperSearchSettings := TRipGrepperSearchSettings.Create;
	FNodeLookSettings := TNodeLookSettings.Create;
	FRipGrepperSettings := TRipGrepperSettings.Create;
end;

destructor TRoot.Destroy;
begin
	FRipGrepSettings.Free;
	FRipGrepperSearchSettings.Free;
	FNodeLookSettings.Free;
	FRipGrepperSettings.Free;
	inherited;
end;

end.
