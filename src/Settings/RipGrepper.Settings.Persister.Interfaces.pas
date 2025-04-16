unit RipGrepper.Settings.Persister.Interfaces;

interface

uses
	ArrayEx;

type
	IFileHandler = interface(IInterface)
		['{500D0067-F8F2-488B-B3B7-5649DF5879E4}']
		procedure ReloadFile();
		procedure UpdateFile();
		procedure EraseSection(const _section : string);
		function GetFilePath() : string;
		procedure SetFilePath(const Value : string);
		property FilePath : string read GetFilePath write SetFilePath;
	end;

	IPersister = interface(IInterface)
		['{76ED2AA3-9E4D-4B54-B061-185BF880C508}']
	end;

	IFilePersister<T> = interface(IPersister)
		['{57B16806-F8F5-447E-9AB6-767E553CCB65}']
		function TryLoadValue(var _value : T) : Boolean;
		function LoadValue(const _section, _key : string) : T; // raises Exception if not exists
		procedure StoreValue(const _value : T);

		function GetFilePath() : string;
		procedure SetFilePath(const Value : string);
		property FilePath : string read GetFilePath write SetFilePath;
	end;

	IPersisterFactory = interface(IInterface)
		['{86CA585C-A4D5-44E4-A778-9C011291F623}']
		function GetStringPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<string>;
		function GetIntegerPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<Integer>;
		function GetBoolPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<Boolean>;
		function GetStrArrayPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<TArrayEx<string>>;
		function ToLogString() : string;

		function GetFilePath() : string;
		procedure SetFilePath(const Value : string);
		property FilePath : string read GetFilePath write SetFilePath;

	end;

implementation

end.
