unit RipGrepper.Common.EncodedStringList;

interface

uses
	RipGrepper.Tools.FileUtils,
	System.Classes,
	System.SysUtils;

type
	TEncodedStringList = class(TStringList)
		private
			FOrigEncoding : TEncoding;
			function GetFileEncoding(const _sFilePath : string) : TEncoding;
		public
			procedure LoadFromFile(const FileName : string); override;
			procedure SaveToFile(const FileName : string); override;
	end;

implementation

function TEncodedStringList.GetFileEncoding(const _sFilePath : string) : TEncoding;
const
	MaxBOMLength = 100;
var
	Stream : TStream;
	Buffer : TBytes;
begin
	Result := nil;
	Stream := TFileStream.Create(_sFilePath, fmOpenRead or fmShareDenyNone);
	try
		SetLength(Buffer, MaxBOMLength);
		Stream.Read(Buffer[0], MaxBOMLength);
		TEncoding.GetBufferEncoding(Buffer, Result);
	finally
		Stream.Free;
	end;
end;

procedure TEncodedStringList.LoadFromFile(const FileName : string);
begin
	FOrigEncoding := GetFileEncoding(FileName);
	inherited LoadFromFile(FileName);
end;

procedure TEncodedStringList.SaveToFile(const FileName : string);
begin
	if Assigned(FOrigEncoding) then begin
		SaveToFile(FileName, FOrigEncoding)
	end else begin
		inherited SaveToFile(FileName);
	end;
end;

end.
