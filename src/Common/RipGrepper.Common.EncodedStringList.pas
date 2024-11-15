unit RipGrepper.Common.EncodedStringList;

interface

uses
	RipGrepper.Tools.FileUtils,
	System.Classes,
	{$IFNDEF STANDALONE}
	RipGrepper.Common.IOTAUtils,
	{$ENDIF}
	System.SysUtils;

type
	TEncodedStringList = class(TStringList)
		private
			FIsBinary : Boolean;
			FOrigEncoding : TEncoding;
			function GetFileEncoding(const _sFilePath : string) : TEncoding;

		public
			// Load a binary/text form file into a TStrings object
			procedure LoadFormFileToStrings(const _sFileName : string; out _bWasBinary : Boolean); overload;
			procedure LoadFromFile(const _sFileName : string); override;
			procedure SaveToFile(const FileName : string); override;
			property IsBinary : Boolean read FIsBinary write FIsBinary;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils;

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

procedure TEncodedStringList.LoadFormFileToStrings(const _sFileName : string; out _bWasBinary : Boolean);
var
	srcStream : TStream;
	destStream : TStream;
	origStreamFormat : TStreamOriginalFormat;
begin
	self.Clear;

	destStream := nil;
	srcStream := TFileStream.Create(_sFileName, fmOpenRead or fmShareDenyWrite);
	try
		destStream := TMemoryStream.Create;
		srcStream.Position := 0;
		origStreamFormat := TestStreamFormat(srcStream);
		srcStream.Position := 0;
		_bWasBinary := (origStreamFormat = sofBinary);
		case origStreamFormat of
			sofUnknown :
			raise Exception.CreateFmt('Invalid stream format for form file: %s.  (sofUnknown)', [_sFileName]);
			sofBinary : begin
				ObjectResourceToText(srcStream, destStream, origStreamFormat);
				destStream.Position := 0;
				if origStreamFormat = sofUTF8Text then begin
					self.LoadFromStream(destStream, TEncoding.UTF8)
				end else begin
					self.LoadFromStream(destStream, TEncoding.Default);
				end;
			end;
			sofText : begin
				self.LoadFromStream(srcStream);
			end;
			sofUTF8Text : begin
				self.LoadFromStream(srcStream, TEncoding.UTF8);
			end

			else
			raise Exception.Create('Unknown form file format: ' + IntToStr(Ord(origStreamFormat)));
		end;
	finally
		FreeAndNil(srcStream);
		FreeAndNil(destStream);
	end;
end;

procedure TEncodedStringList.LoadFromFile(const _sFileName : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TEncodedStringList.LoadFromFile');

	FOrigEncoding := GetFileEncoding(_sFileName);
	dbgMsg.MsgFmt('Encoding of %s is %s', [_sFileName, FOrigEncoding.EncodingName]);
	{$IFDEF STANDALONE}
	inherited LoadFromFile(_sFileName);
	{$ELSE}
	if IOTAUtils.IsForm(_sFileName) then begin
		dbgMsg.MsgFmt('Form file: %s encoding: %s', [_sFileName, FOrigEncoding.EncodingName]);
		LoadFormFileToStrings(_sFileName, FIsBinary);
	end else begin
		inherited LoadFromFile(_sFileName);
	end;
	{$ENDIF}
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
