unit RipGrepper.Common.Settings.JsonTest;

interface

uses
  DUnitX.TestFramework;

type

	[TestFixture]
	TJsonSettingsTest = class

		public
			procedure Convert(const IniFileName, JsonFileName : string);
			[Test]
			procedure ConvertIniTest;
	end;

implementation

uses
	System.SysUtils,
	System.IniFiles,
	System.Classes,
	System.JSON,
	System.IOUtils;

procedure TJsonSettingsTest.Convert(const IniFileName, JsonFileName : string);
var
	IniFile : TIniFile;
	JsonObject : TJSONObject;
	JsonFile : TStringStream;
	Sections : TStringList;
	Section, Key : string;
	i : Integer;
	Values : TStringList;
begin
	// Create instances of necessary classes
	IniFile := TIniFile.Create(IniFileName);
	JsonObject := TJSONObject.Create;
	JsonFile := TStringStream.Create('', TEncoding.UTF8);
	Sections := TStringList.Create;
	Values := TStringList.Create;

	try
		// Get all sections from the INI file
		IniFile.ReadSections(Sections);

		// Iterate over each section
		for i := 0 to Sections.Count - 1 do begin
			Section := Sections[i];
			Values.Clear;
			IniFile.ReadSectionValues(Section, Values);

			// Create a JSON object for this section
			var
			SectionObject := TJSONObject.Create;
			try
				// Add key-value pairs to the section JSON object
				for Key in Values do begin
					SectionObject.AddPair(Key, Values.Values[Key]);
				end;

				// Add the section object to the main JSON object
				JsonObject.AddPair(Section, SectionObject);
			except
				SectionObject.Free;
				raise;
			end;
		end;

		// Save JSON to file
		JsonFile.WriteString(JsonObject.Format(2));
		TFile.WriteAllText(JsonFileName, JsonFile.DataString);

	finally
		// Clean up
		IniFile.Free;
		JsonObject.Free;
		JsonFile.Free;
		Sections.Free;
		Values.Free;
	end;
end;

procedure TJsonSettingsTest.ConvertIniTest;
begin
	Convert('DripGrepper.ini', 'DripGrepper.json');
	Assert.IsTrue(TFile.Exists('DripGrepper.json'), 'Json should be exist.');
end;

initialization

TDUnitX.RegisterTestFixture(TJsonSettingsTest);

end.
