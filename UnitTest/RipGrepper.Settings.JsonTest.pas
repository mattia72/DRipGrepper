unit RipGrepper.Settings.JsonTest;

interface

uses
	DUnitX.TestFramework,
	System.JSON,
	System.Classes;

type

	[TestFixture]
	TJsonSettingsTest = class

		private
			procedure AddValueByType(sectionObject : TJSONObject; const sKey, sValue : string);
			procedure Convert(const _sIniFileName, _sJsonFileName : string);
			function CreateTestJson : string;
			procedure FillSectionObject(JsonObject : TJSONObject; var Section : string; Values : TStringList);

		public
			[Test]
			// [Ignore('Ignore this test, it is for converting ini to json')]
			procedure ConvertIniTest;
			[Test]
			[Ignore('Ignore this test, it is for reading json')]
			procedure ReadJsonTest;
	end;

implementation

uses
	System.SysUtils,
	System.IniFiles,

	System.IOUtils,
	Vcl.Forms,
	RipGrepper.Common.Constants;

procedure TJsonSettingsTest.AddValueByType(sectionObject : TJSONObject; const sKey, sValue : string);
var
	boolValue : Boolean;
	floatValue : Single;
	intValue : Integer;
	valueObject : TJSONObject;
begin
	valueObject := TJSONObject.Create;
	sectionObject.AddPair(sKey, valueObject);
	// try
	if Boolean.TryToParse(sValue, boolValue) then begin
		valueObject
		{ }.AddPair('Value', boolValue)
		{ }.AddPair('DefaultValue', nil);
	end else if Integer.TryParse(sValue, intValue) then begin
		valueObject
		{ }.AddPair('Value', intValue)
		{ }.AddPair('DefaultValue', nil);
	end else if Single.TryParse(sValue, floatValue) then begin
		valueObject
		{ }.AddPair('Value', floatValue)
		{ }.AddPair('DefaultValue', nil);
	end else begin
		valueObject
		{ }.AddPair('Value', sValue)
		{ }.AddPair('DefaultValue', nil);
	end;
	// sectionObject.AddPair(sKey, TJsonObject.ParseJSONValue(valueObject.ToJSON()));
	// finally
	// valueObject.Free;
	// end;
end;

procedure TJsonSettingsTest.Convert(const _sIniFileName, _sJsonFileName : string);
var
	iniFile : TIniFile;
	jo : TJSONObject;
	jf : TStringStream;
	slSections : TStringList;
	sSection : string;
	i : Integer;
	slValues : TStringList;
begin
	iniFile := TIniFile.Create(_sIniFileName);
	jo := TJSONObject.Create;
	jf := TStringStream.Create('', System.SysUtils.TEncoding.UTF8);
	slSections := TStringList.Create;
	slValues := TStringList.Create;

	try
		iniFile.ReadSections(slSections);

		for i := 0 to slSections.Count - 1 do begin
			sSection := slSections[i];
			slValues.Clear;
			iniFile.ReadSectionValues(sSection, slValues);
			FillSectionObject(jo, sSection, slValues);
		end;

		// Save JSON to file
		jf.WriteString(jo.Format(2));
		TFile.WriteAllText(_sJsonFileName, jf.DataString);
	finally
		iniFile.Free;
		jo.Free;
		jf.Free;
		slSections.Free;
		slValues.Free;
	end;
end;

procedure TJsonSettingsTest.ConvertIniTest;
begin
	var jsonFileName := CreateTestJson;
	Assert.IsTrue(TFile.Exists(jsonFileName), 'Json should be exist.');
end;

function TJsonSettingsTest.CreateTestJson : string;
begin
	var dir := TPath.GetDirectoryName(Application.ExeName);
	Result := TPath.Combine(dir, 'DripGrepper.json');
	
	// Use existing ini file from unittest directory
	var iniFile := TPath.Combine(dir, 'DRipGrepperUnittest.ini');
	if not TFile.Exists(iniFile) then begin
		iniFile := TPath.Combine(dir, 'DRipGrepperUnittest.exe.ini');
	end;
	
	// If no ini file exists, create a minimal test JSON file
	if not TFile.Exists(iniFile) then begin
		var testJson := TJSONObject.Create;
		try
			var testSection := TJSONObject.Create;
			var testValue := TJSONObject.Create;
			testValue.AddPair('Value', TJSONTrue.Create);
			testValue.AddPair('DefaultValue', TJSONNull.Create);
			testSection.AddPair('DebugTrace', testValue);
			testJson.AddPair('TestSettings', testSection);
			TFile.WriteAllText(Result, testJson.Format(2), TEncoding.UTF8);
		finally
			testJson.Free;
		end;
	end else begin
		Convert(iniFile, Result);
	end;
end;

procedure TJsonSettingsTest.ReadJsonTest;
var
	jo : TJsonObject;
	jsonPath : string;
begin
	jsonPath := CreateTestJson;
	if not TFile.Exists(jsonPath) then begin
		Assert.Fail('JSON file does not exist: ' + jsonPath);
		Exit;
	end;

	jo := TJSONObject.ParseJSONValue(TFile.ReadAllText(jsonPath)) as TJSONObject;
	try
		if jo = nil then begin
			Assert.Fail('Failed to parse JSON file');
			Exit;
		end;

		// Check if any valid section exists
		var pairCount := jo.Count;
		Assert.IsTrue(pairCount > 0, 'JSON should contain at least one section, but found ' + pairCount.ToString + ' pairs');

		// Check if we have valid content
		var hasValidSection := False;
		for var pair in jo do begin
			if pair.JsonValue is TJSONObject then begin
				var sectionObj := pair.JsonValue as TJSONObject;
				if sectionObj.Count > 0 then begin
					hasValidSection := True;
					break;
				end;
			end;
		end;

		Assert.IsTrue(hasValidSection, 'JSON should contain at least one section with content');
	finally
		jo.Free;
	end;
end;

procedure TJsonSettingsTest.FillSectionObject(JsonObject : TJSONObject; var Section : string; Values : TStringList);
var
	sKey : string;
	sValue : string;
	sectionObject : TJSONObject;
	arrObj : TJSONArray;
begin
	sectionObject := TJSONObject.Create;
	arrObj := TJSONArray.Create;
	try
		// Add key-value pairs to the section JSON object
		for var j := 0 to Values.Count - 1 do begin
			sKey := Values.Names[j];
			sValue := Values.ValueFromIndex[j];
			if sKey.StartsWith('Item_') then begin
				arrObj.Add(sValue);
			end else begin
				AddValueByType(sectionObject, sKey, sValue);
			end;
		end;

		if arrObj.Count > 0 then begin
			JsonObject.AddPair(Section, arrObj);
			// Don't free arrObj here as it's now owned by JsonObject
			arrObj := nil; // Prevent freeing in finally block
		end else begin
			JsonObject.AddPair(Section, sectionObject);
			// Don't free sectionObject here as it's now owned by JsonObject
			sectionObject := nil; // Prevent freeing in finally block
		end;

	finally
		if arrObj <> nil then
			arrObj.Free;
		if sectionObject <> nil then
			sectionObject.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TJsonSettingsTest);

end.

