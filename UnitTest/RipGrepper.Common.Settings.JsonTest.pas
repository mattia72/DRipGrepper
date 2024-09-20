unit RipGrepper.Common.Settings.JsonTest;

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
			procedure ConvertIniTest;
			[Test]
			procedure ReadJsonTest;
//			[Test]
			procedure RootDTOTest;
	end;

implementation

uses
	System.SysUtils,
	System.IniFiles,

	System.IOUtils,
	Vcl.Forms,
	RipGrepper.Common.Constants,
	RootUnit;

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
	CreateTestJson;
	Assert.IsTrue(TFile.Exists('DripGrepper.json'), 'Json should be exist.');
end;

function TJsonSettingsTest.CreateTestJson : string;
begin
	var
	dir := TPath.GetDirectoryName(Application.ExeName);
	Result := TPath.Combine(dir, 'DripGrepper.json');
	Convert(TPath.Combine(dir, 'DripGrepper.ini'), Result);
end;

procedure TJsonSettingsTest.ReadJsonTest;
var
	jo : TJsonObject;
begin
	var
	jsonPath := CreateTestJson;
	jo := TJSONObject.ParseJSONValue(TFile.ReadAllText(jsonPath)) as TJSONObject;
	try
		Assert.IsTrue(jo.P['RipGrepperSettings'].P['DebugTrace'].GetValue<Boolean>('Value'), 'DebugTrace should be exist.');
		Assert.IsTrue(jo.P['RipGrepperSettings'].P['DebugTrace'].GetValue<Boolean>('Value'), 'DebugTrace should be exist.');
		Assert.IsTrue(jo.P['RipGrepperSettings'].P['DebugTrace'].GetValue<Boolean>('NotAValidValue', True), 'DebugTrace should be exist.');
	finally
		jo.Free;
	end;
end;

procedure TJsonSettingsTest.FillSectionObject(JsonObject : TJSONObject; var Section : string; Values : TStringList);
var
	sKey : string;
	sValue : string;
	arrObj : TJSONArray;
begin
	var
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
				sKey := sKey.Replace(DEFAULT_KEY, '');
				AddValueByType(sectionObject, sKey, sValue);
			end;
		end;

		if arrObj.Count > 0 then begin
			JsonObject.AddPair(Section, TJsonObject.ParseJSONValue(arrObj.ToJSON));
		end else begin
			JsonObject.AddPair(Section, TJsonObject.ParseJSONValue(sectionObject.ToJSON));
		end;

	finally
		arrObj.Free;
		sectionObject.Free;
	end;

end;

procedure TJsonSettingsTest.RootDTOTest;
var
	Root : TRoot;
begin
	Root := TRoot.Create;
	try
		var
		jsonPath := CreateTestJson;
		Root.AsJson := TFile.ReadAllText(jsonPath);
		Assert.IsTrue(Root.RipGrepperSettings.DebugTrace.Value, 'DebugTrace should be true');

	finally
		Root.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TJsonSettingsTest);

end.
