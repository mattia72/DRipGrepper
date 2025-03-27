unit RipGrepper.Settings.FontColorsTest;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Settings.FontColors,
	System.UITypes;

type

	[TestFixture]
	TDefaultFontColorsTest = class
		public
			[Test]
			procedure TestSetDefault;
			[Test]
			procedure TestCreateLightTheme;
			[Test]
			procedure TestCreateDarkTheme();
	end;

implementation

uses
	RipGrepper.Helper.UI.DarkMode,
	ArrayEx,
	System.StrUtils,
	System.SysUtils;

procedure TDefaultFontColorsTest.TestSetDefault;
var
	defaultFontColors : TDefaultFontColors;
	fontAttributes : TFontAttributes;
	defaultColors : TArrayEx<TNameColorDef>;
	name, expectedValues : string;
begin
	// Arrange
	defaultFontColors := TDefaultFontColors.Create(EThemeMode.tmLight);
	try
		defaultColors := TDefaultFontColors.DEFAULT_COLORS;

		for var entry : TNameColorDef in defaultColors do begin
			name := entry.Name;
			expectedValues := entry.ColorDef;

 			// Act
			defaultFontColors.SetDefault(name, fontAttributes);

			// Assert
			Assert.AreEqual(expectedValues, fontAttributes.ToString,
				Format('Font attributes for "%s" do not match expected values', [name]));
		end;

	finally
		defaultFontColors.Free;
	end;
end;

procedure TDefaultFontColorsTest.TestCreateLightTheme;
var
	defaultFontColors : TDefaultFontColors;
	fontColors : TFontColors;
	defaultColors : TArrayEx<TNameColorDef>;
	name, expectedValues : string;
begin
	// Test for Light Theme
	defaultFontColors := TDefaultFontColors.Create(EThemeMode.tmLight);
	try
		defaultColors := TDefaultFontColors.DEFAULT_COLORS;
		fontColors.SetDefaultColors(defaultFontColors);

		for var entry : TNameColorDef in defaultColors do begin
			name := entry.Name;
			expectedValues := entry.ColorDef;

			// Get the initialized font attributes
			var
			actual := fontColors.GetByName(name).ToString;

			// Assert
			Assert.AreEqual(expectedValues, actual, Format('Font attributes for "%s" do not match expected values in Light Theme', [name]));
		end;
	finally
		defaultFontColors.Free;
	end;
end;

procedure TDefaultFontColorsTest.TestCreateDarkTheme();
var
	defaultFontColors : TDefaultFontColors;
	fontColors : TFontColors;
	defaultColors : TArrayEx<TNameColorDef>;
	name, expectedValues : string;
begin
	// Test for Light Theme
	defaultFontColors := TDefaultFontColors.Create(EThemeMode.tmDark);
	try
		defaultColors := TDefaultFontColors.DEFAULT_DARK_COLORS;
		fontColors.SetDefaultColors(defaultFontColors);

		for var entry : TNameColorDef in defaultColors do begin
			name := entry.Name;
			expectedValues := entry.ColorDef;

			// Get the initialized font attributes
			var
			actual := fontColors.GetByName(name).ToString;

			// Assert
			Assert.AreEqual(expectedValues, actual, Format('Font attributes for "%s" do not match expected values in Light Theme', [name]));
		end;
	finally
		defaultFontColors.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TDefaultFontColorsTest);

end.
