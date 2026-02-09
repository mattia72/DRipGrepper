# TabSeparatedConfigForm Test Project

This test project allows you to test the `TTabSeparatedConfigForm` independently from the main RipGrepper application.

## Files

- **TabSeparatedConfigTestProject.dpr** - Main project file
- **TabSeparatedConfigTestProject.dproj** - Delphi project configuration
- **TabSeparatedConfigTestForm.pas/.dfm** - Main test form

## How to Use

1. Open `TabSeparatedConfigTestProject.dproj` in Delphi IDE
2. Compile and run the project (F9)
3. The test application will open with the following features:

### Test Form Features

- **Input Data field**: Enter tab-separated data (one row per line)
  - Sample data is pre-loaded for testing
  - Each row should contain values separated by TAB characters

- **Column Headers field**: Enter comma-separated column headers
  - Example: `Name,Email,Position`
  - These headers will be displayed in the TVirtualStringTree

- **Enable Test Action checkbox**: 
  - When checked, the test button will be visible in the config form
  - Demonstrates the optional TestAction functionality

- **Open Tab Config Form button**: Opens the actual `TTabSeparatedConfigForm`
  - Use the toolbar buttons to add, remove, move rows
  - Test the optional test button (if enabled)
  - Click OK to save changes or Cancel to discard

- **Results field**: Shows the modified data after closing the config form with OK

## Testing Scenarios

### Basic Testing
1. Use the pre-loaded sample data
2. Click "Open Tab Config Form"
3. Try adding, removing, and reordering rows
4. Click OK and verify results

### Test Action
1. Check "Enable Test Action"
2. Open the config form
3. The test button (rocket icon) should now be visible
4. Select a row and click the test button

### Custom Headers
1. Modify the column headers (e.g., "First,Last,Company,Email")
2. Adjust input data to match (use TAB as separator)
3. Open the form and verify headers are displayed correctly

### Edge Cases
- Empty input data
- Single column
- Many columns
- Special characters in data

## Dependencies

The test project requires:
- VirtualTreeView component
- SVGIconImageList component
- Various RipGrepper helper units (included in uses clause)

## Notes

- The form uses TAB character (#9) as the separator
- The TestAction parameter is optional (can be nil)
- The Settings parameter can be nil for testing purposes
- The form is derived from `TSettingsBaseForm` but works standalone
