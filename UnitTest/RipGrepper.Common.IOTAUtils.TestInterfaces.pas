// This file is no longer needed
// The original approach with RTTI-enabled test interfaces 
// was too complex for external ToolsAPI interfaces.
// 
// To enable RTTI for external units like ToolsAPI in Delphi:
// 1. You cannot add {$M+} to external units you don't control
// 2. Instead, create wrapper interfaces/classes in your own code
// 3. Use adapter pattern to bridge between external and testable interfaces
// 4. Or use simpler testing approaches that don't require RTTI
//
// For this specific case with IOTAProjectOptions, the solution was
// to simplify the tests to not require complex mocking of external interfaces.

unit RipGrepper.Common.IOTAUtils.TestInterfaces;

interface

// Empty interface - not used anymore

implementation

end.
