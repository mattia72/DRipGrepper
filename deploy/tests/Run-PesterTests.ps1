#!/usr/bin/env pwsh

<#
.SYNOPSIS
    Run Pester unit tests for the version update functionality.

.DESCRIPTION
    This script runs the Pester unit tests for Update-VersionInfoInProjects.ps1.
    It will install Pester if not present and execute all tests.
    Compatible with Pester 3.x (which doesn't support -Configuration parameter).

.PARAMETER Configuration
    Path to Pester configuration file. Defaults to PesterConfig.psd1 in the same directory.
    Note: For Pester 3.x, only basic settings like OutputFormat and OutputPath are used.

.EXAMPLE
    .\Run-PesterTests.ps1
    Runs all tests with default settings.

.EXAMPLE
    .\Run-PesterTests.ps1 -Configuration "MyPesterConfig.psd1"
    Runs tests with settings from the configuration file (Pester 3.x compatible subset).
#>

[CmdletBinding()]
param(
    [string]$Configuration = (Join-Path $PSScriptRoot "PesterConfig.psd1")
)

# Ensure Pester is available
if (-not (Get-Module -ListAvailable -Name Pester)) {
    Write-Host "Installing Pester module..." -ForegroundColor Yellow
    Install-Module -Name Pester -Force -Scope CurrentUser -SkipPublisherCheck
}

# Import Pester
Import-Module Pester -Force

# Run tests
Write-Host "Running Pester tests for version update functionality..." -ForegroundColor Cyan

# Pester 3.x doesn't support -Configuration parameter, use individual parameters instead
$testPath = Join-Path $PSScriptRoot "Test-VersionUpdates.ps1"

if (Test-Path $Configuration) {
    # Read configuration file for Pester 3.x compatible parameters
    $config = Import-PowerShellDataFile -Path $Configuration
    
    # Extract relevant settings for Pester 3.x
    $outputFormat = if ($config.TestResult.OutputFormat) { $config.TestResult.OutputFormat } else { "NUnitXml" }
    $outputPath = if ($config.TestResult.OutputPath) { 
        Join-Path $PSScriptRoot $config.TestResult.OutputPath 
    } else { 
        Join-Path $PSScriptRoot "TestResults.xml" 
    }
    
    # Run with available Pester 3.x parameters
    $result = Invoke-Pester -Path $testPath -PassThru -OutputFormat $outputFormat -OutputFile $outputPath
} else {
    # Fallback to simple test run
    $result = Invoke-Pester -Path $testPath -PassThru
}

# Display results summary
Write-Host ""
Write-Host ("=" * 60) -ForegroundColor Cyan
Write-Host "TEST RESULTS SUMMARY" -ForegroundColor Cyan
Write-Host ("=" * 60) -ForegroundColor Cyan
Write-Host "Total Tests: $($result.TotalCount)" -ForegroundColor White
Write-Host "Passed: $($result.PassedCount)" -ForegroundColor Green
Write-Host "Failed: $($result.FailedCount)" -ForegroundColor Red
Write-Host "Skipped: $($result.SkippedCount)" -ForegroundColor Yellow

if ($result.FailedCount -eq 0) {
    Write-Host ""
    Write-Host "🎉 ALL TESTS PASSED! 🎉" -ForegroundColor Green
    Remove-Item -Path $outputPath -ErrorAction SilentlyContinue
    exit 0
} else {
    Write-Host ""
    Write-Host "⚠️  SOME TESTS FAILED ⚠️" -ForegroundColor Red
    Write-Host "Please review the test output above." -ForegroundColor Red
    # find the errorneous tests in output file if exists
    if (Test-Path $outputPath) {
        Write-Host ""
        $xml = [xml](Get-Content $outputPath)
        $xml.SelectNodes("//test-case[@result='Failure']").ForEach({
            Write-Host "Test Failed: $($_.name)" -ForegroundColor Red
            Write-Host "Message: $($_.failure.message)" -ForegroundColor DarkGray
            Write-Host "StackTrace: $($_.failure.stacktrace)" -ForegroundColor DarkGray
        })
    }
    exit 1
}
