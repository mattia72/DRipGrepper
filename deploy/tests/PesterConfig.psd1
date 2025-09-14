@{
    Run = @{
        Path = @('Test-VersionUpdates.ps1')
        PassThru = $true
    }
    Should = @{
        ErrorAction = 'Stop'
    }
    CodeCoverage = @{
        Enabled = $false
    }
    TestResult = @{
        Enabled = $true
        OutputFormat = 'NUnitXml'
        OutputPath = 'TestResults.xml'
    }
    Output = @{
        Verbosity = 'Detailed'
    }
}
