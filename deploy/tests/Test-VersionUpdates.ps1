#!/usr/bin/env pwsh

<#
.SYNOPSIS
    Pester unit tests to verify Update-VersionInfoInProjects.ps1 behavior with different parameters.

.DESCRIPTION
    This Pester test suite tests various parameter combinations of the Update-VersionInfoInProjects.ps1 script
    to ensure version increments and specific version setting work correctly.
    
    Run with: Invoke-Pester -Path "Test-VersionUpdates.ps1"
#>

# Import Pester if not already loaded
if (-not (Get-Module -Name Pester)) {
    Import-Module Pester -Force
}

# Global variables for test files (Pester 3.x compatible)
$script:TestDir = $PSScriptRoot
$script:TestProjectFile = Join-Path $TestDir "TestProject.dproj"
$script:OriginalProjectFile = Join-Path (Split-Path (Split-Path $TestDir -Parent) -Parent) "src\Project\Delphi11\DRipGrepper.dproj"
$script:UpdateScript = Join-Path (Split-Path $TestDir -Parent) "Update-VersionInfoInProjects.ps1"

# Helper function to get version from project file

function Get-VersionFromProject {
    param([string]$ProjectPath)
    
    try {
        [xml]$xmlDoc = Get-Content $ProjectPath
        
        # Find PropertyGroup with version info using the same robust method as the script
        foreach ($group in $xmlDoc.Project.PropertyGroup) {
            if ($group -and ($group.GetElementsByTagName("VerInfo_MajorVer").Count -gt 0 -or 
                            $group.GetElementsByTagName("VerInfo_MinorVer").Count -gt 0 -or 
                            $group.GetElementsByTagName("VerInfo_Release").Count -gt 0 -or 
                            $group.GetElementsByTagName("VerInfo_Build").Count -gt 0)) {
                
                $majorElement = $group.GetElementsByTagName("VerInfo_MajorVer") | Select-Object -First 1
                $minorElement = $group.GetElementsByTagName("VerInfo_MinorVer") | Select-Object -First 1
                $releaseElement = $group.GetElementsByTagName("VerInfo_Release") | Select-Object -First 1
                $buildElement = $group.GetElementsByTagName("VerInfo_Build") | Select-Object -First 1
                
                $major = if ($majorElement) { $majorElement.InnerText } else { "0" }
                $minor = if ($minorElement) { $minorElement.InnerText } else { "0" }
                $release = if ($releaseElement) { $releaseElement.InnerText } else { "0" }
                $build = if ($buildElement) { $buildElement.InnerText } else { "0" }
                
                return @{
                    Major = [int]$major
                    Minor = [int]$minor
                    Release = [int]$release
                    Build = [int]$build
                    String = "$major.$minor.$release.$build"
                }
            }
        }
    }
    catch {
        Write-Error "Failed to parse version from ${ProjectPath}: $($_.Exception.Message)"
    }
    
    return @{ Major = 0; Minor = 0; Release = 0; Build = 0; String = "0.0.0.0" }
}

function Reset-TestProject {
    Copy-Item $script:OriginalProjectFile $script:TestProjectFile -Force
    $version = Get-VersionFromProject $script:TestProjectFile
    return $version
}

Describe "Update-VersionInfoInProjects Tests" {
    
    BeforeEach {
        # Ensure test project exists and is reset to original state
        if (-not (Test-Path $script:TestProjectFile)) {
            Copy-Item $script:OriginalProjectFile $script:TestProjectFile -Force
        }
        Reset-TestProject | Out-Null
    }
    
    AfterEach {
        # Clean up test files if they exist (Pester 3.x uses AfterEach instead of AfterAll)
        if (Test-Path $script:TestProjectFile) {
            Remove-Item $script:TestProjectFile -Force -ErrorAction SilentlyContinue
        }
    }

    Context "Version Increment Tests" {
        
        It "Should increment major version and reset minor and release to 0" {
            # Arrange
            $startVersion = Reset-TestProject
            
            # Act
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -VersionType Major | Out-Null
            $newVersion = Get-VersionFromProject $script:TestProjectFile
            
            # Assert
            $LASTEXITCODE | Should Be 0
            $newVersion.Major | Should Be ($startVersion.Major + 1)
            $newVersion.Minor | Should Be 0
            $newVersion.Release | Should Be 0
            $newVersion.Build | Should Be $startVersion.Build # Should be preserved
        }

        It "Should increment minor version and reset release to 0" {
            # Arrange
            $startVersion = Reset-TestProject
            
            # Act
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -VersionType Minor | Out-Null
            $newVersion = Get-VersionFromProject $script:TestProjectFile
            
            # Assert
            $LASTEXITCODE | Should Be 0
            $newVersion.Major | Should Be $startVersion.Major
            $newVersion.Minor | Should Be ($startVersion.Minor + 1)
            $newVersion.Release | Should Be 0
            $newVersion.Build | Should Be $startVersion.Build # Should be preserved
        }

        It "Should increment release version only" {
            # Arrange
            $startVersion = Reset-TestProject
            
            # Act
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -VersionType Release | Out-Null
            $newVersion = Get-VersionFromProject $script:TestProjectFile
            
            # Assert
            $LASTEXITCODE | Should Be 0
            $newVersion.Major | Should Be $startVersion.Major
            $newVersion.Minor | Should Be $startVersion.Minor
            $newVersion.Release | Should Be ($startVersion.Release + 1)
            $newVersion.Build | Should Be $startVersion.Build # Should be preserved
        }
    }

    Context "Specific Version Set Tests" {
        
        It "Should set 3-part version and preserve build number" {
            # Arrange
            $startVersion = Reset-TestProject
            $targetVersion = "5.1.3"
            
            # Act
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -Version $targetVersion | Out-Null
            $newVersion = Get-VersionFromProject $script:TestProjectFile
            
            # Assert
            $LASTEXITCODE | Should Be 0
            $newVersion.Major | Should Be 5
            $newVersion.Minor | Should Be 1
            $newVersion.Release | Should Be 3
            $newVersion.Build | Should Be $startVersion.Build # Should be preserved
        }

        It "Should set 4-part version completely" {
            # Arrange
            $startVersion = Reset-TestProject
            $targetVersion = "6.2.4.100"
            
            # Act
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -Version $targetVersion | Out-Null
            $newVersion = Get-VersionFromProject $script:TestProjectFile
            
            # Assert
            $LASTEXITCODE | Should Be 0
            $newVersion.Major | Should Be 6
            $newVersion.Minor | Should Be 2
            $newVersion.Release | Should Be 4
            $newVersion.Build | Should Be 100
        }

        It "Should set version with zeros and preserve build" {
            # Arrange
            $startVersion = Reset-TestProject
            $targetVersion = "1.0.0"
            
            # Act
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -Version $targetVersion | Out-Null
            $newVersion = Get-VersionFromProject $script:TestProjectFile
            
            # Assert
            $LASTEXITCODE | Should Be 0
            $newVersion.Major | Should Be 1
            $newVersion.Minor | Should Be 0
            $newVersion.Release | Should Be 0
            $newVersion.Build | Should Be $startVersion.Build # Should be preserved
        }
    }

    Context "Build Number Validation Tests" {
        
        It "Should set specific build number with 4-part version" {
            # Arrange
            $startVersion = Reset-TestProject
            $targetVersion = "2.5.7.999"
            
            # Act
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -Version $targetVersion | Out-Null
            $newVersion = Get-VersionFromProject $script:TestProjectFile
            
            # Assert
            $LASTEXITCODE | Should Be 0
            $newVersion.Major | Should Be 2
            $newVersion.Minor | Should Be 5
            $newVersion.Release | Should Be 7
            $newVersion.Build | Should Be 999 # Should be set to specific value
        }

        It "Should preserve build number across major/minor/release increments" {
            # Arrange
            $startVersion = Reset-TestProject
            $originalBuild = $startVersion.Build
            
            # Act - Test major increment preserves build
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -VersionType Major | Out-Null
            $majorVersion = Get-VersionFromProject $script:TestProjectFile
            
            # Reset for minor test
            Reset-TestProject | Out-Null
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -VersionType Minor | Out-Null
            $minorVersion = Get-VersionFromProject $script:TestProjectFile
            
            # Reset for release test
            Reset-TestProject | Out-Null
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -VersionType Release | Out-Null
            $releaseVersion = Get-VersionFromProject $script:TestProjectFile
            
            # Assert
            $majorVersion.Build | Should Be $originalBuild
            $minorVersion.Build | Should Be $originalBuild
            $releaseVersion.Build | Should Be $originalBuild
        }

        It "Should handle different build number formats correctly" {
            # Arrange
            $startVersion = Reset-TestProject
            
            # Test different build number scenarios
            $testCases = @(
                @{ Version = "1.2.3.0"; ExpectedBuild = 0 }
                @{ Version = "1.2.3.1"; ExpectedBuild = 1 }
                @{ Version = "1.2.3.999"; ExpectedBuild = 999 }
                @{ Version = "1.2.3.65535"; ExpectedBuild = 65535 } # Max uint16 value
            )
            
            foreach ($testCase in $testCases) {
                # Act
                & $script:UpdateScript -ProjectFiles $script:TestProjectFile -Version $testCase.Version | Out-Null
                $newVersion = Get-VersionFromProject $script:TestProjectFile
                
                # Assert
                $newVersion.Build | Should Be $testCase.ExpectedBuild
                
                # Reset for next test
                Reset-TestProject | Out-Null
            }
        }
    }

    Context "Chained Operations Tests" {
        
        It "Should handle multiple increments in sequence correctly" {
            # Arrange
            $startVersion = Reset-TestProject
            
            # Act & Assert - Minor increment
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -VersionType Minor | Out-Null
            $version1 = Get-VersionFromProject $script:TestProjectFile
            $version1.Major | Should Be $startVersion.Major
            $version1.Minor | Should Be ($startVersion.Minor + 1)
            $version1.Release | Should Be 0
            $version1.Build | Should Be $startVersion.Build # Build should be preserved
            
            # Act & Assert - Release increment
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -VersionType Release | Out-Null
            $version2 = Get-VersionFromProject $script:TestProjectFile
            $version2.Major | Should Be $startVersion.Major
            $version2.Minor | Should Be ($startVersion.Minor + 1)
            $version2.Release | Should Be 1
            $version2.Build | Should Be $startVersion.Build # Build should be preserved
            
            # Act & Assert - Release increment again
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -VersionType Release | Out-Null
            $version3 = Get-VersionFromProject $script:TestProjectFile
            $version3.Major | Should Be $startVersion.Major
            $version3.Minor | Should Be ($startVersion.Minor + 1)
            $version3.Release | Should Be 2
            $version3.Build | Should Be $startVersion.Build # Build should be preserved
            
            # Act & Assert - Major increment (should reset minor and release)
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -VersionType Major | Out-Null
            $version4 = Get-VersionFromProject $script:TestProjectFile
            $version4.Major | Should Be ($startVersion.Major + 1)
            $version4.Minor | Should Be 0
            $version4.Release | Should Be 0
            $version4.Build | Should Be $startVersion.Build # Build should be preserved
        }

        It "Should handle version changes while preserving specific build numbers" {
            # Arrange
            $startVersion = Reset-TestProject
            
            # Act & Assert - Set a specific build number first
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -Version "1.0.0.500" | Out-Null
            $version1 = Get-VersionFromProject $script:TestProjectFile
            $version1.Major | Should Be 1
            $version1.Minor | Should Be 0
            $version1.Release | Should Be 0
            $version1.Build | Should Be 500
            
            # Act & Assert - Release increment should preserve build number
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -VersionType Release | Out-Null
            $version2 = Get-VersionFromProject $script:TestProjectFile
            $version2.Major | Should Be 1
            $version2.Minor | Should Be 0
            $version2.Release | Should Be 1
            $version2.Build | Should Be 500 # Build should be preserved
            
            # Act & Assert - Minor increment should reset release and preserve build
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -VersionType Minor | Out-Null
            $version3 = Get-VersionFromProject $script:TestProjectFile
            $version3.Major | Should Be 1
            $version3.Minor | Should Be 1
            $version3.Release | Should Be 0
            $version3.Build | Should Be 500 # Build should still be preserved
            
            # Act & Assert - Major increment should reset minor/release and preserve build
            & $script:UpdateScript -ProjectFiles $script:TestProjectFile -VersionType Major | Out-Null
            $version4 = Get-VersionFromProject $script:TestProjectFile
            $version4.Major | Should Be 2
            $version4.Minor | Should Be 0
            $version4.Release | Should Be 0
            $version4.Build | Should Be 500 # Build should still be preserved
        }
    }
}
