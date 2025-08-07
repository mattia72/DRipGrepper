#Requires -Version 5.1

<#
.SYNOPSIS
    Updates version information in Delphi project files and CHANGELOG.md

.DESCRIPTION
    This script manages version numbers across DRipGrepper project files (.dproj) and CHANGELOG.md.
    It can increment Major, Minor, or Release versions and optionally update only the CHANGELOG.

.PARAMETER Major
    Increment the major version number (e.g., 4.10.2 -> 5.0.0)

.PARAMETER Minor
    Increment the minor version number (e.g., 4.10.2 -> 4.11.0)

.PARAMETER Release
    Increment the release/patch version number (e.g., 4.10.2 -> 4.10.3)

.PARAMETER InCHANGELOGOnly
    Only update CHANGELOG.md, do not modify .dproj files

.PARAMETER Suffix
    Version suffix to append (default: "-beta")

.EXAMPLE
    .\Update-Version.ps1 -Minor
    Updates minor version in all project files and CHANGELOG

.EXAMPLE
    .\Update-Version.ps1 -Release -InCHANGELOGOnly
    Updates only CHANGELOG.md with incremented release version

.EXAMPLE
    .\Update-Version.ps1 -Minor -Suffix "-rc1"
    Updates minor version with release candidate suffix

.NOTES
    Author: DRipGrepper Team
    Date: 2025-07-31
    Version: 1.0.0
#>

[CmdletBinding(SupportsShouldProcess)]
param(
    [switch]$Major,
    [switch]$Minor, 
    [switch]$Release,
    [switch]$InCHANGELOGOnly,
    [string]$Suffix = "-beta"
)

# Set strict mode for better error handling
Set-StrictMode -Version Latest

# Script configuration
$Script:RootPath = Split-Path -Parent $PSScriptRoot
$Script:ChangelogPath = Join-Path $RootPath "CHANGELOG.md"

# Project file patterns to update
$Script:ProjectFiles = @(
    "src\Project\Delphi11\DRipGrepper.dproj",
    "src\Project\Delphi12\DRipGrepper.dproj",
    "Extension\src\Project\Bpl.Delphi11\DRipExtension.dproj",
    "Extension\src\Project\Bpl.Delphi12\DRipExtension.dproj",
    "Extension\src\Project\Dll.Delphi11\DRipExtensions.D11.dproj",
    "Extension\src\Project\Dll.Delphi12\DRipExtensions.D12.dproj",
    "UnitTest\DRipGrepperUnittest.D11.dproj",
    "UnitTest\DRipGrepperUnittest.D12.dproj"
)

function Write-ColoredMessage {
    param(
        [string]$Message,
        [string]$Color = "White"
    )
    Write-Host $Message -ForegroundColor $Color
}

function Get-CurrentVersionFromChangelog {
    <#
    .SYNOPSIS
        Extracts the current version from CHANGELOG.md
    #>
    
    if (-not (Test-Path $Script:ChangelogPath)) {
        throw "CHANGELOG.md not found at: $Script:ChangelogPath"
    }
    
    $content = Get-Content $Script:ChangelogPath -Raw
    
    # Look for version pattern like [v4.10.2-beta]
    if ($content -match '\[v(\d+)\.(\d+)\.(\d+)(?:-\w+)?\]') {
        return @{
            Major = [int]$Matches[1]
            Minor = [int]$Matches[2] 
            Release = [int]$Matches[3]
            FullVersion = "$($Matches[1]).$($Matches[2]).$($Matches[3])"
        }
    }
    
    throw "Could not parse version from CHANGELOG.md"
}

function Get-NextVersion {
    param(
        [hashtable]$CurrentVersion
    )
    
    $newVersion = $CurrentVersion.Clone()
    
    if ($Major) {
        $newVersion.Major++
        $newVersion.Minor = 0
        $newVersion.Release = 0
        Write-ColoredMessage "Incrementing MAJOR version" "Yellow"
    }
    elseif ($Minor) {
        $newVersion.Minor++
        $newVersion.Release = 0
        Write-ColoredMessage "Incrementing MINOR version" "Yellow"
    }
    elseif ($Release) {
        $newVersion.Release++
        Write-ColoredMessage "Incrementing RELEASE version" "Yellow"
    }
    else {
        throw "Please specify -Major, -Minor, or -Release"
    }
    
    $newVersion.FullVersion = "$($newVersion.Major).$($newVersion.Minor).$($newVersion.Release)"
    return $newVersion
}

function Update-ChangelogVersion {
    param(
        [hashtable]$OldVersion,
        [hashtable]$NewVersion,
        [string]$Suffix
    )
    
    if (-not (Test-Path $Script:ChangelogPath)) {
        throw "CHANGELOG.md not found"
    }
    
    $content = [string](Get-Content $Script:ChangelogPath -Raw)
    $today = Get-Date -Format "yyyy-MM-dd"
    
    # Create new version entry
    $newEntry = @"
# Changelog

## [v$($NewVersion.FullVersion)$Suffix] - $today

### 💥 Added

### 🔄 Changed
- Version updated to $($NewVersion.FullVersion)

### 🐞 Fixed

"@
    
    # Replace the changelog and insert new entry before the first existing version
    $regexPattern = '(?s)(^# Changelog.*?)(## \[v\d[\d\.]+.*?\].*?)$'
    if ($content -match $regexPattern) {
        $updatedContent = $content -replace $regexPattern, "$newEntry`$2"
    } else { # Fallback: just replace the header 
        $updatedContent = $content -replace '(?s)^# Changelog', $newEntry
    }
    
    if ($PSCmdlet.ShouldProcess($Script:ChangelogPath, "Update version to $($NewVersion.FullVersion)")) {
        Set-Content -Path $Script:ChangelogPath -Value $updatedContent -Encoding UTF8
        Write-ColoredMessage "✓ Updated CHANGELOG.md with version $($NewVersion.FullVersion)" "Green"
    }
}

function Update-ProjectFileVersion {
    param(
        [string]$ProjectFile,
        [hashtable]$NewVersion,
        [string]$Suffix
    )
    
    $fullPath = Join-Path $Script:RootPath $ProjectFile
    
    if (-not (Test-Path $fullPath)) {
        Write-ColoredMessage "⚠ Project file not found: $ProjectFile" "Yellow"
        return
    }
    
    try {
        $content = Get-Content $fullPath -Raw
        $modified = $false
        
        # Update all version components
        $patterns = @{
            'VerInfo_MajorVer' = $NewVersion.Major
            'VerInfo_MinorVer' = $NewVersion.Minor
            'VerInfo_Release' = $NewVersion.Release
        }
        
        foreach ($pattern in $patterns.Keys) {
            $regex = "(<$pattern>)\d+(</VerInfo_\w+>)"
            if ($content -match $regex) {
                $newValue = $patterns[$pattern]
                $content = $content -replace $regex, "`${1}$newValue`${2}"
                $modified = $true
            }
        }
        
        # Update FileVersion and ProductVersion in VerInfo_Keys
        $versionString = "$($NewVersion.Major).$($NewVersion.Minor).$($NewVersion.Release)"
        
        # Update each VerInfo_Keys line individually - be more specific to avoid capturing too much
        $content = $content -replace '(<VerInfo_Keys>[^<>]*FileVersion=)[\d\.]+\.(\d+)', "`${1}$versionString.`${2}"
        $content = $content -replace '(<VerInfo_Keys>[^<>]*ProductVersion=)[\d\.]+', "`${1}$versionString"
        
        # Update Comments field with suffix within VerInfo_Keys
        $suffixComment = if ($Suffix) { $Suffix.TrimStart('-') } else { "beta" }
        $content = $content -replace '(<VerInfo_Keys>[^<>]*Comments=)[^;<>]*', "`${1}$suffixComment"
        
        if ($modified -and $PSCmdlet.ShouldProcess($fullPath, "Update version numbers")) {
            Set-Content -Path $fullPath -Value $content -Encoding UTF8
            Write-ColoredMessage "✓ Updated $ProjectFile" "Green"
        }
        elseif (-not $modified) {
            Write-ColoredMessage "⚠ No version info found in $ProjectFile" "Yellow"
        }
    }
    catch {
        Write-Error "Failed to update $ProjectFile`: $_"
    }
}

function Show-Summary {
    param(
        [hashtable]$OldVersion,
        [hashtable]$NewVersion
    )
    
    Write-Host ""
    Write-ColoredMessage "=== VERSION UPDATE SUMMARY ===" "Cyan"
    Write-ColoredMessage "Old Version: $($OldVersion.FullVersion)" "White"
    Write-ColoredMessage "New Version: $($NewVersion.FullVersion)$Suffix" "Green"
    Write-Host ""
    
    if ($InCHANGELOGOnly) {
        Write-ColoredMessage "Updated: CHANGELOG.md only" "Yellow"
    } else {
        Write-ColoredMessage "Updated: All project files and CHANGELOG.md" "Green"
    }
    Write-Host ""
}

# Main execution
try {
    Write-ColoredMessage "=== DRipGrepper Version Updater ===" "Cyan"
    Write-Host ""
    
    # Validate parameters
    $versionSwitches = @()
    if ($Major) { $versionSwitches += "Major" }
    if ($Minor) { $versionSwitches += "Minor" }
    if ($Release) { $versionSwitches += "Release" }
    
    if ($versionSwitches.Count -ne 1) {
        throw "Please specify exactly one version increment: -Major, -Minor, or -Release"
    }
    
    # Get current version from CHANGELOG
    Write-ColoredMessage "Reading current version from CHANGELOG.md..." "White"
    $currentVersion = Get-CurrentVersionFromChangelog
    Write-ColoredMessage "Current version: $($currentVersion.FullVersion)" "White"
    
    # Calculate new version
    $newVersion = Get-NextVersion -CurrentVersion $currentVersion
    Write-ColoredMessage "New version: $($newVersion.FullVersion)$Suffix" "Green"
    Write-Host ""
    
    # Update CHANGELOG.md
    Update-ChangelogVersion -OldVersion $currentVersion -NewVersion $newVersion -Suffix $Suffix
    
    # Update project files (unless InCHANGELOGOnly is specified)
    if (-not $InCHANGELOGOnly) {
        Write-Host ""
        Write-ColoredMessage "Updating project files..." "White"
        
        foreach ($projectFile in $Script:ProjectFiles) {
            Update-ProjectFileVersion -ProjectFile $projectFile -NewVersion $newVersion -Suffix $Suffix
        }
    }
    
    # Show summary
    Show-Summary -OldVersion $currentVersion -NewVersion $newVersion
    
    Write-ColoredMessage "✓ Version update completed successfully!" "Green"
}
catch {
    Write-Error "Version update failed: $_"
    exit 1
}
