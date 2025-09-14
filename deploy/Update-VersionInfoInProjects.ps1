#!/usr/bin/env pwsh

<#
.SYNOPSIS
    Updates VerInfo tags in Delphi project files by incrementing version numbers.

.DESCRIPTION
    This script updates version information in Delphi project files (.dproj) by incrementing
    major, minor, or release version numbers. It handles both individual version properties
    (VerInfo_MajorVer, VerInfo_MinorVer, VerInfo_Release) and version strings in VerInfo_Keys.

.PARAMETER ProjectFiles
    Array of Delphi project file paths (.dproj files) to update. If not specified, 
    the script will search for all .dproj files in the current directory and subdirectories.

.PARAMETER VersionType
    Type of version increment to perform. Valid values are:
    - Major: Increment major version (resets minor and release to 0)
    - Minor: Increment minor version (resets release to 0)
    - Release: Increment release version

.PARAMETER Version
    Specific version to set in format "Major.Minor.Release" or "Major.Minor.Release.Build".
    Cannot be used together with VersionType.

.PARAMETER ResetBuild
    If specified, resets the build number to 0 when incrementing version.

.PARAMETER WhatIf
    Shows what changes would be made without actually modifying the files.

.PARAMETER Verbose
    Provides detailed output about the changes being made.

.EXAMPLE
    .\Update-VersionInfo.ps1 -VersionType Major
    
    Increments the major version in all .dproj files found in current directory and subdirectories.

.EXAMPLE
    .\Update-VersionInfo.ps1 -ProjectFiles "src\Project\Delphi12\DRipGrepper.dproj" -VersionType Minor -ResetBuild
    
    Increments the minor version and resets build number in the specified project file.

.EXAMPLE
    .\Update-VersionInfo.ps1 -Version "2.5.0"
    
    Sets the version to 2.5.0 in all .dproj files found in current directory and subdirectories.

.EXAMPLE
    .\Update-VersionInfo.ps1 -Version "2.5.0.100" -ProjectFiles "src\Project\Delphi12\DRipGrepper.dproj"
    
    Sets the specific version including build number in the specified project file.

.EXAMPLE
    .\Update-VersionInfo.ps1 -VersionType Release -WhatIf

    Shows what release version changes would be made without actually modifying files.

.NOTES
    Author: GitHub Copilot
    Version: 1.0
    The script preserves XML formatting and only updates version-related properties.
#>

[CmdletBinding(SupportsShouldProcess)]
param(
    [Parameter(Mandatory = $false)]
    [string[]]$ProjectFiles,
    
    [Parameter(Mandatory = $true, ParameterSetName = "Increment")]
    [ValidateSet("Major", "Minor", "Release")]
    [string]$VersionType,
    
    [Parameter(Mandatory = $true, ParameterSetName = "Set")]
    [ValidatePattern('^\d+\.\d+\.\d+(\.\d+)?$')]
    [string]$Version,
    
    [Parameter(Mandatory = $false)]
    [switch]$ResetBuild
)

# Validate that either VersionType or Version is provided
if (-not $VersionType -and -not $Version) {
    Write-Error "Either -VersionType or -Version parameter must be specified."
    exit 1
}

function Get-ProjectFiles {
    if ($ProjectFiles) {
        return $ProjectFiles | Where-Object { Test-Path $_ }
    }
    else {
        return Get-ChildItem -Path . -Recurse -Filter "*.dproj" | Select-Object -ExpandProperty FullName
    }
}

function Get-CurrentVersion {
    param(
        [xml]$XmlDoc
    )
    
    $versions = @{
        Major = 0
        Minor = 0
        Release = 0
        Build = 0
    }
    
    try {
        # Use a more robust approach to find PropertyGroups with version info
        $propertyGroups = @()
        
        foreach ($group in $XmlDoc.Project.PropertyGroup) {
            if ($group -and ($group.GetElementsByTagName("VerInfo_MajorVer").Count -gt 0 -or 
                            $group.GetElementsByTagName("VerInfo_MinorVer").Count -gt 0 -or 
                            $group.GetElementsByTagName("VerInfo_Release").Count -gt 0 -or 
                            $group.GetElementsByTagName("VerInfo_Build").Count -gt 0)) {
                $propertyGroups += $group
            }
        }
        
        if ($propertyGroups.Count -gt 0) {
            # Take the first valid version found
            $versionGroup = $propertyGroups[0]
            
            $majorElement = $versionGroup.GetElementsByTagName("VerInfo_MajorVer") | Select-Object -First 1
            if ($majorElement) { $versions.Major = [int]$majorElement.InnerText }
            
            $minorElement = $versionGroup.GetElementsByTagName("VerInfo_MinorVer") | Select-Object -First 1
            if ($minorElement) { $versions.Minor = [int]$minorElement.InnerText }
            
            $releaseElement = $versionGroup.GetElementsByTagName("VerInfo_Release") | Select-Object -First 1
            if ($releaseElement) { $versions.Release = [int]$releaseElement.InnerText }
            
            $buildElement = $versionGroup.GetElementsByTagName("VerInfo_Build") | Select-Object -First 1
            if ($buildElement) { $versions.Build = [int]$buildElement.InnerText }
        }
    }
    catch {
        Write-Warning "Failed to parse version information: $_"
        Write-Warning "Using default version 0.0.0.0"
    }
    
    return $versions
}

function Step-VersionNumbers {
    <#
    .SYNOPSIS
        Increments version numbers based on the specified version type.
    
    .PARAMETER CurrentVersion
        Hashtable containing current version numbers (Major, Minor, Release, Build).
    
    .PARAMETER VersionType
        Type of version increment to perform (Major, Minor, Release).
    
    .PARAMETER ResetBuild
        If specified, resets the build number to 0 when incrementing version.
    #>
    param(
        [hashtable]$CurrentVersion,
        [string]$VersionType,
        [bool]$ResetBuild
    )
    
    $newVersion = $CurrentVersion.Clone()
    
    switch ($VersionType) {
        "Major" {
            $newVersion.Major++
            $newVersion.Minor = 0
            $newVersion.Release = 0
            if ($ResetBuild) { $newVersion.Build = 0 }
        }
        "Minor" {
            $newVersion.Minor++
            $newVersion.Release = 0
            if ($ResetBuild) { $newVersion.Build = 0 }
        }
        "Release" {
            $newVersion.Release++
            if ($ResetBuild) { $newVersion.Build = 0 }
        }
    }
    
    return $newVersion
}

function Set-VersionNumbers {
    <#
    .SYNOPSIS
        Sets version numbers to specific values.
    
    .PARAMETER CurrentVersion
        Hashtable containing current version numbers (Major, Minor, Release, Build).
    
    .PARAMETER VersionString
        Version string in format "Major.Minor.Release" or "Major.Minor.Release.Build".
    #>
    param(
        [hashtable]$CurrentVersion,
        [string]$VersionString
    )
    
    $versionParts = $VersionString.Split('.')
    $newVersion = $CurrentVersion.Clone()
    
    # Set version components from the provided string
    $newVersion.Major = [int]$versionParts[0]
    $newVersion.Minor = [int]$versionParts[1]
    $newVersion.Release = [int]$versionParts[2]
    
    # Set build number if provided, otherwise keep current
    if ($versionParts.Length -gt 3) {
        $newVersion.Build = [int]$versionParts[3]
    }
    
    return $newVersion
}

function Update-VersionProperty {
    param(
        [System.Xml.XmlElement]$PropertyGroup,
        [string]$PropertyName,
        [string]$NewValue
    )
    
    # Try to find the property as a child element
    $property = $PropertyGroup.GetElementsByTagName($PropertyName) | Select-Object -First 1
    if ($property) {
        Write-Verbose "    Found $PropertyName with current value: $($property.InnerText)"
        if ($property.InnerText -ne $NewValue) {
            $property.InnerText = $NewValue
            Write-Verbose "    Updated $PropertyName to: $NewValue"
            return $true
        } else {
            Write-Verbose "    $PropertyName already has value: $NewValue"
            return $false
        }
    } else {
        Write-Verbose "    Property $PropertyName not found in this PropertyGroup"
        return $false
    }
}

function Update-VersionKeysProperty {
    param(
        [System.Xml.XmlElement]$PropertyGroup,
        [hashtable]$NewVersion
    )
    
    $keysProperty = $PropertyGroup.GetElementsByTagName("VerInfo_Keys") | Select-Object -First 1
    if (-not $keysProperty) {
        Write-Verbose "    VerInfo_Keys property not found in this PropertyGroup"
        return $false
    }
    
    $newVersionString = "$($NewVersion.Major).$($NewVersion.Minor).$($NewVersion.Release)"
    $newFileVersionString = "$($NewVersion.Major).$($NewVersion.Minor).$($NewVersion.Release).$($NewVersion.Build)"
    
    # Update FileVersion and ProductVersion in the keys string
    $keysValue = $keysProperty.InnerText
    $originalValue = $keysValue
    
    # Update FileVersion pattern (e.g., FileVersion=4.12.0.287)
    $keysValue = $keysValue -replace 'FileVersion=[\d\.]+', "FileVersion=$newFileVersionString"
    
    # Update ProductVersion pattern (e.g., ProductVersion=4.12.0)
    $keysValue = $keysValue -replace 'ProductVersion=[\d\.]+', "ProductVersion=$newVersionString"
    
    if ($keysValue -ne $originalValue) {
        $keysProperty.InnerText = $keysValue
        Write-Verbose "    Updated VerInfo_Keys with new version strings"
        return $true
    } else {
        Write-Verbose "    VerInfo_Keys already up to date"
        return $false
    }
}

function Update-ProjectFile {
    param(
        [string]$FilePath,
        [hashtable]$NewVersion
    )
    
    Write-Verbose "Processing file: $FilePath"
    
    try {
        # Load XML document
        $xmlDoc = New-Object System.Xml.XmlDocument
        $xmlDoc.PreserveWhitespace = $true
        $xmlDoc.Load($FilePath)
        
        $updated = $false
        
        # Find all PropertyGroups that contain version information
        $propertyGroups = @()
        
        foreach ($group in $xmlDoc.Project.PropertyGroup) {
            if ($group -and ($group.GetElementsByTagName("VerInfo_MajorVer").Count -gt 0 -or 
                            $group.GetElementsByTagName("VerInfo_MinorVer").Count -gt 0 -or 
                            $group.GetElementsByTagName("VerInfo_Release").Count -gt 0 -or 
                            $group.GetElementsByTagName("VerInfo_Build").Count -gt 0 -or
                            $group.GetElementsByTagName("VerInfo_Keys").Count -gt 0)) {
                $propertyGroups += $group
            }
        }
        
        Write-Verbose "  Found $($propertyGroups.Count) PropertyGroups with version information"
        
        foreach ($propertyGroup in $propertyGroups) {
            Write-Verbose "  Processing PropertyGroup with condition: $($propertyGroup.Condition)"
            
            # Update individual version properties
            if (Update-VersionProperty $propertyGroup "VerInfo_MajorVer" $NewVersion.Major.ToString()) {
                Write-Verbose "  Updated VerInfo_MajorVer to $($NewVersion.Major)"
                $updated = $true
            }
            
            if (Update-VersionProperty $propertyGroup "VerInfo_MinorVer" $NewVersion.Minor.ToString()) {
                Write-Verbose "  Updated VerInfo_MinorVer to $($NewVersion.Minor)"
                $updated = $true
            }
            
            if (Update-VersionProperty $propertyGroup "VerInfo_Release" $NewVersion.Release.ToString()) {
                Write-Verbose "  Updated VerInfo_Release to $($NewVersion.Release)"
                $updated = $true
            }
            
            # Update build number if ResetBuild is specified, or if we're setting a specific version with 4 parts
            $shouldUpdateBuild = $ResetBuild -or ($Version -and $Version.Split('.').Length -gt 3)
            if ($shouldUpdateBuild -and (Update-VersionProperty $propertyGroup "VerInfo_Build" $NewVersion.Build.ToString())) {
                Write-Verbose "  Updated VerInfo_Build to $($NewVersion.Build)"
                $updated = $true
            }
            
            # Update version keys
            if (Update-VersionKeysProperty $propertyGroup $NewVersion) {
                Write-Verbose "  Updated VerInfo_Keys with new version strings"
                $updated = $true
            }
        }
        
        if ($updated) {
            if ($PSCmdlet.ShouldProcess($FilePath, "Update version information")) {
                $xmlDoc.Save($FilePath)
                Write-Host "Updated: $FilePath" -ForegroundColor Green
                Write-Host "  Version: $($NewVersion.Major).$($NewVersion.Minor).$($NewVersion.Release).$($NewVersion.Build)" -ForegroundColor Cyan
            }
            else {
                Write-Host "Would update: $FilePath" -ForegroundColor Yellow
                Write-Host "  New version would be: $($NewVersion.Major).$($NewVersion.Minor).$($NewVersion.Release).$($NewVersion.Build)" -ForegroundColor Cyan
            }
            return $true
        }
        else {
            Write-Warning "No version information was updated in: $FilePath"
            return $false
        }
    }
    catch {
        Write-Error "Failed to process file '$FilePath': $($_.Exception.Message)"
        return $false
    }
}

# Main execution
try {
    $projectFiles = Get-ProjectFiles
    
    if (-not $projectFiles) {
        Write-Error "No Delphi project files (.dproj) found."
        exit 1
    }
    
    Write-Host "Found $($projectFiles.Count) project file(s) to process:" -ForegroundColor Cyan
    $projectFiles | ForEach-Object { Write-Host "  $_" -ForegroundColor Gray }
    Write-Host ""
    
    $totalUpdated = 0
    
    foreach ($projectFile in $projectFiles) {
        Write-Host "Analyzing: $projectFile" -ForegroundColor Blue
        
        # Load the file to get current version
        $xmlDoc = New-Object System.Xml.XmlDocument
        $xmlDoc.PreserveWhitespace = $true
        $xmlDoc.Load($projectFile)
        
        $currentVersion = Get-CurrentVersion $xmlDoc
        Write-Host "  Current version: $($currentVersion.Major).$($currentVersion.Minor).$($currentVersion.Release).$($currentVersion.Build)" -ForegroundColor White
        
        # Calculate new version based on operation type
        if ($VersionType) {
            # Increment version
            $newVersion = Step-VersionNumbers $currentVersion $VersionType $ResetBuild
            Write-Host "  Incrementing $VersionType version" -ForegroundColor Yellow
        } else {
            # Set specific version
            $newVersion = Set-VersionNumbers $currentVersion $Version
            Write-Host "  Setting version to: $Version" -ForegroundColor Yellow
        }
        
        Write-Host "  New version: $($newVersion.Major).$($newVersion.Minor).$($newVersion.Release).$($newVersion.Build)" -ForegroundColor Green
        
        # Update the file
        if (Update-ProjectFile $projectFile $newVersion) {
            $totalUpdated++
        }
        
        Write-Host ""
    }
    
    if ($WhatIfPreference) {
        Write-Host "WhatIf mode: No files were actually modified." -ForegroundColor Yellow
        exit 0
    }
    else {
        Write-Host "Successfully updated $totalUpdated of $($projectFiles.Count) project files." -ForegroundColor Green
        exit 0
    }
}
catch {
    Write-Error "Script execution failed: $($_.Exception.Message)"
    exit 1
}
