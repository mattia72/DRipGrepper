[CmdletBinding()]
param (
    $BuildConfig = "Release",
    $BuildPlatform = "",            # Build platform (Win32, Win64, or empty for all)
    [switch] $BuildStandalone,      # Build the standalone DripGrepper application
    [switch] $BuildExtension,       # Build the Delphi IDE extension
    [switch] $BuildUIComponents,    # Build the DRipGrepperComponents package
    [switch] $BuildUnittest,        # Build unit tests only
    [switch] $RunUnittest,          # Run unit tests only (requires built unit tests)
    [switch] $DeployToGitHub,       # Deploy release to GitHub
    [switch] $LocalDeploy,          # Deploy locally
    [switch] $DeployToTransferDrive, # Deploy to transfer drive
    [switch] $Force,                # Force operations without prompts
    [switch] $UpdateScoopManifest,  # Update scoop manifest
    [switch] $AddMissingAsset,      # Add missing assets to GitHub release
    [switch] $DryRun,               # Dry run mode (no actual operations)
    [string] $TestRepo = ""         # Test repository override
)

# Stop script execution on any error
$ErrorActionPreference = "Stop"

# Refresh environment variables from registry
Import-Module PSEnv
Update-SessionEnvironment

Import-Module -Name "$PSScriptRoot\GitHubReleaseUtils.ps1" -Force
    
# - Update Readme.md 
# - Update CHANGELOG.md 
# - Update file and product version in every projects for ALL CONFIGURATION!
# - Commit and push all changes
# - Run this script Ctrl+Shift+T Deploy
$global:TransferDrive = Get-Content "$PSScriptRoot\TransferDrive.txt" -ErrorAction SilentlyContinue
$global:Changelog = "$PSScriptRoot\..\CHANGELOG.md"

$global:PreRelease = $true
$global:StandaloneAppName = "DRipGrepper.exe"
$global:DllNameWithoutExt = "DRipExtensions"
$global:ExtensionFileName = "$global:DllNameWithoutExt.bpl"
$global:ExpertFileName = "$global:DllNameWithoutExt.dll"
$global:ExpertMapFileName = "$global:DllNameWithoutExt.map"

$global:AssetZipName = "DRipGrepper.{0}.{1}.zip"
$global:AssetExpertZipName = "DRipExtension.Dll.{0}.{1}.{2}.zip"
$global:AssetExtensionZipName = "DRipExtension.Bpl.{0}.{1}.{2}.zip"
$global:AssetsDirectoryName = "assets"
$global:AssetsDirectory = "$PSScriptRoot\$global:AssetsDirectoryName"

$global:Owner = "mattia72"
$global:Repo = "DRipGrepper"

# Test repository override
if ($TestRepo -ne "") {
    $global:Repo = $TestRepo
    Write-Host "Using test repository: $TestRepo" -ForegroundColor Yellow
}
if ($DryRun) {
    Write-Host "DRY RUN MODE - No actual GitHub operations will be performed" -ForegroundColor Magenta
}

$global:Url = "https://api.github.com/repos/$global:Owner/$global:Repo/releases"
# $global:Token = $env:GITHUB_TOKEN
$global:Token = (Get-Content -LiteralPath "$PSScriptRoot\SECRET_TOKEN" -TotalCount 1).Trim()

$global:PadRightValue = 45
$global:InstalledDelphiVersions = @()

$global:headers = @{
    "Content-Type"         = "application/json"
    Accept                 = "application/vnd.github+json"
    Authorization          = "Bearer $global:Token"
    "X-GitHub-Api-Version" = "2022-11-28"
}

function Test-DelphiDeploymentEnabled {
    param (
        [string]$Name
    )

    # Delphi 12 support is paused until a licence is sponsored.
    # Accept compiler directory names, asset directories, and archive names.
    return $Name -notmatch '(^|\.)Delphi12(\.|$)'
}

function Get-DelphiName {
    param (
        [string]$versionNumber
    )

    switch -regex ($versionNumber) {
        "^22\.*" { [PSCustomObject]@{ Name = "Delphi 11 Alexandria"; Dir = "Delphi11" } }
        "^23\.*" { [PSCustomObject]@{ Name = "Delphi 12 Athen"; Dir = "Delphi12" } }
        "^37\.*" { [PSCustomObject]@{ Name = "Delphi 13 Florence"; Dir = "Delphi13" } }
        Default { "Unknown" }
    }
}

function Get-ExpertDllBaseName {
    param (
        [string]$DelphiDir
    )

    if ($DelphiDir -eq 'Delphi13') {
        return 'DripExtensionD13'
    }
    return "$global:DllNameWithoutExt.$($DelphiDir -replace 'Delphi', 'D')"
}

function Get-LastInstalledDelphiVersion {
    if ($global:InstalledDelphiVersions.Count -eq 0) {
        Get-InstalledDelphiVersions | Out-Null
    }
    return $global:InstalledDelphiVersions[0]
}
function Get-InstalledDelphiVersions {

    if ($global:InstalledDelphiVersions.Count -ne 0) {
        return $global:InstalledDelphiVersions
    }

    $delphiRegistryPaths = @(
        # "HKLM:\SOFTWARE\Borland\Delphi",
        # "HKLM:\SOFTWARE\Wow6432Node\Borland\Delphi",
        "HKLM:\SOFTWARE\Embarcadero\BDS",
        "HKLM:\SOFTWARE\Wow6432Node\Embarcadero\BDS"
    )
    $installedDelphiVersions = @()
    foreach ($path in $delphiRegistryPaths) {
        if (Test-Path $path) {
            Get-ChildItem -Path $path | ForEach-Object {
                $version = $_.PSChildName
                $delphiName = Get-DelphiName -versionNumber $version
                if (-not (Test-DelphiDeploymentEnabled -Name $delphiName.Dir)) {
                    Write-Host "Skipping $($delphiName.Name): support is paused pending licence sponsorship." -ForegroundColor Yellow
                    return
                }
                $installedDelphiVersions += [PSCustomObject]@{ Version = $version; Data = $delphiName }
            }
        }
    }

    $global:InstalledDelphiVersions = @($installedDelphiVersions | Sort-Object -Property Version -Descending)
    if ($installedDelphiVersions.Count -eq 0) {
        Write-Error "No Delphi versions with active support found. Delphi 12 support is paused pending licence sponsorship."
    }
    else {
        Write-Host "Installed Delphi versions:" -ForegroundColor Blue
        $global:InstalledDelphiVersions | ForEach-Object { Write-Host $_.Data.Name }
    }
    return $global:InstalledDelphiVersions
}

function Test-YesAnswer {
    [CmdletBinding()]
    param (
        [string] $Message,
        [string] $DefaultAnswer = 'y'
    )
    if ($Force) {
        $answer = $DefaultAnswer
    }
    while ($answer -inotmatch "[yn]") {
        Write-Host "$Message [Yn]" -BackgroundColor Yellow -ForegroundColor Black -NoNewline
        Write-Host " " -NoNewline
        $answer = Read-Host
        if ($null -eq $answer -or $answer -eq "") {
            $answer = $DefaultAnswer
            break;
        }
    }
    return $answer -imatch "[y]"
}
function Get-ProjectPath {
    param (
        $Path,
        $Leaf
    )
    $latestVersion = Get-LastInstalledDelphiVersion 
    Join-Path $(Split-Path -Parent $PSScriptRoot) "$Path\$Leaf$($latestVersion.Data.Dir)"
}

function Get-VersionsFromChangelog {
    param (
        [string]$ChangelogPath = $global:Changelog
    )
    
    if (-not (Test-Path $ChangelogPath)) {
        Write-Error "No CHANGELOG.md found at $ChangelogPath."
        return $null
    }
    
    $changelogContent = Get-Content $ChangelogPath
    $versions = @()
    foreach ($line in $changelogContent) {
        if ($line -match "^## \[([^\]]+)\]") {
            $extractedVersion = $matches[1]
            if ($extractedVersion -match '^v?\d+\.\d+') {
                $versions += $extractedVersion
            }
        }
    }
    
    if ($versions.Count -eq 0) {
        Write-Error "No versions found in changelog."
        return $null
    }
    
    $currentVersion = $versions[0]  # First version is the most recent
    $previousVersion = if ($versions.Count -gt 1) { $versions[1] } else { "" }
    
    Write-Host "Extracted from CHANGELOG.md:" -ForegroundColor Green
    Write-Host "  Current Version: $currentVersion" -ForegroundColor Cyan
    Write-Host "  Previous Version: $previousVersion" -ForegroundColor Cyan
    
    return @{
        CurrentVersion  = $currentVersion
        PreviousVersion = $previousVersion
    }
}

function Get-CurrentVersionChangesFromChangelog {
    param (
        [string]$ChangelogPath = $global:Changelog,
        [string]$Version = $global:Version
    )
    
    if (-not (Test-Path $ChangelogPath)) {
        Write-Error "No CHANGELOG.md found."
    }
    
    $changelogContent = Get-Content $ChangelogPath
    $currentVersionChanges = @()
    $inCurrentVersion = $false
    $versionPattern = "^## \[.*\]"
    
    # Remove 'v' prefix if present for comparison
    $cleanVersion = $Version -replace '^v', ''
    
    foreach ($line in $changelogContent) {
        # Check if we found the current version section
        if ($line -match $versionPattern) {
            if ($line -match "^## (\[$cleanVersion\]|\[v$cleanVersion\])") {
                $inCurrentVersion = $true
                $currentVersionChanges += $line
                continue
            }
            elseif ($inCurrentVersion) {
                # We've hit the next version, stop collecting
                break
            }
        }
        
        # If we're in the current version section, collect the lines
        if ($inCurrentVersion) {
            # Skip empty lines at the beginning and end
            if ($line.Trim() -eq "" -and $currentVersionChanges.Count -eq 1) {
                continue
            }
            $currentVersionChanges += $line
        }
    }
    
    if ($currentVersionChanges.Count -eq 0) {
        Write-Warning "No changes found for version $Version in changelog"
        return "No changes documented for this version."
    }
    
    # Remove trailing empty lines
    while ($currentVersionChanges.Count -gt 0 -and $currentVersionChanges[-1].Trim() -eq "") {
        $currentVersionChanges = $currentVersionChanges[0..($currentVersionChanges.Count - 2)]
    }
    
    # Join the changes and return
    return ($currentVersionChanges -join "`n").Trim()
}

function Build-StandaloneRelease {
    # copy scripts
    Import-Module -Name PSDelphi -Force
    $projectPath = Get-ProjectPath "src\Project" ""
    $result = $null
    $platforms = if ($BuildPlatform -ne "") { @($BuildPlatform) } else { @("Win32", "Win64") }
    foreach ($platform in $platforms) {
        Build-DelphiProject -ProjectPath $projectPath\DRipGrepper.dproj -BuildConfig $BuildConfig -Platform $platform -StopOnFirstFailure -CountResult -Result ([ref]$result)
        Test-BuildResult -Result $result
    }
}

function Build-Unittest {
    # Build unit tests only
    Import-Module -Name PSDelphi -Force
    $projectPath = Split-Path -Parent $PSScriptRoot 
    $unittestPath = Join-Path $projectPath "UnitTest"
    $result = $null
    # get installed Delphi version
    $latestVersion = Get-LastInstalledDelphiVersion
    $latestVersion = $latestVersion.Data.Dir -replace "Delphi", "D"
    Build-DelphiProject -ProjectPath $unittestPath\DRipGrepperUnittest.$latestVersion.dproj -BuildConfig $BuildConfig -StopOnFirstFailure -CountResult -Result ([ref]$result)
    Test-BuildResult -Result $result    
    
    Write-Host "Unit test build completed successfully!" -ForegroundColor Green
    return $result
}

function Run-Unittest {
    # Run unit tests only (assumes they are already built)
    $projectPath = Split-Path -Parent $PSScriptRoot 
    $unittestPath = Join-Path $projectPath "UnitTest"
    # get installed Delphi version
    $latestVersion = Get-LastInstalledDelphiVersion
    $latestVersion = $latestVersion.Data.Dir -replace "Delphi", "D"
    
    $unittestPath = "$(Join-Path $unittestPath "\Win32\$BuildConfig")\DRipGrepperUnittest.$latestVersion.exe"
    
    if (-not (Test-Path $unittestPath)) {
        Write-Error "Unit test executable not found at: $unittestPath. Please build unit tests first with -BuildUnittest parameter." -ErrorAction Stop
    }

    # Read-Host "Press Enter to run unit tests:`n$unittestPath"
    & $unittestPath --dontshowignored --exitbehavior:Continue
    
    if ((-not $?) -or $LASTEXITCODE -ne 0) {
        Write-Error "Unit tests failed to run. Please check the output above." -ErrorAction Stop
    }
    
    Write-Host "Unit tests completed successfully!" -ForegroundColor Green
}

function Build-AndRunUnittest {
    # Legacy function that does both build and run for backward compatibility
    Build-Unittest
    Run-Unittest
}

function Build-BplExtensionRelease {
    # copy scripts
    Import-Module -Name PSDelphi -Force
    $projectPath = Get-ProjectPath "Extension\src\Project" "Bpl."
    $result = $null
    Build-DelphiProject -ProjectPath $projectPath\DRipExtension.dproj -BuildConfig $BuildConfig -StopOnFirstFailure -CountResult -Result ([ref]$result)
    Test-BuildResult -Result $result
}

function Build-ExpertDllRelease {
    # copy scripts
    Import-Module -Name PSDelphi -Force

    $projectPath = Get-ProjectPath "Extension\src\Project" "Dll."
    $latestVersion = Get-LastInstalledDelphiVersion 
    $dllProjName = "$(Get-ExpertDllBaseName -DelphiDir $latestVersion.Data.Dir).dproj"
    $result = $null
    # add DesignIde to the msbuild parameters
    # see https://docwiki.embarcadero.com/Libraries/Athens/en/DesignIntf
    Build-DelphiProject -ProjectPath $projectPath\$dllProjName -BuildConfig $BuildConfig `
        -AddMsBuildParameters "/p:mapfile=Detailed;DCC_MapFile=3" -StopOnFirstFailure -CountResult -Result ([ref]$result)
    Test-BuildResult -Result $result
}

function Build-UIComponentsPackage {
    # Build DRipGrepperComponents package
    # Check if bds.exe is running and ask to close it manually
    $bdsProcess = Get-Process | Where-Object { $_.Path -like "*bds.exe" }
    if ($bdsProcess) {
        Write-Host "WARNING: Delphi IDE is currently running!" -ForegroundColor Red
        Write-Host "The IDE must be closed before building UI Components to avoid file conflicts." -ForegroundColor Yellow
        Write-Host "Please save your work and close the Delphi IDE manually, then press Enter to continue..." -ForegroundColor Yellow
        Read-Host
        
        # Check again if it's still running
        $bdsProcess = Get-Process | Where-Object { $_.Path -like "*bds.exe" }
        if ($bdsProcess) {
            Write-Error "Delphi IDE is still running. Please close it and try again." -ErrorAction Stop
        }
        Write-Host "Delphi IDE closed. Continuing with build..." -ForegroundColor Green
    }
    
    Import-Module -Name PSDelphi -Force
    $projectPath = Get-ProjectPath "src\Project" ""
    $result = $null
    Build-DelphiProject -ProjectPath $projectPath\DRipGrepperComponents.dproj -BuildConfig $BuildConfig -StopOnFirstFailure -CountResult -Result ([ref]$result)
    Test-BuildResult -Result $result
    Write-Host "DRipGrepperComponents package build completed successfully!" -ForegroundColor Green
}

function Add-ToAssetsDir {
    param (
        $AssetDir,
        $AssetItemPath,
        [switch]$Win64,
        [switch]$ReplaceContent
    )

    $item = $(Get-Item $AssetItemPath)
    $appVersion = $($item.VersionInfo.FileVersion) # BPL is ok too :)
    # Compare only Major.Minor.Patch – build number is allowed to differ
    $appVersionBase    = ($appVersion -replace '^v', '') -replace '(\d+\.\d+\.\d+).*', '$1'
    $changelogVersionBase = ($global:Version -replace '^v', '') -replace '(\d+\.\d+\.\d+).*', '$1'
    if ($item.Extension -ne '.map' -and $appVersionBase -ne $changelogVersionBase) {
        Write-Error "Version mismatch: CHANGELOG=$global:Version, $($item.Name) FileVersion=$appVersion. Update FileVersion in *.dproj!`r`nDeploy stopped." -ErrorAction Stop
    }
    if (-not $(Test-YesAnswer "Release version: $global:Version. Version of $($item.Name)($($Win64 ? 'Win64': 'Win32')) binary version: $appVersion. Ok?")) {
        Write-Error "Deploy stopped by user." -ErrorAction Stop
    }
    if ($ReplaceContent -and (Test-Path $AssetDir)) {
        Remove-Item -Path $AssetDir -Recurse -Force -ErrorAction SilentlyContinue
    }
    New-Item -Path $AssetDir -ItemType Directory -Force -ErrorAction SilentlyContinue | Out-Null
    
    Copy-Item -Path $AssetItemPath -Destination $AssetDir -ErrorAction Break

    $assetLabel = $($item.FullName -replace "^(.*)(\\.+\\.+\\$BuildConfig.*$)", "`$2" )
    $formattedLabel = $assetLabel.PadRight($global:PadRightValue)

    # if ($item.Extension -eq '.map') {
    #     Write-Host "$formattedLabel  $(" ".PadRight(10)) $($item.LastWriteTime) added to $($AssetDir -replace [regex]::Escape("$PSScriptRoot\"), '')." -ForegroundColor Green
    # }
    # else {
    #     Write-Host "$formattedLabel  $($appVersion.PadRight(10)) $($item.LastWriteTime) added to $($AssetDir -replace [regex]::Escape("$PSScriptRoot\"), '')." -ForegroundColor Green
    # }

    $assetObj = [PSCustomObject]@{
        File          = $formattedLabel
        Version       = $appVersion
        LastWriteTime = $($item.LastWriteTime.ToString("dd/MM/yyyy HH:mm:ss"))
        Length        = Format-FileSize -Length $item.Length
        Dir           = $($AssetDir -replace [regex]::Escape("$PSScriptRoot\"), '')
    }

    $assetObj
}

function Format-FileSize {
    param (
        $Length
    )
    if ($Length -gt 1MB) {
        "{0:N2} MB" -f ($Length / 1MB)
    }
    else {
        "{0:N2} KB" -f ($Length / 1KB)
    }
}
function New-StandaloneZips {
    $projectPath = Split-Path -Parent $PSScriptRoot 

    "Win32" , "Win64" | ForEach-Object {
        $lastDelphiVer = Get-LastInstalledDelphiVersion
        $ZipDir = $(Join-Path $projectPath "src\Project\$($lastDelphiVer.Data.Dir)\$_\$BuildConfig")
        $AssetDir = $(Join-Path $global:AssetsDirectory $_)
    
        $win64 = $($_ -eq 'Win64')
        $assetObj = Add-ToAssetsDir -AssetDir $AssetDir -AssetItemPath $(Join-Path  $ZipDir $global:StandaloneAppName) -Win64:$win64 -ReplaceContent
        # Output to pipeline so callers receive this object (ForEach-Object has its own scope)
        $assetObj
        $assetVersion = Get-BinaryAssetVersion -FileVersion $assetObj.Version
        $dest = "$global:AssetsDirectory\$($global:AssetZipName -f $($win64 ? 'x64' : 'x86'), $assetVersion)"

        $compress = @{
            Path             = "$AssetDir\*.*"
            CompressionLevel = "Fastest"
            DestinationPath  = $dest
            Force            = $true
        }
        Compress-Archive @compress
    }
}
function New-ExtensionZip {
    # find bds.exe in running processes
    # if exists then ask user if he wants to close it, if yes then close it
    Get-Process | Where-Object { $_.Path -like "*bds.exe" } | ForEach-Object {
        $process = $_
        if ($process) {
            Write-Host "Found running process: $($process.Name) with id: $($process.Id)"
            if (Test-YesAnswer "Do you want to close it?") {
                Stop-Process -Id $process.Id -Force
            }
        }
    }
    Get-InstalledDelphiVersions | ForEach-Object {
        $delphiBplRoot = $("$env:PUBLIC\Documents\Embarcadero\Studio\$($_.Version)") 
        $extensionPath = Join-Path "$delphiBplRoot" "Bpl\$global:ExtensionFileName"
    
        $AssetDir = $(Join-Path $global:AssetsDirectory "$($_.Data.Dir).Bpl")
                
        Add-ToAssetsDir -AssetDir $AssetDir $extensionPath -Empty -ReplaceContent
    
        $dest = "$global:AssetsDirectory\$($global:AssetExtensionZipName  -f $($win64 ? 'x64' : 'x86'), $_.Data.Dir ,$global:AssetVersion)"
        # Write-Host "$AssetDir\*.* to`n $dest" 

        $compress = @{
            Path             = "$AssetDir\*.*"
            CompressionLevel = "Fastest"
            DestinationPath  = "$dest"
            Force            = $true
        }
        Compress-Archive @compress
    }
}

function New-ExpertDllZip {
    
    $projectPath = Split-Path -Parent $PSScriptRoot 
    $ReleaseType = "Win32"
    $win64 = $($ReleaseType -eq 'Win64')
    Get-InstalledDelphiVersions | ForEach-Object {
        $ZipDir = $(Join-Path $projectPath "Extension\src\Project\Dll.$($_.Data.Dir)\$ReleaseType\$BuildConfig")
        $AssetDir = $(Join-Path $global:AssetsDirectory "$($_.Data.Dir).Dll")

        $dllBaseName = Get-ExpertDllBaseName -DelphiDir $_.Data.Dir
        $dllName = "$dllBaseName.dll"
        $mapName = "$dllBaseName.map"
        $dllAssetObj = Add-ToAssetsDir -AssetDir $AssetDir $(Join-Path  $ZipDir $dllName) -Win64:$false -ReplaceContent
        # Output to pipeline so callers receive these objects (ForEach-Object has its own scope)
        $dllAssetObj
        Add-ToAssetsDir -AssetDir $AssetDir $(Join-Path  $ZipDir $mapName) -Win64:$false
        $dllAssetVersion = Get-BinaryAssetVersion -FileVersion $dllAssetObj.Version
        $dest = "$global:AssetsDirectory\$($global:AssetExpertZipName -f $($win64 ? 'x64' : 'x86'), $_.Data.Dir, $dllAssetVersion)"

        # Write-Host "$AssetDir\*.* to`n $dest" 

        $compress = @{
            Path             = "$AssetDir\*.*"
            CompressionLevel = "Fastest"
            DestinationPath  = $dest
            Force            = $true
        }
        Compress-Archive @compress
    }
}

function Get-BinaryAssetVersion {
    param (
        [string]$FileVersion
    )

    $binaryVersion = [version]$FileVersion
    if ($binaryVersion.Revision -lt 0) {
        throw "Binary FileVersion must include a build number: $FileVersion"
    }
    if ($global:Version -match '^(v?\d+\.\d+\.\d+)(.*)$') {
        return "$($matches[1]).$($binaryVersion.Revision)$($matches[2])"
    }
    return "v$FileVersion"
}

function Get-LatestBuildAssets {
    [CmdletBinding()]
    param (
        [string]$Path = $global:AssetsDirectory,
        [switch]$RemoveOlderBuilds,
        [switch]$DryRun
    )

    # Group independently by binary type, architecture, and Delphi version.
    $versionedAssets = @(Get-ChildItem -LiteralPath $Path -File -Filter '*.zip' | ForEach-Object {
        if (-not (Test-DelphiDeploymentEnabled -Name $_.Name)) {
            Write-Host "Skipping paused Delphi 12 asset: $($_.Name)" -ForegroundColor Yellow
            return
        }
        if ($_.Name -match '^(?<Family>DRipGrepper\.(?:x86|x64)|DRipExtension\.(?:Dll|Bpl)\.(?:x86|x64)\.Delphi\d+)\.v?(?<Version>\d+\.\d+\.\d+(?:\.\d+)?)(?:-[0-9A-Za-z.-]+)?\.zip$') {
            [PSCustomObject]@{
                File = $_
                Family = $matches.Family
                Version = [version]$matches.Version
            }
        }
        else {
            Write-Warning "Cannot determine binary/build from $($_.Name). Keeping it locally and skipping upload."
        }
    })

    foreach ($group in ($versionedAssets | Group-Object Family)) {
        # Numeric version sorting handles builds such as 9, 10, and 100 correctly.
        # Legacy names without a build number sort below explicitly numbered builds.
        $sorted = @($group.Group | Sort-Object -Property @{ Expression = { $_.Version }; Descending = $true }, @{ Expression = { $_.File.Name } })
        $latest = $sorted[0]
        Write-Host "Selected latest asset: $($latest.File.Name)" -ForegroundColor Cyan

        if ($RemoveOlderBuilds) {
            foreach ($older in ($sorted | Select-Object -Skip 1)) {
                if ($DryRun) {
                    Write-Host "DRY RUN: Would delete older local asset: $($older.File.Name)" -ForegroundColor Yellow
                }
                else {
                    Write-Host "Deleting older local asset: $($older.File.Name)" -ForegroundColor Yellow
                    Remove-Item -LiteralPath $older.File.FullName -Force -ErrorAction Stop
                }
            }
        }

        $latest.File
    }
}

function List-Assets {
    param (
        $Path
    )
    Write-Host "Assets: $Path" -ForegroundColor Blue
    $returnArr = @()
    Get-Childitem $Path | ForEach-Object { 
        if ($_.PSIsContainer) { 
            # Write-Host "$($_.Name)`t$($_.CreationTime)" -ForegroundColor Blue
        }
        else {
            if ($_.Extension -eq '.zip') {
                $color = 'Red'
                # Write-Host "$($_.Name.PadRight($global:PadRightValue))`t$($_.CreationTime)`t$($_.Length)" -ForegroundColor $color
                $returnArr += [PSCustomObject]@{
                    File          = $($_.Name.PadRight($global:PadRightValue))
                    # Version        = $($_.VersionInfo.FileVersion)
                    LastWriteTime = $($_.LastWriteTime.ToString("dd/MM/yyyy HH:mm:ss"))
                    # format length in KB or MB
                    Length        = Format-FileSize -Length $_.Length
                    # Dir            = $Path -replace [regex]::Escape("$PSScriptRoot\")
                }
            }
            else {
                $color = 'Green'
                $formattedLabel = ($_.Name).PadRight($global:PadRightValue)
                Write-Host "$formattedLabel  $(' '.PadRight(10)) $($_.LastWriteTime)`t$($_.Length)" -ForegroundColor $color
            }
        }
    }
    $returnArr
}

function New-ReleaseWithAsset {

    if ($BuildUnittest) {
        Build-Unittest
    }

    if ($RunUnittest) {
        Run-Unittest
    }

    if ($BuildUIComponents) {
        Build-UIComponentsPackage
    }

    if ($BuildStandalone) {
        Build-StandaloneRelease 
    }
  
    if ($BuildExtension) {
        # Build-ExtensionRelease
        Build-ExpertDllRelease
    }

    if ($DeployToGitHub -or $LocalDeploy -or $DeployToTransferDrive) {
        # Each EXE/DLL archive adds the build number from its own binary.
        $global:AssetVersion = $global:Version
        Write-Host "Asset version: $global:AssetVersion" -ForegroundColor Cyan

        # Local deployments do not require packaging confirmations.
        if ($LocalDeploy -or $DeployToTransferDrive) {
            $Force = $true
        }
        $assetArr = New-StandaloneZips 
        # New-ExtensionZip 
        $assetArr += New-ExpertDllZip 

        # zip dll assets in directories not listed in $assetArr
        Get-ChildItem $global:AssetsDirectory -Directory | Where-Object {
            Test-DelphiDeploymentEnabled -Name $_.Name
        } | ForEach-Object {
            if ($($assetArr | Select-Object -ExpandProperty Dir) -notcontains "$global:AssetsDirectoryName\$($_.Name)") {
                $dllFiles = @(Get-ChildItem -LiteralPath $_.FullName -File -Filter '*.dll')
                if ($dllFiles.Count -ne 1) {
                    Write-Warning "Expected one DLL in $($_.FullName). Skipping extra directory."
                    return
                }
                $dllAssetVersion = Get-BinaryAssetVersion -FileVersion $dllFiles[0].VersionInfo.FileVersion
                $zipPath = "$global:AssetsDirectory\$($global:AssetExpertZipName -f 'x86', $($_.Name -replace '\.Dll$'), $dllAssetVersion)"
                if (-not (Test-Path $zipPath)) {
                    Write-Host "Zipping extra directory: $($_.Name)" -ForegroundColor Yellow
                    Compress-Archive -Path "$($_.FullName)\*" -DestinationPath $zipPath -Force
                }
            }
        }

        $latestAssets = @(Get-LatestBuildAssets -RemoveOlderBuilds -DryRun:$DryRun)
        $assetArr | Format-Table -AutoSize -Property File, Version, LastWriteTime, Length, Dir
        $zipArr = List-Assets -Path $global:AssetsDirectory
        $zipArr | Format-Table -AutoSize -Property File, LastWriteTime, Length
    }

    if ($DeployToTransferDrive) {
        Write-Host "Copying files to $global:TransferDrive" -ForegroundColor Green
        Get-ChildItem -LiteralPath $global:AssetsDirectory | Where-Object {
            Test-DelphiDeploymentEnabled -Name $_.Name
        } | Copy-Item -Destination $global:TransferDrive -Force -Recurse
        $zipArr = List-Assets -Path $global:TransferDrive
        
        # Clear Latest directory before copying new assets
        $latestDir = "$global:TransferDrive\Latest"
        if (Test-Path $latestDir) {
            Write-Host "Clearing Latest directory: $latestDir" -ForegroundColor Yellow
            Remove-Item -Path "$latestDir\*" -Force -Recurse -ErrorAction SilentlyContinue
        }
        
        Copy-Item -Path $global:AssetsDirectory\Win64\* -Destination $latestDir -Force
        Copy-Item -Path $global:AssetsDirectory\Delphi11.Dll\* -Destination $latestDir -Force
        $zipArr += List-Assets -Path $latestDir
        $zipArr | Format-Table -AutoSize -Property File, LastWriteTime, Length
    }
    if ($DeployToGitHub) {
        $release = $null
        if (-not $DryRun) {
            try {
                $release = Get-Releases -Url $global:Url -Headers $global:headers -Tag $global:Version
            }
            catch {
                if ($_.Exception.Response.StatusCode -ne 404) {
                    throw
                }
            }
        }

        if ($release) {
            Write-Host "Release $global:Version already exists. Uploading missing assets only." -ForegroundColor Cyan
        }
        else {
            # Generate description and notes only when creating a release.
            $releaseDescription = Get-CurrentVersionChangesFromChangelog
            $release = New-Release -url $global:Url -headers $global:headers -version $global:Version -description $releaseDescription -preRelease:$global:PreRelease -dryRun:$DryRun

            if (-not $DryRun) {
                New-ReleaseNotes -owner $global:Owner -repo $global:Repo -headers $global:headers -version $global:Version -prevVersion $global:PrevVersion
            }
            else {
                Write-Host "DRY RUN: Would generate release notes for $global:Version (previous: $global:PrevVersion)" -ForegroundColor Cyan
            }
        }

        $ReleaseID = $release.id
        $existingAssetNames = @($release.assets | ForEach-Object { $_.name })

        $latestAssets | ForEach-Object {
            if ($existingAssetNames -ccontains $_.Name) {
                Write-Host "Asset $($_.Name) already exists in release $global:Version. Skipping." -ForegroundColor Yellow
            }
            else {
                Add-AssetToRelease -owner $global:Owner -repo $global:Repo -token $global:Token -releaseID $ReleaseID -zipFilePath $_.FullName -dryRun:$DryRun
            }
        }
    }
}

function Clear-ScoopTokenEnvironment {
  # SilentlyContinue still feeds missing-path errors into an enclosing ErrorVariable.
  foreach ($name in 'SCOOP_GH_TOKEN', 'GH_TOKEN', 'GITHUB_TOKEN') {
    $tokenPath = "Env:$name"
    if (Test-Path -LiteralPath $tokenPath) {
      Remove-Item -LiteralPath $tokenPath -ErrorAction Stop
    }
  }
}

function Set-ScoopAssetTemplates {
    param (
        [string]$ManifestPath
    )

    $manifest = Get-Content -LiteralPath $ManifestPath -Raw | ConvertFrom-Json
    $assetGroups = @{}
    $patterns = @()
    $hasVersion = $false
    foreach ($architecture in $manifest.autoupdate.architecture.PSObject.Properties) {
        $currentArchitecture = $manifest.architecture.($architecture.Name)
        $currentUrls = @()
        $currentHashes = @()
        $assetIndex = -1
        $urls = @(foreach ($url in $currentArchitecture.url) {
            $assetIndex++
            if (-not (Test-DelphiDeploymentEnabled -Name (Split-Path $url -Leaf))) {
                continue
            }
            $currentUrls += $url
            $currentHashes += @($currentArchitecture.hash)[$assetIndex]
            if ($url -notmatch '/(?<Family>DRipGrepper\.(?:x86|x64)|DRipExtension\.(?:Dll|Bpl)\.(?:x86|x64)\.Delphi\d+(?:\.\d+)?)\.v?[^/]+\.zip$') {
                throw "Cannot determine Scoop asset family from $url"
            }
            $family = $matches.Family
            if (-not $assetGroups.ContainsKey($family)) {
                $group = "asset$($assetGroups.Count)"
                $assetGroups[$family] = $group
                $versionPattern = '\d+\.\d+\.\d+(?:\.\d+)?(?:-[0-9A-Za-z.-]+)?'
                # Scoop's version includes the standalone build, while every URL
                # is captured independently (DLL and EXE builds can differ).
                if (-not $hasVersion -and $family -like 'DRipGrepper.*') {
                    # Scoop reads capture group 1 as the version.
                    $versionPattern = "($versionPattern)"
                    $hasVersion = $true
                }
                $urlPattern = 'https://github\.com/' + [regex]::Escape("$global:Owner/$global:Repo") +
                    '/releases/download/[^/\s"]+/' + [regex]::Escape($family) + '\.v?' + $versionPattern + '\.zip'
                $patterns += '(?=.*?(?<' + $group + '>' + $urlPattern + '))'
            }
            '$match' + (Get-Culture).TextInfo.ToTitleCase($assetGroups[$family])
        })
        if ($urls.Count -eq 0) {
            throw "No supported Scoop assets for $($architecture.Name)"
        }
        $architecture.Value.url = $urls
        # Scoop updates array entries in place, so remove paused URL/hash pairs
        # here to prevent obsolete trailing entries surviving the update.
        $currentArchitecture.url = $currentUrls
        $currentArchitecture.hash = $currentHashes
    }
    if (-not $hasVersion) {
        throw 'No standalone asset found in the Scoop autoupdate configuration.'
    }

    $manifest.checkver = [ordered]@{
        url = $global:Url
        # Sort numerically before matching so multiple builds in a release
        # resolve to the latest archive for each binary family.
        script = @(
            '$release = $page | ConvertFrom-Json | Where-Object { -not $_.draft } | Select-Object -First 1'
            '$release.assets | Sort-Object -Property @{ Expression = {'
            '    if ($_.name -match "\.v?(\d+\.\d+\.\d+(?:\.\d+)?)(?:-[0-9A-Za-z.-]+)?\.zip$") { [version]$matches[1] } else { [version]"0.0" }'
            '}; Descending = $true } | ForEach-Object { $_.browser_download_url }'
        )
        regex = '(?s)^' + ($patterns -join '')
    }
    $manifest | ConvertTo-Json -Depth 20 | Set-Content -LiteralPath $ManifestPath -Encoding utf8NoBOM
}

function Update-ScoopManifest {

    Push-Location $env:SCOOP\buckets\my-scoop
    try {
        Clear-ScoopTokenEnvironment
        Set-ScoopAssetTemplates -ManifestPath '.\bucket\dripgrepper.json'
        .\bin\checkver.ps1 dripgrepper -ForceUpdate -ThrowError
        if ( -not $(Test-YesAnswer "Commit updated manifests?")) {
            Write-Error "Commit canceled" -ErrorAction Stop
        }
        git add .
        git commit -m "dripgrepper $global:Version"
        if ( -not $(Test-YesAnswer "Push updated manifests?")) {
            Write-Error "Push canceled" -ErrorAction Stop
        }
        git push
    }
    finally {
        Pop-Location
    }
    scoop update dripgrepper
}

function New-Deploy {

    # Extract version information from CHANGELOG.md
    $versionInfo = Get-VersionsFromChangelog -ChangelogPath $global:Changelog
    $global:Version = $versionInfo.CurrentVersion
    if ($global:Version -notmatch "^v\d+\.\d+\.\d+.*") {
        Write-Error "Invalid version format in CHANGELOG.md: $global:Version. Expected format: vX.Y.Z"
        Exit 1
    }
    $global:PrevVersion = $versionInfo.PreviousVersion

    # Check for GitHub token only if GitHub operations are requested
    if (($DeployToGitHub -or $AddMissingAsset) -and -not $global:Token) {
        Write-Error "FATAL: GITHUB_TOKEN environment variable is not set. Please set it with a personal access token for GitHub operations."
        exit 1
    }

    if ($LocalDeploy -or $DeployToGitHub -or $BuildStandalone -or $BuildExtension -or $BuildUIComponents -or $BuildUnittest -or $RunUnittest -or $DeployToTransferDrive) {
        #New-ReleaseNotes
        New-ReleaseWithAsset
    }
    if ($AddMissingAsset) {
        # Add assets to latest release
        $latest = Get-Releases  -URl $global:Url -Headers $global:headers -Tag $global:Version 
        $ReleaseID = $( $latest | Select-Object -Property id).id
        $assets = $( $latest | Select-Object -ExpandProperty assets | Select-Object -ExpandProperty name )
        Get-LatestBuildAssets -RemoveOlderBuilds -DryRun:$DryRun | ForEach-Object {
            if ($assets -notcontains $_.Name) {
                Write-Host "Adding asset $($_.Name) to latest release $global:Version" -ForegroundColor Green
                if ($DryRun -or (Test-YesAnswer "Do you want to add this asset?")) {
                    Add-AssetToRelease -owner $global:Owner -repo $global:Repo -token $global:Token -releaseID $ReleaseID -zipFilePath $_.FullName -dryRun:$DryRun
                }
            }
            else {
                Write-Host "Asset $($_.Name) already exists in latest release $global:Version" -ForegroundColor Yellow
            }
        }
    }
    if ($UpdateScoopManifest) {
        #Update scoop with latest version from github
        Update-ScoopManifest
    }
}

New-Deploy
