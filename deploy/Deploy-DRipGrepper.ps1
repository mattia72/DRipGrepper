[CmdletBinding()]
param (
    $BuildConfig = "Release",
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
$global:Token = $(Get-Content $PSScriptRoot\SECRET_TOKEN)[0]

$global:PadRightValue = 45
$global:InstalledDelphiVersions = @()

$global:headers = @{
    "Content-Type"         = "application/json"
    Accept                 = "application/vnd.github+json"
    Authorization          = "Bearer $global:Token"
    "X-GitHub-Api-Version" = "2022-11-28"
}

function Get-DelphiName {
    param (
        [string]$versionNumber
    )

    switch -regex ($versionNumber) {
        "^22\.*" { [PSCustomObject]@{ Name = "Delphi 11 Alexandria"; Dir = "Delphi11" } }
        "^23\.*" { [PSCustomObject]@{ Name = "Delphi 12 Athen"; Dir = "Delphi12" } }
        Default { "Unknown" }
    }
}

function Get-LastInstalledDelphiVersion {
    if ($global:InstalledDelphiVersions.Count -eq 0) {
        Get-InstalledDelphiVersions
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
                $installedDelphiVersions += [PSCustomObject]@{ Version = $version; Data = $delphiName }
            }
        }
    }

    $global:InstalledDelphiVersions = $installedDelphiVersions | Sort-Object -Property Version -Descending
    if ($installedDelphiVersions.Count -eq 0) {
        Write-Error "No Delphi versions found." 
    }
    else {
        Write-Host "Installed Delphi versions:" -ForegroundColor Blue
        $global:InstalledDelphiVersions | ForEach-Object { Write-Host $_.Data.Name }
    }
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
    Build-DelphiProject -ProjectPath $projectPath\DRipGrepper.dproj -BuildConfig $BuildConfig -Platform "Win32" -StopOnFirstFailure -CountResult -Result ([ref]$result)
    Test-BuildResult -Result $result
    Build-DelphiProject -ProjectPath $projectPath\DRipGrepper.dproj -BuildConfig $BuildConfig -Platform "Win64" -StopOnFirstFailure -CountResult -Result ([ref]$result)
    Test-BuildResult -Result $result    
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
    $dllProjName = "$global:DllNameWithoutExt.$($latestVersion.Data.Dir -replace "Delphi", "D").dproj"
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
    # Extract pre-release suffix from CHANGELOG version (e.g. '-beta' from 'v4.15.0-beta')
    $versionSuffix = if ($global:Version -match '-([\w\.]+)$') { "-$($Matches[1])" } else { '' }

    "Win32" , "Win64" | ForEach-Object {
        $lastDelphiVer = Get-LastInstalledDelphiVersion
        $ZipDir = $(Join-Path $projectPath "src\Project\$($lastDelphiVer.Data.Dir)\$_\$BuildConfig")
        $AssetDir = $(Join-Path $global:AssetsDirectory $_)
    
        $win64 = $($_ -eq 'Win64')
        $assetObj = Add-ToAssetsDir -AssetDir $AssetDir -AssetItemPath $(Join-Path  $ZipDir $global:StandaloneAppName) -Win64:$win64 -ReplaceContent
        # Output to pipeline so callers receive this object (ForEach-Object has its own scope)
        $assetObj
        # Use the full FileVersion (including build number) + pre-release suffix for the zip filename
        $fileVersion = "v$($assetObj.Version.Trim())$versionSuffix"
        $dest = "$global:AssetsDirectory\$($global:AssetZipName -f $($win64 ? 'x64' : 'x86'), $fileVersion)"

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
    
        $dest = "$global:AssetsDirectory\$($global:AssetExtensionZipName  -f $($win64 ? 'x64' : 'x86'), $_.Data.Dir ,$global:Version)"
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
    # Extract pre-release suffix from CHANGELOG version (e.g. '-beta' from 'v4.15.0-beta')
    $versionSuffix = if ($global:Version -match '-([\w\.]+)$') { "-$($Matches[1])" } else { '' }
    Get-InstalledDelphiVersions | ForEach-Object {
        $ZipDir = $(Join-Path $projectPath "Extension\src\Project\Dll.$($_.Data.Dir)\$ReleaseType\$BuildConfig")
        $AssetDir = $(Join-Path $global:AssetsDirectory "$($_.Data.Dir).Dll")

        $dllName = "$global:DllNameWithoutExt.$($_.Data.Dir -replace "Delphi", "D").dll"
        $mapName = "$global:DllNameWithoutExt.$($_.Data.Dir -replace "Delphi", "D").map"
        $dllAssetObj = Add-ToAssetsDir -AssetDir $AssetDir $(Join-Path  $ZipDir $dllName) -Win64:$false -ReplaceContent
        # Output to pipeline so callers receive these objects (ForEach-Object has its own scope)
        $dllAssetObj
        Add-ToAssetsDir -AssetDir $AssetDir $(Join-Path  $ZipDir $mapName) -Win64:$false
        # Use the full FileVersion (including build number) + pre-release suffix for the zip filename
        $fileVersion = "v$($dllAssetObj.Version.Trim())$versionSuffix"
        $dest = "$global:AssetsDirectory\$($global:AssetExpertZipName -f $($win64 ? 'x64' : 'x86'), $_.Data.Dir, $fileVersion)"

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
        # Remove items recursively from the AssetsDirectory
        if ($LocalDeploy -or $DeployToTransferDrive) {
            $Force = $true
        }
        if ($BuildStandalone -or $BuildExtension) {
            Remove-Item -Path "$global:AssetsDirectory\*.zip" -Force -Confirm:$(-not $Force) -ErrorAction SilentlyContinue
        }

        $assetArr = New-StandaloneZips 
        # New-ExtensionZip 
        $assetArr += New-ExpertDllZip 

        # zip dll assets in directories not listed in $assetArr
        $versionSuffix = if ($global:Version -match '-([\w\.]+)$') { "-$($Matches[1])" } else { '' }
        Get-ChildItem $global:AssetsDirectory -Directory | ForEach-Object {
            if ($($assetArr | Select-Object -ExpandProperty Dir) -notcontains "$global:AssetsDirectoryName\$($_.Name)") {
                # Derive version from the first binary in the directory; fall back to CHANGELOG version
                $binaryInDir = Get-ChildItem $_.FullName -File | Where-Object { $_.Extension -in '.exe','.dll','.bpl' } | Select-Object -First 1
                $fileVersion = if ($binaryInDir) { "v$($binaryInDir.VersionInfo.FileVersion.Trim())$versionSuffix" } else { $global:Version }
                $zipPath = "$global:AssetsDirectory\$($global:AssetExpertZipName -f 'x86', $($_.Name -replace '\.Dll$'), $fileVersion)"
                if (-not (Test-Path $zipPath)) {
                    Write-Host "Zipping extra directory: $($_.Name)" -ForegroundColor Yellow
                    Compress-Archive -Path "$($_.FullName)\*" -DestinationPath $zipPath -Force
                }
            }
        }

        $assetArr | Format-Table -AutoSize -Property File, Version, LastWriteTime, Length, Dir
        $zipArr = List-Assets -Path $global:AssetsDirectory
        $zipArr | Format-Table -AutoSize -Property File, LastWriteTime, Length
    }

    if ($DeployToTransferDrive) {
        Write-Host "Copying files to $global:TransferDrive" -ForegroundColor Green
        Copy-Item -Path $global:AssetsDirectory\* -Destination $global:TransferDrive -Force -Recurse 
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
        # generate description from CHANGELOG.md
        $releaseDescription = Get-CurrentVersionChangesFromChangelog
        $release = New-Release -url $global:Url -headers $global:headers -version $global:Version -description $releaseDescription -preRelease:$global:PreRelease -dryRun:$DryRun
        
        if (-not $DryRun) {
            New-ReleaseNotes -owner $global:Owner -repo $global:Repo -headers $global:headers -version $global:Version -prevVersion $global:PrevVersion
        }
        else {
            Write-Host "DRY RUN: Would generate release notes with parameters" -ForegroundColor Cyan
            Write-Host "Version:`t$global:Version,`r`ntPrevious Version:`t$global:PrevVersion" 
            Write-Host "Owner:`t$global:Owner,`r`ntRepo`t$global:Repo" 
            Write-Host "Headers:`t$($global:headers | Out-String)" 
        }

        $ReleaseID = $release.id
        #$ReleaseID = $( Get-Releases -Latest | Select-Object -Property id).id

        Get-ChildItem $global:AssetsDirectory -Filter "*.zip" | ForEach-Object {
            Add-AssetToRelease -owner $global:Owner -repo $global:Repo -token $global:Token -releaseID $ReleaseID -zipFilePath $_.FullName -dryRun:$DryRun }
    }
}

function Update-ScoopManifest {

    Push-Location $env:SCOOP\buckets\my-scoop
    .\bin\checkver.ps1 -Update
    if ( -not $(Test-YesAnswer "Commit updated manifests?")) {
        Write-Error "Commit canceled" -ErrorAction Stop
    }
    git add . 
    git commit -m "dripgrepper $global:Version"
    if ( -not $(Test-YesAnswer "Push updated manifests?")) {
        Write-Error "Push canceled" -ErrorAction Stop
    }
    git push

    Pop-Location
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

    if ($LocalDeploy -or $DeployToGitHub -or $BuildStandalone -or $BuildExtension -or $BuildUIComponents -or $BuildUnittest -or $RunUnittest -or $DeployToTransferDrive) {
        #New-ReleaseNotes
        New-ReleaseWithAsset
    }
    if ($AddMissingAsset) {
        # Add assets to latest release
        $latest = Get-Releases  -URl $global:Url -Headers $global:headers -Tag $global:Version 
        $ReleaseID = $( $latest | Select-Object -Property id).id
        $assets = $( $latest | Select-Object -ExpandProperty assets | Select-Object -ExpandProperty name )
        Get-ChildItem $global:AssetsDirectory -Filter "*.zip" | ForEach-Object {
            if ($assets -notcontains $_.Name) {
                Write-Host "Adding asset $($_.Name) to latest release $global:Version" -ForegroundColor Green
                $answere = Read-Host "Do you want to add this asset? (y/n)"
                if ($answere -eq "y") {
                    Add-AssetToRelease -owner $global:Owner -repo $global:Repo -token $global:Token -releaseID $ReleaseID -zipFilePath $_.FullName 
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