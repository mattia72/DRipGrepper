[CmdletBinding()]
param (
    $BuildConfig = "Release",
    [switch] $BuildStandalone,
    [switch] $BuildExtension,
    [switch] $RunUnittest,
    [switch] $Deploy,
    [switch] $LocalDeploy,
    [switch] $DeployToTransferDrive,
    [switch] $Force,
    [switch] $UpdateScoopManifest
)
    
# - Update Readme.md 
# - Update Deploy-Description.md 
# - Update file and product version in every projects for ALL CONFIGURATION!
# - Commit and push all changes
# - Run this script Ctrl+Shift+T Deploy
$global:TransferDrive = Get-Content "$PSScriptRoot\TransferDrive.txt" -ErrorAction SilentlyContinue
$global:Description = Get-Content "$PSScriptRoot\Deploy-Description.md"
$global:Version = ($global:Description | Select-String '^Version:') -replace 'Version:\s*'
$global:PrevVersion = ($global:Description | Select-String '^PrevVersion:') -replace 'PrevVersion:\s*'

$global:PreRelease = $true
$global:StandaloneAppName = "DRipGrepper.exe"
$global:ExtensionFileName = "DRipExtension.bpl"
$global:ExpertFileName = "DRipExtensions.dll"

$global:AssetZipName = "DRipGrepper.{0}.{1}.zip"
$global:AssetExpertZipName = "DRipExtension.Dll.{0}.{1}.{2}.zip"
$global:AssetExtensionZipName = "DRipExtension.Bpl.{0}.{1}.{2}.zip"
$global:AssetsDirectory = "$PSScriptRoot\assets"

$global:Owner = "mattia72"
$global:Repo = "DRipGrepper"
$global:Url = "https://api.github.com/repos/$global:Owner/$global:Repo/releases"
$global:Token = $(Get-Content $PSScriptRoot\SECRET_TOKEN)

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

function Build-StandaloneRelease {
    # copy scripts
    Import-Module -Name PSDelphi -Force
    $projectPath = Get-ProjectPath "src\Project" ""
    $result = $null
    Build-DelphiProject -ProjectPath $projectPath\DRipGrepper.dproj -BuildConfig $BuildConfig -Platform "Win32" -StopOnFirstFailure -CountResult -Result ([ref]$result)
    Test-BuildResult -result $result
    Build-DelphiProject -ProjectPath $projectPath\DRipGrepper.dproj -BuildConfig $BuildConfig -Platform "Win64" -StopOnFirstFailure -CountResult -Result ([ref]$result)
    Test-BuildResult -result $result    
}

function Build-AndRunUnittest {
    # copy scripts
    Import-Module -Name PSDelphi -Force
    $projectPath = Split-Path -Parent $PSScriptRoot 
    $unittestPath = Join-Path $projectPath "UnitTest"
    $result = $null
    Build-DelphiProject -ProjectPath $unittestPath\DRipGrepperUnittest.dproj -BuildConfig $BuildConfig -StopOnFirstFailure -CountResult -Result ([ref]$result)
    Test-BuildResult -result $result    
    $unittestPath = Join-Path $unittestPath "\Win32\$BuildConfig"

    & $unittestPath\DRipGrepperUnittest.exe -exitbehavior:Continue
    
    Write-Host -ForegroundColor Blue @"
    -------------
    Unittest results:
    Succeded?   : $?
    LASTEXITCODE: $LASTEXITCODE
    -------------
"@
    if (-not $? -or $LASTEXITCODE -ne 0) {
        Write-Error "Unittest failed, deploy canceled." -ErrorAction Stop
    }
}

function Build-ExtensionRelease {
    # copy scripts
    Import-Module -Name PSDelphi -Force
    $projectPath = Get-ProjectPath "Extension\src\Project" "Bpl."
    $result = $null
    Build-DelphiProject -ProjectPath $projectPath\DRipExtension.dproj -BuildConfig $BuildConfig -StopOnFirstFailure -CountResult -Result ([ref]$result) 
    Test-BuildResult -result $result
}

function Build-ExpertDllRelease {
    # copy scripts
    Import-Module -Name PSDelphi -Force
    $projectPath = Get-ProjectPath "Extension\src\Project" "Dll."
    $result = $null
    # add -LUDesignIde to the msbuild parameters
    # see https://docwiki.embarcadero.com/Libraries/Athens/en/DesignIntf
    Build-DelphiProject -ProjectPath $projectPath\DRipExtensions.dproj  -BuildConfig $BuildConfig -StopOnFirstFailure -CountResult -Result ([ref]$result)
    Test-BuildResult -result $result
}

function Add-ToAssetsDir {
    param (
        $AssetDir,
        $AssetItemPath,
        [switch]$Win64
    )

    $item = $(Get-Item $AssetItemPath)
    $appVersion = $($item.VersionInfo.FileVersion) # BPL is ok too :)
    if (-not $(Test-YesAnswer "Release version: $global:Version. Version of $($item.Name)($($Win64 ? 'Win64': 'Win32')) appName: $appVersion. Ok?")) {
        Write-Error "Search FileVersion=$appVersion in *.dproj and change it!`r`nDeploy stopped." -ErrorAction Stop
    }
    New-Item -Path $AssetDir -ItemType Directory -Force -ErrorAction SilentlyContinue | Out-Null
    Copy-Item -Path $AssetItemPath -Destination $AssetDir -ErrorAction Stop

    $assetLabel = $($item.FullName -replace "^(.*)($BuildConfig.*$)", "`$2" )
    $formattedLabel = $assetLabel.PadRight($global:PadRightValue)
    Write-Host "$formattedLabel  $($appVersion.PadRight(10)) $($item.LastWriteTime) added to $($AssetDir -replace [regex]::Escape("$PSScriptRoot\"), '')." -ForegroundColor Green
}

function New-StandaloneZips {
    $projectPath = Split-Path -Parent $PSScriptRoot 

    "Win32" , "Win64" | ForEach-Object {
        $lastDelphiVer = Get-LastInstalledDelphiVersion
        $ZipDir = $(Join-Path $projectPath "src\Project\$($lastDelphiVer.Data.Dir)\$_\$BuildConfig")
        $AssetDir = $(Join-Path $global:AssetsDirectory $_)
    
        $win64 = $($_ -eq 'Win64')
        Add-ToAssetsDir -AssetDir $AssetDir $(Join-Path  $ZipDir $global:StandaloneAppName) -Win64:$win64
    
        $dest = "$global:AssetsDirectory\$($global:AssetZipName -f $($win64 ? 'x64' : 'x86'), $global:Version)"

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
    Get-InstalledDelphiVersions | ForEach-Object {
        $delphiBplRoot = $("$env:PUBLIC\Documents\Embarcadero\Studio\$($_.Version)") 
        $extensionPath = Join-Path "$delphiBplRoot" "Bpl\$global:ExtensionFileName"
    
        $AssetDir = $(Join-Path $global:AssetsDirectory "$($_.Data.Dir).Bpl")
                
        Add-ToAssetsDir -AssetDir $AssetDir $extensionPath 
    
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
    Get-InstalledDelphiVersions | ForEach-Object {
        $ZipDir = $(Join-Path $projectPath "Extension\src\Project\Dll.$($_.Data.Dir)\$ReleaseType\$BuildConfig")
        $AssetDir = $(Join-Path $global:AssetsDirectory "$($_.Data.Dir).Dll")
        Add-ToAssetsDir -AssetDir $AssetDir $(Join-Path  $ZipDir $global:ExpertFileName) -Win64:$false
        $dest = "$global:AssetsDirectory\$($global:AssetExpertZipName -f $($win64 ? 'x64' : 'x86'), $_.Data.Dir ,$global:Version)"

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
function New-ReleaseWithAsset {

    if ($RunUnittest) {
        Build-AndRunUnittest
    }

    if ($BuildStandalone) {
        Build-StandaloneRelease 
    }
  
    if ($BuildExtension) {
        # Build-ExtensionRelease
        Build-ExpertDllRelease
    }

    if ($Deploy -or $LocalDeploy -or $DeployToTransferDrive) {
        # Remove items recursively from the AssetsDirectory
        if ($LocalDeploy -or $DeployToTransferDrive) {
            $Force = $true
        }
        Remove-Item -Path "$global:AssetsDirectory\*" -Recurse -Force -Confirm:$(-not $Force) -ErrorAction SilentlyContinue
        New-StandaloneZips 
        # New-ExtensionZip 
        New-ExpertDllZip 
        Write-Host "Assets:" -ForegroundColor Blue
        Get-Childitem $global:AssetsDirectory | ForEach-Object { 
            if ($_.PSIsContainer) { 
                # Write-Host "$($_.Name)`t$($_.CreationTime)" -ForegroundColor Blue
            }
            else {
                Write-Host "$($_.Name.PadRight($global:PadRightValue))`t$($_.CreationTime)`t$($_.Length)" -ForegroundColor Red 
            }
        }

        if ($Deploy -or $DeployToTransferDrive) {
            Copy-Item -Path $global:AssetsDirectory\Win64\* -Destination $global:TransferDrive\Latest -Verbose
            Copy-Item -Path $global:AssetsDirectory\Delphi11.Dll\* -Destination $global:TransferDrive\Latest -Verbose
            Copy-Item -Path $global:AssetsDirectory\* -Destination $global:TransferDrive -Verbose
        }
    }

    if ($Deploy) {
        New-Release
        New-ReleaseNotes

        $ReleaseID = $( Get-Releases -Tag $global:Version | Select-Object -Property id).id
        #$ReleaseID = $( Get-Releases -Latest | Select-Object -Property id).id

        Get-ChildItem $global:AssetsDirectory -Filter "*.zip" | ForEach-Object {
            New-Asset -ReleaseID $ReleaseID -ZipFilePath "$_" }
    }
}
function Get-Releases {
    param (
        [switch] $Latest,
        $Tag
    )
    $params = @{
        Uri     = "$global:Url$( $Latest ? "/latest" : '' )$($Tag -ne '' ? "/tags/$Tag" : '' )"
        Method  = "GET"
        Headers = $global:headers
        Body    = ''
    }
    $content = $(Invoke-RestMethod @params)
    $content | Select-Object -Property id, tag_name, html_url
}

function New-Release {
    $params = @{
        Uri     = $global:Url
        Method  = "POST"
        Headers = $global:headers
        Body    = $( @{ 
                tag_name               = "$global:Version"
                target_commitish       = "master"
                name                   = "$global:Version"
                body                   = "$($global:Description| Out-String)"
                draft                  = $false
                prerelease             = $global:PreRelease
                generate_release_notes = $true
            } | ConvertTo-Json )
    } 
    Write-Host "Send rest method with body: $($params.Body)"
    $response = $(Invoke-RestMethod @params)
    $response | Select-Object -Property id, tag_name, body, created_at
}

function New-ReleaseNotes {
    $params = @{
        Uri     = "https://api.github.com/repos/$global:Owner/$global:Repo/releases/generate-notes"
        Method  = "POST"
        Headers = $global:headers
        Body    = $( @{
                tag_name          = $global:Version
                target_commitish  = "master"
                previous_tag_name = $global:PrevVersion
                # configuration_file_path = ".github/release.yml"     
            } | ConvertTo-Json )
    }
    Write-Host "Send rest method with body: $($params.Body)"
    $response = $(Invoke-RestMethod @params)
    $response | Select-Object -Property name, body
}

function New-Asset {
    param (
        $ReleaseID,
        $ZipFilePath
    )
    $AssetZipName = $(Split-Path $ZipFilePath -Leaf)
    $CurlArgument = '-L', 
    '-X', 'POST',
    "-H", "Accept: application/vnd.github+json" ,
    "-H", "Authorization: Bearer $global:Token" ,
    "-H", "X-GitHub-Api-Version: 2022-11-28" ,
    "-H", "Content-Type: application/octet-stream" ,
    "https://uploads.github.com/repos/$global:Owner/$global:Repo/releases/$ReleaseID/assets?name=$AssetZipName" ,
    "--data-binary", "@$ZipFilePath"
    & curl.exe @CurlArgument
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
    if ($LocalDeploy -or $Deploy -or $BuildStandalone -or $BuildExtension -or $RunUnittest) {
        #New-ReleaseNotes
        New-ReleaseWithAsset
    }
    if ($UpdateScoopManifest) {
        #Update scoop with latest version from github
        Update-ScoopManifest
    }
}

New-Deploy