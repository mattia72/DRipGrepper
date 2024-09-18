[CmdletBinding()]
param (
    [switch] $BuildStandalone,
    [switch] $BuildExtension,
    [switch] $RunUnittest,
    [switch] $Deploy,
    [switch] $UpdateScoopManifest
)
    
# - Change Readme.md 
# - Change Deploy-Description.md 
# - Change file and product version in every projects for ALL CONFIGURATION!
# - Commit and push all changes
# - Run this script Ctrl+Shift+T Deploy

$global:Description = Get-Content "$PSScriptRoot\Deploy-Description.md"
$global:Version = ($global:Description | Select-String '^Version:') -replace 'Version:\s*'
$global:PrevVersion = ($global:Description | Select-String '^PrevVersion:') -replace 'PrevVersion:\s*'

$global:PreRelease = $true
$global:StandaloneAppName = "DRipGrepper.exe"
$global:ExtensionFileName = "DRipExtension.bpl";
$global:ExtensionPath = "$env:PUBLIC\Documents\Embarcadero\Studio\22.0\Bpl\"
$global:AssetZipName = "DRipGrepper.$global:Version.zip"
$global:AssetsDirectory = "$PSScriptRoot\assets"

$global:Owner = "mattia72"
$global:Repo = "DRipGrepper"
$global:Url = "https://api.github.com/repos/$global:Owner/$global:Repo/releases"
$global:Token = $(Get-Content $PSScriptRoot\SECRET_TOKEN)


$global:headers = @{
    "Content-Type"         = "application/json"
    Accept                 = "application/vnd.github+json"
    Authorization          = "Bearer $global:Token"
    "X-GitHub-Api-Version" = "2022-11-28"
}
function Test-YesAnswer {
    [CmdletBinding()]
    param (
        [string] $Message,
        [string] $DefaultAnswer = 'y'
    )
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
function Build-StandaloneRelease {
    # copy scripts
    Import-Module -Name PSDelphi -Force
    $parentPath = Split-Path -Parent $PSScriptRoot 
    $result = $null
    Build-DelphiProject -ProjectPath $parentPath\DRipGrepper.dproj -BuildConfig Release -StopOnFirstFailure -CountResult -Result ([ref]$result)
    if ($null -ne $result -and $result.ErrorCount -gt 0) {
        Write-Error "Deploy canceled." -ErrorAction Stop
    }
}

function Build-AndRunUnittest {
    # copy scripts
    Import-Module -Name PSDelphi -Force
    $parentPath = Split-Path -Parent $PSScriptRoot 
    $unittestPath = Join-Path $parentPath "UnitTest"
    $result = $null
    Build-DelphiProject -ProjectPath $unittestPath\DRipGrepperUnittest.dproj -BuildConfig Release -StopOnFirstFailure -CountResult -Result ([ref]$result)
    if ($null -ne $result -and $result.ErrorCount -gt 0) {
        Write-Error "Deploy canceled." -ErrorAction Stop
    }

    $unittestPath = Join-Path $unittestPath "\Win32\Release"

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
    $parentPath = Split-Path -Parent $PSScriptRoot 
    $extensionPath = Join-Path $parentPath "Extension"
    $result = $null
    Build-DelphiProject -ProjectPath $extensionPath\DRipExtension.dproj -BuildConfig Release -StopOnFirstFailure -CountResult -Result ([ref]$result)
    if ($null -ne $result -and $result.ErrorCount -gt 0) {
        Write-Error "Deploy canceled." -ErrorAction Stop
    }
}

function Add-ToAssetsDir {
    param (
        $AssetPath
    )
    $appVersion = $((Get-Command $AssetPath).FileVersionInfo.FileVersion) # BPL is ok too :)
    if (-not $(Test-YesAnswer "Release version: $global:Version. Version of builded app: $appVersion. Ok?")) {
        Write-Error "Search FileVersion=$appVersion in *.dproj and change it!`r`nDeploy stopped." -ErrorAction Stop
    }
    New-Item -Path $global:AssetsDirectory -ItemType Directory -ErrorAction SilentlyContinue
    Copy-Item -Path $AssetPath -Destination $global:AssetsDirectory
}
function New-ReleaseWithAsset {
   # Remove items recursively from the AssetsDirectory
    Remove-Item -Path "$global:AssetsDirectory\*" -Recurse -Force -Verbose -Confirm

    if ($RunUnittest) {
        Build-AndRunUnittest
    }

    if ($BuildStandalone) {
        Build-StandaloneRelease 
    }
  
    if ($BuildExtension) {
        Build-ExtensionRelease
    }

    if ($Deploy) {
        $parentPath = Split-Path -Parent $PSScriptRoot 
        $ZipDir = $(Join-Path $parentPath 'Win32\Release')

        Add-ToAssetsDir $(Join-Path  $ZipDir $global:StandaloneAppName) 
        Add-ToAssetsDir $(Join-Path $global:ExtensionPath $global:ExtensionFileName) 

        $compress = @{
            Path             = "$global:AssetsDirectory\*.*"
            CompressionLevel = "Fastest"
            DestinationPath  = "$global:AssetsDirectory\$global:AssetZipName"
            Force            = $true
        }
        Compress-Archive @compress

        New-Release
        New-ReleaseNotes

        $ReleaseID = $( Get-Releases -Tag $global:Version | Select-Object -Property id).id
        #$ReleaseID = $( Get-Releases -Latest | Select-Object -Property id).id

        New-Asset -ReleaseID $ReleaseID -ZipFilePath "$global:AssetsDirectory\$global:AssetZipName"
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
    $CurlArgument = '-L', 
    '-X', 'POST',
    "-H", "Accept: application/vnd.github+json" ,
    "-H", "Authorization: Bearer $global:Token" ,
    "-H", "X-GitHub-Api-Version: 2022-11-28" ,
    "-H", "Content-Type: application/octet-stream" ,
    "https://uploads.github.com/repos/$global:Owner/$global:Repo/releases/$ReleaseID/assets?name=$global:AssetZipName" ,
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
    #New-ReleaseNotes
    New-ReleaseWithAsset
    if ($UpdateScoopManifest) {
        #Update scoop
        Update-ScoopManifest
    }
}

New-Deploy