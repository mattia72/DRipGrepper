# - Change Readme.md 
# - Change Description in this file (https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax, 
#                                    https://github.com/ikatyang/emoji-cheat-sheet/blob/master/README.md)
# - Change file and product version in every projects for ALL CONFIGURATION!
# - Commit and push all changes
# - run this script
$global:Version = "v2.6.1-beta"
$global:PrevVersion = "v2.6.0-beta"
$global:Description = @"
## Improvements and Bug Fixes

### :warning: Bug Fixes
* Align toolbars on split move and resize
* Commands in "Open with..." settings form will be saved to settings even if they are not valid

### Extension
* First release of `DripExtension280.bpl`
* Docking window location saved in layout file
* Selected text search started on hotkey (default: Shift+Alt+R)
* :warning: Settings should be stored on change 
"@

### :mag: Search Dialog
# + new feature
# + new feature
 
### :warning: Bug Fixes
#* bug

### Main Window
# + new feature

$global:PreRelease = $true
$global:StandaloneAppName = "DRipGrepper.exe"
$global:ExtensionFileName = "DRipExtension280.bpl";
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
    Import-Module -Name PSDelphi
    $parentPath = Split-Path -Parent $PSScriptRoot 
    $result = $null
    Build-DelphiProject -ProjectPath $parentPath\DRipGrepper.dproj -BuildConfig Release -StopOnFirstFailure -CountResult -Result ([ref]$result)
    if ($null -ne $result -and $result.ErrorCount -gt 0) {
        Write-Error "Deploy canceled." -ErrorAction Stop
    }
}

function Build-ExtensionRelease {
    # copy scripts
    Import-Module -Name PSDelphi
    $parentPath = Split-Path -Parent $PSScriptRoot 
    $extensionPath = Join-Path $parentPath "Extension"
    $result = $null
    Build-DelphiProject -ProjectPath $extensionPath\DRipExtension280.dproj -BuildConfig Release -StopOnFirstFailure -CountResult -Result ([ref]$result)
    if ($null -ne $result -and $result.ErrorCount -gt 0) {
        Write-Error "Deploy canceled." -ErrorAction Stop
    }
}

function Add-ToAssetsDir {
    param (
        $AssetPath
    )

    if ($AssetPath -match 'exe$' -and `
            -not $(Test-YesAnswer "Release version: $global:Version. Version of builded app: $((Get-Command $AssetPath).FileVersionInfo.FileVersion). Ok?")) {
        Write-Error "Deploy canceled" -ErrorAction Stop
    }
    New-Item -Path $global:AssetsDirectory -ItemType Directory -ErrorAction SilentlyContinue
    Copy-Item -Path $AssetPath -Destination $global:AssetsDirectory
}
function New-ReleaseWithAsset {
    $parentPath = Split-Path -Parent $PSScriptRoot 
    Build-StandaloneRelease
    $ZipDir = $(Join-Path $parentPath 'Win32\Release')
    Add-ToAssetsDir $(Join-Path  $ZipDir $global:StandaloneAppName) 
  
    Build-ExtensionRelease
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
                body                   = "$global:Description"
                draft                  = $false
                prerelease             = $global:PreRelease
                generate_release_notes = $true
            } | ConvertTo-Json )
    } 
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

#New-ReleaseNotes
New-ReleaseWithAsset

#Update scoop
Push-Location $env:SCOOP\buckets\my-scoop-bucket
.\bin\checkver.ps1 -Update
if ( -not $(Test-YesAnswer "Commit updated manifests?")) {
    Write-Error "Commit canceled" -ErrorAction Stop
}
git commit -m "dripgrepper $global:Version"
if ( -not $(Test-YesAnswer "Push updated manifests?")) {
    Write-Error "Push canceled" -ErrorAction Stop
}
git push

Pop-Location
scoop update dripgrepper