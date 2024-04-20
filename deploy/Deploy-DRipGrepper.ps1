# - Change Readme.md 
# - Change Description in this file (https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax, 
#                                    https://github.com/ikatyang/emoji-cheat-sheet/blob/master/README.md)
# - Change file version in project for all! configuration
# - Commit and push all changes
# - run this script
$global:Version = "v2.4.0-beta"
$global:PrevVersion = "v2.3.0-beta"
$global:Description = @"
## Improvements and Bug Fixes

### Main Window
+ Error parser on rg exit code 1 on lines beginning with ``rg:``
+ No match parser (0 in 0)

### :warning: Bug Fixes
* Expand / collapse nodes on history item click 
* Status line shows the correct released version
"@

### :mag: Search Dialog
# + new feature
# + new feature
 
### Main Window
# + new feature

$global:PreRelease = $true
$global:StandaloneAppName = "DRipGrepper.exe"
$global:AssetZipName = "DRipGrepper.Standalone.exe.zip"

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
function Build-Release {
    # copy scripts
    Import-Module -Name PSDelphi -Force
    $parentPath = Split-Path -Parent $PSScriptRoot 
    $result = $null
    Build-DelphiProject -ProjectPath $parentPath\DRipGrepper.dproj -BuildConfig Release -StopOnFirstFailure -CountResult -Result ([ref]$result)
    if ($null -ne $result -and $result.ErrorCount -gt 0) {
        Write-Error "Deploy canceled." -ErrorAction Stop
    }
}

function New-ReleaseWithAsset {
    $parentPath = Split-Path -Parent $PSScriptRoot 
    Build-Release
    $ZipDir = Join-Path $parentPath 'Win32\Release'
    $ExePath = "$ZipDir\$global:StandaloneAppName"
    if ( -not $(Test-YesAnswer "Release version: $global:Version. Version of builded app: $((Get-Command $ExePath).FileVersionInfo.FileVersion). Ok?")) {
        Write-Error "Deploy canceled" -ErrorAction Stop
    }
    New-Release
    New-ReleaseNotes
    Import-Module PSZip
    Compress-Archive -Path $ExePath -DestinationPath $ZipDir\$global:AssetZipName -Force
    $ReleaseID = $( Get-Releases -Tag $global:Version | Select-Object -Property id).id
    #$ReleaseID = $( Get-Releases -Latest | Select-Object -Property id).id
    New-Asset -ReleaseID $ReleaseID -ZipDir $ZipDir
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
        $ZipDir
    )
    $CurlArgument = '-L', 
    '-X', 'POST',
    "-H", "Accept: application/vnd.github+json" ,
    "-H", "Authorization: Bearer $global:Token" ,
    "-H", "X-GitHub-Api-Version: 2022-11-28" ,
    "-H", "Content-Type: application/octet-stream" ,
    "https://uploads.github.com/repos/$global:Owner/$global:Repo/releases/$ReleaseID/assets?name=$global:AssetZipName" ,
    "--data-binary", "@$ZipDir\$global:AssetZipName"
    & curl.exe @CurlArgument
}

#New-ReleaseNotes
New-ReleaseWithAsset