$global:Version = "v1.0.4-beta"
$global:PrevVersion = "v1.0.3-alpha"      
$global:PreRelease = $true
$global:Description = "Bug Fixes and some improvements"
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

function New-ReleaseWithAsset {
    New-Release
    New-ReleaseNotes
    Import-Module PSZip
    $parentPath = Split-Path -Parent $PSScriptRoot 
    $ZipDir = Join-Path $parentPath 'Win32\Release'
    Compress-Archive -Path $ZipDir\DRipGrepper.exe -DestinationPath $ZipDir\$global:AssetZipName -Force
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