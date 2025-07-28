function Get-Releases {
    param (
        [string] $Url,
        [string] $Headers,
        [switch] $Latest, # only for real releases, not for pre-releases
        [string] $Tag
    )
    $params = @{
        Uri     = "$Url$( $Latest ? "/latest" : '' )$(($Tag -eq '' -or $null -eq $Tag) ? '' : "/tags/$Tag" )"
        Method  = "GET"
        Headers = $Headers
        Body    = ''
    }
    $content = $(Invoke-RestMethod @params)
    $content | Select-Object -Property id, tag_name, html_url, assets
}

function New-Release {
    param(
        [string]$url,
        [hashtable]$headers,
        [string]$version,
        [string]$description,
        [bool]$preRelease = $true
    )
    $params = @{
        Uri     = $url
        Method  = "POST"
        Headers = $headers
        Body    = $( @{ 
                tag_name               = $version
                target_commitish       = "master"
                name                   = $version
                body                   = $description
                draft                  = $false
                prerelease             = $preRelease
                generate_release_notes = $true
            } | ConvertTo-Json )
    } 
    Write-Host "Send rest method with body: $($params.Body)"
    $response = $(Invoke-RestMethod @params)
    $response | Select-Object -Property id, tag_name, body, created_at
}

function New-ReleaseNotes {
    param(
        [string]$owner,
        [string]$repo,
        [hashtable]$headers,
        [string]$version,
        [string]$prevVersion
    )
    $params = @{
        Uri     = "https://api.github.com/repos/$owner/$repo/releases/generate-notes"
        Method  = "POST"
        Headers = $headers
        Body    = $( @{
                tag_name          = $version
                target_commitish  = "master"
                previous_tag_name = $prevVersion
                # configuration_file_path = ".github/release.yml"     
            } | ConvertTo-Json )
    }
    Write-Host "Send rest method with body: $($params.Body)"
    $response = $(Invoke-RestMethod @params)
    $response | Select-Object -Property id, name, body
}

function Add-AssetToRelease {
    param (
        [string]$owner,
        [string]$repo,
        [string]$token,
        [string]$releaseID,
        [string]$zipFilePath
    )
    $AssetZipName = $(Split-Path $ZipFilePath -Leaf)
    $CurlArgument = '-L', 
    '-X', 'POST',
    "-H", "Accept: application/vnd.github+json" ,
    "-H", "Authorization: Bearer $global:Token" ,
    "-H", "X-GitHub-Api-Version: 2022-11-28" ,
    "-H", "Content-Type: application/octet-stream" ,
    "https://uploads.github.com/repos/$owner/$repo/releases/$releaseID/assets?name=$AssetZipName" ,
    "--data-binary", "@$ZipFilePath"
    & curl.exe @CurlArgument
}
