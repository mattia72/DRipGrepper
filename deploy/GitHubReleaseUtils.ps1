# GitHubReleaseUtils.ps1
# This module contains utility functions for creating GitHub releases and managing assets.

function Get-Releases {
    param (
        [string] $Url,
        [hashtable] $Headers,
        [switch] $Latest, # only for real releases, not for pre-releases
        [string] $Tag
    )
    if ($Latest) {
        $uri = "$Url/latest"
    } elseif ($Tag -and $Tag -ne '') {
        $uri = "$Url/tags/$Tag"
    } else {
        $uri = $Url
    }
    $params = @{
        Uri     = $uri
        Method  = "GET"
        Headers = $Headers
    }
    $content = Invoke-RestMethod @params
    if ($null -eq $content) { return $null }
    $content | Select-Object -Property id, tag_name, html_url, assets
}

function New-Release {
    param(
        [string]$Url,
        [hashtable]$Headers,
        [string]$version,
        [string]$description,
        [bool]$preRelease = $true,
        [bool]$DryRun = $false
    )
    $params = @{
        Uri     = $Url
        Method  = "POST"
        Headers = $Headers
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
    
    if ($DryRun) {
        Write-Host "DRY RUN: Would create release with the following parameters:" -ForegroundColor Cyan
        Write-Host "URL: $Url" -ForegroundColor Gray
        Write-Host "Body: $($params.Body -replace '\\n', "`r`n")" -ForegroundColor Gray
        # Return mock response for dry run
        return @{
            id = "DRYRUN-$(Get-Random)"
            tag_name = $version
            body = $description
            created_at = Get-Date
        }
    }
    
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
        [string]$zipFilePath,
        [bool]$dryRun = $false
    )
    $AssetZipName = $(Split-Path $ZipFilePath -Leaf)
    $CurlArgument = '-L', 
    '-X', 'POST',
    "-H", "Accept: application/vnd.github+json" ,
    "-H", "Authorization: Bearer $token" ,
    "-H", "X-GitHub-Api-Version: 2022-11-28" ,
    "-H", "Content-Type: application/octet-stream" ,
    "https://uploads.github.com/repos/$owner/$repo/releases/$releaseID/assets?name=$AssetZipName" ,
    "--data-binary", "@$ZipFilePath"
    
    if ($DryRun) {
        Write-Host "DRY RUN: Would upload asset: $AssetZipName to release $releaseID" -ForegroundColor Cyan
        Write-Host "File path: $zipFilePath" -ForegroundColor Gray
        # Group arguments in pairs for better readability
        $formattedArgs = @($CurlArgument[0])
        for ($i = 1; $i -lt $CurlArgument.Length; $i += 2) {
            if ($i + 1 -lt $CurlArgument.Length) {
                $formattedArgs += "$($CurlArgument[$i]) $($CurlArgument[$i + 1])"
            } else {
                $formattedArgs += $CurlArgument[$i]
            }
        }
        Write-Host "Command: curl.exe `r`n    $($formattedArgs -join " `r`n    ")" -ForegroundColor Gray
        return
    }
    
    & curl.exe @CurlArgument
}
