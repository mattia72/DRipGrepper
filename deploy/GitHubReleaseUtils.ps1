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
    Write-Host "New-Release: sends rest msg with body: $($params.Body)"
    
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
    Write-Host "New-ReleaseNotes: sends rest msg with body: $($params.Body)"
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
    
    # GitHub API expects raw binary data for asset uploads
    $CurlArgument = @(
        '-L'
        '-X', 'POST'
        '-H', "Accept: application/vnd.github+json"
        '-H', "Authorization: Bearer $token"
        '-H', "X-GitHub-Api-Version: 2022-11-28"
        '-H', "Content-Type: application/zip"
        '--data-binary', "@$ZipFilePath"
        "https://uploads.github.com/repos/$owner/$repo/releases/$releaseID/assets?name=$AssetZipName"
    )
    
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
    
    Write-Host "Uploading asset: $AssetZipName to release $releaseID" -ForegroundColor Green
    $result = & curl.exe @CurlArgument 2>&1
    
    # Check for errors
    if ($LASTEXITCODE -ne 0) {
        Write-Error "Failed to upload asset $AssetZipName. Exit code: $LASTEXITCODE. Output: $result"
        return $false
    }
    
    Write-Host "Successfully uploaded: $AssetZipName" -ForegroundColor Green
    return $true
}

<#
.SYNOPSIS
    Creates a new GitHub issue in the specified repository.

.DESCRIPTION
    This function creates a new issue in a GitHub repository using the GitHub REST API.
    It supports setting title, body, labels, assignees, and milestone.

.PARAMETER owner
    The GitHub repository owner (username or organization name).

.PARAMETER repo
    The name of the GitHub repository.

.PARAMETER headers
    Hashtable containing HTTP headers, including authorization token.

.PARAMETER title
    The title of the issue.

.PARAMETER body
    The body/description of the issue.

.PARAMETER labels
    Optional array of labels to apply to the issue.

.PARAMETER assignee
    Optional single assignee username.

.PARAMETER assignees
    Optional array of assignee usernames.

.PARAMETER milestone
    Optional milestone number to assign to the issue.

.PARAMETER dryRun
    If true, shows what would be created without actually creating the issue.

.EXAMPLE
    $headers = @{ "Authorization" = "token $token"; "Accept" = "application/vnd.github.v3+json" }
    New-Issue -owner "mattia72" -repo "DRipGrepper" -headers $headers -title "Bug fix needed" -body "Description" -labels @("bug")

.EXAMPLE
    New-Issue -owner "user" -repo "project" -headers $headers -title "Feature request" -body "Details" -assignee "developer" -dryRun $true
#>
function New-Issue {
    param(
        [string]$owner,
        [string]$repo,
        [hashtable]$headers,
        [string]$title,
        [string]$body,
        [string[]]$labels = @(),
        [string]$assignee = "",
        [string[]]$assignees = @(),
        [int]$milestone = $null,
        [bool]$dryRun = $false
    )
    
    $issueData = @{
        title = $title
        body = $body
    }
    
    if ($labels.Count -gt 0) {
        $issueData.labels = $labels
    }
    
    if ($assignee -and $assignee -ne "") {
        $issueData.assignee = $assignee
    }
    
    if ($assignees.Count -gt 0) {
        $issueData.assignees = $assignees
    }
    
    if ($milestone) {
        $issueData.milestone = $milestone
    }
    
    $params = @{
        Uri     = "https://api.github.com/repos/$owner/$repo/issues"
        Method  = "POST"
        Headers = $headers
        Body    = $($issueData | ConvertTo-Json)
    }
    
    if ($dryRun) {
        Write-Host "DRY RUN: Would create issue with the following parameters:" -ForegroundColor Cyan
        Write-Host "URL: $($params.Uri)" -ForegroundColor Gray
        Write-Host "Title: $title" -ForegroundColor Gray
        Write-Host "Labels: $($labels -join ', ')" -ForegroundColor Gray
        Write-Host "Body: $($body -replace '\\n', "`r`n")" -ForegroundColor Gray
        # Return mock response for dry run
        return @{
            number = "DRYRUN-$(Get-Random)"
            title = $title
            body = $body
            html_url = "https://github.com/$owner/$repo/issues/DRYRUN"
            created_at = Get-Date
        }
    }
    
    Write-Host "New-Issue: Creating issue '$title' in $owner/$repo"
    $response = Invoke-RestMethod @params
    $response | Select-Object -Property number, title, body, html_url, created_at
}
