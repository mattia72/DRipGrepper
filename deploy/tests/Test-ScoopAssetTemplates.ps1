# Run directly with PowerShell; no GitHub access or Scoop changes required.
$ErrorActionPreference = 'Stop'
$deployScript = Join-Path (Split-Path $PSScriptRoot -Parent) 'Deploy-DRipGrepper.ps1'
$tokens = $null
$parseErrors = $null
$ast = [System.Management.Automation.Language.Parser]::ParseFile($deployScript, [ref]$tokens, [ref]$parseErrors)
if ($parseErrors.Count) { throw ($parseErrors.Message -join '; ') }
foreach ($name in 'Test-DelphiDeploymentEnabled', 'Set-ScoopAssetTemplates') {
    $definition = $ast.Find({
        param($node)
        $node -is [System.Management.Automation.Language.FunctionDefinitionAst] -and $node.Name -eq $name
    }, $true)
    . ([scriptblock]::Create($definition.Extent.Text))
}

function Assert-Equal($Actual, $Expected, [string]$Message) {
    if (($Actual -join '|') -cne ($Expected -join '|')) {
        throw "${Message}: expected [$Expected], got [$Actual]"
    }
}

$global:Owner = 'mattia72'
$global:Repo = 'DRipGrepper'
$global:Url = "https://api.github.com/repos/$global:Owner/$global:Repo/releases"
$baseUrl = "https://github.com/$global:Owner/$global:Repo/releases/download"
$manifestPath = Join-Path ([IO.Path]::GetTempPath()) ('RipGrepper-ScoopTest-' + [guid]::NewGuid().ToString('N') + '.json')
try {
    $architectures = [ordered]@{}
    foreach ($entry in @(@('64bit', 'x64'), @('32bit', 'x86'))) {
        $architectures[$entry[0]] = @{
            url = @(
                "$baseUrl/v4.15.0-beta/DRipGrepper.$($entry[1]).v4.15.0-beta.zip"
                "$baseUrl/v4.15.0-beta/DRipExtension.Dll.x86.Delphi11.v4.15.0-beta.zip"
                "$baseUrl/v4.15.0-beta/DRipExtension.Dll.x86.Delphi12.v4.15.0-beta.zip"
                "$baseUrl/v4.9.0-beta/DRipExtension.Dll.x86.Delphi12.3.v4.9.0-beta.zip"
            )
            hash = @('old-app-hash', 'old-dll-hash', 'paused-hash', 'legacy-hash')
        }
    }
    @{
        architecture = $architectures
        autoupdate = @{ architecture = $architectures }
        checkver = @{ jsonpath = '$[0].tag_name'; regex = 'v([\d.]+)' }
        persist = 'DRipGrepper.ini'
    } | ConvertTo-Json -Depth 20 | Set-Content -LiteralPath $manifestPath

    Set-ScoopAssetTemplates -ManifestPath $manifestPath
    $firstResult = Get-Content -LiteralPath $manifestPath -Raw
    Set-ScoopAssetTemplates -ManifestPath $manifestPath
    Assert-Equal (Get-Content -LiteralPath $manifestPath -Raw) $firstResult 'Template setup can be repeated'
    $manifest = $firstResult | ConvertFrom-Json
    Assert-Equal $manifest.persist 'DRipGrepper.ini' 'Preserve installation settings'
    Assert-Equal $manifest.autoupdate.architecture.'64bit'.url.Count 2 'Exclude paused Delphi 12 assets'
    Assert-Equal $manifest.architecture.'64bit'.url.Count 2 'Remove obsolete installation URLs'
    Assert-Equal $manifest.architecture.'64bit'.hash @('old-app-hash', 'old-dll-hash') 'Keep hashes aligned with retained URLs'
    Assert-Equal $manifest.checkver.jsonpath $null 'Remove tag-only version extraction'

    function Get-ResolvedAssets([string[]]$Names, [string]$Tag) {
        $assets = @($Names | ForEach-Object { @{ name = $_; browser_download_url = "$baseUrl/$Tag/$_" } })
        $page = ConvertTo-Json -Depth 10 -InputObject @(
            @{ draft = $true; assets = @() }
            @{ draft = $false; assets = $assets }
        )
        $output = Invoke-Command ([scriptblock]::Create($manifest.checkver.script -join "`n"))
        $match = [regex]::Match([string]$output, $manifest.checkver.regex)
        if (-not $match.Success) { throw 'Scoop checkver did not match the release assets' }
        $resolved = @{}
        foreach ($arch in '64bit', '32bit') {
            $resolved[$arch] = @($manifest.autoupdate.architecture.$arch.url | ForEach-Object {
                $group = $_ -replace '^\$match', ''
                # .NET group names are case-sensitive; Scoop title-cases substitutions.
                $group = $group.Substring(0, 1).ToLowerInvariant() + $group.Substring(1)
                $match.Groups[$group].Value
            })
        }
        @{ Version = $match.Groups[1].Value; Urls = $resolved }
    }

    $result = Get-ResolvedAssets -Tag 'v5.0.0-beta' -Names @(
        'DRipGrepper.x86.v5.0.0.9-beta.zip'
        'DRipGrepper.x86.v5.0.0.10-beta.zip'
        'DRipGrepper.x86.v5.0.0.481-beta.zip'
        'DRipGrepper.x64.v5.0.0.482-beta.zip'
        'DRipExtension.Dll.x86.Delphi11.v5.0.0.600-beta.zip'
    )
    Assert-Equal $result.Version '5.0.0.482-beta' 'Scoop version includes standalone build'
    Assert-Equal $result.Urls.'32bit'[0] "$baseUrl/v5.0.0-beta/DRipGrepper.x86.v5.0.0.481-beta.zip" 'Use real x86 URL with separate release tag'
    Assert-Equal $result.Urls.'64bit'[1] "$baseUrl/v5.0.0-beta/DRipExtension.Dll.x86.Delphi11.v5.0.0.600-beta.zip" 'Independent DLL build'
    Assert-Equal $result.Urls.'32bit'[1] $result.Urls.'64bit'[1] 'Shared DLL URL across architectures'
    Write-Host 'PASS: Real asset URLs, numeric build selection, independent binary versions, draft exclusion, and repeatable setup'

    $result = Get-ResolvedAssets -Tag 'v4.15.0-beta' -Names @(
        'DRipGrepper.x86.v4.15.0-beta.zip'
        'DRipGrepper.x64.v4.15.0-beta.zip'
        'DRipExtension.Dll.x86.Delphi11.v4.15.0-beta.zip'
    )
    Assert-Equal $result.Version '4.15.0-beta' 'Legacy archives remain supported'
    Write-Host 'PASS: Legacy names without build numbers'
}
finally {
    if (Test-Path -LiteralPath $manifestPath) {
        Remove-Item -LiteralPath $manifestPath -Force
    }
}
